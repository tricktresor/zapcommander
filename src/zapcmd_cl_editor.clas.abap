CLASS zapcmd_cl_editor DEFINITION
  PUBLIC
  FINAL
  CREATE PUBLIC .

*"* public components of class ZAPCMD_CL_EDITOR
*"* do not include other source files here!!!
  PUBLIC SECTION.

    CLASS-DATA gf_instance TYPE REF TO zapcmd_cl_editor .
    DATA cf_gui_edit TYPE REF TO cl_gui_textedit .
    DATA cf_file TYPE REF TO zapcmd_cl_file .
    DATA ct_file TYPE string .
    DATA cf_readonly TYPE xfeld .
    DATA cf_gui_sedit TYPE REF TO cl_gui_sourceedit .

    METHODS show
      IMPORTING
        !pf_container TYPE REF TO cl_gui_container .
    METHODS constructor .
    METHODS save .
    METHODS user_command
      CHANGING
        !e_ucomm TYPE syucomm .
    METHODS set_file
      IMPORTING
        !pf_file TYPE REF TO zapcmd_cl_file .
    METHODS set_readonly
      IMPORTING
        !pf_readonly TYPE xfeld DEFAULT 'X' .
    METHODS free .
    METHODS get_title
      EXPORTING
        !pf_title1 TYPE c
        !pf_title2 TYPE c
        !pf_title3 TYPE c
        !pf_title4 TYPE c .
    CLASS-METHODS class_constructor .
    CLASS-METHODS call_editor
      IMPORTING
        !pf_file     TYPE REF TO zapcmd_cl_file
        !pf_readonly TYPE xfeld .
  PROTECTED SECTION.
*"* protected components of class ZAPCMD_CL_EDITOR
*"* do not include other source files here!!!
  PRIVATE SECTION.
*"* private components of class ZAPCMD_CL_EDITOR
*"* do not include other source files here!!!
ENDCLASS.



CLASS ZAPCMD_CL_EDITOR IMPLEMENTATION.


  METHOD call_editor.

    CALL METHOD gf_instance->set_file
      EXPORTING
        pf_file = pf_file.

    gf_instance->set_readonly( pf_readonly ).

    CALL FUNCTION 'ZAPCMD_CALL_EDITOR'
      EXPORTING
        pf_editor = gf_instance.

    gf_instance->free( ).

  ENDMETHOD.


  METHOD class_constructor.

    CREATE OBJECT gf_instance.

  ENDMETHOD.


  METHOD constructor.
  ENDMETHOD.


  METHOD free.

    IF cf_gui_edit IS BOUND.
      CALL METHOD cf_gui_edit->free.
      FREE cf_gui_edit.
    ENDIF.
    IF cf_gui_sedit IS BOUND.
      CALL METHOD cf_gui_sedit->free.
      FREE cf_gui_sedit.
    ENDIF.

    CLEAR cf_file.
    CLEAR cf_readonly.

  ENDMETHOD.


  METHOD get_title.

    pf_title1 = cf_file->name.

  ENDMETHOD.


  METHOD save.

    DATA lt_data TYPE zapcmd_tbl_string.
    DATA lf_is_modified TYPE i.

    DATA lf_data TYPE string.

    IF cf_readonly IS INITIAL.

      IF cf_gui_edit IS BOUND.

        cf_gui_edit->get_textstream(
          EXPORTING
            only_when_modified     = 1
          IMPORTING
            text                   = lf_data
            is_modified            = lf_is_modified
          EXCEPTIONS
            error_cntl_call_method = 1
            not_supported_by_gui   = 2
            OTHERS                 = 3
               ).
        IF sy-subrc <> 0.
          MESSAGE ID sy-msgid TYPE sy-msgty NUMBER sy-msgno
                     WITH sy-msgv1 sy-msgv2 sy-msgv3 sy-msgv4.
        ENDIF.

        CALL METHOD cl_gui_cfw=>flush.
        IF lf_is_modified <> 0.
          lt_data = zapcmd_cl_file=>stream2text( lf_data ).

          CALL METHOD cf_file->write_text
            EXPORTING
              pt_file = lt_data.
          CALL METHOD cf_gui_edit->set_textmodified_status
            EXPORTING
              status                 = 0
            EXCEPTIONS
              error_cntl_call_method = 1
              OTHERS                 = 2.
          IF sy-subrc <> 0.
            MESSAGE ID sy-msgid TYPE sy-msgty NUMBER sy-msgno
                       WITH sy-msgv1 sy-msgv2 sy-msgv3 sy-msgv4.
          ENDIF.


        ENDIF.
      ENDIF.
      IF cf_gui_sedit IS BOUND.

        cf_gui_sedit->get_text(
          IMPORTING
            table                   = lt_data
            is_modified            = lf_is_modified
          EXCEPTIONS
            error_cntl_call_method = 1
            error_dp               = 2
            OTHERS                 = 3
               ).
        IF sy-subrc <> 0.
          MESSAGE ID sy-msgid TYPE sy-msgty NUMBER sy-msgno
                     WITH sy-msgv1 sy-msgv2 sy-msgv3 sy-msgv4.
        ENDIF.

        CALL METHOD cl_gui_cfw=>flush.
        IF lf_is_modified <> 0.

          CALL METHOD cf_file->write_text
            EXPORTING
              pt_file = lt_data.
          CALL METHOD cf_gui_sedit->set_textmodified_status
            EXCEPTIONS
              error_cntl_call_method = 1
              OTHERS                 = 2.
          IF sy-subrc <> 0.
            MESSAGE ID sy-msgid TYPE sy-msgty NUMBER sy-msgno
                       WITH sy-msgv1 sy-msgv2 sy-msgv3 sy-msgv4.
          ENDIF.


        ENDIF.
      ENDIF.
    ENDIF.

  ENDMETHOD.


  METHOD set_file.

    cf_file = pf_file.

  ENDMETHOD.


  METHOD set_readonly.

    cf_readonly = pf_readonly.

  ENDMETHOD.


  METHOD show.

    DATA lt_data TYPE zapcmd_tbl_string.

    IF cf_gui_edit IS INITIAL AND cf_gui_sedit IS INITIAL.

      DATA l_extension TYPE string.
      l_extension = cf_file->get_extension( ).
      IF strlen( l_extension ) >= 3 AND l_extension(3) = 'ABA'.

        CREATE OBJECT cf_gui_sedit TYPE cl_gui_abapedit
          EXPORTING
            parent = pf_container.

        cf_file->read_text(
          IMPORTING
            pt_file = lt_data ).
        cf_gui_sedit->set_text( table = lt_data ).
      ELSE.

        CREATE OBJECT cf_gui_edit
          EXPORTING
            parent        = pf_container
            wordwrap_mode = 0.

        cf_gui_edit->set_font_fixed( 1 ).
        cf_file->read_text( IMPORTING pt_file = lt_data ).
        ct_file = zapcmd_cl_file=>text2stream( lt_data ).
        cf_gui_edit->set_textstream( text = ct_file ).
      ENDIF.

    ENDIF.

    IF cf_gui_edit IS BOUND.
      IF cf_readonly = 'X'.
        cf_gui_edit->set_readonly_mode( 1 ).
      ELSE.
        cf_gui_edit->set_readonly_mode( 0 ).
      ENDIF.
    ENDIF.

    IF cf_gui_sedit IS BOUND.
      IF cf_readonly = 'X'.
        cf_gui_sedit->set_readonly_mode( 1 ).
      ELSE.
        cf_gui_sedit->set_readonly_mode( 0 ).
      ENDIF.
    ENDIF.

  ENDMETHOD.


  METHOD user_command.

    DATA lf_is_modified TYPE i.
    DATA lf_answer TYPE char1.

    CASE e_ucomm.
      WHEN 'BACK' OR 'EXIT' OR 'REFRESH'.
        IF cf_readonly IS INITIAL.

          IF cf_gui_edit IS BOUND.
            cf_gui_edit->get_textmodified_status(
              IMPORTING
                status                 = lf_is_modified ).
          ENDIF.
          IF cf_gui_sedit IS BOUND.
            cf_gui_sedit->get_textmodified_status(
              IMPORTING
                status                 = lf_is_modified ).
          ENDIF.


          IF lf_is_modified <> 0.
            CALL FUNCTION 'POPUP_TO_CONFIRM'
              EXPORTING
                titlebar      = 'Text verändert.'(300)
                text_question = 'Soll der Text gespeichert werden?'(301)
              IMPORTING
                answer        = lf_answer.
            IF sy-subrc <> 0.
              MESSAGE ID sy-msgid TYPE sy-msgty NUMBER sy-msgno
                      WITH sy-msgv1 sy-msgv2 sy-msgv3 sy-msgv4.
              RETURN.
            ENDIF.

            IF lf_answer = '1'.
              save( ).
            ENDIF.
            IF lf_answer = 'A'.
              e_ucomm = ''.
            ENDIF.
          ENDIF.
        ENDIF.

      WHEN 'SAVE'.
        save( ).
    ENDCASE.

  ENDMETHOD.
ENDCLASS.
