class ZAPCMD_CL_EDITOR definition
  public
  final
  create public .

*"* public components of class ZAPCMD_CL_EDITOR
*"* do not include other source files here!!!
public section.

  class-data GF_INSTANCE type ref to ZAPCMD_CL_EDITOR .
  data CF_GUI_EDIT type ref to CL_GUI_TEXTEDIT .
  data CF_FILE type ref to ZAPCMD_CL_FILE .
  data CT_FILE type STRING .
  data CF_READONLY type XFELD .
  data CF_GUI_SEDIT type ref to CL_GUI_SOURCEEDIT .

  methods SHOW
    importing
      !PF_CONTAINER type ref to CL_GUI_CONTAINER .
  methods CONSTRUCTOR .
  methods SAVE .
  methods USER_COMMAND
    changing
      !E_UCOMM type SYUCOMM .
  methods SET_FILE
    importing
      !PF_FILE type ref to ZAPCMD_CL_FILE .
  methods SET_READONLY
    importing
      !PF_READONLY type XFELD default 'X' .
  methods FREE .
  methods GET_TITLE
    exporting
      !PF_TITLE1 type C
      !PF_TITLE2 type C
      !PF_TITLE3 type C
      !PF_TITLE4 type C .
  class-methods CLASS_CONSTRUCTOR .
  class-methods CALL_EDITOR
    importing
      !PF_FILE type ref to ZAPCMD_CL_FILE
      !PF_READONLY type XFELD .
protected section.
*"* protected components of class ZAPCMD_CL_EDITOR
*"* do not include other source files here!!!
private section.
*"* private components of class ZAPCMD_CL_EDITOR
*"* do not include other source files here!!!
ENDCLASS.



CLASS ZAPCMD_CL_EDITOR IMPLEMENTATION.


method CALL_EDITOR.

  CALL METHOD gf_instance->set_file
    EXPORTING
      pf_file = pf_file.

  CALL METHOD gf_instance->set_readonly
    EXPORTING
      pf_readonly = pf_readonly.

  CALL FUNCTION 'ZAPCMD_CALL_EDITOR'
    EXPORTING
      pf_editor = gf_instance.

*        call screen 200.

  CALL METHOD gf_instance->free.


endmethod.


method CLASS_CONSTRUCTOR.

  CREATE OBJECT gf_instance.

endmethod.


method CONSTRUCTOR.
endmethod.


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


method GET_TITLE.

*    pf_title1 = cf_file->full_name.
    pf_title1 = cf_file->name.

endmethod.


method SAVE.

  DATA lt_data TYPE zapcmd_tbl_string.
  DATA lf_is_modified TYPE i.

  DATA lf_data TYPE string.

  IF cf_readonly IS INITIAL.

    if cf_gui_edit is bound.

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
    endif.
    if cf_gui_sedit is bound.

    cf_gui_sedit->get_text(
      IMPORTING
        table                   = lt_data
        is_modified            = lf_is_modified
      EXCEPTIONS
        error_cntl_call_method = 1
        ERROR_DP               = 2
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
    endif.
  ENDIF.

endmethod.


method SET_FILE.

  cf_file = pf_file.

endmethod.


method SET_READONLY.

  cf_readonly = pf_readonly.

endmethod.


METHOD show.

  DATA lt_data TYPE zapcmd_tbl_string.

  IF cf_gui_edit IS INITIAL AND cf_gui_sedit IS INITIAL.

    DATA l_extension TYPE string.
    l_extension = cf_file->get_extension( ).
    IF STRLEN( l_extension ) >= 3 AND l_extension(3) = 'ABA'.

      CREATE OBJECT cf_gui_sedit type cl_gui_abapedit
        EXPORTING
        parent = pf_container.

      CALL METHOD cf_file->read_text
        IMPORTING
          pt_file = lt_data.

      cf_gui_sedit->set_text(
        EXPORTING
          table = lt_data
        EXCEPTIONS
          error_dp = 1
          error_dp_create = 2
          error_code_page = 3
      ).
      IF sy-subrc <> 0.
        MESSAGE ID sy-msgid TYPE sy-msgty NUMBER sy-msgno
                   WITH sy-msgv1 sy-msgv2 sy-msgv3 sy-msgv4.
      ENDIF.

    ELSE.

      CREATE OBJECT cf_gui_edit
        EXPORTING
          parent = pf_container
          wordwrap_mode = 0.

      cf_gui_edit->set_font_fixed( 1 ).

      CALL METHOD cf_file->read_text
        IMPORTING
          pt_file = lt_data.

      ct_file = zapcmd_cl_file=>text2stream( lt_data ).


      cf_gui_edit->set_textstream(
        EXPORTING
          text                   = ct_file
        EXCEPTIONS
          error_cntl_call_method = 1
          not_supported_by_gui   = 2
          OTHERS                 = 3
             ).
      IF sy-subrc <> 0.
        MESSAGE ID sy-msgid TYPE sy-msgty NUMBER sy-msgno
                   WITH sy-msgv1 sy-msgv2 sy-msgv3 sy-msgv4.
      ENDIF.

    ENDIF.

  ENDIF.

  IF cf_gui_edit IS BOUND.
    IF cf_readonly = 'X'.
      CALL METHOD cf_gui_edit->set_readonly_mode
        EXPORTING
          readonly_mode = 1.
    ELSE.
      CALL METHOD cf_gui_edit->set_readonly_mode
        EXPORTING
          readonly_mode = 0.
    ENDIF.
  ENDIF.
  IF cf_gui_sedit IS BOUND.
    IF cf_readonly = 'X'.
      CALL METHOD cf_gui_sedit->set_readonly_mode
        EXPORTING
          readonly_mode = 1.
    ELSE.
      CALL METHOD cf_gui_sedit->set_readonly_mode
        EXPORTING
          readonly_mode = 0.
    ENDIF.
  ENDIF.

*    CALL METHOD cl_gui_cfw=>flush.

ENDMETHOD.


METHOD user_command.

  DATA lf_is_modified TYPE i.
  DATA lf_answer TYPE char1.

  CASE e_ucomm.
    WHEN 'BACK' OR 'EXIT' OR 'REFRESH'.
      IF cf_readonly IS INITIAL.

        IF cf_gui_edit IS BOUND.
          CALL METHOD cf_gui_edit->get_textmodified_status
            IMPORTING
              status                 = lf_is_modified
            EXCEPTIONS
              error_cntl_call_method = 1
              OTHERS                 = 2.
          IF sy-subrc <> 0.
            MESSAGE ID sy-msgid TYPE sy-msgty NUMBER sy-msgno
                       WITH sy-msgv1 sy-msgv2 sy-msgv3 sy-msgv4.
          ENDIF.
        ENDIF.
        IF cf_gui_sedit IS BOUND.
          CALL METHOD cf_gui_sedit->get_textmodified_status
            IMPORTING
              status                 = lf_is_modified
            EXCEPTIONS
              error_cntl_call_method = 1
              OTHERS                 = 2.
          IF sy-subrc <> 0.
            MESSAGE ID sy-msgid TYPE sy-msgty NUMBER sy-msgno
                       WITH sy-msgv1 sy-msgv2 sy-msgv3 sy-msgv4.
          ENDIF.
        ENDIF.


        IF lf_is_modified <> 0.
          CALL FUNCTION 'POPUP_TO_CONFIRM'
            EXPORTING
              titlebar                    = 'Text verändert.'(300)
*               DIAGNOSE_OBJECT             = ' '
          text_question    = 'Soll der Text gespeichert werden?'(301)
*               TEXT_BUTTON_1               = 'Ja'(001)
*               ICON_BUTTON_1               = ' '
*               TEXT_BUTTON_2               = 'Nein'(002)
*               ICON_BUTTON_2               = ' '
*               DEFAULT_BUTTON              = '1'
*               DISPLAY_CANCEL_BUTTON       = 'X'
*               USERDEFINED_F1_HELP         = ' '
*               START_COLUMN                = 25
*               START_ROW                   = 6
*               POPUP_TYPE                  =
           IMPORTING
             answer                      = lf_answer
*             TABLES
*               PARAMETER                   =
*             EXCEPTIONS
*               TEXT_NOT_FOUND              = 1
*               OTHERS                      = 2
                    .
          IF sy-subrc <> 0.
            MESSAGE ID sy-msgid TYPE sy-msgty NUMBER sy-msgno
                    WITH sy-msgv1 sy-msgv2 sy-msgv3 sy-msgv4.
          ENDIF.

          IF lf_answer = '1'.
            CALL METHOD save.
          ENDIF.
          IF lf_answer = 'A'.
            e_ucomm = ''.
          ENDIF.

        ENDIF.

      ENDIF.
    WHEN 'SAVE'.
      CALL METHOD save.


  ENDCASE.

ENDMETHOD.
ENDCLASS.
