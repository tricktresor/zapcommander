CLASS zapcmd_cl_commander DEFINITION
  PUBLIC
  FINAL
  CREATE PUBLIC .

*"* public components of class ZAPCMD_CL_COMMANDER
*"* do not include other source files here!!!
  PUBLIC SECTION.
    TYPE-POOLS abap .

    DATA cf_gui_splitter_container TYPE REF TO cl_gui_easy_splitter_container .
    DATA cf_container_left TYPE REF TO cl_gui_container .
    DATA cf_container_right TYPE REF TO cl_gui_container .
    DATA cf_dirname TYPE REF TO string .
    DATA cf_filesleft TYPE REF TO zapcmd_cl_filelist_left .
    DATA cf_filesright TYPE REF TO zapcmd_cl_filelist_right .
    DATA cf_activelist TYPE REF TO zapcmd_cl_filelist .

    METHODS show
      IMPORTING
        !pf_container TYPE REF TO cl_gui_container .
    METHODS constructor
      IMPORTING
        !pf_left_type  TYPE syucomm DEFAULT 'FRONTEND'
        !pf_left_dir   TYPE string OPTIONAL
        !pf_right_type TYPE syucomm DEFAULT 'FRONTEND'
        !pf_right_dir  TYPE string OPTIONAL
        !pf_dirname    TYPE REF TO string OPTIONAL .
    METHODS user_command
      IMPORTING
        !e_ucomm TYPE syucomm .
    METHODS handle_activate
          FOR EVENT set_active OF zapcmd_cl_filelist
      IMPORTING
          !sender .
  PROTECTED SECTION.
*"* protected components of class ZAPCMD_CL_COMMANDER
*"* do not include other source files here!!!
  PRIVATE SECTION.
*"* private components of class ZAPCMD_CL_COMMANDER
*"* do not include other source files here!!!
ENDCLASS.



CLASS ZAPCMD_CL_COMMANDER IMPLEMENTATION.


  METHOD constructor.

    CREATE OBJECT cf_filesleft
      EXPORTING
        pf_type = pf_left_type
        pf_dir  = pf_left_dir.

    CREATE OBJECT cf_filesright
      EXPORTING
        pf_type = pf_right_type
        pf_dir  = pf_right_dir.

    cf_dirname = pf_dirname.
    SET HANDLER handle_activate FOR ALL INSTANCES.

  ENDMETHOD.


  METHOD handle_activate.

    cf_activelist = sender.
    FIELD-SYMBOLS <dirname> TYPE string.
    ASSIGN cf_dirname->* TO <dirname>.
    <dirname> = cf_activelist->cf_ref_dir->full_name.

  ENDMETHOD.


  METHOD show.

    IF cf_gui_splitter_container IS INITIAL.
      CREATE OBJECT cf_gui_splitter_container
        EXPORTING
          parent      = pf_container
          orientation = cf_gui_splitter_container->orientation_horizontal.
* get the containers of the splitter control
      cf_container_left  = cf_gui_splitter_container->top_left_container.
      cf_container_right = cf_gui_splitter_container->bottom_right_container.
    ENDIF.

    cf_filesleft->show( cf_container_left ).
    cf_filesright->show( cf_container_right ).

    IF cf_filesleft->cf_active = abap_true.
      cf_activelist = cf_filesleft.
    ELSE.
      cf_activelist = cf_filesright.
    ENDIF.

    FIELD-SYMBOLS <dirname> TYPE string.
    ASSIGN cf_dirname->* TO <dirname>.
    <dirname> = cf_activelist->cf_ref_dir->full_name.

  ENDMETHOD.


  METHOD user_command.

    DATA lf_gui_comp TYPE REF TO cl_gui_control.

    cl_gui_control=>get_focus(
      IMPORTING
        control           = lf_gui_comp
      EXCEPTIONS
        cntl_error        = 1
        cntl_system_error = 2
        OTHERS            = 3 ).
    IF sy-subrc = 0.
      cf_filesleft->check_active( lf_gui_comp ).
      cf_filesright->check_active( lf_gui_comp ).
    ENDIF.

    IF cf_filesleft->cf_active = 'X'.
      cf_activelist = cf_filesleft.
    ELSE.
      cf_activelist = cf_filesright.
    ENDIF.

    FIELD-SYMBOLS <dirname> TYPE string.
    ASSIGN cf_dirname->* TO <dirname>.
    <dirname> = cf_activelist->cf_ref_dir->full_name.

    DATA lt_files TYPE zapcmd_tbl_filelist.
    DATA lf_file TYPE REF TO zapcmd_cl_knot.
    DATA lf_editorfile TYPE REF TO zapcmd_cl_file.
    DATA lf_destdir TYPE REF TO zapcmd_cl_dir.

    DATA lf_answer TYPE c.

    IF cf_filesleft->cf_active = 'X'.
      lt_files   = cf_filesleft->get_files( ).
      lf_destdir = cf_filesright->get_dir( ).
    ELSE.
      lt_files   = cf_filesright->get_files( ).
      lf_destdir = cf_filesleft->get_dir( ).
    ENDIF.

    CASE e_ucomm.

      WHEN 'COPY'.
        IF cf_filesleft->cf_active = 'X'.
          cf_filesright->copy(
              pt_files   = lt_files
              pf_destdir = lf_destdir ).
        ELSE.
          cf_filesleft->copy(
           pt_files   = lt_files
           pf_destdir = lf_destdir ).
        ENDIF.

      WHEN 'DEL'.
        cf_activelist->delete( lt_files ).

      WHEN 'MOVE'.

        IF cf_filesleft->cf_active = 'X'.
          cf_filesright->copy(
              pt_files   = lt_files
              pf_destdir = lf_destdir ).
        ELSE.
          cf_filesleft->copy(
           pt_files   = lt_files
           pf_destdir = lf_destdir ).
        ENDIF.

        cf_activelist->delete( lt_files ).

      WHEN 'NEWDIR'.

        IF cf_filesleft->cf_active = 'X'.
          lf_destdir = cf_filesleft->get_dir( ).
        ELSE.
          lf_destdir = cf_filesright->get_dir( ).
        ENDIF.

        DATA lf_dirname TYPE filename-fileextern.
        DATA lf_string TYPE string.

        CALL FUNCTION 'POPUP_TO_GET_VALUE'
          EXPORTING
            fieldname           = 'FILEEXTERN'
            tabname             = 'FILENAME'
            titel               = 'Neues Verzeichnis anlegen:'(005)
            valuein             = ''
          IMPORTING
            answer              = lf_answer
            valueout            = lf_dirname
          EXCEPTIONS
            fieldname_not_found = 1
            OTHERS              = 2.
        IF sy-subrc <> 0.
          MESSAGE ID sy-msgid TYPE sy-msgty NUMBER sy-msgno
            WITH sy-msgv1 sy-msgv2 sy-msgv3 sy-msgv4.
          RETURN.
        ENDIF.

        IF lf_answer <> 'A'.
          lf_string = lf_dirname.
          lf_destdir->create_dir( lf_string ).
        ENDIF.

        IF cf_filesleft->cf_active = 'X'.
          cf_filesleft->reload_dir( ).
          cf_filesleft->refresh( ).
        ELSE.
          cf_filesright->reload_dir( ).
          cf_filesright->refresh( ).
        ENDIF.

      WHEN 'SHOW'.

        READ TABLE lt_files INDEX 1
        INTO lf_file.

        lf_editorfile ?= lf_file.

        zapcmd_cl_editor=>call_editor(
          pf_file     = lf_editorfile
          pf_readonly = 'X' ).

      WHEN 'EDIT'.

        READ TABLE lt_files INDEX 1
        INTO lf_file.

        lf_editorfile ?= lf_file.

        zapcmd_cl_editor=>call_editor(
          pf_file     = lf_editorfile
          pf_readonly = '' ).

      WHEN 'BACK'.

        IF cf_filesleft->cf_active = 'X'.
          cf_filesleft->undo( ).
        ELSE.
          cf_filesright->undo( ).
        ENDIF.

      WHEN 'EXIT'.
        DATA lf_id TYPE indx_srtfd.
        CONCATENATE 'ZAPCMD' sy-uname INTO lf_id.
        DATA lf_dir TYPE REF TO zapcmd_cl_dir.
        lf_dir = cf_filesleft->get_dir( ).
        DATA l_left TYPE zapcmd_t_dir.

        IF lf_dir->server_area = 'P'.
          l_left-type = zapcmd_cl_dir=>co_frontend.
        ELSE.
          l_left-type = zapcmd_cl_dir=>co_applserv.
        ENDIF.
        l_left-dir = lf_dir->full_name.

        lf_dir = cf_filesright->get_dir( ).

        DATA l_right TYPE zapcmd_t_dir.

        IF lf_dir->server_area = 'P'.
          l_right-type = zapcmd_cl_dir=>co_frontend.
        ELSE.
          l_right-type = zapcmd_cl_dir=>co_applserv.
        ENDIF.
        l_right-dir = lf_dir->full_name.

        EXPORT
           left = l_left
           right = l_right
        TO DATABASE indx(zc)
        ID lf_id.

      WHEN 'INFO'.

        DATA lt_links TYPE STANDARD TABLE OF tline.
        CALL FUNCTION 'DOKU_OBJECT_SHOW'
          EXPORTING
            dokclass         = 'TX'
            dokname          = 'ZAPCMD01'
          TABLES
            links            = lt_links
          EXCEPTIONS
            object_not_found = 1
            sapscript_error  = 2
            OTHERS           = 3.
        IF sy-subrc <> 0.
          MESSAGE ID sy-msgid TYPE 'I' NUMBER sy-msgno DISPLAY LIKE sy-msgty
           WITH sy-msgv1 sy-msgv2 sy-msgv3 sy-msgv4.
          RETURN.
        ENDIF.
    ENDCASE.

  ENDMETHOD.
ENDCLASS.
