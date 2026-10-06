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

    "! Stores both panes' directories for the next start
    METHODS save_last_dirs.
    "! Only frontend and application server can be restored; for other
    "! areas (RFC, custom factories) the previously stored entry is kept
    METHODS set_last_dir
      IMPORTING
        !io_dir TYPE REF TO zapcmd_cl_dir
      CHANGING
        !cs_dir TYPE zapcmd_t_dir.
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

    DATA li_user_exit TYPE REF TO zapcmd_if_user_exit.

    IF cf_gui_splitter_container IS INITIAL.

      li_user_exit = zapcmd_cl_user_exit_factory=>get( ).
      IF li_user_exit IS BOUND.
        cf_gui_splitter_container = li_user_exit->commander_create_splitter( pf_container ).
      ENDIF.

      IF cf_gui_splitter_container IS NOT BOUND.
        CREATE OBJECT cf_gui_splitter_container
          EXPORTING
            parent      = pf_container
            orientation = cf_gui_splitter_container->orientation_horizontal.
      ENDIF.

      " get the containers of the splitter control
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

    DATA lf_gui_comp  TYPE REF TO cl_gui_control.
    DATA li_user_exit TYPE REF TO zapcmd_if_user_exit.

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

        " copy( ) activates the target list, so remember the source list
        DATA lo_sourcelist TYPE REF TO zapcmd_cl_filelist.
        DATA lt_copied TYPE zapcmd_tbl_filelist.
        lo_sourcelist = cf_activelist.

        IF cf_filesleft->cf_active = 'X'.
          cf_filesright->copy(
            EXPORTING
              pt_files   = lt_files
              pf_destdir = lf_destdir
            IMPORTING
              et_copied  = lt_copied ).
        ELSE.
          cf_filesleft->copy(
            EXPORTING
              pt_files   = lt_files
              pf_destdir = lf_destdir
            IMPORTING
              et_copied  = lt_copied ).
        ENDIF.

        " only delete what was copied successfully
        IF lt_copied IS NOT INITIAL.
          lo_sourcelist->delete( lt_copied ).
        ELSE.
          lo_sourcelist->refresh( ).
        ENDIF.

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
            titel               = 'Create new directory:'(005)
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

        " only files can be shown in the editor, not directories
        CLEAR lf_editorfile.
        READ TABLE lt_files INDEX 1
        INTO lf_file.
        IF sy-subrc = 0.
          TRY.
              lf_editorfile ?= lf_file.
            CATCH cx_sy_move_cast_error.
              CLEAR lf_editorfile.
          ENDTRY.
        ENDIF.
        IF lf_editorfile IS NOT BOUND.
          MESSAGE 'Please select a file'(006) TYPE 'S' DISPLAY LIKE 'E'.
          RETURN.
        ENDIF.

        zapcmd_cl_editor=>call_editor(
          pf_file     = lf_editorfile
          pf_readonly = 'X' ).

      WHEN 'EDIT'.

        " only files can be shown in the editor, not directories
        CLEAR lf_editorfile.
        READ TABLE lt_files INDEX 1
        INTO lf_file.
        IF sy-subrc = 0.
          TRY.
              lf_editorfile ?= lf_file.
            CATCH cx_sy_move_cast_error.
              CLEAR lf_editorfile.
          ENDTRY.
        ENDIF.
        IF lf_editorfile IS NOT BOUND.
          MESSAGE 'Please select a file'(006) TYPE 'S' DISPLAY LIKE 'E'.
          RETURN.
        ENDIF.

        zapcmd_cl_editor=>call_editor(
          pf_file     = lf_editorfile
          pf_readonly = '' ).

      WHEN 'BACK'.

        IF cf_filesleft->cf_active = 'X'.
          cf_filesleft->undo( ).
        ELSE.
          cf_filesright->undo( ).
        ENDIF.

      WHEN 'SWITCH'.
        IF cf_filesleft->cf_active = abap_true.
          cf_filesright->activate( ).
        ELSE.
          cf_filesleft->activate( ).
        ENDIF.

      WHEN 'EXIT' OR 'ABORT'.
        save_last_dirs( ).

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

      WHEN OTHERS.

        li_user_exit = zapcmd_cl_user_exit_factory=>get( ).
        IF li_user_exit IS BOUND.
          li_user_exit->commander_user_command( iv_function_code  = e_ucomm
                                                io_filelist_left  = cf_filesleft
                                                io_filelist_right = cf_filesright ).
        ENDIF.

    ENDCASE.

  ENDMETHOD.


  METHOD save_last_dirs.

    DATA lf_id TYPE indx_srtfd.
    DATA l_left TYPE zapcmd_t_dir.
    DATA l_right TYPE zapcmd_t_dir.

    CONCATENATE 'ZAPCMD' sy-uname INTO lf_id.

    IMPORT left = l_left
           right = l_right
      FROM DATABASE indx(zc)
      ID lf_id.                                         "#EC CI_SUBRC

    set_last_dir( EXPORTING io_dir = cf_filesleft->get_dir( )
                  CHANGING  cs_dir = l_left ).
    set_last_dir( EXPORTING io_dir = cf_filesright->get_dir( )
                  CHANGING  cs_dir = l_right ).

    EXPORT
       left = l_left
       right = l_right
    TO DATABASE indx(zc)
    ID lf_id.

  ENDMETHOD.


  METHOD set_last_dir.

    IF io_dir IS NOT BOUND.
      RETURN.
    ENDIF.

    CASE io_dir->server_area.
      WHEN zapcmd_cl_knot=>co_area_frontend.
        cs_dir-type = zapcmd_cl_dir=>co_frontend.
        cs_dir-dir  = io_dir->full_name.
      WHEN zapcmd_cl_knot=>co_area_applserv.
        cs_dir-type = zapcmd_cl_dir=>co_applserv.
        cs_dir-dir  = io_dir->full_name.
    ENDCASE.

  ENDMETHOD.


ENDCLASS.
