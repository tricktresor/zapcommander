class ZAPCMD_CL_COMMANDER definition
  public
  final
  create public .

*"* public components of class ZAPCMD_CL_COMMANDER
*"* do not include other source files here!!!
public section.
  type-pools ABAP .

  data CF_GUI_SPLITTER_CONTAINER type ref to CL_GUI_EASY_SPLITTER_CONTAINER .
  data CF_CONTAINER_LEFT type ref to CL_GUI_CONTAINER .
  data CF_CONTAINER_RIGHT type ref to CL_GUI_CONTAINER .
  data CF_DIRNAME type ref to STRING .
  data CF_FILESLEFT type ref to ZAPCMD_CL_FILELIST .
  data CF_FILESRIGHT type ref to ZAPCMD_CL_FILELIST .
  data CF_ACTIVELIST type ref to ZAPCMD_CL_FILELIST .

  methods SHOW
    importing
      !PF_CONTAINER type ref to CL_GUI_CONTAINER .
  methods CONSTRUCTOR
    importing
      !PF_LEFT_TYPE type SYUCOMM default 'FRONTEND'
      !PF_LEFT_DIR type STRING optional
      !PF_RIGHT_TYPE type SYUCOMM default 'FRONTEND'
      !PF_RIGHT_DIR type STRING optional
      !PF_DIRNAME type ref to STRING optional .
  methods USER_COMMAND
    importing
      !E_UCOMM type SYUCOMM .
  methods HANDLE_ACTIVATE
    for event SET_ACTIVE of ZAPCMD_CL_FILELIST
    importing
      !SENDER .
protected section.
*"* protected components of class ZAPCMD_CL_COMMANDER
*"* do not include other source files here!!!
private section.
*"* private components of class ZAPCMD_CL_COMMANDER
*"* do not include other source files here!!!
ENDCLASS.



CLASS ZAPCMD_CL_COMMANDER IMPLEMENTATION.


method CONSTRUCTOR.

     CREATE OBJECT cf_filesleft
      EXPORTING
        pf_type = pf_left_type
        pf_dir  = pf_left_dir.
*    set handler cf_filesleft->handle_activate.
    CREATE OBJECT cf_filesright
      EXPORTING
        pf_type = pf_right_type
        pf_dir  = pf_right_dir.
*    set handler cf_filesright->handle_activate.

    cf_dirname = pf_dirname.
    set handler handle_activate for all instances.

endmethod.


method HANDLE_ACTIVATE.

    cf_activelist = sender.
    field-symbols <dirname> type string.
    assign cf_dirname->* to <dirname>.
    <dirname> = cf_activelist->cf_ref_dir->full_name.

endmethod.


method SHOW.

    IF cf_gui_splitter_container IS INITIAL.
      CREATE OBJECT cf_gui_splitter_container
        EXPORTING
          parent      = pf_container
          orientation = cf_gui_splitter_container->orientation_horizontal.
* get the containers of the splitter control
      cf_container_left =
        cf_gui_splitter_container->top_left_container.
      cf_container_right =
          cf_gui_splitter_container->bottom_right_container.


    ENDIF.

    CALL METHOD cf_filesleft->show
      EXPORTING
        pf_container = cf_container_left.
    CALL METHOD cf_filesright->show
      EXPORTING
        pf_container = cf_container_right.

    if cf_filesleft->cf_active = abap_true.
      cf_activelist = cf_filesleft.
    else.
      cf_activelist = cf_filesright.
    endif.
    field-symbols <dirname> type string.
    assign cf_dirname->* to <dirname>.
    <dirname> = cf_activelist->cf_ref_dir->full_name.

endmethod.


METHOD user_command.

  DATA lf_gui_comp TYPE REF TO cl_gui_control.

  CALL METHOD cl_gui_control=>get_focus
    IMPORTING
      control           = lf_gui_comp
    EXCEPTIONS
      cntl_error        = 1
      cntl_system_error = 2
      OTHERS            = 3.
  IF sy-subrc <> 0.
*      MESSAGE ID SY-MSGID TYPE SY-MSGTY NUMBER SY-MSGNO
*                 WITH SY-MSGV1 SY-MSGV2 SY-MSGV3 SY-MSGV4.
  ENDIF.

  CALL METHOD cf_filesleft->check_active
    EXPORTING
      pf_control = lf_gui_comp.

  CALL METHOD cf_filesright->check_active
    EXPORTING
      pf_control = lf_gui_comp.

  IF cf_filesleft->cf_active = 'X'.
    cf_activelist = cf_filesleft.
  ELSE.
    cf_activelist = cf_filesright.
  ENDIF.
  FIELD-SYMBOLS <dirname> TYPE string.
  ASSIGN cf_dirname->* TO <dirname>.
  <dirname> = cf_activelist->cf_ref_dir->full_name.

*    data lt_file type ZAPCMD_TBL_XSTRING.
*  DATA lf_filetype TYPE char10.
  DATA lt_files TYPE zapcmd_tbl_filelist.
  DATA lf_file TYPE REF TO zapcmd_cl_knot.
  DATA lf_editorfile TYPE REF TO zapcmd_cl_file.
*    data lf_destfile type ref to ZAPCMD_CL_FILE.
  DATA lf_destdir TYPE REF TO zapcmd_cl_dir.

  DATA lf_answer TYPE c.

  IF cf_filesleft->cf_active = 'X'.
    lt_files = cf_filesleft->get_files( ).
    lf_destdir = cf_filesright->get_dir( ).
  ELSE.
    lt_files = cf_filesright->get_files( ).
    lf_destdir = cf_filesleft->get_dir( ).
  ENDIF.

  CASE e_ucomm.

    WHEN 'COPY'.


     IF cf_filesleft->cf_active = 'X'.
      CALL METHOD cf_filesright->copy
        EXPORTING
          pt_files   = lt_files
          pf_destdir = lf_destdir.
      ELSE.
         CALL METHOD cf_filesleft->copy
        EXPORTING
          pt_files   = lt_files
          pf_destdir = lf_destdir.

      endif.


    WHEN 'DEL'.

      CALL METHOD cf_activelist->delete
        EXPORTING
          pt_files = lt_files.


    WHEN 'MOVE'.

       IF cf_filesleft->cf_active = 'X'.
      CALL METHOD cf_filesright->copy
        EXPORTING
          pt_files   = lt_files
          pf_destdir = lf_destdir.
      ELSE.
         CALL METHOD cf_filesleft->copy
        EXPORTING
          pt_files   = lt_files
          pf_destdir = lf_destdir.
      endif.

      CALL METHOD cf_activelist->delete
        EXPORTING
          pt_files = lt_files.

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
      ENDIF.

      IF lf_answer <> 'A'.

        lf_string = lf_dirname.

        CALL METHOD lf_destdir->create_dir
          EXPORTING
            pf_filename = lf_string.
      ENDIF.

      IF cf_filesleft->cf_active = 'X'.
        CALL METHOD cf_filesleft->reload_dir.
        CALL METHOD cf_filesleft->refresh.
      ELSE.
        CALL METHOD cf_filesright->reload_dir.
        CALL METHOD cf_filesright->refresh.
      ENDIF.

    WHEN 'SHOW'.

      READ TABLE lt_files INDEX 1
        INTO lf_file.

      lf_editorfile ?= lf_file.

      zapcmd_cl_editor=>call_editor(
        pf_file = lf_editorfile
        pf_readonly = 'X'
        ).

*        CALL METHOD gf_editor->set_file
*          EXPORTING
*            pf_file = lf_editorfile.
*
*        CALL METHOD gf_editor->set_readonly
*          EXPORTING
*            pf_readonly = 'X'.
*
*        CALL SCREEN 200.
*
*        CALL METHOD gf_editor->free.


    WHEN 'EDIT'.

      READ TABLE lt_files INDEX 1
        INTO lf_file.

      lf_editorfile ?= lf_file.

*      data lt_text type table of string.
*      lf_editorfile->read_text(
*        importing
*        pt_file = lt_text
*      ).
*      EDITOR-CALL FOR lt_text.
*
** Bei User-Wahl beenden
*      IF sy-ucomm EQ 'WB_SAVE'.
*        lf_editorfile->write_text(
*          pt_file = lt_text
*        ).
*      ENDIF.

      zapcmd_cl_editor=>call_editor(
        pf_file = lf_editorfile
        pf_readonly = ''
        ).


    WHEN 'BACK'.

      IF cf_filesleft->cf_active = 'X'.
        CALL METHOD cf_filesleft->undo.

      ELSE.
        CALL METHOD cf_filesright->undo.

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
      IF sy-subrc = 0.
      ENDIF.
    WHEN 'INFO'.

      DATA lt_links TYPE STANDARD TABLE OF tline.
      CALL FUNCTION 'DOKU_OBJECT_SHOW'
        EXPORTING
          dokclass               = 'TX'
*           DOKLANGU               = SY-LANGU
          dokname                = 'ZAPCMD01'
*           DOKTITLE               = ' '
        TABLES
          links                  = lt_links
         EXCEPTIONS
           OBJECT_NOT_FOUND       = 1
           SAPSCRIPT_ERROR        = 2
           OTHERS                 = 3
                .
      IF sy-subrc <> 0.
        MESSAGE ID SY-MSGID TYPE 'I' NUMBER SY-MSGNO DISPLAY LIKE SY-MSGTY
         WITH SY-MSGV1 SY-MSGV2 SY-MSGV3 SY-MSGV4.
      ENDIF.



  ENDCASE.

ENDMETHOD.
ENDCLASS.
