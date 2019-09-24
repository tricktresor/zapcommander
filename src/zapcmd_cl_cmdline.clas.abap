class ZAPCMD_CL_CMDLINE definition
  public
  final
  create public .

*"* public components of class ZAPCMD_CL_CMDLINE
*"* do not include other source files here!!!
public section.

   data cf_gui_textedit type ref to cl_gui_textedit.
    data cf_gui_dirname type ref to cl_gui_textedit.
    data ct_line type zapcmd_tbl_string.
    data cf_commandlinenumber type i.
    DATA cf_dirname TYPE string.
    data cf_cmdline type ref to string.
    data cf_filelist type ref to zapcmd_refref_filelist.

    methods show1
      importing
         !PF_CONTAINER type ref to CL_GUI_CONTAINER.

    methods SHOW
       importing
         !PF_CONTAINER type ref to CL_GUI_CONTAINER.

*    methods HANDLE_DOUBLECLICK
*      for event DOUBLE_CLICK of CL_GUI_ALV_GRID.

     methods HANDLE_DOUBLECLICK
      for event set_active of zapcmd_cl_filelist.

    methods constructor
      importing
        pf_cmdline type ref to string
        pf_dir type ref to zapcmd_refref_filelist.

    methods execute
      importing pf_cmdline type string
                pf_filelist type ref to ZApcmd_CL_filelist.

*    methods handle_dblclick for event dblclick of cl_gui_textedit.

    methods user_command
      importing
        e_ucomm type syucomm.
protected section.
*"* protected components of class ZAPCMD_CL_CMDLINE
*"* do not include other source files here!!!
private section.
*"* private components of class ZAPCMD_CL_CMDLINE
*"* do not include other source files here!!!
ENDCLASS.



CLASS ZAPCMD_CL_CMDLINE IMPLEMENTATION.


method CONSTRUCTOR.

    cf_cmdline = pf_cmdline.
    cf_filelist = pf_dir.

endmethod.


method EXECUTE.

    IF pf_cmdline IS NOT INITIAL.
      DATA ls_dir TYPE REF TO zapcmd_cl_dir.
      DATA lf_cmdline TYPE string.
      ls_dir ?= pf_filelist->get_dir( ).
      DATA lf_cd(3) TYPE c.
      IF STRLEN( pf_cmdline ) > 2.
        lf_cd = pf_cmdline(3).
        CONDENSE lf_cd.
        TRANSLATE lf_cd TO UPPER CASE.
      ENDIF.
      IF lf_cd = 'CD'.
        DATA lf_dirname TYPE string.
        lf_dirname = pf_cmdline+3.
        pf_filelist->load_dir(
          EXPORTING
           pf_dir = lf_dirname
        ).
        RETURN.
      ENDIF.
      IF ls_dir->server_area = zapcmd_cl_knot=>co_area_frontend.
        DATA lf_params TYPE string.
        CONCATENATE '/k' pf_cmdline
          INTO lf_params SEPARATED BY space.
        CALL METHOD cl_gui_frontend_services=>execute
          EXPORTING
*            DOCUMENT               = 'c:\WINDOWS\system32\cmd.exe'
            application            = 'c:\WINDOWS\system32\cmd.exe'
            parameter              = lf_params
            default_directory      = ls_dir->full_name
*            MAXIMIZED              =
*            MINIMIZED              =
            synchronous            = 'X'
*            OPERATION              = 'OPEN'
          EXCEPTIONS
            cntl_error             = 1
            error_no_gui           = 2
            bad_parameter          = 3
            file_not_found         = 4
            path_not_found         = 5
            file_extension_unknown = 6
            error_execute_failed   = 7
            synchronous_failed     = 8
            not_supported_by_gui   = 9
            OTHERS                 = 10
                .
        IF sy-subrc <> 0.
          MESSAGE ID sy-msgid TYPE 'I' NUMBER sy-msgno
                     WITH sy-msgv1 sy-msgv2 sy-msgv3 sy-msgv4.
        ENDIF.

      ELSE.

        DATA lf_line TYPE string.

        DATA ls_filedescr TYPE zapcmd_file_descr.
*      ls_filedescr = pf_filelist->cf_ref_dir->get_info( ).
        lf_line = ls_dir->full_name.
        CONCATENATE lf_line '>' pf_cmdline INTO lf_line.
        APPEND lf_line TO ct_line.
        cf_commandlinenumber = lines( ct_line ).
        DATA lt_output TYPE zapcmd_tbl_string.
*        concatenate 'cd' ls_dir->full_name
*          into lf_cmdline separated by space.
*
*          zavl_cl_knot=>exec_server(
*          EXPORTING
*            pf_command = lf_cmdline
*          IMPORTING
*            ptx_output = lt_output
*            ).

*      concatenate ls_dir->full_name ls_dir->separator pf_cmdline
*        into lf_cmdline.
        lf_cmdline = pf_cmdline.
        zapcmd_cl_knot=>exec_server(
          EXPORTING
            pf_command = lf_cmdline
            pf_dir = ls_dir->full_name
          IMPORTING
            ptx_output = lt_output
            ).
        APPEND LINES OF lt_output TO ct_line.

*       CALL SCREEN 300 STARTING AT 10 3 ENDING AT 100 27.
        CALL FUNCTION 'ZAPCMD_CALL_CMDLINE'
          EXPORTING
            pf_cmdline       = me
                  .

*      cf_gui_textedit->SET_TEXT_AS_R3TABLE(
*        exporting
*          table = ct_line
*        ).

      ENDIF.
    ENDIF.

endmethod.


method HANDLE_DOUBLECLICK.
    FIELD-SYMBOLS: <filelist> TYPE REF TO zapcmd_cl_filelist.
    ASSIGN cf_filelist->* TO <filelist>.
    cf_dirname = <filelist>->cf_ref_dir->full_name.
    cf_gui_dirname->set_textstream(
      EXPORTING
        TEXT                   = cf_dirname
      EXCEPTIONS
        ERROR_CNTL_CALL_METHOD = 1
        NOT_SUPPORTED_BY_GUI   = 2
        others                 = 3
           ).
    IF sy-subrc <> 0.
     MESSAGE ID SY-MSGID TYPE SY-MSGTY NUMBER SY-MSGNO
                WITH SY-MSGV1 SY-MSGV2 SY-MSGV3 SY-MSGV4.
    ENDIF.

**    DATA ct_dirname TYPE TABLE OF string.
*    refresh ct_dirname.
*    FIELD-SYMBOLS: <filelist> TYPE REF TO zapcmd_cl_filelist.
*    ASSIGN cf_filelist->* TO <filelist>.
*    APPEND <filelist>->cf_ref_dir->full_name TO ct_dirname.
*    cf_gui_dirname->set_text_as_r3table(
*      EXPORTING
*        table = ct_dirname
*      ).
*    CALL METHOD cl_gui_cfw=>flush.
*    IF sy-subrc NE 0.
*    ENDIF.

endmethod.


method SHOW.

*      importing
*        !PF_CONTAINER type ref to CL_GUI_CONTAINER .
  IF cf_gui_textedit IS INITIAL.
    CREATE OBJECT cf_gui_textedit
      EXPORTING
        style = 0
        wordwrap_mode = 0
        parent  = pf_container.
    cf_gui_textedit->set_statusbar_mode( 0 ).
    cf_gui_textedit->set_toolbar_mode( 0 ).
*      cf_gui_textedit->SET_MODE( 0 ).
*      set handler handle_dblclick for cf_gui_textedit.
    cf_gui_textedit->register_event_dblclick(
      register = 1 ).

  ENDIF.

  CALL METHOD cf_gui_textedit->set_readonly_mode
    EXPORTING
      readonly_mode = 1.

  DATA lf_stream TYPE string.
  lf_stream = zapcmd_cl_file=>text2stream( ct_line ).

  cf_gui_textedit->set_textstream(
    EXPORTING
      text                   = lf_stream
    EXCEPTIONS
      error_cntl_call_method = 1
      not_supported_by_gui   = 2
      OTHERS                 = 3
         ).
  IF sy-subrc <> 0.
    MESSAGE ID sy-msgid TYPE sy-msgty NUMBER sy-msgno
               WITH sy-msgv1 sy-msgv2 sy-msgv3 sy-msgv4.
  ENDIF.


*    cf_gui_textedit->set_text_as_r3table(
*      EXPORTING
*        table = ct_line
*      ).

  IF cf_commandlinenumber IS INITIAL.
    cf_commandlinenumber = 1.
  ENDIF.

  cf_gui_textedit->go_to_line(
    EXPORTING
      line                   = cf_commandlinenumber
*      EXCEPTIONS
*        ERROR_CNTL_CALL_METHOD = 1
*        others                 = 2
         ).
  IF sy-subrc <> 0.
*     MESSAGE ID SY-MSGID TYPE SY-MSGTY NUMBER SY-MSGNO
*                WITH SY-MSGV1 SY-MSGV2 SY-MSGV3 SY-MSGV4.
  ENDIF.

  cf_gui_textedit->set_first_visible_line(
    EXPORTING
      line                   = cf_commandlinenumber
*      EXCEPTIONS
*        ERROR_CNTL_CALL_METHOD = 1
*        others                 = 2
         ).
  IF sy-subrc <> 0.
*     MESSAGE ID SY-MSGID TYPE SY-MSGTY NUMBER SY-MSGNO
*                WITH SY-MSGV1 SY-MSGV2 SY-MSGV3 SY-MSGV4.
  ENDIF.

endmethod.


method SHOW1.

*      importing
*        !PF_CONTAINER type ref to CL_GUI_CONTAINER .
    IF cf_gui_dirname IS INITIAL.
      CREATE OBJECT cf_gui_dirname
        EXPORTING
          style = 0
          wordwrap_mode = 0
          parent  = pf_container.
      cf_gui_dirname->set_statusbar_mode( 0 ).
      cf_gui_dirname->set_toolbar_mode( 0 ).

      set handler handle_doubleclick for all instances.

    ENDIF.

    CALL METHOD cf_gui_dirname->set_readonly_mode
      EXPORTING
        readonly_mode = 1.

      FIELD-SYMBOLS: <filelist> TYPE REF TO zapcmd_cl_filelist.
    ASSIGN cf_filelist->* TO <filelist>.
    cf_dirname = <filelist>->cf_ref_dir->full_name.
    cf_gui_dirname->set_textstream(
      EXPORTING
        TEXT                   = cf_dirname
      EXCEPTIONS
        ERROR_CNTL_CALL_METHOD = 1
        NOT_SUPPORTED_BY_GUI   = 2
        others                 = 3
           ).
    IF sy-subrc <> 0.
     MESSAGE ID SY-MSGID TYPE SY-MSGTY NUMBER SY-MSGNO
                WITH SY-MSGV1 SY-MSGV2 SY-MSGV3 SY-MSGV4.
    ENDIF.

**    DATA ct_dirname TYPE TABLE OF string.
*    refresh ct_dirname.
*    FIELD-SYMBOLS: <filelist> TYPE REF TO zapcmd_cl_filelist.
*    ASSIGN cf_filelist->* TO <filelist>.
*    APPEND <filelist>->cf_ref_dir->full_name TO ct_dirname.
*    cf_gui_dirname->set_text_as_r3table(
*      EXPORTING
*        table = ct_dirname
*      ).
*    CALL METHOD cl_gui_cfw=>flush.
*    IF sy-subrc NE 0.
*    ENDIF.

endmethod.


method USER_COMMAND.

    IF e_ucomm IS INITIAL.
      FIELD-SYMBOLS: <cmdline> TYPE string.
      ASSIGN cf_cmdline->* TO <cmdline>.
      FIELD-SYMBOLS: <filelist> TYPE REF TO zapcmd_cl_filelist.
      ASSIGN cf_filelist->* TO <filelist>.

      execute(
        pf_cmdline = <cmdline>
        pf_filelist = <filelist>
        ).

      CLEAR <cmdline>.

    ENDIF.

endmethod.
ENDCLASS.
