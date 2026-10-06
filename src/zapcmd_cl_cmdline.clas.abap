CLASS zapcmd_cl_cmdline DEFINITION
  PUBLIC
  FINAL
  CREATE PUBLIC .

*"* public components of class ZAPCMD_CL_CMDLINE
*"* do not include other source files here!!!
  PUBLIC SECTION.

    DATA cf_gui_textedit TYPE REF TO cl_gui_textedit.
    DATA cf_gui_dirname TYPE REF TO cl_gui_textedit.
    DATA ct_line TYPE zapcmd_tbl_string.
    DATA cf_commandlinenumber TYPE i.
    DATA cf_dirname TYPE string.
    DATA cf_cmdline TYPE REF TO string.
    DATA cf_filelist TYPE REF TO zapcmd_refref_filelist.

    METHODS show1
      IMPORTING
        !pf_container TYPE REF TO cl_gui_container.

    METHODS show
      IMPORTING
        !pf_container TYPE REF TO cl_gui_container.

*    methods HANDLE_DOUBLECLICK
*      for event DOUBLE_CLICK of CL_GUI_ALV_GRID.

    METHODS handle_doubleclick
        FOR EVENT set_active OF zapcmd_cl_filelist.

    METHODS constructor
      IMPORTING
        pf_cmdline TYPE REF TO string
        pf_dir     TYPE REF TO zapcmd_refref_filelist.

    METHODS execute
      IMPORTING pf_cmdline  TYPE string
                pf_filelist TYPE REF TO zapcmd_cl_filelist.

*    methods handle_dblclick for event dblclick of cl_gui_textedit.

    METHODS user_command
      IMPORTING
        e_ucomm TYPE syucomm.
  PROTECTED SECTION.
*"* protected components of class ZAPCMD_CL_CMDLINE
*"* do not include other source files here!!!
  PRIVATE SECTION.
*"* private components of class ZAPCMD_CL_CMDLINE
*"* do not include other source files here!!!

    "! Target directory of "cd <arg>": absolute paths as they are,
    "! ".." is the parent, everything else is relative to io_dir
    METHODS resolve_cd_target
      IMPORTING
        !io_dir       TYPE REF TO zapcmd_cl_dir
        !iv_arg       TYPE string
      RETURNING
        VALUE(rv_dir) TYPE string.
ENDCLASS.



CLASS ZAPCMD_CL_CMDLINE IMPLEMENTATION.


  METHOD constructor.

    cf_cmdline = pf_cmdline.
    cf_filelist = pf_dir.    

  ENDMETHOD.


  METHOD execute.

    IF pf_cmdline IS NOT INITIAL.
      DATA ls_dir TYPE REF TO zapcmd_cl_dir.
      DATA lf_cmdline TYPE string.
      ls_dir ?= pf_filelist->get_dir( ).
      " cd, cd <dir>, cd.., cd\dir and cd/dir change the directory of the list
      DATA lf_upper TYPE string.
      DATA lf_arg TYPE string.
      lf_upper = pf_cmdline.
      SHIFT lf_upper LEFT DELETING LEADING space.
      TRANSLATE lf_upper TO UPPER CASE.
      IF lf_upper = 'CD' OR lf_upper CP 'CD *' OR lf_upper CP 'CD.*'
      OR lf_upper CP 'CD\*' OR lf_upper CP 'CD/*'.
        lf_arg = pf_cmdline.
        SHIFT lf_arg LEFT DELETING LEADING space.
        SHIFT lf_arg LEFT BY 2 PLACES.
        SHIFT lf_arg LEFT DELETING LEADING space.
        SHIFT lf_arg RIGHT DELETING TRAILING space.
        SHIFT lf_arg LEFT DELETING LEADING space.
        IF lf_arg IS INITIAL.
          MESSAGE ls_dir->full_name TYPE 'S'.
        ELSE.
          pf_filelist->load_dir( pf_dir = resolve_cd_target( io_dir = ls_dir
                                                             iv_arg = lf_arg ) ).
        ENDIF.
        RETURN.
      ENDIF.
      IF ls_dir->server_area = zapcmd_cl_knot=>co_area_frontend.
        DATA lf_params TYPE string.
        CONCATENATE '/k' pf_cmdline
          INTO lf_params SEPARATED BY space.
        CALL METHOD cl_gui_frontend_services=>execute
          EXPORTING
            application            = 'c:\WINDOWS\system32\cmd.exe'
            parameter              = lf_params
            default_directory      = ls_dir->full_name
            synchronous            = 'X'
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
            OTHERS                 = 10.
        IF sy-subrc <> 0.
          MESSAGE ID sy-msgid TYPE 'I' NUMBER sy-msgno
                     WITH sy-msgv1 sy-msgv2 sy-msgv3 sy-msgv4.
          RETURN.
        ENDIF.

      ELSE.

        DATA lf_line TYPE string.

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
        " runs on the system the pane shows (local server or RFC destination)
        ls_dir->execute_command(
          EXPORTING
            pf_command = lf_cmdline
          IMPORTING
            et_output  = lt_output ).
        APPEND LINES OF lt_output TO ct_line.

*       CALL SCREEN 300 STARTING AT 10 3 ENDING AT 100 27.
        CALL FUNCTION 'ZAPCMD_CALL_CMDLINE'
          EXPORTING
            pf_cmdline = me.

*      cf_gui_textedit->SET_TEXT_AS_R3TABLE(
*        exporting
*          table = ct_line
*        ).

      ENDIF.
    ENDIF.

  ENDMETHOD.


  METHOD handle_doubleclick.

    FIELD-SYMBOLS: <filelist> TYPE REF TO zapcmd_cl_filelist.
    ASSIGN cf_filelist->* TO <filelist>.
    cf_dirname = <filelist>->cf_ref_dir->full_name.

    cf_gui_dirname->set_textstream( text = cf_dirname ).

  ENDMETHOD.


  METHOD show.
    IF cf_gui_textedit IS INITIAL.
      CREATE OBJECT cf_gui_textedit
        EXPORTING
          style         = 0
          wordwrap_mode = 0
          parent        = pf_container.
      cf_gui_textedit->set_statusbar_mode( 0 ).
      cf_gui_textedit->set_toolbar_mode( 0 ).
*      cf_gui_textedit->SET_MODE( 0 ).
*      set handler handle_dblclick for cf_gui_textedit.
      cf_gui_textedit->register_event_dblclick( register = 1 ).

    ENDIF.

    cf_gui_textedit->set_readonly_mode( 1 ).

    DATA lf_stream TYPE string.
    lf_stream = zapcmd_cl_file=>text2stream( ct_line ).

    cf_gui_textedit->set_textstream( text = lf_stream ).

    IF cf_commandlinenumber IS INITIAL.
      cf_commandlinenumber = 1.
    ENDIF.

    cf_gui_textedit->go_to_line( line = cf_commandlinenumber ).

    cf_gui_textedit->set_first_visible_line( line = cf_commandlinenumber ).

  ENDMETHOD.


  METHOD show1.
*      importing
*        !PF_CONTAINER type ref to CL_GUI_CONTAINER .
    IF cf_gui_dirname IS INITIAL.
      CREATE OBJECT cf_gui_dirname
        EXPORTING
          style         = 0
          wordwrap_mode = 0
          parent        = pf_container.
      cf_gui_dirname->set_statusbar_mode( 0 ).
      cf_gui_dirname->set_toolbar_mode( 0 ).

      SET HANDLER handle_doubleclick FOR ALL INSTANCES.
    ENDIF.

    cf_gui_dirname->set_readonly_mode( 1 ).

    FIELD-SYMBOLS: <filelist> TYPE REF TO zapcmd_cl_filelist.
    ASSIGN cf_filelist->* TO <filelist>.
    cf_dirname = <filelist>->cf_ref_dir->full_name.
    cf_gui_dirname->set_textstream( text = cf_dirname ).

  ENDMETHOD.


  METHOD user_command.

    IF e_ucomm IS INITIAL.
      FIELD-SYMBOLS: <cmdline> TYPE string.
      ASSIGN cf_cmdline->* TO <cmdline>.
      FIELD-SYMBOLS: <filelist> TYPE REF TO zapcmd_cl_filelist.
      ASSIGN cf_filelist->* TO <filelist>.
      execute(
        pf_cmdline = <cmdline>
        pf_filelist = <filelist> ).
      CLEAR <cmdline>.
    ENDIF.
  ENDMETHOD.


  METHOD resolve_cd_target.

    DATA lv_sep TYPE string.
    DATA lv_len TYPE i.
    DATA lv_off TYPE i.
    DATA lt_results TYPE match_result_tab.
    DATA ls_result TYPE match_result.

    lv_sep = io_dir->separator.
    rv_dir = io_dir->full_name.

    IF iv_arg = '..'.
      " parent: drop a trailing separator, then the last path segment
      lv_len = strlen( rv_dir ) - 1.
      IF lv_len > 0 AND rv_dir+lv_len(1) = lv_sep.
        rv_dir = rv_dir(lv_len).
      ENDIF.
      FIND ALL OCCURRENCES OF lv_sep IN rv_dir RESULTS lt_results.
      IF sy-subrc <> 0.
        RETURN.
      ENDIF.
      DESCRIBE TABLE lt_results LINES lv_len.
      READ TABLE lt_results INDEX lv_len INTO ls_result ##SUBRC_OK.
      lv_off = ls_result-offset.
      IF lv_off = 0.
        rv_dir = lv_sep.                     " Unix root
      ELSEIF lv_off = 2 AND rv_dir+1(1) = ':'.
        rv_dir = rv_dir(3).                  " Windows drive, e.g. C:\
      ELSE.
        rv_dir = rv_dir(lv_off).
      ENDIF.
    ELSEIF iv_arg(1) = '/' OR iv_arg(1) = '\'
        OR ( strlen( iv_arg ) >= 2 AND iv_arg+1(1) = ':' ).
      rv_dir = iv_arg.
    ELSE.
      lv_len = strlen( rv_dir ) - 1.
      IF lv_len >= 0 AND rv_dir+lv_len(1) = lv_sep.
        CONCATENATE rv_dir iv_arg INTO rv_dir.
      ELSE.
        CONCATENATE rv_dir lv_sep iv_arg INTO rv_dir.
      ENDIF.
    ENDIF.

  ENDMETHOD.


ENDCLASS.
