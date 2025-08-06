CLASS zapcmd_cl_filelist DEFINITION
  PUBLIC
  ABSTRACT
  CREATE PUBLIC .

*"* public components of class ZAPCMD_CL_FILELIST
*"* do not include other source files here!!!
  PUBLIC SECTION.

    TYPE-POOLS icon .

    CONSTANTS: BEGIN OF gc_side,
                 left  TYPE string VALUE `LEFT`,
                 right TYPE string VALUE `RIGHT`,
               END OF gc_side.

    DATA cf_active TYPE xfeld .
    DATA cf_ref_dir TYPE REF TO zapcmd_cl_dir .

    EVENTS set_active .

    METHODS load_dir
      IMPORTING
        !pf_dir TYPE string OPTIONAL .
    METHODS reload_dir .
    METHODS root_dir .
    METHODS delete
      IMPORTING
        !pt_files TYPE zapcmd_tbl_filelist .
    METHODS edit_dir .
    METHODS show
      IMPORTING
        !pf_container TYPE REF TO cl_gui_container .
    METHODS constructor
      IMPORTING
        !iv_side TYPE string
        !pf_type TYPE syucomm DEFAULT 'FRONTEND'
        !pf_dir  TYPE string OPTIONAL.
    METHODS refresh .
    METHODS handle_activate
        FOR EVENT set_active OF zapcmd_cl_filelist
      IMPORTING
        !sender .
    METHODS get_files
      RETURNING
        VALUE(ptx_files) TYPE zapcmd_tbl_filelist .
    METHODS get_dir
      RETURNING
        VALUE(pfx_dir) TYPE REF TO zapcmd_cl_dir .
    METHODS check_active
      IMPORTING
        !pf_control TYPE REF TO cl_gui_control OPTIONAL .
    METHODS undo .
    METHODS handle_contextmenu
        FOR EVENT context_menu_request OF cl_gui_alv_grid
      IMPORTING
        !e_object .
    METHODS copy
      IMPORTING
        !pt_files   TYPE zapcmd_tbl_filelist
        !pf_destdir TYPE REF TO zapcmd_cl_dir .
    CLASS-METHODS get_imp .
  PROTECTED SECTION.
*"* protected components of class ZAPCMD_CL_FILELIST
*"* do not include other source files here!!!

    METHODS get_side
      RETURNING VALUE(result) TYPE string.

  PRIVATE SECTION.
    CONSTANTS c_field_sort_prio TYPE lvc_s_sort-fieldname VALUE 'SORT_PRIO' ##NO_TEXT.
*"* private components of class ZAPCMD_CL_FILELIST
*"* do not include other source files here!!!

    DATA cf_gui_alv TYPE REF TO cl_gui_alv_grid .
    DATA ct_files TYPE zapcmd_tbl_filelist .
    DATA ct_fileinfo TYPE zapcmd_tbl_file_info .
    DATA temp_non_sort LIKE ct_fileinfo.
    DATA ct_undo TYPE TABLE OF REF TO zapcmd_cl_dir .
    DATA gv_side TYPE string.
    DATA gt_fcode_factory TYPE zapcmd_tbl_fcode_factory .
    DATA gt_factory_button TYPE ttb_button .

    METHODS handle_doubleclick
        FOR EVENT double_click OF cl_gui_alv_grid
      IMPORTING
        !e_row .
    METHODS handle_set_toolbar
        FOR EVENT toolbar OF cl_gui_alv_grid
      IMPORTING
        !e_object .
    METHODS handle_usercommand
        FOR EVENT user_command OF cl_gui_alv_grid
      IMPORTING
        !e_ucomm .


    METHODS get_title
      EXPORTING
        !pf_title TYPE string .
    METHODS handle_drag
        FOR EVENT ondrag OF cl_gui_alv_grid
      IMPORTING
        !e_row
        !e_column
        !es_row_no
        !e_dragdropobj .
    METHODS handle_drop
        FOR EVENT ondrop OF cl_gui_alv_grid
      IMPORTING
        !sender
        !e_row
        !e_column
        !es_row_no
        !e_dragdropobj .
    METHODS get_factories
      RETURNING
        VALUE(et_imp) TYPE zapcmd_tbl_factory .
    METHODS get_factory_buttons
      CHANGING
        ct_button TYPE ttb_button .

ENDCLASS.



CLASS zapcmd_cl_filelist IMPLEMENTATION.


  METHOD check_active.

    DATA lf_control TYPE REF TO cl_gui_control.
    lf_control = pf_control.
    IF lf_control IS INITIAL.

      CALL METHOD cl_gui_control=>get_focus
        IMPORTING
          control           = lf_control
        EXCEPTIONS
          cntl_error        = 1
          cntl_system_error = 2
          OTHERS            = 3.
      IF sy-subrc <> 0.
*        MESSAGE ID SY-MSGID TYPE SY-MSGTY NUMBER SY-MSGNO
*                   WITH SY-MSGV1 SY-MSGV2 SY-MSGV3 SY-MSGV4.
      ENDIF.


    ENDIF.
    IF lf_control = cf_gui_alv.
      RAISE EVENT set_active.
    ENDIF.

  ENDMETHOD.


  METHOD constructor.

    DATA l_fcode      TYPE syucomm.
    DATA l_directory  TYPE string.
    DATA li_user_exit TYPE REF TO zapcmd_if_user_exit.

    SET HANDLER handle_activate FOR ALL INSTANCES.

    gv_side = iv_side.

    l_directory = pf_dir.

    IF pf_type = 'DEFAULT'.

      CASE iv_side.
        WHEN zapcmd_cl_filelist=>gc_side-left.
          l_fcode = zapcmd_cl_frontend_factory=>gc_fcode-frontend.

        WHEN zapcmd_cl_filelist=>gc_side-right.
          l_fcode = zapcmd_cl_server_factory=>gc_fcode-application_server.

      ENDCASE.

      li_user_exit = zapcmd_cl_user_exit_factory=>get( ).
      IF li_user_exit IS BOUND.
        li_user_exit->change_default_directory( EXPORTING iv_side          = get_side( )
                                                CHANGING  cv_function_code = l_fcode
                                                          cv_directory     = l_directory ).
      ENDIF.

    ELSE.
      l_fcode = pf_type.
    ENDIF.

    DATA lt_imp TYPE zapcmd_tbl_factory.
    lt_imp = get_factories( ).
    DATA l_imp TYPE REF TO zapcmd_if_factory.
    LOOP AT lt_imp INTO l_imp.
      cf_ref_dir = l_imp->create_dir( i_fcode = l_fcode i_dir = l_directory ).
      IF cf_ref_dir IS BOUND.
        EXIT.
      ENDIF.
    ENDLOOP.

    IF cf_ref_dir IS BOUND.
      reload_dir( ).
    ENDIF.

  ENDMETHOD.


  METHOD copy.

    DATA lf_file TYPE REF TO zapcmd_cl_knot.
    DATA lf_sourcefile TYPE REF TO zapcmd_cl_file.
    DATA lf_destfile TYPE REF TO zapcmd_cl_file.
    DATA lt_file TYPE zapcmd_tbl_xstring.
    DATA lf_answer TYPE c.
    lf_answer = '1'.

    LOOP AT pt_files INTO lf_file
       WHERE table_line->filetype <> 'DIR'
         AND table_line->filetype <> 'UP'.

      lf_sourcefile ?= lf_file.
      DATA lf_string TYPE string.
      CONCATENATE 'COPY: ' lf_sourcefile->name INTO lf_string.

      CALL FUNCTION 'SAPGUI_PROGRESS_INDICATOR'
        EXPORTING
          percentage = 0
          text       = lf_string.

      IF pf_destdir->check_fileexist( lf_sourcefile->name ) = abap_true.
        IF lf_answer <> '2'.

          CONCATENATE '"' lf_sourcefile->name '"' ' existiert.'(006) ' Überschreiben?'(007) INTO
            lf_string.

          CALL FUNCTION 'POPUP_TO_CONFIRM'
            EXPORTING
              titlebar              = lf_sourcefile->full_name
*             DIAGNOSE_OBJECT       = ' '
              text_question         = lf_string
              text_button_1         = 'Ja'(008)
              icon_button_1         = ' '
              text_button_2         = 'Ja, Alle'(009)
              icon_button_2         = ' '
              default_button        = '1'
              display_cancel_button = 'X'
*             USERDEFINED_F1_HELP   = ' '
*             START_COLUMN          = 25
*             START_ROW             = 6
*             POPUP_TYPE            =
            IMPORTING
              answer                = lf_answer
*           TABLES
*             PARAMETER             =
            EXCEPTIONS
              text_not_found        = 1
              OTHERS                = 2.
          IF sy-subrc <> 0.
            MESSAGE ID sy-msgid TYPE sy-msgty NUMBER sy-msgno
                    WITH sy-msgv1 sy-msgv2 sy-msgv3 sy-msgv4.
          ENDIF.
          IF lf_answer = 'A'.
**          Abbruch.
            RETURN.
          ENDIF.

        ENDIF.


      ENDIF.

      CALL METHOD pf_destdir->create_file
        EXPORTING
          pf_filename = lf_sourcefile->name
        IMPORTING
          pf_file     = lf_destfile.

      DATA lf_filesize TYPE i.

      CALL METHOD lf_sourcefile->read_bin
        IMPORTING
          pt_file     = lt_file
          pf_filesize = lf_filesize.

      CALL METHOD lf_destfile->write_bin
        EXPORTING
          pt_file     = lt_file
          pf_filesize = lf_filesize.


    ENDLOOP.
    LOOP AT pt_files INTO lf_file
      WHERE table_line->filetype = 'DIR'
      AND   table_line->name <> '..'.

*          message lf_file->name type 'S'.
      DATA lf_destdir TYPE REF TO zapcmd_cl_dir.

      CALL METHOD pf_destdir->create_dir
        EXPORTING
          pf_filename = lf_file->name
        IMPORTING
          pf_file     = lf_destdir.

      DATA lf_files TYPE zapcmd_tbl_filelist.
      DATA lf_dir TYPE REF TO zapcmd_cl_dir.
      lf_dir ?= lf_file.
      CALL METHOD lf_dir->read_dir
        IMPORTING
          pt_filelist       = lf_files
        EXCEPTIONS
          permission_denied = 1.
      IF sy-subrc <> 0.
        MESSAGE ID sy-msgid TYPE 'I' NUMBER sy-msgno
          WITH sy-msgv1 sy-msgv2 sy-msgv3 sy-msgv4.
      ELSE.
        CALL METHOD copy
          EXPORTING
            pt_files   = lf_files
            pf_destdir = lf_destdir.
      ENDIF.


    ENDLOOP.

    CALL METHOD reload_dir.
    CALL METHOD refresh.


  ENDMETHOD.


  METHOD delete.

    DATA lf_file TYPE REF TO zapcmd_cl_knot.
    DATA lf_answer TYPE c.

    LOOP AT pt_files INTO lf_file.

      IF lf_file->name <> '..'.

        IF lf_answer <> '2'.

          DATA lf_string TYPE string.
          CONCATENATE '"' lf_file->name '"' ' löschen?'(010) INTO
            lf_string.

          CALL FUNCTION 'POPUP_TO_CONFIRM'
            EXPORTING
              titlebar              = lf_file->full_name
*             DIAGNOSE_OBJECT       = ' '
              text_question         = lf_string
              text_button_1         = 'Ja'(008)
              icon_button_1         = ' '
              text_button_2         = 'Ja, Alle'(009)
              icon_button_2         = ' '
              default_button        = '1'
              display_cancel_button = 'X'
*             USERDEFINED_F1_HELP   = ' '
*             START_COLUMN          = 25
*             START_ROW             = 6
*             POPUP_TYPE            =
            IMPORTING
              answer                = lf_answer
*           TABLES
*             PARAMETER             =
            EXCEPTIONS
              text_not_found        = 1
              OTHERS                = 2.
          IF sy-subrc <> 0.
            MESSAGE ID sy-msgid TYPE sy-msgty NUMBER sy-msgno
                    WITH sy-msgv1 sy-msgv2 sy-msgv3 sy-msgv4.
          ENDIF.

        ENDIF.

        IF lf_answer = '1' OR lf_answer = '2'.
          IF lf_file->filetype = 'DIR'.
            DATA lf_files TYPE zapcmd_tbl_filelist.
            DATA lf_dir TYPE REF TO zapcmd_cl_dir.
            lf_dir ?= lf_file.
            CALL METHOD lf_dir->read_dir
              IMPORTING
                pt_filelist       = lf_files
              EXCEPTIONS
                permission_denied = 1.
            IF sy-subrc <> 0.
              MESSAGE ID sy-msgid TYPE 'I' NUMBER sy-msgno
                WITH sy-msgv1 sy-msgv2 sy-msgv3 sy-msgv4.
              EXIT.
            ENDIF.
            CALL METHOD delete
              EXPORTING
                pt_files = lf_files.

          ENDIF.
          CALL METHOD lf_file->delete.
        ELSEIF lf_answer = 'A'.
          EXIT.
        ENDIF.
      ENDIF.
    ENDLOOP.

    CALL METHOD reload_dir.
    CALL METHOD refresh.

  ENDMETHOD.


  METHOD edit_dir.



    DATA values TYPE TABLE OF sval.
    DATA value TYPE sval.
    value-tabname = 'ZAPCMD_FILE_DESCR'.
    value-fieldname = 'FULL_NAME'.
    value-fieldtext = 'Pfad:'(002).
    APPEND value TO values.


    CALL FUNCTION 'POPUP_GET_VALUES'
      EXPORTING
*       NO_VALUE_CHECK        = ' '
        popup_title = 'Direkte Pfadeingabe'(202)
*       START_COLUMN          = '5'
*       START_ROW   = '5'
*  IMPORTING
*       RETURNCODE  =
      TABLES
        fields      = values
*  EXCEPTIONS
*       ERROR_IN_FIELDS       = 1
*       OTHERS      = 2
      .
    IF sy-subrc <> 0.
* MESSAGE ID SY-MSGID TYPE SY-MSGTY NUMBER SY-MSGNO
*         WITH SY-MSGV1 SY-MSGV2 SY-MSGV3 SY-MSGV4.
    ENDIF.

    LOOP AT values INTO value.
    ENDLOOP.

    DATA lf_rootdir TYPE string.
    lf_rootdir = value-value.

    DATA l_class TYPE abap_abstypename.
    l_class = cl_abap_classdescr=>get_class_name( cf_ref_dir ).

    CREATE OBJECT cf_ref_dir TYPE (l_class).

    CALL METHOD cf_ref_dir->init
      EXPORTING
        pf_full_name = lf_rootdir.

    CALL METHOD reload_dir.



  ENDMETHOD.


  METHOD get_dir.

    pfx_dir = cf_ref_dir.

  ENDMETHOD.


  METHOD get_factories.

    DATA lt_imp       TYPE TABLE OF seoclsname.
    DATA li_user_exit TYPE REF TO zapcmd_if_user_exit.

    DATA: BEGIN OF ls_para_tab,
            name      TYPE abap_parmname,
            tabname   TYPE tabname,
            fieldname TYPE fieldname,
          END OF ls_para_tab.
    DATA lt_para_tab LIKE TABLE OF ls_para_tab.

    SELECT clsname FROM seometarel INTO TABLE lt_imp
      WHERE refclsname = 'ZAPCMD_IF_FACTORY'.

    IF lt_imp IS NOT INITIAL.
      SELECT clsname FROM seometarel APPENDING TABLE lt_imp
        FOR ALL ENTRIES IN lt_imp
        WHERE refclsname = lt_imp-table_line.
    ENDIF.

    SORT lt_imp.
    DELETE ADJACENT DUPLICATES FROM lt_imp.

    REFRESH et_imp.

    DATA l_imp TYPE seoclsname.
    DATA lo_object TYPE REF TO zapcmd_if_factory.
    LOOP AT lt_imp INTO l_imp.
      CREATE OBJECT lo_object TYPE (l_imp).
      APPEND lo_object TO et_imp.
    ENDLOOP.

    li_user_exit = zapcmd_cl_user_exit_factory=>get( ).
    IF li_user_exit IS BOUND.
      li_user_exit->change_factories( EXPORTING iv_side      = get_side( )
                                      CHANGING  ct_factories = et_imp ).
    ENDIF.

  ENDMETHOD.


  METHOD get_factory_buttons.

    IF gt_factory_button IS INITIAL.

      DATA lt_imp TYPE zapcmd_tbl_factory.
      lt_imp = get_factories( ).

      DATA l_imp TYPE REF TO zapcmd_if_factory.
      DATA ls_fcode TYPE zapcmd_str_fcode_factory.
      DATA lt_button TYPE ttb_button.
      DATA ls_button TYPE stb_button.
      LOOP AT lt_imp INTO l_imp.

        lt_button = l_imp->get_button( ).
        LOOP AT lt_button INTO ls_button.
          IF ls_button-function IS NOT INITIAL.
            ls_fcode-fcode = ls_button-function.
            ls_fcode-factory = l_imp.
            INSERT ls_fcode INTO TABLE gt_fcode_factory.
          ENDIF.
          APPEND ls_button TO gt_factory_button.
        ENDLOOP.

      ENDLOOP.

    ENDIF.

*  loop at gt_factory_button into ls_button.
*    read table gt_fcode_factory into ls_fcode
*      with table key fcode = ls_button-function.
*    if ls_fcode is not INITIAL and ls_fcode-factory <> cf_ref_factory.
*      APPEND ls_button TO CT_BUTTON.
*    endif.
*  endloop.

    CLEAR ls_button.
    MOVE 3 TO ls_button-butn_type.
    INSERT ls_button INTO ct_button INDEX 1.

    LOOP AT gt_factory_button INTO ls_button.
      INSERT ls_button INTO ct_button INDEX 1.
    ENDLOOP.

*  append lines of gt_factory_button TO CT_BUTTON.

  ENDMETHOD.


  METHOD get_files.

    DATA lf_lines TYPE i.

    DATA lt_rows TYPE lvc_t_row.
    DATA ls_row TYPE lvc_s_row.
    CALL METHOD cf_gui_alv->get_selected_rows
      IMPORTING
        et_index_rows = lt_rows.

    DESCRIBE TABLE lt_rows LINES lf_lines.
    IF lf_lines = 0.
      DATA lt_cells TYPE lvc_t_cell.
      DATA ls_cell TYPE lvc_s_cell.

      CALL METHOD cf_gui_alv->get_selected_cells
        IMPORTING
          et_cell = lt_cells.
      LOOP AT lt_cells INTO ls_cell.
        APPEND ls_cell-row_id TO lt_rows.
      ENDLOOP.
      DELETE ADJACENT DUPLICATES FROM lt_rows COMPARING index.
    ENDIF.

    LOOP AT lt_rows INTO ls_row.

      DATA ls_fileinfo TYPE zapcmd_file_descr.
      DATA lf_file TYPE REF TO zapcmd_cl_knot.
      READ TABLE ct_fileinfo INDEX ls_row-index INTO ls_fileinfo.

      READ TABLE ct_files INDEX ls_fileinfo-indx INTO lf_file.
      APPEND lf_file TO ptx_files.

    ENDLOOP.

  ENDMETHOD.


  METHOD get_imp.

    DATA lo_object TYPE REF TO zapcmd_cl_dir.
    DATA lt_imp TYPE TABLE OF seoclsname.

    DATA: BEGIN OF ls_para_tab,
            name      TYPE abap_parmname,
            tabname   TYPE tabname,
            fieldname TYPE fieldname,
          END OF ls_para_tab.
    DATA lt_para_tab LIKE TABLE OF ls_para_tab.


    SELECT clsname FROM seometarel INTO TABLE lt_imp
      WHERE refclsname = 'ZAPCMD_CL_DIR'.

    IF lt_imp IS NOT INITIAL.
      SELECT clsname FROM seometarel APPENDING TABLE lt_imp
        FOR ALL ENTRIES IN lt_imp
        WHERE refclsname = lt_imp-table_line.
    ENDIF.

    SORT lt_imp.
    DELETE ADJACENT DUPLICATES FROM lt_imp.

    DATA lo_classdescr TYPE REF TO cl_abap_classdescr.
    DATA l_imp TYPE seoclsname.
    DATA l_tab_parmbind TYPE abap_parmbind_tab.
    DATA l_tab_excpbind TYPE abap_excpbind_tab.
    DATA l_parmbind TYPE abap_parmbind.
    DATA l_excpbind TYPE abap_excpbind.
    DATA lt_sval TYPE TABLE OF sval.
    DATA ls_sval TYPE sval.
    FIELD-SYMBOLS <value> TYPE any.
    LOOP AT lt_imp INTO l_imp.
      CLEAR l_tab_parmbind.
      CLEAR l_tab_excpbind.
      CLEAR lt_sval.
      CLEAR lt_para_tab.
      lo_classdescr ?= cl_abap_classdescr=>describe_by_name( l_imp ).
      DATA l_method TYPE abap_methdescr.

      LOOP AT lo_classdescr->methods INTO l_method
        WHERE name = 'CONSTRUCTOR'.
        DATA ls_param TYPE abap_parmdescr.
        LOOP AT l_method-parameters INTO ls_param.
          DATA lo_paramtype TYPE REF TO cl_abap_datadescr.
          lo_paramtype ?= lo_classdescr->get_method_parameter_type(
             p_method_name = l_method-name
             p_parameter_name = ls_param-name
           ).
          l_parmbind-name = ls_param-name.
          l_parmbind-kind = 'E'. "ls_param-parm_kind.
          CREATE DATA l_parmbind-value TYPE HANDLE lo_paramtype.
          INSERT l_parmbind INTO TABLE l_tab_parmbind.
          DATA l_tab TYPE tabname.
          DATA l_rollname TYPE dd03l-rollname.
          l_rollname = lo_paramtype->get_relative_name( ).
          CLEAR ls_sval.
          SELECT checktable FROM dd03l INTO l_tab
            UP TO 1 ROWS
            WHERE rollname = l_rollname
              AND as4local = 'A'
              AND checktable IS NOT NULL
              AND checktable > ''
              AND checktable <> '*'.
          ENDSELECT.
          IF sy-subrc = 0.
            SELECT tabname fieldname FROM dd03l INTO CORRESPONDING FIELDS OF ls_sval
              UP TO 1 ROWS
              WHERE rollname = l_rollname
                AND as4local = 'A'
                AND tabname = l_tab.
            ENDSELECT.
          ELSE.
            SELECT tabname fieldname FROM dd03l INTO CORRESPONDING FIELDS OF ls_sval
            UP TO 1 ROWS
            WHERE rollname = l_rollname
              AND as4local = 'A'
              AND keyflag = 'X'.
            ENDSELECT.
          ENDIF.
          IF ls_sval-tabname IS NOT INITIAL.
            IF ls_param-is_optional IS INITIAL.
              ls_sval-field_obl = 'X'.
            ENDIF.
            APPEND ls_sval TO lt_sval.
            ls_para_tab-name = ls_param-name.
            ls_para_tab-tabname = ls_sval-tabname.
            ls_para_tab-fieldname = ls_sval-fieldname.
            INSERT ls_para_tab INTO TABLE lt_para_tab.
          ENDIF.
        ENDLOOP.

        DATA ls_excp TYPE abap_excpdescr.
        DATA l_count TYPE i.
        CLEAR l_count.
        LOOP AT l_method-exceptions INTO ls_excp.
          l_count = l_count + 1.
          l_excpbind-name = ls_excp-name.
          l_excpbind-value = l_count.
          INSERT l_excpbind INTO TABLE l_tab_excpbind.
        ENDLOOP.

      ENDLOOP.

      IF lt_sval IS NOT INITIAL.

        CALL FUNCTION 'POPUP_GET_VALUES'
          EXPORTING
*           NO_VALUE_CHECK        = ' '
            popup_title = 'Werteingabe'(018)
*           START_COLUMN          = '5'
*           START_ROW   = '5'
*       IMPORTING
*           RETURNCODE  = RETURNCODE
          TABLES
            fields      = lt_sval
*       EXCEPTIONS
*           ERROR_IN_FIELDS       = 1
*           OTHERS      = 2
          .
        IF sy-subrc <> 0.
* MESSAGE ID SY-MSGID TYPE SY-MSGTY NUMBER SY-MSGNO
*         WITH SY-MSGV1 SY-MSGV2 SY-MSGV3 SY-MSGV4.
        ENDIF.
        LOOP AT lt_sval INTO ls_sval.
          LOOP AT lt_para_tab INTO ls_para_tab
             WHERE tabname = ls_sval-tabname
              AND  fieldname = ls_sval-fieldname.
          ENDLOOP.
          READ TABLE l_tab_parmbind INTO l_parmbind WITH TABLE KEY name =  ls_para_tab-name.
          IF sy-subrc = 0.
            ASSIGN l_parmbind-value->* TO <value>.
            <value> = ls_sval-value.
          ENDIF.
        ENDLOOP.

      ENDIF.



      CREATE OBJECT lo_object
        TYPE
          (l_imp)
        PARAMETER-TABLE
          l_tab_parmbind
        EXCEPTION-TABLE
          l_tab_excpbind.
      IF sy-subrc <> 0.
        MESSAGE 'Nicht instanziierbar.' TYPE 'E'.
      ENDIF.




    ENDLOOP.



  ENDMETHOD.


  METHOD get_title.


*      case cf_ref_dir->SERVER_AREA.
*        when 'A'. lf_areastring = 'Appl. Serv.'(014).
*        when 'P'. lf_areastring = 'Frontend'(015).
*        when 'R'. lf_areastring = 'RFC'(016).
*        when others. lf_areastring = cf_ref_dir->SERVER_AREA.
*      endcase.


    DATA lf_space TYPE p.
    CALL METHOD cf_ref_dir->get_freespace
      IMPORTING
        pf_space = lf_space.
    IF lf_space > 0.
      DATA lf_spacestr TYPE string.
      CALL METHOD zapcmd_cl_file=>convert_size
        EXPORTING
          pf_size     = lf_space
        IMPORTING
          pfx_sizestr = lf_spacestr.

      CONCATENATE
        '<' cf_ref_dir->area_string '>°'
        cf_ref_dir->full_name ' (' lf_spacestr ' frei)'
        INTO pf_title.
    ELSE.
      CONCATENATE
        '<' cf_ref_dir->area_string '>°'
        cf_ref_dir->full_name
        INTO pf_title.
    ENDIF.
    TRANSLATE pf_title USING '° '.

  ENDMETHOD.


  METHOD handle_activate.

    IF me = sender.
      cf_active = abap_true.
    ELSE.
      cf_active = abap_false.
    ENDIF.

  ENDMETHOD.


  METHOD handle_contextmenu.

    e_object->add_separator( ).

    e_object->add_function(
        fcode       = zapcmd_cl_dir=>co_rename
        text        = 'Umbenennen'(231)
        icon        = icon_rename
*      FTYPE       = FTYPE
*      DISABLED    = DISABLED
*      HIDDEN      = HIDDEN
*      CHECKED     = CHECKED
*      ACCELERATOR = ACCELERATOR
           ).

    e_object->add_function(
       fcode       = zapcmd_cl_dir=>co_delete
       text        = 'Löschen'(011)
       icon        = icon_delete
*      FTYPE       = FTYPE
*      DISABLED    = DISABLED
*      HIDDEN      = HIDDEN
*      CHECKED     = CHECKED
*      ACCELERATOR = ACCELERATOR
          ).



  ENDMETHOD.


  METHOD get_side.

    result = gv_side.

  ENDMETHOD.


  METHOD handle_doubleclick.

*    raise event set_active.
*    exporting sender = me.

    DATA lf_file TYPE REF TO zapcmd_cl_knot.
    DATA ls_fileinto TYPE zapcmd_file_descr.
    DATA ls_fileinfo TYPE zapcmd_file_descr.
    READ TABLE ct_fileinfo INDEX e_row-index INTO ls_fileinfo.

    READ TABLE ct_files INDEX ls_fileinfo-indx INTO lf_file.

    IF lf_file->is_dir = abap_true.
      IF cf_ref_dir IS BOUND.
        APPEND cf_ref_dir TO ct_undo.
      ENDIF.
      cf_ref_dir ?= lf_file.
      IF sy-subrc = 0.
        CALL METHOD cf_ref_dir->read_dir
          IMPORTING
            pt_filelist       = ct_files
          EXCEPTIONS
            permission_denied = 1.
        IF sy-subrc <> 0.
          MESSAGE ID sy-msgid TYPE 'I' NUMBER sy-msgno
            WITH sy-msgv1 sy-msgv2 sy-msgv3 sy-msgv4.
          EXIT.
        ENDIF.

        REFRESH ct_fileinfo.
        DATA lf_index TYPE i VALUE 0.
        LOOP AT ct_files INTO lf_file.
          lf_index = lf_index + 1.
          CALL METHOD lf_file->get_info
            RECEIVING
              psx_fileinfo = ls_fileinto.
          ls_fileinto-indx = lf_index.
          APPEND ls_fileinto TO ct_fileinfo.
        ENDLOOP.

        CALL METHOD refresh.
      ENDIF.
    ELSE.
      CALL METHOD lf_file->execute.
    ENDIF.

    RAISE EVENT set_active.

    CALL METHOD cl_gui_cfw=>flush.
    IF sy-subrc <> 0.
    ENDIF.

  ENDMETHOD.


  METHOD handle_drag.

    DATA lo_list TYPE REF TO zapcmd_cl_knotlist.
    DATA lo_knot TYPE REF TO zapcmd_cl_knot.

    CREATE OBJECT lo_list.

    DATA lt_selected_rows TYPE lvc_t_row.
    DATA l_row TYPE lvc_s_row.

    cf_gui_alv->get_selected_rows(
      IMPORTING
        et_index_rows = lt_selected_rows
    ).

    IF lt_selected_rows IS NOT INITIAL.
      READ TABLE lt_selected_rows TRANSPORTING NO FIELDS
        WITH KEY table_line = e_row.
      IF sy-subrc <> 0.
        e_dragdropobj->abort( ).
        RETURN.
      ENDIF.
      LOOP AT lt_selected_rows INTO l_row.

        DATA ls_fileinfo TYPE zapcmd_file_descr.
        READ TABLE ct_fileinfo INDEX l_row-index INTO ls_fileinfo.
        READ TABLE ct_files INDEX ls_fileinfo-indx INTO lo_knot.

        lo_list->add( lo_knot ).

      ENDLOOP.
    ELSE.
      READ TABLE ct_fileinfo INDEX e_row-index INTO ls_fileinfo.
      READ TABLE ct_files INDEX ls_fileinfo-indx INTO lo_knot.

      lo_list->add( lo_knot ).


    ENDIF.


    e_dragdropobj->object = lo_list.

  ENDMETHOD.


  METHOD handle_drop.

    DATA lo_knot          TYPE REF TO zapcmd_cl_knot.
    DATA lo_dir           TYPE REF TO zapcmd_cl_dir.
    DATA lo_destination           TYPE REF TO zapcmd_cl_dir.
    DATA lo_list          TYPE REF TO zapcmd_cl_knotlist.
    DATA lt_selected_rows TYPE lvc_t_row.
    DATA l_row            TYPE lvc_s_row.
    DATA lv_question TYPE string.
    DATA lv_answer TYPE c LENGTH 1.

    IF e_dragdropobj->droptargetctrl = e_dragdropobj->dragsourcectrl.
      e_dragdropobj->abort( ).
      RETURN.
    ENDIF.

    cf_gui_alv->get_selected_rows( IMPORTING et_index_rows = lt_selected_rows ).

    IF lt_selected_rows IS NOT INITIAL.

      IF NOT line_exists( lt_selected_rows[ table_line = e_row ] ).
        e_dragdropobj->abort( ).
        RETURN.
      ENDIF.

      LOOP AT lt_selected_rows INTO l_row.

        DATA ls_fileinfo TYPE zapcmd_file_descr.
        READ TABLE ct_fileinfo INDEX l_row-index INTO ls_fileinfo.
        READ TABLE ct_files INDEX ls_fileinfo-indx INTO lo_knot.

      ENDLOOP.

    ELSE.

      READ TABLE ct_fileinfo INDEX e_row-index INTO ls_fileinfo.
      READ TABLE ct_files INDEX ls_fileinfo-indx INTO lo_knot.

    ENDIF.

    IF lo_knot IS BOUND.

      TRY.
          lo_dir ?= lo_knot.

          lv_question = |{ 'To current directory'(501) }| &&
                        | { 'or'(502) }| &&
                        | { 'to subdirectory'(503) }: { lo_dir->name }|.

          CALL FUNCTION 'POPUP_TO_CONFIRM'
            EXPORTING
              titlebar              = 'Create file'(505)
              text_question         = lv_question
              text_button_1         = 'Current'(504)
              icon_button_1         = ' '
              text_button_2         = lo_dir->name
              icon_button_2         = ' '
              default_button        = '1'
              display_cancel_button = 'X'
            IMPORTING
              answer                = lv_answer
            EXCEPTIONS
              text_not_found        = 1
              OTHERS                = 2.
          IF sy-subrc <> 0.
            MESSAGE ID sy-msgid TYPE sy-msgty NUMBER sy-msgno
                    WITH sy-msgv1 sy-msgv2 sy-msgv3 sy-msgv4.
          ENDIF.

          CASE lv_answer.
            WHEN '1'.
              lo_destination = cf_ref_dir.

            WHEN '2'.
              lo_destination = lo_dir.

            WHEN 'A'.
              RETURN.

          ENDCASE.

        CATCH cx_sy_move_cast_error.
          CLEAR lo_dir.
          CLEAR lo_destination.
      ENDTRY.

    ENDIF.

    IF lo_destination IS NOT BOUND.
      lo_destination = cf_ref_dir.
    ENDIF.

    TRY.

        lo_list ?= e_dragdropobj->object.

        CALL METHOD copy
          EXPORTING
            pt_files   = lo_list->ct_list
            pf_destdir = lo_destination.


      CATCH cx_root.
        e_dragdropobj->abort( ).
    ENDTRY.

  ENDMETHOD.


  METHOD handle_set_toolbar.

    DATA ls_toolbar     TYPE stb_button.
    DATA lt_toolbar     TYPE ttb_button.
    DATA lt_toolbar_uex TYPE ttb_button.
    DATA li_user_exit   TYPE REF TO zapcmd_if_user_exit.

    CLEAR ls_toolbar.
    MOVE 3 TO ls_toolbar-butn_type.
    APPEND ls_toolbar TO e_object->mt_toolbar.

    get_factory_buttons(
      CHANGING
        ct_button    = e_object->mt_toolbar
           ).

    IF  cf_ref_dir IS BOUND.
      CLEAR ls_toolbar.
      MOVE 3 TO ls_toolbar-butn_type.
      APPEND ls_toolbar TO e_object->mt_toolbar.

      CLEAR ls_toolbar.
      MOVE 0 TO ls_toolbar-butn_type.
      MOVE zapcmd_cl_dir=>co_refresh TO ls_toolbar-function.
      MOVE icon_refresh TO ls_toolbar-icon.
      MOVE 'Refresh'(201) TO ls_toolbar-quickinfo.
      MOVE space TO ls_toolbar-disabled.
      APPEND ls_toolbar TO lt_toolbar_uex.

      MOVE 0 TO ls_toolbar-butn_type.
      MOVE zapcmd_cl_dir=>co_edit_dir TO ls_toolbar-function.
      MOVE icon_fast_entry TO ls_toolbar-icon.
      MOVE 'Direkte Pfadeingabe'(202) TO ls_toolbar-quickinfo.
      MOVE space TO ls_toolbar-disabled.
      APPEND ls_toolbar TO lt_toolbar_uex.

      li_user_exit = zapcmd_cl_user_exit_factory=>get( ).
      IF li_user_exit IS BOUND.
        li_user_exit->change_toolbar( EXPORTING iv_side    = get_side( )
                                      CHANGING  ct_buttons = lt_toolbar_uex ).
      ENDIF.

      APPEND LINES OF lt_toolbar_uex TO e_object->mt_toolbar.

      CALL METHOD cf_ref_dir->get_toolbar
        IMPORTING
          pt_toolbar = lt_toolbar.

      APPEND LINES OF lt_toolbar TO e_object->mt_toolbar.
    ENDIF.

*    MOVE 0 TO ls_toolbar-butn_type.
*    MOVE co_server TO ls_toolbar-function.
*    MOVE ICON_SYM_ALT_SERVER TO ls_toolbar-icon.
*    MOVE 'Server/Laufwerke'(230) to ls_toolbar-text.
*    MOVE 'Server/Laufmerke'(231) TO ls_toolbar-quickinfo.
*    MOVE SPACE TO ls_toolbar-disabled.
*    APPEND ls_toolbar TO e_object->mt_toolbar.
*


  ENDMETHOD.


  METHOD handle_usercommand.

    DATA l_dir_temp   TYPE REF TO zapcmd_cl_dir.
    DATA li_user_exit TYPE REF TO zapcmd_if_user_exit.

    RAISE EVENT set_active.

    DATA ls_factory TYPE zapcmd_str_fcode_factory.
    READ TABLE gt_fcode_factory INTO ls_factory
      WITH TABLE KEY fcode = e_ucomm.
    IF sy-subrc = 0.
      l_dir_temp = ls_factory-factory->create_dir( i_fcode = e_ucomm ).
      IF l_dir_temp IS BOUND.
        IF cf_ref_dir IS BOUND.
          APPEND cf_ref_dir TO ct_undo.
        ENDIF.
        cf_ref_dir = l_dir_temp.

        CALL METHOD reload_dir.
        CALL METHOD refresh.
        RETURN.
      ELSE.
        MESSAGE 'Verzeichnis konnte nicht eingelesen werden.'(017) TYPE 'I' DISPLAY LIKE 'E'.
        RETURN.
      ENDIF.

    ENDIF.

    l_dir_temp = cf_ref_dir->create_new( e_ucomm ).
    IF l_dir_temp IS NOT INITIAL.
      IF cf_ref_dir IS BOUND.
        APPEND cf_ref_dir TO ct_undo.
      ENDIF.
      cf_ref_dir = l_dir_temp.

      CALL METHOD reload_dir.
      CALL METHOD refresh.
      RETURN.
    ENDIF.

    CASE e_ucomm.
      WHEN zapcmd_cl_dir=>co_refresh.
        CALL METHOD reload_dir.
        CALL METHOD refresh.
      WHEN zapcmd_cl_dir=>co_edit_dir.
        CALL METHOD edit_dir.
        CALL METHOD refresh.
      WHEN zapcmd_cl_dir=>co_rename.
        DATA lt_file TYPE zapcmd_tbl_filelist.
        DATA lf_file TYPE REF TO zapcmd_cl_knot.
        lt_file = me->get_files( ).
        LOOP AT lt_file INTO lf_file.

          DATA lt_fields TYPE TABLE OF sval.
          DATA ls_field TYPE sval.
          ls_field-tabname = 'ZAPCMD_FILE_DESCR'.
          ls_field-fieldname = 'NAME'.
          ls_field-field_obl = 'X'.
          ls_field-value = lf_file->name.
          APPEND ls_field TO lt_fields.

          CALL FUNCTION 'POPUP_GET_VALUES'
            EXPORTING
*             NO_VALUE_CHECK        = ' '
              popup_title = 'Neuer Dateiname'(001)
*             START_COLUMN          = '5'
*             START_ROW   = '5'
*           IMPORTING
*             RETURNCODE  = RETURNCODE
            TABLES
              fields      = lt_fields
*           EXCEPTIONS
*             ERROR_IN_FIELDS       = 1
*             OTHERS      = 2
            .
          IF sy-subrc <> 0.
* MESSAGE ID SY-MSGID TYPE SY-MSGTY NUMBER SY-MSGNO
*         WITH SY-MSGV1 SY-MSGV2 SY-MSGV3 SY-MSGV4.
          ENDIF.
          DATA lf_newname TYPE zapcmd_filename.
          READ TABLE lt_fields INDEX 1 INTO ls_field.
          lf_newname = ls_field-value.
          IF lf_newname <> lf_file->name.
            lf_file->rename( lf_newname ).
          ENDIF.

        ENDLOOP.
        reload_dir( ).
        refresh( ).

      WHEN zapcmd_cl_dir=>co_delete.
        lt_file = me->get_files( ).
        delete( lt_file ).

      WHEN OTHERS.

        li_user_exit = zapcmd_cl_user_exit_factory=>get( ).
        IF li_user_exit IS BOUND.
          li_user_exit->filelist_user_command( iv_function_code = e_ucomm
                                               io_directory     = cf_ref_dir ).
        ENDIF.

    ENDCASE.

  ENDMETHOD.


  METHOD load_dir.

    IF cf_ref_dir IS BOUND.
      APPEND cf_ref_dir TO ct_undo.
    ENDIF.

    IF cf_ref_dir IS BOUND.
      DATA l_class TYPE abap_abstypename.
      l_class = cl_abap_classdescr=>get_class_name( cf_ref_dir ).

      CREATE OBJECT cf_ref_dir TYPE (l_class).
    ELSE.
      CREATE OBJECT cf_ref_dir TYPE zapcmd_cl_frontend_dir.
    ENDIF.

    CALL METHOD cf_ref_dir->init
      EXPORTING
        pf_full_name = pf_dir.

    CALL METHOD reload_dir.

  ENDMETHOD.


  METHOD refresh.

    DATA ls_layout TYPE lvc_s_layo.
    DATA lf_title TYPE string.

    CALL METHOD get_title
      IMPORTING
        pf_title = lf_title.


    CALL METHOD cf_gui_alv->get_frontend_layout
      IMPORTING
        es_layout = ls_layout.
    ls_layout-grid_title = lf_title.
    CALL METHOD cf_gui_alv->set_frontend_layout
      EXPORTING
        is_layout = ls_layout.

*    ensure_sort_priority( ).

    CALL METHOD cf_gui_alv->refresh_table_display.


*  CALL METHOD cl_gui_control=>set_focus EXPORTING control = grid2.
    CALL METHOD cl_gui_cfw=>flush.
    IF sy-subrc NE 0.
      DATA lf_repid TYPE string.
      lf_repid = sy-repid.
* add your handling, for example
      CALL FUNCTION 'POPUP_TO_INFORM'
        EXPORTING
          titel = lf_repid
          txt2  = sy-subrc
          txt1  = 'Error in FLush'(500).
    ENDIF.

    RAISE EVENT set_active.

  ENDMETHOD.


  METHOD reload_dir.


    DATA ls_fileinto TYPE zapcmd_file_descr.

    DATA lf_file TYPE REF TO zapcmd_cl_knot.

    CALL METHOD cf_ref_dir->read_dir
      IMPORTING
        pt_filelist       = ct_files
      EXCEPTIONS
        permission_denied = 1.

    IF sy-subrc <> 0.
      MESSAGE ID sy-msgid TYPE 'I' NUMBER sy-msgno
        WITH sy-msgv1 sy-msgv2 sy-msgv3 sy-msgv4.
      EXIT.
    ENDIF.

    REFRESH ct_fileinfo.
    DATA lf_index TYPE i VALUE 0.
    LOOP AT ct_files INTO lf_file.
      lf_index = lf_index + 1.
      CALL METHOD lf_file->get_info
        RECEIVING
          psx_fileinfo = ls_fileinto.
      ls_fileinto-indx = lf_index.
      APPEND ls_fileinto TO ct_fileinfo.
    ENDLOOP.

  ENDMETHOD.


  METHOD root_dir.

    IF cf_ref_dir IS BOUND.
      APPEND cf_ref_dir TO ct_undo.
    ENDIF.

    DATA lf_rootdir TYPE string.
    lf_rootdir = cf_ref_dir->separator.

    DATA l_class TYPE abap_abstypename.
    l_class = cl_abap_classdescr=>get_class_name( cf_ref_dir ).

    CREATE OBJECT cf_ref_dir TYPE (l_class).

    CALL METHOD cf_ref_dir->init
      EXPORTING
        pf_full_name = lf_rootdir.

    CALL METHOD reload_dir.

  ENDMETHOD.


  METHOD show.

    DATA lf_title TYPE string.
    CALL METHOD get_title
      IMPORTING
        pf_title = lf_title.

    IF cf_gui_alv IS INITIAL.
      CREATE OBJECT cf_gui_alv
        EXPORTING
          i_parent = pf_container.

      SET HANDLER handle_doubleclick FOR cf_gui_alv.
      SET HANDLER handle_set_toolbar FOR cf_gui_alv.
      SET HANDLER handle_usercommand FOR cf_gui_alv.
      SET HANDLER handle_contextmenu  FOR cf_gui_alv.
      SET HANDLER handle_drag FOR cf_gui_alv.
      SET HANDLER handle_drop FOR cf_gui_alv.

      DATA ls_fileinto TYPE zapcmd_file_descr.



      DATA ls_layout TYPE lvc_s_layo.
      ls_layout-zebra = 'X'.
      ls_layout-cwidth_opt = 'X'.
      ls_layout-no_headers = ''.
      ls_layout-sel_mode = 'A'.
      ls_layout-smalltitle = 'X'.
      ls_layout-grid_title = lf_title.
      ls_layout-cwidth_opt = 'X'.



      DATA lt_toolbarexcludings TYPE ui_functions.
      REFRESH lt_toolbarexcludings.
      APPEND cf_gui_alv->mc_fc_sum TO lt_toolbarexcludings.
      APPEND cf_gui_alv->mc_fc_subtot TO lt_toolbarexcludings.
      APPEND cf_gui_alv->mc_fc_find TO lt_toolbarexcludings.
      APPEND cf_gui_alv->mc_fc_views TO lt_toolbarexcludings.
      APPEND cf_gui_alv->mc_fc_info TO lt_toolbarexcludings.
      APPEND cf_gui_alv->mc_fc_graph TO lt_toolbarexcludings.


      DATA lt_fieldcatalog TYPE lvc_t_fcat.

      FIELD-SYMBOLS <wrk_fieldcat> TYPE lvc_s_fcat.

      CALL FUNCTION 'LVC_FIELDCATALOG_MERGE'
        EXPORTING
          i_structure_name       = 'ZAPCMD_FILE_DESCR'
          i_client_never_display = 'X'
        CHANGING
          ct_fieldcat            = lt_fieldcatalog
        EXCEPTIONS
          inconsistent_interface = 1
          program_error          = 2
          OTHERS                 = 3.

      LOOP AT lt_fieldcatalog ASSIGNING <wrk_fieldcat>.

        CASE <wrk_fieldcat>-fieldname.
          WHEN 'ICON'.
            <wrk_fieldcat>-icon = abap_true.
            <wrk_fieldcat>-col_pos = 1.
          WHEN 'SHORTNAME'.
            <wrk_fieldcat>-col_pos = 2.
          WHEN 'EXTENSION'.
            <wrk_fieldcat>-col_pos = 3.
          WHEN 'FILESIZE'.
            <wrk_fieldcat>-col_pos = 4.
          WHEN 'MODDATE'.
            <wrk_fieldcat>-col_pos = 5.
          WHEN 'MODTIME'.
            <wrk_fieldcat>-col_pos = 6.
          WHEN 'ATTR'.
            <wrk_fieldcat>-col_pos = 7.
          WHEN OTHERS.
            <wrk_fieldcat>-no_out = abap_true.

        ENDCASE.
      ENDLOOP.


      DATA ls_variant TYPE disvariant.
      DATA l_save TYPE char01.
      ls_variant-report = sy-repid.
      l_save = 'U'.

      DATA lt_sort TYPE lvc_t_sort.
      DATA ls_sort TYPE lvc_s_sort.
      ls_sort-spos = 1.
      ls_sort-fieldname = c_field_sort_prio.
      ls_sort-up = 'X'.
      ls_sort-down = ''.
      ls_sort-obligatory = abap_true.
      APPEND ls_sort TO lt_sort.

      CLEAR ls_sort.
      ls_sort-spos = 2.
      ls_sort-fieldname = 'FILETYPE'.
      ls_sort-up = ''.
      ls_sort-down = 'X'.
      APPEND ls_sort TO lt_sort.

      CLEAR ls_sort.
      ls_sort-spos = 3.
      ls_sort-fieldname = 'NAME'.
      ls_sort-up = 'X'.
      ls_sort-down = ''.
      APPEND ls_sort TO lt_sort.


      DATA:  effect TYPE i.
      DATA:  handle_alv TYPE i.

      DATA l_cl_dragdrop TYPE REF TO cl_dragdrop.

      CREATE OBJECT l_cl_dragdrop.
      effect = cl_dragdrop=>copy.
      CALL METHOD l_cl_dragdrop->add
        EXPORTING
          flavor     = 'COPYFILE'
          dragsrc    = 'X'
          droptarget = 'X'
          effect     = effect.
*      effect = cl_dragdrop=>move.
*    CALL METHOD l_cl_dragdrop->add
*      EXPORTING
*        flavor     = 'MOVEFILE'
*        dragsrc    = 'X'
*        droptarget = 'X'
*        effect     = effect.
      CALL METHOD l_cl_dragdrop->get_handle
        IMPORTING
          handle = handle_alv.

      ls_layout-s_dragdrop-row_ddid = handle_alv.



      CALL METHOD cf_gui_alv->set_table_for_first_display
        EXPORTING
          is_variant           = ls_variant
          i_save               = l_save
          is_layout            = ls_layout
          it_toolbar_excluding = lt_toolbarexcludings
        CHANGING
          it_fieldcatalog      = lt_fieldcatalog
          it_outtab            = ct_fileinfo
          it_sort              = lt_sort.
    ELSE.
      cf_gui_alv->set_adjust_design(
        EXPORTING
          adjust_design     = 1
*        EXCEPTIONS
*          CNTL_ERROR        = 1
*          CNTL_SYSTEM_ERROR = 2
*          others            = 3
             ).

      CALL METHOD cf_gui_alv->get_frontend_layout
        IMPORTING
          es_layout = ls_layout.
      ls_layout-grid_title = lf_title.
      CALL METHOD cf_gui_alv->set_frontend_layout
        EXPORTING
          is_layout = ls_layout.

      DATA l_stable TYPE lvc_s_stbl.
      l_stable-col = 'X'.
      l_stable-row = 'X'.
      cf_gui_alv->refresh_table_display(
        is_stable = l_stable
        i_soft_refresh = 'X' ).

    ENDIF.

*  CALL METHOD cl_gui_control=>set_focus EXPORTING control = grid2.
    CALL METHOD cl_gui_cfw=>flush.
    IF sy-subrc NE 0.
      DATA lf_repid TYPE string.
      lf_repid = sy-repid.
* add your handling, for example
      CALL FUNCTION 'POPUP_TO_INFORM'
        EXPORTING
          titel = lf_repid
          txt2  = sy-subrc
          txt1  = 'Error in FLush'(500).
    ENDIF.

  ENDMETHOD.


  METHOD undo.

    DATA l_lines TYPE i.
    l_lines = lines( ct_undo ).
    IF l_lines = 0.
      RETURN.
    ELSE.
      READ TABLE ct_undo INDEX l_lines
        INTO cf_ref_dir.
      DELETE ct_undo INDEX l_lines.
      CALL METHOD reload_dir.
      CALL METHOD refresh.
    ENDIF.

  ENDMETHOD.

ENDCLASS.
