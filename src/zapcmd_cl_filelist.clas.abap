class ZAPCMD_CL_FILELIST definition
  public
  final
  create public .

*"* public components of class ZAPCMD_CL_FILELIST
*"* do not include other source files here!!!
public section.
  type-pools ICON .

  data CF_ACTIVE type XFELD .
  data CF_REF_DIR type ref to ZAPCMD_CL_DIR .

  events SET_ACTIVE .

  methods LOAD_DIR
    importing
      !PF_DIR type STRING optional .
  methods RELOAD_DIR .
  methods ROOT_DIR .
  methods DELETE
    importing
      !PT_FILES type ZAPCMD_TBL_FILELIST .
  methods EDIT_DIR .
  methods SHOW
    importing
      !PF_CONTAINER type ref to CL_GUI_CONTAINER .
  methods CONSTRUCTOR
    importing
      !PF_TYPE type SYUCOMM default 'FRONTEND'
      !PF_DIR type STRING optional .
  methods REFRESH .
  methods HANDLE_ACTIVATE
    for event SET_ACTIVE of ZAPCMD_CL_FILELIST
    importing
      !SENDER .
  methods GET_FILES
    returning
      value(PTX_FILES) type ZAPCMD_TBL_FILELIST .
  methods GET_DIR
    returning
      value(PFX_DIR) type ref to ZAPCMD_CL_DIR .
  methods CHECK_ACTIVE
    importing
      !PF_CONTROL type ref to CL_GUI_CONTROL optional .
  methods UNDO .
  methods HANDLE_CONTEXTMENU
    for event CONTEXT_MENU_REQUEST of CL_GUI_ALV_GRID
    importing
      !E_OBJECT .
  methods COPY
    importing
      !PT_FILES type ZAPCMD_TBL_FILELIST
      !PF_DESTDIR type ref to ZAPCMD_CL_DIR .
  class-methods GET_IMP .
protected section.
*"* protected components of class ZAPCMD_CL_FILELIST
*"* do not include other source files here!!!
private section.
*"* private components of class ZAPCMD_CL_FILELIST
*"* do not include other source files here!!!

  data CF_GUI_ALV type ref to CL_GUI_ALV_GRID .
  data CT_FILES type ZAPCMD_TBL_FILELIST .
  data CT_FILEINFO type ZAPCMD_TBL_FILE_INFO .
  data:
    ct_undo type table of ref to zaPCMD_cl_dir .
  class-data GT_FCODE_FACTORY type ZAPCMD_TBL_FCODE_FACTORY .
  class-data GT_FACTORY_BUTTON type TTB_BUTTON .

  methods HANDLE_DOUBLECLICK
    for event DOUBLE_CLICK of CL_GUI_ALV_GRID
    importing
      !E_ROW .
  methods HANDLE_SET_TOOLBAR
    for event TOOLBAR of CL_GUI_ALV_GRID
    importing
      !E_OBJECT .
  methods HANDLE_USERCOMMAND
    for event USER_COMMAND of CL_GUI_ALV_GRID
    importing
      !E_UCOMM .
  methods GET_TITLE
    exporting
      !PF_TITLE type STRING .
  methods HANDLE_DRAG
    for event ONDRAG of CL_GUI_ALV_GRID
    importing
      !E_ROW
      !E_COLUMN
      !ES_ROW_NO
      !E_DRAGDROPOBJ .
  methods HANDLE_DROP
    for event ONDROP of CL_GUI_ALV_GRID
    importing
      !SENDER
      !E_ROW
      !E_COLUMN
      !ES_ROW_NO
      !E_DRAGDROPOBJ .
  methods GET_FACTORIES
    returning
      value(ET_IMP) type ZAPCMD_TBL_FACTORY .
  methods GET_FACTORY_BUTTONS
    changing
      !CT_BUTTON type TTB_BUTTON .
ENDCLASS.



CLASS ZAPCMD_CL_FILELIST IMPLEMENTATION.


method CHECK_ACTIVE.

    data lf_control type ref to cl_gui_control.
    lf_control = pf_control.
    if lf_control is initial.

      CALL METHOD CL_GUI_CONTROL=>GET_FOCUS
        IMPORTING
          CONTROL           = lf_control
        EXCEPTIONS
          CNTL_ERROR        = 1
          CNTL_SYSTEM_ERROR = 2
          others            = 3.
      IF SY-SUBRC <> 0.
*        MESSAGE ID SY-MSGID TYPE SY-MSGTY NUMBER SY-MSGNO
*                   WITH SY-MSGV1 SY-MSGV2 SY-MSGV3 SY-MSGV4.
      ENDIF.


    endif.
    if lf_control = cf_gui_alv.
      raise event set_active.
    endif.

endmethod.


method CONSTRUCTOR.

     set handler handle_activate for all instances.

     data l_fcode type syucomm.
     if pf_type = 'DEFAULT'.
       l_fcode = 'FRONTEND'.
     else.
       l_fcode = pf_type.
     endif.


     data lt_imp TYPE ZAPCMD_TBL_FACTORY.
     lt_imp = get_factories( ).
     data l_imp type ref to zapcmd_if_factory.
     loop at lt_imp into l_imp.
       cf_ref_dir = l_imp->create_dir( i_fcode = l_fcode  i_dir = pf_dir ).
       if cf_ref_dir is bound.
         exit.
       endif.
     endloop.

     if cf_ref_dir is bound.
       reload_dir( ).
     endif.



endmethod.


method COPY.

     DATA lf_file TYPE REF TO ZAPCMD_cl_knot.
    DATA lf_sourcefile TYPE REF TO ZAPCMD_cl_file.
    DATA lf_destfile TYPE REF TO ZAPCMD_cl_file.
    DATA lt_file TYPE ZAPCMD_tbl_xstring.
    data lf_answer type c.
    lf_answer = '1'.

    LOOP AT pt_files INTO lf_file
       WHERE table_line->filetype <> 'DIR'
         and table_line->filetype <> 'UP'.

      lf_sourcefile ?= lf_file.
      DATA lf_string TYPE string.
      CONCATENATE 'COPY: ' lf_sourcefile->name INTO lf_string.

      CALL FUNCTION 'SAPGUI_PROGRESS_INDICATOR'
        EXPORTING
          percentage = 0
          text       = lf_string.

      if pf_destdir->check_fileexist( lf_sourcefile->name ) = abap_true.
        IF lf_answer <> '2'.

          CONCATENATE '"' lf_sourcefile->name '"' ' existiert.'(006) ' Überschreiben?'(007) INTO
            lf_string.

          CALL FUNCTION 'POPUP_TO_CONFIRM'
            EXPORTING
              titlebar                    = lf_sourcefile->full_name
*             DIAGNOSE_OBJECT             = ' '
              text_question               = lf_string
              text_button_1               = 'Ja'(008)
              icon_button_1               = ' '
              text_button_2               = 'Ja, Alle'(009)
              icon_button_2               = ' '
              default_button              = '1'
              display_cancel_button       = 'X'
*             USERDEFINED_F1_HELP         = ' '
*             START_COLUMN                = 25
*             START_ROW                   = 6
*             POPUP_TYPE                  =
           IMPORTING
              answer                      = lf_answer
*           TABLES
*             PARAMETER                   =
             EXCEPTIONS
               text_not_found              = 1
               OTHERS                      = 2
                    .
          IF sy-subrc <> 0.
            MESSAGE ID sy-msgid TYPE sy-msgty NUMBER sy-msgno
                    WITH sy-msgv1 sy-msgv2 sy-msgv3 sy-msgv4.
          ENDIF.
          if lf_answer = 'A'.
**          Abbruch.
            return.
          endif.

        ENDIF.


      endif.

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
      DATA lf_destdir TYPE REF TO ZAPCMD_cl_dir.

      CALL METHOD pf_destdir->create_dir
        EXPORTING
          pf_filename = lf_file->name
        IMPORTING
          pf_file     = lf_destdir.

      DATA lf_files TYPE ZAPCMD_tbl_filelist.
      DATA lf_dir TYPE REF TO ZAPCMD_cl_dir.
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


endmethod.


method DELETE.

    DATA lf_file TYPE REF TO ZAPCMD_cl_knot.
    DATA lf_answer TYPE c.

    LOOP AT pt_files INTO lf_file.

      IF lf_file->name <> '..'.

        IF lf_answer <> '2'.

          DATA lf_string TYPE string.
          CONCATENATE '"' lf_file->name '"' ' löschen?'(010) INTO
            lf_string.

          CALL FUNCTION 'POPUP_TO_CONFIRM'
            EXPORTING
              titlebar                    = lf_file->full_name
*             DIAGNOSE_OBJECT             = ' '
              text_question               = lf_string
              text_button_1               = 'Ja'(008)
              icon_button_1               = ' '
              text_button_2               = 'Ja, Alle'(009)
              icon_button_2               = ' '
              default_button              = '1'
              display_cancel_button       = 'X'
*             USERDEFINED_F1_HELP         = ' '
*             START_COLUMN                = 25
*             START_ROW                   = 6
*             POPUP_TYPE                  =
           IMPORTING
              answer                      = lf_answer
*           TABLES
*             PARAMETER                   =
             EXCEPTIONS
               text_not_found              = 1
               OTHERS                      = 2
                    .
          IF sy-subrc <> 0.
            MESSAGE ID sy-msgid TYPE sy-msgty NUMBER sy-msgno
                    WITH sy-msgv1 sy-msgv2 sy-msgv3 sy-msgv4.
          ENDIF.

        ENDIF.

        IF lf_answer = '1' OR lf_answer = '2'.
          IF lf_file->filetype = 'DIR'.
            DATA lf_files TYPE ZAPCMD_tbl_filelist.
            DATA lf_dir TYPE REF TO ZAPCMD_cl_dir.
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

endmethod.


method EDIT_DIR.



 data values type table of SVAL.
 data value type sval.
 value-tabname = 'ZAPCMD_FILE_DESCR'.
 value-fieldname = 'FULL_NAME'.
 value-FIELDTEXT = 'Pfad:'(002).
 append value to values.


 CALL FUNCTION 'POPUP_GET_VALUES'
   EXPORTING
*    NO_VALUE_CHECK        = ' '
     popup_title           = 'Direkte Pfadeingabe'(202)
*    START_COLUMN          = '5'
*    START_ROW             = '5'
*  IMPORTING
*    RETURNCODE            =
   tables
     fields                = values
*  EXCEPTIONS
*    ERROR_IN_FIELDS       = 1
*    OTHERS                = 2
           .
 IF sy-subrc <> 0.
* MESSAGE ID SY-MSGID TYPE SY-MSGTY NUMBER SY-MSGNO
*         WITH SY-MSGV1 SY-MSGV2 SY-MSGV3 SY-MSGV4.
 ENDIF.

 loop at values into value.
 endloop.

    data lf_rootdir type string.
    lf_rootdir = value-value.

    data l_class TYPE ABAP_ABSTYPENAME.
    l_class = cl_abap_classdescr=>get_class_name( cf_ref_dir ).

    create object cf_ref_dir type (l_class).

    call method cf_ref_dir->init
      EXPORTING
        pf_full_name = lf_rootdir.

    call method reload_dir.



endmethod.


method GET_DIR.

   pfx_dir = cf_ref_dir.

endmethod.


method GET_FACTORIES.


  DATA lt_imp TYPE TABLE OF seoclsname.

  data: begin of ls_para_tab,
          name type abap_parmname,
          tabname type tabname,
          fieldname type fieldname,
        end of ls_para_tab.
  data lt_para_tab like table of ls_para_tab.


  SELECT clsname FROM seometarel INTO TABLE lt_imp
    WHERE refclsname = 'ZAPCMD_IF_FACTORY'.

  IF lt_imp IS NOT INITIAL.
    SELECT clsname FROM seometarel APPENDING TABLE lt_imp
      FOR ALL ENTRIES IN lt_imp
      WHERE refclsname = lt_imp-table_line.
  ENDIF.

  SORT lt_imp.
  DELETE ADJACENT DUPLICATES FROM lt_imp.

  refresh et_imp.

  data l_imp type seoclsname.
  data lO_OBJECT type ref to zapcmd_if_factory.
  loop at lt_imp into l_imp.
    create object lo_object type (l_imp).
    append lo_object to et_imp.
  endloop.

endmethod.


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


method GET_FILES.

     data lf_lines type i.

    data lt_rows type LVC_T_ROW.
    data ls_row type lvc_s_row.
    call method cf_gui_alv->GET_SELECTED_ROWS
      IMPORTING
        ET_INDEX_ROWS = lt_rows.

    describe table lt_rows lines lf_lines.
    if lf_lines = 0.
      data lt_cells type LVC_T_CELL.
      data ls_cell type lvc_s_CELL.

      CALL METHOD CF_GUI_ALV->GET_SELECTED_CELLS
        IMPORTING
          ET_CELL = lt_cells.
      loop at lt_cells into ls_cell.
        append ls_cell-row_ID to lt_rows.
      endloop.
      delete adjacent duplicates from lt_rows comparing index.
    endif.

    loop at lt_rows into ls_row.

      data ls_fileinfo type zapcmd_file_descr.
      data lf_file type ref to zapcmd_cl_knot.
      read table ct_fileinfo index ls_row-index into ls_fileinfo.

      read table ct_files index ls_fileinfo-indx into lf_file.
      append lf_file to ptx_files.

    endloop.

endmethod.


METHOD get_imp.

  DATA lo_object TYPE REF TO zapcmd_cl_dir.
  DATA lt_imp TYPE TABLE OF seoclsname.

  data: begin of ls_para_tab,
          name type abap_parmname,
          tabname type tabname,
          fieldname type fieldname,
        end of ls_para_tab.
  data lt_para_tab like table of ls_para_tab.


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
  FIELD-SYMBOLS <value> type any.
  LOOP AT lt_imp INTO l_imp.
    CLEAR l_tab_parmbind.
    CLEAR l_tab_excpbind.
    CLEAR lt_sval.
    clear lt_para_tab.
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
        data l_rollname type dd03l-rollname.
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
          insert ls_para_tab into table lt_para_tab.
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
*         NO_VALUE_CHECK        = ' '
         popup_title           = 'Werteingabe'(018)
*         START_COLUMN          = '5'
*         START_ROW             = '5'
*       IMPORTING
*         RETURNCODE            = RETURNCODE
       TABLES
         fields                = lt_sval
*       EXCEPTIONS
*         ERROR_IN_FIELDS       = 1
*         OTHERS                = 2
               .
      IF sy-subrc <> 0.
* MESSAGE ID SY-MSGID TYPE SY-MSGTY NUMBER SY-MSGNO
*         WITH SY-MSGV1 SY-MSGV2 SY-MSGV3 SY-MSGV4.
      ENDIF.
      loop at lt_sval into ls_sval.
       loop at lt_para_tab into ls_para_tab
          where tabname = ls_sval-tabname
           and  fieldname = ls_sval-fieldname.
       endloop.
       read table l_tab_parmbind into l_parmbind with table key name =  ls_para_tab-name.
       if sy-subrc = 0.
         ASSIGN l_parmbind-value->* to <value>.
         <value> = ls_sval-value.
       endif.
      endloop.

    ENDIF.



    CREATE OBJECT lo_object
      TYPE
        (l_imp)
      PARAMETER-TABLE
        l_tab_parmbind
      EXCEPTION-TABLE
        l_tab_excpbind.
      if sy-subrc <> 0.
        message 'Nicht instanziierbar.' type 'E'.
      endif.




  ENDLOOP.



ENDMETHOD.


method GET_TITLE.


*      case cf_ref_dir->SERVER_AREA.
*        when 'A'. lf_areastring = 'Appl. Serv.'(014).
*        when 'P'. lf_areastring = 'Frontend'(015).
*        when 'R'. lf_areastring = 'RFC'(016).
*        when others. lf_areastring = cf_ref_dir->SERVER_AREA.
*      endcase.


    data lf_space type p.
    call method cf_ref_dir->get_freespace
      IMPORTING
        pf_space = lf_space.
    if lf_space > 0.
      data lf_spacestr type string.
      call method zapcmd_cl_file=>convert_size
        EXPORTING
          pf_size     = lf_space
        IMPORTING
          pfx_sizestr = lf_spacestr.

      concatenate
        '<' cf_ref_dir->area_string '>°'
        cf_ref_dir->full_name ' (' lf_spacestr ' frei)'
        into pf_title.
    else.
      concatenate
        '<' cf_ref_dir->area_string '>°'
        cf_ref_dir->full_name
        into pf_title.
    endif.
    TRANSLATE pf_title USING '° '.

endmethod.


method HANDLE_ACTIVATE.

    if me = sender.
      cf_active = abap_true.
    else.
      cf_active = abap_false.
    endif.

endmethod.


method HANDLE_CONTEXTMENU.

 e_object->ADD_SEPARATOR( ).

  e_object->add_function(
      fcode       = zapcmd_cl_dir=>co_rename
      text        = 'Umbenennen'(231)
      ICON        = ICON_RENAME
*      FTYPE       = FTYPE
*      DISABLED    = DISABLED
*      HIDDEN      = HIDDEN
*      CHECKED     = CHECKED
*      ACCELERATOR = ACCELERATOR
         ).

   e_object->add_function(
      fcode       = zapcmd_cl_dir=>co_delete
      text        = 'Löschen'(011)
      ICON        = ICON_delete
*      FTYPE       = FTYPE
*      DISABLED    = DISABLED
*      HIDDEN      = HIDDEN
*      CHECKED     = CHECKED
*      ACCELERATOR = ACCELERATOR
         ).



endmethod.


method HANDLE_DOUBLECLICK.

*    raise event set_active.
*    exporting sender = me.

    data lf_file type ref to zapcmd_cl_knot.
    data ls_fileinto type zapcmd_file_descr.
    data ls_fileinfo type zapcmd_file_descr.
    read table ct_fileinfo index e_row-index into ls_fileinfo.

    read table ct_files index ls_fileinfo-indx into lf_file.

    if lf_file->is_dir = abap_true.
      if cf_ref_dir is bound.
        append cf_ref_dir to ct_undo.
      endif.
      cf_ref_dir ?= lf_file.
      if sy-subrc = 0.
        call method cf_ref_dir->read_dir
          IMPORTING
            pt_filelist = ct_files
          exceptions
            permission_denied = 1.
        if sy-subrc <> 0.
          message id sy-msgid type 'I' number sy-msgno
            with sy-msgv1 sy-msgv2 sy-msgv3 sy-msgv4.
          exit.
        endif.

        refresh ct_fileinfo.
        data lf_index type i value 0.
        loop at ct_files into lf_file.
          lf_index = lf_index + 1.
          call method lf_file->get_info
            RECEIVING
              psx_fileinfo = ls_fileinto.
          ls_fileinto-indx = lf_index.
          append ls_fileinto to ct_fileinfo.
        endloop.

        call Method refresh.
      endif.
    else.
      call method lf_file->execute.
    endif.

    raise event set_active.

    CALL METHOD cl_gui_cfw=>flush.
    if sy-subrc <> 0.
    endif.

endmethod.


method HANDLE_DRAG.

    data lo_list type ref to zapcmd_cl_knotlist.
    data lo_knot type ref to zapcmd_cl_knot.

    create object lo_list.

    DATA LT_selected_rows TYPE LVC_T_ROW.
    data l_row type lvc_s_row.

    cf_gui_alv->get_selected_rows(
      IMPORTING
        ET_INDEX_ROWS = LT_selected_rows
    ).

    if LT_selected_rows is not INITIAL.
    read TABLE LT_selected_rows TRANSPORTING NO FIELDS
      with key table_line = e_row.
    if sy-subrc <> 0.
      e_dragdropobj->abort( ).
      return.
    endif.
      loop at LT_selected_rows into l_row.

         data ls_fileinfo type zapcmd_file_descr.
         read table ct_fileinfo index l_row-index into ls_fileinfo.
         read table ct_files index ls_fileinfo-indx into lo_knot.

        lo_list->add( lo_knot ).

      endloop.
    else.
      read table ct_fileinfo index e_row-index into ls_fileinfo.
      read table ct_files index ls_fileinfo-indx into lo_knot.

        lo_list->add( lo_knot ).


    endif.


  e_dragdropobj->object = lo_list.

endmethod.


METHOD handle_drop.

  DATA lo_list TYPE REF TO zapcmd_cl_knotlist.

  IF e_dragdropobj->droptargetctrl = e_dragdropobj->dragsourcectrl.
    e_dragdropobj->abort( ).
    RETURN.
  ENDIF.

  DATA lf_file TYPE REF TO zapcmd_cl_knot.
  TRY.
      lo_list ?= e_dragdropobj->object.

      CALL METHOD copy
        EXPORTING
          pt_files   = lo_list->ct_list
          pf_destdir = cf_ref_dir.


    CATCH cx_root.
      e_dragdropobj->abort( ).
  ENDTRY.

ENDMETHOD.


METHOD handle_set_toolbar.

  DATA: ls_toolbar TYPE stb_button.

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
    APPEND ls_toolbar TO e_object->mt_toolbar.

    MOVE 0 TO ls_toolbar-butn_type.
    MOVE zapcmd_cl_dir=>co_edit_dir TO ls_toolbar-function.
    MOVE icon_fast_entry TO ls_toolbar-icon.
    MOVE 'Direkte Pfadeingabe'(202) TO ls_toolbar-quickinfo.
    MOVE space TO ls_toolbar-disabled.
    APPEND ls_toolbar TO e_object->mt_toolbar.

    DATA lt_toolbar TYPE ttb_button.
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
  RAISE EVENT set_active.

  DATA l_dir_temp TYPE REF TO zapcmd_cl_dir.

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
    else.
      message 'Verzeichnis konnte nicht eingelesen werden.'(017) type 'I' DISPLAY LIKE 'E'.
      return.
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
            popup_title           = 'Neuer Dateiname'(001)
*             START_COLUMN          = '5'
*             START_ROW             = '5'
*           IMPORTING
*             RETURNCODE            = RETURNCODE
          TABLES
            fields                = lt_fields
*           EXCEPTIONS
*             ERROR_IN_FIELDS       = 1
*             OTHERS                = 2
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


method REFRESH.

    data ls_layout type LVC_S_LAYO.


    data lf_title type string.
    call method get_title
      importing
        pf_title = lf_title.


    call method cf_gui_alv->get_frontend_layout
      IMPORTING
        es_layout = ls_layout.
    ls_layout-GRID_TITLE = lf_title.
    call method cf_gui_alv->set_frontend_layout
      EXPORTING
        is_layout = ls_layout.
    CALL METHOD cf_gui_alv->refresh_table_display.

*  CALL METHOD cl_gui_control=>set_focus EXPORTING control = grid2.
    CALL METHOD cl_gui_cfw=>flush.
    IF sy-subrc ne 0.
      data lf_repid type string.
      lf_repid = sy-repid.
* add your handling, for example
      CALL FUNCTION 'POPUP_TO_INFORM'
           EXPORTING
                titel = lf_repid
                txt2  = sy-subrc
                txt1  = 'Error in FLush'(500).
    ENDIF.

    raise event set_active.

endmethod.


method RELOAD_DIR.


    data ls_fileinto type zapcmd_file_descr.

    data lf_file type ref to zapcmd_cl_knot.

    call method cf_ref_dir->read_dir
      IMPORTING
        pt_filelist = ct_files
      exceptions
        PERMISSION_DENIED = 1.

    if sy-subrc <> 0.
      message id sy-msgid type 'I' number sy-msgno
        with sy-msgv1 sy-msgv2 sy-msgv3 sy-msgv4.
      exit.
    endif.

    refresh ct_fileinfo.
    data lf_index type i value 0.
    loop at ct_files into lf_file.
      lf_index = lf_index + 1.
      call method lf_file->get_info
        RECEIVING
          psx_fileinfo = ls_fileinto.
      ls_fileinto-indx = lf_index.
      append ls_fileinto to ct_fileinfo.
    endloop.

endmethod.


method ROOT_DIR.

     if cf_ref_dir is bound.
      append cf_ref_dir to ct_undo.
    endif.

    data lf_rootdir type string.
    lf_rootdir = cf_ref_dir->separator.

    data l_class TYPE ABAP_ABSTYPENAME.
    l_class = cl_abap_classdescr=>get_class_name( cf_ref_dir ).

    create object cf_ref_dir type (l_class).

    call method cf_ref_dir->init
      EXPORTING
        pf_full_name = lf_rootdir.

    call method reload_dir.

endmethod.


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
    ls_sort-fieldname = 'FILETYPE'.
    ls_sort-up = ''.
    ls_sort-down = 'X'.
    APPEND ls_sort TO lt_sort.
    ls_sort-spos = 2.
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


method UNDO.

    data l_lines type i.
    l_lines = lines( ct_undo ).
    if l_lines = 0.
      return.
    else.
      read table ct_undo index l_lines
        into cf_ref_dir.
      delete ct_undo index l_lines.
      call method reload_dir.
      call Method refresh.
    endif.

endmethod.
ENDCLASS.
