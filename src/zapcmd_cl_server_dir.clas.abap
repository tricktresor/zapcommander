class ZAPCMD_CL_SERVER_DIR definition
  public
  inheriting from ZAPCMD_CL_DIR
  final
  create public .

*"* public components of class ZAPCMD_CL_SERVER_DIR
*"* do not include other source files here!!!
public section.
  type-pools ICON .

  methods CONSTRUCTOR .

  methods CREATE_DIR
    redefinition .
  methods CREATE_FILE
    redefinition .
  methods DELETE
    redefinition .
  methods GET_FREESPACE
    redefinition .
  methods GET_TOOLBAR
    redefinition .
  methods INIT
    redefinition .
  methods READ_DIR
    redefinition .
  methods RENAME
    redefinition .
  methods CREATE_NEW
    redefinition .
protected section.
*"* protected components of class ZAPCMD_CL_SERVER_DIR
*"* do not include other source files here!!!
private section.
*"* private components of class ZAPCMD_CL_SERVER_DIR
*"* do not include other source files here!!!

  methods READ_DRIVES
    exporting
      value(PT_FILELIST) type ZAPCMD_TBL_FILELIST .
  methods READ_SERVER
    exporting
      value(PT_FILELIST) type ZAPCMD_TBL_FILELIST .
  methods READ_AL11
    exporting
      value(PT_FILELIST) type ZAPCMD_TBL_FILELIST .
  methods READ_LOGICALDIR
    exporting
      value(PT_FILELIST) type ZAPCMD_TBL_FILELIST .
ENDCLASS.



CLASS ZAPCMD_CL_SERVER_DIR IMPLEMENTATION.


method CONSTRUCTOR.

* ...
    call method super->constructor.
    if sy-opsys = 'Windows NT' or sy-opsys = 'DOS'. "#EC NOTEXT
      separator = '\'.
    else.
      separator = '/'.
    endif.
    server_area = 'A'.
    AREA_STRING = 'Appl. Server'(005).

endmethod.


method CREATE_DIR.

    create object pf_file type Zapcmd_CL_SERVER_DIR.

    call method pf_file->init
      EXPORTING
        pf_name    = pf_filename
        pf_size    = 0
        pf_dir     = full_name
        pf_moddate = sy-datum
        pf_modtime = sy-uzeit
        pf_attr    = space.

    call method exec_server
      exporting
        pf_command = 'mkdir'
        pf_parameter = pf_file->full_name.

endmethod.


method CREATE_FILE.

    create object pf_file type ZAPCMD_CL_SERVER_FILE.

    call method pf_file->init
      EXPORTING
        pf_name    = pf_filename
        pf_size    = 0
        pf_dir     = full_name
        pf_moddate = sy-datum
        pf_modtime = sy-uzeit
        pf_attr    = space.

endmethod.


METHOD create_new.

  CASE i_fcode.
    WHEN co_al11.
      CREATE OBJECT eo_dir TYPE zapcmd_cl_server_dir.
      eo_dir->init( pf_full_name = co_al11dir ).
    WHEN co_server.
      CREATE OBJECT eo_dir TYPE zapcmd_cl_server_dir.
      eo_dir->init( pf_full_name = co_rootdir ).
    WHEN co_drives.
      CREATE OBJECT eo_dir TYPE zapcmd_cl_server_dir.
      eo_dir->init( pf_full_name = me->separator ).
    WHEN co_logicalfile.
      DATA values TYPE TABLE OF sval.
      DATA value TYPE sval.
      value-tabname = 'PATH'.
      value-fieldname = 'PATHINTERN'.
      value-fieldtext = 'Logischer Pfad'(012).
      APPEND value TO values.


      DATA l_subrc TYPE char1.
      CALL FUNCTION 'POPUP_GET_VALUES'
        EXPORTING
*    NO_VALUE_CHECK        = ' '
          popup_title           = 'logischer Pfad'(013)
*    START_COLUMN          = '5'
*    START_ROW             = '5'
      IMPORTING
          returncode            = l_subrc
        TABLES
          fields                = values
      EXCEPTIONS
        error_in_fields       = 1
        OTHERS                = 2
                .
      IF sy-subrc <> 0 AND l_subrc <> ''.
        CLEAR eo_dir.
        RETURN.
      ENDIF.

      LOOP AT values INTO value.
      ENDLOOP.

      DATA l_path TYPE filepath-pathintern.
      l_path = value-value.
      IF l_path IS NOT INITIAL.

        CREATE OBJECT eo_dir
          TYPE
            zapcmd_cl_server_dir.

        DATA lf_dir TYPE string.

        CALL FUNCTION 'FILE_GET_NAME_USING_PATH'
          EXPORTING
*       CLIENT                           = SY-MANDT
            logical_path                     = l_path
*       OPERATING_SYSTEM                 = SY-OPSYS
*       PARAMETER_1                      = ' '
*       PARAMETER_2                      = ' '
*       PARAMETER_3                      = ' '
*       USE_BUFFER                       = ' '
            file_name                        = '°'
*       USE_PRESENTATION_SERVER          = ' '
*       ELEMINATE_BLANKS                 = 'X'
         IMPORTING
           file_name_with_path              = lf_dir
   EXCEPTIONS
     path_not_found                   = 1
     missing_parameter                = 2
     operating_system_not_found       = 3
     file_system_not_found            = 4
     OTHERS                           = 5
                  .
        IF sy-subrc <> 0.
          MESSAGE ID sy-msgid TYPE 'I' NUMBER sy-msgno DISPLAY LIKE sy-msgty
                  WITH sy-msgv1 sy-msgv2 sy-msgv3 sy-msgv4.
        ENDIF.
        DATA l_offset TYPE i.
        FIND '°' IN lf_dir MATCH OFFSET l_offset.
        IF sy-subrc = 0.
          lf_dir = lf_dir(l_offset).
        ENDIF.


        CALL METHOD eo_dir->init
          EXPORTING
            pf_full_name = lf_dir.


      ELSE.
        CREATE OBJECT eo_dir TYPE zapcmd_cl_server_dir.
        lf_dir = eo_dir->co_logicaldir.
        CALL METHOD eo_dir->init
          EXPORTING
            pf_full_name = lf_dir.
      ENDIF.

  ENDCASE.

ENDMETHOD.


method DELETE.

*    data lf_filename type DXFILE-FILENAME.
*    lf_filename = full_name.
*
*    CALL FUNCTION 'DX_FILE_DELETE'
*     EXPORTING
*       FILETYPE                          = 'P'
*       FILENAME                          = lf_filename
*     EXCEPTIONS
*       FILE_HANDLING_ERROR               = 1
*       NO_AUTHORITY_TO_DELETE_FILE       = 2
*       FILE_DOESNOT_EXIST                = 3
*       FILE_IS_USED                      = 4
*       DB_ERROR                          = 5
*       OTHERS                            = 6
*              .
*    IF SY-SUBRC <> 0.
*      MESSAGE ID SY-MSGID TYPE 'I' NUMBER SY-MSGNO
*           WITH SY-MSGV1 SY-MSGV2 SY-MSGV3 SY-MSGV4.
*    ENDIF.

  call method exec_server
    exporting
      pf_command = 'rmdir'
      pf_parameter = full_name.



endmethod.


method GET_FREESPACE.

     data lf_pathname type DEF_PAR_FU-PATHNAME.
    data lf_freespace type DEF_PAR_FU-FREESPACE.
    lf_pathname = full_name.

    CALL FUNCTION 'SHOW_FILEPATH_FREESPACE' "FREESPACE FROM OSCOL
     EXPORTING
        file_path = lf_pathname
        dest_type = 'L'
     IMPORTING
        free_space = lf_freespace
     EXCEPTIONS
       cant_find_destination    = 1
       cant_get_destinations    = 1
       OTHERS                   = 4.
    if sy-subrc <> 0.
      lf_freespace = 0.
    endif.
*   Umwandlung von KByte in Byte
    pf_space = lf_freespace * 1024.

endmethod.


method GET_TOOLBAR.

    data ls_toolbar type STB_BUTTON.

    CLEAR ls_toolbar.
    MOVE 0 TO ls_toolbar-butn_type.
    MOVE co_server TO ls_toolbar-function.
    MOVE ICON_SYM_ALT_SERVER TO ls_toolbar-icon.
    MOVE 'Server'(234) to ls_toolbar-text.
    MOVE 'Liste verbundener Server'(235) TO ls_toolbar-quickinfo.
    MOVE SPACE TO ls_toolbar-disabled.
    APPEND ls_toolbar TO pt_toolbar.

    CLEAR ls_toolbar.
    MOVE 0 TO ls_toolbar-butn_type.
    MOVE co_al11 TO ls_toolbar-function.
    MOVE ICON_OPEN_LINKED_FOLDER TO ls_toolbar-icon.
    MOVE 'AL11'(236) to ls_toolbar-text.
    MOVE 'Verzeichnisse aus der AL11'(237) TO ls_toolbar-quickinfo.
    MOVE SPACE TO ls_toolbar-disabled.
    APPEND ls_toolbar TO pt_toolbar.


    if separator = '\'.
      CLEAR ls_toolbar.
      MOVE 0 TO ls_toolbar-butn_type.
      MOVE co_drives TO ls_toolbar-function.
      MOVE ICON_SYSTEM_SAVE TO ls_toolbar-icon.
      MOVE 'Laufwerke'(232) to ls_toolbar-text.
      MOVE 'Laufwerke'(232) TO ls_toolbar-quickinfo.
      MOVE SPACE TO ls_toolbar-disabled.
      APPEND ls_toolbar TO pt_toolbar.
    endif.

      CLEAR ls_toolbar.
    MOVE 0 TO ls_toolbar-butn_type.
    MOVE co_logicalfile TO ls_toolbar-function.
    MOVE ICON_PUBLIC_FILES TO ls_toolbar-icon.
    MOVE 'log. Datei'(003) to ls_toolbar-text.
    MOVE 'logische Pfade und Dateien'(004) TO ls_toolbar-quickinfo.
    MOVE SPACE TO ls_toolbar-disabled.
    APPEND ls_toolbar TO pt_toolbar.



endmethod.


METHOD init.
  CALL METHOD super->init
    EXPORTING
      pf_name      = pf_name
      pf_full_name = pf_full_name
      pf_size      = pf_size
      pf_moddate   = pf_moddate
      pf_modtime   = pf_modtime
      pf_attr      = pf_attr
      pf_dir       = pf_dir.

  IF full_name IS INITIAL.


    DATA lf_temp(255) TYPE c.
    CALL 'C_SAPGPARAM' ID 'NAME'  FIELD 'DIR_HOME'
                       ID 'VALUE' FIELD lf_temp.

    full_name = lf_temp.
  ENDIF.

  IF full_name = '.'.
    IF separator = '\'.
      full_name = 'C:\'.
    ELSE.
      full_name = separator.
    ENDIF.
  ENDIF.

ENDMETHOD.


method READ_AL11.

* ...
    data lf_name type string.

    data lf_ref_file type ref to ZAPCMD_CL_KNOT.

    types:
    begin of ZAVL_AL11,
      name(40) type c,
      dirname(1024) type c,
    end of ZAVL_AL11.

    data ls_names type ZAVL_AL11.
    data lt_names type table of ZAVL_AL11.
    data lt_searchnames type table of char40.

    append 'DIR_ATRA' to lt_searchnames.
    append 'DIR_BINARY' to lt_searchnames.
    append 'DIR_CT_LOGGING' to lt_searchnames.
    append 'DIR_CT_RUN' to lt_searchnames.
    append 'DIR_DATA' to lt_searchnames.
    append 'DIR_DBMS' to lt_searchnames.
    append 'DIR_EXECUTABLE' to lt_searchnames.
    append 'DIR_EXE_ROOT' to lt_searchnames.
    append 'DIR_GEN' to lt_searchnames.
    append 'DIR_GEN_ROOT' to lt_searchnames.
    append 'DIR_GLOBAL' to lt_searchnames.
    append 'DIR_GRAPH_EXE' to lt_searchnames.
    append 'DIR_GRAPH_LIB' to lt_searchnames.
    append 'DIR_HOME' to lt_searchnames.
    append 'DIR_INSTALL' to lt_searchnames.
    append 'DIR_INSTANCE' to lt_searchnames.
    append 'DIR_LIBRARY' to lt_searchnames.
    append 'DIR_LOGGING' to lt_searchnames.
    append 'DIR_MEMORY_INSPECTOR' to lt_searchnames.
    append 'DIR_PAGING' to lt_searchnames.
    append 'DIR_PUT' to lt_searchnames.
    append 'DIR_PERF' to lt_searchnames.
    append 'DIR_PROFILE' to lt_searchnames.
    append 'DIR_PROTOKOLLS' to lt_searchnames.
    append 'DIR_REORG' to lt_searchnames.
    append 'DIR_ROLL' to lt_searchnames.
    append 'DIR_RSYN' to lt_searchnames.
    append 'DIR_SAPUSERS' to lt_searchnames.
    append 'DIR_SETUPS' to lt_searchnames.
    append 'DIR_SORTTMP' to lt_searchnames.
    append 'DIR_SOURCE' to lt_searchnames.
    append 'DIR_TEMP' to lt_searchnames.
    append 'DIR_TRANS' to lt_searchnames.
    append 'DIR_TRFILES' to lt_searchnames.
    append 'DIR_TRSUB' to lt_searchnames.
    loop at lt_searchnames into ls_names-name.
      CALL 'C_SAPGPARAM' ID 'NAME'  FIELD ls_names-name
                     ID 'VALUE' FIELD ls_names-dirname.
      IF ls_names-dirname = '.'.
        IF sy-opsys = 'Windows NT'.
          ls_names-dirname = '.\'.
        ELSE.
          ls_names-dirname = './'.
        ENDIF.
      ENDIF.

      append ls_names to lt_names.
    endloop.

    CASE sy-dbsys(3).
      WHEN 'ORA'.
        CALL 'C_SAPGPARAM' ID 'NAME'  FIELD 'DIR_ORAHOME'
                           ID 'VALUE' FIELD ls_names-dirname.
        ls_names-name = 'DIR_ORAHOME'.
        append ls_names to lt_names.
      WHEN 'ADA'.
        CALL 'C_GETENV' ID 'NAME'  FIELD 'DBROOT'
                        ID 'VALUE' FIELD ls_names-dirname.
        ls_names-name = 'DIR_ADA_DBROOT'.
        append ls_names to lt_names.
      WHEN 'INF'.
        CALL 'C_GETENV' ID 'NAME'  FIELD 'INFORMIXDIR'
                        ID 'VALUE' FIELD ls_names-dirname.
        ls_names-name = 'DIR_INF_INFORMIXDIR'.
        append ls_names to lt_names.
      WHEN 'DB6'.
        CALL 'C_GETENV' ID 'NAME'  FIELD 'INSTHOME'
                        ID 'VALUE' FIELD ls_names-dirname.
        IF SY-SUBRC = 0.
          ls_names-name = 'DIR_DB2_HOME'.
          append ls_names to lt_names.
        ENDIF.
      WHEN OTHERS.
    ENDCASE.
    sort lt_names by name.


    DATA: my_name(20) type c.
*  get the name of the current server.
    CALL 'C_SAPGPARAM' ID 'NAME' FIELD 'rdisp/myname'
                       ID 'VALUE' FIELD my_name.

    data lt_searchpoints type table of user_dir.
    data ls_searchpoints type user_dir.


* get the name and aliases of ALL userdefined directories
    SELECT * FROM user_dir INTO ls_searchpoints
      WHERE svrname = my_name.
      APPEND ls_searchpoints to lt_searchpoints.
    ENDSELECT.
    SELECT * FROM user_dir INTO ls_searchpoints
      WHERE svrname = 'all'.
      APPEND ls_searchpoints to lt_searchpoints.
    ENDSELECT.

    LOOP AT lt_searchpoints into ls_searchpoints.
      ls_names-name = ls_searchpoints-aliass.
      ls_names-dirname = ls_searchpoints-dirname.
      append ls_names to lt_names.
    ENDLOOP.

    loop at lt_names into ls_names.
      create object lf_ref_file type ZAPCMD_CL_SERVER_DIR.
      call method lf_ref_file->init
        EXPORTING
          pf_full_name = ls_names-dirname
          pf_name      = ls_names-name
          pf_size      = 0
          pf_dir       = full_name.

      append lf_ref_file to pt_filelist.
    endloop.

endmethod.


method READ_DIR.


    data lf_ref_file type ref to Zapcmd_CL_KNOT.
*    data lt_filelist_undo like pt_filelist.
*    lt_filelist_undo[] = pt_filelist[].
    refresh pt_filelist.

    data lf_filter(255) type c.
    data lf_dir(255) type c.

    if pf_mask is initial.
      lf_filter = filter.
    else.
      lf_filter = pf_mask.
    endif.
    lf_dir = full_name.


    data lf_server type string.
    data lf_strlen type i.
    lf_strlen = strlen( full_name ).

    if full_name = co_rootdir.
      call method read_server
        IMPORTING
          pt_filelist = pt_filelist.
      exit.
    endif.

    if full_name = co_AL11dir.
      call method read_al11
        IMPORTING
          pt_filelist = pt_filelist.
      exit.
    endif.

    if full_name = co_logicaldir.
      call method read_logicaldir
        IMPORTING
          pt_filelist = pt_filelist.
      exit.
    endif.

     if full_name = co_rfcdir.
      call method read_rfc
        IMPORTING
          pt_filelist = pt_filelist.
      exit.
    endif.

    if full_name = '\'.
      call method read_drives
        IMPORTING
          pt_filelist = pt_filelist.
      exit.
    endif.

    if full_name+1 = ':\' and '*' ca lf_filter.

      create object lf_ref_file type ZAPCMD_CL_SERVER_DIR.
      call method lf_ref_file->init
        EXPORTING
          pf_name      = '..'
          pf_full_name = '\'
          pf_size      = 0
          pf_dir       = full_name.
      append lf_ref_file to pt_filelist.


    endif.




**data lf_data(255) type c.
*data lf_data type string.
*data lt_data like table of lf_data.
*data mess(255) type c.
*
*open dataset lf_dir IN TEXT MODE MESSAGE mess.
*write: / mess.
*do.
*  read dataset lf_dir into  lf_data.
*  if sy-subrc <> 0.
*    exit.
*  endif.
*  append lf_data to lt_data.
*enddo.
*
*close dataset lf_dir.
    DATA: g_rec_level_120 TYPE i VALUE 0.

    DATA: BEGIN OF g_file,
         dirname(255) TYPE c, " name of directory. (possibly truncated.)
            name(255)    TYPE c, " name of entry. (possibly truncated.)
            type(10)     TYPE c,           " type of entry.
            len(8)       TYPE p,           " length in bytes.
            owner(8)     TYPE c,           " owner of the entry.
        mtime(6)     TYPE p, " last modification date,seconds since 1970
            mode(9)      TYPE c, " like "rwx-r-x--x": protection mode.
            usable(1)    TYPE c,
            subrc(4)     TYPE c,
            errno(3)     TYPE c,
            errmsg(40)   TYPE c,
            mod_date     TYPE d,
            mod_time(8)  TYPE c,           " hh:mm:ss
            seen(1)      TYPE c,
            changed(1)   TYPE c,
            rec_level    TYPE i,
            dir_flag     TYPE xflag,
          END OF g_file.

    DATA: gt_file_list LIKE STANDARD TABLE OF g_file.

    CONSTANTS: c_true(1)     TYPE c VALUE 'X',
               c_false(1)    TYPE c VALUE ' '.



    DATA: l_errcnt(2) TYPE p VALUE 0.

    g_file-rec_level = g_rec_level_120.

    CALL 'C_DIR_READ_FINISH'             " just to be sure
        ID 'ERRNO'  FIELD g_file-errno
        ID 'ERRMSG' FIELD g_file-errmsg.

    CALL 'C_DIR_READ_START' ID 'DIR'    FIELD lf_dir
                            ID 'FILE'   FIELD lf_filter
                            ID 'ERRNO'  FIELD g_file-errno
                            ID 'ERRMSG' FIELD g_file-errmsg.
    IF sy-subrc <> 0.
      MESSAGE i204(s_dx_bapi) WITH g_file-errmsg g_file-errmsg.
      exit.
    ENDIF.

    DO.
      CLEAR g_file.
      g_file-rec_level = g_rec_level_120.
      CALL 'C_DIR_READ_NEXT'
        ID 'TYPE'   FIELD g_file-type
        ID 'NAME'   FIELD g_file-name
        ID 'LEN'    FIELD g_file-len
*        ID 'OWNER'  FIELD g_file-owner
        ID 'MTIME'  FIELD g_file-mtime
        ID 'MODE'   FIELD g_file-mode
        ID 'ERRNO'  FIELD g_file-errno
        ID 'ERRMSG' FIELD g_file-errmsg.
      g_file-dirname = lf_dir.
      MOVE sy-subrc TO g_file-subrc.

      CASE sy-subrc.
        WHEN 0.
          CLEAR: g_file-errno, g_file-errmsg.
          CASE g_file-type(1).
            WHEN 'F'.                    " normal file.
              MOVE c_true  TO g_file-usable.
            WHEN 'f'.                    " normal file.
              MOVE c_true  TO g_file-usable.
            WHEN OTHERS. " directory, device, fifo, socket,...
              MOVE c_false TO g_file-usable.
          ENDCASE.
          IF ( g_file-type(1) = 'D' ) OR
             ( g_file-type(1) = 'd' ).
            g_file-dir_flag = c_true.
          ELSE.
            g_file-dir_flag = c_false.
          ENDIF.
          IF g_file-len = 0.
            MOVE c_false TO g_file-usable.
          ENDIF.
        WHEN 1.
          EXIT.
        WHEN OTHERS.                     " SY-SUBRC >= 2
          ADD 1 TO l_errcnt.
          IF l_errcnt > 20.
*            message E002 with full_name raising permission_denied.
          ENDIF.
          IF sy-subrc = 5.
            IF ( g_file-name = '..' ) OR
             ( g_file-name = '.' ).
              g_file-dir_flag = c_true.
            ELSE.
              g_file-dir_flag = c_false.
            ENDIF.

            MOVE: '???' TO g_file-type,
                  '???' TO g_file-owner,
                  '???' TO g_file-mode.
          ELSE.
*         ULINE.
*         WRITE: / 'C_DIR_READ_NEXT', 'SUBRC', SY-SUBRC.
          ENDIF.
          MOVE c_false TO g_file-usable.
      ENDCASE.
      if g_file-errno > 0.
        MOVE: '???' TO g_file-type,
              '???' TO g_file-owner,
              '???' TO g_file-mode.
        ADD 1 TO l_errcnt.
        IF l_errcnt > 20.
*          message E002 with full_name raising permission_denied.
        ENDIF.
        MOVE c_false TO g_file-usable.
      endif.
      PERFORM p6_to_date_time_tz in program rstr0400 USING g_file-mtime
                                                 g_file-mod_time
                                                 g_file-mod_date.
*    MOVE-CORRESPONDING g_file TO gt_file_list.
      APPEND g_file to gt_file_list.
      case g_file-dir_flag.
        when 'X'.
          create object lf_ref_file type ZAPCMD_CL_SERVER_DIR.
        when others.
          create object lf_ref_file type ZAPCMD_CL_SERVER_FILE.
      endcase.
      data lf_name type string.
      lf_name = g_file-name.
      data lf_time type tims.
      concatenate g_file-mod_time(2) g_file-mod_time+3(2)
        g_file-mod_time+6(2) into lf_time.
      data lf_attr type char10.
      lf_attr = g_file-mode.
      if lf_name <> space and lf_name <> '.'.
        call method lf_ref_file->init
          EXPORTING
            pf_name    = lf_name
            pf_size    = g_file-len
            pf_dir     = full_name
            pf_modtime = lf_time
            pf_moddate = g_file-mod_date
            pf_attr    = lf_attr.
        append lf_ref_file to pt_filelist.
      endif.
    ENDDO.

    CALL 'C_DIR_READ_FINISH'
        ID 'ERRNO'  FIELD g_file-errno
        ID 'ERRMSG' FIELD g_file-errmsg.
    IF sy-subrc <> 0.
      WRITE: / 'C_DIR_READ_FINISH', 'SUBRC', sy-subrc.
    ENDIF.

*  READ TABLE GT_FILE_LIST WITH KEY REC_LEVEL = G_REC_LEVEL_120
*                                   NAME      = '..'.
*  IF SY-SUBRC NE 0.
*     CLEAR GT_FILE_LIST.
*     GT_FILE_LIST-REC_LEVEL = G_REC_LEVEL_120.
*     GT_FILE_LIST-DIRNAME   = P_PATH.
*     GT_FILE_LIST-USABLE    = c_true.
*     GT_FILE_LIST-DIR_FLAG  = c_true.
*     GT_FILE_LIST-NAME      = '..'.
*     append GT_FILE_LIST.
*  ENDIF.

*  SORT gt_file_list STABLE BY rec_level ASCENDING
*                              name      ASCENDING
*                              mtime     DESCENDING.

*    sort pt_filelist by table_line->is_dir descending table_line->name.

endmethod.


METHOD read_drives.

* ...
  DATA lf_drives(26) TYPE c VALUE 'ABCDEFGHIJKLMNOPQRSTUVWXYZ'.
  DATA lf_index TYPE i VALUE 0.
  DATA lf_drive TYPE string.
  DATA lf_name TYPE string.
  DATA lf_drivetype TYPE string.
  DATA errno(3)     TYPE c.
  DATA errmsg(40)   TYPE c.
  DATA lf_filter(255) TYPE c.

  DATA lf_ref_file TYPE REF TO zapcmd_cl_knot.
  lf_filter = '*.*'.

  DO 26 TIMES.
    lf_drive = lf_drives+lf_index(1).
    CONCATENATE lf_drive ':' separator INTO lf_drive.

    DATA lf_temp(255) TYPE c.
    lf_temp = lf_drive.

    CALL 'C_DIR_READ_FINISH'             " just to be sure
        ID 'ERRNO'  FIELD errno
        ID 'ERRMSG' FIELD errmsg.

    CALL 'C_DIR_READ_START' ID 'DIR'    FIELD lf_temp
                            ID 'FILE'   FIELD lf_filter
                            ID 'ERRNO'  FIELD errno
                            ID 'ERRMSG' FIELD errmsg.
    IF sy-subrc = 0.

      CALL 'C_DIR_READ_NEXT'
        ID 'ERRNO'  FIELD errno
        ID 'ERRMSG' FIELD errmsg.
      IF sy-subrc < 4.


        CONCATENATE lf_drive space INTO lf_name.
        CREATE OBJECT lf_ref_file TYPE zapcmd_cl_server_dir.
        CALL METHOD lf_ref_file->init
          EXPORTING
            pf_full_name = lf_drive
            pf_name      = lf_name
            pf_size      = 0
            pf_dir       = full_name.

        APPEND lf_ref_file TO pt_filelist.

      ENDIF.

      CALL 'C_DIR_READ_FINISH'
        ID 'ERRNO'  FIELD errno
        ID 'ERRMSG' FIELD errmsg.


    ENDIF.



    lf_index = lf_index + 1.


  ENDDO.

ENDMETHOD.


METHOD read_logicaldir.

  DATA lt_path TYPE TABLE OF filepath.
  DATA l_path TYPE filepath.

  SELECT *
     FROM filepath
     INTO TABLE lt_path.

  LOOP AT lt_path INTO l_path.

    DATA l_fullname TYPE string.

    TRY.

        CALL FUNCTION 'FILE_GET_NAME_USING_PATH'
          EXPORTING
*     CLIENT                           = SY-MANDT
            logical_path                     = l_path
*     OPERATING_SYSTEM                 = SY-OPSYS
         parameter_1                      = 'p1'
         parameter_2                      = 'p2'
         parameter_3                      = 'p3'
*     USE_BUFFER                       = ' '
            file_name                        = '°'
*     USE_PRESENTATION_SERVER          = ' '
*     ELEMINATE_BLANKS                 = 'X'
         IMPORTING
           file_name_with_path              = l_fullname
         EXCEPTIONS
           path_not_found                   = 1
           missing_parameter                = 2
           operating_system_not_found       = 3
           file_system_not_found            = 4
           OTHERS                           = 5
                  .
        IF sy-subrc <> 0.
          CONTINUE.
        ENDIF.
      CATCH cx_root.
        CONTINUE.
    ENDTRY.
    DATA l_offset TYPE i.
    FIND '°' IN l_fullname MATCH OFFSET l_offset.
    IF sy-subrc = 0.
      l_fullname = l_fullname(l_offset).
    ENDIF.

    DATA lf_ref_file TYPE REF TO zapcmd_cl_knot.
    DATA l_name TYPE string.
    l_name = l_path.

    CREATE OBJECT lf_ref_file TYPE zapcmd_cl_server_dir.
    CALL METHOD lf_ref_file->init
      EXPORTING
        pf_full_name = l_fullname
        pf_name      = l_name
        pf_size      = 0
        pf_dir       = full_name.

    APPEND lf_ref_file TO pt_filelist.



  ENDLOOP.

ENDMETHOD.


method READ_SERVER.

    data lt_server type table of MSXXLIST.
    data ls_server type MSXXLIST.

    CALL FUNCTION 'TH_SERVER_LIST'
         TABLES
              LIST          = lt_server
         EXCEPTIONS
              NO_SERVER_LIST.
    IF SY-SUBRC <> 0.
      exit.
    ENDIF.
    data lf_rfcdest type rfcdest.

    loop at lt_server into ls_server.

      data lf_name type string.
      data lf_ref_file type ref to ZAPCMD_CL_KNOT.
      data l_host type syhost.


      lf_name = ls_server-name.
      l_host = sy-host.
      TRANslate l_host to UPPER CASE.
      translate ls_server-host to UPPER CASE.
      if ls_server-host <> l_host.
        lf_rfcdest = ls_server-name.
        create object lf_ref_file type ZAPCMD_CL_rfc_DIR
          EXPORTING
            iv_rfcdest = lf_rfcdest.
      else.
        create object lf_ref_file type ZAPCMD_CL_SERVER_DIR.
      endif.
      call method lf_ref_file->init
        EXPORTING
          pf_full_name = SEPARATOR
          pf_name      = lf_name
          pf_size      = 0
          pf_dir       = full_name.

      append lf_ref_file to pt_filelist.

    endloop.

endmethod.


method RENAME.

data lf_oldlongname type zapcmd_fullname.

 lf_oldlongname = me->full_name.

CALL METHOD SUPER->RENAME
  EXPORTING
    PF_NEWNAME = PF_NEWNAME
    .

data lf_command type string.
data lf_parameter type string.
if separator = '\'.
  concatenate lf_oldlongname name into lf_parameter
    separated by space.
  lf_command = 'ren'.
else.
  concatenate lf_oldlongname full_name into lf_parameter
    separated by space.
  lf_command = 'mv'.
endif.
 call method exec_server
      exporting
        pf_command = lf_command
        pf_parameter = lf_parameter .
endmethod.
ENDCLASS.
