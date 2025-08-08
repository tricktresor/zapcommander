CLASS zapcmd_cl_server_dir DEFINITION
  PUBLIC
  INHERITING FROM zapcmd_cl_dir
  CREATE PUBLIC .

*"* public components of class ZAPCMD_CL_SERVER_DIR
*"* do not include other source files here!!!
  PUBLIC SECTION.
    TYPE-POOLS icon .

    METHODS constructor .

    METHODS create_dir
        REDEFINITION .
    METHODS create_file
        REDEFINITION .
    METHODS delete
        REDEFINITION .
    METHODS get_freespace
        REDEFINITION .
    METHODS get_toolbar
        REDEFINITION .
    METHODS init
        REDEFINITION .
    METHODS read_dir
        REDEFINITION .
    METHODS rename
        REDEFINITION .
    METHODS create_new
        REDEFINITION .
  PROTECTED SECTION.
*"* protected components of class ZAPCMD_CL_SERVER_DIR
*"* do not include other source files here!!!

    METHODS read_drives
      EXPORTING
        VALUE(pt_filelist) TYPE zapcmd_tbl_filelist .
    METHODS read_server
      EXPORTING
        VALUE(pt_filelist) TYPE zapcmd_tbl_filelist .
    METHODS read_al11
      EXPORTING
        VALUE(pt_filelist) TYPE zapcmd_tbl_filelist .
    METHODS read_logicaldir
      EXPORTING
        VALUE(pt_filelist) TYPE zapcmd_tbl_filelist .
  PRIVATE SECTION.
*"* private components of class ZAPCMD_CL_SERVER_DIR
*"* do not include other source files here!!!
ENDCLASS.



CLASS zapcmd_cl_server_dir IMPLEMENTATION.


  METHOD constructor.

    super->constructor( ).
    IF sy-opsys = 'Windows NT' OR sy-opsys = 'DOS'.         "#EC NOTEXT
      separator = '\'.
    ELSE.
      separator = '/'.
    ENDIF.

    server_area = zapcmd_cl_knot=>co_area_applserv.
    area_string = 'Appl. Server'(005).

  ENDMETHOD.


  METHOD create_dir.

    CREATE OBJECT pf_file TYPE Zapcmd_CL_SERVER_DIR.

    pf_file->init(
        pf_name    = pf_filename
        pf_size    = 0
        pf_dir     = full_name
        pf_moddate = sy-datum
        pf_modtime = sy-uzeit
        pf_attr    = space ).

    exec_server(
        pf_command   = 'mkdir'
        pf_parameter = pf_file->full_name ).

  ENDMETHOD.


  METHOD create_file.

    CREATE OBJECT pf_file TYPE zapcmd_cl_server_file.

    pf_file->init(
         pf_name    = pf_filename
         pf_size    = 0
         pf_dir     = full_name
         pf_moddate = sy-datum
         pf_modtime = sy-uzeit
         pf_attr    = space ).

  ENDMETHOD.


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
            popup_title     = 'logischer Pfad'(013)
          IMPORTING
            returncode      = l_subrc
          TABLES
            fields          = values
          EXCEPTIONS
            error_in_fields = 1
            OTHERS          = 2.
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
              logical_path               = l_path
              file_name                  = '°'
            IMPORTING
              file_name_with_path        = lf_dir
            EXCEPTIONS
              path_not_found             = 1
              missing_parameter          = 2
              operating_system_not_found = 3
              file_system_not_found      = 4
              OTHERS                     = 5.
          IF sy-subrc <> 0.
            MESSAGE ID sy-msgid TYPE 'I' NUMBER sy-msgno DISPLAY LIKE sy-msgty
                    WITH sy-msgv1 sy-msgv2 sy-msgv3 sy-msgv4.
          ENDIF.
          DATA l_offset TYPE i.
          FIND '°' IN lf_dir MATCH OFFSET l_offset.
          IF sy-subrc = 0.
            lf_dir = lf_dir(l_offset).
          ENDIF.

          eo_dir->init( pf_full_name = lf_dir ).
        ELSE.
          CREATE OBJECT eo_dir TYPE zapcmd_cl_server_dir.
          lf_dir = eo_dir->co_logicaldir.
          eo_dir->init( pf_full_name = lf_dir ).
        ENDIF.

    ENDCASE.

  ENDMETHOD.


  METHOD delete.

    exec_server(
        pf_command = 'rmdir'
        pf_parameter = full_name ).

  ENDMETHOD.


  METHOD get_freespace.

    DATA lf_pathname TYPE def_par_fu-pathname.
    DATA lf_freespace TYPE def_par_fu-freespace.
    lf_pathname = full_name.

    CALL FUNCTION 'SHOW_FILEPATH_FREESPACE' "FREESPACE FROM OSCOL
      EXPORTING
        file_path             = lf_pathname
        dest_type             = 'L'
      IMPORTING
        free_space            = lf_freespace
      EXCEPTIONS
        cant_find_destination = 1
        cant_get_destinations = 1
        OTHERS                = 4.
    IF sy-subrc <> 0.
      lf_freespace = 0.
    ENDIF.
*   Umwandlung von KByte in Byte
    pf_space = lf_freespace * 1024.

  ENDMETHOD.


  METHOD get_toolbar.

    DATA ls_toolbar TYPE stb_button.

    CLEAR ls_toolbar.
    MOVE 0 TO ls_toolbar-butn_type.
    MOVE co_server TO ls_toolbar-function.
    MOVE icon_sym_alt_server TO ls_toolbar-icon.
    MOVE 'Server'(234) TO ls_toolbar-text.
    MOVE 'Liste verbundener Server'(235) TO ls_toolbar-quickinfo.
    MOVE space TO ls_toolbar-disabled.
    APPEND ls_toolbar TO pt_toolbar.

    CLEAR ls_toolbar.
    MOVE 0 TO ls_toolbar-butn_type.
    MOVE co_al11 TO ls_toolbar-function.
    MOVE icon_open_linked_folder TO ls_toolbar-icon.
    MOVE 'AL11'(236) TO ls_toolbar-text.
    MOVE 'Verzeichnisse aus der AL11'(237) TO ls_toolbar-quickinfo.
    MOVE space TO ls_toolbar-disabled.
    APPEND ls_toolbar TO pt_toolbar.


    IF separator = '\'.
      CLEAR ls_toolbar.
      MOVE 0 TO ls_toolbar-butn_type.
      MOVE co_drives TO ls_toolbar-function.
      MOVE icon_system_save TO ls_toolbar-icon.
      MOVE 'Laufwerke'(232) TO ls_toolbar-text.
      MOVE 'Laufwerke'(232) TO ls_toolbar-quickinfo.
      MOVE space TO ls_toolbar-disabled.
      APPEND ls_toolbar TO pt_toolbar.
    ENDIF.

    CLEAR ls_toolbar.
    MOVE 0 TO ls_toolbar-butn_type.
    MOVE co_logicalfile TO ls_toolbar-function.
    MOVE icon_public_files TO ls_toolbar-icon.
    MOVE 'log. Datei'(003) TO ls_toolbar-text.
    MOVE 'logische Pfade und Dateien'(004) TO ls_toolbar-quickinfo.
    MOVE space TO ls_toolbar-disabled.
    APPEND ls_toolbar TO pt_toolbar.

  ENDMETHOD.


  METHOD init.

    super->init(
        pf_name      = pf_name
        pf_full_name = pf_full_name
        pf_size      = pf_size
        pf_moddate   = pf_moddate
        pf_modtime   = pf_modtime
        pf_attr      = pf_attr
        pf_dir       = pf_dir ).

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


  METHOD read_al11.

* ...
    DATA lf_ref_file TYPE REF TO zapcmd_cl_knot.

    TYPES:
      BEGIN OF zavl_al11,
        name(40)      TYPE c,
        dirname(1024) TYPE c,
      END OF zavl_al11.

    DATA ls_names TYPE zavl_al11.
    DATA lt_names TYPE TABLE OF zavl_al11.
    DATA lt_searchnames TYPE TABLE OF char40.

    APPEND 'DIR_ATRA' TO lt_searchnames.
    APPEND 'DIR_BINARY' TO lt_searchnames.
    APPEND 'DIR_CT_LOGGING' TO lt_searchnames.
    APPEND 'DIR_CT_RUN' TO lt_searchnames.
    APPEND 'DIR_DATA' TO lt_searchnames.
    APPEND 'DIR_DBMS' TO lt_searchnames.
    APPEND 'DIR_EXECUTABLE' TO lt_searchnames.
    APPEND 'DIR_EXE_ROOT' TO lt_searchnames.
    APPEND 'DIR_GEN' TO lt_searchnames.
    APPEND 'DIR_GEN_ROOT' TO lt_searchnames.
    APPEND 'DIR_GLOBAL' TO lt_searchnames.
    APPEND 'DIR_GRAPH_EXE' TO lt_searchnames.
    APPEND 'DIR_GRAPH_LIB' TO lt_searchnames.
    APPEND 'DIR_HOME' TO lt_searchnames.
    APPEND 'DIR_INSTALL' TO lt_searchnames.
    APPEND 'DIR_INSTANCE' TO lt_searchnames.
    APPEND 'DIR_LIBRARY' TO lt_searchnames.
    APPEND 'DIR_LOGGING' TO lt_searchnames.
    APPEND 'DIR_MEMORY_INSPECTOR' TO lt_searchnames.
    APPEND 'DIR_PAGING' TO lt_searchnames.
    APPEND 'DIR_PUT' TO lt_searchnames.
    APPEND 'DIR_PERF' TO lt_searchnames.
    APPEND 'DIR_PROFILE' TO lt_searchnames.
    APPEND 'DIR_PROTOKOLLS' TO lt_searchnames.
    APPEND 'DIR_REORG' TO lt_searchnames.
    APPEND 'DIR_ROLL' TO lt_searchnames.
    APPEND 'DIR_RSYN' TO lt_searchnames.
    APPEND 'DIR_SAPUSERS' TO lt_searchnames.
    APPEND 'DIR_SETUPS' TO lt_searchnames.
    APPEND 'DIR_SORTTMP' TO lt_searchnames.
    APPEND 'DIR_SOURCE' TO lt_searchnames.
    APPEND 'DIR_TEMP' TO lt_searchnames.
    APPEND 'DIR_TRANS' TO lt_searchnames.
    APPEND 'DIR_TRFILES' TO lt_searchnames.
    APPEND 'DIR_TRSUB' TO lt_searchnames.
    LOOP AT lt_searchnames INTO ls_names-name.
      CALL 'C_SAPGPARAM' ID 'NAME'  FIELD ls_names-name
                     ID 'VALUE' FIELD ls_names-dirname.
      IF ls_names-dirname = '.'.
        IF sy-opsys = 'Windows NT'.
          ls_names-dirname = '.\'.
        ELSE.
          ls_names-dirname = './'.
        ENDIF.
      ENDIF.

      APPEND ls_names TO lt_names.
    ENDLOOP.

    CASE sy-dbsys(3).
      WHEN 'ORA'.
        CALL 'C_SAPGPARAM' ID 'NAME'  FIELD 'DIR_ORAHOME'
                           ID 'VALUE' FIELD ls_names-dirname.
        ls_names-name = 'DIR_ORAHOME'.
        APPEND ls_names TO lt_names.
      WHEN 'ADA'.
        CALL 'C_GETENV' ID 'NAME'  FIELD 'DBROOT'
                        ID 'VALUE' FIELD ls_names-dirname.
        ls_names-name = 'DIR_ADA_DBROOT'.
        APPEND ls_names TO lt_names.
      WHEN 'INF'.
        CALL 'C_GETENV' ID 'NAME'  FIELD 'INFORMIXDIR'
                        ID 'VALUE' FIELD ls_names-dirname.
        ls_names-name = 'DIR_INF_INFORMIXDIR'.
        APPEND ls_names TO lt_names.
      WHEN 'DB6'.
        CALL 'C_GETENV' ID 'NAME'  FIELD 'INSTHOME'
                        ID 'VALUE' FIELD ls_names-dirname.
        IF sy-subrc = 0.
          ls_names-name = 'DIR_DB2_HOME'.
          APPEND ls_names TO lt_names.
        ENDIF.
      WHEN OTHERS.
    ENDCASE.
    SORT lt_names BY name.


    DATA: my_name(20) TYPE c.
*  get the name of the current server.
    CALL 'C_SAPGPARAM' ID 'NAME' FIELD 'rdisp/myname'
                       ID 'VALUE' FIELD my_name.

    DATA lt_searchpoints TYPE TABLE OF user_dir.
    DATA ls_searchpoints TYPE user_dir.


* get the name and aliases of ALL userdefined directories
    SELECT * FROM user_dir INTO ls_searchpoints
      WHERE svrname = my_name.
      APPEND ls_searchpoints TO lt_searchpoints.
    ENDSELECT.
    SELECT * FROM user_dir INTO ls_searchpoints
      WHERE svrname = 'all'.
      APPEND ls_searchpoints TO lt_searchpoints.
    ENDSELECT.

    LOOP AT lt_searchpoints INTO ls_searchpoints.
      ls_names-name = ls_searchpoints-aliass.
      ls_names-dirname = ls_searchpoints-dirname.
      APPEND ls_names TO lt_names.
    ENDLOOP.

    LOOP AT lt_names INTO ls_names.
      CREATE OBJECT lf_ref_file TYPE zapcmd_cl_server_dir.
      lf_ref_file->init(
          pf_full_name = ls_names-dirname
          pf_name      = ls_names-name
          pf_size      = 0
          pf_dir       = full_name ).

      APPEND lf_ref_file TO pt_filelist.
    ENDLOOP.

  ENDMETHOD.


  METHOD read_dir.

    DATA lf_ref_file TYPE REF TO Zapcmd_CL_KNOT.
    REFRESH pt_filelist.

    DATA lf_filter(255) TYPE c.
    DATA lf_dir(255) TYPE c.

    IF pf_mask IS INITIAL.
      lf_filter = filter.
    ELSE.
      lf_filter = pf_mask.
    ENDIF.
    lf_dir = full_name.


    DATA lf_strlen TYPE i.
    lf_strlen = strlen( full_name ).

    IF full_name = co_rootdir.
      read_server(
        IMPORTING
          pt_filelist = pt_filelist ).
      EXIT.
    ENDIF.

    IF full_name = co_AL11dir.
      read_al11(
        IMPORTING
          pt_filelist = pt_filelist ).
      EXIT.
    ENDIF.

    IF full_name = co_logicaldir.
      read_logicaldir(
        IMPORTING
          pt_filelist = pt_filelist ).
      EXIT.
    ENDIF.

    IF full_name = co_rfcdir.
      read_rfc(
        IMPORTING
          pt_filelist = pt_filelist ).
      EXIT.
    ENDIF.

    IF full_name = '\'.
      read_drives(
        IMPORTING
          pt_filelist = pt_filelist ).
      EXIT.
    ENDIF.

    IF full_name+1 = ':\' AND '*' CA lf_filter.

      CREATE OBJECT lf_ref_file TYPE zapcmd_cl_server_dir.
      lf_ref_file->init(
           pf_name      = ' )..'
           pf_full_name = '\'
           pf_size      = 0
           pf_dir       = full_name ).
      APPEND lf_ref_file TO pt_filelist.

    ENDIF.

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

    CONSTANTS: c_true(1)  TYPE c VALUE 'X',
               c_false(1) TYPE c VALUE ' '.

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
      EXIT.
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
          ENDIF.
          MOVE c_false TO g_file-usable.
      ENDCASE.
      IF g_file-errno > 0.
        MOVE: '???' TO g_file-type,
              '???' TO g_file-owner,
              '???' TO g_file-mode.
        ADD 1 TO l_errcnt.
        IF l_errcnt > 20.
*          message E002 with full_name raising permission_denied.
        ENDIF.
        MOVE c_false TO g_file-usable.
      ENDIF.
      PERFORM p6_to_date_time_tz IN PROGRAM rstr0400 USING g_file-mtime
                                                 g_file-mod_time
                                                 g_file-mod_date.
      APPEND g_file TO gt_file_list.
      CASE g_file-dir_flag.
        WHEN 'X'.
          CREATE OBJECT lf_ref_file TYPE zapcmd_cl_server_dir.
        WHEN OTHERS.
          CREATE OBJECT lf_ref_file TYPE zapcmd_cl_server_file.
      ENDCASE.
      DATA lf_name TYPE string.
      lf_name = g_file-name.
      DATA lf_time TYPE tims.
      CONCATENATE g_file-mod_time(2) g_file-mod_time+3(2)
        g_file-mod_time+6(2) INTO lf_time.
      DATA lf_attr TYPE char10.
      lf_attr = g_file-mode.
      IF lf_name <> space AND lf_name <> '.'.
        lf_ref_file->init(
            pf_name    = lf_name
            pf_size    = g_file-len
            pf_dir     = full_name
            pf_modtime = lf_time
            pf_moddate = g_file-mod_date
            pf_attr    = lf_attr ).
        APPEND lf_ref_file TO pt_filelist.
      ENDIF.
    ENDDO.

    CALL 'C_DIR_READ_FINISH'
        ID 'ERRNO'  FIELD g_file-errno
        ID 'ERRMSG' FIELD g_file-errmsg.
    IF sy-subrc <> 0.
      WRITE: / 'C_DIR_READ_FINISH', 'SUBRC', sy-subrc.
    ENDIF.

  ENDMETHOD.


  METHOD read_drives.

    DATA lf_drives(26) TYPE c VALUE 'ABCDEFGHIJKLMNOPQRSTUVWXYZ'.
    DATA lf_index TYPE i VALUE 0.
    DATA lf_drive TYPE string.
    DATA lf_name TYPE string.
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
          lf_ref_file->init(
              pf_full_name = lf_drive
              pf_name      = lf_name
              pf_size      = 0
              pf_dir       = full_name ).

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
              logical_path               = l_path
              parameter_1                = 'p1'
              parameter_2                = 'p2'
              parameter_3                = 'p3'
              file_name                  = '°'
            IMPORTING
              file_name_with_path        = l_fullname
            EXCEPTIONS
              path_not_found             = 1
              missing_parameter          = 2
              operating_system_not_found = 3
              file_system_not_found      = 4
              OTHERS                     = 5.
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
      lf_ref_file->init(
          pf_full_name = l_fullname
          pf_name      = l_name
          pf_size      = 0
          pf_dir       = full_name ).

      APPEND lf_ref_file TO pt_filelist.

    ENDLOOP.

  ENDMETHOD.


  METHOD read_server.

    DATA lt_server TYPE TABLE OF msxxlist.
    DATA ls_server TYPE msxxlist.
    DATA lf_rfcdest TYPE rfcdest.

    CALL FUNCTION 'TH_SERVER_LIST'
      TABLES
        list = lt_server
               EXCEPTIONS
               no_server_list.
    IF sy-subrc <> 0.
      EXIT.
    ENDIF.

    LOOP AT lt_server INTO ls_server.

      DATA lf_name TYPE string.
      DATA lf_ref_file TYPE REF TO zapcmd_cl_knot.
      DATA l_host TYPE syhost.

      lf_name = ls_server-name.
      l_host = sy-host.
      TRANSLATE l_host TO UPPER CASE.
      TRANSLATE ls_server-host TO UPPER CASE.
      IF ls_server-host <> l_host.
        lf_rfcdest = ls_server-name.
        CREATE OBJECT lf_ref_file TYPE ZAPCMD_CL_rfc_DIR
          EXPORTING
            iv_rfcdest = lf_rfcdest.
      ELSE.
        CREATE OBJECT lf_ref_file TYPE zapcmd_cl_server_dir.
      ENDIF.
      lf_ref_file->init(
          pf_full_name = separator
          pf_name      = lf_name
          pf_size      = 0
          pf_dir       = full_name ).

      APPEND lf_ref_file TO pt_filelist.

    ENDLOOP.

  ENDMETHOD.


  METHOD rename.

    DATA lf_oldlongname TYPE zapcmd_fullname.

    lf_oldlongname = me->full_name.

    super->rename( pf_newname = pf_newname ).

    DATA lf_command TYPE string.
    DATA lf_parameter TYPE string.
    IF separator = '\'.
      CONCATENATE lf_oldlongname name INTO lf_parameter
        SEPARATED BY space.
      lf_command = 'ren'.
    ELSE.
      CONCATENATE lf_oldlongname full_name INTO lf_parameter
        SEPARATED BY space.
      lf_command = 'mv'.
    ENDIF.
    exec_server(
        pf_command   = lf_command
        pf_parameter = lf_parameter ).
  ENDMETHOD.
ENDCLASS.
