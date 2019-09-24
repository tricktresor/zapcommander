FUNCTION ZAPCMD_READ_DIR.
*"----------------------------------------------------------------------
*"*"Lokale Schnittstelle:
*"  IMPORTING
*"     VALUE(IV_DIR) TYPE  TEXT255
*"     VALUE(IV_FILTER) TYPE  TEXT255 OPTIONAL
*"  TABLES
*"      ET_FILE STRUCTURE  ZAPCMD_FILE_DESCR
*"  EXCEPTIONS
*"      NOT_FOUND
*"----------------------------------------------------------------------


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
    data ls_file type ZAPCMD_FILE_DESCR.

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

*    DATA: gt_file_list LIKE STANDARD TABLE OF g_file.

    CONSTANTS: c_true(1)     TYPE c VALUE 'X',
               c_false(1)    TYPE c VALUE ' '.



    DATA: l_errcnt(2) TYPE p VALUE 0.

    g_file-rec_level = g_rec_level_120.

    CALL 'C_DIR_READ_FINISH'             " just to be sure
        ID 'ERRNO'  FIELD g_file-errno
        ID 'ERRMSG' FIELD g_file-errmsg.

    CALL 'C_DIR_READ_START' ID 'DIR'    FIELD iv_dir
                            ID 'FILE'   FIELD iv_filter
                            ID 'ERRNO'  FIELD g_file-errno
                            ID 'ERRMSG' FIELD g_file-errmsg.
    IF sy-subrc <> 0.

      MESSAGE i204(s_dx_bapi) WITH g_file-errmsg g_file-errmsg RAISING not_found.
*      exit.
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
      g_file-dirname = iv_dir.
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
*      APPEND g_file to gt_file_list.
      data lf_name type string.
      lf_name = g_file-name.
      data lf_time type tims.
      concatenate g_file-mod_time(2) g_file-mod_time+3(2)
        g_file-mod_time+6(2) into lf_time.
      data lf_attr type char10.
      lf_attr = g_file-mode.
      if lf_name <> space and lf_name <> '.'.
            ls_file-name    = lf_name.
            ls_file-filesize    = g_file-len.
            ls_file-modtime = lf_time.
            ls_file-moddate = g_file-mod_date.
            ls_file-attr    = lf_attr.
            if g_file-dir_flag = 'X'.
              if lf_name = '..'.
                ls_file-filetype = 'UP'.
              else.
                ls_file-filetype = 'DIR'.
              endif.
            else.
              clear ls_file-filetype.
            endif.
        append ls_file to et_file.
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



ENDFUNCTION.
