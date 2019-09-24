class ZAPCMD_CL_FRONTEND_FILE definition
  public
  inheriting from ZAPCMD_CL_FILE
  final
  create public .

*"* public components of class ZAPCMD_CL_FRONTEND_FILE
*"* do not include other source files here!!!
public section.

  methods CONSTRUCTOR .

  methods DELETE
    redefinition .
  methods EXECUTE
    redefinition .
  methods INIT
    redefinition .
  methods READ_BIN
    redefinition .
  methods READ_TEXT
    redefinition .
  methods WRITE_BIN
    redefinition .
  methods WRITE_TEXT
    redefinition .
  methods RENAME
    redefinition .
protected section.
*"* protected components of class ZAPCMD_CL_FRONTEND_FILE
*"* do not include other source files here!!!
private section.
*"* private components of class ZAPCMD_CL_FRONTEND_FILE
*"* do not include other source files here!!!
ENDCLASS.



CLASS ZAPCMD_CL_FRONTEND_FILE IMPLEMENTATION.


method CONSTRUCTOR.

* ...
    call method super->constructor.
    separator = '\'.
    server_area = 'P'.

endmethod.


method DELETE.

    data lf_rc type sysubrc.

     CALL METHOD CL_GUI_FRONTEND_SERVICES=>FILE_DELETE
       EXPORTING
         FILENAME             = me->full_name
       CHANGING
         RC                   = lf_rc
       EXCEPTIONS
         FILE_DELETE_FAILED   = 1
         CNTL_ERROR           = 2
         ERROR_NO_GUI         = 3
         FILE_NOT_FOUND       = 4
         ACCESS_DENIED        = 5
         UNKNOWN_ERROR        = 6
*        NOT_SUPPORTED_BY_GUI = 7
*        WRONG_PARAMETER      = 8
         others               = 9
             .
     IF SY-SUBRC <> 0.
      MESSAGE ID SY-MSGID TYPE SY-MSGTY NUMBER SY-MSGNO
                 WITH SY-MSGV1 SY-MSGV2 SY-MSGV3 SY-MSGV4.
     ENDIF.

endmethod.


METHOD execute.

* ...
  DATA l_extension TYPE string.
  l_extension = extension.
  TRANSLATE l_extension TO UPPER CASE.

  IF strlen( l_extension ) >= 3 and l_extension(3) = 'ABA'.

     execute_abap( ).

  ELSE.

    CALL METHOD cl_gui_frontend_services=>execute
      EXPORTING
        document               = full_name
*    APPLICATION            =
*    PARAMETER              =
*    DEFAULT_DIRECTORY      =
*    MAXIMIZED              =
*    MINIMIZED              =
*    SYNCHRONOUS            =
      EXCEPTIONS
        cntl_error             = 1
        error_no_gui           = 2
        bad_parameter          = 3
        file_not_found         = 4
        path_not_found         = 5
        file_extension_unknown = 6
        error_execute_failed   = 7
        OTHERS                 = 8
            .
    IF sy-subrc <> 0.
      MESSAGE ID sy-msgid TYPE 'I' NUMBER sy-msgno
                 WITH sy-msgv1 sy-msgv2 sy-msgv3 sy-msgv4.
    ENDIF.
  ENDIF.

ENDMETHOD.


method INIT.
  call method super->init
      EXPORTING
        pf_name      = pf_name
        pf_full_name = pf_full_name
        pf_size      = pf_size
        pf_moddate   = pf_moddate
        pf_modtime   = pf_modtime
        pf_attr      = pf_attr
        pf_dir       = pf_dir.

endmethod.


method READ_BIN.


    CALL METHOD CL_GUI_FRONTEND_SERVICES=>GUI_UPLOAD
      EXPORTING
        FILENAME                = full_name
        FILETYPE                = 'BIN'
*    HAS_FIELD_SEPARATOR     = SPACE
*    HEADER_LENGTH           = 0
      IMPORTING
        FILELENGTH              = pf_filesize
*    HEADER                  =
      CHANGING
        DATA_TAB                = pt_file
      EXCEPTIONS
        FILE_OPEN_ERROR         = 1
        FILE_READ_ERROR         = 2
        NO_BATCH                = 3
        GUI_REFUSE_FILETRANSFER = 4
        INVALID_TYPE            = 5
        NO_AUTHORITY            = 6
        UNKNOWN_ERROR           = 7
        BAD_DATA_FORMAT         = 8
        HEADER_NOT_ALLOWED      = 9
        SEPARATOR_NOT_ALLOWED   = 10
        HEADER_TOO_LONG         = 11
        UNKNOWN_DP_ERROR        = 12
        ACCESS_DENIED           = 13
        DP_OUT_OF_MEMORY        = 14
        DISK_FULL               = 15
        DP_TIMEOUT              = 16
        others                  = 17
            .
    IF SY-SUBRC <> 0.
      MESSAGE ID SY-MSGID TYPE 'I' NUMBER SY-MSGNO
                 WITH SY-MSGV1 SY-MSGV2 SY-MSGV3 SY-MSGV4.
      exit.
    ENDIF.

endmethod.


method READ_TEXT.

      CALL METHOD CL_GUI_FRONTEND_SERVICES=>GUI_UPLOAD
      EXPORTING
        FILENAME                = full_name
        FILETYPE                = 'ASC'
*    HAS_FIELD_SEPARATOR     = SPACE
*    HEADER_LENGTH           = 0
*  IMPORTING
*    FILELENGTH              =
*    HEADER                  =
      CHANGING
        DATA_TAB                = pt_file
      EXCEPTIONS
        FILE_OPEN_ERROR         = 1
        FILE_READ_ERROR         = 2
        NO_BATCH                = 3
        GUI_REFUSE_FILETRANSFER = 4
        INVALID_TYPE            = 5
        NO_AUTHORITY            = 6
        UNKNOWN_ERROR           = 7
        BAD_DATA_FORMAT         = 8
        HEADER_NOT_ALLOWED      = 9
        SEPARATOR_NOT_ALLOWED   = 10
        HEADER_TOO_LONG         = 11
        UNKNOWN_DP_ERROR        = 12
        ACCESS_DENIED           = 13
        DP_OUT_OF_MEMORY        = 14
        DISK_FULL               = 15
        DP_TIMEOUT              = 16
        others                  = 17
            .
    IF SY-SUBRC <> 0.
      MESSAGE ID SY-MSGID TYPE 'I' NUMBER SY-MSGNO
                 WITH SY-MSGV1 SY-MSGV2 SY-MSGV3 SY-MSGV4.
      exit.
    ENDIF.

endmethod.


method RENAME.
*CALL METHOD SUPER->RENAME
*  EXPORTING
*    PF_NEWNAME = PF_NEWNAME
*    .

message 'Noch nicht implementiert'(001) type 'I'.

endmethod.


method WRITE_BIN.

    data lt_file type zapcmd_tbl_xstring.
    lt_file[] = pt_file[].

    CALL METHOD CL_GUI_FRONTEND_SERVICES=>GUI_DOWNLOAD
      EXPORTING
        BIN_FILESIZE            =  pf_filesize
        FILENAME                = full_name
        FILETYPE                = 'BIN'
*    APPEND                  = SPACE
*    WRITE_FIELD_SEPARATOR   = SPACE
*    HEADER                  = '00'
*    TRUNC_TRAILING_BLANKS   = SPACE
*    WRITE_LF                = 'X'
*    COL_SELECT              = SPACE
*    COL_SELECT_MASK         = SPACE
*  IMPORTING
*    FILELENGTH              =
      CHANGING
        DATA_TAB                = lt_file
      EXCEPTIONS
        FILE_WRITE_ERROR        = 1
        NO_BATCH                = 2
        GUI_REFUSE_FILETRANSFER = 3
        INVALID_TYPE            = 4
        NO_AUTHORITY            = 5
        UNKNOWN_ERROR           = 6
        HEADER_NOT_ALLOWED      = 7
        SEPARATOR_NOT_ALLOWED   = 8
        FILESIZE_NOT_ALLOWED    = 9
        HEADER_TOO_LONG         = 10
        DP_ERROR_CREATE         = 11
        DP_ERROR_SEND           = 12
        DP_ERROR_WRITE          = 13
        UNKNOWN_DP_ERROR        = 14
        ACCESS_DENIED           = 15
        DP_OUT_OF_MEMORY        = 16
        DISK_FULL               = 17
        DP_TIMEOUT              = 18
        FILE_NOT_FOUND          = 19
        DATAPROVIDER_EXCEPTION  = 20
        CONTROL_FLUSH_ERROR     = 21
        others                  = 22
            .
    IF SY-SUBRC <> 0.
      MESSAGE ID SY-MSGID TYPE 'I' NUMBER SY-MSGNO
                 WITH SY-MSGV1 SY-MSGV2 SY-MSGV3 SY-MSGV4.
      exit.
    ENDIF.

endmethod.


method WRITE_TEXT.

     data lt_file type zapcmd_tbl_string.
    lt_file[] = pt_file[].

    CALL METHOD CL_GUI_FRONTEND_SERVICES=>GUI_DOWNLOAD
      EXPORTING
*    BIN_FILESIZE            =
        FILENAME                = full_name
        FILETYPE                = 'ASC'
*    APPEND                  = SPACE
*    WRITE_FIELD_SEPARATOR   = SPACE
*    HEADER                  = '00'
*    TRUNC_TRAILING_BLANKS   = SPACE
*    WRITE_LF                = 'X'
*    COL_SELECT              = SPACE
*    COL_SELECT_MASK         = SPACE
*  IMPORTING
*    FILELENGTH              =
      CHANGING
        DATA_TAB                = lt_file
      EXCEPTIONS
        FILE_WRITE_ERROR        = 1
        NO_BATCH                = 2
        GUI_REFUSE_FILETRANSFER = 3
        INVALID_TYPE            = 4
        NO_AUTHORITY            = 5
        UNKNOWN_ERROR           = 6
        HEADER_NOT_ALLOWED      = 7
        SEPARATOR_NOT_ALLOWED   = 8
        FILESIZE_NOT_ALLOWED    = 9
        HEADER_TOO_LONG         = 10
        DP_ERROR_CREATE         = 11
        DP_ERROR_SEND           = 12
        DP_ERROR_WRITE          = 13
        UNKNOWN_DP_ERROR        = 14
        ACCESS_DENIED           = 15
        DP_OUT_OF_MEMORY        = 16
        DISK_FULL               = 17
        DP_TIMEOUT              = 18
        FILE_NOT_FOUND          = 19
        DATAPROVIDER_EXCEPTION  = 20
        CONTROL_FLUSH_ERROR     = 21
        others                  = 22
            .
    IF SY-SUBRC <> 0.
      MESSAGE ID SY-MSGID TYPE 'I' NUMBER SY-MSGNO
                 WITH SY-MSGV1 SY-MSGV2 SY-MSGV3 SY-MSGV4.
      exit.
    ENDIF.

endmethod.
ENDCLASS.
