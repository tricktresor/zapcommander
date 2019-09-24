class ZAPCMD_CL_RFC_FILE definition
  public
  inheriting from ZAPCMD_CL_FILE
  final
  create public .

*"* public components of class ZAPCMD_CL_RFC_FILE
*"* do not include other source files here!!!
public section.

  methods CONSTRUCTOR
    importing
      !IV_RFCDEST type RFCDEST
    exceptions
      NOT_INSTALLED .

  methods DELETE
    redefinition .
  methods EXECUTE
    redefinition .
  methods READ_BIN
    redefinition .
  methods READ_TEXT
    redefinition .
  methods RENAME
    redefinition .
  methods WRITE_BIN
    redefinition .
  methods WRITE_TEXT
    redefinition .
protected section.
*"* protected components of class ZAPCMD_CL_SERVER_FILE
*"* do not include other source files here!!!
private section.
*"* private components of class ZAPCMD_CL_RFC_FILE
*"* do not include other source files here!!!

  data RFCDEST type RFCDEST .
ENDCLASS.



CLASS ZAPCMD_CL_RFC_FILE IMPLEMENTATION.


method CONSTRUCTOR.

* ...
    call method super->constructor.

    server_area = 'R'.
    rfcdest = iv_rfcdest.
    try.
    data l_opsys type syopsys.
    CALL FUNCTION 'ZAPCMD_GET_OPSYS'
      DESTINATION rfcdest
      IMPORTING
         EV_OPSYS       = l_opsys
      EXCEPTIONS
        SYSTEM_FAILURE = 1
        COMMUNICATION_failure = 2.
      .
    if sy-subrc <> 0.
      raise not_installed.
    endif.
    CATCH cx_root.
      raise not_installed.
    endtry.
    if l_opsys = 'Windows NT' or sy-opsys = 'DOS'. "#EC NOTEXT
      separator = '\'.
    else.
      separator = '/'.
    endif.


endmethod.


method DELETE.

    data l_full_name type text255.
    l_full_name = full_name.
  CALL FUNCTION 'ZAPCMD_DELETE_FILE'
  DESTINATION rfcdest
    EXPORTING
      iv_full_name       = l_full_name
   EXCEPTIONS
     NOT_FOUND          = 1
     OTHERS             = 2
            .
  IF sy-subrc <> 0.
 MESSAGE ID SY-MSGID TYPE SY-MSGTY NUMBER SY-MSGNO
         WITH SY-MSGV1 SY-MSGV2 SY-MSGV3 SY-MSGV4.
  ENDIF.


endmethod.


METHOD EXECUTE.

*        ZAPCMD_CL_EDITOR=>call_editor(
*          pf_file = me
*          pf_readonly = 'X'
*        ).

  DATA l_extension TYPE string.
  l_extension = extension.
  TRANSLATE l_extension TO UPPER CASE.

  IF l_extension(3) = 'ABA'.

   execute_abap( ).

  ELSE.


    DATA lf_destfile TYPE REF TO zapcmd_cl_file.
    DATA lf_destdir TYPE REF TO zapcmd_cl_frontend_dir.
    DATA lt_file TYPE zapcmd_tbl_xstring.

    CREATE OBJECT lf_destdir TYPE zapcmd_cl_frontend_dir.
    lf_destdir->init( ).

    CALL METHOD lf_destdir->create_file
      EXPORTING
        pf_filename = me->name
      IMPORTING
        pf_file     = lf_destfile.

    DATA lf_filesize TYPE i.

    CALL METHOD me->read_bin
      IMPORTING
        pt_file     = lt_file
        pf_filesize = lf_filesize.

    CALL METHOD lf_destfile->write_bin
      EXPORTING
        pt_file     = lt_file
        pf_filesize = lf_filesize.

    lf_destfile->execute( ).

  ENDIF.

ENDMETHOD.


method READ_BIN.

    data lt_file type TABLE OF ZAPCMD_DATA_BUFFER.

    data l_full_name type text255.
    l_full_name = full_name.
    CALL FUNCTION 'ZAPCMD_READ_BINFILE'
      DESTINATION rfcdest
      EXPORTING
        iv_full_name       = l_full_name
     IMPORTING
       EV_FILESIZE        = pf_filesize
      tables
        et_file            = lt_file
     EXCEPTIONS
       NOT_FOUND          = 1
       OTHERS             = 2
              .
    IF sy-subrc <> 0.
      MESSAGE ID SY-MSGID TYPE SY-MSGTY NUMBER SY-MSGNO
         WITH SY-MSGV1 SY-MSGV2 SY-MSGV3 SY-MSGV4.
      return.
    ENDIF.

    import bin = pt_file from INTERNAL TABLE lt_file.


endmethod.


method READ_TEXT.

    data lt_file type table of ZAPCMD_DATA_BUFFER.
    data l_full_name type text255.
    l_full_name = full_name.
    CALL FUNCTION 'ZAPCMD_READ_TEXTFILE'
      DESTINATION rfcdest
      EXPORTING
        iv_full_name       = l_full_name
     IMPORTING
       EV_FILESIZE        = pf_filesize
      tables
        et_file            = lt_file
     EXCEPTIONS
       NOT_FOUND          = 1
       OTHERS             = 2
              .
    IF sy-subrc <> 0.
       MESSAGE ID SY-MSGID TYPE SY-MSGTY NUMBER SY-MSGNO
         WITH SY-MSGV1 SY-MSGV2 SY-MSGV3 SY-MSGV4.
       return.
    ENDIF.

    import text = pt_file from INTERNAL TABLE lt_file.


endmethod.


method RENAME.

data lf_oldlongname type zapcmd_fullname.

 lf_oldlongname = me->full_name.

CALL METHOD SUPER->RENAME
  EXPORTING
    PF_NEWNAME = PF_NEWNAME
    .

data lf_command type text255.
data lf_parameter type text255.
if separator = '\'.
  concatenate lf_oldlongname name into lf_parameter
    separated by space.
  lf_command = 'ren'.
else.
  concatenate lf_oldlongname full_name into lf_parameter
    separated by space.
  lf_command = 'mv'.
endif.

 call function 'ZAPCMD_EXEC_CMD'
   DESTINATION rfcdest
  EXPORTING
    IV_COMMAND         = lf_command
*    IV_DIR             =
    IV_PARAMETER       = lf_parameter
*  TABLES
*    ET_OUTPUT          =
  EXCEPTIONS
    NOT_FOUND          = 1
    OTHERS             = 2
           .
 if sy-subrc <> 0.
  MESSAGE ID SY-MSGID TYPE SY-MSGTY NUMBER SY-MSGNO
          WITH SY-MSGV1 SY-MSGV2 SY-MSGV3 SY-MSGV4.
 endif.


endmethod.


METHOD write_bin.

  DATA lt_file TYPE TABLE OF zapcmd_data_buffer.

  EXPORT bin = pt_file TO INTERNAL TABLE lt_file[].


  CALL FUNCTION 'ZAPCMD_WRITE_BINFILE'
    DESTINATION rfcdest
    EXPORTING
      iv_full_name = full_name
      iv_filesize  = pf_filesize
    TABLES
      it_file      = lt_file
    EXCEPTIONS
      not_found    = 1
      OTHERS       = 2.
  IF sy-subrc <> 0.
    MESSAGE ID sy-msgid TYPE sy-msgty NUMBER sy-msgno
            WITH sy-msgv1 sy-msgv2 sy-msgv3 sy-msgv4.
  ENDIF.


ENDMETHOD.


METHOD write_text.

  DATA lt_file TYPE TABLE OF zapcmd_data_buffer.

  EXPORT text = pt_file TO INTERNAL TABLE lt_file[].

  CALL FUNCTION 'ZAPCMD_WRITE_TEXTFILE'
    DESTINATION rfcdest
    EXPORTING
      iv_full_name = full_name
    TABLES
      it_file      = lt_file
    EXCEPTIONS
      not_found    = 1
      OTHERS       = 2.
  IF sy-subrc <> 0.
    MESSAGE ID sy-msgid TYPE sy-msgty NUMBER sy-msgno
            WITH sy-msgv1 sy-msgv2 sy-msgv3 sy-msgv4.
  ENDIF.

ENDMETHOD.
ENDCLASS.
