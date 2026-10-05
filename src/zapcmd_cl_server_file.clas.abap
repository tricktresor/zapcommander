class ZAPCMD_CL_SERVER_FILE definition
  public
  inheriting from ZAPCMD_CL_FILE
  create public .

*"* public components of class ZAPCMD_CL_SERVER_FILE
*"* do not include other source files here!!!
public section.

  methods CONSTRUCTOR .

  methods DELETE
    redefinition .
  methods EXECUTE
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
*"* protected components of class ZAPCMD_CL_SERVER_FILE
*"* do not include other source files here!!!
private section.
*"* private components of class ZAPCMD_CL_SERVERFILE
*"* do not include other source files here!!!
ENDCLASS.



CLASS ZAPCMD_CL_SERVER_FILE IMPLEMENTATION.


method CONSTRUCTOR.

    call method super->constructor.
    if sy-opsys = 'Windows NT' or sy-opsys = 'DOS'. "#EC NOTEXT
      separator = '\'.
    else.
      separator = '/'.
    endif.
    server_area = zapcmd_cl_knot=>co_area_applserv.

endmethod.


method DELETE.

*    CALL FUNCTION 'DX_FILE_DELETE'
*       EXPORTING
**     FILETYPE                          =
*         FILENAME                          = full_name
*       EXCEPTIONS
*         FILE_HANDLING_ERROR               = 1
*         NO_AUTHORITY_TO_DELETE_FILE       = 2
*         FILE_DOESNOT_EXIST                = 3
*         FILE_IS_USED                      = 4
*         DB_ERROR                          = 5
*         OTHERS                            = 6
*                .
    DATA lx_error TYPE REF TO cx_root.
    DATA lv_text TYPE string.
    TRY.
        delete dataset full_name.
        IF sy-subrc <> 0.
          MESSAGE 'File could not be deleted'(003) TYPE 'S' DISPLAY LIKE 'E'.
        ENDIF.
      CATCH cx_sy_file_access_error INTO lx_error.
        lv_text = lx_error->get_text( ).
        MESSAGE lv_text TYPE 'S' DISPLAY LIKE 'E'.
    ENDTRY.


endmethod.


METHOD execute.

*        ZAPCMD_CL_EDITOR=>call_editor(
*          pf_file = me
*          pf_readonly = 'X'
*        ).

  DATA l_extension TYPE string.
  l_extension = extension.
  TRANSLATE l_extension TO UPPER CASE.

  IF strlen( l_extension ) >= 3 and l_extension(3) = 'ABA'.

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
        pf_filesize = lf_filesize
      EXCEPTIONS
        failed      = 1.
    IF sy-subrc <> 0.
      RETURN.
    ENDIF.

    CALL METHOD lf_destfile->write_bin
      EXPORTING
        pt_file     = lt_file
        pf_filesize = lf_filesize
      EXCEPTIONS
        failed      = 1.
    IF sy-subrc <> 0.
      RETURN.
    ENDIF.

    lf_destfile->execute( ).

  ENDIF.
*    data lf_string type string.
*    data lt_string type table of string.
*    data ok_code type sy-ucomm.
*
**** Datei oeffen ***
*    open dataset full_name for input in text mode encoding default.
*    if sy-subrc eq 0.
*      do.
**** jede Zeile einzeln einlesen ***
*        read dataset full_name into lf_string.
*        if sy-subrc ne 0.
*          exit.
*        else.
*          append lf_string to lt_string.
*        endif.
*      enddo.
*      close dataset full_name.
*    else.
*      message id sy-msgid type 'I' number sy-msgno
*      with sy-msgv1 sy-msgv2 sy-msgv3 sy-msgv4.
*      exit.
*    endif.
*
**----------------------------------------------------------------------
**
** Editor aufrufen für die interne Tabelle
**
**----------------------------------------------------------------------
**
*
*    editor-call for lt_string.
*    ok_code = sy-ucomm.
*
** Bei User-Wahl beenden
*    if ok_code eq 'WB_CANCEL' or
*    ok_code eq 'WB_END' or
*    ok_code eq 'WB_BACK'.
*      exit.
*    endif.
*
*    if ok_code eq 'WB_SAVE'.
*
**----------------------------------------------------------------------
**
** Änderungen von Editor in Datei zurückschreiben
**
**----------------------------------------------------------------------
**
**      open dataset full_name for output in text mode.
**      if sy-subrc eq 0.
**        loop at lt_string into lf_string.
**          transfer lf_string to full_name.
**        endloop.
**        close dataset full_name.
**      else.
**        message id sy-msgid type 'I' number sy-msgno
**        with sy-msgv1 sy-msgv2 sy-msgv3 sy-msgv4.
**        exit.
**      endif.
*
*    endif.
ENDMETHOD.


method READ_BIN.

    data lf_string like line of pt_file.
    data lf_subrc type sysubrc.
    data lf_length type i.
    data lf_mess(100) type c.
    data lx_error type ref to cx_root.
    data lv_text type string.
    refresh pt_file.
*** Datei oeffen ***
    pf_filesize = 0.
    try.
        open dataset full_name for input in binary mode
          message lf_mess. "encoding default.
        if sy-subrc eq 0.
          do.
*** jede Zeile einzeln einlesen ***
            clear lf_string.
            read dataset full_name into lf_string length lf_length.
            lf_subrc = sy-subrc.
            append lf_string to pt_file.
            pf_filesize = pf_filesize + lf_length.
            if lf_subrc ne 0.
              exit.
            endif.
          enddo.
          close dataset full_name.
        else.
          message lf_mess type 'I'.
          raise failed.
        endif.
      " no authority, I/O error, or file too large for type I (2 GB)
      catch cx_sy_file_access_error cx_sy_arithmetic_overflow into lx_error.
        close dataset full_name.
        refresh pt_file.
        lv_text = lx_error->get_text( ).
        message lv_text type 'I'.
        raise failed.
    endtry.

endmethod.


method READ_TEXT.

    data lf_string type line of zapcmd_tbl_string.
    data lf_length type i.
    data lf_mess type c length 100.
    data lx_error type ref to cx_root.
    data lv_text type string.
    refresh pt_file.
    pf_filesize = 0.
*** Datei oeffen ***
    try.
        open dataset full_name for input in text mode encoding default
          with smart linefeed
          message lf_mess.
        if sy-subrc eq 0.
          do.
*** jede Zeile einzeln einlesen ***
            read dataset full_name into lf_string length lf_length.
            if sy-subrc ne 0.
              exit.
            else.
              append lf_string to pt_file.
              pf_filesize = pf_filesize + lf_length.
            endif.
          enddo.
          close dataset full_name.
        else.
          message lf_mess type 'I'.
          exit.
        endif.
      " no authority, I/O error, file too large, or not in the system code
      " page; nothing is shown then, so a later save cannot destroy the file
      catch cx_sy_file_access_error cx_sy_conversion_codepage
            cx_sy_arithmetic_overflow into lx_error.
        close dataset full_name.
        refresh pt_file.
        lv_text = lx_error->get_text( ).
        message lv_text type 'I'.
    endtry.


endmethod.


method RENAME.

data lf_oldlongname type zapcmd_fullname.
data lf_oldname type zapcmd_filename.
data lt_output type zapcmd_tbl_string.
data lv_return_code type i.
data lv_output type string.

 lf_oldlongname = me->full_name.
 lf_oldname = me->name.

CALL METHOD SUPER->RENAME
  EXPORTING
    PF_NEWNAME = PF_NEWNAME
    .

data lf_command type string.
data lf_parameter type string.
if separator = '\'.
  lf_parameter = |{ quote_os_arg( lf_oldlongname ) } { quote_os_arg( name ) }|.
  lf_command = 'ren'.
else.
  lf_parameter = |{ quote_os_arg( lf_oldlongname ) } { quote_os_arg( full_name ) }|.
  lf_command = 'mv'.
endif.
 call method exec_server
      exporting
        pf_command = lf_command
        pf_parameter = lf_parameter
      importing
        ptx_output = lt_output
        pf_return_code = lv_return_code.
 if lv_return_code <> 0.
   super->rename( pf_newname = lf_oldname ).
   read table lt_output index 1 into lv_output.
   message lv_output type 'S' display like 'E'.
 endif.

endmethod.


method WRITE_BIN.

    data lf_string like line of pt_file.
    data lf_filesize type i.
    data lf_mess type c length 100.
    data lx_error type ref to cx_root.
    data lv_text type string.
    lf_filesize = pf_filesize.
    try.
        open dataset full_name for output in binary mode
          message lf_mess.
        if sy-subrc eq 0.
          loop at pt_file into lf_string.
            if lf_filesize >= 1024.
              transfer lf_string to full_name.
              lf_filesize = lf_filesize - 1024.
            else.
              transfer lf_string to full_name length lf_filesize.
            endif.
          endloop.
          close dataset full_name.
        else.
          message lf_mess type 'I'.
          raise failed.
        endif.
      catch cx_sy_file_access_error into lx_error.
        close dataset full_name.
        lv_text = lx_error->get_text( ).
        message lv_text type 'I'.
        raise failed.
    endtry.

endmethod.


method WRITE_TEXT.

    data lf_string type string.
    data lf_mess type c length 100.
    data lx_error type ref to cx_root.
    data lv_text type string.

    try.
        open dataset full_name for output in text mode encoding default
          message lf_mess.
        if sy-subrc eq 0.
          loop at pt_file into lf_string.
            transfer lf_string to full_name.
          endloop.
          close dataset full_name.
        else.
          message lf_mess type 'I'.
          exit.
        endif.
      catch cx_sy_file_access_error cx_sy_conversion_codepage into lx_error.
        close dataset full_name.
        lv_text = lx_error->get_text( ).
        message lv_text type 'I'.
    endtry.

endmethod.
ENDCLASS.
