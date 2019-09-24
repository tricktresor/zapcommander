FUNCTION ZAPCMD_WRITE_BINFILE.
*"----------------------------------------------------------------------
*"*"Lokale Schnittstelle:
*"  IMPORTING
*"     VALUE(IV_FULL_NAME) TYPE  TEXT255
*"     VALUE(IV_FILESIZE) TYPE  I
*"  TABLES
*"      IT_FILE STRUCTURE  ZAPCMD_DATA_BUFFER
*"  EXCEPTIONS
*"      NOT_FOUND
*"----------------------------------------------------------------------

 data lt_file type zapcmd_tbl_xstring.


  import bin = lt_file from INTERNAL TABLE it_file[].

    data lf_string like line of lt_file.
    data lf_subrc type sysubrc.
    data lf_filesize type i.
    data lf_mess(100) type c.
    lf_filesize = iv_filesize.
    refresh lt_file.
*** Datei oeffen ***
    open dataset iv_full_name for output in binary mode
      message lf_mess. "encoding default.
    if sy-subrc eq 0.
       loop at lt_file into lf_string.
        if lf_filesize >= 1024.
          transfer lf_string to iv_full_name.
          lf_filesize = lf_filesize - 1024.
        else.
          transfer lf_string to iv_full_name length lf_filesize.
        endif.
      endloop.
      close dataset iv_full_name.
    else.
      message id sy-msgid type 'I' number sy-msgno
      with sy-msgv1 sy-msgv2 sy-msgv3 sy-msgv4 RAISING not_found.
    endif.







ENDFUNCTION.
