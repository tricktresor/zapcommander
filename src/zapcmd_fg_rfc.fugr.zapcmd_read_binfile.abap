FUNCTION ZAPCMD_READ_BINFILE.
*"----------------------------------------------------------------------
*"*"Lokale Schnittstelle:
*"  IMPORTING
*"     VALUE(IV_FULL_NAME) TYPE  TEXT255
*"  EXPORTING
*"     VALUE(EV_FILESIZE) TYPE  I
*"  TABLES
*"      ET_FILE STRUCTURE  ZAPCMD_DATA_BUFFER
*"  EXCEPTIONS
*"      NOT_FOUND
*"----------------------------------------------------------------------
 data lt_file type zapcmd_tbl_xstring.
 data lf_string like line of lt_file.
    data lf_subrc type sysubrc.
    data lf_length type i.
    data lf_mess(100) type c.
    refresh lt_file.
*** Datei oeffen ***
    ev_filesize = 0.
    open dataset iv_full_name for input in binary mode
      message lf_mess. "encoding default.
    if sy-subrc eq 0.
      do.
*** jede Zeile einzeln einlesen ***
        clear lf_string.
        read dataset iv_full_name into lf_string length lf_length.
        lf_subrc = sy-subrc.
        append lf_string to lt_file.
        ev_filesize = ev_filesize + lf_length.
        if lf_subrc ne 0.
          exit.
        endif.
      enddo.
      close dataset iv_full_name.
    else.
      message id sy-msgid type 'I' number sy-msgno
      with sy-msgv1 sy-msgv2 sy-msgv3 sy-msgv4 RAISING not_found.
    endif.

    export bin = lt_file to INTERNAL TABLE et_file[].



ENDFUNCTION.
