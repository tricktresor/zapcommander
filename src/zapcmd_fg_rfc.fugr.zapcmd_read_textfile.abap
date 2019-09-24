FUNCTION ZAPCMD_READ_TEXTFILE.
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

    data lf_string type line of zapcmd_tbl_string.
    data lt_string type zapcmd_tbl_string.
    data lf_length type i.
    refresh et_file.
    ev_filesize = 0.
*** Datei oeffen ***
    open dataset iv_full_name for input in text mode encoding default.
    if sy-subrc eq 0.
      do.
*** jede Zeile einzeln einlesen ***
        read dataset iv_full_name into lf_string length lf_length.
        if sy-subrc ne 0.
          exit.
        else.
          append lf_string to lt_string.
          ev_filesize = ev_filesize + lf_length.
        endif.
      enddo.
      close dataset iv_full_name.
    else.
      message id sy-msgid type 'I' number sy-msgno
      with sy-msgv1 sy-msgv2 sy-msgv3 sy-msgv4 RAISING not_found.
    endif.

    export text = lt_string TO internal table et_file[].



ENDFUNCTION.
