FUNCTION ZAPCMD_EXEC_ABAP.
*"----------------------------------------------------------------------
*"*"Lokale Schnittstelle:
*"  IMPORTING
*"     VALUE(IV_FULL_NAME) TYPE  TEXT255
*"     VALUE(IV_SHORTNAME) TYPE  TEXT255
*"  EXCEPTIONS
*"      NOT_FOUND
*"      NO_EXEC
*"----------------------------------------------------------------------

  DATA l_repname TYPE syrepid.
data: syn_err_mess(240),                       "Syntax-check
      syn_err_line type i,                     "Syntax-check
      syn_err_word(72).                        "Syntax-check

  CONCATENATE 'ZTMP_' iv_shortname INTO l_repname.
  DATA lt_code TYPE TABLE OF string.



  data lf_string type line of zapcmd_tbl_string.
    data lt_string type zapcmd_tbl_string.
    data lf_length type i.
    data lf_filesize type i.
    refresh lt_code.
    lf_filesize = 0.
*** Datei oeffen ***
    open dataset iv_full_name for input in text mode encoding default.
    if sy-subrc eq 0.
      do.
*** jede Zeile einzeln einlesen ***
        read dataset iv_full_name into lf_string length lf_length.
        if sy-subrc ne 0.
          exit.
        else.
          append lf_string to lt_code.
          lf_filesize = lf_filesize + lf_length.
        endif.
      enddo.
      close dataset iv_full_name.
    else.
      message id sy-msgid type 'I' number sy-msgno
      with sy-msgv1 sy-msgv2 sy-msgv3 sy-msgv4 RAISING not_found.
    endif.


  SYNTAX-CHECK FOR lt_code
    program 'ZAPCMD_SAPCOMMANDER'
    MESSAGE syn_err_mess
    LINE   syn_err_line
    WORD   syn_err_word.

  IF sy-subrc EQ 0.

    INSERT REPORT l_repname FROM lt_code.
    IF sy-subrc EQ 0.
*     Modifzierter i_code zu Report generieren
      GENERATE REPORT l_repname.

*     generierter i_code aufrufen
      SUBMIT (l_repname) VIA SELECTION-SCREEN
        AND RETURN.
    ENDIF.
  ELSE.

    data l_errortext type string.
    data l_line type string.
    l_line = syn_err_line.
    concatenate 'Syntaxfehler in Zeile'(001) l_line ':"' syn_err_mess '"' into l_errortext.


    message l_errortext type 'E' RAISING no_exec.

  ENDIF.



ENDFUNCTION.
