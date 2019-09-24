FUNCTION ZAPCMD_DELETE_FILE.
*"----------------------------------------------------------------------
*"*"Lokale Schnittstelle:
*"  IMPORTING
*"     VALUE(IV_FULL_NAME) TYPE  TEXT255
*"  EXCEPTIONS
*"      NOT_FOUND
*"----------------------------------------------------------------------

  delete dataset iv_full_name.
    IF SY-SUBRC <> 0.
      MESSAGE ID SY-MSGID TYPE SY-MSGTY NUMBER SY-MSGNO
           WITH SY-MSGV1 SY-MSGV2 SY-MSGV3 SY-MSGV4 RAISING not_found.
    ENDIF.



ENDFUNCTION.
