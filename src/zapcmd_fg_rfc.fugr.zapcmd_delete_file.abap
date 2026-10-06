FUNCTION ZAPCMD_DELETE_FILE.
*"----------------------------------------------------------------------
*"*"Lokale Schnittstelle:
*"  IMPORTING
*"     VALUE(IV_FULL_NAME) TYPE  TEXT255
*"  EXCEPTIONS
*"      NOT_FOUND
*"----------------------------------------------------------------------

  " DELETE DATASET sets no SY-MSG* fields, so the caller shows its own text
  TRY.
      DELETE DATASET iv_full_name.
      IF sy-subrc <> 0.
        RAISE not_found.
      ENDIF.
    CATCH cx_sy_file_access_error.
      RAISE not_found.
  ENDTRY.



ENDFUNCTION.
