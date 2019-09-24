FUNCTION ZAPCMD_CHECK_DIR.
*"----------------------------------------------------------------------
*"*"Lokale Schnittstelle:
*"  IMPORTING
*"     VALUE(IV_DIR) TYPE  TEXT255
*"  EXPORTING
*"     VALUE(EV_REACHABLE) TYPE  XFELD
*"----------------------------------------------------------------------

  DATA errno(3)     TYPE c.
  DATA errmsg(40)   TYPE c.
  DATA lf_filter(255) TYPE c.

  clear ev_reachable.

   CALL 'C_DIR_READ_FINISH'             " just to be sure
        ID 'ERRNO'  FIELD errno
        ID 'ERRMSG' FIELD errmsg.

    CALL 'C_DIR_READ_START' ID 'DIR'    FIELD IV_DIR
                            ID 'FILE'   FIELD lf_filter
                            ID 'ERRNO'  FIELD errno
                            ID 'ERRMSG' FIELD errmsg.
    IF sy-subrc = 0.

      CALL 'C_DIR_READ_NEXT'
        ID 'ERRNO'  FIELD errno
        ID 'ERRMSG' FIELD errmsg.
      IF sy-subrc < 4.


        ev_reachable = 'X'.

      ENDIF.

      CALL 'C_DIR_READ_FINISH'
        ID 'ERRNO'  FIELD errno
        ID 'ERRMSG' FIELD errmsg.


    ENDIF.



ENDFUNCTION.
