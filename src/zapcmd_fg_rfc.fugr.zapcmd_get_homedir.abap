FUNCTION ZAPCMD_GET_HOMEDIR.
*"----------------------------------------------------------------------
*"*"Lokale Schnittstelle:
*"  EXPORTING
*"     VALUE(EV_FULLNAME) TYPE  TEXT255
*"----------------------------------------------------------------------

    DATA lf_temp(255) TYPE c.
    CALL 'C_SAPGPARAM' ID 'NAME'  FIELD 'DIR_HOME'
                       ID 'VALUE' FIELD EV_FULLNAME.


ENDFUNCTION.
