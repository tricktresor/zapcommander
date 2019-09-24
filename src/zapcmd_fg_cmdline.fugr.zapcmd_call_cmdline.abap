FUNCTION ZAPCMD_CALL_CMDLINE.
*"--------------------------------------------------------------------
*"*"Lokale Schnittstelle:
*"  IMPORTING
*"     REFERENCE(PF_CMDLINE) TYPE REF TO ZAPCMD_CL_CMDLINE
*"--------------------------------------------------------------------
gf_cmdline = pf_cmdline.

  CALL SCREEN 300 STARTING AT 10 3 ENDING AT 100 27.





ENDFUNCTION.
