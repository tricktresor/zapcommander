FUNCTION-POOL ZAPCMD_FG_CMDLINE.            "MESSAGE-ID ..

 DATA gf_gui_cmdline_container type ref to cl_gui_custom_container.
  data gf_cmdline type ref to zapcmd_cl_cmdline.

*&---------------------------------------------------------------------*
*&      Module  STATUS_0300  OUTPUT
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
module STATUS_0300 output.
*  SET PF-STATUS 'xxxxxxxx'.
*  SET TITLEBAR 'xxx'.
  SET PF-STATUS 'STATUS300'.
*  SET TITLEBAR 'xxx'.
  IF gf_gui_cmdline_container IS INITIAL.
    create object gf_gui_cmdline_container
      exporting
        container_name = 'CUST300'
        repid = sy-repid
        dynnr = '0300'.
  ENDIF.

  CALL METHOD gf_cmdline->show
    EXPORTING
      pf_container = gf_gui_cmdline_container.
endmodule.                 " STATUS_0300  OUTPUT
*&---------------------------------------------------------------------*
*&      Module  USER_COMMAND_0300  INPUT
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
module USER_COMMAND_0300 input.

    leave to screen 0.

endmodule.                 " USER_COMMAND_0300  INPUT
