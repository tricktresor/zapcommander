FUNCTION-POOL ZAPCMD_FG_EDITOR.             "MESSAGE-ID ..

  DATA gf_gui_editor_container TYPE REF TO cl_gui_docking_container.
  DATA gf_editor TYPE REF TO zapcmd_cl_editor.
  DATA ok_code200 LIKE sy-ucomm.

*&---------------------------------------------------------------------*
*&      Module  STATUS_0200  OUTPUT
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
module STATUS_0200 output.

*  SET PF-STATUS 'xxxxxxxx'.
*  SET TITLEBAR 'xxx'.
  SET PF-STATUS 'STATUS200'.
*  SET TITLEBAR 'TITLE200'.

  DATA lf_title1(200) TYPE c.
  DATA lf_title2(200) TYPE c.
  DATA lf_title3(200) TYPE c.
  DATA lf_title4(200) TYPE c.
  CALL METHOD gf_editor->get_title
    IMPORTING
      pf_title1 = lf_title1
      pf_title2 = lf_title2
      pf_title3 = lf_title3
      pf_title4 = lf_title4.

  SET TITLEBAR 'TITLE200' WITH lf_title1 lf_title2 lf_title3 lf_title4.

  IF gf_gui_editor_container IS INITIAL.
    CREATE OBJECT gf_gui_editor_container
      EXPORTING
*        ratio = 100
        extension = 2000
        .
*           EXPORTING CONTAINER_NAME = 'CTRL2'.

*    data lf_height type i.
*    lf_height = ( gs_factors-screen-y - 0 ) / 1.
*    data lf_width type i.
*    lf_width = ( gs_factors-screen-x - 0 ) / 1.
*
*    CALL METHOD gf_gui_editor_container->SET_HEIGHT
*      EXPORTING
*        HEIGHT     = lf_height
*            .
*    CALL METHOD gf_gui_editor_container->SET_WIDTH
*      EXPORTING
*        WIDTH     = lf_WIDTH
*            .

  ENDIF.

  CALL METHOD gf_editor->show
    EXPORTING
      pf_container = gf_gui_editor_container.

endmodule.                 " STATUS_0200  OUTPUT
*&---------------------------------------------------------------------*
*&      Module  USER_COMMAND_0200  INPUT
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
module USER_COMMAND_0200 input.

*   to react on oi_custom_events:
  CALL METHOD cl_gui_cfw=>dispatch.
  CALL METHOD gf_editor->user_command
    CHANGING
      e_ucomm = ok_code200.
  CASE ok_code200.
    WHEN 'BACK'.
      LEAVE TO SCREEN 0.
    WHEN 'EXIT'.
      LEAVE PROGRAM.
  ENDCASE.
  CLEAR ok_code200.

endmodule.                 " USER_COMMAND_0200  INPUT
