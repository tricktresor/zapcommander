*&---------------------------------------------------------------------*
*& Report  ZAPCMD_SAPCOMMANDER                                           *
*&---------------------------------------------------------------------*
*& https://github.com/tricktresor/zapcommander
*& https://code.google.com/archive/p/sapcommander/
*&---------------------------------------------------------------------*
REPORT  zapcmd_sapcommander.


PARAMETER p_ltype TYPE syucomm DEFAULT zapcmd_cl_dir=>co_default NO-DISPLAY.
PARAMETER p_ldir TYPE string DEFAULT space NO-DISPLAY.
PARAMETER p_rtype TYPE syucomm DEFAULT zapcmd_cl_dir=>co_default NO-DISPLAY.
PARAMETER p_rdir TYPE string DEFAULT space NO-DISPLAY.


START-OF-SELECTION.
  DATA lf_id TYPE indx_srtfd.
  CONCATENATE 'ZAPCMD' sy-uname INTO lf_id.
  IF p_ltype = zapcmd_cl_dir=>co_default OR p_rtype = zapcmd_cl_dir=>co_default.
    DATA l_left TYPE zapcmd_t_dir.
    DATA l_right TYPE zapcmd_t_dir.
    IMPORT left = l_left
    right = l_right
    FROM DATABASE indx(zc)
    ID lf_id.
    IF sy-subrc = 0.
      IF p_ltype = zapcmd_cl_dir=>co_default.

        p_ltype = l_left-type.
        p_ldir  = l_left-dir.
      ENDIF.
      IF p_rtype = zapcmd_cl_dir=>co_default.

        p_rtype = l_right-type.
        p_rdir  = l_right-dir.

      ENDIF.
    ENDIF.
  ENDIF.


  DATA ok_code100 LIKE sy-ucomm.
  DATA gf_gui_commander_container TYPE REF TO cl_gui_custom_container.
  DATA gf_gui_dirname_container TYPE REF TO cl_gui_custom_container.
  DATA gf_commander TYPE REF TO zapcmd_cl_commander.
  DATA gf_cmdline TYPE REF TO zapcmd_cl_cmdline.
  DATA g_cmdline TYPE string.
  DATA g_dirname TYPE string.
  DATA lf_temp TYPE REF TO string.
  DATA lf_dirname TYPE REF TO string.

  GET REFERENCE OF g_cmdline INTO lf_temp.
  GET REFERENCE OF g_dirname INTO lf_dirname.

  CREATE OBJECT gf_commander
      EXPORTING
        pf_left_type  = p_ltype
        pf_left_dir   = p_ldir
        pf_right_type = p_rtype
        pf_right_dir  = p_rdir
        pf_dirname    = lf_dirname.

  DATA lf_activelist TYPE REF TO zapcmd_refref_filelist.
  GET REFERENCE OF gf_commander->cf_activelist INTO lf_activelist.


  CREATE OBJECT gf_cmdline
    EXPORTING
      pf_cmdline = lf_temp
      pf_dir = lf_activelist.


  CALL SCREEN 110.


*type-pools: cntl.
*data gs_factors type cntl_metric_factors.
*
*CALL METHOD CL_GUI_CFW=>GET_METRIC_FACTORS
*  RECEIVING
*    METRIC_FACTORS = gs_factors
*    .
**if gs_factors-screen-y > 768.
**  call screen 110.
**else.
**  call screen 100.
**endif.
*&---------------------------------------------------------------------*
*&      Module  STATUS_0100  OUTPUT
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
MODULE status_0100 OUTPUT.
  SET PF-STATUS 'STATUS100'.
  SET TITLEBAR 'TITLE100'.

*  IF gf_gui_parent_container IS INITIAL.
*    CREATE OBJECT gf_gui_parent_container
*           EXPORTING CONTAINER_NAME = 'CTRL1'.
*  endif.
*
*  call method gf_commander->show
*    EXPORTING
*      pf_container = gf_gui_parent_container.

  IF gf_gui_commander_container IS INITIAL.

    CREATE OBJECT gf_gui_commander_container
      EXPORTING
       container_name = 'CUST100'.

  ENDIF.

  gf_commander->show( gf_gui_commander_container ).

  DATA gs_dir TYPE REF TO zapcmd_cl_dir.
  gs_dir = gf_commander->cf_activelist->get_dir( ).
  g_dirname = gs_dir->full_name.

  IF sy-dynnr = '0100'.
    IF gf_gui_dirname_container IS INITIAL.
      CREATE OBJECT gf_gui_dirname_container
        EXPORTING
          container_name = 'CUST400'.
    ENDIF.

    gf_cmdline->show1( gf_gui_dirname_container ).
  ENDIF.


ENDMODULE.                 " STATUS_0100  OUTPUT

*&---------------------------------------------------------------------*
*&      Module  USER_COMMAND_0100  INPUT
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
MODULE user_command_0100 INPUT.

*   to react on oi_custom_events:
  CALL METHOD cl_gui_cfw=>dispatch.
  CALL METHOD gf_commander->user_command
    EXPORTING
      e_ucomm = ok_code100.
  IF sy-dynnr = '0100'.
    CALL METHOD gf_cmdline->user_command
      EXPORTING
        e_ucomm = ok_code100.
  ENDIF.

  CASE ok_code100.
    WHEN 'EXIT'.
      LEAVE TO SCREEN 0.
    WHEN 'ABORT'.
      LEAVE PROGRAM.
    when 'CMDLINE'.
      if sy-dynnr = '0100'.
        leave to screen 110.
      elseif sy-dynnr = '0110'.
        leave to screen 100.
      endif.
  ENDCASE.
  CLEAR ok_code100.


ENDMODULE.                 " USER_COMMAND_0100  INPUT
