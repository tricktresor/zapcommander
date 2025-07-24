*&---------------------------------------------------------------------*
*& Report  ZAPCMD_SAPCOMMANDER                                           *
*&---------------------------------------------------------------------*
*& https://github.com/tricktresor/zapcommander
*& https://code.google.com/archive/p/sapcommander/
*&---------------------------------------------------------------------*
REPORT zapcmd_sapcommander.


PARAMETERS p_ltype TYPE syucomm DEFAULT zapcmd_cl_dir=>co_default NO-DISPLAY.
PARAMETERS p_ldir TYPE string DEFAULT space NO-DISPLAY.
PARAMETERS p_rtype TYPE syucomm DEFAULT zapcmd_cl_dir=>co_default NO-DISPLAY.
PARAMETERS p_rdir TYPE string DEFAULT space NO-DISPLAY.


START-OF-SELECTION.
  DATA gf_id TYPE indx_srtfd.
  CONCATENATE 'ZAPCMD' sy-uname INTO gf_id.
  IF p_ltype = zapcmd_cl_dir=>co_default OR p_rtype = zapcmd_cl_dir=>co_default.
    DATA gf_left TYPE zapcmd_t_dir.
    DATA gf_right TYPE zapcmd_t_dir.
    IMPORT left = gf_left
    right = gf_right
    FROM DATABASE indx(zc)
    ID gf_id.
    IF sy-subrc = 0.
      IF p_ltype = zapcmd_cl_dir=>co_default.
        p_ltype = gf_left-type.
        p_ldir  = gf_left-dir.
      ENDIF.
      IF p_rtype = zapcmd_cl_dir=>co_default.
        p_rtype = gf_right-type.
        p_rdir  = gf_right-dir.
      ENDIF.
    ENDIF.
  ENDIF.

  DATA ok_code100 LIKE sy-ucomm.
  DATA go_gui_commander_container TYPE REF TO cl_gui_custom_container.
  DATA go_gui_dirname_container TYPE REF TO cl_gui_custom_container.
  DATA go_commander TYPE REF TO zapcmd_cl_commander.
  DATA go_cmdline TYPE REF TO zapcmd_cl_cmdline.
  DATA gf_cmdline TYPE string.
  DATA gf_dirname TYPE string.
  DATA go_temp TYPE REF TO string.
  DATA go_dirname TYPE REF TO string.

  GET REFERENCE OF gf_cmdline INTO go_temp.
  GET REFERENCE OF gf_dirname INTO go_dirname.

  CREATE OBJECT go_commander
    EXPORTING
      pf_left_type  = p_ltype
      pf_left_dir   = p_ldir
      pf_right_type = p_rtype
      pf_right_dir  = p_rdir
      pf_dirname    = go_dirname.

  DATA go_activelist TYPE REF TO zapcmd_refref_filelist.
  GET REFERENCE OF go_commander->cf_activelist INTO go_activelist.

  CREATE OBJECT go_cmdline
    EXPORTING
      pf_cmdline = go_temp
      pf_dir     = go_activelist.

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
MODULE status_0100 OUTPUT.
  PERFORM status_0100.
ENDMODULE.                 " STATUS_0100  OUTPUT

*&---------------------------------------------------------------------*
*&      Module  USER_COMMAND_0100  INPUT
*&---------------------------------------------------------------------*
MODULE user_command_0100 INPUT.
  PERFORM user_command_0100.
ENDMODULE.                 " USER_COMMAND_0100  INPUT

FORM status_0100.

  DATA li_user_exit TYPE REF TO zapcmd_if_user_exit.

  SET PF-STATUS 'STATUS100'.
  SET TITLEBAR 'TITLE100'.

  li_user_exit = zapcmd_cl_user_exit_factory=>get( ).
  IF li_user_exit IS BOUND.
    li_user_exit->commander_set_pfstatus( ).
  ENDIF.

*  IF gf_gui_parent_container IS INITIAL.
*    CREATE OBJECT gf_gui_parent_container
*           EXPORTING CONTAINER_NAME = 'CTRL1'.
*  endif.
*
*  call method gf_commander->show
*    EXPORTING
*      pf_container = gf_gui_parent_container.

  IF go_gui_commander_container IS INITIAL.
    CREATE OBJECT go_gui_commander_container
      EXPORTING
        container_name = 'CUST100'.
  ENDIF.

  go_commander->show( go_gui_commander_container ).

  DATA gs_dir TYPE REF TO zapcmd_cl_dir.
  gs_dir = go_commander->cf_activelist->get_dir( ).
  gf_dirname = gs_dir->full_name.

  IF sy-dynnr = '0100'.
    IF go_gui_dirname_container IS INITIAL.
      CREATE OBJECT go_gui_dirname_container
        EXPORTING
          container_name = 'CUST400'.
    ENDIF.

    go_cmdline->show1( go_gui_dirname_container ).
  ENDIF.

ENDFORM.

FORM user_command_0100.
*   to react on oi_custom_events:
  cl_gui_cfw=>dispatch( ).
  go_commander->user_command( ok_code100 ).

  IF sy-dynnr = '0100'.
    go_cmdline->user_command( ok_code100 ).
  ENDIF.

  CASE ok_code100.
    WHEN 'EXIT'.
      LEAVE TO SCREEN 0.
    WHEN 'ABORT'.
      LEAVE PROGRAM.
    WHEN 'CMDLINE'.
      IF sy-dynnr = '0100'.
        LEAVE TO SCREEN 110.
      ELSEIF sy-dynnr = '0110'.
        LEAVE TO SCREEN 100.
      ENDIF.
  ENDCASE.
  CLEAR ok_code100.
ENDFORM.
