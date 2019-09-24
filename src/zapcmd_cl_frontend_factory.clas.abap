class ZAPCMD_CL_FRONTEND_FACTORY definition
  public
  final
  create public .

*"* public components of class ZAPCMD_CL_FRONTEND_FACTORY
*"* do not include other source files here!!!
public section.

  interfaces ZAPCMD_IF_FACTORY .
protected section.
*"* protected components of class ZAPCMD_CL_FRONTEND_FACTORY
*"* do not include other source files here!!!
private section.
*"* private components of class ZAPCMD_CL_FRONTEND_FACTORY
*"* do not include other source files here!!!
ENDCLASS.



CLASS ZAPCMD_CL_FRONTEND_FACTORY IMPLEMENTATION.


method ZAPCMD_IF_FACTORY~CREATE_DIR.

  if i_fcode = 'FRONTEND'.

    create object eo_dir type ZAPCMD_CL_FRONTEND_DIR.
    eo_dir->init( pf_full_name = i_dir ).

  endif.
endmethod.


method ZAPCMD_IF_FACTORY~GET_BUTTON.

      data ls_toolbar type STB_BUTTON.

    CLEAR ls_toolbar.
    MOVE 0 TO ls_toolbar-butn_type.
    MOVE 'FRONTEND' TO ls_toolbar-function.
    MOVE ICON_WORKPLACE TO ls_toolbar-icon.
    MOVE 'Frontend'(220) to ls_toolbar-text.
    MOVE 'Präsentationsserver'(221) TO ls_toolbar-quickinfo.
    MOVE SPACE TO ls_toolbar-disabled.
    APPEND ls_toolbar TO et_button.

endmethod.
ENDCLASS.
