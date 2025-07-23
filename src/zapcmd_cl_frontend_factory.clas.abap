CLASS zapcmd_cl_frontend_factory DEFINITION
  PUBLIC
  FINAL
  CREATE PUBLIC .

*"* public components of class ZAPCMD_CL_FRONTEND_FACTORY
*"* do not include other source files here!!!
  PUBLIC SECTION.

    INTERFACES zapcmd_if_factory .

  CONSTANTS: BEGIN OF gc_fcode,
               frontend TYPE syucomm VALUE 'FRONTEND',
             END OF gc_fcode.

  PROTECTED SECTION.
*"* protected components of class ZAPCMD_CL_FRONTEND_FACTORY
*"* do not include other source files here!!!
  PRIVATE SECTION.
*"* private components of class ZAPCMD_CL_FRONTEND_FACTORY
*"* do not include other source files here!!!
ENDCLASS.



CLASS ZAPCMD_CL_FRONTEND_FACTORY IMPLEMENTATION.


  METHOD zapcmd_if_factory~create_dir.

    IF i_fcode = gc_fcode-frontend.

      CREATE OBJECT eo_dir TYPE zapcmd_cl_frontend_dir.
      eo_dir->init( pf_full_name = i_dir ).

    ENDIF.
  ENDMETHOD.


  METHOD zapcmd_if_factory~get_button.

    DATA ls_toolbar TYPE stb_button.

    CLEAR ls_toolbar.
    MOVE 0 TO ls_toolbar-butn_type.
    MOVE gc_fcode-frontend TO ls_toolbar-function.
    MOVE icon_workplace TO ls_toolbar-icon.
    MOVE 'Frontend'(220) TO ls_toolbar-text.
    MOVE 'Präsentationsserver'(221) TO ls_toolbar-quickinfo.
    MOVE space TO ls_toolbar-disabled.
    APPEND ls_toolbar TO et_button.

  ENDMETHOD.
ENDCLASS.
