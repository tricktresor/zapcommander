class ZAPCMD_CL_SERVER_FACTORY definition
  public
  final
  create public .

*"* public components of class ZAPCMD_CL_SERVER_FACTORY
*"* do not include other source files here!!!
public section.

  interfaces ZAPCMD_IF_FACTORY .
protected section.
*"* protected components of class ZAPCMD_CL_FRONTEND_FACTORY
*"* do not include other source files here!!!
private section.
*"* private components of class ZAPCMD_CL_SERVER_FACTORY
*"* do not include other source files here!!!

  data CO_APPLSERV type SYUCOMM value 'APPLSERV'. "#EC NOTEXT .
  data CO_LOGICALFILE type SYUCOMM value 'LOGICALF'. "#EC NOTEXT .
ENDCLASS.



CLASS ZAPCMD_CL_SERVER_FACTORY IMPLEMENTATION.


METHOD zapcmd_if_factory~create_dir.

  CASE i_fcode.
    WHEN co_applserv.

      CREATE OBJECT eo_dir TYPE zapcmd_cl_server_dir.
      eo_dir->init( pf_full_name = i_dir ).


*    WHEN co_logicalfile.
*      IF i_dir IS INITIAL.
*        DATA values TYPE TABLE OF sval.
*        DATA value TYPE sval.
*        value-tabname = 'PATH'.
*        value-fieldname = 'PATHINTERN'.
*        value-fieldtext = 'Logischer Pfad'(012).
*        APPEND value TO values.
*
*
*        DATA l_subrc TYPE char1.
*        CALL FUNCTION 'POPUP_GET_VALUES'
*          EXPORTING
**    NO_VALUE_CHECK        = ' '
*            popup_title           = 'logischer Pfad'(013)
**    START_COLUMN          = '5'
**    START_ROW             = '5'
*        IMPORTING
*            returncode            = l_subrc
*          TABLES
*            fields                = values
*        EXCEPTIONS
*          error_in_fields       = 1
*          OTHERS                = 2
*                  .
*        IF sy-subrc <> 0 AND l_subrc <> ''.
*          CLEAR eo_dir.
*          RETURN.
*        ENDIF.
*
*        LOOP AT values INTO value.
*        ENDLOOP.
*
*        DATA l_path TYPE filepath-pathintern.
*        l_path = value-value.
*        IF l_path IS NOT INITIAL.
*
*          CREATE OBJECT eo_dir
*            TYPE
*              zapcmd_cl_server_dir.
*
*          DATA lf_dir TYPE string.
*
*          CALL FUNCTION 'FILE_GET_NAME_USING_PATH'
*            EXPORTING
**       CLIENT                           = SY-MANDT
*              logical_path                     = l_path
**       OPERATING_SYSTEM                 = SY-OPSYS
**       PARAMETER_1                      = ' '
**       PARAMETER_2                      = ' '
**       PARAMETER_3                      = ' '
**       USE_BUFFER                       = ' '
*              file_name                        = '°'
**       USE_PRESENTATION_SERVER          = ' '
**       ELEMINATE_BLANKS                 = 'X'
*           IMPORTING
*             file_name_with_path              = lf_dir
**     EXCEPTIONS
**       PATH_NOT_FOUND                   = 1
**       MISSING_PARAMETER                = 2
**       OPERATING_SYSTEM_NOT_FOUND       = 3
**       FILE_SYSTEM_NOT_FOUND            = 4
**       OTHERS                           = 5
*                    .
*          IF sy-subrc <> 0.
** MESSAGE ID SY-MSGID TYPE SY-MSGTY NUMBER SY-MSGNO
**         WITH SY-MSGV1 SY-MSGV2 SY-MSGV3 SY-MSGV4.
*          ENDIF.
*          DATA l_offset TYPE i.
*          FIND '°' IN lf_dir MATCH OFFSET l_offset.
*          IF sy-subrc = 0.
*            lf_dir = lf_dir(l_offset).
*          ENDIF.
*
*
*          CALL METHOD eo_dir->init
*            EXPORTING
*              pf_full_name = lf_dir.
*
*
*        ELSE.
*          CREATE OBJECT eo_dir TYPE zapcmd_cl_server_dir.
*          lf_dir = eo_dir->co_logicaldir.
*          CALL METHOD eo_dir->init
*            EXPORTING
*              pf_full_name = lf_dir.
*        ENDIF.
*      ELSE.
*        CREATE OBJECT eo_dir TYPE zapcmd_cl_server_dir.
*        CALL METHOD eo_dir->init
*          EXPORTING
*            pf_full_name = i_dir.
*      ENDIF.


  ENDCASE.
ENDMETHOD.


method ZAPCMD_IF_FACTORY~GET_BUTTON.

      data ls_toolbar type STB_BUTTON.

     CLEAR ls_toolbar.
    MOVE 0 TO ls_toolbar-butn_type.
    MOVE CO_APPLSERV TO ls_toolbar-function.
    MOVE ICON_SYM_REAL_SERVER TO ls_toolbar-icon.
    MOVE 'Appl. Server'(210) to ls_toolbar-text.
    MOVE 'Applikationsserver'(211) TO ls_toolbar-quickinfo.
    MOVE SPACE TO ls_toolbar-disabled.
    APPEND ls_toolbar TO et_button.


*    CLEAR ls_toolbar.
*    MOVE 0 TO ls_toolbar-butn_type.
*    MOVE co_logicalfile TO ls_toolbar-function.
*    MOVE ICON_PUBLIC_FILES TO ls_toolbar-icon.
*    MOVE 'log. Datei'(003) to ls_toolbar-text.
*    MOVE 'logische Pfade und Dateien'(004) TO ls_toolbar-quickinfo.
*    MOVE SPACE TO ls_toolbar-disabled.
*    APPEND ls_toolbar TO et_button.

endmethod.
ENDCLASS.
