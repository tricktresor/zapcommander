class ZAPCMD_CL_RFC_FACTORY definition
  public
  final
  create public .

*"* public components of class ZAPCMD_CL_RFC_FACTORY
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



CLASS ZAPCMD_CL_RFC_FACTORY IMPLEMENTATION.


method ZAPCMD_IF_FACTORY~CREATE_DIR.

  if i_fcode = 'RFC'.


    DATA values TYPE TABLE OF sval.
  DATA value TYPE sval.
  value-tabname = 'ZAPCMD_RFCDEST'.
  value-fieldname = 'RFCDEST'.
  value-fieldtext = 'RFC-Destination:'(004).
  APPEND value TO values.

  data l_subrc type char1.


  CALL FUNCTION 'POPUP_GET_VALUES'
    EXPORTING
*    NO_VALUE_CHECK        = ' '
      popup_title           = 'RFC-Dest'(003)
*    START_COLUMN          = '5'
*    START_ROW             = '5'
    IMPORTING
      RETURNCODE            = l_subrc
    TABLES
      fields                = values
*  EXCEPTIONS
*    ERROR_IN_FIELDS       = 1
*    OTHERS                = 2
            .
  IF sy-subrc <> 0 and l_subrc <> ''.
    clear eo_dir.
    return.
  ENDIF.

  LOOP AT values INTO value.
  ENDLOOP.

  DATA l_rfcdest TYPE rfcdest.
  l_rfcdest = value-value.
  IF l_rfcdest IS NOT INITIAL.

    CREATE OBJECT eo_dir
      TYPE
        zapcmd_cl_rfc_dir
      EXPORTING
        iv_rfcdest        = l_rfcdest
      EXCEPTIONS
        not_installed     = 1.

    IF sy-subrc = 0.
      CALL METHOD eo_dir->init( pf_full_name = i_dir ).
    else.
      MESSAGE 'RFC-Destination nicht erreichbar'(005) type 'S' DISPLAY LIKE 'E'.
      clear eo_dir.
      return.
    ENDIF.
  ELSE.
    CREATE OBJECT eo_dir TYPE zapcmd_cl_server_dir.
    DATA lf_rootdir TYPE string.
    lf_rootdir = eo_dir->co_rfcdir.
    CALL METHOD eo_dir->init
      EXPORTING
        pf_full_name = lf_rootdir.
  ENDIF.

  endif.
endmethod.


method ZAPCMD_IF_FACTORY~GET_BUTTON.

      data ls_toolbar type STB_BUTTON.

    CLEAR ls_toolbar.
    MOVE 0 TO ls_toolbar-butn_type.
    MOVE 'RFC' TO ls_toolbar-function.
    MOVE ICON_CONNECT TO ls_toolbar-icon.
    MOVE 'RFC'(001) to ls_toolbar-text.
    MOVE 'Verzeichnisse über RFC'(002) TO ls_toolbar-quickinfo.
    MOVE SPACE TO ls_toolbar-disabled.
    APPEND ls_toolbar TO et_button.

endmethod.
ENDCLASS.
