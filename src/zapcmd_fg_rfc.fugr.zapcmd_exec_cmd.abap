FUNCTION ZAPCMD_EXEC_CMD.
*"----------------------------------------------------------------------
*"*"Lokale Schnittstelle:
*"  IMPORTING
*"     VALUE(IV_COMMAND) TYPE  TEXT255 DEFAULT 'mkdir'
*"     VALUE(IV_DIR) TYPE  TEXT255 OPTIONAL
*"     VALUE(IV_PARAMETER) TYPE  TEXT255 OPTIONAL
*"  TABLES
*"      ET_OUTPUT STRUCTURE  ZAPCMD_T_TEXT255 OPTIONAL
*"  EXCEPTIONS
*"      NOT_FOUND
*"      COMMAND_TOO_LONG
*"----------------------------------------------------------------------


 DATA: lf_command(255) TYPE c.
  DATA: l_lines TYPE TABLE OF char255.
  FIELD-SYMBOLS: <l_line> TYPE char255.
  DATA lf_command_string TYPE string.
  CONCATENATE iv_command iv_parameter
    INTO lf_command_string SEPARATED BY space.
  " never run a truncated command
  IF strlen( lf_command_string ) > 255.
    RAISE command_too_long.
  ENDIF.
  lf_command = lf_command_string.
  DATA: lf_dir(255) TYPE c.
  lf_dir = iv_dir.

  IF lf_dir IS NOT INITIAL.
    CALL 'SYSTEM' ID 'COMMAND' FIELD lf_command
      ID 'PATH' FIELD lf_dir
      ID 'TAB' FIELD l_lines.
  ELSE.
    CALL 'SYSTEM' ID 'COMMAND' FIELD lf_command
      ID 'TAB' FIELD l_lines.
  ENDIF.

* Check any files exits in the directory.......................
  IF sy-subrc <> 0.
    raise not_found.
  ENDIF.

  DATA lf_conv1 TYPE REF TO cl_abap_conv_out_ce.
  DATA lf_conv2 TYPE REF TO cl_abap_conv_in_ce.
  DATA lf_buffer TYPE xstring.
*  data lf_codepage type CPCODEPAGE.
*
*  CALL FUNCTION 'SCP_CODEPAGE_BY_EXTERNAL_NAME'
*    EXPORTING
*      external_name       = 'CP437'
**     KIND                = 'H'
*   IMPORTING
*     SAP_CODEPAGE        = lf_codepage
**   EXCEPTIONS
**     NOT_FOUND           = 1
**     OTHERS              = 2
*            .
*  IF sy-subrc <> 0.
** MESSAGE ID SY-MSGID TYPE SY-MSGTY NUMBER SY-MSGNO
**         WITH SY-MSGV1 SY-MSGV2 SY-MSGV3 SY-MSGV4.
*  ENDIF.



  " only the Windows console writes in the DOS codepage 850 (SAP 1103)
  IF NOT ( sy-opsys CS 'Windows' OR sy-opsys = 'DOS' ).     "#EC NOTEXT
    et_output[] = l_lines[].
    RETURN.
  ENDIF.

   lf_conv1 = cl_abap_conv_out_ce=>create(
           encoding    = 'NON-UNICODE'
           endian      = 'L'
           ignore_cerr = abap_true
         ).
  lf_conv2 = cl_abap_conv_in_ce=>create(
           encoding    = '1103'
           endian      = 'L'
           ignore_cerr = abap_true
         ).

  LOOP AT l_lines ASSIGNING <l_line>.

    lf_conv1->convert(
        EXPORTING data = <l_line>
        IMPORTING buffer = lf_buffer ).

    lf_conv2->convert(
       EXPORTING input = lf_buffer
       IMPORTING data = <l_line> ).


  ENDLOOP.
  et_output[] = l_lines[].


ENDFUNCTION.
