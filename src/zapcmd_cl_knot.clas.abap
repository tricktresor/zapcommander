class ZAPCMD_CL_KNOT definition
  public
  abstract
  create public .

*"* public components of class ZAPCMD_CL_KNOT
*"* do not include other source files here!!!
public section.
  type-pools ICON .

  constants CO_AREA_APPLSERV type ZAPCMD_SERVER_AREA value 'A'. "#EC NOTEXT
  constants CO_AREA_FRONTEND type ZAPCMD_SERVER_AREA value 'P'. "#EC NOTEXT
  data NAME type STRING .
  data EXTENSION type STRING .
  data SHORTNAME type STRING .
  data FULL_NAME type STRING .
  data DIR type STRING .
  data FILESIZE type ZAPCMD_FILESIZE .
  data SIZESTR type STRING .
  data MODDATE type DATS .
  data MODTIME type TIMS .
  data FILETYPE type CHAR10 .
  data ATTR type CHAR10 .
  data IS_DIR type XFELD value SPACE .
  data SEPARATOR type CHAR10 value '/' .
  data SERVER_AREA type ZAPCMD_SERVER_AREA value 'A' .
  constants CO_ROOTDIR type C value '^'. "#EC NOTEXT
  constants:
    co_al11dir(6) type c value '<AL11>'.
  constants:
    CO_RFCDIR(5) type c value '<RFC>'.
  constants:
    CO_LOGICALDIR(9) type c value '<LOGICAL>'.

  class-methods CONVERT_SIZE
    importing
      !PF_SIZE type P
    exporting
      !PFX_SIZESTR type STRING .
  class-methods EXEC_SERVER
    importing
      !PF_COMMAND type CSEQUENCE default 'mkdir'
      !PF_DIR type CSEQUENCE optional
      !PF_PARAMETER type CSEQUENCE optional
    exporting
      !PTX_OUTPUT type ZAPCMD_TBL_STRING .
  methods GET_INFO
    returning
      value(PSX_FILEINFO) type ZAPCMD_FILE_DESCR .
  methods INIT
    importing
      !PF_NAME type CSEQUENCE optional
      !PF_FULL_NAME type CSEQUENCE optional
      !PF_SIZE type P optional
      !PF_MODDATE type DATS optional
      !PF_MODTIME type TIMS optional
      !PF_ATTR type CHAR10 optional
      !PF_DIR type CSEQUENCE optional .
  methods EXECUTE
  abstract .
  methods DELETE
  abstract .
  methods RENAME
    importing
      !PF_NEWNAME type ZAPCMD_FILENAME .
protected section.
*"* protected components of class ZAPCMD_KNOT
*"* do not include other source files here!!!
private section.
*"* private components of class ZAPCMD_CL_KNOT
*"* do not include other source files here!!!

  methods SPLIT_FULLNAME
    importing
      value(PF_FULLNAME) type CSEQUENCE
    exporting
      value(PFX_NAME) type CSEQUENCE
      value(PFX_SHORTNAME) type CSEQUENCE
      value(PFX_EXTENSION) type CSEQUENCE
      value(PFX_DIR) type CSEQUENCE .
  methods SPLIT_NAME
    importing
      value(PF_NAME) type CSEQUENCE
    exporting
      value(PFX_SHORTNAME) type CSEQUENCE
      value(PFX_EXTENSION) type CSEQUENCE .
ENDCLASS.



CLASS ZAPCMD_CL_KNOT IMPLEMENTATION.


method CONVERT_SIZE.

*data lf_temp(15) type c.
*write pf_size to lf_temp.
*pfx_sizestr = lf_temp.

    DATA lf_size TYPE p DECIMALS 2.
    DATA lf_kmg(4) TYPE c.

    lf_size = pf_size.
    IF lf_size > 0.
      lf_kmg = 'B'.
    ENDIF.
    IF lf_size > 1000.
      lf_size = lf_size / 1000.
      lf_kmg = 'kB'.
    ENDIF.
    IF lf_size > 1000.
      lf_size = lf_size / 1000.
      lf_kmg = 'MB'.
    ENDIF.
    IF lf_size > 1000.
      lf_size = lf_size / 1000.
      lf_kmg = 'GB'.
    ENDIF.
    DATA lf_temp(7) TYPE c.
    WRITE lf_size TO lf_temp.
    CONCATENATE lf_temp lf_kmg INTO pfx_sizestr SEPARATED BY space.

endmethod.


method EXEC_SERVER.

  DATA: lf_command(255) TYPE c.
  DATA: l_lines TYPE TABLE OF char255.
  FIELD-SYMBOLS: <l_line> TYPE char255.
  CONCATENATE pf_command pf_parameter
    INTO lf_command SEPARATED BY space.
  DATA: lf_dir(255) TYPE c.
  lf_dir = pf_dir.

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
    APPEND 'Datei nicht gefunden.'(130) TO ptx_output.
    RETURN.
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



   lf_conv1 = cl_abap_conv_out_ce=>create(
           encoding = 'NON-UNICODE'
           endian = 'L'
         ).
  lf_conv2 = cl_abap_conv_in_ce=>create(
           encoding = '1103'
           endian = 'L'
         ).

  LOOP AT l_lines ASSIGNING <l_line>.

    lf_conv1->convert(
        EXPORTING data = <l_line>
        IMPORTING buffer = lf_buffer ).

    lf_conv2->convert(
       EXPORTING input = lf_buffer
       IMPORTING data = <l_line> ).


  ENDLOOP.
  ptx_output[] = l_lines[].
endmethod.


METHOD get_info.

* ...
  psx_fileinfo-filetype = filetype.
  IF filetype = 'DIR'.
    CONCATENATE '[' shortname ']' INTO psx_fileinfo-shortname.
    IF shortname = '..'.
      psx_fileinfo-icon = icon_collapse_all.
      psx_fileinfo-filetype = 'UP'.
    ELSE.
      psx_fileinfo-icon = icon_folder.
    ENDIF.
  ELSE.
    psx_fileinfo-shortname = shortname.
    DATA l_extension LIKE extension.
    IF STRLEN( extension ) >= 3.
      l_extension = extension.
      TRANSLATE l_extension TO UPPER CASE.
      CASE l_extension(3).
        WHEN 'EXE' OR 'BAT' OR 'CMD' OR 'JAR' OR 'COM'. psx_fileinfo-icon = icon_execute_object.
        WHEN 'TXT'. psx_fileinfo-icon = icon_change_text.
        WHEN 'XML' OR 'XSD'. psx_fileinfo-icon = icon_xml_doc.
        WHEN 'OTF'. psx_fileinfo-icon = icon_otf_document.
        WHEN 'ZIP' OR 'RAR' OR '7Z' OR 'CAB'. psx_fileinfo-icon = icon_package_standard.
        WHEN 'WAV' OR 'MP3' or 'OGG' or 'VOX'. psx_fileinfo-icon = icon_voice_output.
        WHEN 'AVI' OR 'DIV' OR 'MPG' OR 'MPE' or 'MP4'.
          psx_fileinfo-icon = icon_video.
        WHEN 'BMP'. psx_fileinfo-icon = icon_bmp.
        WHEN 'DOC'. psx_fileinfo-icon = icon_doc.
        WHEN 'DOT'. psx_fileinfo-icon = icon_dot.
        WHEN 'EFF'. psx_fileinfo-icon = icon_eff.
        WHEN 'EML'. psx_fileinfo-icon = icon_eml.
        WHEN 'FAX'. psx_fileinfo-icon = icon_fax.
        WHEN 'GIF' or 'PNG'. psx_fileinfo-icon = icon_gif.
        WHEN 'HLP'. psx_fileinfo-icon = icon_hlp.
        WHEN 'HTM'. psx_fileinfo-icon = icon_htm.
        WHEN 'HTT'. psx_fileinfo-icon = icon_htt.
        WHEN 'ITS'. psx_fileinfo-icon = icon_its.
        WHEN 'JPG' OR 'JPE' or 'JP2'. psx_fileinfo-icon = icon_jpg.
        WHEN 'LWP'. psx_fileinfo-icon = icon_lwp.
        WHEN 'MSG'. psx_fileinfo-icon = icon_msg.
        WHEN 'PDF'. psx_fileinfo-icon = icon_pdf.
        WHEN 'PPT'. psx_fileinfo-icon = icon_ppt.
        WHEN 'RTF'. psx_fileinfo-icon = icon_rtf.
        WHEN 'SAP' or 'SAR' or 'SCA'. psx_fileinfo-icon = icon_sap.
        WHEN 'TBH'. psx_fileinfo-icon = icon_tbh.
        WHEN 'TIF'. psx_fileinfo-icon = icon_tif.
        WHEN 'VSD'. psx_fileinfo-icon = icon_vsd.
        WHEN 'WRI'. psx_fileinfo-icon = icon_wri.
        WHEN 'XLS' OR 'CSV'. psx_fileinfo-icon = icon_xls.
        WHEN 'XLV'. psx_fileinfo-icon = icon_xlv.
        WHEN 'URL'. psx_fileinfo-icon = icon_url.
        WHEN 'LNK'. psx_fileinfo-icon = icon_linked_document.
        WHEN 'OLD' OR 'BAK'. psx_fileinfo-icon = icon_hold.
        WHEN 'LOG'. psx_fileinfo-icon = icon_protocol.
        WHEN 'DAT' OR 'BIN'. psx_fileinfo-icon = icon_binary_document.
        WHEN 'DLL' OR 'LIB'. psx_fileinfo-icon = icon_wd_tag_library.
        WHEN 'ABA'. psx_fileinfo-icon = icon_test.
        WHEN 'SYS'. psx_fileinfo-icon = icon_systems.
        WHEN 'INI'. psx_fileinfo-icon = icon_configuration.
        WHEN 'INF'. psx_fileinfo-icon = icon_information.
        WHEN 'PAT' or 'CAR'. psx_fileinfo-icon = ICON_PACKAGE_APPLICATION.
        when 'ATT'. psx_fileinfo-icon = ICON_ATTACHMENT.
        WHEN OTHERS.
          if l_extension = sy-sysid.
            psx_fileinfo-icon = ICON_IMPORT_TRANSPORT_REQUEST.
          else.
            psx_fileinfo-icon = icon_create.
          endif.
      ENDCASE.
      IF STRLEN( extension ) = 3 AND extension+2 = '_'.
        psx_fileinfo-icon = icon_collapse.
      ENDIF.
      IF l_extension = 'CONV'.
        psx_fileinfo-icon = icon_convert_all.
      ENDIF.
    ELSE.
      IF extension IS INITIAL.
        CLEAR psx_fileinfo-icon.
      ELSE.
        psx_fileinfo-icon = icon_create.
      ENDIF.
    ENDIF.
  ENDIF.
  psx_fileinfo-name = name.

  psx_fileinfo-extension = extension.
  psx_fileinfo-full_name = full_name.
  psx_fileinfo-filesize = filesize.
  psx_fileinfo-sizestr = sizestr.
  psx_fileinfo-moddate = moddate.
  psx_fileinfo-modtime = modtime.
  psx_fileinfo-attr = attr.

ENDMETHOD.


method INIT.

* ...
    name = pf_name.
    full_name = pf_full_name.
    dir = pf_dir.
    filesize = pf_size.
    modtime = pf_modtime.
    moddate = pf_moddate.
    attr = pf_attr.

    CALL METHOD convert_size
      EXPORTING
        pf_size     = pf_size
      IMPORTING
        pfx_sizestr = sizestr.

    IF pf_full_name IS INITIAL AND NOT pf_dir IS INITIAL.
      DATA lf_slen TYPE i.
      lf_slen = STRLEN( pf_dir ) - STRLEN( separator ).
      IF pf_dir+lf_slen = separator.
        CONCATENATE pf_dir pf_name INTO full_name.
      ELSE.
        CONCATENATE pf_dir separator pf_name INTO full_name.
      ENDIF.
    ENDIF.
    if name is initial.
      call method split_fullname
        exporting
          pf_fullname = full_name
        importing
          pfx_name = name
          pfx_shortname = shortname
          pfx_extension = extension
          pfx_dir = dir.

    else.
        call method split_name
        exporting
          pf_name = name
        importing
          pfx_shortname = shortname
          pfx_extension = extension.
    endif.


endmethod.


method RENAME.



  name = pf_newname.
  concatenate dir separator name into full_name.
  me->split_name(
    EXPORTING
      pf_name       = name
    IMPORTING
      PFX_SHORTNAME = SHORTNAME
      PFX_EXTENSION = EXTENSION
         ).




endmethod.


method SPLIT_FULLNAME.

*  data separator type c value '\'.
  data lf_dirs type string.
  data lt_dirs like table of lf_dirs.
  split pf_fullname at separator into table lt_dirs.
  loop at lt_dirs into lf_dirs.
    if sy-tabix < lines( lt_dirs ).
      if pfx_dir is initial.
        pfx_dir = lf_dirs.
      else.
        concatenate pfx_dir separator lf_dirs into pfx_dir.
      endif.
    else.
      pfx_name = lf_dirs.
    endif.
  endloop.
  data l_count type i.
  l_count = strlen( pfx_dir ).
  l_count = l_count - 1.
  if l_count > 0 and pfx_dir+l_count = ':'.
    concatenate pfx_dir separator into pfx_dir.
  endif.

  me->split_name(
    EXPORTING
      pf_name       = pfx_name
    IMPORTING
      PFX_SHORTNAME = PFX_SHORTNAME
      PFX_EXTENSION = PFX_EXTENSION
         ).


endmethod.


method SPLIT_NAME.
  if pf_name is initial or pf_name = '..' or pf_name = '.'.
    pfx_shortname = pf_name.
    pfx_extension = space.
    return.
  endif.
  data lf_names type string.
  data lt_names like table of lf_names.
  split pf_name at '.' into table lt_names.
  loop at lt_names into lf_names.
    if sy-tabix < lines( lt_names ).
      if pfx_shortname is initial.
        pfx_shortname = lf_names.
      else.
        concatenate pfx_shortname '.' lf_names into pfx_shortname.
      endif.
    else.
      pfx_extension = lf_names.
    endif.
  endloop.
  if pfx_shortname is initial.
    pfx_shortname = pfx_extension.
    clear pfx_extension.
  endif.
  if pf_name(1) = '.'.
    concatenate '.' pfx_shortname into pfx_shortname.
  endif.
  if pfx_extension co '1234567890'.
    pfx_shortname = pf_name.
    clear pfx_extension.
  endif.
  if pfx_extension ca '-)]'.
    pfx_shortname = pf_name.
    clear pfx_extension.
  endif.

endmethod.
ENDCLASS.
