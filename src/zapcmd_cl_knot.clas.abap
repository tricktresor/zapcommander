CLASS zapcmd_cl_knot DEFINITION
  PUBLIC
  ABSTRACT
  CREATE PUBLIC .

*"* public components of class ZAPCMD_CL_KNOT
*"* do not include other source files here!!!
  PUBLIC SECTION.
    TYPE-POOLS icon .

    CONSTANTS co_area_applserv TYPE zapcmd_server_area VALUE 'A'. "#EC NOTEXT
    CONSTANTS co_area_frontend TYPE zapcmd_server_area VALUE 'P'. "#EC NOTEXT
    DATA name TYPE string .
    DATA extension TYPE string .
    DATA shortname TYPE string .
    DATA full_name TYPE string .
    DATA dir TYPE string .
    DATA filesize TYPE zapcmd_filesize .
    DATA sizestr TYPE string .
    DATA moddate TYPE dats .
    DATA modtime TYPE tims .
    DATA filetype TYPE char10 .
    DATA attr TYPE char10 .
    DATA is_dir TYPE xfeld VALUE space .
    DATA separator TYPE char10 VALUE '/' .
    DATA server_area TYPE zapcmd_server_area VALUE 'A' .
    CONSTANTS co_rootdir TYPE c VALUE '^'.                  "#EC NOTEXT
    CONSTANTS:
      co_al11dir(6) TYPE c VALUE '<AL11>'.
    CONSTANTS:
      co_rfcdir(5) TYPE c VALUE '<RFC>'.
    CONSTANTS:
      co_logicaldir(9) TYPE c VALUE '<LOGICAL>'.

    CLASS-METHODS convert_size
      IMPORTING
        !pf_size     TYPE p
      EXPORTING
        !pfx_sizestr TYPE string .
    CLASS-METHODS exec_server
      IMPORTING
        !pf_command   TYPE csequence DEFAULT 'mkdir'
        !pf_dir       TYPE csequence OPTIONAL
        !pf_parameter TYPE csequence OPTIONAL
      EXPORTING
        !ptx_output   TYPE zapcmd_tbl_string .
    METHODS get_info
      RETURNING
        VALUE(psx_fileinfo) TYPE zapcmd_file_descr .
    METHODS init
      IMPORTING
        !pf_name      TYPE csequence OPTIONAL
        !pf_full_name TYPE csequence OPTIONAL
        !pf_size      TYPE p OPTIONAL
        !pf_moddate   TYPE dats OPTIONAL
        !pf_modtime   TYPE tims OPTIONAL
        !pf_attr      TYPE char10 OPTIONAL
        !pf_dir       TYPE csequence OPTIONAL .
    METHODS execute
        ABSTRACT .
    METHODS delete
        ABSTRACT .
    METHODS rename
      IMPORTING
        !pf_newname TYPE zapcmd_filename .
  PROTECTED SECTION.
*"* protected components of class ZAPCMD_KNOT
*"* do not include other source files here!!!
  PRIVATE SECTION.
*"* private components of class ZAPCMD_CL_KNOT
*"* do not include other source files here!!!

    METHODS split_fullname
      IMPORTING
        VALUE(pf_fullname)   TYPE csequence
      EXPORTING
        VALUE(pfx_name)      TYPE csequence
        VALUE(pfx_shortname) TYPE csequence
        VALUE(pfx_extension) TYPE csequence
        VALUE(pfx_dir)       TYPE csequence .
    METHODS split_name
      IMPORTING
        VALUE(pf_name)       TYPE csequence
      EXPORTING
        VALUE(pfx_shortname) TYPE csequence
        VALUE(pfx_extension) TYPE csequence .
ENDCLASS.



CLASS zapcmd_cl_knot IMPLEMENTATION.


  METHOD convert_size.

*data lf_temp(15) type c.
*write pf_size to lf_temp.
*pfx_sizestr = lf_temp.

    DATA lf_size TYPE p DECIMALS 2.
    DATA lf_kmg(4) TYPE c.

    lf_size = pf_size.
    IF lf_size > 0.
      lf_kmg = 'B'.
    ENDIF.
    IF lf_size > 1024.
      lf_size = lf_size / 1024.
      lf_kmg = 'kB'.
    ENDIF.
    IF lf_size > 1024.
      lf_size = lf_size / 1024.
      lf_kmg = 'MB'.
    ENDIF.
    IF lf_size > 1024.
      lf_size = lf_size / 1024.
      lf_kmg = 'GB'.
    ENDIF.
    DATA lf_temp(7) TYPE c.
    WRITE lf_size TO lf_temp.
    CONCATENATE lf_temp lf_kmg INTO pfx_sizestr SEPARATED BY space.

  ENDMETHOD.


  METHOD exec_server.

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
  ENDMETHOD.


  METHOD get_info.

    CONSTANTS:
      BEGIN OF c_sort_prio,
        no_sorting      TYPE zapcmd_sort_prio VALUE '1',
        regular_sorting TYPE zapcmd_sort_prio VALUE '2',
      END OF c_sort_prio.


* ...
    psx_fileinfo-filetype = filetype.
    IF filetype = 'DIR'.
      CONCATENATE '[' shortname ']' INTO psx_fileinfo-shortname.
      IF shortname = '..'.
        psx_fileinfo-icon = icon_collapse_all.
        psx_fileinfo-filetype = 'UP'.
        psx_fileinfo-sort_prio = c_sort_prio-no_sorting. " Stays on the top, no sorting...
      ELSE.
        psx_fileinfo-icon = icon_folder.
        psx_fileinfo-sort_prio = c_sort_prio-regular_sorting.
      ENDIF.
    ELSE.
      psx_fileinfo-shortname = shortname.
      psx_fileinfo-sort_prio = c_sort_prio-regular_sorting.
      DATA l_extension LIKE extension.
      IF strlen( extension ) >= 3.
        l_extension = extension.
        TRANSLATE l_extension TO UPPER CASE.
        CASE l_extension(3).
          WHEN 'EXE' OR 'BAT' OR 'CMD' OR 'JAR' OR 'COM'. psx_fileinfo-icon = icon_execute_object.
          WHEN 'TXT'. psx_fileinfo-icon = icon_change_text.
          WHEN 'XML' OR 'XSD'. psx_fileinfo-icon = icon_xml_doc.
          WHEN 'OTF'. psx_fileinfo-icon = icon_otf_document.
          WHEN 'ZIP' OR 'RAR' OR '7Z' OR 'CAB'. psx_fileinfo-icon = icon_package_standard.
          WHEN 'WAV' OR 'MP3' OR 'OGG' OR 'VOX'. psx_fileinfo-icon = icon_voice_output.
          WHEN 'AVI' OR 'DIV' OR 'MPG' OR 'MPE' OR 'MP4'.
            psx_fileinfo-icon = icon_video.
          WHEN 'BMP'. psx_fileinfo-icon = icon_bmp.
          WHEN 'DOC'. psx_fileinfo-icon = icon_doc.
          WHEN 'DOT'. psx_fileinfo-icon = icon_dot.
          WHEN 'EFF'. psx_fileinfo-icon = icon_eff.
          WHEN 'EML'. psx_fileinfo-icon = icon_eml.
          WHEN 'FAX'. psx_fileinfo-icon = icon_fax.
          WHEN 'GIF' OR 'PNG'. psx_fileinfo-icon = icon_gif.
          WHEN 'HLP'. psx_fileinfo-icon = icon_hlp.
          WHEN 'HTM'. psx_fileinfo-icon = icon_htm.
          WHEN 'HTT'. psx_fileinfo-icon = icon_htt.
          WHEN 'ITS'. psx_fileinfo-icon = icon_its.
          WHEN 'JPG' OR 'JPE' OR 'JP2'. psx_fileinfo-icon = icon_jpg.
          WHEN 'LWP'. psx_fileinfo-icon = icon_lwp.
          WHEN 'MSG'. psx_fileinfo-icon = icon_msg.
          WHEN 'PDF'. psx_fileinfo-icon = icon_pdf.
          WHEN 'PPT'. psx_fileinfo-icon = icon_ppt.
          WHEN 'RTF'. psx_fileinfo-icon = icon_rtf.
          WHEN 'SAP' OR 'SAR' OR 'SCA'. psx_fileinfo-icon = icon_sap.
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
          WHEN 'PAT' OR 'CAR'. psx_fileinfo-icon = icon_package_application.
          WHEN 'ATT'. psx_fileinfo-icon = icon_attachment.
          WHEN OTHERS.
            IF l_extension = sy-sysid.
              psx_fileinfo-icon = icon_import_transport_request.
            ELSE.
              psx_fileinfo-icon = icon_create.
            ENDIF.
        ENDCASE.
        IF strlen( extension ) = 3 AND extension+2 = '_'.
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


  METHOD init.

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
      lf_slen = strlen( pf_dir ) - strlen( separator ).
      IF pf_dir+lf_slen = separator.
        CONCATENATE pf_dir pf_name INTO full_name.
      ELSE.
        CONCATENATE pf_dir separator pf_name INTO full_name.
      ENDIF.
    ENDIF.
    IF name IS INITIAL.
      CALL METHOD split_fullname
        EXPORTING
          pf_fullname   = full_name
        IMPORTING
          pfx_name      = name
          pfx_shortname = shortname
          pfx_extension = extension
          pfx_dir       = dir.

    ELSE.
      CALL METHOD split_name
        EXPORTING
          pf_name       = name
        IMPORTING
          pfx_shortname = shortname
          pfx_extension = extension.
    ENDIF.


  ENDMETHOD.


  METHOD rename.



    name = pf_newname.
    CONCATENATE dir separator name INTO full_name.
    me->split_name(
      EXPORTING
        pf_name       = name
      IMPORTING
        pfx_shortname = shortname
        pfx_extension = extension
           ).




  ENDMETHOD.


  METHOD split_fullname.

*  data separator type c value '\'.
    DATA lf_dirs TYPE string.
    DATA lt_dirs LIKE TABLE OF lf_dirs.
    SPLIT pf_fullname AT separator INTO TABLE lt_dirs.
    LOOP AT lt_dirs INTO lf_dirs.
      IF sy-tabix < lines( lt_dirs ).
        IF pfx_dir IS INITIAL.
          pfx_dir = lf_dirs.
        ELSE.
          CONCATENATE pfx_dir separator lf_dirs INTO pfx_dir.
        ENDIF.
      ELSE.
        pfx_name = lf_dirs.
      ENDIF.
    ENDLOOP.
    DATA l_count TYPE i.
    l_count = strlen( pfx_dir ).
    l_count = l_count - 1.
    IF l_count > 0 AND pfx_dir+l_count = ':'.
      CONCATENATE pfx_dir separator INTO pfx_dir.
    ENDIF.

    me->split_name(
      EXPORTING
        pf_name       = pfx_name
      IMPORTING
        pfx_shortname = pfx_shortname
        pfx_extension = pfx_extension
           ).


  ENDMETHOD.


  METHOD split_name.
    IF pf_name IS INITIAL OR pf_name = '..' OR pf_name = '.'.
      pfx_shortname = pf_name.
      pfx_extension = space.
      RETURN.
    ENDIF.
    DATA lf_names TYPE string.
    DATA lt_names LIKE TABLE OF lf_names.
    SPLIT pf_name AT '.' INTO TABLE lt_names.
    LOOP AT lt_names INTO lf_names.
      IF sy-tabix < lines( lt_names ).
        IF pfx_shortname IS INITIAL.
          pfx_shortname = lf_names.
        ELSE.
          CONCATENATE pfx_shortname '.' lf_names INTO pfx_shortname.
        ENDIF.
      ELSE.
        pfx_extension = lf_names.
      ENDIF.
    ENDLOOP.
    IF pfx_shortname IS INITIAL.
      pfx_shortname = pfx_extension.
      CLEAR pfx_extension.
    ENDIF.
    IF pf_name(1) = '.'.
      CONCATENATE '.' pfx_shortname INTO pfx_shortname.
    ENDIF.
    IF pfx_extension CO '1234567890'.
      pfx_shortname = pf_name.
      CLEAR pfx_extension.
    ENDIF.
    IF pfx_extension CA '-)]'.
      pfx_shortname = pf_name.
      CLEAR pfx_extension.
    ENDIF.

  ENDMETHOD.
ENDCLASS.
