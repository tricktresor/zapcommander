CLASS zapcmd_cl_dir DEFINITION
  PUBLIC
  INHERITING FROM zapcmd_cl_knot
  ABSTRACT
  CREATE PUBLIC .

*"* public components of class ZAPCMD_CL_DIR
*"* do not include other source files here!!!
  PUBLIC SECTION.

    CONSTANTS co_default TYPE syucomm VALUE 'DEFAULT' ##NO_TEXT.
    CONSTANTS co_frontend TYPE syucomm VALUE 'FRONTEND' ##NO_TEXT.
    CONSTANTS co_applserv TYPE syucomm VALUE 'APPLSERV' ##NO_TEXT.
    CONSTANTS co_server TYPE syucomm VALUE 'SERVER' ##NO_TEXT.
    CONSTANTS co_drives TYPE syucomm VALUE 'DRIVES' ##NO_TEXT.
    CONSTANTS co_rename TYPE syucomm VALUE 'RENAME' ##NO_TEXT.
    CONSTANTS co_delete TYPE syucomm VALUE 'DELETE' ##NO_TEXT.
    CONSTANTS co_refresh TYPE syucomm VALUE 'REFRESH' ##NO_TEXT.
    CONSTANTS co_al11 TYPE syucomm VALUE 'AL11' ##NO_TEXT.
    CONSTANTS co_edit_dir TYPE syucomm VALUE 'EDDIR' ##NO_TEXT.
    CONSTANTS co_rfc TYPE syucomm VALUE 'RFC' ##NO_TEXT.
    CONSTANTS co_logicalfile TYPE syucomm VALUE 'LOGICALF' ##NO_TEXT.
    DATA filter TYPE string .
    DATA area_string TYPE string .

    CLASS-METHODS class_constructor .
    METHODS constructor .
    METHODS read_dir
          ABSTRACT
      IMPORTING
        !pf_mask           TYPE zapcmd_filename OPTIONAL
      EXPORTING
        VALUE(pt_filelist) TYPE zapcmd_tbl_filelist
      EXCEPTIONS
        permission_denied .
    METHODS create_file
          ABSTRACT
      IMPORTING
        VALUE(pf_filename) TYPE string
      EXPORTING
        !pf_file           TYPE REF TO zapcmd_cl_file .
    METHODS create_dir
          ABSTRACT
      IMPORTING
        VALUE(pf_filename) TYPE string
      EXPORTING
        !pf_file           TYPE REF TO zapcmd_cl_dir .
    METHODS get_freespace
          ABSTRACT
      EXPORTING
        !pf_space TYPE p .
    METHODS get_toolbar
          ABSTRACT
      EXPORTING
        !pt_toolbar TYPE ttb_button .
    METHODS check_fileexist
      IMPORTING
        !pf_filename     TYPE csequence
      RETURNING
        VALUE(pfx_exist) TYPE abap_bool .
    METHODS create_new
          ABSTRACT
      IMPORTING
        !i_fcode      TYPE syucomm OPTIONAL
      RETURNING
        VALUE(eo_dir) TYPE REF TO zapcmd_cl_dir .

    METHODS execute
        REDEFINITION .
    METHODS init
        REDEFINITION .
  PROTECTED SECTION.
*"* protected components of class ZAPCMD_CL_DIR
*"* do not include other source files here!!!

    METHODS read_rfc
      EXPORTING
        !pt_filelist TYPE zapcmd_tbl_filelist .
  PRIVATE SECTION.
*"* private components of class ZAPCMD_CL_DIR
*"* do not include other source files here!!!
ENDCLASS.



CLASS ZAPCMD_CL_DIR IMPLEMENTATION.


  METHOD check_fileexist.

    DATA lt_filelist TYPE zapcmd_tbl_filelist.
    DATA lf_mask TYPE zapcmd_filename.
    lf_mask = pf_filename.
    read_dir(
      EXPORTING
        pf_mask           = lf_mask
      IMPORTING
        pt_filelist       = lt_filelist
      EXCEPTIONS
        permission_denied = 1
        OTHERS            = 2
           ).
    IF sy-subrc <> 0.
      CLEAR pfx_exist.
      RETURN.
    ENDIF.
    IF lt_filelist IS INITIAL.
      pfx_exist = abap_false.
    ELSE.
      pfx_exist = abap_true.
    ENDIF.


  ENDMETHOD.


  METHOD class_constructor.
  ENDMETHOD.


  METHOD constructor.
    super->constructor( ).
    filetype = 'DIR'.
  ENDMETHOD.


  METHOD execute.
*** No Execution per default avalable.
  ENDMETHOD.


  METHOD init.

    super->init(
        pf_name      = pf_name
        pf_full_name = pf_full_name
        pf_size      = pf_size
        pf_moddate   = pf_moddate
        pf_modtime   = pf_modtime
        pf_attr      = pf_attr
        pf_dir       = pf_dir ).

    is_dir = 'X'.

    DATA lf_temp TYPE i.
    DATA lf_temp2 TYPE string.
    CONCATENATE separator '..' INTO lf_temp2.
    lf_temp = strlen( full_name ).
    lf_temp = lf_temp - strlen( lf_temp2 ).
    IF lf_temp > 0.
      IF full_name+lf_temp = lf_temp2.
        full_name = full_name(lf_temp).
        SEARCH full_name FOR separator.
        IF sy-subrc = 0.
          lf_temp = sy-fdpos + strlen( separator ).
          DO.
            SEARCH full_name+lf_temp FOR separator.
            IF sy-subrc = 0.
              lf_temp = lf_temp + sy-fdpos + strlen( separator ).
            ELSE.
              EXIT.
            ENDIF.
          ENDDO.
          lf_temp = lf_temp - strlen( separator ).
          full_name = full_name(lf_temp).
          SEARCH full_name FOR separator.
          IF sy-subrc <> 0.
            CONCATENATE full_name separator INTO full_name.
          ENDIF.
        ELSE.
          full_name = separator.
        ENDIF.
      ENDIF.
    ENDIF.


*  concatenate '[' name ']' into name.
    filesize = 0.
    CLEAR sizestr.
    filter = '*.*'.

  ENDMETHOD.


  METHOD read_rfc.

    DATA lt_rfc TYPE TABLE OF rfcdest.
    DATA l_rfc TYPE rfcdest.

    SELECT rfcdest FROM rfcdes
      INTO TABLE lt_rfc
      WHERE rfctype = '3'.

    LOOP AT lt_rfc INTO l_rfc.

      DATA lf_name TYPE string.
      DATA lf_ref_file TYPE REF TO zapcmd_cl_knot.
      lf_name = l_rfc.

      CREATE OBJECT lf_ref_file TYPE zapcmd_cl_rfc_dir
        EXPORTING
          iv_rfcdest    = l_rfc
        EXCEPTIONS
          not_installed = 1.

      IF sy-subrc = 0.
        lf_ref_file->init(
            pf_full_name = separator
            pf_name      = lf_name
            pf_size      = 0
            pf_dir       = full_name ).

        APPEND lf_ref_file TO pt_filelist.
      ENDIF.

    ENDLOOP.

  ENDMETHOD.
ENDCLASS.
