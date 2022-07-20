class ZAPCMD_CL_DIR definition
  public
  inheriting from ZAPCMD_CL_KNOT
  abstract
  create public .

*"* public components of class ZAPCMD_CL_DIR
*"* do not include other source files here!!!
public section.

  constants CO_DEFAULT type SYUCOMM value 'DEFAULT' ##NO_TEXT.
  constants CO_FRONTEND type SYUCOMM value 'FRONTEND' ##NO_TEXT.
  constants CO_APPLSERV type SYUCOMM value 'APPLSERV' ##NO_TEXT.
  constants CO_SERVER type SYUCOMM value 'SERVER' ##NO_TEXT.
  constants CO_DRIVES type SYUCOMM value 'DRIVES' ##NO_TEXT.
  constants CO_RENAME type SYUCOMM value 'RENAME' ##NO_TEXT.
  constants CO_DELETE type SYUCOMM value 'DELETE' ##NO_TEXT.
  constants CO_REFRESH type SYUCOMM value 'REFRESH' ##NO_TEXT.
  constants CO_AL11 type SYUCOMM value 'AL11' ##NO_TEXT.
  constants CO_EDIT_DIR type SYUCOMM value 'EDDIR' ##NO_TEXT.
  constants CO_RFC type SYUCOMM value 'RFC' ##NO_TEXT.
  constants CO_LOGICALFILE type SYUCOMM value 'LOGICALF' ##NO_TEXT.
  data FILTER type STRING .
  data AREA_STRING type STRING .
  constants CO_DIRECTORY_UP type SYUCOMM value '..' ##NO_TEXT.

  class-methods CLASS_CONSTRUCTOR .
  methods CONSTRUCTOR .
  methods READ_DIR
  abstract
    importing
      !PF_MASK type ZAPCMD_FILENAME optional
    exporting
      value(PT_FILELIST) type ZAPCMD_TBL_FILELIST
    exceptions
      PERMISSION_DENIED .
  methods CREATE_FILE
  abstract
    importing
      value(PF_FILENAME) type STRING
    exporting
      !PF_FILE type ref to ZAPCMD_CL_FILE .
  methods CREATE_DIR
  abstract
    importing
      value(PF_FILENAME) type STRING
    exporting
      !PF_FILE type ref to ZAPCMD_CL_DIR .
  methods GET_FREESPACE
  abstract
    exporting
      !PF_SPACE type P .
  methods GET_TOOLBAR
  abstract
    exporting
      !PT_TOOLBAR type TTB_BUTTON .
  methods CHECK_FILEEXIST
    importing
      !PF_FILENAME type CSEQUENCE
    returning
      value(PFX_EXIST) type ABAP_BOOL .
  methods CREATE_NEW
  abstract
    importing
      !I_FCODE type SYUCOMM optional
    returning
      value(EO_DIR) type ref to ZAPCMD_CL_DIR .

  methods EXECUTE
    redefinition .
  methods INIT
    redefinition .
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
