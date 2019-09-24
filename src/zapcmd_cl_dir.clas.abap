class ZAPCMD_CL_DIR definition
  public
  inheriting from ZAPCMD_CL_KNOT
  abstract
  create public .

*"* public components of class ZAPCMD_CL_DIR
*"* do not include other source files here!!!
public section.

  constants CO_DEFAULT type SYUCOMM value 'DEFAULT'. "#EC NOTEXT
  constants CO_FRONTEND type SYUCOMM value 'FRONTEND'. "#EC NOTEXT
  constants CO_APPLSERV type SYUCOMM value 'APPLSERV'. "#EC NOTEXT
  constants CO_SERVER type SYUCOMM value 'SERVER'. "#EC NOTEXT
  constants CO_DRIVES type SYUCOMM value 'DRIVES'. "#EC NOTEXT
  constants CO_RENAME type SYUCOMM value 'RENAME'. "#EC NOTEXT
  constants CO_DELETE type SYUCOMM value 'DELETE'. "#EC NOTEXT
  constants CO_REFRESH type SYUCOMM value 'REFRESH'. "#EC NOTEXT
  constants CO_AL11 type SYUCOMM value 'AL11'. "#EC NOTEXT
  constants CO_EDIT_DIR type SYUCOMM value 'EDDIR'. "#EC NOTEXT
  constants CO_RFC type SYUCOMM value 'RFC'. "#EC NOTEXT
  constants CO_LOGICALFILE type SYUCOMM value 'LOGICALF'. "#EC NOTEXT
  data FILTER type STRING .
  data AREA_STRING type STRING .

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
  type-pools ABAP .
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
protected section.
*"* protected components of class ZAPCMD_CL_DIR
*"* do not include other source files here!!!

  methods READ_RFC
    exporting
      !PT_FILELIST type ZAPCMD_TBL_FILELIST .
private section.
*"* private components of class ZAPCMD_CL_DIR
*"* do not include other source files here!!!
ENDCLASS.



CLASS ZAPCMD_CL_DIR IMPLEMENTATION.


method CHECK_FILEEXIST.

data LT_filelist type ZAPCMD_TBL_FILELIST.
data lf_mask type zapcmd_filename.
lf_mask = pf_filename.
read_dir(
  EXPORTING
    PF_MASK           = lf_mask
  IMPORTING
    PT_FILELIST       = LT_FILELIST
  EXCEPTIONS
    PERMISSION_DENIED = 1
    others            = 2
       ).
IF sy-subrc <> 0.
  clear pfx_exist.
  return.
ENDIF.
if lt_filelist is initial.
  pfx_exist = abap_false.
else.
  pfx_exist = abap_true.
endif.


endmethod.


method CONSTRUCTOR.

  CALL METHOD super->constructor.

  filetype = 'DIR'.

endmethod.


method EXECUTE.
*** No Execution per default avalable.
endmethod.


method INIT.
   call method super->init
      EXPORTING
        pf_name      = pf_name
        pf_full_name = pf_full_name
        pf_size      = pf_size
        pf_moddate   = pf_moddate
        pf_modtime   = pf_modtime
        pf_attr      = pf_attr
        pf_dir       = pf_dir.

  IS_DIR = 'X'.

  data lf_temp type i.
  data lf_temp2 type string.
  concatenate separator '..' into lf_temp2.
  lf_temp = strlen( full_name ).
  lf_temp = lf_temp - strlen( lf_temp2 ).
  if lf_temp > 0.
    if full_name+lf_temp = lf_temp2.
      full_name = full_name(lf_temp).
      search full_name for separator.
      if sy-subrc = 0.
        lf_temp = SY-FDPOS + strlen( separator ).
        do.
          search full_name+lf_temp for separator.
          if sy-subrc = 0.
            lf_temp = lf_temp + SY-FDPOS + strlen( separator ).
          else.
            exit.
          endif.
        enddo.
        lf_temp = lf_temp - strlen( separator ).
        full_name = full_name(lf_temp).
        search full_name for separator.
        if sy-subrc <> 0.
          concatenate full_name separator into full_name.
        endif.
      else.
        full_name = separator.
      endif.
    endif.
  endif.


*  concatenate '[' name ']' into name.
  filesize = 0.
  clear sizestr.
  filter = '*.*'.
endmethod.


method READ_RFC.

data lt_rfc type table of rfcdest.
data l_rfc type rfcdest.

select rfcdest from RFCDES
  into table lt_rfc
  where rfctype = '3'.

  loop at lt_rfc into l_rfc.

      data lf_name type string.
      data lf_ref_file type ref to ZAPCMD_CL_KNOT.
      lf_name = l_rfc.

        create object lf_ref_file type ZAPCMD_CL_rfc_DIR
          EXPORTING
            iv_rfcdest = l_rfc
          EXCEPTIONS
            not_installed = 1.

      if sy-subrc = 0.

      call method lf_ref_file->init
        EXPORTING
          pf_full_name = SEPARATOR
          pf_name      = lf_name
          pf_size      = 0
          pf_dir       = full_name.

      append lf_ref_file to pt_filelist.

      endif.

  endloop.


endmethod.
ENDCLASS.
