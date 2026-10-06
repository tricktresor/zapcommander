class ZAPCMD_CL_FILE definition
  public
  inheriting from ZAPCMD_CL_KNOT
  abstract
  create public .

*"* public components of class ZAPCMD_CL_FILE
*"* do not include other source files here!!!
public section.

  methods READ_TEXT
  abstract
    exporting
      !PF_FILESIZE type I
      !PT_FILE type ZAPCMD_TBL_STRING .
  methods READ_BIN
  abstract
    exporting
      !PF_FILESIZE type I
      !PT_FILE type ZAPCMD_TBL_XSTRING
    exceptions
      FAILED .
  methods WRITE_TEXT
  abstract
    importing
      !PT_FILE type ZAPCMD_TBL_STRING .
  methods WRITE_BIN
  abstract
    importing
      !PT_FILE type ZAPCMD_TBL_XSTRING
      !PF_FILESIZE type I
    exceptions
      FAILED .
  class-methods TEXT2STREAM
    importing
      !PT_TEXT type ZAPCMD_TBL_STRING
    returning
      value(PFX_STREAM) type STRING .
  class-methods STREAM2TEXT
    importing
      !PF_STREAM type STRING
    returning
      value(PTX_TEXT) type ZAPCMD_TBL_STRING .
  methods GET_EXTENSION
    returning
      value(R_EXTENSION) type STRING .

  methods INIT
    redefinition .
protected section.
*"* protected components of class ZAPCMD_FILE
*"* do not include other source files here!!!
private section.
*"* private components of class ZAPCMD_CL_FILE
*"* do not include other source files here!!!
ENDCLASS.



CLASS ZAPCMD_CL_FILE IMPLEMENTATION.


method GET_EXTENSION.

  r_extension = extension.
  TRANSLATE r_extension to upper case.

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

    data lf_index type i.
    data lf_lastdotindex type i.
    data lf_len type i.
    lf_index = 0.
    lf_lastdotindex = 0.
    lf_len = strlen( name ).

    do.
      if lf_index >= lf_len.
        exit.
      endif.
      if name+lf_index(1) = '.'.
        lf_lastdotindex =  lf_index.
      endif.
      lf_index = lf_index  + 1.
    enddo.
    if lf_lastdotindex > 1 and lf_lastdotindex < lf_len.

      lf_lastdotindex = lf_lastdotindex + 1.
      extension = name+lf_lastdotindex.
      lf_lastdotindex = lf_lastdotindex - 1.
      shortname = name(lf_lastdotindex).

    endif.
endmethod.


method STREAM2TEXT.
  DATA lf_stream TYPE string.
  lf_stream = pf_stream.
  REPLACE ALL OCCURRENCES OF cl_abap_char_utilities=>cr_lf IN lf_stream
       WITH cl_abap_char_utilities=>newline.
  SPLIT lf_stream AT cl_abap_char_utilities=>newline
    INTO TABLE ptx_text.
endmethod.


method TEXT2STREAM.
  data lf_line like line of pt_text.
  loop at pt_text into lf_line.
    if sy-tabix > 1.
      CONCATENATE pfx_stream cl_abap_char_utilities=>newline lf_line
          INTO pfx_stream.
    else.
      pfx_stream = lf_line.
    endif.
  endloop.

endmethod.
ENDCLASS.
