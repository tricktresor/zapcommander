class ZAPCMD_CL_RFC_DIR definition
  public
  inheriting from ZAPCMD_CL_DIR
  create public .

*"* public components of class ZAPCMD_CL_RFC_DIR
*"* do not include other source files here!!!
public section.

  methods CONSTRUCTOR
    importing
      !IV_RFCDEST type RFCDEST
    exceptions
      NOT_INSTALLED .

  methods CREATE_DIR
    redefinition .
  methods CREATE_FILE
    redefinition .
  methods DELETE
    redefinition .
  methods GET_FREESPACE
    redefinition .
  methods GET_TOOLBAR
    redefinition .
  methods INIT
    redefinition .
  methods READ_DIR
    redefinition .
  methods CREATE_NEW
    redefinition .
protected section.
*"* protected components of class ZAPCMD_CL_RFC_DIR
*"* do not include other source files here!!!
private section.
*"* private components of class ZAPCMD_CL_RFC_DIR
*"* do not include other source files here!!!

  data RFCDEST type RFCDEST .

  methods READ_DRIVES
    exporting
      value(PT_FILELIST) type ZAPCMD_TBL_FILELIST .
ENDCLASS.



CLASS ZAPCMD_CL_RFC_DIR IMPLEMENTATION.


method CONSTRUCTOR.
    call method super->constructor.

    server_area = 'R'.
    rfcdest = iv_rfcdest.

    try.

    data l_opsys type syopsys.
    CALL FUNCTION 'ZAPCMD_GET_OPSYS'
      DESTINATION rfcdest
      IMPORTING
         EV_OPSYS       = l_opsys
      EXCEPTIONS
        SYSTEM_FAILURE = 1
        COMMUNICATION_failure = 2.
      .
    if sy-subrc <> 0.
      raise not_installed.
    endif.
    CATCH cx_root.
      raise not_installed.
    endtry.

    if l_opsys = 'Windows NT' or sy-opsys = 'DOS'. "#EC NOTEXT
      separator = '\'.
    else.
      separator = '/'.
    endif.

    AREA_STRING = 'RFC-Verb.'(001).

endmethod.


method CREATE_DIR.

    create object pf_file type Zapcmd_CL_RFC_DIR
      exporting
       iv_rfcdest = rfcdest.

    call method pf_file->init
      EXPORTING
        pf_name    = pf_filename
        pf_size    = 0
        pf_dir     = full_name
        pf_moddate = sy-datum
        pf_modtime = sy-uzeit
        pf_attr    = space.

    call function 'ZAPCMD_EXEC_CMD'
      DESTINATION rfcdest
      exporting
        pf_command = 'mkdir'
        pf_parameter = pf_file->full_name.


endmethod.


method CREATE_FILE.

   create object pf_file type ZAPCMD_CL_RFC_FILE
     EXPORTING
       iv_rfcdest = rfcdest.

    call method pf_file->init
      EXPORTING
        pf_name    = pf_filename
        pf_size    = 0
        pf_dir     = full_name
        pf_moddate = sy-datum
        pf_modtime = sy-uzeit
        pf_attr    = space.

endmethod.


METHOD create_new.
  CASE i_fcode.
    WHEN co_drives.
      CREATE OBJECT eo_dir TYPE zapcmd_cl_rfc_dir
        EXPORTING
          iv_rfcdest = me->rfcdest.
      eo_dir->init( pf_full_name = me->separator ).
  ENDCASE.
ENDMETHOD.


method DELETE.

  call function 'ZAPCMD_EXEC_CMD'
   DESTINATION rfcdest
   EXPORTING
     IV_COMMAND         = 'rmdir'
*     IV_DIR             =
     IV_PARAMETER       = full_name
*   TABLES
*     ET_OUTPUT          =
   EXCEPTIONS
     NOT_FOUND          = 1
     OTHERS             = 2
            .
  if sy-subrc <> 0.
   MESSAGE ID SY-MSGID TYPE SY-MSGTY NUMBER SY-MSGNO
           WITH SY-MSGV1 SY-MSGV2 SY-MSGV3 SY-MSGV4.
  endif.


endmethod.


method GET_FREESPACE.

     data lf_pathname type DEF_PAR_FU-PATHNAME.
    data lf_freespace type DEF_PAR_FU-FREESPACE.
    lf_pathname = full_name.

    CALL FUNCTION 'SHOW_FILEPATH_FREESPACE' "FREESPACE FROM OSCOL
     EXPORTING
        file_path = lf_pathname
        dest_type = 'S'
        I_LOCAL_REMOTE = 'REMOTE'
        I_LOGICAL_DEST = RFCDEST
     IMPORTING
        free_space = lf_freespace
     EXCEPTIONS
       cant_find_destination    = 1
       cant_get_destinations    = 1
       OTHERS                   = 4.
    if sy-subrc <> 0.
      lf_freespace = 0.
    endif.
*   Umwandlung von KByte in Byte
    pf_space = lf_freespace * 1024.

endmethod.


method GET_TOOLBAR.


    data ls_toolbar type STB_BUTTON.

    if separator = '\'.
      CLEAR ls_toolbar.
      MOVE 0 TO ls_toolbar-butn_type.
      MOVE co_drives TO ls_toolbar-function.
      MOVE ICON_SYSTEM_SAVE TO ls_toolbar-icon.
      MOVE 'Laufwerke'(232) to ls_toolbar-text.
      MOVE 'Laufwerke'(232) TO ls_toolbar-quickinfo.
      MOVE SPACE TO ls_toolbar-disabled.
      APPEND ls_toolbar TO pt_toolbar.
    endif.


endmethod.


METHOD init.
  CALL METHOD super->init
    EXPORTING
      pf_name      = pf_name
      pf_full_name = pf_full_name
      pf_size      = pf_size
      pf_moddate   = pf_moddate
      pf_modtime   = pf_modtime
      pf_attr      = pf_attr
      pf_dir       = pf_dir.

  IF full_name IS INITIAL.

    DATA lf_temp(255) TYPE c.
    CALL FUNCTION 'ZAPCMD_GET_HOMEDIR'
      DESTINATION rfcdest
      IMPORTING
        ev_fullname = lf_temp
      EXCEPTIONS
        SYSTEM_FAILURE = 1
        COMMUNICATION_failure = 2.
      .
    if sy-subrc <> 0.
    endif.

    full_name = lf_temp.

  ENDIF.

  IF full_name = '.'.
    IF separator = '\'.
      full_name = 'C:\'.
    ELSE.
      full_name = separator.
    ENDIF.
  ENDIF.




ENDMETHOD.


METHOD read_dir.


  DATA lf_ref_file TYPE REF TO zapcmd_cl_knot.
*    data lt_filelist_undo like pt_filelist.
*    lt_filelist_undo[] = pt_filelist[].
  REFRESH pt_filelist.

  DATA lf_filter(255) TYPE c.
  DATA lf_dir(255) TYPE c.

  IF pf_mask IS INITIAL.
    lf_filter = filter.
  ELSE.
    lf_filter = pf_mask.
  ENDIF.
  lf_dir = full_name.


  DATA lf_server TYPE string.
  DATA lf_strlen TYPE i.
  lf_strlen = STRLEN( full_name ).

  IF full_name = '\'.
    CALL METHOD read_drives
      IMPORTING
        pt_filelist = pt_filelist.
    EXIT.
  ENDIF.

  IF strlen( full_name ) > 0 and  full_name+1 = ':\' AND '*' CA lf_filter.

    CREATE OBJECT lf_ref_file TYPE zapcmd_cl_rfc_dir
      EXPORTING
        iv_rfcdest = rfcdest.
    CALL METHOD lf_ref_file->init
      EXPORTING
        pf_name      = '..'
        pf_full_name = '\'
        pf_size      = 0
        pf_dir       = full_name.
    APPEND lf_ref_file TO pt_filelist.


  ENDIF.

  data lt_dir type table of ZAPCMD_FILE_DESCR.
  data ls_dir type ZAPCMD_FILE_DESCR.

  CALL FUNCTION 'ZAPCMD_READ_DIR'
    DESTINATION rfcdest
    EXPORTING
      iv_dir          = lf_dir
      IV_FILTER       = lf_filter
    tables
      et_file         = lt_dir
 EXCEPTIONS
   NOT_FOUND       = 1
   OTHERS          = 2
            .
  IF sy-subrc <> 0.
    MESSAGE ID SY-MSGID TYPE 'I' NUMBER SY-MSGNO display like SY-MSGTY
         WITH SY-MSGV1 SY-MSGV2 SY-MSGV3 SY-MSGV4.
  ENDIF.

  loop at lt_dir into ls_dir.


     case ls_dir-filetype.
        when  'DIR' or 'UP'.
          create object lf_ref_file type ZAPCMD_CL_RFC_DIR
            EXPORTING
              iv_RFCDEST = RFCDEST.
        when others.
          create object lf_ref_file type ZAPCMD_CL_RFC_FILE
             EXPORTING
              iv_RFCDEST = RFCDEST.
      endcase.
      if ls_dir-name <> space and ls_dir-name <> '.'.
        call method lf_ref_file->init
          EXPORTING
            pf_name    = ls_dir-name
            pf_size    = ls_dir-FILESIZE
            pf_dir     = full_name
            pf_modtime = ls_dir-modtime
            pf_moddate = ls_dir-moddate
            pf_attr    = ls_dir-attr.
        append lf_ref_file to pt_filelist.
      endif.

  endloop.


ENDMETHOD.


method READ_DRIVES.

* ...
    data lf_drives(26) type c value 'ABCDEFGHIJKLMNOPQRSTUVWXYZ'.
    data lf_index type i value 0.
    data lf_drive type string.
    data lf_name type string.
    data lf_drivetype type string.

    data lf_ref_file type ref to Zapcmd_CL_KNOT.

    do 26 times.
      lf_drive = lf_drives+lf_index(1).
      concatenate lf_drive ':' separator into lf_drive.

      data lf_temp(255) type c.
      lf_temp = lf_drive.
      data l_reachable type xfeld.
      clear l_reachable.

      CALL FUNCTION 'ZAPCMD_CHECK_DIR'
        DESTINATION rfcdest
        EXPORTING
          iv_dir             = lf_temp
       IMPORTING
         EV_REACHABLE       = l_reachable
                .


       if l_reachable = 'X'.

        concatenate lf_drive space into lf_name.
        create object lf_ref_file type Zapcmd_CL_rfc_DIR
          EXPORTING
            iv_rfcdest = rfcdest.
        call method lf_ref_file->init
          EXPORTING
            pf_full_name = lf_drive
            pf_name      = lf_name
            pf_size      = 0
            pf_dir       = full_name.

        append lf_ref_file to pt_filelist.
      endif.

      lf_index = lf_index + 1.
    enddo.

endmethod.
ENDCLASS.
