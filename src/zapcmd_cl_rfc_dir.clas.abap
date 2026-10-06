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
  methods NEW_INSTANCE
    redefinition .
  methods EXECUTE_COMMAND
    redefinition .
protected section.
*"* protected components of class ZAPCMD_CL_RFC_DIR
*"* do not include other source files here!!!

  methods GET_RFCDEST final
    returning
      value(RESULT) type RFCDEST ##CALLED.

  methods READ_DRIVES
    exporting
      value(PT_FILELIST) type ZAPCMD_TBL_FILELIST .
private section.
*"* private components of class ZAPCMD_CL_RFC_DIR
*"* do not include other source files here!!!

  data RFCDEST type RFCDEST .
ENDCLASS.



CLASS ZAPCMD_CL_RFC_DIR IMPLEMENTATION.


method CONSTRUCTOR.
    call method super->constructor.

    server_area = zapcmd_cl_knot=>co_area_rfc.
    rfcdest     = iv_rfcdest.

    try.

    data l_opsys type syopsys.
    CALL FUNCTION 'ZAPCMD_GET_OPSYS'
      DESTINATION rfcdest
      IMPORTING
         EV_OPSYS       = l_opsys
      EXCEPTIONS
        SYSTEM_FAILURE = 1
        COMMUNICATION_failure = 2.
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

    AREA_STRING = 'RFC-conn.'(001).

endmethod.


method CREATE_DIR.

    data l_parameter type text255.
    data l_message type c length 255.

    create object pf_file type Zapcmd_CL_RFC_DIR
      exporting
       iv_rfcdest = rfcdest
      exceptions
       not_installed = 1.
    if sy-subrc <> 0.
      clear pf_file.
      return.
    endif.

    call method pf_file->init
      EXPORTING
        pf_name    = pf_filename
        pf_dir     = full_name
        pf_moddate = sy-datum
        pf_modtime = sy-uzeit
        pf_attr    = space.

    if fits_os_command( quote_os_arg( pf_file->full_name ) ) = abap_false.
      clear pf_file.
      return.
    endif.
    l_parameter = quote_os_arg( pf_file->full_name ).
    call function 'ZAPCMD_EXEC_CMD'
      DESTINATION rfcdest
      exporting
        iv_command   = 'mkdir'
        iv_parameter = l_parameter
      exceptions
        not_found             = 1
        system_failure        = 2 message l_message
        communication_failure = 3 message l_message
        others                = 4.
    if sy-subrc = 2 or sy-subrc = 3.
      clear pf_file.
      message l_message type 'S' display like 'E'.
    elseif sy-subrc <> 0.
      clear pf_file.
      message 'OS command failed'(006) type 'S' display like 'E'.
    endif.


endmethod.


method CREATE_FILE.

   create object pf_file type ZAPCMD_CL_RFC_FILE
     EXPORTING
       iv_rfcdest = rfcdest
     EXCEPTIONS
       not_installed = 1.
    if sy-subrc <> 0.
      clear pf_file.
      return.
    endif.

    call method pf_file->init
      EXPORTING
        pf_name    = pf_filename
        pf_dir     = full_name
        pf_moddate = sy-datum
        pf_modtime = sy-uzeit
        pf_attr    = space.

endmethod.


METHOD create_new.
  CASE i_fcode.
    WHEN co_drives.
      eo_dir = new_instance( me->separator ).
  ENDCASE.
ENDMETHOD.


METHOD new_instance.

  CREATE OBJECT eo_dir TYPE zapcmd_cl_rfc_dir
    EXPORTING
      iv_rfcdest    = me->rfcdest
    EXCEPTIONS
      not_installed = 1.
  IF sy-subrc <> 0.
    CLEAR eo_dir.
    MESSAGE 'RFC-Destination not reachable'(005) TYPE 'S' DISPLAY LIKE 'E'.
    RETURN.
  ENDIF.

  eo_dir->init( pf_full_name = pf_full_name ).

ENDMETHOD.


method DELETE.

  data l_parameter type text255.
  data l_message type c length 255.

  if fits_os_command( quote_os_arg( full_name ) ) = abap_false.
    return.
  endif.
  l_parameter = quote_os_arg( full_name ).

  call function 'ZAPCMD_EXEC_CMD'
   DESTINATION rfcdest
   EXPORTING
     IV_COMMAND         = 'rmdir'
*     IV_DIR             =
     IV_PARAMETER       = l_parameter
*   TABLES
*     ET_OUTPUT          =
   EXCEPTIONS
     NOT_FOUND          = 1
     system_failure        = 2 MESSAGE l_message
     communication_failure = 3 MESSAGE l_message
     OTHERS             = 4
            .
  if sy-subrc = 2 or sy-subrc = 3.
    MESSAGE l_message TYPE 'S' DISPLAY LIKE 'E'.
  elseif sy-subrc <> 0.
    MESSAGE 'OS command failed'(006) TYPE 'S' DISPLAY LIKE 'E'.
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
       cant_get_destinations    = 2
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
      MOVE 'Drives'(232) to ls_toolbar-text.
      MOVE 'Drives'(232) TO ls_toolbar-quickinfo.
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
        iv_rfcdest    = rfcdest
      EXCEPTIONS
        not_installed = 1.
    IF sy-subrc <> 0.
      MESSAGE 'RFC-Destination not reachable'(005) TYPE 'S' DISPLAY LIKE 'E'.
      RETURN.
    ENDIF.
    CALL METHOD lf_ref_file->init
      EXPORTING
        pf_name      = '..'
        pf_full_name = '\'
        pf_dir       = full_name.
    APPEND lf_ref_file TO pt_filelist.


  ENDIF.

  data lt_dir type table of ZAPCMD_FILE_DESCR.
  data ls_dir type ZAPCMD_FILE_DESCR.
  data l_message type c length 255.

  CALL FUNCTION 'ZAPCMD_READ_DIR'
    DESTINATION rfcdest
    EXPORTING
      iv_dir          = lf_dir
      IV_FILTER       = lf_filter
    tables
      et_file         = lt_dir
 EXCEPTIONS
   NOT_FOUND       = 1
   SYSTEM_FAILURE        = 2 MESSAGE l_message
   COMMUNICATION_FAILURE = 3 MESSAGE l_message
   OTHERS          = 4
            .
  IF sy-subrc = 2 OR sy-subrc = 3.
    MESSAGE l_message TYPE 'S' DISPLAY LIKE 'E'.
    RETURN.
  ELSEIF sy-subrc <> 0.
    MESSAGE ID SY-MSGID TYPE 'I' NUMBER SY-MSGNO display like SY-MSGTY
         WITH SY-MSGV1 SY-MSGV2 SY-MSGV3 SY-MSGV4.
  ENDIF.

  loop at lt_dir into ls_dir.


     case ls_dir-filetype.
        when  'DIR' or 'UP'.
          create object lf_ref_file type ZAPCMD_CL_RFC_DIR
            EXPORTING
              iv_RFCDEST = RFCDEST
            EXCEPTIONS
              not_installed = 1.
        when others.
          create object lf_ref_file type ZAPCMD_CL_RFC_FILE
             EXPORTING
              iv_RFCDEST = RFCDEST
            EXCEPTIONS
              not_installed = 1.
      endcase.
      if sy-subrc <> 0.
        MESSAGE 'RFC-Destination not reachable'(005) TYPE 'S' DISPLAY LIKE 'E'.
        return.
      endif.
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


METHOD get_rfcdest.

  result = rfcdest.

ENDMETHOD.


method READ_DRIVES.

* ...
    data lf_drives(26) type c value 'ABCDEFGHIJKLMNOPQRSTUVWXYZ'.
    data lf_index type i value 0.
    data lf_drive type string.
    data lf_name type string.

    data lf_ref_file type ref to Zapcmd_CL_KNOT.

    do 26 times.
      lf_drive = lf_drives+lf_index(1).
      concatenate lf_drive ':' separator into lf_drive.

      data lf_temp(255) type c.
      lf_temp = lf_drive.
      data l_reachable type xfeld.
      clear l_reachable.

      data l_message type c length 255.
      CALL FUNCTION 'ZAPCMD_CHECK_DIR'
        DESTINATION rfcdest
        EXPORTING
          iv_dir             = lf_temp
       IMPORTING
         EV_REACHABLE       = l_reachable
       EXCEPTIONS
         system_failure        = 1 MESSAGE l_message
         communication_failure = 2 MESSAGE l_message.
      if sy-subrc <> 0.
        message l_message type 'S' display like 'E'.
        exit.
      endif.


       if l_reachable = 'X'.

        concatenate lf_drive space into lf_name.
        create object lf_ref_file type Zapcmd_CL_rfc_DIR
          EXPORTING
            iv_rfcdest = rfcdest
          EXCEPTIONS
            not_installed = 1.
        if sy-subrc <> 0.
          exit.
        endif.
        call method lf_ref_file->init
          EXPORTING
            pf_full_name = lf_drive
            pf_name      = lf_name
            pf_dir       = full_name.

        append lf_ref_file to pt_filelist.
      endif.

      lf_index = lf_index + 1.
    enddo.

endmethod.


METHOD execute_command.

  DATA l_command TYPE text255.
  DATA l_dir TYPE text255.
  DATA l_message TYPE c LENGTH 255.
  DATA lt_output TYPE STANDARD TABLE OF zapcmd_t_text255.
  DATA ls_output TYPE zapcmd_t_text255.
  DATA l_line TYPE string.

  CLEAR et_output.
  IF fits_os_command( pf_command ) = abap_false
  OR fits_os_command( full_name ) = abap_false.
    ev_return_code = 8.
    RETURN.
  ENDIF.
  l_command = pf_command.
  l_dir = full_name.

  CALL FUNCTION 'ZAPCMD_EXEC_CMD'
    DESTINATION rfcdest
    EXPORTING
      iv_command            = l_command
      iv_dir                = l_dir
    TABLES
      et_output             = lt_output
    EXCEPTIONS
      not_found             = 1
      system_failure        = 2 MESSAGE l_message
      communication_failure = 3 MESSAGE l_message
      OTHERS                = 4.
  ev_return_code = sy-subrc.
  CASE ev_return_code.
    WHEN 0.
      LOOP AT lt_output INTO ls_output.
        l_line = ls_output-text.
        APPEND l_line TO et_output.
      ENDLOOP.
    WHEN 2 OR 3.
      l_line = l_message.
      APPEND l_line TO et_output.
    WHEN OTHERS.
      APPEND 'OS command failed'(006) TO et_output.
  ENDCASE.

ENDMETHOD.

ENDCLASS.
