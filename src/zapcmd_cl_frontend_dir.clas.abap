class ZAPCMD_CL_FRONTEND_DIR definition
  public
  inheriting from ZAPCMD_CL_DIR
  final
  create public .

*"* public components of class ZAPCMD_CL_FRONTEND_DIR
*"* do not include other source files here!!!
public section.
  type-pools ICON .

  methods CONSTRUCTOR .

  methods CREATE_DIR
    redefinition .
  methods CREATE_FILE
    redefinition .
  methods DELETE
    redefinition .
  methods EXECUTE
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
*"* protected components of class ZAPCMD_CL_FRONTEND_DIR
*"* do not include other source files here!!!
private section.
*"* private components of class ZAPCMD_CL_FRONTEND_DIR
*"* do not include other source files here!!!

   methods READ_DRIVES
      exporting
        value(PT_FILELIST) type ZAPCMD_TBL_FILELIST .
ENDCLASS.



CLASS ZAPCMD_CL_FRONTEND_DIR IMPLEMENTATION.


method CONSTRUCTOR.
* ...
    call method super->constructor.
    separator = '\'.
    server_area = 'P'.
    AREA_STRING = 'Frontend'(003).
endmethod.


method CREATE_DIR.

     create object pf_file type ZAPCMD_CL_FRONTEND_DIR.

    call method pf_file->init
      EXPORTING
        pf_name    = pf_filename
        pf_size    = 0
        pf_dir     = full_name
        pf_moddate = sy-datum
        pf_modtime = sy-uzeit
        pf_attr    = space.

    data lf_rc type sysubrc.

    CALL METHOD CL_GUI_FRONTEND_SERVICES=>DIRECTORY_CREATE
      EXPORTING
        DIRECTORY                = pf_file->full_name
      CHANGING
        RC                       = lf_rc
      EXCEPTIONS
        DIRECTORY_CREATE_FAILED  = 1
        CNTL_ERROR               = 2
        ERROR_NO_GUI             = 3
        DIRECTORY_ACCESS_DENIED  = 4
        DIRECTORY_ALREADY_EXISTS = 5
        PATH_NOT_FOUND           = 6
        UNKNOWN_ERROR            = 7
*           NOT_SUPPORTED_BY_GUI     = 8
*           WRONG_PARAMETER          = 9
        others                   = 10
            .
    IF SY-SUBRC <> 0.
      MESSAGE ID SY-MSGID TYPE SY-MSGTY NUMBER SY-MSGNO
                 WITH SY-MSGV1 SY-MSGV2 SY-MSGV3 SY-MSGV4.
    ENDIF.


endmethod.


method CREATE_FILE.

  CREATE OBJECT pf_file TYPE zapcmd_cl_frontend_file.

  CALL METHOD pf_file->init
    EXPORTING
      pf_name    = pf_filename
      pf_size    = 0
      pf_dir     = full_name
      pf_moddate = sy-datum
      pf_modtime = sy-uzeit
      pf_attr    = space.


endmethod.


method CREATE_NEW.

  case i_fcode.
    when CO_drives.


    data lf_rootdir type string.
    lf_rootdir = me->separator.

    create object eo_dir type zapcmd_cl_frontend_dir.

    call method eo_dir->init
      EXPORTING
        pf_full_name = lf_rootdir.

  endcase.


endmethod.


method DELETE.

     data lf_rc type sysubrc.

    CALL METHOD CL_GUI_FRONTEND_SERVICES=>DIRECTORY_DELETE
      EXPORTING
        DIRECTORY               = full_name
      CHANGING
        RC                      = lf_rc
      EXCEPTIONS
        DIRECTORY_DELETE_FAILED = 1
        CNTL_ERROR              = 2
        ERROR_NO_GUI            = 3
        PATH_NOT_FOUND          = 4
        DIRECTORY_ACCESS_DENIED = 5
        UNKNOWN_ERROR           = 6
*       NOT_SUPPORTED_BY_GUI    = 7
*       WRONG_PARAMETER         = 8
        others                  = 9.
    IF SY-SUBRC <> 0.
      MESSAGE ID SY-MSGID TYPE SY-MSGTY NUMBER SY-MSGNO
                 WITH SY-MSGV1 SY-MSGV2 SY-MSGV3 SY-MSGV4.
    ENDIF.

endmethod.


method EXECUTE.



endmethod.


METHOD get_freespace.

  DATA lf_space TYPE i.
  DATA len TYPE i.
  len = STRLEN( full_name ).

  IF len >= 2.
    DATA lf_drive TYPE string.
    lf_drive = full_name(2).
    IF lf_drive <> '\\'.

      CALL METHOD cl_gui_frontend_services=>get_free_space_for_drive
        EXPORTING
          drive                 = lf_drive
        CHANGING
          free_space            = lf_space
        EXCEPTIONS
          cntl_error            = 1
          get_free_space_failed = 2
          error_no_gui          = 3
          wrong_parameter       = 4
          not_supported_by_gui  = 5
          OTHERS                = 6.
      IF sy-subrc <> 0.
        EXIT.
      ENDIF.
      CALL METHOD cl_gui_cfw=>flush
        EXCEPTIONS
          cntl_system_error = 1
          cntl_error        = 2
          OTHERS            = 3.

      IF sy-subrc <> 0.
        MESSAGE ID sy-msgid TYPE sy-msgty NUMBER sy-msgno
                   WITH sy-msgv1 sy-msgv2 sy-msgv3 sy-msgv4.
      ENDIF.


      pf_space = lf_space.
    ENDIF.
  ENDIF.

ENDMETHOD.


method GET_TOOLBAR.

    data ls_toolbar type STB_BUTTON.

    CLEAR ls_toolbar.
    MOVE 0 TO ls_toolbar-butn_type.
    MOVE co_drives TO ls_toolbar-function.
    MOVE ICON_SYSTEM_SAVE TO ls_toolbar-icon.
    MOVE 'Laufwerke'(232) to ls_toolbar-text.
    MOVE 'Laufwerke'(232) TO ls_toolbar-quickinfo.
    MOVE SPACE TO ls_toolbar-disabled.
    APPEND ls_toolbar TO pt_toolbar.

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

    CALL METHOD cl_gui_frontend_services=>directory_get_current
      CHANGING
        current_directory            = full_name
      EXCEPTIONS
        directory_get_current_failed = 1
        cntl_error                   = 2
        OTHERS                       = 3.
    IF sy-subrc <> 0.
      MESSAGE ID sy-msgid TYPE sy-msgty NUMBER sy-msgno
                 WITH sy-msgv1 sy-msgv2 sy-msgv3 sy-msgv4.
    ENDIF.

    CALL METHOD cl_gui_cfw=>flush
      EXCEPTIONS
        cntl_system_error = 1
        cntl_error        = 2
        OTHERS            = 3.

    IF sy-subrc <> 0.
      MESSAGE ID sy-msgid TYPE sy-msgty NUMBER sy-msgno
                 WITH sy-msgv1 sy-msgv2 sy-msgv3 sy-msgv4.
    ENDIF.

  ENDIF.

  IF full_name IS INITIAL OR full_name = '.'.
    full_name = 'C:\'.
  ENDIF.



ENDMETHOD.


method READ_DIR.

  DATA lf_filter TYPE string.
  DATA lf_dir TYPE string.

  DATA lf_ref_file TYPE REF TO zapcmd_cl_knot.

  IF pf_mask IS INITIAL.
    lf_filter = filter.
  ELSE.
    lf_filter = pf_mask.
  ENDIF.
  lf_dir = full_name.

  DATA lt_fileinfo TYPE TABLE OF file_info.
  DATA ls_fileinfo TYPE file_info.
  FIELD-SYMBOLS <ls_fileinfo> TYPE file_info.
  DATA lf_count TYPE i.

  IF full_name = separator OR full_name = co_rootdir.
    CALL METHOD read_drives
      IMPORTING
        pt_filelist = pt_filelist.
    EXIT.
  ENDIF.

      if full_name = co_rfcdir.
      call method read_rfc
        IMPORTING
          pt_filelist = pt_filelist.
      exit.
    endif.

  CREATE OBJECT lf_ref_file TYPE zapcmd_cl_frontend_dir.
  IF lf_filter CA '*'.
    CALL METHOD lf_ref_file->init
      EXPORTING
        pf_name = '..'
        pf_size = 0
        pf_dir  = full_name.

    APPEND lf_ref_file TO pt_filelist.
  ENDIF.


  CALL METHOD cl_gui_frontend_services=>directory_list_files
    EXPORTING
      directory                   = lf_dir
      filter                      = lf_filter
*      FILES_ONLY                  =
*      DIRECTORIES_ONLY            =
    CHANGING
      file_table                  = lt_fileinfo
      count                       = lf_count
    EXCEPTIONS
      cntl_error                  = 1
      directory_list_files_failed = 2
      wrong_parameter             = 3
      error_no_gui                = 4
      OTHERS                      = 5
          .
  IF sy-subrc <> 0.
    MESSAGE ID sy-msgid TYPE sy-msgty NUMBER sy-msgno
               WITH sy-msgv1 sy-msgv2 sy-msgv3 sy-msgv4.
  ENDIF.

  CALL METHOD cl_gui_cfw=>flush
    EXCEPTIONS
      cntl_system_error = 1
      cntl_error        = 2
      OTHERS            = 3.

*    data lf_string type string.
*    loop at lt_fileinfo assigning <ls_fileinfo>.
*      DATA:
*        buffer TYPE XSTRING,
*        conv TYPE REF TO cl_abap_conv_out_ce.
*
*      conv = cl_abap_conv_out_ce=>create(
**         encoding = '1100'
**         endian = 'L'
*             ).
*
*      CALL METHOD conv->write( data = <ls_fileinfo>-filename ).
*
*      buffer = conv->get_buffer( ).
*
*      data conv2 TYPE REF TO cl_abap_conv_in_ce.
*
*      conv2 = cl_abap_conv_in_ce=>create(
*               encoding = 'UTF-8'
*               endian = 'L'
*               input = buffer
*             ).
*
*      CALL METHOD conv2->read(
*        IMPORTING data = <ls_fileinfo>-filename
*      ).
*
*
*      lf_string = <ls_fileinfo>-filename.
*      CALL METHOD CL_GUI_FRONTEND_SERVICES=>FILE_GET_SIZE
*        EXPORTING
*          FILE_NAME            = lf_string
*        IMPORTING
*          FILE_SIZE            = <ls_fileinfo>-filelength
*        EXCEPTIONS
*          FILE_GET_SIZE_FAILED = 1
*          CNTL_ERROR           = 2
*          ERROR_NO_GUI         = 3
*          NOT_SUPPORTED_BY_GUI = 4
*          others               = 5.
*
*    endloop.
*    CALL METHOD CL_GUI_CFW=>FLUSH
*      EXCEPTIONS
*        CNTL_SYSTEM_ERROR = 1
*        CNTL_ERROR        = 2
*        others            = 3.


*  SORT lt_fileinfo BY isdir DESCENDING filename.

  LOOP AT lt_fileinfo INTO ls_fileinfo.
    CASE ls_fileinfo-isdir.
      WHEN '1'.
        CREATE OBJECT lf_ref_file TYPE zapcmd_cl_frontend_dir.
      WHEN OTHERS.
        CREATE OBJECT lf_ref_file TYPE zapcmd_cl_frontend_file.
    ENDCASE.
    DATA lf_name TYPE string.
    lf_name = ls_fileinfo-filename.
    DATA lf_size TYPE p.
    lf_size = ls_fileinfo-filelength.
    DATA lf_modus TYPE char10.
    CLEAR lf_modus.
    IF ls_fileinfo-isreadonly > 0.
      CONCATENATE lf_modus 'r' INTO lf_modus.
    ELSE.
      CONCATENATE lf_modus '-' INTO lf_modus.
    ENDIF.
    IF ls_fileinfo-isarchived > 0.
      CONCATENATE lf_modus 'a' INTO lf_modus.
    ELSE.
      CONCATENATE lf_modus '-' INTO lf_modus.
    ENDIF.
    IF ls_fileinfo-ishidden > 0.
      CONCATENATE lf_modus 'h' INTO lf_modus.
    ELSE.
      CONCATENATE lf_modus '-' INTO lf_modus.
    ENDIF.
    IF ls_fileinfo-issystem > 0.
      CONCATENATE lf_modus 's' INTO lf_modus.
    ELSE.
      CONCATENATE lf_modus '-' INTO lf_modus.
    ENDIF.




    CALL METHOD lf_ref_file->init
      EXPORTING
        pf_name    = lf_name
        pf_size    = lf_size
        pf_dir     = full_name
        pf_moddate = ls_fileinfo-writedate
        pf_modtime = ls_fileinfo-writetime
        pf_attr    = lf_modus.

    APPEND lf_ref_file TO pt_filelist.


  ENDLOOP.

endmethod.


method READ_DRIVES.
* ...
    data lf_drives(26) type c value 'ABCDEFGHIJKLMNOPQRSTUVWXYZ'.
    data lf_index type i value 0.
    data lf_drive type string.
    data lf_name type string.
    data lf_drivetype type string.

    data lf_ref_file type ref to ZAPCMD_CL_KNOT.

    do 26 times.
      lf_drive = lf_drives+lf_index(1).
      concatenate lf_drive ':' separator into lf_drive.
      CALL METHOD CL_GUI_FRONTEND_SERVICES=>GET_DRIVE_TYPE
        EXPORTING
          DRIVE                = lf_drive
        CHANGING
          DRIVE_TYPE           = lf_drivetype
        EXCEPTIONS
          CNTL_ERROR           = 1
          BAD_PARAMETER        = 2
          ERROR_NO_GUI         = 3
          NOT_SUPPORTED_BY_GUI = 4
          others               = 5.

      CALL METHOD CL_GUI_CFW=>FLUSH
        EXCEPTIONS
          CNTL_SYSTEM_ERROR = 1
          CNTL_ERROR        = 2
          others            = 3.

      IF SY-SUBRC <> 0.
        MESSAGE ID SY-MSGID TYPE SY-MSGTY NUMBER SY-MSGNO
                   WITH SY-MSGV1 SY-MSGV2 SY-MSGV3 SY-MSGV4.
      ENDIF.

      IF SY-SUBRC = 0 and not lf_drivetype is initial.

        concatenate lf_drive ' (' lf_drivetype ')' into lf_name.
        create object lf_ref_file type ZAPCMD_CL_FRONTEND_DIR.
        call method lf_ref_file->init
          EXPORTING
            pf_full_name = lf_drive
            pf_name      = lf_name
            pf_size      = 0
            pf_dir       = full_name.

        append lf_ref_file to pt_filelist.

      ENDIF.

      lf_index = lf_index + 1.
    enddo.
endmethod.
ENDCLASS.
