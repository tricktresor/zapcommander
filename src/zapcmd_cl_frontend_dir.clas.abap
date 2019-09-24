CLASS zapcmd_cl_frontend_dir DEFINITION
  PUBLIC
  INHERITING FROM zapcmd_cl_dir
  FINAL
  CREATE PUBLIC .

*"* public components of class ZAPCMD_CL_FRONTEND_DIR
*"* do not include other source files here!!!
  PUBLIC SECTION.
    TYPE-POOLS icon .

    METHODS constructor .

    METHODS create_dir
        REDEFINITION .
    METHODS create_file
        REDEFINITION .
    METHODS delete
        REDEFINITION .
    METHODS execute
        REDEFINITION .
    METHODS get_freespace
        REDEFINITION .
    METHODS get_toolbar
        REDEFINITION .
    METHODS init
        REDEFINITION .
    METHODS read_dir
        REDEFINITION .
    METHODS create_new
        REDEFINITION .
  PROTECTED SECTION.
*"* protected components of class ZAPCMD_CL_FRONTEND_DIR
*"* do not include other source files here!!!
  PRIVATE SECTION.
*"* private components of class ZAPCMD_CL_FRONTEND_DIR
*"* do not include other source files here!!!

    METHODS read_drives
      EXPORTING
        VALUE(pt_filelist) TYPE zapcmd_tbl_filelist .
ENDCLASS.



CLASS ZAPCMD_CL_FRONTEND_DIR IMPLEMENTATION.


  METHOD constructor.
* ...
    super->constructor( ).
    separator = '\'.
    server_area = 'P'.
    area_string = 'Frontend'(003).

  ENDMETHOD.


  METHOD create_dir.

    CREATE OBJECT pf_file TYPE zapcmd_cl_frontend_dir.

    pf_file->init(
        pf_name    = pf_filename
        pf_size    = 0
        pf_dir     = full_name
        pf_moddate = sy-datum
        pf_modtime = sy-uzeit
        pf_attr    = space ).

    DATA lf_rc TYPE sysubrc.

    cl_gui_frontend_services=>directory_create(
      EXPORTING
        directory                = pf_file->full_name
      CHANGING
        rc                       = lf_rc
      EXCEPTIONS
        directory_create_failed  = 1
        cntl_error               = 2
        error_no_gui             = 3
        directory_access_denied  = 4
        directory_already_exists = 5
        path_not_found           = 6
        unknown_error            = 7
        OTHERS                   = 10 ).
    IF sy-subrc <> 0.
      MESSAGE ID sy-msgid TYPE sy-msgty NUMBER sy-msgno
                 WITH sy-msgv1 sy-msgv2 sy-msgv3 sy-msgv4.
    ENDIF.


  ENDMETHOD.


  METHOD create_file.

    CREATE OBJECT pf_file TYPE zapcmd_cl_frontend_file.

    pf_file->init(
        pf_name    = pf_filename
        pf_size    = 0
        pf_dir     = full_name
        pf_moddate = sy-datum
        pf_modtime = sy-uzeit
        pf_attr    = space ).

  ENDMETHOD.


  METHOD create_new.

    CASE i_fcode.
      WHEN co_drives.

        DATA lf_rootdir TYPE string.
        lf_rootdir = me->separator.
        CREATE OBJECT eo_dir TYPE zapcmd_cl_frontend_dir.

        eo_dir->init(
            pf_full_name = lf_rootdir ).
    ENDCASE.

  ENDMETHOD.


  METHOD delete.

    DATA lf_rc TYPE sysubrc.

    cl_gui_frontend_services=>directory_delete(
      EXPORTING
        directory               = full_name
      CHANGING
        rc                      = lf_rc
      EXCEPTIONS
        directory_delete_failed = 1
        cntl_error              = 2
        error_no_gui            = 3
        path_not_found          = 4
        directory_access_denied = 5
        unknown_error           = 6
        OTHERS                  = 9 ).
    IF sy-subrc <> 0.
      MESSAGE ID sy-msgid TYPE sy-msgty NUMBER sy-msgno
                 WITH sy-msgv1 sy-msgv2 sy-msgv3 sy-msgv4.
    ENDIF.

  ENDMETHOD.


  METHOD execute.



  ENDMETHOD.


  METHOD get_freespace.

    DATA lf_space TYPE i.
    DATA len TYPE i.
    len = strlen( full_name ).

    IF len >= 2.
      DATA lf_drive TYPE string.
      lf_drive = full_name(2).
      IF lf_drive <> '\\'.

        cl_gui_frontend_services=>get_free_space_for_drive(
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
            OTHERS                = 6 ).
        IF sy-subrc <> 0.
          EXIT.
        ENDIF.
        cl_gui_cfw=>flush( ).
        pf_space = lf_space.
      ENDIF.
    ENDIF.

  ENDMETHOD.


  METHOD get_toolbar.

    DATA ls_toolbar TYPE stb_button.

    CLEAR ls_toolbar.
    MOVE 0 TO ls_toolbar-butn_type.
    MOVE co_drives TO ls_toolbar-function.
    MOVE icon_system_save TO ls_toolbar-icon.
    MOVE 'Laufwerke'(232) TO ls_toolbar-text.
    MOVE 'Laufwerke'(232) TO ls_toolbar-quickinfo.
    MOVE space TO ls_toolbar-disabled.
    APPEND ls_toolbar TO pt_toolbar.

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

    IF full_name IS INITIAL.

      cl_gui_frontend_services=>directory_get_current(
        CHANGING
          current_directory            = full_name
        EXCEPTIONS
          directory_get_current_failed = 1
          cntl_error                   = 2
          OTHERS                       = 3 ).
      IF sy-subrc <> 0.
        MESSAGE ID sy-msgid TYPE sy-msgty NUMBER sy-msgno
                   WITH sy-msgv1 sy-msgv2 sy-msgv3 sy-msgv4.
      ENDIF.

      cl_gui_cfw=>flush( ).

    ENDIF.

    IF full_name IS INITIAL OR full_name = '.'.
      full_name = 'C:\'.
    ENDIF.

  ENDMETHOD.


  METHOD read_dir.

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
      read_drives(
        IMPORTING
          pt_filelist = pt_filelist ).
      EXIT.
    ENDIF.

    IF full_name = co_rfcdir.
      read_rfc(
        IMPORTING
          pt_filelist = pt_filelist ).
      EXIT.
    ENDIF.

    CREATE OBJECT lf_ref_file TYPE zapcmd_cl_frontend_dir.
    IF lf_filter CA '*'.
      lf_ref_file->init(
          pf_name = '..'
          pf_size = 0
          pf_dir  = full_name ).
      APPEND lf_ref_file TO pt_filelist.
    ENDIF.


    cl_gui_frontend_services=>directory_list_files(
      EXPORTING
        directory                   = lf_dir
        filter                      = lf_filter
      CHANGING
        file_table                  = lt_fileinfo
        count                       = lf_count
      EXCEPTIONS
        cntl_error                  = 1
        directory_list_files_failed = 2
        wrong_parameter             = 3
        error_no_gui                = 4
        OTHERS                      = 5 ).
    IF sy-subrc <> 0.
      MESSAGE ID sy-msgid TYPE sy-msgty NUMBER sy-msgno
                 WITH sy-msgv1 sy-msgv2 sy-msgv3 sy-msgv4.
    ENDIF.

    cl_gui_cfw=>flush( ).

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

      lf_ref_file->init(
          pf_name    = lf_name
          pf_size    = lf_size
          pf_dir     = full_name
          pf_moddate = ls_fileinfo-writedate
          pf_modtime = ls_fileinfo-writetime
          pf_attr    = lf_modus ).
      APPEND lf_ref_file TO pt_filelist.
    ENDLOOP.

  ENDMETHOD.


  METHOD read_drives.
* ...

    DATA lf_drives(26) TYPE c VALUE 'ABCDEFGHIJKLMNOPQRSTUVWXYZ'.
    DATA lf_index TYPE i VALUE 0.
    DATA lf_drive TYPE string.
    DATA lf_name TYPE string.
    DATA lf_drivetype TYPE string.

    DATA lf_ref_file TYPE REF TO zapcmd_cl_knot.

    DO 26 TIMES.
      lf_drive = lf_drives+lf_index(1).
      CONCATENATE lf_drive ':' separator INTO lf_drive.
      cl_gui_frontend_services=>get_drive_type(
        EXPORTING
          drive                = lf_drive
        CHANGING
          drive_type           = lf_drivetype
        EXCEPTIONS
          cntl_error           = 1
          bad_parameter        = 2
          error_no_gui         = 3
          not_supported_by_gui = 4
          OTHERS               = 5 ).

      cl_gui_cfw=>flush( ).

      IF sy-subrc = 0 AND NOT lf_drivetype IS INITIAL.

        CONCATENATE lf_drive ' (' lf_drivetype ')' INTO lf_name.
        CREATE OBJECT lf_ref_file TYPE zapcmd_cl_frontend_dir.
        lf_ref_file->init(
            pf_full_name = lf_drive
            pf_name      = lf_name
            pf_size      = 0
            pf_dir       = full_name ).
        APPEND lf_ref_file TO pt_filelist.
      ENDIF.
      lf_index = lf_index + 1.
    ENDDO.

  ENDMETHOD.
ENDCLASS.
