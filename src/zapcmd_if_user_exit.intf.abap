INTERFACE zapcmd_if_user_exit
  PUBLIC.

  METHODS commander_set_pfstatus.

  METHODS commander_user_command
    IMPORTING iv_function_code  TYPE syst_ucomm
              io_filelist_left  TYPE REF TO zapcmd_cl_filelist
              io_filelist_right TYPE REF TO zapcmd_cl_filelist.

  METHODS change_default_directory
    IMPORTING iv_side          TYPE string
    CHANGING  cv_function_code TYPE syucomm
              cv_directory     TYPE string.

  METHODS change_factories
    IMPORTING iv_side      TYPE string
    CHANGING  ct_factories TYPE zapcmd_tbl_factory.

  METHODS change_toolbar
    IMPORTING iv_side    TYPE string
    CHANGING  ct_buttons TYPE ttb_button.

  METHODS filelist_user_command
    IMPORTING iv_function_code TYPE syst_ucomm
              io_directory     TYPE REF TO zapcmd_cl_dir.

ENDINTERFACE.
