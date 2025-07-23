INTERFACE zapcmd_if_user_exit
  PUBLIC.

  METHODS change_default_directory
    IMPORTING iv_side          TYPE string
    CHANGING  cv_function_code TYPE syucomm
              cv_directory     TYPE string.

  METHODS change_factories
    IMPORTING iv_side      TYPE string
    CHANGING  ct_factories TYPE zapcmd_tbl_factory.

ENDINTERFACE.
