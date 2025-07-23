INTERFACE zapcmd_if_user_exit
  PUBLIC.

  METHODS change_default_function_code
    CHANGING cv_function_code TYPE syucomm.

  METHODS change_factories
    CHANGING ct_factories TYPE zapcmd_tbl_factory.

ENDINTERFACE.
