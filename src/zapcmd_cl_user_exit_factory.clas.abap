CLASS zapcmd_cl_user_exit_factory DEFINITION
  PUBLIC FINAL
  CREATE PRIVATE.

  PUBLIC SECTION.

    CLASS-METHODS get
      RETURNING VALUE(result) TYPE REF TO zapcmd_if_user_exit.

  PROTECTED SECTION.

  PRIVATE SECTION.

    CONSTANTS gc_class_name TYPE string VALUE 'ZAPCMD_CL_USER_EXIT'.

    CLASS-DATA gi_instance   TYPE REF TO zapcmd_if_user_exit.
    CLASS-DATA gv_not_exists TYPE abap_bool.

    CLASS-METHODS create
      RETURNING VALUE(result) TYPE REF TO zapcmd_if_user_exit.

ENDCLASS.



CLASS zapcmd_cl_user_exit_factory IMPLEMENTATION.


  METHOD get.

    IF gv_not_exists = abap_false AND gi_instance IS NOT BOUND.
      gi_instance = create( ).
    ENDIF.

    result = gi_instance.

  ENDMETHOD.


  METHOD create.

    TRY.
        CREATE OBJECT result TYPE (gc_class_name).
      CATCH cx_sy_create_object_error ##NO_HANDLER.
        gv_not_exists = abap_true.
    ENDTRY.

  ENDMETHOD.


ENDCLASS.
