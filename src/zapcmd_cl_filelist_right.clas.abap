CLASS zapcmd_cl_filelist_right DEFINITION
  PUBLIC
  INHERITING FROM zapcmd_cl_filelist
  CREATE PUBLIC.

  PUBLIC SECTION.

    METHODS constructor
      IMPORTING pf_type TYPE syucomm OPTIONAL
                pf_dir  TYPE string  OPTIONAL.

  PROTECTED SECTION.

  PRIVATE SECTION.

ENDCLASS.



CLASS zapcmd_cl_filelist_right IMPLEMENTATION.


  METHOD constructor.

    super->constructor( iv_side = gc_side-right
                        pf_type = pf_type
                        pf_dir  = pf_dir ).

  ENDMETHOD.


ENDCLASS.
