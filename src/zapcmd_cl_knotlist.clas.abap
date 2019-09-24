class ZAPCMD_CL_KNOTLIST definition
  public
  create public .

*"* public components of class ZAPCMD_CL_KNOTLIST
*"* do not include other source files here!!!
public section.

  data CT_LIST type ZAPCMD_TBL_FILELIST .

  methods ADD
    importing
      !PF_REF_KNOT type ref to ZAPCMD_CL_KNOT .
protected section.
*"* protected components of class ZAPCMD_CL_KNOTLIST
*"* do not include other source files here!!!
private section.
*"* private components of class ZAPCMD_CL_KNOTLIST
*"* do not include other source files here!!!
ENDCLASS.



CLASS ZAPCMD_CL_KNOTLIST IMPLEMENTATION.


method ADD.

  append pf_ref_knot to ct_list.

endmethod.
ENDCLASS.
