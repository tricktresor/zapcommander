*"* components of interface ZAPCMD_IF_FACTORY
interface ZAPCMD_IF_FACTORY
  public .


  methods GET_BUTTON
    returning
      value(ET_BUTTON) type TTB_BUTTON .
  methods CREATE_DIR
    importing
      !I_FCODE type SYUCOMM
      !I_DIR type CSEQUENCE optional
    returning
      value(EO_DIR) type ref to ZAPCMD_CL_DIR .
endinterface.
