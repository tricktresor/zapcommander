class ZCX_ZAPCMD_ERROR definition
  public
  inheriting from CX_STATIC_CHECK
  final
  create public .

*"* public components of class ZCX_ZAPCMD_ERROR
*"* do not include other source files here!!!
public section.

  interfaces IF_T100_MESSAGE .

  methods CONSTRUCTOR
    importing
      !TEXTID like IF_T100_MESSAGE=>T100KEY optional
      !PREVIOUS like PREVIOUS optional .
protected section.
*"* protected components of class ZCX_ZAPCMD_ERROR
*"* do not include other source files here!!!
private section.
*"* private components of class ZCX_ZAPCMD_ERROR
*"* do not include other source files here!!!
ENDCLASS.



CLASS ZCX_ZAPCMD_ERROR IMPLEMENTATION.


method CONSTRUCTOR.
CALL METHOD SUPER->CONSTRUCTOR
EXPORTING
PREVIOUS = PREVIOUS
.
clear me->textid.
if textid is initial and ME->IF_T100_MESSAGE~T100KEY IS INITIAL.
  IF_T100_MESSAGE~T100KEY = IF_T100_MESSAGE=>DEFAULT_TEXTID.
else.
  IF_T100_MESSAGE~T100KEY = TEXTID.
endif.
endmethod.
ENDCLASS.
