class ZCX_BC definition
  public
  inheriting from CX_STATIC_CHECK
  final
  create public .

public section.

  interfaces IF_T100_DYN_MSG .
  interfaces IF_T100_MESSAGE .

  constants:
    begin of TYPE_DATA_NOT_VALID,
      msgid type symsgid value 'ZBC',
      msgno type symsgno value '001',
      attr1 type scx_attrname value '',
      attr2 type scx_attrname value '',
      attr3 type scx_attrname value '',
      attr4 type scx_attrname value '',
    end of TYPE_DATA_NOT_VALID .
  constants:
    begin of GET_ID,
      msgid type symsgid value 'ZBC',
      msgno type symsgno value '002',
      attr1 type scx_attrname value '',
      attr2 type scx_attrname value '',
      attr3 type scx_attrname value '',
      attr4 type scx_attrname value '',
    end of GET_ID .
  constants:
    begin of PROCESS_BLOCKCHAIN_RUNNING,
      msgid type symsgid value 'ZBC',
      msgno type symsgno value '004',
      attr1 type scx_attrname value '',
      attr2 type scx_attrname value '',
      attr3 type scx_attrname value '',
      attr4 type scx_attrname value '',
    end of PROCESS_BLOCKCHAIN_RUNNING .
  constants:
    begin of ID_REQUEST_NOT_EXIST,
      msgid type symsgid value 'ZBC',
      msgno type symsgno value '006',
      attr1 type scx_attrname value 'MV_ATTR1',
      attr2 type scx_attrname value '',
      attr3 type scx_attrname value '',
      attr4 type scx_attrname value '',
    end of ID_REQUEST_NOT_EXIST .
  constants:
    begin of NOT_DETERMINE_START_BLOCK,
      msgid type symsgid value 'ZBC',
      msgno type symsgno value '010',
      attr1 type scx_attrname value '',
      attr2 type scx_attrname value '',
      attr3 type scx_attrname value '',
      attr4 type scx_attrname value '',
    end of NOT_DETERMINE_START_BLOCK .
  constants:
    begin of ID_REQUEST_NO_PREV_HASH,
      msgid type symsgid value 'ZBC',
      msgno type symsgno value '011',
      attr1 type scx_attrname value 'MV_ATTR1',
      attr2 type scx_attrname value '',
      attr3 type scx_attrname value '',
      attr4 type scx_attrname value '',
    end of ID_REQUEST_NO_PREV_HASH .
  class-data MV_ATTR1 type STRING .

  methods CONSTRUCTOR
    importing
      !TEXTID like IF_T100_MESSAGE=>T100KEY optional
      !PREVIOUS like PREVIOUS optional
      !MV_ATTR1 type STRING optional .
protected section.
private section.
ENDCLASS.



CLASS ZCX_BC IMPLEMENTATION.


  method CONSTRUCTOR.
CALL METHOD SUPER->CONSTRUCTOR
EXPORTING
PREVIOUS = PREVIOUS
.
me->MV_ATTR1 = MV_ATTR1 .
clear me->textid.
if textid is initial.
  IF_T100_MESSAGE~T100KEY = IF_T100_MESSAGE=>DEFAULT_TEXTID.
else.
  IF_T100_MESSAGE~T100KEY = TEXTID.
endif.
  endmethod.
ENDCLASS.
