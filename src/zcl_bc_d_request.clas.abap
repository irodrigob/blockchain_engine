CLASS zcl_bc_d_request DEFINITION
  PUBLIC
  INHERITING FROM /bobf/cl_lib_d_supercl_simple
  FINAL
  CREATE PUBLIC .

  PUBLIC SECTION.

    METHODS /bobf/if_frw_determination~execute
        REDEFINITION .
  PROTECTED SECTION.
    METHODS new_request
      IMPORTING
        is_ctx        TYPE /bobf/s_frw_ctx_det
        it_key        TYPE /bobf/t_frw_key
        io_read       TYPE REF TO /bobf/if_frw_read
        io_modify     TYPE REF TO /bobf/if_frw_modify
      EXPORTING
        eo_message    TYPE REF TO /bobf/if_frw_message
        et_failed_key TYPE /bobf/t_frw_key
      RAISING
        /bobf/cx_frw.
  PRIVATE SECTION.
ENDCLASS.



CLASS zcl_bc_d_request IMPLEMENTATION.


  METHOD /bobf/if_frw_determination~execute.

    CASE is_ctx-det_key.
      WHEN zif_bc_ddl_request_c=>sc_determination-zbc_ddl_request-new_request.
        new_request(  EXPORTING is_ctx  = is_ctx
                                it_key  = it_key
                                io_read  = io_read
                                io_modify = io_modify
                       IMPORTING eo_message  = eo_message
                                 et_failed_key  = et_failed_key ).
    ENDCASE.
  ENDMETHOD.

  METHOD new_request.
    DATA lt_data TYPE ztbcddl_request.

    IF eo_message IS NOT BOUND. " Se instancia la clase de mensajes en caso de no estarlo
      eo_message = /bobf/cl_frw_factory=>get_message( ).
    ENDIF.

    " Recuperación de los datos
    io_read->retrieve(
                 EXPORTING
                      iv_node = is_ctx-node_key
                      it_key = it_key
                 IMPORTING
                      et_data = lt_data ).

    " Por cada registro
    LOOP AT lt_data REFERENCE INTO DATA(lo_data).

      " Se determinan los otros campos
      lo_data->erdat = sy-datum.
      lo_data->ernam = sy-uname.
      lo_data->erzet = sy-uzeit.


      " Se graban los datos en el nodo
      io_modify->update( iv_node = is_ctx-node_key
                         iv_key = lo_data->key
                         is_data = lo_data ).

    ENDLOOP.
  ENDMETHOD.

ENDCLASS.
