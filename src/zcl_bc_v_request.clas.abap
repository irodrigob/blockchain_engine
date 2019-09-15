CLASS zcl_bc_v_request DEFINITION
  PUBLIC
  INHERITING FROM /bobf/cl_lib_v_supercl_simple
  FINAL
  CREATE PUBLIC .

  PUBLIC SECTION.

    METHODS /bobf/if_frw_validation~execute
        REDEFINITION .
  PROTECTED SECTION.
    METHODS new_request
      IMPORTING
                is_ctx        TYPE /bobf/s_frw_ctx_val
                it_key        TYPE /bobf/t_frw_key
                io_read       TYPE REF TO /bobf/if_frw_read
      EXPORTING
                eo_message    TYPE REF TO /bobf/if_frw_message
                et_failed_key TYPE /bobf/t_frw_key
      RAISING   /bobf/cx_frw.
  PRIVATE SECTION.
ENDCLASS.



CLASS zcl_bc_v_request IMPLEMENTATION.


  METHOD /bobf/if_frw_validation~execute.
    CASE is_ctx-val_key.
      WHEN zif_bc_ddl_request_c=>sc_validation-zbc_ddl_request-new_request.
        new_request(  EXPORTING is_ctx  = is_ctx
                                    it_key  = it_key
                                    io_read  = io_read
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


    LOOP AT lt_data REFERENCE INTO DATA(lo_data).

      IF lo_data->id_request IS INITIAL. " Si no hay ID Request se lanza un mensaje de error
        eo_message->add_message(
          EXPORTING
            is_msg       = VALUE #( msgty = zif_bc_data=>cs_message-error msgid = zif_bc_data=>cs_message-id msgno = '003' )
            iv_node      = is_ctx-node_key
            iv_key       = lo_data->key ).
      ENDIF.
    ENDLOOP.
  ENDMETHOD.

ENDCLASS.
