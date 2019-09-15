CLASS zcl_bc_d_data_block DEFINITION
  PUBLIC
  INHERITING FROM /bobf/cl_lib_d_supercl_simple
  FINAL
  CREATE PUBLIC .

  PUBLIC SECTION.

    METHODS /bobf/if_frw_determination~execute
        REDEFINITION .
  PROTECTED SECTION.
    METHODS new_block
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
    METHODS calculate_diff_pow
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



CLASS zcl_bc_d_data_block IMPLEMENTATION.


  METHOD /bobf/if_frw_determination~execute.
    CASE is_ctx-det_key.
      WHEN zif_bc_bo_blockchain_c=>sc_determination-block-new_block.
        new_block(  EXPORTING is_ctx  = is_ctx
                                it_key  = it_key
                                io_read  = io_read
                                io_modify = io_modify
                       IMPORTING eo_message  = eo_message
                                 et_failed_key  = et_failed_key ).
      WHEN zif_bc_bo_blockchain_c=>sc_determination-block-calculate_diff_pow.
        calculate_diff_pow(  EXPORTING is_ctx  = is_ctx
                          it_key  = it_key
                          io_read  = io_read
                          io_modify = io_modify
                 IMPORTING eo_message  = eo_message
                           et_failed_key  = et_failed_key ).
    ENDCASE.
  ENDMETHOD.

  METHOD new_block.
    DATA lt_root TYPE zbc_i_bo_header.
    DATA lt_data TYPE zbc_i_bo_block.

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


      " Se recupera el ID del block de nodo padre
      CLEAR lt_root.
      io_read->retrieve_by_association(
        EXPORTING
          iv_node                 =   zif_bc_bo_blockchain_c=>sc_node-block
          it_key                  = VALUE #( ( key = lo_data->key ) )
          iv_association          = zif_bc_bo_blockchain_c=>sc_association-block-to_root
          iv_fill_data            = abap_true
          it_requested_attributes = VALUE #( ( zif_bc_bo_blockchain_c=>sc_node_attribute-root-id_block ) )
        IMPORTING
          et_data                 = lt_root ).

      " Tiene que a ver registro padre, si no existe que pegue dump porque estaríamos en un caso muy raro.
      READ TABLE lt_root ASSIGNING FIELD-SYMBOL(<ls_root>) INDEX 1.
      lo_data->id_block = <ls_root>-id_block.
      UNASSIGN <ls_root>.

      " Si el index del bloque es el inicial se calcula el hash inicial. El motivo es que se necesita el ID que en el momento de la creación
      " del bloque porque se determina en la cabecera. En el siguiente bloque ese ID ya se podrá recuperar de la cabecera y utilizarlo para
      " calcularlo.
      IF lo_data->index_block = zif_bc_data=>cs_blockchain-index_block_initial.
        lo_data->hash = zcl_bc_blockchain=>calculate_hash( is_values = VALUE #( id_block = lo_data->id_block
                                                                                 index_block = lo_data->index_block
                                                                                 data = lo_data->data
                                                                                 timestamp = lo_data->timestamp
                                                                                 nonce = lo_data->nonce
                                                                                 id_request = lo_data->id_request ) ).
      ENDIF.

      " Se graban los datos en el nodo
      io_modify->update( iv_node = is_ctx-node_key
                         iv_key = lo_data->key
                         is_data = lo_data ).

    ENDLOOP.
  ENDMETHOD.


  METHOD calculate_diff_pow.
    DATA lt_data TYPE zbc_i_bo_block.

    " Recuperación de los datos
    io_read->retrieve(
                 EXPORTING
                      iv_node = is_ctx-node_key
                      it_key = it_key
                 IMPORTING
                      et_data = lt_data ).

    " Por cada registro se calcula en segundos cuanto ha tardado el proceso de calculo
    LOOP AT lt_data REFERENCE INTO DATA(lo_data).

      " El genesis block no se calcula al ser el inicia
      IF lo_data->index_block NE zif_bc_data=>cs_blockchain-index_block_initial.
        zcl_bc_util=>datetime_diff( EXPORTING iv_date_from = lo_data->pow_init_date
                                              iv_time_from    = lo_data->pow_init_time
                                              iv_date_to      = lo_data->pow_end_date
                                              iv_time_to      = lo_data->pow_end_time
                                    IMPORTING ev_diff_seconds = lo_data->pow_diff_seconds ).

      ENDIF.

      " Se graban los datos en el nodo
      io_modify->update( iv_node = is_ctx-node_key
                         iv_key = lo_data->key
                         is_data = lo_data ).

    ENDLOOP.
  ENDMETHOD.

ENDCLASS.
