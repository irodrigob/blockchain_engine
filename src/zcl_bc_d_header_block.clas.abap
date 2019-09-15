CLASS zcl_bc_d_header_block DEFINITION
  PUBLIC
  INHERITING FROM /bobf/cl_lib_d_supercl_simple
  FINAL
  CREATE PUBLIC .

  PUBLIC SECTION.

    METHODS /bobf/if_frw_determination~execute
        REDEFINITION .
  PROTECTED SECTION.
    METHODS new_header
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
    METHODS update_header
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
    METHODS get_text
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
    METHODS add_execution_log
      IMPORTING
        iv_key      TYPE zbc_sc_bo_header-key
        iv_id_block TYPE zbc_e_id_block
        io_modify   TYPE REF TO /bobf/if_frw_modify.
  PRIVATE SECTION.

ENDCLASS.



CLASS zcl_bc_d_header_block IMPLEMENTATION.


  METHOD /bobf/if_frw_determination~execute.
    CASE is_ctx-det_key.
      WHEN zif_bc_bo_blockchain_c=>sc_determination-root-new_header.
        new_header(  EXPORTING is_ctx  = is_ctx
                                it_key  = it_key
                                io_read  = io_read
                                io_modify = io_modify
                       IMPORTING eo_message  = eo_message
                                 et_failed_key  = et_failed_key ).
      WHEN zif_bc_bo_blockchain_c=>sc_determination-root-update_header.
        update_header(  EXPORTING is_ctx  = is_ctx
                                it_key  = it_key
                                io_read  = io_read
                                io_modify = io_modify
                       IMPORTING eo_message  = eo_message
                                 et_failed_key  = et_failed_key ).
      WHEN zif_bc_bo_blockchain_c=>sc_determination-root-get_text.
        get_text(  EXPORTING is_ctx  = is_ctx
                                it_key  = it_key
                                io_read  = io_read
                                io_modify = io_modify
                       IMPORTING eo_message  = eo_message
                                 et_failed_key  = et_failed_key ).
    ENDCASE.
  ENDMETHOD.

  METHOD new_header.
    DATA lt_data TYPE zbc_i_bo_header.

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

      " Se informa de las fechas de creación y modificacion
      lo_data->aedat = lo_data->erdat = sy-datum.
      lo_data->auname = lo_data->ernam = sy-uname.
      lo_data->aetime = lo_data->erzet = sy-uzeit.


      TRY.
          lo_data->id_block = zcl_bc_util=>get_id_block(  ). " Se obtiene el ID de bloque
        CATCH zcx_bc INTO DATA(lx_bc).
          eo_message->add_exception( lx_bc ).
          INSERT VALUE #( key = lo_data->key ) INTO TABLE et_failed_key.
      ENDTRY.

      " Se graban los datos en el nodo
      io_modify->update( iv_node = is_ctx-node_key
                         iv_key = lo_data->key
                         is_data = lo_data ).

    ENDLOOP.
  ENDMETHOD.

  METHOD update_header.
    DATA lt_data TYPE zbc_i_bo_header.

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

      " Se informa de las fechas de modificacion
      lo_data->aedat = sy-datum.
      lo_data->auname = sy-uname.
      lo_data->aetime = sy-uzeit.

      " Si el status es en "In proces" significa que se va ejecutar el PoW y por lo tanto
      " añado un registro en log de ejecución. Dentro del nodo de log de ejecución se determina
      " fecha, hora, usuario y datos del job(si se ejecuta en fondo)
      IF lo_data->status = zif_bc_data=>cs_status_block-in_process.
        add_execution_log( EXPORTING iv_key = lo_data->key
                                     iv_id_block = lo_data->id_block
                                     io_modify = io_modify ).
      ENDIF.

      " Se graban los datos en el nodo
      io_modify->update( iv_node = is_ctx-node_key
                         iv_key = lo_data->key
                         is_data = lo_data ).

    ENDLOOP.
  ENDMETHOD.

  METHOD get_text.
    DATA lt_data TYPE zbc_i_bo_header.

    " Recuperación de los datos
    io_read->retrieve(
                 EXPORTING
                      iv_node = is_ctx-node_key
                      it_key = it_key
                 IMPORTING
                      et_data = lt_data ).

    IF lt_data IS NOT INITIAL.
      " Como los status son tres, a día de escribir este código, leo los tres de golpes sin
      " filtros y sin nada.
      SELECT domvalue_l, ddtext INTO TABLE @DATA(lt_status_text)
             FROM dd07t
             WHERE domname = @zif_bc_data=>cs_domain_with_text-status
                   AND ddlanguage = @sy-langu.

      " Por cada registro
      LOOP AT lt_data REFERENCE INTO DATA(lo_data).

        READ TABLE lt_status_text ASSIGNING FIELD-SYMBOL(<ls_status_text>) WITH KEY domvalue_l = lo_data->status.
        IF sy-subrc = 0.
          lo_data->status_text = <ls_status_text>-ddtext.
        ENDIF.

        " Se graban los datos en el nodo
        io_modify->update( iv_node = is_ctx-node_key
                           iv_key = lo_data->key
                           is_data = lo_data ).

      ENDLOOP.

    ENDIF.
  ENDMETHOD.


  METHOD add_execution_log.

    DATA(lo_data) = NEW zbc_sc_bo_execution_log(  ).

    " Solo se informa la clave del nodo y su bloque
    lo_data->key = /bobf/cl_frw_factory=>get_new_key( ).
    lo_data->id_block = iv_id_block.

    io_modify->create(
      EXPORTING
        iv_node            = zif_bc_bo_blockchain_c=>sc_node-execution_log
        iv_key             = lo_data->key
        is_data            = lo_data
        iv_assoc_key       = zif_bc_bo_blockchain_c=>sc_association-root-execution_log
        iv_source_node_key = zif_bc_bo_blockchain_c=>sc_node-root
        iv_source_key      = iv_key
        iv_root_key        = iv_key
      IMPORTING
        ev_key             = DATA(lv_key) ).


  ENDMETHOD.

ENDCLASS.
