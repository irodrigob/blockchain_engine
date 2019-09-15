CLASS zcl_bc_a_header_block DEFINITION
  PUBLIC
  INHERITING FROM /bobf/cl_lib_a_supercl_simple
  FINAL
  CREATE PUBLIC .

  PUBLIC SECTION.

    METHODS /bobf/if_frw_action~execute
        REDEFINITION .
  PROTECTED SECTION.
    METHODS act_process_pow
      IMPORTING is_ctx                   TYPE /bobf/s_frw_ctx_act
                it_key                   TYPE /bobf/t_frw_key
                io_read                  TYPE REF TO /bobf/if_frw_read
                io_modify                TYPE REF TO /bobf/if_frw_modify
                is_parameters            TYPE REF TO data
      EXPORTING !eo_message              TYPE REF TO /bobf/if_frw_message
                !et_failed_key           TYPE /bobf/t_frw_key
                !ev_static_action_failed TYPE abap_bool
                !et_data                 TYPE INDEX TABLE
      RAISING   /bobf/cx_frw.
    METHODS calculate_pow
      CHANGING
        co_blocks TYPE REF TO zbc_sc_bo_block .
    METHODS process_pow
      IMPORTING io_modify     TYPE REF TO /bobf/if_frw_modify
                is_parameters TYPE zbc_s_params_process_pow
                iv_id_block   TYPE zbc_e_id_block
      CHANGING
                ct_blocks     TYPE zbc_i_bo_block
      RAISING   zcx_bc.
    METHODS get_start_block
      IMPORTING
                it_blocks     TYPE zbc_i_bo_block
                is_parameters TYPE zbc_s_params_process_pow
                iv_id_block   TYPE zbc_e_id_block
      EXPORTING
                ev_index      TYPE sytabix
      RAISING   zcx_bc.
  PRIVATE SECTION.
ENDCLASS.



CLASS zcl_bc_a_header_block IMPLEMENTATION.


  METHOD /bobf/if_frw_action~execute.

    CASE is_ctx-act_key.
      WHEN zif_bc_bo_blockchain_c=>sc_action-root-process_pow.
        act_process_pow(
          EXPORTING
            is_ctx                  = is_ctx
            it_key                  =  it_key
            io_read                 =  io_read
            io_modify               =  io_modify
            is_parameters           =  is_parameters
          IMPORTING
            eo_message              = eo_message
            et_failed_key           = et_failed_key
            ev_static_action_failed = ev_static_action_failed
            et_data                 = et_data ).

    ENDCASE.
  ENDMETHOD.
  METHOD act_process_pow.
    FIELD-SYMBOLS <ls_params> TYPE zbc_s_params_process_pow.
    DATA lt_blocks TYPE zbc_i_bo_block.
    DATA lt_header TYPE zbc_i_bo_header.

    IF eo_message IS NOT BOUND. " Se instancia la clase de mensajes en caso de no estarlo
      eo_message = /bobf/cl_frw_factory=>get_message( ).
    ENDIF.

    " Recuperamos la cabecera
    io_read->retrieve(
      EXPORTING
        iv_node                 = zif_bc_bo_blockchain_c=>sc_node-root
        it_key                  = it_key
        iv_fill_data            = abap_true
        it_requested_attributes = VALUE #( ( zif_bc_bo_blockchain_c=>sc_node_attribute-root-id_block ) )
      IMPORTING
        et_data                 = lt_header ).


    " Se recupera los datos del bloque
    io_read->retrieve_by_association(
  EXPORTING
    iv_node                 =   zif_bc_bo_blockchain_c=>sc_node-root
    it_key                  = it_key
    iv_association          = zif_bc_bo_blockchain_c=>sc_association-root-block
    iv_fill_data            = abap_true
  IMPORTING
    et_data                 = lt_blocks ).

    " Paso los parámetros a una estructura para poderla pasar por parámetro
    ASSIGN is_parameters->* TO <ls_params>.

    " Se recorre cada registro de cabecera
    LOOP AT lt_header ASSIGNING FIELD-SYMBOL(<ls_header>).

      " Por cada registro se obtiene sus datos del bloque
      DATA(lt_blocks_filter) = VALUE zbc_i_bo_block( FOR <ls_blocks_filt> IN lt_blocks WHERE ( parent_key = <ls_header>-key  ) ( <ls_blocks_filt> ) ).

      " Ordeno los bloques para tener el indice de mayor a menor
      SORT lt_blocks_filter BY index_block ASCENDING.

      TRY.
          " Se lanza el proceso del proof at work
          process_pow( EXPORTING io_modify = io_modify
                                 is_parameters = <ls_params>
                                 iv_id_block = <ls_header>-id_block
                       CHANGING ct_blocks = lt_blocks_filter ).

        CATCH zcx_bc INTO DATA(lx_bc).

          eo_message->add_message(
            EXPORTING
              is_msg       = VALUE #( msgty = zif_bc_data=>cs_message-error
                                      msgid = lx_bc->if_t100_message~t100key-msgid
                                      msgno = lx_bc->if_t100_message~t100key-msgno
                                      msgv1 = lx_bc->mv_attr1 )
              iv_node = is_ctx-node_key
              iv_key = <ls_header>-key ).

      ENDTRY.

      " Se devuelve los datos del bloque
      INSERT LINES OF lt_blocks_filter INTO TABLE et_data.
      CLEAR lt_blocks_filter.

    ENDLOOP.

  ENDMETHOD.

  METHOD calculate_pow.
    co_blocks->nonce = zif_bc_data=>cs_blockchain-nonce_initial. " Nonce initial.

    " Se cálculo el offset que habrá que usar para chequear si el proof at work se cumple
    DATA(lv_offset_pow) = strlen( zif_bc_data=>cs_blockchain-proof_at_work ).



    " Mientra no se encuentra el proceso se irá calculalando
    DATA(lv_pow_found) = abap_false.
    WHILE lv_pow_found = abap_false.

      co_blocks->hash = zcl_bc_blockchain=>calculate_hash( is_values = VALUE #( id_block = co_blocks->id_block
                                                                                 prev_hash = co_blocks->prev_hash
                                                                                 index_block = co_blocks->index_block
                                                                                 data = co_blocks->data
                                                                                 timestamp = co_blocks->timestamp
                                                                                 nonce = co_blocks->nonce
                                                                                 id_request = co_blocks->id_request ) ).

      " Se compara si el hash obtenido cumple con el requisito. Si no es así, se añade un valor al nonce y se vuelve a calcular.
      " Si se obtiene se sale del proceso
      IF co_blocks->hash(lv_offset_pow) = zif_bc_data=>cs_blockchain-proof_at_work.
        lv_pow_found = abap_true.
      ELSE.
        co_blocks->nonce = co_blocks->nonce + 1.
      ENDIF.

    ENDWHILE.
  ENDMETHOD.


  METHOD process_pow.


    " Se determina a partir de que bloque hay que comenzar. EL motivo es que se según los parámetros
    " pueden comenzar desde en uno concreto, desde el inicio o solo los nuevos.
    get_start_block( EXPORTING it_blocks = ct_blocks
                               is_parameters = is_parameters
                               iv_id_block = iv_id_block
                     IMPORTING ev_index = DATA(lv_index) ).

    " Se calcula el indice anterior al devuelto para obtener el hash previo. Si se comienza desde el principio puede el
    " hash previo será el de genesis block que no se usa para calcular el PoW
    DATA(lv_prev_index) = COND #( WHEN lv_index = 1 THEN 1 ELSE ( lv_index - 1 ) ).
    READ TABLE ct_blocks ASSIGNING FIELD-SYMBOL(<ls_prev_block>) INDEX lv_prev_index.
    DATA(lv_prev_hash) = <ls_prev_block>-hash.


    LOOP AT ct_blocks REFERENCE INTO DATA(lo_blocks) FROM lv_index.

      " El genesis block no se realiza ningún cálculo, ya tiene un hash hecho cuando se creo, que se usará como hash previo
      " para el siguiente bloque
      IF lo_blocks->index_block NE zif_bc_data=>cs_blockchain-index_block_initial.

        " Se hace resetea el tiempo para saber el momento exacto que comienza el proceso el PoW.
        " El objetivo es tener un log de cuanto tarda el proceso de obtención.
        GET TIME.
        lo_blocks->pow_init_date = sy-datum.
        lo_blocks->pow_init_time = sy-uzeit.

        lo_blocks->prev_hash = lv_prev_hash. " Se guarda el hash del registro anterior

        " Calculo del Pow
        calculate_pow( CHANGING co_blocks = lo_blocks ).

        " Una vez calculado se hace la foto de cuando ha terminado
        GET TIME.
        lo_blocks->pow_end_date = sy-datum.
        lo_blocks->pow_end_time = sy-uzeit.

        lv_prev_hash = lo_blocks->hash. " Se guarda el hash previo

        " Solo si por parámetro se indica se actualizan los datos del bloque a nivel interno
        IF is_parameters-modify_block = abap_true.
          io_modify->update( EXPORTING iv_node           = zif_bc_bo_blockchain_c=>sc_node-block
                                       iv_key            = lo_blocks->key
                                       is_data           = lo_blocks ).
        ENDIF.
      ENDIF.

    ENDLOOP.
  ENDMETHOD.


  METHOD get_start_block.

    ev_index = 0.

    " Si no se comienza por un id de petición ni tampoco se quiere procesos solos los nuevos bloques, lo que se hace
    " es empezar desde el inciio
    IF is_parameters-id_request IS INITIAL AND is_parameters-only_new_blocks = abap_false.
      ev_index = 1.
      " Si se indica un ID de petición se busca y se obtiene su posición
    ELSEIF is_parameters-id_request IS NOT INITIAL.
      READ TABLE it_blocks ASSIGNING FIELD-SYMBOL(<ls_block>) WITH KEY id_request = is_parameters-id_request.
      IF sy-subrc = 0.
        " Si el el bloque donde esta el ID informado no tiene el previus hash calculado entonces se lanza una excepción
        " porque significa que no se puede calcular el hash sin tener el hash del registro previo calculado
        IF <ls_block>-prev_hash IS NOT INITIAL.
          ev_index = sy-tabix.
        ELSE.
          RAISE EXCEPTION TYPE zcx_bc
            EXPORTING
              textid   = zcx_bc=>id_request_no_prev_hash
              mv_attr1 = CONV #( is_parameters-id_request ).
        ENDIF.
      ELSE. " Si no existe se lanza excepcion
        RAISE EXCEPTION TYPE zcx_bc
          EXPORTING
            textid   = zcx_bc=>id_request_not_exist
            mv_attr1 = CONV #( is_parameters-id_request ).
      ENDIF.
      " Si solo se quiere los nuevos bloques se busca el primer registro que tenga
      " el hash en blanco
    ELSEIF is_parameters-only_new_blocks = abap_true.

      LOOP AT it_blocks TRANSPORTING NO FIELDS WHERE hash IS INITIAL.
        ev_index = sy-tabix.
        EXIT.
      ENDLOOP.
      IF sy-subrc NE 0. " Si no hay ninguno se lanza excepción.
        RAISE EXCEPTION TYPE zcx_bc
          EXPORTING
            textid   = zcx_bc=>not_determine_start_block
            mv_attr1 = CONV #( iv_id_block ).
      ENDIF.
    ENDIF.

  ENDMETHOD.

ENDCLASS.
