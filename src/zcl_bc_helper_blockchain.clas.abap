CLASS zcl_bc_helper_blockchain DEFINITION
  PUBLIC
  FINAL
  CREATE PUBLIC .

  PUBLIC SECTION.
    TYPES: tt_id_block TYPE STANDARD TABLE OF zbc_e_id_block WITH EMPTY KEY.
    TYPES: ts_blocks TYPE zbc_sc_bo_block.
    TYPES: tt_blocks TYPE STANDARD TABLE OF ts_blocks WITH EMPTY KEY.
    TYPES: ts_header TYPE zbc_sc_bo_header.
    TYPES: BEGIN OF ts_monitor_selparam,
             node TYPE /bobf/obm_node_key.
             INCLUDE TYPE /bobf/s_frw_query_selparam.
           TYPES:
                  END OF ts_monitor_selparam.
    TYPES: tt_monitor_selparam TYPE STANDARD TABLE OF ts_monitor_selparam WITH EMPTY KEY.
    METHODS constructor
      IMPORTING
        iv_langu TYPE sylangu DEFAULT sy-langu.
    METHODS get_header_block_by_id
      IMPORTING
        iv_id_block    TYPE zbc_e_id_block
        iv_only_header TYPE sap_bool DEFAULT abap_false
      EXPORTING
        es_header      TYPE zcl_bc_helper_blockchain=>ts_header
        et_blocks      TYPE zcl_bc_helper_blockchain=>tt_blocks.
    METHODS get_header_block_active
      EXPORTING
        es_header TYPE zcl_bc_helper_blockchain=>ts_header
        et_blocks TYPE zcl_bc_helper_blockchain=>tt_blocks.
    METHODS get_header_blocks
      IMPORTING it_params      TYPE /bobf/t_frw_query_selparam
                iv_only_header TYPE sap_bool DEFAULT abap_false
      EXPORTING
                es_header      TYPE zcl_bc_helper_blockchain=>ts_header
                et_blocks      TYPE zcl_bc_helper_blockchain=>tt_blocks.
    METHODS get_data_monitor
      IMPORTING it_params        TYPE zcl_bc_helper_blockchain=>tt_monitor_selparam
      EXPORTING et_header        TYPE zbc_i_bo_header
                et_blocks        TYPE zbc_i_bo_block
                et_execution_log TYPE zbc_i_bo_execution_log.

    METHODS new_blockchain
      EXPORTING
        es_header TYPE zcl_bc_helper_blockchain=>ts_header
        et_blocks TYPE zcl_bc_helper_blockchain=>tt_blocks
        et_return TYPE bapiret2_t.
    METHODS update_blockchain
      IMPORTING
        iv_do_save TYPE sap_bool DEFAULT abap_true
      EXPORTING
        et_return  TYPE bapiret2_t
      CHANGING
        cs_header  TYPE zcl_bc_helper_blockchain=>ts_header
        ct_blocks  TYPE zcl_bc_helper_blockchain=>tt_blocks OPTIONAL.
    METHODS calculate_pow
      IMPORTING
        iv_key             TYPE /bobf/conf_key OPTIONAL
        iv_id_block        TYPE zbc_e_id_block OPTIONAL
        iv_only_new_blocks TYPE sap_bool DEFAULT abap_true
        iv_modify_block    TYPE sap_bool DEFAULT abap_true
      EXPORTING
        et_blocks          TYPE zcl_bc_helper_blockchain=>tt_blocks
        et_return          TYPE bapiret2_t.
    METHODS get_data_block_by_id_request
      IMPORTING
        iv_id_request     TYPE zbc_e_id_request
        iv_get_all_blocks TYPE sap_bool DEFAULT abap_false
      EXPORTING
        es_header         TYPE zcl_bc_helper_blockchain=>ts_header
        et_blocks         TYPE zcl_bc_helper_blockchain=>tt_blocks.
  PROTECTED SECTION.
    DATA mo_svc_mngr TYPE REF TO /bobf/if_tra_service_manager.
    DATA mo_txn_mngr TYPE REF TO /bobf/if_tra_transaction_mgr.
    DATA mo_conf_mngr TYPE REF TO /bobf/if_frw_configuration.

    DATA mv_langu TYPE sylangu.
    METHODS get_data_monitor_block
      IMPORTING
        it_selparam TYPE /bobf/t_frw_query_selparam
      EXPORTING
        et_blocks   TYPE zbc_i_bo_block
        et_header   TYPE zbc_i_bo_header.
    METHODS get_data_monitor_header
      IMPORTING
        it_selparam TYPE /bobf/t_frw_query_selparam
      EXPORTING
        et_header   TYPE zbc_i_bo_header.
  PRIVATE SECTION.
ENDCLASS.



CLASS zcl_bc_helper_blockchain IMPLEMENTATION.
  METHOD constructor.
    mv_langu = sy-langu.

    " Se instancian las clases del BOPF
    TRY.
        " Inicialización del gestor transaccional actualizaciones, bloqueos, etc..
        mo_txn_mngr = /bobf/cl_tra_trans_mgr_factory=>get_transaction_manager( ).

        " Creación del el gestor de servicios del BOPF. Permite realizar las llamadas al BOPF para ejecutar validaciones, acciones, añadir, etc..
        " Es la clase más importante ya que toda la gestión CRUD se realiza en esta clase
        mo_svc_mngr = /bobf/cl_tra_serv_mgr_factory=>get_service_manager( zif_bc_bo_blockchain_c=>sc_bo_key ).

        " Creación de la configuración del BOPF, permite obtener los metadas del BOPF
        mo_conf_mngr = /bobf/cl_frw_factory=>get_configuration( zif_bc_bo_blockchain_c=>sc_bo_key ).

      CATCH /bobf/cx_frw.
        "TODO: Error handling...
    ENDTRY.

  ENDMETHOD.

  METHOD get_header_block_active.

    CLEAR: es_header, et_blocks.


    " Buscamos el primer registro con el status activo
    DATA(lt_params) = VALUE /bobf/t_frw_query_selparam( ( attribute_name = zif_bc_bo_blockchain_c=>sc_query_attribute-root-select_by_elements-status
                                                          sign = 'I'
                                                          option = 'EQ'
                                                          low = zif_bc_data=>cs_status_block-active ) ).

    get_header_blocks(
   EXPORTING
     it_params      = lt_params
   IMPORTING
     es_header      = es_header
     et_blocks      = et_blocks ).

  ENDMETHOD.


  METHOD new_blockchain.
    DATA lt_mod TYPE /bobf/t_frw_modification.

    CLEAR: et_return, es_header, et_blocks.

    " Se rellenan las estructuras de salida que luego se usarán para guardar en el BOPF

    TRY.

        " Se añade los datos de cabecera
        DATA(lo_header) = NEW zbc_sc_bo_header(  ).

        " En la cabecera solo se añade la clave porque el resto de campos se determinan
        lo_header->key = /bobf/cl_frw_factory=>get_new_key( ).
        lo_header->status = zif_bc_data=>cs_status_block-active. " Status activo por defeto

        " Se añade el registro a la tabla de modificaciones
        INSERT VALUE #( node = zif_bc_bo_blockchain_c=>sc_node-root
                        change_mode = /bobf/if_frw_c=>sc_modify_create
                        key = lo_header->key
                        data = lo_header )
               INTO TABLE lt_mod.

        " Se añade el bloque inicial o llamado "genesis block"
        DATA(lo_block) = NEW zbc_sc_bo_block(  ).

        lo_block->key = /bobf/cl_frw_factory=>get_new_key( ).
        lo_block->index_block = zif_bc_data=>cs_blockchain-index_block_initial.
        lo_block->data = zif_bc_data=>cs_blockchain-data_genesis_block.
        GET TIME STAMP FIELD lo_block->timestamp.
        lo_block->nonce = zif_bc_data=>cs_blockchain-nonce_initial.

        " Nota el hash inicial se calcula dentro de la determinación del BOPF porque es donde el ID del bloque
        " se determina y es posible usarlo para hacer el cálculo



        INSERT VALUE #( node = zif_bc_bo_blockchain_c=>sc_node-block
                        change_mode = /bobf/if_frw_c=>sc_modify_create
                        source_node = zif_bc_bo_blockchain_c=>sc_node-root
                        association = zif_bc_bo_blockchain_c=>sc_association-root-block
                        source_key = lo_header->key
                        key = lo_block->key
                        data = lo_block )
               INTO TABLE lt_mod.


        mo_svc_mngr->modify(
          EXPORTING
            it_modification = lt_mod
          IMPORTING
            eo_change       = DATA(lo_change)
            eo_message      = DATA(lo_message) ).

        IF lo_message IS BOUND AND lo_message->check( ) EQ abap_true.

          zcl_bc_util=>conv_message_bopf_2_return( EXPORTING io_message = lo_message
                                                             iv_langu = mv_langu
                                                   CHANGING ct_return = et_return ).

        ELSE.

          mo_txn_mngr->save(
        IMPORTING
          ev_rejected            = DATA(lv_rejected)
          eo_message             = DATA(lo_message_txn) ).

          IF lv_rejected = abap_true. " Se ha producido un error.
            zcl_bc_util=>conv_message_bopf_2_return( EXPORTING io_message = lo_message_txn
                                                           iv_langu = mv_langu
                                                 CHANGING ct_return = et_return ).

          ENDIF.

        ENDIF.

      CATCH zcx_bc INTO DATA(lx_bc).
    ENDTRY.

  ENDMETHOD.

  METHOD update_blockchain.
    DATA lt_mod TYPE /bobf/t_frw_modification.
    DATA lo_header TYPE REF TO zbc_sc_bo_header.
    DATA lo_block TYPE REF TO  zbc_sc_bo_block.

    CLEAR: et_return.

* Primero hay que obtener los datos del BOPF para poder actualizar
    get_header_block_by_id(
      EXPORTING
        iv_id_block    = cs_header-id_block
      IMPORTING
        es_header = DATA(ls_header)
        et_blocks = DATA(lt_blocks) ).

    IF ls_header IS NOT INITIAL.

      " Se rellena los datos de cabecera
      GET REFERENCE OF ls_header INTO lo_header.
      " El BASE es para que no machaque los campos que solo existen en la estructura de la cabecera
      " del BOPF.
      ls_header = CORRESPONDING #( BASE ( ls_header ) cs_header ).

      INSERT VALUE #( node = zif_bc_bo_blockchain_c=>sc_node-root
                      change_mode = /bobf/if_frw_c=>sc_modify_update
                      key = lo_header->key
                      data = lo_header )
             INTO TABLE lt_mod.

      " Si los bloques se ha pasado se actualizan
      IF ct_blocks IS SUPPLIED.

        LOOP AT ct_blocks ASSIGNING FIELD-SYMBOL(<ls_blocks>).
          " Nos posicionamos en el bloque que hay en base de datos
          READ TABLE lt_blocks ASSIGNING FIELD-SYMBOL(<ls_block_bopf>) WITH KEY index_block = <ls_blocks>-index_block.
          IF sy-subrc = 0.
            DATA(lv_change_mode) = /bobf/if_frw_c=>sc_modify_update. " Se va actualizar
            " Se pasan los datos del bloque a la estructura del BOPF
            GET REFERENCE OF <ls_block_bopf> INTO lo_block.
            <ls_block_bopf> = CORRESPONDING #( BASE ( <ls_block_bopf> ) <ls_blocks> ).

          ELSE.
            " Creo un objeto nuevo para poder añadir los datos nuevos. Se tiene que hacer objeto nuevo
            " y no reaprovechar variables porque como al BOPF se le pasan referencias se pueden cruzar datos
            " debido a dichas referencias. Por eso, mejor es crear una referencia nueva para los datos a insertar
            CREATE DATA lo_block.
            ASSIGN lo_block->* TO <ls_block_bopf>.
            <ls_block_bopf>-key =  /bobf/cl_frw_factory=>get_new_key( ).
            <ls_block_bopf> = CORRESPONDING #( BASE ( <ls_block_bopf> ) <ls_blocks> ).
            lv_change_mode = /bobf/if_frw_c=>sc_modify_create. " Se va insertar
          ENDIF.

          INSERT VALUE #( node = zif_bc_bo_blockchain_c=>sc_node-block
                                  change_mode = lv_change_mode
                                  source_node = zif_bc_bo_blockchain_c=>sc_node-root
                                  association = zif_bc_bo_blockchain_c=>sc_association-root-block
                                  source_key = lo_header->key
                                  key = lo_block->key
                                  data = lo_block )
                         INTO TABLE lt_mod.

        ENDLOOP.

      ENDIF.

      mo_svc_mngr->modify( EXPORTING it_modification = lt_mod
                           IMPORTING eo_change       = DATA(lo_change)
                                     eo_message      = DATA(lo_message) ).

      IF lo_message IS BOUND AND lo_message->check( ) EQ abap_true.

        zcl_bc_util=>conv_message_bopf_2_return( EXPORTING io_message = lo_message
                                                           iv_langu = mv_langu
                                                 CHANGING ct_return = et_return ).

      ELSE.
        IF iv_do_save = abap_true.
          mo_txn_mngr->save(
        IMPORTING
          ev_rejected            = DATA(lv_rejected)
          eo_message             = DATA(lo_message_txn) ).

          IF lv_rejected = abap_true. " Se ha producido un error.
            zcl_bc_util=>conv_message_bopf_2_return( EXPORTING io_message = lo_message_txn
                                                           iv_langu = mv_langu
                                                 CHANGING ct_return = et_return ).
          ELSE.
            " Se recuperán de nuevo los datos por el ID introducido. El mótivo es que se tengan los campos
            " calculados actualizados
            CLEAR: ls_header, lt_blocks.
            get_header_block_by_id(
              EXPORTING
                iv_id_block    = cs_header-id_block
              IMPORTING
                es_header = ls_header
                et_blocks = lt_blocks ).

            " Podría haber puesto los parámetros de salida directamente en la llamada al bloque. Pero como
            " estoy en continuo ajuste prefiero hacerlo manualmente por si las estructuras cambian.
            cs_header = ls_header.
            ct_blocks = lt_blocks.

          ENDIF.
        ENDIF.

      ENDIF.

    ELSE.
      INSERT zcl_bc_util=>fill_return( iv_type = zif_bc_data=>cs_message-error
                                 iv_number = '006'
                                 iv_message_v1 = cs_header-id_block
                                 iv_langu = mv_langu ) INTO TABLE et_return.
    ENDIF.

  ENDMETHOD.

  METHOD get_header_block_by_id.

    CLEAR: es_header, et_blocks.

    " Buscamos el primer registro con el status activo
    DATA(lt_params) = VALUE /bobf/t_frw_query_selparam( ( attribute_name = zif_bc_bo_blockchain_c=>sc_query_attribute-root-select_by_elements-id_block
                                                          sign = 'I'
                                                          option = 'EQ'
                                                          low = iv_id_block ) ).

    get_header_blocks(
      EXPORTING
        it_params      = lt_params
        iv_only_header = iv_only_header
      IMPORTING
        es_header      = es_header
        et_blocks      = et_blocks ).
  ENDMETHOD.

  METHOD get_header_blocks.
    DATA lt_header TYPE zbc_i_bo_header.

    CLEAR: es_header, et_blocks.

    " Lo más antiguos el primero y a nivel de bloque por el campo index
    DATA(lt_sorting) = VALUE /bobf/t_frw_query_sorting( ( attribute_name = zif_bc_bo_blockchain_c=>sc_node_attribute-root-erdat ascending = abap_true )
                                                        ( attribute_name = zif_bc_bo_blockchain_c=>sc_node_attribute-root-erzet ascending = abap_true ) ).

    mo_svc_mngr->query( EXPORTING iv_query_key = zif_bc_bo_blockchain_c=>sc_query-root-select_by_elements
                                  it_selection_parameters = it_params
                                  iv_fill_data = abap_true
                                  is_query_options = VALUE #( maximum_rows = 1 " Solo se quiere el primer registro, que será el más antiguo
                                                              sorting_options = lt_sorting )
                          IMPORTING et_data = lt_header
                                    et_key = DATA(lt_keys) ).

    " Como solo habrá un registro de cabecera, lo paso a una estructura y se informa el parámetro de salida
    READ TABLE lt_header INTO es_header INDEX 1.

    " Si hay datos y no se quiere solo la cabecera se buscan los datos del bloque
    IF sy-subrc = 0 AND iv_only_header = abap_false.


      DATA(lt_filter_keys) = VALUE /bobf/t_frw_key( ( key = es_header-key )  ). " Se indica de que ID quiere buscar
      mo_svc_mngr->retrieve_by_association(
        EXPORTING
          iv_node_key             = zif_bc_bo_blockchain_c=>sc_node-root
          it_key                  = lt_filter_keys
          iv_association          = zif_bc_bo_blockchain_c=>sc_association-root-block
          iv_fill_data            = abap_true
        IMPORTING
          et_data                 = et_blocks ).

      SORT et_blocks BY index_block ASCENDING. " Se orden por el index block, de menor a mayor.

    ENDIF.
  ENDMETHOD.

  METHOD calculate_pow.
    DATA lt_data TYPE zbc_i_bo_block.


    CLEAR: et_blocks, et_return.

    " Si no se informa ni la clave ni el id de bloque genera mensaje de error y se sale
    IF iv_key IS INITIAL AND iv_id_block IS INITIAL.
      INSERT zcl_bc_util=>fill_return(
    EXPORTING
      iv_type       = zif_bc_data=>cs_message-error
      iv_number     = '009' ) INTO TABLE et_return.
      EXIT.
      " Si se pasa el id del bloque hay que encontrar su clave
    ELSEIF iv_id_block IS NOT INITIAL.
      get_header_block_by_id(
        EXPORTING
          iv_id_block    = iv_id_block
          iv_only_header = abap_true
        IMPORTING
          es_header      = DATA(ls_header) ).

      " Se mueve la clave a una tabla interna para pasarla a la acción
      DATA(lt_key) = VALUE /bobf/t_frw_key( ( key = ls_header-key ) ).
    ELSE.
      lt_key = VALUE /bobf/t_frw_key( ( key = iv_key ) ).
    ENDIF.

* Parámetros
    DATA(lo_params) = NEW zbc_s_params_process_pow(  ).
    lo_params->only_new_blocks = iv_only_new_blocks. " Solo los nuevos bloques
    " Indica que no se quiere que el bloque se modifique internamente con el resultado del cáculo.
    lo_params->modify_block = iv_modify_block.

    " Se lanza la acción
    mo_svc_mngr->do_action(
      EXPORTING
        iv_act_key              = zif_bc_bo_blockchain_c=>sc_action-root-process_pow
        it_key = lt_key
        is_parameters = lo_params
      IMPORTING
        eo_message              = DATA(lo_message)
        et_data                 = lt_data ).

    " Se vuelca el resultado al tabla de salida.
    et_blocks = CORRESPONDING #( lt_data ).

    " Si hay algun mensaje se devuelve como mensaje de retorno. Esta acción solo tendrá mensajes de error
    IF lo_message->check( ).
      zcl_bc_util=>conv_message_bopf_2_return( EXPORTING io_message = lo_message
                                                     iv_langu = mv_langu
                                           CHANGING ct_return = et_return ).
    ELSE.
      " Se devuelven los bloques calculados
      et_blocks = CORRESPONDING #( lt_data ).

    ENDIF.

  ENDMETHOD.

  METHOD get_data_block_by_id_request.
    DATA lt_block TYPE zbc_i_bo_block.
    DATA lt_header TYPE zbc_i_bo_header.

    CLEAR: es_header, et_blocks.

    " Buscamos el bloque
    DATA(lt_params) = VALUE /bobf/t_frw_query_selparam( ( attribute_name = zif_bc_bo_blockchain_c=>sc_query_attribute-block-select_by_elements-id_request
                                                          sign = 'I'
                                                          option = 'EQ'
                                                          low = iv_id_request ) ).

    mo_svc_mngr->query( EXPORTING iv_query_key = zif_bc_bo_blockchain_c=>sc_query-block-select_by_elements
                                  it_selection_parameters = lt_params
                                  iv_fill_data = abap_true
                          IMPORTING et_data = lt_block
                                    et_key = DATA(lt_keys) ).

    IF lt_block IS NOT INITIAL.

      " Si se ha informado la cabecera se leen sus datos
      IF es_header IS SUPPLIED.

        mo_svc_mngr->retrieve_by_association( EXPORTING iv_node_key             = zif_bc_bo_blockchain_c=>sc_node-block
                                                        it_key                  = lt_keys
                                                        iv_association          = zif_bc_bo_blockchain_c=>sc_association-block-to_parent
                                                        iv_fill_data            = abap_true
                                              IMPORTING et_data                 = lt_header ).
        READ TABLE lt_header INTO es_header INDEX 1.

      ENDIF.

      " Si se quiere leer todos los bloques se obtiene el id del bloque y se usa el método ya existente para obtenerlo
      IF iv_get_all_blocks = abap_true.
        READ TABLE lt_block ASSIGNING FIELD-SYMBOL(<ls_block>) INDEX 1.
        get_header_block_by_id(
          EXPORTING
            iv_id_block    = <ls_block>-id_block
          IMPORTING
            et_blocks      = et_blocks ).

      ELSE. " Si no se quiere todos los bloques se devuelve el obtenido

        et_blocks = lt_block.

      ENDIF.

    ENDIF.

  ENDMETHOD.

  METHOD get_data_monitor.
    DATA lt_selparam_header TYPE /bobf/t_frw_query_selparam.
    DATA lt_selparam_block TYPE /bobf/t_frw_query_selparam.

    CLEAR: et_blocks, et_header, et_execution_log.

* Se separán los datos por nodos
    lt_selparam_header = VALUE /bobf/t_frw_query_selparam( FOR <ls_selparam> IN it_params
                                                                 WHERE ( node = zif_bc_bo_blockchain_c=>sc_node-root )
                                                                 ( attribute_name = <ls_selparam>-attribute_name
                                                                   sign = <ls_selparam>-sign
                                                                   option = <ls_selparam>-option
                                                                   low = <ls_selparam>-low
                                                                   high = <ls_selparam>-high ) ).

    lt_selparam_block = VALUE /bobf/t_frw_query_selparam( FOR <ls_selparam> IN it_params
                                                                 WHERE ( node = zif_bc_bo_blockchain_c=>sc_node-block )
                                                                 ( attribute_name = <ls_selparam>-attribute_name
                                                                   sign = <ls_selparam>-sign
                                                                   option = <ls_selparam>-option
                                                                   low = <ls_selparam>-low
                                                                   high = <ls_selparam>-high ) ).

* La búsqueda comienza a nivel de bloque cuando no hay datos a nivel de cabecera y si a nivel de bloque. Cualquier otro
    " caso distinto se empieza por la cabecera
    IF lt_selparam_block IS NOT INITIAL AND lt_selparam_header IS INITIAL.
      " En la búsqueda del bloque también se buscarán los datos de cabecera
      get_data_monitor_block( EXPORTING it_selparam = lt_selparam_block
                               IMPORTING et_blocks = et_blocks
                                         et_header = et_header ).

    ELSE.
      get_data_monitor_header( EXPORTING it_selparam = lt_selparam_header
                                  IMPORTING et_header = et_header ).

      IF et_header IS NOT INITIAL.

        " Se busca los datos del bloque pero se añade a la búsqueda el id de bloque
        LOOP AT et_header ASSIGNING FIELD-SYMBOL(<ls_header>).
          INSERT VALUE #( attribute_name = zif_bc_bo_blockchain_c=>sc_node_attribute-block-id_block
                          sign = 'I'
                          option = 'EQ'
                          low = <ls_header>-id_block ) INTO TABLE lt_selparam_block.
        ENDLOOP.

        get_data_monitor_block( EXPORTING it_selparam = lt_selparam_block
                               IMPORTING et_blocks = et_blocks ).

        LOOP AT et_header ASSIGNING <ls_header>.
          DATA(lv_tabix) = sy-tabix.
          " Si el id de bloque no se ha encontrado a nivel de bloque, este id se elimina a nivel de cabecera
          READ TABLE et_blocks TRANSPORTING NO FIELDS WITH KEY id_block = <ls_header>-id_block.
          IF sy-subrc NE 0.
            DELETE et_header INDEX lv_tabix.
          ENDIF.
        ENDLOOP.

      ENDIF.

      " Si aún quedan datos de cabecera se busca los datos de ejecución
      IF et_header IS NOT INITIAL.
        mo_svc_mngr->retrieve_by_association( EXPORTING iv_node_key             = zif_bc_bo_blockchain_c=>sc_node-root
                                                         it_key                  = VALUE #( FOR <ls_header1> IN et_header ( key = <ls_header1>-key ) )
                                                         iv_association          = zif_bc_bo_blockchain_c=>sc_association-root-execution_log
                                                         iv_fill_data            = abap_true
                                               IMPORTING et_data                 = et_execution_log ).

      ENDIF.

    ENDIF.
  ENDMETHOD.


  METHOD get_data_monitor_block.

    CLEAR: et_blocks, et_header.

    " Se orden por ID bloque e indice
    DATA(lt_sorting) = VALUE /bobf/t_frw_query_sorting( ( attribute_name = zif_bc_bo_blockchain_c=>sc_node_attribute-block-id_block ascending = abap_true )
                                                        ( attribute_name = zif_bc_bo_blockchain_c=>sc_node_attribute-block-index_block ascending = abap_true ) ).

    " Buscan los datos del bloque en base a los datos de selección
    mo_svc_mngr->query( EXPORTING iv_query_key = zif_bc_bo_blockchain_c=>sc_query-block-select_by_elements
                          it_selection_parameters = it_selparam
                          iv_fill_data = abap_true
                           is_query_options = VALUE #( sorting_options = lt_sorting )
                  IMPORTING et_data = et_blocks
                            et_key = DATA(lt_keys) ).

    " Se busca los datos de cabecera si se han encontrado datos y la cabecera se ha pasado por parámetro.
    " Eso solo pasará cuando haya datos de cabecera
    IF lt_keys IS NOT INITIAL AND et_header IS SUPPLIED.
      mo_svc_mngr->retrieve_by_association( EXPORTING iv_node_key             = zif_bc_bo_blockchain_c=>sc_node-block
                                                             it_key                  = lt_keys
                                                             iv_association          = zif_bc_bo_blockchain_c=>sc_association-block-to_parent
                                                             iv_fill_data            = abap_true
                                                   IMPORTING et_data                 = et_header ).
    ENDIF.

  ENDMETHOD.


  METHOD get_data_monitor_header.

    " Lo más antiguos el primero y a nivel de bloque por el campo index
    DATA(lt_sorting) = VALUE /bobf/t_frw_query_sorting( ( attribute_name = zif_bc_bo_blockchain_c=>sc_node_attribute-root-id_block ascending = abap_true ) ).

    " Buscan los datos del bloque en base a los datos de selección
    mo_svc_mngr->query( EXPORTING iv_query_key = zif_bc_bo_blockchain_c=>sc_query-root-select_by_elements
                          it_selection_parameters = it_selparam
                          iv_fill_data = abap_true
                          is_query_options = VALUE #( sorting_options = lt_sorting )
                  IMPORTING et_data = et_header
                            et_key = DATA(lt_keys) ).

  ENDMETHOD.

ENDCLASS.
