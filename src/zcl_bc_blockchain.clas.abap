CLASS zcl_bc_blockchain DEFINITION
  PUBLIC
  CREATE PUBLIC .

  PUBLIC SECTION.


    TYPES: BEGIN OF ts_values_calculate_hash,
             id_block    TYPE zbc_e_id_block,
             index_block TYPE zbc_e_index_block,
             prev_hash   TYPE zbc_e_prev_hash,
             timestamp   TYPE timestampl,
             nonce       TYPE zbc_e_nonce,
             data        TYPE zbc_e_data,
             id_request  TYPE zbc_e_id_request,
           END OF ts_values_calculate_hash.
    METHODS constructor
      IMPORTING
        iv_langu TYPE sylangu DEFAULT sy-langu.
    METHODS process_new_request
      EXPORTING
                et_return TYPE bapiret2_t
                et_data   TYPE zif_bc_data=>tt_blockchain
      RAISING   zcx_bc.
    METHODS id_request_data_check
      IMPORTING iv_id_request   TYPE zbc_e_id_request
                iv_data         TYPE string
      RETURNING VALUE(rv_valid) TYPE sap_bool
      RAISING   zcx_bc.
    CLASS-METHODS calculate_hash
      IMPORTING
                is_values      TYPE ts_values_calculate_hash
      RETURNING VALUE(rv_hash) TYPE zbc_e_hash.

  PROTECTED SECTION.
    DATA mv_langu TYPE sylangu.
    DATA mt_request TYPE zcl_bc_helper_request=>tt_request.
    DATA mo_helper_request TYPE REF TO zcl_bc_helper_request.
    DATA mo_helper_blockchain TYPE REF TO zcl_bc_helper_blockchain.
    DATA ms_header TYPE zcl_bc_helper_blockchain=>ts_header.
    DATA mt_blocks TYPE zcl_bc_helper_blockchain=>tt_blocks.
    DATA mt_blockchain TYPE zif_bc_data=>tt_blockchain.
    DATA mv_block_completed TYPE sap_bool.
    METHODS enqueue_process
      IMPORTING
                iv_action TYPE enqmode
      RAISING   zcx_bc.
    METHODS get_request.
    METHODS get_block_active
      EXPORTING et_return TYPE bapiret2_t.
    METHODS init_var_process.
    METHODS pre_process_pow
      EXPORTING et_return TYPE bapiret2_t.
    METHODS add_request_2_block.
    METHODS process_pow
      EXPORTING et_return TYPE bapiret2_t.

    METHODS post_process_pow
      EXPORTING
        et_return TYPE bapiret2_t.
    METHODS fill_itab_blockchain
      EXPORTING
        et_blockchain TYPE zif_bc_data=>tt_blockchain.


  PRIVATE SECTION.
ENDCLASS.



CLASS zcl_bc_blockchain IMPLEMENTATION.
  METHOD constructor.
    mv_langu = iv_langu.

    " Se instancias las clases que simplifican los accesos a los BOPF
    mo_helper_request = NEW zcl_bc_helper_request( iv_langu = mv_langu  ).
    mo_helper_blockchain = NEW zcl_bc_helper_blockchain( iv_langu = mv_langu  ).

  ENDMETHOD.

  METHOD process_new_request.

    CLEAR: et_return, et_data.

    " Inicialización de variables del proceso
    init_var_process(  ).

    " Primero hay que bloquear el proceso para que no haya dos corriendo a la vez.
    enqueue_process( iv_action = zif_bc_data=>cs_enqueue-lock ).

    " Se buscan las solicitudes
    get_request(  ).

    IF mt_request IS NOT INITIAL.

      " Se recuperán los datos del bloque activo
      get_block_active( IMPORTING et_return = et_return ).

      IF et_return IS INITIAL.

        " Antes de hacer el cálculo(proof at work, aka, pow) que puede tardar más o menos tiempo, lo que se hacen preparar los datos para el cálculo
        " para a continuar liberar el proceso y puede entrar el siguiente proceso para generar el siguiente bloque.
        pre_process_pow( IMPORTING et_return = et_return  ).


        " Después del proceso previo del cálculo el proceso se desbloqueo, tanto si hay error como no. El motivo es que a partir de ahora
        " va a comenzar el cálculo(si no hay errores) con lo que ese bloque ya no entrará en otros procesos paralelos que se lancen por que no esta activo.
        " Y si hay errores también se tiene que desbloquear para que no se quede bloqueado permanentemente.
        " NOTA: En la versión S/4 HANA al grabar en el BOPF el bloque manual se elimina. Por eso motivo esta comentado el desbloqueo, pero se deja
        " por si se necesita en otra versión de SAP
*        enqueue_process( iv_action = zif_bc_data=>cs_enqueue-lock ).

        IF et_return IS INITIAL.

          " Sin errores comienza el proceso Proof At Work(PoW)
          process_pow( IMPORTING et_return = et_return ).

          " Si hay errores en el proceso se visualizan
          IF et_return IS INITIAL.

            " En el post proceso del PoW se guardán los datos obtenidos
            post_process_pow( IMPORTING et_return = et_return ).

            " Se rellena la tabla con el resultado
            fill_itab_blockchain( IMPORTING et_blockchain = et_data ).

          ENDIF.

        ENDIF.


      ELSE. " Si hay errores se desbloquea el proceso

        enqueue_process( iv_action = zif_bc_data=>cs_enqueue-unlock ).

      ENDIF.

    ELSE. " Sin datos se sale del proceso, desbloqueandolo
      " Mensaje que no hay datos
      INSERT zcl_bc_util=>fill_return(
        EXPORTING
          iv_type       = zif_bc_data=>cs_message-success
          iv_number     = '005' ) INTO TABLE et_return.

      enqueue_process( iv_action = zif_bc_data=>cs_enqueue-unlock ).
    ENDIF.

  ENDMETHOD.


  METHOD enqueue_process.
    CASE iv_action.
      WHEN zif_bc_data=>cs_enqueue-lock.
        CALL FUNCTION 'ENQUEUE_ESINDX'
          EXPORTING
            mode_indx      = 'E'
            relid          = zif_bc_data=>cs_enqueue-relid
            srtfd          = zif_bc_data=>cs_enqueue-process_launch
          EXCEPTIONS
            foreign_lock   = 1
            system_failure = 2
            OTHERS         = 3.
        IF sy-subrc <> 0.
          " Si no se puede bloquear es porque el proceso se esta ejecutando
          RAISE EXCEPTION TYPE zcx_bc
            EXPORTING
              textid = zcx_bc=>process_blockchain_running.
        ENDIF.
      WHEN zif_bc_data=>cs_enqueue-unlock.
        CALL FUNCTION 'DEQUEUE_ESINDX'
          EXPORTING
            mode_indx = 'E'
            relid     = zif_bc_data=>cs_enqueue-relid
            srtfd     = zif_bc_data=>cs_enqueue-process_launch.

    ENDCASE.
  ENDMETHOD.


  METHOD get_request.

    CLEAR mt_request.

    " Solo leemos el máximo de registros que puede tener un bloque
    mo_helper_request->get_request(
      EXPORTING
      iv_number_rows = zif_bc_data=>cs_blockchain-number_blocks
      IMPORTING
        et_data = mt_request ).


  ENDMETHOD.


  METHOD get_block_active.
    CLEAR: ms_header, mt_blocks.

    " Se recupera los datos del bloque activo
    mo_helper_blockchain->get_header_block_active(
      IMPORTING
        es_header = ms_header
        et_blocks = mt_blocks ).

    " Si no hay bloque (porque están completos, en proceso de cálculo o simplemente es la primera que se lanza el proceso,
    " hay que crearlo
    IF ms_header IS INITIAL.

      mo_helper_blockchain->new_blockchain( IMPORTING es_header = ms_header
                                                      et_blocks = mt_blocks
                                                      et_return = et_return ).

      IF et_return IS INITIAL. " Si no hay errores, vuelva a recuperar el bloque activo
        mo_helper_blockchain->get_header_block_active(
           IMPORTING
             es_header = ms_header
             et_blocks = mt_blocks ).
      ENDIF.
    ENDIF.

  ENDMETHOD.

  METHOD calculate_hash.

    CLEAR rv_hash.

    " Para evitar concatenar todos los campos manualmente, prefiero hacer algo dinámico y que me permita
    " calcular el hash aunque los campos de la estructura varien.
    DATA(lo_struc) = CAST cl_abap_structdescr( cl_abap_typedescr=>describe_by_data( is_values ) ).

    DATA(lv_data) = VALUE string(  ).
    LOOP AT lo_struc->get_components( ) ASSIGNING FIELD-SYMBOL(<ls_components>).
      ASSIGN COMPONENT <ls_components>-name OF STRUCTURE is_values TO FIELD-SYMBOL(<field>).

      lv_data = COND #( WHEN lv_data IS INITIAL THEN |{ <field> }| ELSE |{ lv_data }-{ <field> }| ).

    ENDLOOP.

    TRY.
        cl_abap_message_digest=>calculate_hash_for_char( EXPORTING if_data = lv_data if_algorithm = zif_bc_data=>cs_blockchain-algorithm IMPORTING ef_hashstring = rv_hash ).
      CATCH cx_abap_message_digest INTO DATA(lx_excep).
        DATA(lv_msg) = lx_excep->get_text(  ).
        MESSAGE lv_msg TYPE 'X'.
    ENDTRY..
  ENDMETHOD.


  METHOD init_var_process.
    CLEAR: mt_blockchain, ms_header, mt_blocks,mt_request.
  ENDMETHOD.


  METHOD pre_process_pow.
    " El pre proceso del proof at work consiste en:
    " 1) Poner el status de cabecera a "In process"
    " 2) Añadir los request en el bloque, solo se rellenará hasta completarlo.
    " Si sobran request se borrarán para que no se procesen en el paso 3
    " 3) Grabar en los BOPF de Blockchain
    " 4) Borrar los request añadidos, si el paso 3 no da errores. Y siempre y cuando haya request a eliminar.
    "    Ya que este proceso puede que se lance desde un proceso donde no haya datos en MT_REQUEST.

    ms_header-status = zif_bc_data=>cs_status_block-in_process. " Paso 1
    add_request_2_block(  ). " Paso 2

    " Se lanza el paso 3 para grabar los datos del bloque
    mo_helper_blockchain->update_blockchain(
      EXPORTING
        iv_do_save = abap_true
      IMPORTING
        et_return  = et_return
      CHANGING
        cs_header  = ms_header
        ct_blocks  = mt_blocks   ).

    " Si no hay errores se lanza el paso 4 que es borrar las request
    IF et_return IS INITIAL AND mt_request IS NOT INITIAL.
      " Finalmente el paso 4 borrando las request añadidas, son las que quedan en MT_REQUEST, de la tabla de peticion.
      " El resultado del borrado no se controla porque la sentencia DELETE no debería fallar salvo que se haya borrando previamente
      " los id request. En teoria dentro de la arquitectura no puede pasar salvo que se borre externamente, si eso ocurre, pues nada
      " ya están borradas.
      mo_helper_request->delete_request(
        EXPORTING
          it_id_request = VALUE #( FOR <ls_request> IN mt_request ( <ls_request>-id_request ) )
          iv_do_save    = abap_true
        IMPORTING
          et_return     = DATA(lt_return) ).

    ENDIF.

  ENDMETHOD.


  METHOD add_request_2_block.

    " Se pone a falso la variable que sirve para saber si el bloque esta lleno o se le pueden añadir más request.
    " Esta variable se usará cuando se grabe el resultado de la PoW para indicar si el bloque esta completo o vuelve
    " a estar activo para añadir más request.
    mv_block_completed = abap_false.

    " Buscamos el último index añadido. El helper del BOPF de blockchain las consultas
    " que atacan al bloque siempre se orden por el campo INDEX_BLOCK
    DESCRIBE TABLE mt_blocks.
    READ TABLE mt_blocks INTO DATA(ls_last_block) INDEX sy-tfill.

    " Si por cualquier motivo este proceso se llama desde otro proceso donde no hay request controlo si el bloque esta lleno.
    " De esta manera garantizo la consistencia del proceso

    LOOP AT mt_request ASSIGNING FIELD-SYMBOL(<ls_request>).
      DATA(lv_tabix) = sy-tabix.
      " Mientras el index del bloque sea inferior al límite iré añadiendo.
      IF ls_last_block-index_block < zif_bc_data=>cs_blockchain-number_blocks.
        ls_last_block-index_block = ls_last_block-index_block + 1.
        GET TIME STAMP FIELD DATA(lv_timestamp).

        " En la conversión de la petición al bloque solo se añaden los campos básicos. El resto de
        " campo se rellenarán en el proceso de cálculo
        INSERT VALUE #( index_block = ls_last_block-index_block
                        id_request = <ls_request>-id_request
                        timestamp = lv_timestamp
                        data = <ls_request>-data ) INTO TABLE mt_blocks.


      ELSE.
        mv_block_completed = abap_true. " Bloque completo

        " Una vez superado iré quitando los bloques que no entrarán en el proceso
        DELETE mt_request INDEX lv_tabix.
      ENDIF.

    ENDLOOP.
    IF sy-subrc NE 0.
      IF ls_last_block-index_block >= zif_bc_data=>cs_blockchain-number_blocks.
        mv_block_completed = abap_true. " Bloque completo
      ENDIF.
      " Si al salir del bucle el numero de bloque es igual al máximo entonces el bloque se tratará
      " como completo.
    ELSEIF ls_last_block-index_block = zif_bc_data=>cs_blockchain-number_blocks.
      mv_block_completed = abap_true. " Bloque completo
    ENDIF.


  ENDMETHOD.


  METHOD process_pow.

    " Nota: Al proceso de cálculo se le indica que en la acción dentro del BOPF no modifique a nivel interno
    " lo datos del cálculo del bloque. El motivo es que se hará en el post proceso que permitira grabar datos de
    " cabecera y bloque y además, recuperar las determinaciones que realizan en el BOPF
    mo_helper_blockchain->calculate_pow(
      EXPORTING
      iv_key      = ms_header-key
      iv_only_new_blocks = abap_true
      iv_modify_block = abap_false
    IMPORTING
      et_blocks   = DATA(lt_blocks)
      et_return = et_return ).

    " Si hay errores no se continua el proceso y se sale.
    IF et_return IS NOT INITIAL. EXIT. ENDIF.

    " Inicialmente los datos del bloque que se devuelve son los mismos, con el pow calculado, que los que hay a nivel global.
    " Aún asi, no opto por machacar los datos globales si no que los voy actualizando uno a uno.

    LOOP AT mt_blocks ASSIGNING FIELD-SYMBOL(<ls_blocks>).

      READ TABLE lt_blocks ASSIGNING FIELD-SYMBOL(<ls_blocks_pow>) WITH KEY index_block = <ls_blocks>-index_block.
      IF sy-subrc = 0.
        <ls_blocks> = CORRESPONDING #( BASE ( <ls_blocks> ) <ls_blocks_pow> ).
      ENDIF.

    ENDLOOP.

  ENDMETHOD.


  METHOD post_process_pow.

    " Se cambia el status según si el bloque esta completo o no.
    ms_header-status = COND #( WHEN mv_block_completed = abap_false THEN zif_bc_data=>cs_status_block-active ELSE zif_bc_data=>cs_status_block-completed ).

    " Se graban los datos de la cabecera, los de bloque se han actualizado en la acción de cálculo del POW
    mo_helper_blockchain->update_blockchain(
  EXPORTING
    iv_do_save = abap_true
  IMPORTING
    et_return  = et_return
  CHANGING
    cs_header  = ms_header
    ct_blocks = mt_blocks ).




  ENDMETHOD.


  METHOD fill_itab_blockchain.
    DATA lt_blockchain TYPE zif_bc_data=>tt_blockchain.


    LOOP AT mt_blocks ASSIGNING FIELD-SYMBOL(<ls_blocks>).
      APPEND INITIAL LINE TO lt_blockchain ASSIGNING FIELD-SYMBOL(<ls_blockchain>).

      <ls_blockchain>-status = ms_header-status.

      " Se pasan los datos de cabecera y del bloque a la tabla global.
      <ls_blockchain> = CORRESPONDING #( BASE ( <ls_blockchain> ) ms_header ).
      <ls_blockchain> = CORRESPONDING #( BASE ( <ls_blockchain> ) <ls_blocks> ).

    ENDLOOP.

    " Si no se ha pasado el parámetro de salida el resultado se guarda en la tabla global
    IF et_blockchain IS REQUESTED.
      et_blockchain = lt_blockchain.
    ELSE.
      mt_blockchain = lt_blockchain.
    ENDIF.

  ENDMETHOD.

  METHOD id_request_data_check.

    rv_valid = abap_false. " Por defecto no es valido

    " Recuperamos los datos del bloque
    mo_helper_blockchain->get_data_block_by_id_request(
      EXPORTING
        iv_id_request     =  iv_id_request
      IMPORTING
        et_blocks         = DATA(lt_blocks) ).

    IF lt_blocks IS NOT INITIAL.

      READ TABLE lt_blocks ASSIGNING FIELD-SYMBOL(<ls_blocks>) INDEX 1.

      " Se calcula el hash de los datos del bloque pero cambiando los datos por el pasado por parámetro
      DATA(lv_hash) = zcl_bc_blockchain=>calculate_hash( is_values = VALUE #( id_block = <ls_blocks>-id_block
                                                                               prev_hash = <ls_blocks>-prev_hash
                                                                               index_block = <ls_blocks>-index_block
                                                                               data = zcl_bc_util=>convert_data_2_request( iv_data )
                                                                               timestamp = <ls_blocks>-timestamp
                                                                               nonce = <ls_blocks>-nonce
                                                                               id_request = <ls_blocks>-id_request ) ).

      IF lv_hash = <ls_blocks>-hash. " Si son iguales es válido
        rv_valid = abap_true.
      ENDIF.

    ELSE.
      RAISE EXCEPTION TYPE zcx_bc
        EXPORTING
          textid   = zcx_bc=>id_request_not_exist
          mv_attr1 = CONV #( iv_id_request ).
    ENDIF.

  ENDMETHOD.

ENDCLASS.
