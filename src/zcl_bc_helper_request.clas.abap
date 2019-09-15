CLASS zcl_bc_helper_request DEFINITION
  PUBLIC
  FINAL
  CREATE PUBLIC .

  PUBLIC SECTION.
    TYPES: tt_request TYPE STANDARD TABLE OF zsbcddl_request_d WITH EMPTY KEY.
    TYPES: tt_id_request TYPE STANDARD TABLE OF zbc_e_id_request WITH EMPTY KEY.
    METHODS constructor
      IMPORTING
        iv_langu TYPE sylangu DEFAULT sy-langu.
    METHODS add_request
      IMPORTING
        iv_data              TYPE any
        iv_launch_blockchain TYPE sap_bool DEFAULT abap_false
      EXPORTING
        ev_id_request        TYPE zbc_e_id_request
        et_return            TYPE bapiret2_t.
    METHODS launch_blockchain
      IMPORTING iv_batch     TYPE sap_bool DEFAULT abap_true
      EXPORTING et_return    TYPE bapiret2_t
                !ev_jobcount TYPE tbtcjob-jobcount
                !ev_jobname  TYPE tbtcjob-jobname.
    METHODS get_request
      IMPORTING
        iv_number_rows TYPE i OPTIONAL
      EXPORTING
        et_data        TYPE zcl_bc_helper_request=>tt_request.
    METHODS delete_request
      IMPORTING
        it_id_request TYPE zcl_bc_helper_request=>tt_id_request
        iv_do_save    TYPE sap_bool DEFAULT abap_true
      EXPORTING
        et_return     TYPE bapiret2_t.

  PROTECTED SECTION.
    DATA mo_svc_mngr TYPE REF TO /bobf/if_tra_service_manager.
    DATA mo_txn_mngr TYPE REF TO /bobf/if_tra_transaction_mgr.
    DATA mo_conf_mngr TYPE REF TO /bobf/if_frw_configuration.

    DATA mv_langu TYPE sylangu.


  PRIVATE SECTION.
ENDCLASS.



CLASS zcl_bc_helper_request IMPLEMENTATION.
  METHOD add_request.
    DATA lt_mod TYPE /bobf/t_frw_modification.

    CLEAR: et_return, ev_id_request.

    " Se convierte los datos de entrada a string. En caso de tablas y estructuras se adaptará a JSON
    TRY.

        " Se crea la instancia a la estructura donde se guardarán los datos
        DATA(lo_request) = NEW zsbcddl_request_d(  ).

        " Se adaptan los valores pasados. En caso de datos simples se pasan tal cual.
        " En caso de tablas o estructuras se pasan a JSON.
        lo_request->data = zcl_bc_util=>convert_data_2_request( iv_data ).
        lo_request->id_request = zcl_bc_util=>get_id_request(  ).

        " Se añade el registro a la tabla de modificaciones
        INSERT VALUE #( node = zif_bc_ddl_request_c=>sc_node-zbc_ddl_request
                        change_mode = /bobf/if_frw_c=>sc_modify_create
                        key = lo_request->id_request
                        data = lo_request )
               INTO TABLE lt_mod.

        " Se graban los datos en la memoria del BOPF
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
          " Finalmente se pasan los datos de la shared memory a la base de datos

          mo_txn_mngr->save(
        IMPORTING
          ev_rejected            = DATA(lv_rejected)
          eo_message             = DATA(lo_message_txn) ).

          IF lv_rejected = abap_true. " Se ha producido un error.
            zcl_bc_util=>conv_message_bopf_2_return( EXPORTING io_message = lo_message_txn
                                                           iv_langu = mv_langu
                                                 CHANGING ct_return = et_return ).

          ELSE.
            ev_id_request = lo_request->id_request.

            IF iv_launch_blockchain = abap_true. " Se lanza el proceso de blockchain
              launch_blockchain( IMPORTING et_return = et_return ).
            ENDIF.
          ENDIF.

        ENDIF.


      CATCH zcx_bc INTO DATA(lo_excep).
        INSERT  zcl_bc_util=>fill_return( iv_type = zif_bc_data=>cs_message-error
                                              iv_number = lo_excep->if_t100_dyn_msg~msgty
                                              iv_message_v1 = lo_excep->if_t100_dyn_msg~msgv1
                                              iv_message_v2 = lo_excep->if_t100_dyn_msg~msgv2
                                              iv_message_v3 = lo_excep->if_t100_dyn_msg~msgv3
                                              iv_message_v4 = lo_excep->if_t100_dyn_msg~msgv4
                                              iv_langu = mv_langu ) INTO TABLE et_return.
    ENDTRY.
  ENDMETHOD.

  METHOD constructor.
    mv_langu = sy-langu.

    " Se instancian las clases del BOPF
    TRY.
        " Inicialización del gestor transaccional actualizaciones, bloqueos, etc..
        mo_txn_mngr = /bobf/cl_tra_trans_mgr_factory=>get_transaction_manager( ).

        " Creación del el gestor de servicios del BOPF. Permite realizar las llamadas al BOPF para ejecutar validaciones, acciones, añadir, etc..
        " Es la clase más importante ya que toda la gestión CRUD se realiza en esta clase
        mo_svc_mngr = /bobf/cl_tra_serv_mgr_factory=>get_service_manager( zif_bc_ddl_request_c=>sc_bo_key ).

        " Creación de la configuración del BOPF, permite obtener los metadas del BOPF
        mo_conf_mngr = /bobf/cl_frw_factory=>get_configuration( zif_bc_ddl_request_c=>sc_bo_key ).

      CATCH /bobf/cx_frw.
        "TODO: Error handling...
    ENDTRY.

  ENDMETHOD.

  METHOD launch_blockchain.

    CLEAR: et_return, ev_jobcount, ev_jobname.

    " Se informa de los parámetros
    DATA(lt_params) = VALUE zif_bc_data=>tt_rsparams( ( selname = 'P_NREQ' kind = 'P' low = 'X' )
                                                      ( selname = 'P_RNREQ' kind = 'P' low = 'X' ) ).

    " Se lanza el programa que lanzará el proceso de blockchaing
    zcl_bc_util=>execute_report( EXPORTING it_params = lt_params
                                           iv_jobname = zif_bc_data=>cs_process_bc_custo-jobname_new_request
                                           iv_report = zif_bc_data=>cs_process_bc_custo-report_launch
                                           iv_batch = iv_batch
                                           iv_langu = mv_langu
                                 IMPORTING et_return = et_return
                                           ev_jobcount = ev_jobcount ).
    IF et_return[ 1 ]-type = zif_bc_data=>cs_message-success.
      ev_jobname = zif_bc_data=>cs_process_bc_custo-jobname_new_request.
    ELSE. " El numero de job puede estar informado porque el fallo puede ser al cerrar el job
      CLEAR: ev_jobcount.
    ENDIF.

  ENDMETHOD.

  METHOD get_request.
    DATA lt_data TYPE ztbcddl_request.

    CLEAR et_data.

    " Se lanza la query que devuelve las solicitudes.
    " Para que funcione la búsqueda, ya que se usa el SADL, hay que informar los campos que se quiere recuperar


    mo_svc_mngr->query(
      EXPORTING
        iv_query_key            = zif_bc_ddl_request_c=>sc_query-zbc_ddl_request-get_request
        is_query_options        = VALUE #( maximum_rows = iv_number_rows
                                  sorting_options = VALUE #( ( attribute_name = zif_bc_ddl_request_c=>sc_node_attribute-zbc_ddl_request-erdat ascending = abap_true )
                                                             ( attribute_name = zif_bc_ddl_request_c=>sc_node_attribute-zbc_ddl_request-erzet ascending = abap_true ) ) )
        iv_fill_data            = abap_true
        it_requested_attributes  = VALUE #( ( zif_bc_ddl_request_c=>sc_node_attribute-zbc_ddl_request-id_request ) ( zif_bc_ddl_request_c=>sc_node_attribute-zbc_ddl_request-data )
                                               ( zif_bc_ddl_request_c=>sc_node_attribute-zbc_ddl_request-erdat ) ( zif_bc_ddl_request_c=>sc_node_attribute-zbc_ddl_request-ernam )
                                               ( zif_bc_ddl_request_c=>sc_node_attribute-zbc_ddl_request-erzet ) )
        IMPORTING
          et_data                 = lt_data
          et_key                  = DATA(lt_key) ).

    " Se adapta los valores devuelto a la tabla de salida
    et_data = CORRESPONDING #( lt_data ).

  ENDMETHOD.

  METHOD delete_request.
    DATA lt_r_id_request TYPE RANGE OF zbc_e_id_request.

    IF it_id_request IS INITIAL. EXIT. ENDIF. " Borraría toda la tabla

    " Como no consigo hacer funcionar el DELETE del BOPF al estar basado en CDS, opto por el camino del medio y es hacer delete a pelo
    lt_r_id_request = VALUE #( FOR <ls_id_request> IN it_id_request
                               ( sign = 'I' option = 'EQ' low = <ls_id_request> ) ).

    DELETE FROM zbc_t_0001 WHERE id_request IN lt_r_id_request.
    IF sy-subrc = 0.
      INSERT zcl_bc_util=>fill_return(
               iv_type       = zif_bc_data=>cs_message-success
               iv_number     = '007'
               iv_langu      = mv_langu
             ) INTO TABLE et_return.
      IF iv_do_save = abap_true.
        COMMIT WORK AND WAIT.
      ENDIF.
    ELSE.
      INSERT zcl_bc_util=>fill_return(
               iv_type       = zif_bc_data=>cs_message-error
               iv_number     = '008'
               iv_langu      = mv_langu
             ) INTO TABLE et_return.
      IF iv_do_save = abap_true.
        ROLLBACK WORK.
      ENDIF.
    ENDIF.
*    DATA lt_mod TYPE /bobf/t_frw_modification.
*    DATA lt_data TYPE ztbcddl_request.
*
*    DATA(lt_params) = VALUE /bobf/t_frw_query_selparam( FOR <ls_id_request> IN it_id_request
*                                                         ( attribute_name = zif_bc_ddl_request_c=>sc_query_result_type_attribute-zbc_ddl_request-get_request-id_request
*                                                          sign = 'I'
*                                                          option = 'EQ'
*                                                          low = <ls_id_request> ) ).
*
*    mo_svc_mngr->query(
*        EXPORTING
*          iv_query_key            = zif_bc_ddl_request_c=>sc_query-zbc_ddl_request-get_request
*          it_selection_parameters = lt_params
*          iv_fill_data            = abap_true
*          it_requested_attributes  = VALUE #( ( zif_bc_ddl_request_c=>sc_node_attribute-zbc_ddl_request-id_request ) ( zif_bc_ddl_request_c=>sc_node_attribute-zbc_ddl_request-data )
*                                               ( zif_bc_ddl_request_c=>sc_node_attribute-zbc_ddl_request-erdat ) ( zif_bc_ddl_request_c=>sc_node_attribute-zbc_ddl_request-ernam )
*                                               ( zif_bc_ddl_request_c=>sc_node_attribute-zbc_ddl_request-erzet ) )
*          IMPORTING
*            et_data                 = lt_data
*            et_key                  = DATA(lt_key) ).
*
*    LOOP AT lt_data REFERENCE INTO DATA(lo_data).
*      APPEND VALUE #( node = zif_bc_ddl_request_c=>sc_node-zbc_ddl_request
*                      change_mode = /bobf/if_frw_c=>sc_modify_delete
*                      key = lo_data->key
*                      data = lo_data ) TO lt_mod.
*    ENDLOOP.
*
*    BREAK-POINT.
*
*    mo_svc_mngr->modify(
*             EXPORTING
*               it_modification = lt_mod
*             IMPORTING
*               eo_change       = DATA(lo_change)
*               eo_message      = DATA(lo_message) ).
*
*    IF lo_message IS BOUND AND lo_message->check( ) EQ abap_true.
*
*      zcl_bc_util=>conv_message_bopf_2_return( EXPORTING io_message = lo_message
*                                                         iv_langu = mv_langu
*                                               CHANGING ct_return = et_return ).
*
*    ELSE.
*      " Finalmente se pasan los datos de la shared memory a la base de datos
*
*      mo_txn_mngr->save(
*    IMPORTING
*      ev_rejected            = DATA(lv_rejected)
*      eo_message             = DATA(lo_message_txn) ).
*
*      IF lv_rejected = abap_true. " Se ha producido un error.
*        zcl_bc_util=>conv_message_bopf_2_return( EXPORTING io_message = lo_message_txn
*                                                       iv_langu = mv_langu
*                                             CHANGING ct_return = et_return ).
*
*      ELSE.
*
*      ENDIF.
*
*    ENDIF.

  ENDMETHOD.

ENDCLASS.
