*&---------------------------------------------------------------------*
*& Include          ZBC_R_MONITOR_BLOCKCHAIN_C01
*&---------------------------------------------------------------------*
CLASS lcl_monitor DEFINITION.

  PUBLIC SECTION.
    TYPES: BEGIN OF ts_data.
             INCLUDE TYPE zif_bc_data=>ts_blockchain.
             TYPES: view_exec_log   TYPE lvc_s_icon,
             view_block      TYPE lvc_s_icon,
             view_block_data TYPE lvc_s_icon,
             cellstyle       TYPE salv_t_int4_column,
           END OF ts_data.
    TYPES tt_data TYPE STANDARD TABLE OF ts_data WITH EMPTY KEY.
    TYPES: BEGIN OF ts_blocks_data.
             INCLUDE TYPE zif_bc_data=>ts_block_fields.
             TYPES: view_block_data TYPE lvc_s_icon,
             cellstyle       TYPE salv_t_int4_column,
           END OF ts_blocks_data.
    TYPES: tt_blocks_data TYPE STANDARD TABLE OF ts_blocks_data.
    TYPES: tt_exec_log_data TYPE STANDARD TABLE OF zbc_s_bo_execution_log WITH EMPTY KEY.
    DATA mt_data TYPE tt_data.
    DATA mt_blocks_data TYPE tt_blocks_data.
    DATA mt_exec_log_data TYPE tt_exec_log_data.

    METHODS constructor.

    METHODS search_data.
    METHODS get_blocks_by_id
      IMPORTING iv_id_block TYPE zbc_e_id_block.
    METHODS get_exec_log_by_id
      IMPORTING iv_id_block TYPE zbc_e_id_block.

  PROTECTED SECTION.
    DATA mo_blockchain TYPE REF TO zcl_bc_blockchain.
    DATA mo_helper_blockchain TYPE REF TO zcl_bc_helper_blockchain.
    DATA mt_sel_screen TYPE pivb_rsparamsl_255_t .
    DATA mt_bopf_selparam TYPE zcl_bc_helper_blockchain=>tt_monitor_selparam.
    DATA mt_header TYPE zbc_i_bo_header.
    DATA mt_block TYPE zbc_i_bo_block.
    DATA mt_execution_log TYPE zbc_i_bo_execution_log.


    METHODS load_sel_screen.
    METHODS consolidate_data.


ENDCLASS.

CLASS lcl_monitor IMPLEMENTATION.
  METHOD constructor.
    mo_blockchain = NEW zcl_bc_blockchain(  ).
    mo_helper_blockchain = NEW zcl_bc_helper_blockchain(  ).
  ENDMETHOD.
  METHOD load_sel_screen.
    DATA lt_params TYPE STANDARD TABLE OF rsparams WITH EMPTY KEY.

    CALL FUNCTION 'RS_REFRESH_FROM_SELECTOPTIONS'
      EXPORTING
        curr_report         = sy-repid
      TABLES
        selection_table     = lt_params
        selection_table_255 = mt_sel_screen[]
      EXCEPTIONS
        not_found           = 1
        no_report           = 2
        OTHERS              = 3.


* Ahora se convierte la pantalla de selección al formato de query. Solo se procesan aquellos
* registros que estén informados
    LOOP AT mt_sel_screen ASSIGNING FIELD-SYMBOL(<ls_sel_screen>)
                          WHERE ( kind = 'P' AND low IS NOT INITIAL )
                                OR ( kind = 'S' AND ( low IS NOT INITIAL OR high IS NOT INITIAL ) ).                                                          .

      " Se adapta el parámetro del report al atributo del nodo
      CASE <ls_sel_screen>-selname.
        WHEN 'S_IBLOCK'.
          DATA(lv_attribute) = zif_bc_bo_blockchain_c=>sc_node_attribute-root-id_block.
          DATA(lv_node) = zif_bc_bo_blockchain_c=>sc_node-root.
        WHEN 'S_IREQ'.
          lv_attribute = zif_bc_bo_blockchain_c=>sc_node_attribute-block-id_request.
          lv_node = zif_bc_bo_blockchain_c=>sc_node-block.
        WHEN 'S_STATUS'.
          lv_attribute = zif_bc_bo_blockchain_c=>sc_node_attribute-root-status.
          lv_node = zif_bc_bo_blockchain_c=>sc_node-root.
        WHEN 'S_ERDAT'.
          lv_attribute = zif_bc_bo_blockchain_c=>sc_node_attribute-root-erdat.
          lv_node = zif_bc_bo_blockchain_c=>sc_node-root.
        WHEN 'S_ERZET'.
          lv_attribute = zif_bc_bo_blockchain_c=>sc_node_attribute-root-erzet.
          lv_node = zif_bc_bo_blockchain_c=>sc_node-root.
        WHEN 'S_ERNAM'.
          lv_attribute = zif_bc_bo_blockchain_c=>sc_node_attribute-root-ernam.
          lv_node = zif_bc_bo_blockchain_c=>sc_node-root.
        WHEN 'S_AEDAT'.
          lv_attribute = zif_bc_bo_blockchain_c=>sc_node_attribute-root-aedat.
          lv_node = zif_bc_bo_blockchain_c=>sc_node-root.
        WHEN 'S_AETIME'.
          lv_attribute = zif_bc_bo_blockchain_c=>sc_node_attribute-root-aetime.
          lv_node = zif_bc_bo_blockchain_c=>sc_node-root.
        WHEN 'S_AUNAME'.
          lv_attribute = zif_bc_bo_blockchain_c=>sc_node_attribute-root-auname.
          lv_node = zif_bc_bo_blockchain_c=>sc_node-root.
      ENDCASE.
      IF lv_attribute IS NOT INITIAL.
        APPEND INITIAL LINE TO mt_bopf_selparam ASSIGNING FIELD-SYMBOL(<ls_selparam>).
        " Los campos restantes son comunes y se pasan tal cual
        <ls_selparam> = CORRESPONDING #( <ls_sel_screen> ) .
        <ls_selparam>-attribute_name = lv_attribute.
        <ls_selparam>-node = lv_node.
        CLEAR: lv_attribute, lv_node.
      ENDIF.

    ENDLOOP.

  ENDMETHOD.

  METHOD search_data.
    CLEAR: mt_block, mt_header, mt_execution_log, mt_data.

    " Se recupera los datos de la pantalla de selección
    load_sel_screen(  ).

    " Se buscan los datos
    mo_helper_blockchain->get_data_monitor(
      EXPORTING
        it_params        = mt_bopf_selparam
      IMPORTING
        et_header        = mt_header
        et_blocks        =  mt_block
        et_execution_log =  mt_execution_log ).

    " Consolidación de datos en una tabla global
    consolidate_data(  ).
  ENDMETHOD.


  METHOD consolidate_data.
    DATA lv_summary TYPE sap_bool.


    " Recupero si quieren ver el listado en formato resumen o detalle.
    zcl_bc_util=>conv_params_2_value( EXPORTING iv_paramname = 'P_SUMM'
                                                it_params_sl = mt_sel_screen
                                      IMPORTING ev_value     = lv_summary ).
    LOOP AT mt_header ASSIGNING FIELD-SYMBOL(<ls_header>).

      DATA(ls_data) = CORRESPONDING ts_data( <ls_header> ).

      " Si hay datos de ejecución se mostrará un icocno

      READ TABLE mt_execution_log TRANSPORTING NO FIELDS WITH KEY id_block = <ls_header>-id_block.
      IF sy-subrc = 0.
        ls_data-view_exec_log = icon_biw_monitor.
        INSERT VALUE salv_s_int4_column( columnname = 'VIEW_EXEC_LOG'  value = if_salv_c_cell_type=>hotspot ) INTO TABLE ls_data-cellstyle.
      ENDIF.

      " En el resumen no se añade los datos del bloque
      IF lv_summary = abap_false.
        " Se añade el icono para poder ver los datos del bloque
        ls_data-view_block_data = icon_content_object.
        INSERT VALUE salv_s_int4_column( columnname = 'VIEW_BLOCK_DATA'  value = if_salv_c_cell_type=>hotspot ) INTO TABLE ls_data-cellstyle.

        LOOP AT mt_block ASSIGNING FIELD-SYMBOL(<ls_block>) WHERE id_block = <ls_header>-id_block.
          ls_data = CORRESPONDING #( BASE ( ls_data ) <ls_block> ).
          INSERT ls_data INTO TABLE mt_data.
        ENDLOOP.
      ELSE.
        " Se añade un icono para poder ver el bloque
        ls_data-view_block = icon_detail.
        INSERT VALUE salv_s_int4_column( columnname = 'VIEW_BLOCK'  value = if_salv_c_cell_type=>hotspot ) INTO TABLE ls_data-cellstyle.


        INSERT ls_data INTO TABLE mt_data.
      ENDIF.


    ENDLOOP.
  ENDMETHOD.

  METHOD get_blocks_by_id.
    CLEAR mt_blocks_data.

    LOOP AT mt_block ASSIGNING FIELD-SYMBOL(<ls_blocks>) WHERE id_block = iv_id_block.
      APPEND INITIAL LINE TO mt_blocks_data ASSIGNING FIELD-SYMBOL(<ls_b>).
      <ls_b> = CORRESPONDING #( <ls_blocks> ).
      " Se añade el icono para poder ver los datos del bloque
      <ls_b>-view_block_data = icon_content_object.
      INSERT VALUE salv_s_int4_column( columnname = 'VIEW_BLOCK_DATA'  value = if_salv_c_cell_type=>hotspot ) INTO TABLE <ls_b>-cellstyle.
    ENDLOOP.

  ENDMETHOD.

  METHOD get_exec_log_by_id.
    CLEAR mt_exec_log_data.

    LOOP AT mt_execution_log ASSIGNING FIELD-SYMBOL(<ls_exec>) WHERE id_block = iv_id_block.
      APPEND INITIAL LINE TO mt_exec_log_data ASSIGNING FIELD-SYMBOL(<ls_e>).
      <ls_e> = CORRESPONDING #( <ls_exec> ).
    ENDLOOP.
  ENDMETHOD.

ENDCLASS.
