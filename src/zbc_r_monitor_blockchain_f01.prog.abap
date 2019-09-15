*----------------------------------------------------------------------*
***INCLUDE ZBC_R_MONITOR_BLOCKCHAIN_F01.
*----------------------------------------------------------------------*
*&---------------------------------------------------------------------*
*& Form INITIALIZATION
*&---------------------------------------------------------------------*
*& text
*&---------------------------------------------------------------------*
FORM initialization .
  mo_monitor = NEW lcl_monitor( ).
ENDFORM.
*&---------------------------------------------------------------------*
*& Form SEARCH_DATA
*&---------------------------------------------------------------------*
*& text
*&---------------------------------------------------------------------*
*& -->  p1        text
*& <--  p2        text
*&---------------------------------------------------------------------*
FORM search_data .

  mo_monitor->search_data( ).

ENDFORM.
*&---------------------------------------------------------------------*
*& Form SHOW_DATA
*&---------------------------------------------------------------------*
*& text
*&---------------------------------------------------------------------*
FORM show_data .
  TRY.
      cl_salv_table=>factory(
        IMPORTING
          r_salv_table = mo_alv_monitor
        CHANGING
          t_table      = mo_monitor->mt_data ).
    CATCH cx_salv_msg.                                  "#EC NO_HANDLER
  ENDTRY.

  " Activamos las funciones del ALV
  DATA(lo_functions) = mo_alv_monitor->get_functions( ).
  lo_functions->set_all( abap_true ).

  " Layout
  DATA(lo_layout) = mo_alv_monitor->get_layout( ).

*** Selecciones
  DATA(lo_selections) = mo_alv_monitor->get_selections( ).

* Los tipos de selección lo indica la interface: IF_SALV_C_SELECTION_MODE
  lo_selections->set_selection_mode( if_salv_c_selection_mode=>single ).

  CREATE OBJECT mo_event_alv_monitor.

  DATA(lo_events) = mo_alv_monitor->get_event( ).

  SET HANDLER:
    mo_event_alv_monitor->on_link_click FOR lo_events,
    mo_event_alv_monitor->on_user_command FOR lo_events.

  " Adaptación del catalogo de campos
  PERFORM adapt_fieldcatalog.

  " Se muestra el ALV
  mo_alv_monitor->display( ).


ENDFORM.
*&---------------------------------------------------------------------*
*& Form ADAPT_FIELDCATALOG
*&---------------------------------------------------------------------*
*& text
*&---------------------------------------------------------------------*
FORM adapt_fieldcatalog .
  DATA lv_long_text TYPE scrtext_l.
  DATA lv_short_text TYPE scrtext_s.
  DATA lv_medium_text TYPE scrtext_m.

* Columnas
  DATA(lo_columns) = mo_alv_monitor->get_columns( ).

  lo_columns->set_optimize( abap_true ). " Optimizadas
  lo_columns->set_cell_type_column( 'CELLSTYLE' ).


  lo_columns->set_column_position( columnname = 'STATUS_TEXT' position = '3' ).

  DATA(lo_column) = CAST cl_salv_column_table( lo_columns->get_column( 'VIEW_EXEC_LOG' ) ).
  lo_columns->set_column_position( columnname = 'VIEW_EXEC_LOG' position = '10' ).
  lo_column->set_symbol( abap_true ).
  lv_short_text = lv_medium_text = lv_long_text = TEXT-c06.
  lo_column->set_long_text( lv_long_text ).
  lo_column->set_medium_text( lv_medium_text ).
  lo_column->set_short_text( lv_short_text ).


  lo_column = CAST cl_salv_column_table( lo_columns->get_column( 'VIEW_BLOCK' ) ).
  IF p_summ = abap_true. " Solo se verá en el resumen
    lo_columns->set_column_position( columnname = 'VIEW_BLOCK' position = '11' ).
    lo_column->set_symbol( abap_true ).
    lv_short_text = lv_medium_text = lv_long_text = TEXT-c07.
    lo_column->set_long_text( lv_long_text ).
    lo_column->set_medium_text( lv_medium_text ).
    lo_column->set_short_text( lv_short_text ).
  ELSE.
    lo_column->set_technical( abap_true ).
  ENDIF.

  " Si el listado es en formato resumen se oculta los campos del bloque
  IF p_summ = abap_true.
    PERFORM hide_blocks_fields.
  ELSE.
    " Campos del bloque
    PERFORM fcat_fields_block USING 'M' CHANGING mo_alv_monitor .

  ENDIF.
ENDFORM.
*&---------------------------------------------------------------------*
*& Form HIDE_BLOCKS_FIELDS
*&---------------------------------------------------------------------*
*& text
*&---------------------------------------------------------------------*
FORM hide_blocks_fields .
  DATA ls_block TYPE zif_bc_data=>ts_block_fields.

  " Los campos del bloque los saco de la estructura que se usa en la tabla global que guardar dichos valores
  DATA(lo_struc) = CAST cl_abap_structdescr( cl_abap_typedescr=>describe_by_data( ls_block ) ).

  DATA(lo_columns) = mo_alv_monitor->get_columns( ).

  LOOP AT lo_struc->get_components( ) ASSIGNING FIELD-SYMBOL(<ls_components>).

    DATA(lo_column) = CAST cl_salv_column_table( lo_columns->get_column( CONV #( <ls_components>-name ) ) ).
    lo_column->set_technical( abap_true ).

  ENDLOOP.

  lo_column = CAST cl_salv_column_table( lo_columns->get_column( 'VIEW_BLOCK_DATA' ) ).
  lo_column->set_technical( abap_true ).

ENDFORM.

FORM hotspot USING pe_row pe_column.
  READ TABLE mo_monitor->mt_data ASSIGNING FIELD-SYMBOL(<ls_data>) INDEX pe_row.
  IF sy-subrc = 0.
    CASE pe_column.
      WHEN 'VIEW_BLOCK'.
        PERFORM show_block USING <ls_data>-id_block.
      WHEN 'VIEW_EXEC_LOG'.
        READ TABLE mo_monitor->mt_data ASSIGNING <ls_data> INDEX pe_row.
        IF sy-subrc = 0.
          PERFORM show_exec_log USING <ls_data>-id_block.
        ENDIF.
      WHEN 'VIEW_BLOCK_DATA'.
        PERFORM show_data_block USING <ls_data>-data <ls_data>-index_block.
    ENDCASE.
  ENDIF.
ENDFORM.
*&---------------------------------------------------------------------*
*& Form SHOW_BLOCK
*&---------------------------------------------------------------------*
*& text
*&---------------------------------------------------------------------*
FORM show_block  USING pe_id_block.

  mo_monitor->get_blocks_by_id( EXPORTING iv_id_block = pe_id_block ).

  IF mo_monitor->mt_blocks_data IS NOT INITIAL.

    TRY.
        cl_salv_table=>factory(
          IMPORTING
            r_salv_table = mo_alv_block
          CHANGING
            t_table      = mo_monitor->mt_blocks_data ).
      CATCH cx_salv_msg.                                "#EC NO_HANDLER
    ENDTRY.

    " Activamos las funciones del ALV
    DATA(lo_functions) = mo_alv_block->get_functions( ).
    lo_functions->set_all( abap_true ).

    " Layout
    DATA(lo_layout) = mo_alv_block->get_layout( ).


    " Adaptación del catalogo de campos
    PERFORM adapt_fieldcatalog_block.

    CREATE OBJECT mo_event_alv_block.

    DATA(lo_events) = mo_alv_block->get_event( ).

    SET HANDLER:
      mo_event_alv_block->on_link_click FOR lo_events,
      mo_event_alv_block->on_user_command FOR lo_events.

    " Se indica que se verá como un popup
    mo_alv_block->set_screen_popup( start_column = 2
                                    end_column  = 120
                                    start_line  = 2
                                    end_line    = 15 ).

    " Se muestra el ALV
    mo_alv_block->display( ).

  ELSE.
    MESSAGE s014.
  ENDIF.

ENDFORM.

FORM show_exec_log  USING pe_id_block.

  mo_monitor->get_exec_log_by_id( EXPORTING iv_id_block = pe_id_block ).

  IF mo_monitor->mt_exec_log_data IS NOT INITIAL.

    TRY.
        cl_salv_table=>factory(
          IMPORTING
            r_salv_table = mo_alv_exec_log
          CHANGING
            t_table      = mo_monitor->mt_exec_log_data ).
      CATCH cx_salv_msg.                                "#EC NO_HANDLER
    ENDTRY.

    " Activamos las funciones del ALV
    DATA(lo_functions) = mo_alv_exec_log->get_functions( ).
    lo_functions->set_all( abap_true ).

    " Layout
    DATA(lo_layout) = mo_alv_exec_log->get_layout( ).


    " Adaptación del catalogo de campos
*    PERFORM adapt_fieldcatalog_block.

    " Se indica que se verá como un popup
    mo_alv_exec_log->set_screen_popup( start_column = 2
                                    end_column  = 100
                                    start_line  = 2
                                    end_line    = 15 ).

    " Se muestra el ALV
    mo_alv_exec_log->display( ).

  ELSE.
    MESSAGE s014.
  ENDIF.

ENDFORM.
*&---------------------------------------------------------------------*
*& Form ADAPT_FIELDCATALOG_BLOCK
*&---------------------------------------------------------------------*
*& text
*&---------------------------------------------------------------------*
FORM adapt_fieldcatalog_block .

* Columnas
  DATA(lo_columns) = mo_alv_block->get_columns( ).

  lo_columns->set_optimize( abap_true ). " Optimizadas
  lo_columns->set_cell_type_column( 'CELLSTYLE' ).

* El campo de datos del bloque siempre se oculta
  DATA(lo_column) = CAST cl_salv_column_table( lo_columns->get_column( 'DATA' ) ).
  lo_column->set_visible( abap_false ).

  " Campos del bloque
  PERFORM fcat_fields_block USING 'B' CHANGING mo_alv_block .

ENDFORM.
*&---------------------------------------------------------------------*
*& Form FCAT_FIELDS_BLOCK
*&---------------------------------------------------------------------*
*& text
*&---------------------------------------------------------------------*
*&      --> MO_ALV_MONITOR
*&---------------------------------------------------------------------*
FORM fcat_fields_block  USING pe_source
                        CHANGING ps_alv TYPE REF TO cl_salv_table .
  DATA lv_long_text TYPE scrtext_l.
  DATA lv_short_text TYPE scrtext_s.
  DATA lv_medium_text TYPE scrtext_m.

* Columnas
  DATA(lo_columns) = ps_alv->get_columns( ).

  " Texto en determinadas columnas
  DATA(lo_column) = CAST cl_salv_column_table( lo_columns->get_column( 'POW_DIFF_SECONDS' ) ).
  lv_long_text = TEXT-c01.
  lo_column->set_long_text( lv_long_text ).

  lo_column = CAST cl_salv_column_table( lo_columns->get_column( 'POW_INIT_DATE' ) ).
  lv_short_text = lv_medium_text = lv_long_text = TEXT-c02.
  lo_column->set_long_text( lv_long_text ).
  lo_column->set_medium_text( lv_medium_text ).
  lo_column->set_short_text( lv_short_text ).

  lo_column = CAST cl_salv_column_table( lo_columns->get_column( 'POW_INIT_TIME' ) ).
  lv_short_text = lv_medium_text = lv_long_text = TEXT-c03.
  lo_column->set_long_text( lv_long_text ).
  lo_column->set_medium_text( lv_medium_text ).
  lo_column->set_short_text( lv_short_text ).

  lo_column = CAST cl_salv_column_table( lo_columns->get_column( 'POW_END_DATE' ) ).
  lv_short_text = lv_medium_text = lv_long_text = TEXT-c04.
  lo_column->set_long_text( lv_long_text ).
  lo_column->set_medium_text( lv_medium_text ).
  lo_column->set_short_text( lv_short_text ).

  lo_column = CAST cl_salv_column_table( lo_columns->get_column( 'POW_END_TIME' ) ).
  lv_short_text = lv_medium_text = lv_long_text = TEXT-c05.
  lo_column->set_long_text( lv_long_text ).
  lo_column->set_medium_text( lv_medium_text ).
  lo_column->set_short_text( lv_short_text ).

  " Ver el bloque solo se verá en el detalle
  lo_column = CAST cl_salv_column_table( lo_columns->get_column( 'VIEW_BLOCK_DATA' ) ).

  CASE pe_source.
    WHEN 'M'. " ALV Monitor
      lo_columns->set_column_position( columnname = 'VIEW_BLOCK_DATA' position = '12' ).
    WHEN 'B'. " ALV block
      lo_columns->set_column_position( columnname = 'VIEW_BLOCK_DATA' position = '7' ).
  ENDCASE.
  lo_column->set_symbol( abap_true ).
  lv_short_text = lv_medium_text = lv_long_text = TEXT-c08.
  lo_column->set_long_text( lv_long_text ).
  lo_column->set_medium_text( lv_medium_text ).
  lo_column->set_short_text( lv_short_text ).


* El campo de datos del bloque siempre se oculta
  lo_column = CAST cl_salv_column_table( lo_columns->get_column( 'DATA' ) ).
  lo_column->set_visible( abap_false ).

ENDFORM.
*&---------------------------------------------------------------------*
*& Form SHOW_DATA_BLOCK
*&---------------------------------------------------------------------*
*& text
*&---------------------------------------------------------------------*
FORM show_data_block  USING    pe_data pe_index.

  " Solución mala para saber si el contenido es JSON.
  IF pe_data(1) = '{'.
    cl_demo_output=>display_json( pe_data ).
  ELSE.
    cl_demo_output=>display( pe_data ).
  ENDIF.
ENDFORM.
*&---------------------------------------------------------------------*
*& Form HOTSPOT_BLOCK
*&---------------------------------------------------------------------*
*& text
*&---------------------------------------------------------------------*
*&      --> ROW
*&      --> COLUMN
*&---------------------------------------------------------------------*
FORM hotspot_block  USING    pe_row
                             pe_column.
  READ TABLE mo_monitor->mt_blocks_data ASSIGNING FIELD-SYMBOL(<ls_data>) INDEX pe_row.
  IF sy-subrc = 0.
    CASE pe_column.
      WHEN 'VIEW_BLOCK_DATA'.
        PERFORM show_data_block USING <ls_data>-data <ls_data>-index_block.
    ENDCASE.
  ENDIF.
ENDFORM.
