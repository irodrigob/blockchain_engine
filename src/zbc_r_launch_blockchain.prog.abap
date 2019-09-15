*&---------------------------------------------------------------------*
*& Report zbc_r_launch_blockchain
*&---------------------------------------------------------------------*
*&
*&---------------------------------------------------------------------*
REPORT zbc_r_launch_blchain_new_req MESSAGE-ID zbc.

*----------------------------------------------------------------------*
* Variables
*----------------------------------------------------------------------*
TABLES: zbc_t_bo_header.

DATA mo_alv   TYPE REF TO cl_salv_table.
DATA mt_blockchain TYPE zif_bc_data=>tt_blockchain.
DATA mt_return TYPE bapiret2_t.

DATA mo_blockchain TYPE REF TO zcl_bc_blockchain.

*----------------------------------------------------------------------*
* Selection screen
*----------------------------------------------------------------------*
" Launch for new request
PARAMETERS p_nreq RADIOBUTTON GROUP g1.
SELECTION-SCREEN BEGIN OF BLOCK b1 WITH FRAME TITLE TEXT-t01.
PARAMETERS p_rnreq AS CHECKBOX DEFAULT abap_true.
SELECTION-SCREEN END OF BLOCK b1.

* Reprocess a id block
PARAMETERS p_rre RADIOBUTTON GROUP g1.
SELECTION-SCREEN BEGIN OF BLOCK b2 WITH FRAME TITLE TEXT-t02.
PARAMETERS p_idbl TYPE zbc_e_id_block.
SELECTION-SCREEN END OF BLOCK b2.

*----------------------------------------------------------------------*
* Start of selection
*----------------------------------------------------------------------*
START-OF-SELECTION.

  " Initializacion of data
  PERFORM initialization.


  IF p_nreq = abap_true. " Process for new request
    PERFORM new_request.
  ENDIF.

*----------------------------------------------------------------------*
* Display data
*----------------------------------------------------------------------*
END-OF-SELECTION.

  IF mt_return IS INITIAL.

    PERFORM show_data.

  ELSE.
    " Se muestra el primer mensaje, que generalmente solo habrá uno
    READ TABLE mt_return ASSIGNING FIELD-SYMBOL(<ls_return>) INDEX 1.
    MESSAGE ID <ls_return>-id TYPE 'S' NUMBER <ls_return>-number
            DISPLAY LIKE <ls_return>-type
            WITH <ls_return>-message_v1 <ls_return>-message_v2 <ls_return>-message_v3 <ls_return>-message_v4.

  ENDIF.

*&---------------------------------------------------------------------*
*& Form INITIALIZATION
*&---------------------------------------------------------------------*
FORM initialization .
  mo_blockchain = NEW zcl_bc_blockchain( ).
ENDFORM.
*&---------------------------------------------------------------------*
*& Form SHOW_DATA
*&---------------------------------------------------------------------*
*& text
*&---------------------------------------------------------------------*
FORM show_data .
  DATA lv_long_text TYPE scrtext_l.
  DATA lv_short_text TYPE scrtext_s.
  DATA lv_medium_text TYPE scrtext_m.
  TRY.
      cl_salv_table=>factory(
        IMPORTING
          r_salv_table = mo_alv
        CHANGING
          t_table      = mt_blockchain ).
    CATCH cx_salv_msg.                                  "#EC NO_HANDLER
  ENDTRY.

  " Activamos las funciones del ALV
  DATA(lo_functions) = mo_alv->get_functions( ).
  lo_functions->set_all( abap_true ).

* Columnas
  DATA(lo_columns) = mo_alv->get_columns( ).

  lo_columns->set_optimize( abap_true ). " Optimizadas

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

  lo_columns->set_column_position( columnname = 'STATUS_TEXT' position = '3' ).


  " Se muestra el ALV
  mo_alv->display( ).

ENDFORM.
*&---------------------------------------------------------------------*
*& Form NEW_REQUEST
*&---------------------------------------------------------------------*
*& text
*&---------------------------------------------------------------------*
FORM new_request .
  TRY.
      mo_blockchain->process_new_request(
        IMPORTING
          et_return = mt_return
          et_data = mt_blockchain ).

      " Si se ha marcado que se requiere que se procese de nuevo en caso de haber peticiones se hace los siguientes pasos:
      " 1) Primero se mira si hay request, si las hay se lanza el proceso
      IF p_rnreq = abap_true.
        DATA(lo_request) = NEW zcl_bc_helper_request( ).

        " Solo recuperamos una, porque es la que necesitamos para saber si queremos procesar o no el bloque
        lo_request->get_request( EXPORTING iv_number_rows = 1
                                 IMPORTING et_data        = DATA(lt_request) ).
        IF lt_request IS NOT INITIAL.
          lo_request->launch_blockchain( EXPORTING iv_batch    = abap_true
                                         IMPORTING et_return   = DATA(lt_return)
                                                   ev_jobcount = DATA(lv_jobcount)
                                                   ev_jobname  = DATA(lv_jobname) ).

          " El mensaje que se devuelve se muestra como un success
          READ TABLE lt_return ASSIGNING FIELD-SYMBOL(<ls_return>) INDEX 1.
          IF sy-subrc = 0.
            MESSAGE ID <ls_return>-id TYPE zif_bc_data=>cs_message-success NUMBER <ls_return>-number
                    WITH <ls_return>-message_v1 <ls_return>-message_v2 <ls_return>-message_v3 <ls_return>-message_v4.
          ENDIF.
        ENDIF.
      ENDIF.


    CATCH zcx_bc INTO DATA(lx_bc). " BC - Exception class
      MESSAGE lx_bc TYPE 'S'.
  ENDTRY.
ENDFORM.
