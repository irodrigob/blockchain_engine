*&---------------------------------------------------------------------*
*& Report ZBC_R_MONITOR_BLOCKCHAIN
*&---------------------------------------------------------------------*
*&
*&---------------------------------------------------------------------*
REPORT zbc_r_monitor_blockchain MESSAGE-ID zbc.

INCLUDE zbc_r_monitor_blockchain_top.
INCLUDE zbc_r_monitor_blockchain_c01. " Main local class
INCLUDE zbc_r_monitor_blockchain_c02. " Events ALV monitor

*----------------------------------------------------------------------*
* Selection screen
*----------------------------------------------------------------------*
SELECTION-SCREEN BEGIN OF BLOCK b1 WITH FRAME TITLE TEXT-b01.
SELECT-OPTIONS: s_iblock FOR zbc_s_moni_sel_screen-id_block,
                s_ireq FOR zbc_s_moni_sel_screen-id_request,
                s_status FOR zbc_s_moni_sel_screen-status,
                s_erdat FOR zbc_s_moni_sel_screen-erdat,
                s_erzet FOR zbc_s_moni_sel_screen-erzet,
                s_ernam FOR zbc_s_moni_sel_screen-ernam,
                s_aedat FOR zbc_s_moni_sel_screen-aedat,
                s_aetime FOR zbc_s_moni_sel_screen-aetime,
                s_auname FOR zbc_s_moni_sel_screen-auname.
SELECTION-SCREEN END OF BLOCK b1.

SELECTION-SCREEN BEGIN OF BLOCK b2 WITH FRAME TITLE TEXT-b02.
PARAMETERS: p_summ   RADIOBUTTON GROUP g1,
            p_detail RADIOBUTTON GROUP g1.
SELECTION-SCREEN END OF BLOCK b2.

*----------------------------------------------------------------------*
* Start of selection
*----------------------------------------------------------------------*
START-OF-SELECTION.

  " Initializacion of data
  PERFORM initialization.

  " Search data
  PERFORM search_data.

*----------------------------------------------------------------------*
* Display data
*----------------------------------------------------------------------*
END-OF-SELECTION.

  IF mo_monitor->mt_data IS NOT INITIAL.
    PERFORM show_data.
  ELSE.
    MESSAGE s013.
  ENDIF.


  INCLUDE zbc_r_monitor_blockchain_f01.
