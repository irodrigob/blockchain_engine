INTERFACE zif_bc_data
  PUBLIC .
  TYPES: tt_rsparams TYPE STANDARD TABLE OF rsparamsl_255 WITH EMPTY KEY.

  CONSTANTS: BEGIN OF cs_status_block,
               completed  TYPE zbc_e_status_block VALUE 'CO',
               active     TYPE zbc_e_status_block VALUE 'AC',
               in_process TYPE zbc_e_status_block VALUE 'IP',
             END OF cs_status_block.

  CONSTANTS: BEGIN OF cs_range_numbers,
               object     TYPE nriv-object VALUE 'ZBC_ID_NUM',
               request_iv TYPE nriv_iv-nrrangenr VALUE '01',
               block_iv   TYPE nriv_iv-nrrangenr VALUE '02',
             END OF cs_range_numbers.
  CONSTANTS: BEGIN OF cs_message,
               id      TYPE sy-msgid VALUE 'ZBC',
               error   TYPE sy-msgty VALUE 'E',
               success TYPE sy-msgty VALUE 'S',
             END OF cs_message.
  CONSTANTS: BEGIN OF cs_enqueue,
               lock           TYPE enqmode VALUE 'E',
               unlock         TYPE enqmode VALUE 'D',
               relid          TYPE indx_relid VALUE 'BC',
               process_launch TYPE indx_srtfd VALUE 'ZLAUNCH_BLOCKCHAIN',
             END OF cs_enqueue.
  CONSTANTS: BEGIN OF cs_blockchain,
               number_blocks       TYPE i VALUE 5, " Number blocks in a chain
               data_genesis_block  TYPE string VALUE 'Genesis Block',
               algorithm           TYPE string VALUE 'SHA1',
               index_block_initial TYPE i VALUE 1,
               nonce_initial       TYPE i VALUE 0,
               proof_at_work       TYPE string VALUE '0000',

             END OF cs_blockchain.
  CONSTANTS: BEGIN OF cs_process_bc_custo,
               report_launch       TYPE syrepid VALUE 'ZBC_R_LAUNCH_BLOCKCHAIN',
               jobname_new_request TYPE tbtcjob-jobname VALUE 'ZBC_NEW_REQUEST',
             END OF cs_process_bc_custo.
  CONSTANTS: BEGIN OF cs_domain_with_text,
               status TYPE domname VALUE 'ZBC_D_STATUS_BLOCK',
             END OF cs_domain_with_text.
  TYPES: BEGIN OF ts_block_fields,
           index_block      TYPE zbc_e_index_block,
           prev_hash        TYPE zbc_e_prev_hash,
           timestamp        TYPE timestampl,
           nonce            TYPE zbc_e_nonce,
           data             TYPE zbc_e_data,
           id_request       TYPE zbc_e_id_request,
           hash             TYPE zbc_e_hash,
           pow_init_date    TYPE sydatum,
           pow_init_time    TYPE syuzeit,
           pow_end_date     TYPE sydatum,
           pow_end_time     TYPE syuzeit,
           pow_diff_seconds TYPE i,
         END OF ts_block_fields.
  TYPES: tt_block_fields TYPE STANDARD TABLE OF ts_block_fields WITH EMPTY KEY.
  TYPES: BEGIN OF ts_blockchain.
           INCLUDE TYPE zbc_s_bo_header.
           INCLUDE TYPE zbc_st_bo_header.
           INCLUDE TYPE ts_block_fields.
         TYPES:
                END OF ts_blockchain.
  TYPES: tt_blockchain TYPE STANDARD TABLE OF ts_blockchain WITH EMPTY KEY.
ENDINTERFACE.
