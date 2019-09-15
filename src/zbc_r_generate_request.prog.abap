*&---------------------------------------------------------------------*
*& Report ZBC_R_GENERATE_REQUEST
*&---------------------------------------------------------------------*
*&
*&---------------------------------------------------------------------*
REPORT zbc_r_generate_request.

PARAMETERS p_number TYPE i DEFAULT 1.
PARAMETERS p_lbc AS CHECKBOX DEFAULT abap_false.

START-OF-SELECTION.

  DATA(mo_request) = NEW zcl_bc_helper_request( ).

  SELECT  * INTO TABLE @DATA(lt_bkpf) UP TO @p_number ROWS
         FROM bkpf.

  LOOP AT lt_bkpf ASSIGNING FIELD-SYMBOL(<ls_bkpf>).
    mo_request->add_request( EXPORTING iv_data = <ls_bkpf>
                             IMPORTING ev_id_request = DATA(lv_id)
                                       et_return = DATA(lt_return) ).
    " Si hay algun error lo muestro.
    IF lt_return IS NOT INITIAL.
      READ TABLE lt_return  ASSIGNING FIELD-SYMBOL(<ls_return>) INDEX 1.
      WRITE:/ 'Error message: ', <ls_return>-message.
    ELSE.
      WRITE:/ 'ID Request: ', lv_id.

      READ TABLE lt_return  ASSIGNING <ls_return> INDEX 1.
      IF sy-subrc = 0.
        WRITE:/ <ls_return>-message.
      ENDIF.

    ENDIF.
  ENDLOOP.

  IF p_lbc = abap_true. " Se lanza el proceso de blockchain
    mo_request->launch_blockchain( EXPORTING iv_batch  = abap_true
                                   IMPORTING et_return = lt_return ).
    READ TABLE lt_return  ASSIGNING <ls_return> INDEX 1.
    IF sy-subrc = 0.
      WRITE:/ <ls_return>-message.
    ENDIF.
  ENDIF.
