CLASS zcl_bc_util DEFINITION
  PUBLIC
  FINAL
  CREATE PUBLIC .

  PUBLIC SECTION.
    CLASS-METHODS fill_return
      IMPORTING
        !iv_type         TYPE any
        !iv_number       TYPE any
        !iv_message_v1   TYPE any OPTIONAL
        !iv_message_v2   TYPE any OPTIONAL
        !iv_message_v3   TYPE any OPTIONAL
        !iv_message_v4   TYPE any OPTIONAL
        !iv_id           TYPE symsgid OPTIONAL
        !iv_langu        TYPE sylangu DEFAULT sy-langu
      RETURNING
        VALUE(rs_return) TYPE bapiret2 .
    CLASS-METHODS get_id_request
      RETURNING VALUE(rv_id) TYPE zbc_e_id_request
      RAISING   zcx_bc.
    CLASS-METHODS get_id_block
      RETURNING VALUE(rv_id) TYPE zbc_e_id_block
      RAISING   zcx_bc.
    CLASS-METHODS conv_message_bopf_2_return
      IMPORTING
        io_message TYPE REF TO /bobf/if_frw_message
        iv_langu   TYPE sylangu DEFAULT sy-langu
      CHANGING
        ct_return  TYPE bapiret2_t.
    CLASS-METHODS datetime_diff
      IMPORTING
        iv_date_from    TYPE d
        iv_date_to      TYPE d
        iv_time_from    TYPE t OPTIONAL
        iv_time_to      TYPE t OPTIONAL
      EXPORTING
        ev_diff_seconds TYPE int4
        ev_diff_minutes TYPE int4.
    CLASS-METHODS execute_report
      IMPORTING
        VALUE(it_params)  TYPE zif_bc_data=>tt_rsparams
        VALUE(iv_jobname) TYPE tbtcjob-jobname
        VALUE(iv_report)  TYPE syrepid
        VALUE(iv_batch)   TYPE sap_bool
        VALUE(iv_langu)   TYPE sylangu DEFAULT sy-langu
      EXPORTING
        !et_return        TYPE bapiret2_t
        !ev_jobcount      TYPE tbtcjob-jobcount.
    CLASS-METHODS convert_data_2_request
      IMPORTING
                iv_data          TYPE any
      RETURNING
                VALUE(rv_result) TYPE string
      RAISING   zcx_bc.
    CLASS-METHODS conv_params_2_ranges
      IMPORTING
        !iv_paramname            TYPE any
        !it_params_sl        TYPE pivb_rsparamsl_255_t
        !iv_apply_conversion TYPE abap_bool DEFAULT abap_false
      EXPORTING
        !et_r_ranges          TYPE STANDARD TABLE .
    CLASS-METHODS conv_params_2_value
      IMPORTING
        !iv_paramname TYPE any
        !it_params_sl TYPE pivb_rsparamsl_255_t
      EXPORTING
        !ev_value     TYPE any.
  PROTECTED SECTION.


  PRIVATE SECTION.
ENDCLASS.



CLASS zcl_bc_util IMPLEMENTATION.

  METHOD fill_return.

    CLEAR rs_return.

    rs_return-type = iv_type.

    rs_return-id = COND #( WHEN iv_id IS NOT INITIAL THEN iv_id ELSE zif_bc_data=>cs_message-id ).
    rs_return-number = iv_number.
    rs_return-message_v1 = iv_message_v1.
    rs_return-message_v2 = iv_message_v2.
    rs_return-message_v3 = iv_message_v3.
    rs_return-message_v4 = iv_message_v4.


    CALL FUNCTION 'BAPI_MESSAGE_GETDETAIL'
      EXPORTING
        id         = rs_return-id
        number     = rs_return-number
        language   = iv_langu
        textformat = 'ASC'
        message_v1 = rs_return-message_v1
        message_v2 = rs_return-message_v2
        message_v3 = rs_return-message_v3
        message_v4 = rs_return-message_v4
      IMPORTING
        message    = rs_return-message.

  ENDMETHOD.
  METHOD get_id_request.
    CLEAR rv_id.
    CALL FUNCTION 'NUMBER_GET_NEXT'
      EXPORTING
        nr_range_nr             = zif_bc_data=>cs_range_numbers-request_iv
        object                  = zif_bc_data=>cs_range_numbers-object
      IMPORTING
        number                  = rv_id
      EXCEPTIONS
        interval_not_found      = 1
        number_range_not_intern = 2
        object_not_found        = 3
        quantity_is_0           = 4
        quantity_is_not_1       = 5
        interval_overflow       = 6
        buffer_overflow         = 7
        OTHERS                  = 8.
    IF sy-subrc NE 0.
      RAISE EXCEPTION TYPE zcx_bc
        EXPORTING
          textid = zcx_bc=>get_id.
    ENDIF.
  ENDMETHOD.

  METHOD get_id_block.
    CLEAR rv_id.
    CALL FUNCTION 'NUMBER_GET_NEXT'
      EXPORTING
        nr_range_nr             = zif_bc_data=>cs_range_numbers-block_iv
        object                  = zif_bc_data=>cs_range_numbers-object
      IMPORTING
        number                  = rv_id
      EXCEPTIONS
        interval_not_found      = 1
        number_range_not_intern = 2
        object_not_found        = 3
        quantity_is_0           = 4
        quantity_is_not_1       = 5
        interval_overflow       = 6
        buffer_overflow         = 7
        OTHERS                  = 8.
    IF sy-subrc NE 0.
      RAISE EXCEPTION TYPE zcx_bc
        EXPORTING
          textid = zcx_bc=>get_id.
    ENDIF.
  ENDMETHOD.


  METHOD conv_message_bopf_2_return.
    io_message->get_messages( IMPORTING et_message = DATA(lt_messages) ).

    LOOP AT lt_messages ASSIGNING FIELD-SYMBOL(<ls_messages>).

      DATA(lo_msg) = CAST /bobf/cm_frw_symsg( <ls_messages>-message ).

      INSERT zcl_bc_util=>fill_return( iv_type = zif_bc_data=>cs_message-error
                                         iv_number = <ls_messages>-message->if_t100_message~t100key-msgno
                                         iv_id = <ls_messages>-message->if_t100_message~t100key-msgid
                                         iv_message_v1 = lo_msg->mv_attr1
                                         iv_message_v2 = lo_msg->mv_attr2
                                         iv_message_v3 = lo_msg->mv_attr3
                                         iv_message_v4 = lo_msg->mv_attr4
                                         iv_langu = iv_langu ) INTO TABLE ct_return.
    ENDLOOP.
  ENDMETHOD.

  METHOD datetime_diff.
    DATA lv_date_from TYPE d.
    DATA lv_date_to TYPE d.
    DATA lv_time_from TYPE t.
    DATA lv_time_to TYPE t.

    CLEAR: ev_diff_minutes, ev_diff_seconds.

    " En los procesos no ocurrirá pero pongo la lógica por si ponen la fecha al revés para girarlas.
    IF iv_date_from > iv_date_to OR
     ( iv_date_from = iv_date_to AND
       iv_time_from > iv_time_to ).

      lv_date_from = iv_date_to.
      lv_date_to = iv_date_from.
      lv_time_from = iv_time_to.
      lv_time_to = iv_time_from.
    ELSE.
      lv_date_from = iv_date_from.
      lv_date_to = iv_date_to.
      lv_time_from = iv_time_from.
      lv_time_to = iv_time_to.
    ENDIF.

    " Calculo en minutos.
    " 1440 minutos por día, 60 segundos por minuto
    ev_diff_minutes = ( ( lv_date_to - lv_date_from ) * 1440 ) + ( ( lv_time_to - lv_time_from ) / 60  ).

    " 86400 segundos por día
    ev_diff_seconds = ( ( lv_date_to - lv_date_from ) * 86400 ) + ( lv_time_to - lv_time_from ).


  ENDMETHOD.


  METHOD execute_report.
    DATA lv_ret TYPE i.
    DATA lt_params TYPE zif_bc_data=>tt_rsparams.

    CLEAR: et_return, ev_jobcount.

    " Si no se quiere en fondo se lanza tal cual
    IF iv_batch = abap_false.
      SUBMIT (iv_report)
              WITH SELECTION-TABLE it_params
              AND RETURN.
    ELSE.
      CALL FUNCTION 'JOB_OPEN'
        EXPORTING
          jobname          = iv_jobname
          jobclass         = 'B'
        IMPORTING
          jobcount         = ev_jobcount
        CHANGING
          ret              = lv_ret
        EXCEPTIONS
          cant_create_job  = 1
          invalid_job_data = 2
          jobname_missing  = 3
          OTHERS           = 4.
      IF sy-subrc = 0 AND lv_ret IS INITIAL.

        SUBMIT (iv_report)
          WITH SELECTION-TABLE it_params
           VIA JOB iv_jobname NUMBER ev_jobcount
          AND RETURN.

        CALL FUNCTION 'JOB_CLOSE'
          EXPORTING
            jobcount             = ev_jobcount
            jobname              = iv_jobname
            strtimmed            = abap_true
          CHANGING
            ret                  = lv_ret
          EXCEPTIONS
            cant_start_immediate = 1
            invalid_startdate    = 2
            jobname_missing      = 3
            job_close_failed     = 4
            job_nosteps          = 5
            job_notex            = 6
            lock_failed          = 7
            invalid_target       = 8
            OTHERS               = 9.
        IF sy-subrc = 0 AND lv_ret IS INITIAL. " Sin errores
          INSERT zcl_bc_util=>fill_return( iv_type = zif_bc_data=>cs_message-success
                                iv_number =  '012'
                                iv_message_v1 = iv_jobname ) INTO TABLE et_return.
        ELSE.
          INSERT zcl_bc_util=>fill_return( iv_type = sy-msgty
                                 iv_id = sy-msgid
                                 iv_number =  sy-msgno
                                 iv_message_v1 = sy-msgv1
                                 iv_message_v2 = sy-msgv2
                                 iv_message_v3 = sy-msgv3
                                 iv_message_v4 = sy-msgv4 ) INTO TABLE et_return.
        ENDIF.
      ELSE.
        INSERT zcl_bc_util=>fill_return( iv_type = sy-msgty
                                  iv_id = sy-msgid
                                  iv_number =  sy-msgno
                                  iv_message_v1 = sy-msgv1
                                  iv_message_v2 = sy-msgv2
                                  iv_message_v3 = sy-msgv3
                                  iv_message_v4 = sy-msgv4 ) INTO TABLE et_return.
      ENDIF.
    ENDIF.

  ENDMETHOD.

  METHOD convert_data_2_request.
    DATA(lo_data_type) = cl_abap_typedescr=>describe_by_data( iv_data ).

    CASE lo_data_type->kind.
      WHEN cl_abap_typedescr=>kind_elem. " Tipo elemental se devuelve tal cual
        rv_result = iv_data.
        " Para tablas o estructura se convierte a JSON
      WHEN cl_abap_typedescr=>kind_struct OR cl_abap_typedescr=>kind_table.
        rv_result = /ui2/cl_json=>serialize( data = iv_data
                                             compress = abap_true
                                             pretty_name = /ui2/cl_json=>pretty_mode-camel_case ).
      WHEN OTHERS.
        RAISE EXCEPTION TYPE zcx_bc
          EXPORTING
            textid = zcx_bc=>type_data_not_valid.
    ENDCASE.
  ENDMETHOD.
  METHOD conv_params_2_ranges.
    FIELD-SYMBOLS <ls_params> TYPE LINE OF pivb_rsparamsl_255_t.
    FIELD-SYMBOLS <campo> TYPE any.
    FIELD-SYMBOLS <ls_ranges> TYPE any.
    DATA: ls_ranges TYPE REF TO data.

*   Crear una Work area del mismo tipo que una linea del ranges pasado por parámetro
    CREATE DATA ls_ranges LIKE LINE OF et_r_ranges.
    ASSIGN ls_ranges->* TO <ls_ranges>.

*   buscamos la mascara de edición adecuada:
    IF iv_apply_conversion EQ abap_true.
      ASSIGN COMPONENT 'LOW' OF STRUCTURE <ls_ranges> TO <campo>.
      IF sy-subrc EQ 0.
        DATA(lo_elem_descr) = CAST cl_abap_elemdescr( cl_abap_datadescr=>describe_by_data( <campo> ) ).
        IF lo_elem_descr->edit_mask IS NOT INITIAL.
          DATA(lv_fm_name) = |CONVERSION_EXIT_{ lo_elem_descr->edit_mask+2 }_INPUT|.
        ENDIF.
      ENDIF.
      UNASSIGN <campo>.
    ENDIF.

    CLEAR et_r_ranges.
    LOOP AT it_params_sl ASSIGNING <ls_params> WHERE selname = iv_paramname
                                                  AND ( low IS NOT INITIAL OR
                                                        option IS NOT INITIAL ).

      CASE <ls_params>-kind.
        WHEN 'S'.

          IF <ls_params>-sign IS NOT INITIAL AND <ls_params>-option IS NOT INITIAL.
*   Asignamos campo Low. Si esta en blanco, no proceso el registro.
            ASSIGN COMPONENT 'LOW' OF STRUCTURE <ls_ranges> TO <campo>.
            IF sy-subrc = 0.

              IF iv_apply_conversion EQ abap_true AND lv_fm_name IS NOT INITIAL.
                CALL FUNCTION lv_fm_name
                  EXPORTING
                    input  = <ls_params>-low
                  IMPORTING
                    output = <campo>
                  EXCEPTIONS
                    OTHERS = 1.
              ELSE.
                <campo> = <ls_params>-low.
              ENDIF.

*   Asignamos y llenamos campo Sign
              ASSIGN COMPONENT 'SIGN'   OF STRUCTURE <ls_ranges> TO <campo>.
              IF sy-subrc = 0.
                <campo> = <ls_params>-sign.
              ENDIF.

*   Asignamos y llenamos campo Option
              ASSIGN COMPONENT 'OPTION' OF STRUCTURE <ls_ranges> TO <campo>.
              IF sy-subrc = 0.
                <campo> = <ls_params>-option.
              ENDIF.

*   Asignamos campo high
              ASSIGN COMPONENT 'HIGH' OF STRUCTURE <ls_ranges> TO <campo>.
              IF sy-subrc = 0.
                IF <ls_params>-high IS NOT INITIAL.
                  IF iv_apply_conversion EQ abap_true AND lv_fm_name IS NOT INITIAL.
                    CALL FUNCTION lv_fm_name
                      EXPORTING
                        input  = <ls_params>-low
                      IMPORTING
                        output = <campo>
                      EXCEPTIONS
                        OTHERS = 1.
                  ELSE.
                    <campo> = <ls_params>-high.
                  ENDIF.
                ENDIF.
              ENDIF.

              APPEND <ls_ranges> TO et_r_ranges.

            ENDIF.

          ENDIF.
        WHEN 'P'.
*   Asignamos y llenamos campo Sign
          ASSIGN COMPONENT 'SIGN'   OF STRUCTURE <ls_ranges> TO <campo>.
          IF sy-subrc = 0.
            <campo> = 'I'.
          ENDIF.
*   Asignamos y llenamos campo Option
          ASSIGN COMPONENT 'OPTION' OF STRUCTURE <ls_ranges> TO <campo>.
          IF sy-subrc = 0.
            <campo> = 'EQ'.
          ENDIF.
*   Asignamos y llenamos campo Option
          ASSIGN COMPONENT 'LOW' OF STRUCTURE <ls_ranges> TO <campo>.
          IF sy-subrc = 0.
            IF iv_apply_conversion EQ abap_true AND lv_fm_name IS NOT INITIAL.
              CALL FUNCTION lv_fm_name
                EXPORTING
                  input  = <ls_params>-low
                IMPORTING
                  output = <campo>
                EXCEPTIONS
                  OTHERS = 1.
            ELSE.
              <campo> = <ls_params>-low.
            ENDIF.
          ENDIF.
          APPEND <ls_ranges> TO et_r_ranges.
      ENDCASE.

    ENDLOOP.
  ENDMETHOD.


  METHOD conv_params_2_value.
    FIELD-SYMBOLS <ls_params> TYPE LINE OF pivb_rsparamsl_255_t.
    DATA ld_field TYPE string.

* Busco la condicion seleccionada en la pantalla de seleccion.
    READ TABLE it_params_sl ASSIGNING <ls_params> WITH KEY selname = iv_paramname.
    IF sy-subrc = 0.
      ev_value = <ls_params>-low.
    ENDIF.
  ENDMETHOD.

ENDCLASS.
