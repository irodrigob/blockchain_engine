CLASS zcl_bc_q_request DEFINITION
  PUBLIC
  INHERITING FROM /bobf/cl_lib_q_superclass
  FINAL
  CREATE PUBLIC .

  PUBLIC SECTION.
    METHODS /bobf/if_frw_query~query REDEFINITION.
  PROTECTED SECTION.

  PRIVATE SECTION.
ENDCLASS.



CLASS zcl_bc_q_request IMPLEMENTATION.
  METHOD /bobf/if_frw_query~query.


    " Se instancia una clase vacia para que no haya control de autorizacion
    DATA(lo_query_auth) = NEW zcl_bc_q_request_auth(  ).
    " Nota: El IT_REQUESTED_ATTRIBUTES debe tener los campos ya que sino el SADL produce un DUMP.

    CASE is_ctx-query_key.
      WHEN zif_bc_ddl_request_c=>sc_query-zbc_ddl_request-get_request.
        query_using_sadl(
          EXPORTING
            is_ctx                  = is_ctx
            io_query_authorities    = lo_query_auth " Se pone una clase a medida que no hace control de autorizaciones
            is_query_options        = is_query_options
            it_filter_key           = it_filter_key
            it_selection_parameters = it_selection_parameters
            iv_fill_data            = iv_fill_data
            it_requested_element    =  it_requested_attributes
          IMPORTING
            es_query_info           = es_query_info
            et_data                 = et_data
            et_key                  =  et_key ).
    ENDCASE.

  ENDMETHOD.

ENDCLASS.
