CLASS zcl_bc_q_request_auth DEFINITION
  PUBLIC
  FINAL
  CREATE PUBLIC .

  PUBLIC SECTION.

    INTERFACES /bobf/if_frw_authority_query .
  PROTECTED SECTION.
  PRIVATE SECTION.
ENDCLASS.



CLASS zcl_bc_q_request_auth IMPLEMENTATION.
  METHOD /bobf/if_frw_authority_query~check_authority.

  ENDMETHOD.

  METHOD /bobf/if_frw_authority_query~get_authority_profile_reader.

  ENDMETHOD.

  METHOD /bobf/if_frw_authority_query~get_query_condition_provider.

  ENDMETHOD.

  METHOD /bobf/if_frw_authority_query~is_privileged.
  ENDMETHOD.

ENDCLASS.
