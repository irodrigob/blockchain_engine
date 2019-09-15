*&---------------------------------------------------------------------*
*& Include          ZBC_R_MONITOR_BLOCKCHAIN_C02
*&---------------------------------------------------------------------*
CLASS lcl_event_alv_monitor DEFINITION.
  PUBLIC SECTION.

    METHODS: on_link_click FOR EVENT link_click OF cl_salv_events_table
      IMPORTING row column,
      on_user_command FOR EVENT added_function OF cl_salv_events
        IMPORTING e_salv_function.
ENDCLASS.

CLASS lcl_event_alv_monitor IMPLEMENTATION.
  METHOD on_link_click.
    PERFORM hotspot USING row column.
  ENDMETHOD.

  METHOD on_user_command.
  ENDMETHOD.

ENDCLASS.

CLASS lcl_event_alv_block DEFINITION.
  PUBLIC SECTION.

    METHODS: on_link_click FOR EVENT link_click OF cl_salv_events_table
      IMPORTING row column,
      on_user_command FOR EVENT added_function OF cl_salv_events
        IMPORTING e_salv_function.
ENDCLASS.

CLASS lcl_event_alv_block IMPLEMENTATION.
  METHOD on_link_click.
    PERFORM hotspot_block USING row column.
  ENDMETHOD.

  METHOD on_user_command.
  ENDMETHOD.

ENDCLASS.
