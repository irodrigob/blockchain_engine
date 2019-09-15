*&---------------------------------------------------------------------*
*& Include          ZBC_R_MONITOR_BLOCKCHAIN_TOP
*&---------------------------------------------------------------------*
*----------------------------------------------------------------------*
* Tables
*----------------------------------------------------------------------*
TABLES: zbc_s_moni_sel_screen.

*----------------------------------------------------------------------*
* Variables
*----------------------------------------------------------------------*

* Monitor controller
CLASS lcl_monitor DEFINITION DEFERRED.
DATA mo_monitor TYPE REF TO lcl_monitor.

* ALV
* Monitor
DATA mo_alv_monitor TYPE REF TO cl_salv_table.
CLASS lcl_event_alv_monitor DEFINITION DEFERRED.
DATA mo_event_alv_monitor TYPE REF TO lcl_event_alv_monitor.

* Bloque
DATA mo_alv_block TYPE REF TO cl_salv_table.
CLASS lcl_event_alv_block DEFINITION DEFERRED.
DATA mo_event_alv_block TYPE REF TO lcl_event_alv_block.

* Log de ejecución
DATA mo_alv_exec_log TYPE REF TO cl_salv_table.
