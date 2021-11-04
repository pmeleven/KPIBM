*&---------------------------------------------------------------------*
*&  Include           /PM11/KPIREP2_CLS
*&---------------------------------------------------------------------*

CLASS lcl_handle_event DEFINITION.
  PUBLIC SECTION.
    METHODS:
      on_user_command FOR EVENT added_function OF cl_salv_events
        IMPORTING e_salv_function.
ENDCLASS.

CLASS lcl_handle_event IMPLEMENTATION.
  METHOD on_user_command.

    CASE e_salv_function.
      WHEN 'REFRESH' .
        PERFORM get_data.
        go_alv->refresh( ).
    ENDCASE.
  ENDMETHOD.
ENDCLASS.
