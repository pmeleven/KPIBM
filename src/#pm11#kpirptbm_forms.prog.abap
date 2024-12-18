*&---------------------------------------------------------------------*
*&  Include           /PM11/KPIREP2_FORMS
*&---------------------------------------------------------------------*
FORM s_rpi_f4 CHANGING p_rpi TYPE any.

  DATA: lt_return  TYPE STANDARD TABLE OF ddshretval.

* Get all KPIIDs defined in the system.
  SELECT kpiid
    FROM /pm11/kpirptbm
    INTO TABLE @DATA(lt_kpiid).

* For the KPIIDs, get the description for that
  IF lt_kpiid[] IS NOT INITIAL.
    SELECT *
      FROM /pm11/kpirptbml
      INTO TABLE @DATA(lt_kpiid_text)
       FOR ALL ENTRIES IN @lt_kpiid
     WHERE kpiid = @lt_kpiid-kpiid
       AND spras = @sy-langu.
  ENDIF.

* Display the lookup for KPIID with the KPIID and Description.
* Pass the selected KPIID back to the selection parameter
  CALL FUNCTION 'F4IF_INT_TABLE_VALUE_REQUEST'
    EXPORTING
      retfield        = 'KPIID'
      value_org       = 'S'
    TABLES
      value_tab       = lt_kpiid_text
      return_tab      = lt_return
    EXCEPTIONS
      parameter_error = 1
      no_values_found = 2
      OTHERS          = 3.
  IF sy-subrc = 0.
    TRY.
        p_rpi = lt_return[ 1 ]-fieldval.
      CATCH cx_sy_itab_line_not_found.
    ENDTRY.
  ENDIF.

ENDFORM.

*&---------------------------------------------------------------------*
*&      Form  GET_DATA
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM get_data .

  DATA: lt_werks  TYPE /pm11/werks_range_t,
        lt_arbpl  TYPE /pm11/arbpl_range_t,
        lt_rpi    TYPE /pm11/kpiid_range_t,
        ls_return TYPE bapiret2.

* Convert ranges to the form of internal tables to pass to the FM.
  lt_werks[] = s_werks[].
  lt_arbpl[] = s_arbpl[].
  lt_rpi[]   = s_rpi[].

* Call the main RFC to fetch the data and calculate the RPI result
  CALL FUNCTION '/PM11/FM_KPIBM'
    EXPORTING
      ir_werks     = lt_werks
      ir_arbpl     = lt_arbpl
      ir_rpi       = lt_rpi
    IMPORTING
      et_output    = gt_output
      es_return    = ls_return.

  IF ls_return-type = 'E'.
    MESSAGE ls_return-message TYPE 'E'.
    LEAVE LIST-PROCESSING.
  ENDIF.

ENDFORM.
*&---------------------------------------------------------------------*
*&      Form  DISPLAY_DATA
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM display_data .

* Create an object for the ALV display
  TRY.
      cl_salv_table=>factory(
        EXPORTING
          r_container    = cl_gui_container=>screen0
      IMPORTING
        r_salv_table = go_alv
      CHANGING
        t_table = gt_output ).
    CATCH cx_salv_msg INTO DATA(lx_msg).
  ENDTRY.

  DATA: l_icon    TYPE string,
        l_text    TYPE string,
        l_tooltip TYPE string.

  l_icon    = icon_refresh.
  l_text    = TEXT-002.
  l_tooltip = TEXT-002.

* Display all standard ALV functions.
  DATA: lr_functions TYPE REF TO cl_salv_functions.
*  DATA(lo_functions) = go_alv->get_functions( ).
  lr_functions = go_alv->get_functions( ).
  lr_functions->set_all( if_salv_c_bool_sap=>true ).

  TRY.
      lr_functions->add_function(  name     = 'REFRESH'
                                   icon     = l_icon
                                   text     = l_text
                                   tooltip  = l_tooltip
                                   position = if_salv_c_function_position=>left_of_salv_functions ).
    CATCH cx_salv_existing cx_salv_wrong_call.
  ENDTRY.

  DATA: lr_events                TYPE REF TO cl_salv_events_table.
  DATA: lr_handle_events         TYPE REF TO lcl_handle_event.
  lr_events = go_alv->get_event( ).
  CREATE OBJECT lr_handle_events .
  SET HANDLER lr_handle_events->on_user_command          FOR lr_events.

  DATA: lr_display   TYPE REF TO cl_salv_display_settings.
  DATA: l_title TYPE lvc_title.

  l_title = TEXT-t01.
  lr_display = go_alv->get_display_settings( ).
  lr_display->set_list_header( l_title ).
  lr_display->set_striped_pattern( cl_salv_display_settings=>true ).

  DATA: lr_columns TYPE REF TO cl_salv_columns_table,
        lr_column  TYPE REF TO cl_salv_column.

  lr_columns = go_alv->get_columns( ).

  TRY.
      lr_column = lr_columns->get_column( 'SWERK' ).
      lr_column->set_technical( if_salv_c_bool_sap=>false ).
      lr_column->set_short_text( 'Para.' ).
      lr_column->set_medium_text( 'Parameter' ).
      lr_column->set_long_text( 'Parameter' ).
    CATCH cx_salv_not_found.
  ENDTRY.

  TRY.
      lr_column = lr_columns->get_column( 'DATE_CALCULATED' ).
      lr_column->set_technical( if_salv_c_bool_sap=>false ).
      lr_column->set_short_text( 'Date' ).
      lr_column->set_medium_text( 'Date Calculated' ).
      lr_column->set_long_text( 'Date KPI Calculated On' ).
    CATCH cx_salv_not_found.
  ENDTRY.

* Display Data
  go_alv->display( ).
  WRITE '.'.

ENDFORM.
*&---------------------------------------------------------------------*
*&      Form  F_DEFAULT_DATES
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM f_default_dates .
  IF s_werks[] IS NOT INITIAL AND s_arbpl[] IS NOT INITIAL.

    SELECT objid
      FROM crhd
      INTO TABLE @DATA(lt_arbid)
      WHERE werks IN @s_werks
      AND arbpl IN @s_arbpl.

  ENDIF.
ENDFORM.
