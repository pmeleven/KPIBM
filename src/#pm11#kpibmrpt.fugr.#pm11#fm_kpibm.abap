FUNCTION /PM11/FM_KPIBM.
*"----------------------------------------------------------------------
*"*"Local Interface:
*"  IMPORTING
*"     VALUE(IR_WERKS) TYPE  /PM11/WERKS_RANGE_T
*"     VALUE(IR_ARBPL) TYPE  /PM11/ARBPL_RANGE_T
*"     VALUE(IR_RPI) TYPE  /PM11/KPIID_RANGE_T
*"  EXPORTING
*"     VALUE(ET_OUTPUT) TYPE  /PM11/KPIREP2T_RESULTS
*"     VALUE(ES_RETURN) TYPE  BAPIRET2
*"----------------------------------------------------------------------

  DATA: lt_output TYPE /pm11/kpirep2t_results,
        lt_crhd_t TYPE /pm11/kpirep2t_crhd,
        lt_crhd   TYPE /pm11/kpirep2t_crhd,
        ls_werks  TYPE /pm11/werks_range_t,
        lv_initial TYPE CR_OBJID.

* Get the object IDs corresponding to Plant and Work Centres entered
  IF ir_arbpl IS INITIAL.
    SELECT DISTINCT objty, @lv_initial, ' ', werks, @lv_initial
      FROM crhd
      INTO TABLE @lt_crhd
    WHERE werks IN @ir_werks.
  ELSE.
    SELECT objty objid arbpl werks objid
      FROM crhd
      INTO TABLE lt_crhd
    WHERE werks IN ir_werks
      AND arbpl IN ir_arbpl.
  ENDIF.

  IF sy-subrc <> 0.
* Raise error if Work Centre and Plants do not exist.
    es_return-type = 'E'.
    es_return-message = TEXT-m02.
  ENDIF.

* Proceed if Objects exist.
  CHECK es_return-type IS INITIAL.

  SELECT *
      FROM /pm11/kpirptbm
      INTO TABLE @DATA(lt_kpiid)
    WHERE kpiid IN @ir_rpi.
  IF sy-subrc NE 0.
* Raise error if invalid KPI
    es_return-type = 'E'.
    es_return-message = TEXT-m03.
  ENDIF.

  SORT lt_kpiid BY kpiid.

* Proceed if Objects exist and KPI is valid
  CHECK es_return-type IS INITIAL.

  LOOP AT lt_kpiid INTO DATA(lwa_kpiid).

    CLEAR: lt_output[].
* For each KPI call the corresponding FM to determine the KPI Result
* for all the Work Centre and plants.
    CALL FUNCTION lwa_kpiid-obj_name
      EXPORTING
        it_crhd    = lt_crhd
      IMPORTING
        et_results = lt_output.

* Keep adding the result of each KPI to the output table
    APPEND LINES OF lt_output TO et_output.

  ENDLOOP.

ENDFUNCTION.
