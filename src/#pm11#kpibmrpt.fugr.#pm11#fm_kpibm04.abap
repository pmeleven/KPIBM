FUNCTION /PM11/FM_KPIBM04.
*"----------------------------------------------------------------------
*"*"Local Interface:
*"  IMPORTING
*"     REFERENCE(IT_CRHD) TYPE  /PM11/KPIREP2T_CRHD
*"  EXPORTING
*"     REFERENCE(ET_RESULTS) TYPE  /PM11/KPIREP2T_RESULTS
*"----------------------------------------------------------------------

  DATA: ls_results      TYPE /pm11/kpirep2s_results,
        ls_crhd         TYPE /pm11/kpirep2s_crhd,
        ls_crss         TYPE /pm11/kpirep2s_crhd,
        lv_total_res    TYPE /pm11/numer,
        lv_res_from_bom TYPE /pm11/numer,
        wa_date         TYPE sy-datum.

* Get Description of Object ID
  DATA(lt_crss) = it_crhd[].
  SORT lt_crss BY werks.
  DELETE ADJACENT DUPLICATES FROM lt_crss
                        COMPARING werks.

* Get KPI Description
  SELECT SINGLE l~kpiid,
                l~kpitext
    FROM /pm11/kpirptbml AS l
    INNER JOIN /pm11/kpirptbm AS a ON a~kpiid = l~kpiid
    INTO @DATA(ls_kpiid_text)
   WHERE a~obj_name = '/PM11/FM_KPIBM04'
     AND l~spras = @sy-langu.                           "#EC CI_NOORDER

  CALL FUNCTION 'RP_CALC_DATE_IN_INTERVAL'
    EXPORTING
      date      = sy-datlo
      days      = 85
      months    = 0
      years     = 0
      signum    = '-'
    IMPORTING
      calc_date = wa_date.

* Calculate the KPI result for each WorkCentre and update return table
  LOOP AT lt_crss INTO ls_crss.
    CLEAR: ls_results, lv_total_res, lv_res_from_bom.

    SELECT SINGLE COUNT(*)
      FROM resb
     INNER JOIN rsadd
        ON resb~rsnum = rsadd~rsnum
       AND resb~rspos = rsadd~rspos
      INTO @lv_total_res
     WHERE bwart = '261'
       AND resb~werks = @ls_crss-werks
       AND rsadd~creadat > @wa_date.

    SELECT SINGLE COUNT(*)
      FROM resb
     INNER JOIN rsadd
        ON resb~rsnum = rsadd~rsnum
       AND resb~rspos = rsadd~rspos
     WHERE bwart = '261'
       AND resb~stlty = ''
       AND resb~werks = @ls_crss-werks
       AND rsadd~creadat > @wa_date
      INTO @lv_res_from_bom.

    ls_results-werks = ls_crss-werks.
    IF lv_total_res IS NOT INITIAL.
      ls_results-kpiresult = lv_res_from_bom / lv_total_res * 100.
    ELSE.
      ls_results-kpiresult = 0.
    ENDIF.
    ls_results-denom   = lv_total_res.
    ls_results-numer   = lv_res_from_bom.
    ls_results-kpiid   = ls_kpiid_text-kpiid.
    ls_results-kpitext = ls_kpiid_text-kpitext.
    ls_results-date_calculated = sy-datlo.
    APPEND ls_results TO et_results.

  ENDLOOP.

ENDFUNCTION.
