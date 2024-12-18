FUNCTION /PM11/FM_KPIBM06.
*"----------------------------------------------------------------------
*"*"Local Interface:
*"  IMPORTING
*"     REFERENCE(IT_CRHD) TYPE  /PM11/KPIREP2T_CRHD
*"  EXPORTING
*"     REFERENCE(ET_RESULTS) TYPE  /PM11/KPIREP2T_RESULTS
*"----------------------------------------------------------------------

  TYPES: BEGIN OF ty_warpl,
           maint_plan TYPE warpl,
         END OF ty_warpl.

  DATA: ls_results    TYPE /pm11/kpirep2s_results,
        ls_crss       TYPE /pm11/kpirep2s_crhd,
        lv_total_mp   TYPE /pm11/kpiresult,
        lv_mp_counter TYPE /pm11/kpiresult,
        lt_mp         TYPE STANDARD TABLE OF ty_warpl.

* Get Description of Object ID
  DATA(lt_crss) = it_crhd[].
  SORT lt_crss BY werks.
  DELETE ADJACENT DUPLICATES FROM lt_crss
                        COMPARING werks.

  LOOP AT lt_crss INTO ls_crss.
    IF ls_crss-objid <> ls_crss-objid_up.
      DELETE lt_crss.
    ENDIF.
  ENDLOOP.

* Get KPI Description
  SELECT SINGLE l~kpiid,
                l~kpitext
    FROM /pm11/kpirptbml AS l
    INNER JOIN /pm11/kpirptbm AS a ON a~kpiid = l~kpiid
    INTO @DATA(ls_kpiid_text)
   WHERE a~obj_name = '/PM11/FM_KPIBM06'
     AND l~spras = @sy-langu.                           "#EC CI_NOORDER

* Calculate the KPI result for each WorkCentre and update return table
  LOOP AT lt_crss INTO ls_crss.

    CLEAR: ls_results, lt_mp, lv_mp_counter, lv_total_mp.

    SELECT mpla~warpl
      FROM mpla
     INNER JOIN mpos ON mpos~warpl = mpla~warpl
      INTO TABLE @lt_mp
     WHERE mpos~iwerk = @ls_crss-werks
       AND ( mpla~objnr NOT IN
            ( SELECT objnr FROM jest WHERE ( jest~inact <> 'X'
                                AND  jest~stat IN ( 'I0076', 'I0320'  ) ) ) ). "Created.

    SORT lt_mp BY maint_plan.
    DELETE ADJACENT DUPLICATES FROM lt_mp
                              COMPARING maint_plan.
    DELETE lt_mp WHERE maint_plan IS INITIAL.

    SELECT SINGLE COUNT(*)
    FROM @lt_mp AS mp
    INTO @lv_total_mp.

    SELECT SINGLE COUNT( DISTINCT mmpt~warpl )
      FROM mmpt
LEFT OUTER JOIN mpla
        ON mmpt~warpl = mpla~warpl
     INNER JOIN @lt_mp AS mp
        ON mpla~warpl = mp~maint_plan
      INTO @lv_mp_counter.

    ls_results-werks = ls_crss-werks.

    IF lv_total_mp IS NOT INITIAL.
      ls_results-kpiresult = lv_mp_counter / lv_total_mp * 100.
    ELSE.
      ls_results-kpiresult = 0.
    ENDIF.
    ls_results-denom   = lv_total_mp.
    ls_results-numer   = lv_mp_counter.
    ls_results-kpiid   = ls_kpiid_text-kpiid.
    ls_results-kpitext = ls_kpiid_text-kpitext.
    ls_results-date_calculated = sy-datlo.
    APPEND ls_results TO et_results.

  ENDLOOP.

ENDFUNCTION.
