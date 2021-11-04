FUNCTION /PM11/FM_KPIBM09.
*"--------------------------------------------------------------------
*"*"Local Interface:
*"  IMPORTING
*"     REFERENCE(IT_CRHD) TYPE  /ZPM/KPIREP2T_CRHD
*"  EXPORTING
*"     REFERENCE(ET_RESULTS) TYPE  /PM11/KPIREP2T_RESULTS
*"--------------------------------------------------------------------

  TYPES: BEGIN OF ty_measuring_pt,
           measuring_pt TYPE imrc_point,
         END OF ty_measuring_pt.

  DATA: ls_crss        TYPE /pm11/kpirep2s_crhd,
        ls_results     TYPE /pm11/kpirep2s_results,
        lt_mp          TYPE STANDARD TABLE OF ty_measuring_pt,
        lv_overflow_mp TYPE /pm11/kpiresult,
        lv_total_mp    TYPE /pm11/kpiresult.

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
   WHERE a~kpiid_tx = 'BM09'
     AND l~spras = @sy-langu.                           "#EC CI_NOORDER

* Calculate the KPI result for each Plant and update return table
  LOOP AT lt_crss INTO ls_crss.

    CLEAR: ls_results, lt_mp, lv_overflow_mp, lv_total_mp.

    SELECT imptt~point
    FROM imptt
    INNER JOIN mmpt
       ON imptt~point = mmpt~point
    INNER JOIN mpla
       ON mpla~warpl = mmpt~warpl
     INTO TABLE @lt_mp
    WHERE mmpt~warpl IN ( SELECT warpl FROM mpos WHERE iwerk = @ls_crss-werks )
    AND  ( mpla~objnr NOT IN
          ( SELECT objnr FROM jest WHERE ( jest~inact <> 'X'
                                    AND  jest~stat IN ( 'I0076', 'I0320'  ) ) ) ). "Created.

    SELECT SINGLE COUNT(*)
      FROM @lt_mp AS mp
      INTO @lv_total_mp.

    SELECT SINGLE COUNT(*)
      FROM imptt
INNER JOIN @lt_mp AS mp
        ON imptt~point = mp~measuring_pt
     WHERE ( imptt~cjump > 0 )
      INTO @lv_overflow_mp.

    ls_results-werks = ls_crss-werks.

    IF lv_total_mp IS NOT INITIAL.
      ls_results-kpiresult = lv_overflow_mp / lv_total_mp * 100.
    ELSE.
      ls_results-kpiresult = 0.
    ENDIF.
    ls_results-denom   = lv_total_mp.
    ls_results-numer   = lv_overflow_mp.
    ls_results-kpiid   = ls_kpiid_text-kpiid.
    ls_results-kpitext = ls_kpiid_text-kpitext.
    ls_results-date_calculated = sy-datlo.
    APPEND ls_results TO et_results.

  ENDLOOP.

ENDFUNCTION.
