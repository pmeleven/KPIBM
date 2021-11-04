FUNCTION /PM11/FM_KPIBM07.
*"--------------------------------------------------------------------
*"*"Local Interface:
*"  IMPORTING
*"     REFERENCE(IT_CRHD) TYPE  /PM11/KPIREP2T_CRHD
*"  EXPORTING
*"     REFERENCE(ET_RESULTS) TYPE  /PM11/KPIREP2T_RESULTS
*"--------------------------------------------------------------------

  TYPES: BEGIN OF ty_warpl,
           maint_plan TYPE warpl,
         END OF ty_warpl.

  DATA: ls_results  TYPE /pm11/kpirep2s_results,
        ls_crhd     TYPE /pm11/kpirep2s_crhd,
        ls_crss     TYPE /pm11/kpirep2s_crhd,
        lv_total_mp TYPE /pm11/kpiresult,
        lv_mp_sf100 TYPE /pm11/kpiresult,
        lt_mp       TYPE STANDARD TABLE OF ty_warpl.

* Get Description of Object ID
  DATA(lt_crhd) = it_crhd[].
  SORT lt_crhd BY objid werks.
  DELETE ADJACENT DUPLICATES FROM lt_crhd
                        COMPARING objid werks.
  DELETE lt_crhd WHERE arbpl IS INITIAL.

  DATA(lt_crss) = it_crhd[].

  LOOP AT lt_crss INTO ls_crss.
    IF ls_crss-objid <> ls_crss-objid_up.
      DELETE lt_crss.
    ENDIF.
  ENDLOOP.

  IF lt_crhd[] IS NOT INITIAL.
    SELECT *
      FROM crtx
      INTO TABLE @DATA(lt_crtx)
       FOR ALL ENTRIES IN @lt_crhd
     WHERE objty = @lt_crhd-objty
       AND objid = @lt_crhd-objid
       AND spras = @sy-langu.
  ENDIF.

* Get KPI Description
  SELECT SINGLE l~kpiid,
                l~kpitext
    FROM /pm11/kpirptbml AS l
    INNER JOIN /pm11/kpirptbm AS a ON a~kpiid = l~kpiid
    INTO @DATA(ls_kpiid_text)
   WHERE a~kpiid_tx = 'BM07'
     AND l~spras = @sy-langu.                           "#EC CI_NOORDER

  MOVE it_crhd[] TO lt_crhd[].

* Calculate the KPI result for each WorkCentre and update return table
  SORT lt_crss BY werks arbpl.
  LOOP AT lt_crss INTO ls_crss.

    IF ls_crss-objid IS INITIAL.
      SELECT mpos~warpl
        FROM mpos
       INNER JOIN mpla ON mpos~warpl = mpla~warpl
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

      SELECT SINGLE COUNT(*)
        FROM mpla
  INNER JOIN @lt_mp AS mp
          ON mpla~warpl = mp~maint_plan
       WHERE ( mpla~vspos = 100
          OR mpla~vsneg = 100 )
        INTO @lv_mp_sf100.

      ls_results-werks = ls_crss-werks.

      IF lv_total_mp IS NOT INITIAL.
        ls_results-kpiresult = lv_mp_sf100 / lv_total_mp * 100.
      ELSE.
        ls_results-kpiresult = 0.
      ENDIF.
      ls_results-denom   = lv_total_mp.
      ls_results-numer   = lv_mp_sf100.
      ls_results-kpiid   = ls_kpiid_text-kpiid.
      ls_results-kpitext = ls_kpiid_text-kpitext.
      ls_results-date_calculated = sy-datlo.
      APPEND ls_results TO et_results.
    ELSE.

      CLEAR: ls_results, lt_mp, lv_mp_sf100, lv_total_mp.

      TRY .
          DATA(lwa_crhd) = it_crhd[ objid = ls_crss-objid ].

          TRY .
              DATA(lwa_crtx) = lt_crtx[ objty = lwa_crhd-objty
                                       objid = lwa_crhd-objid ]-ktext.
            CATCH cx_sy_itab_line_not_found.
          ENDTRY.
        CATCH cx_sy_itab_line_not_found.
      ENDTRY.

      LOOP AT lt_crhd INTO ls_crhd WHERE objid_up = ls_crss-objid.
        SELECT mpos~warpl
          FROM mpos
         INNER JOIN mpla ON mpos~warpl = mpla~warpl
          INTO TABLE @lt_mp
         WHERE mpos~gewrk = @ls_crss-objid
           AND mpos~iwerk = @lwa_crhd-werks
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

        SELECT SINGLE COUNT(*)
          FROM mpla
    INNER JOIN @lt_mp AS mp
            ON mpla~warpl = mp~maint_plan
         WHERE ( mpla~vspos = 100
            OR mpla~vsneg = 100 )
          INTO @lv_mp_sf100.

        ls_results-werks = lwa_crhd-werks.
        ls_results-arbpl = lwa_crhd-arbpl.
        ls_results-ktext = lwa_crtx.

        IF lv_total_mp IS NOT INITIAL.
          ls_results-kpiresult = lv_mp_sf100 / lv_total_mp * 100.
        ELSE.
          ls_results-kpiresult = 0.
        ENDIF.
        ls_results-denom   = lv_total_mp.
        ls_results-numer   = lv_mp_sf100.
        ls_results-kpiid   = ls_kpiid_text-kpiid.
        ls_results-kpitext = ls_kpiid_text-kpitext.
        ls_results-date_calculated = sy-datlo.
        APPEND ls_results TO et_results.

        CLEAR: ls_results, lt_mp, lv_mp_sf100, lv_total_mp.

      ENDLOOP.
    ENDIF.
  ENDLOOP.

ENDFUNCTION.
