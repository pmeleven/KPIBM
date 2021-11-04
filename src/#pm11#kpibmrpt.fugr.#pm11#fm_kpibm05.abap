FUNCTION /PM11/FM_KPIBM05.
*"----------------------------------------------------------------------
*"*"Local Interface:
*"  IMPORTING
*"     REFERENCE(IT_CRHD) TYPE  /ZPM/KPIREP2T_CRHD
*"  EXPORTING
*"     REFERENCE(ET_RESULTS) TYPE  /PM11/KPIREP2T_RESULTS
*"----------------------------------------------------------------------

  TYPES: BEGIN OF ty_measuring_pt,
           measuring_pt TYPE imrc_point,
         END OF ty_measuring_pt.

  DATA: ls_results    TYPE /pm11/kpirep2s_results,
        ls_crhd       TYPE /pm11/kpirep2s_crhd,
        ls_crss       TYPE /pm11/kpirep2s_crhd,
        lv_total_mp   TYPE /pm11/kpiresult,
        lv_updated_mp TYPE /pm11/kpiresult,
        lt_mp         TYPE STANDARD TABLE OF ty_measuring_pt,
        lt_updated_mp TYPE STANDARD TABLE OF ty_measuring_pt,
        wa_date       TYPE sy-datum.

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
   WHERE a~kpiid_tx = 'BM05'
     AND l~spras = @sy-langu.                           "#EC CI_NOORDER

  MOVE it_crhd[] TO lt_crhd[].

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
  SORT lt_crss BY werks arbpl.
  LOOP AT lt_crss INTO ls_crss.

    CLEAR: ls_results, lt_mp, lt_updated_mp, lv_updated_mp, lv_total_mp.

    IF ls_crss-objid IS INITIAL.
      SELECT imptt~point
        FROM imptt
       INNER JOIN mmpt
          ON imptt~point = mmpt~point
       INNER JOIN mpos
          ON mpos~warpl = mmpt~warpl
       INNER JOIN mpla
          ON mpla~warpl = mpos~warpl
       WHERE mpos~iwerk = @ls_crss-werks
         AND ( mpla~objnr NOT IN
                 ( SELECT objnr FROM jest WHERE ( jest~inact <> 'X'
                         AND  jest~stat IN ( 'I0076', 'I0320'  ) ) ) )
        INTO TABLE @lt_mp.

      SORT lt_mp BY measuring_pt.
      DELETE ADJACENT DUPLICATES FROM lt_mp
                            COMPARING measuring_pt.
      DELETE lt_mp WHERE measuring_pt IS INITIAL.

      SELECT SINGLE COUNT(*)
        FROM @lt_mp AS mp
        INTO @lv_total_mp.

      SELECT DISTINCT cdpos~objectid
        FROM cdpos
       INNER JOIN @lt_mp AS mp
          ON cdpos~objectid = mp~measuring_pt
        LEFT OUTER JOIN cdhdr
          ON cdpos~objectclas = cdhdr~objectclas
         AND cdpos~objectid = cdhdr~objectid
         AND cdpos~changenr = cdhdr~changenr
       WHERE cdpos~fname = 'PYEAR'
         AND cdhdr~udate > @wa_date
       GROUP BY cdpos~objectid
        INTO TABLE @lt_updated_mp.

      SELECT SINGLE COUNT(*)
        FROM @lt_updated_mp AS updated_mp
        INTO @lv_updated_mp.

      ls_results-werks = ls_crss-werks.

      IF lv_total_mp IS NOT INITIAL.
        ls_results-kpiresult = lv_updated_mp / lv_total_mp * 100.
      ELSE.
        ls_results-kpiresult = 0.
      ENDIF.
      ls_results-denom   = lv_total_mp.
      ls_results-numer   = lv_updated_mp.
      ls_results-kpiid   = ls_kpiid_text-kpiid.
      ls_results-kpitext = ls_kpiid_text-kpitext.
      ls_results-date_calculated = sy-datlo.
      APPEND ls_results TO et_results.

    ELSE.
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
        SELECT imptt~point
          FROM imptt
         INNER JOIN mmpt
            ON imptt~point = mmpt~point
         INNER JOIN mpos
            ON mpos~warpl = mmpt~warpl
         INNER JOIN mpla
            ON mpla~warpl = mpos~warpl
         WHERE mpos~iwerk = @ls_crss-werks
           AND mpos~gewrk = @ls_crss-objid
           AND ( mpla~objnr NOT IN
                 ( SELECT objnr FROM jest WHERE ( jest~inact <> 'X'
                         AND  jest~stat IN ( 'I0076', 'I0320'  ) ) ) )
        INTO TABLE @lt_mp.

        SORT lt_mp BY measuring_pt.
        DELETE ADJACENT DUPLICATES FROM lt_mp
                              COMPARING measuring_pt.
        DELETE lt_mp WHERE measuring_pt IS INITIAL.

        SELECT COUNT(*)
        FROM @lt_mp AS mp
        INTO @lv_total_mp.

        SELECT DISTINCT cdpos~objectid
          FROM cdpos
         INNER JOIN @lt_mp AS mp
            ON cdpos~objectid = mp~measuring_pt
          LEFT OUTER JOIN cdhdr
            ON cdpos~objectclas = cdhdr~objectclas
           AND cdpos~objectid = cdhdr~objectid
           AND cdpos~changenr = cdhdr~changenr
         WHERE cdpos~fname = 'PYEAR'
           AND cdhdr~udate > @wa_date
         GROUP BY cdpos~objectid
          INTO TABLE @lt_updated_mp.

        SELECT SINGLE COUNT(*)
          FROM @lt_updated_mp AS updated_mp
          INTO @lv_updated_mp.

        ls_results-werks = lwa_crhd-werks.
        ls_results-arbpl = lwa_crhd-arbpl.
        ls_results-ktext = lwa_crtx.

        IF lv_total_mp IS NOT INITIAL.
          ls_results-kpiresult = lv_updated_mp / lv_total_mp * 100.
        ELSE.
          ls_results-kpiresult = 0.
        ENDIF.
        ls_results-denom   = lv_total_mp.
        ls_results-numer   = lv_updated_mp.
        ls_results-kpiid   = ls_kpiid_text-kpiid.
        ls_results-kpitext = ls_kpiid_text-kpitext.
        ls_results-date_calculated = sy-datlo.
        APPEND ls_results TO et_results.

        CLEAR: ls_results, lt_mp, lt_updated_mp, lv_updated_mp, lv_total_mp.

      ENDLOOP.
    ENDIF.

  ENDLOOP.

ENDFUNCTION.
