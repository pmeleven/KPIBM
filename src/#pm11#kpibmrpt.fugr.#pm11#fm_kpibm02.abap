FUNCTION /pm11/fm_kpibm02.
*"----------------------------------------------------------------------
*"*"Local Interface:
*"  IMPORTING
*"     REFERENCE(IT_CRHD) TYPE  /PM11/KPIREP2T_CRHD
*"  EXPORTING
*"     REFERENCE(ET_RESULTS) TYPE  /PM11/KPIREP2T_RESULTS
*"----------------------------------------------------------------------

  TYPES: BEGIN OF ty_tl,
           plnty TYPE plnty,
           plnnr TYPE plnnr,
           plnal TYPE plnal,
         END OF ty_tl.

  DATA: ls_results     TYPE /pm11/kpirep2s_results,
        ls_crhd        TYPE /pm11/kpirep2s_crhd,
        ls_crss        TYPE /pm11/kpirep2s_crhd,
        lv_total_tl    TYPE /pm11/kpiresult,
        lv_total_mp    TYPE /pm11/kpiresult,
        lv_tl_withplan TYPE /pm11/kpiresult,
        lv_tl_noplan   TYPE /pm11/kpiresult,
        lt_tl          TYPE STANDARD TABLE OF ty_tl,
        lt_mp_tl       TYPE STANDARD TABLE OF ty_tl.

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
   WHERE a~obj_name = '/PM11/FM_KPIBM02'
     AND l~spras = @sy-langu.                           "#EC CI_NOORDER

  MOVE it_crhd[] TO lt_crhd[].

* Calculate the KPI result for each WorkCentre and update return table
  SORT lt_crss BY werks arbpl.
  LOOP AT lt_crss INTO ls_crss.

    CLEAR: ls_results, lt_tl, lt_mp_tl, lv_total_tl, lv_tl_noplan, lv_tl_withplan.

    IF ls_crss-objid IS INITIAL.
      SELECT plnty,
             plnnr,
             plnal
        INTO TABLE @lt_tl
        FROM plko
       WHERE plko~werks = @ls_crss-werks
         AND plko~delkz <> 'X'
         AND plko~loekz <> 'X'.

      SELECT DISTINCT mpos~plnty,
             mpos~plnnr,
             mpos~plnal
        FROM mpos
       INNER JOIN mpla
          ON mpos~warpl = mpla~warpl
       INNER JOIN @lt_tl AS tl
          ON tl~plnty = mpos~plnty
         AND tl~plnnr = mpos~plnnr
         AND tl~plnal = mpos~plnal
       WHERE mpos~iwerk = @ls_crss-werks
         AND ( mpla~objnr NOT IN
                 ( SELECT objnr FROM jest WHERE ( jest~inact <> 'X'
                        AND  jest~stat IN ( 'I0076', 'I0320'  ) ) ) )
        INTO TABLE @lt_mp_tl.  "Created..

      SELECT COUNT(*)
        FROM @lt_tl AS tl
        INTO @lv_total_tl.

      SELECT COUNT(*)
        FROM @lt_mp_tl AS mp_tl
        INTO @lv_tl_withplan.

      lv_tl_noplan = lv_total_tl - lv_tl_withplan.

      ls_results-werks = ls_crss-werks.

      IF lv_total_tl IS NOT INITIAL.
        ls_results-kpiresult = lv_tl_noplan / lv_total_tl * 100.
      ELSE.
        ls_results-kpiresult = 0.
      ENDIF.
      ls_results-denom   = lv_total_tl.
      ls_results-numer   = lv_tl_noplan.
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
        SELECT plnty,
               plnnr,
               plnal
          INTO TABLE @lt_tl
          FROM plko
         WHERE plko~arbid = @ls_crss-objid
           AND plko~werks = @ls_crss-werks
           AND plko~delkz <> 'X'
           AND plko~loekz <> 'X'.

        SELECT DISTINCT mpos~plnty,
               mpos~plnnr,
               mpos~plnal
          FROM mpos
         INNER JOIN mpla
            ON mpos~warpl = mpla~warpl
         INNER JOIN @lt_tl AS tl
            ON tl~plnty = mpos~plnty
           AND tl~plnnr = mpos~plnnr
           AND tl~plnal = mpos~plnal
         WHERE mpos~gewrk = @ls_crss-objid
           AND mpos~iwerk = @ls_crss-werks
           AND ( mpla~objnr NOT IN
                   ( SELECT objnr FROM jest WHERE ( jest~inact <> 'X'
                          AND  jest~stat IN ( 'I0076', 'I0320'  ) ) ) )
          INTO TABLE @lt_mp_tl.

        SELECT COUNT(*)
          FROM @lt_tl AS tl
          INTO @lv_total_tl.

        SELECT COUNT(*)
          FROM @lt_mp_tl AS mp_tl
          INTO @lv_tl_withplan.

        lv_tl_noplan = lv_total_tl - lv_tl_withplan.

        ls_results-werks = lwa_crhd-werks.
        ls_results-arbpl = lwa_crhd-arbpl.
        ls_results-ktext = lwa_crtx.

        IF lv_total_tl IS NOT INITIAL.
          ls_results-kpiresult = lv_tl_noplan / lv_total_tl * 100.
        ELSE.
          ls_results-kpiresult = 0.
        ENDIF.
        ls_results-denom   = lv_total_tl.
        ls_results-numer   = lv_tl_noplan.
        ls_results-kpiid   = ls_kpiid_text-kpiid.
        ls_results-kpitext = ls_kpiid_text-kpitext.
        ls_results-date_calculated = sy-datlo.
        APPEND ls_results TO et_results.

        CLEAR: ls_results, lt_tl, lt_mp_tl, lv_total_tl, lv_tl_noplan, lv_tl_withplan.

      ENDLOOP.
    ENDIF.
  ENDLOOP.

ENDFUNCTION.
