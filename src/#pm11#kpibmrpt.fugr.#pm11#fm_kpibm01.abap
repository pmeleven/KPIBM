FUNCTION /PM11/FM_KPIBM01.
*"----------------------------------------------------------------------
*"*"Local Interface:
*"  IMPORTING
*"     REFERENCE(IT_CRHD) TYPE  /ZPM/KPIREP2T_CRHD
*"  EXPORTING
*"     REFERENCE(ET_RESULTS) TYPE  /PM11/KPIREP2T_RESULTS
*"----------------------------------------------------------------------

  TYPES: BEGIN OF ty_warpl,
           order_no TYPE aufnr,
         END OF ty_warpl.

  DATA: ls_results    TYPE /pm11/kpirep2s_results,
        ls_crhd       TYPE /pm11/kpirep2s_crhd,
        ls_crss       TYPE /pm11/kpirep2s_crhd,
        lv_total_wo   TYPE /pm11/kpiresult,
        lv_wo_with_tl TYPE /pm11/kpiresult,
        lt_wo         TYPE STANDARD TABLE OF ty_warpl,
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
   WHERE a~kpiid_tx = 'BM01'
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

    CLEAR: ls_results, lt_wo, lv_total_wo, lv_wo_with_tl.

    IF ls_crss-objid IS INITIAL.
      SELECT aufk~aufnr
        FROM aufk
       INNER JOIN afih
          ON aufk~aufnr = afih~aufnr
        INTO TABLE @lt_wo
       WHERE afih~warpl = ''
         AND aufk~erdat > @wa_date
         AND aufk~werks = @ls_crss-werks.

      SORT lt_wo BY order_no.
      DELETE ADJACENT DUPLICATES FROM lt_wo
                                 COMPARING order_no.

      SELECT SINGLE COUNT(*)
        FROM @lt_wo AS wo
        INTO @lv_total_wo.

      SELECT SINGLE COUNT(*)
        FROM afko
       INNER JOIN @lt_wo AS wo
          ON afko~aufnr = wo~order_no
       WHERE afko~plnnr <> ''
        INTO @lv_wo_with_tl.

      ls_results-werks = ls_crss-werks.

      IF lv_total_wo IS NOT INITIAL.
        ls_results-kpiresult = lv_wo_with_tl / lv_total_wo * 100.
      ELSE.
        ls_results-kpiresult = 0.
      ENDIF.
      ls_results-denom   = lv_total_wo.
      ls_results-numer   = lv_wo_with_tl.
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
        SELECT aufk~aufnr
          FROM aufk
         INNER JOIN afih
            ON aufk~aufnr = afih~aufnr
          INTO TABLE @lt_wo
         WHERE afih~warpl = ''
           AND aufk~erdat > @wa_date
           AND aufk~werks = @ls_crss-werks
           AND afih~gewrk = @ls_crss-objid.

        SORT lt_wo BY order_no.
        DELETE ADJACENT DUPLICATES FROM lt_wo
                                   COMPARING order_no.

        SELECT SINGLE COUNT(*)
          FROM @lt_wo AS wo
          INTO @lv_total_wo.

        SELECT SINGLE COUNT(*)
          FROM afko
         INNER JOIN @lt_wo AS wo
            ON afko~aufnr = wo~order_no
         WHERE afko~plnnr <> ''
          INTO @lv_wo_with_tl.

        ls_results-werks = lwa_crhd-werks.
        ls_results-arbpl = lwa_crhd-arbpl.
        ls_results-ktext = lwa_crtx.

        IF lv_total_wo IS NOT INITIAL.
          ls_results-kpiresult = lv_wo_with_tl / lv_total_wo * 100.
        ELSE.
          ls_results-kpiresult = 0.
        ENDIF.
        ls_results-denom   = lv_total_wo.
        ls_results-numer   = lv_wo_with_tl.
        ls_results-kpiid   = ls_kpiid_text-kpiid.
        ls_results-kpitext = ls_kpiid_text-kpitext.
        ls_results-date_calculated = sy-datlo.
        APPEND ls_results TO et_results.

        CLEAR: ls_results, lt_wo, lv_wo_with_tl, lv_total_wo.

      ENDLOOP.
    ENDIF.
  ENDLOOP.

ENDFUNCTION.
