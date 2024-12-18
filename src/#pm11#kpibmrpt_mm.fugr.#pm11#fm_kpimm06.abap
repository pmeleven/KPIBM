FUNCTION /PM11/FM_KPIMM06.
*"----------------------------------------------------------------------
*"*"Local Interface:
*"  IMPORTING
*"     REFERENCE(IT_CRHD) TYPE  /PM11/KPIREP2T_CRHD
*"  EXPORTING
*"     REFERENCE(ET_RESULTS) TYPE  /PM11/KPIREP2T_RESULTS
*"----------------------------------------------------------------------

  TYPES: BEGIN OF ty_warpl,
           order_no TYPE aufnr,
         END OF ty_warpl.

  DATA: ls_results TYPE /pm11/kpirep2s_results,
        ls_crhd    TYPE /pm11/kpirep2s_crhd,
        ls_crss    TYPE /pm11/kpirep2s_crhd,
        lines_261  TYPE /pm11/kpiresult,
        lines_262  TYPE /pm11/kpiresult,
        lt_wo      TYPE STANDARD TABLE OF ty_warpl,
        wa_date    TYPE sy-datum.

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
   WHERE a~kpiid_tx = 'MM01'
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

    CLEAR: ls_results, lt_wo, lines_261, lines_262.

    IF ls_crss-objid IS INITIAL.
      SELECT mseg~bwart
        FROM mseg
        WHERE mseg~budat_mkpf = @wa_date
        AND mseg~bwtar IS INITIAL
        AND mseg~bwart = '261'
        AND mseg~werks = @ls_crss-werks
       INTO TABLE @DATA(results_261).

      SELECT mseg~bwart
        FROM mseg
        WHERE mseg~budat_mkpf = @wa_date
        AND mseg~bwtar IS INITIAL
        AND mseg~bwart = '262'
        AND mseg~werks = @ls_crss-werks
       INTO TABLE @DATA(results_262).

      DESCRIBE TABLE results_261 LINES lines_261.
      DESCRIBE TABLE results_262 LINES lines_262.

      ls_results-werks = ls_crss-werks.

      IF lines_261 IS NOT INITIAL OR lines_262 IS NOT INITIAL.
        ls_results-kpiresult = lines_262 / ( lines_261 + lines_262 ) * 100.
      ELSE.
        ls_results-kpiresult = 0.
      ENDIF.
      ls_results-denom   = lines_261 + lines_262.
      ls_results-numer   = lines_262.
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
*        SELECT aufk~aufnr
*          FROM aufk
*         INNER JOIN afih
*            ON aufk~aufnr = afih~aufnr
*          INTO TABLE @lt_wo
*         WHERE afih~warpl = ''
*           AND aufk~erdat > @wa_date
*           AND aufk~werks = @ls_crss-werks
*           AND afih~gewrk = @ls_crss-objid.
*
*        SORT lt_wo BY order_no.
*        DELETE ADJACENT DUPLICATES FROM lt_wo
*                                   COMPARING order_no.
*
*        SELECT SINGLE COUNT(*)
*          FROM @lt_wo AS wo
*          INTO @lv_total_wo.
*
*        SELECT SINGLE COUNT(*)
*          FROM afko
*         INNER JOIN @lt_wo AS wo
*            ON afko~aufnr = wo~order_no
*         WHERE afko~plnnr <> ''
*          INTO @lv_wo_with_tl.

        ls_results-werks = lwa_crhd-werks.
        ls_results-arbpl = lwa_crhd-arbpl.
        ls_results-ktext = lwa_crtx.

        IF lines_261 IS NOT INITIAL OR lines_262 IS NOT INITIAL.
          ls_results-kpiresult = lines_262 / ( lines_261 + lines_262 ) * 100.
        ELSE.
          ls_results-kpiresult = 0.
        ENDIF.
        ls_results-denom   = lines_261 + lines_262.
        ls_results-numer   = lines_262.
        ls_results-kpiid   = ls_kpiid_text-kpiid.
        ls_results-kpitext = ls_kpiid_text-kpitext.
        ls_results-date_calculated = sy-datlo.
        APPEND ls_results TO et_results.

        CLEAR: ls_results, lt_wo, lines_261, lines_262.

      ENDLOOP.
    ENDIF.
  ENDLOOP.

ENDFUNCTION.
