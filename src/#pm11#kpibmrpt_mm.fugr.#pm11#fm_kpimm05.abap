FUNCTION /pm11/fm_kpimm05.
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
        lt_wo      TYPE STANDARD TABLE OF ty_warpl,
        wa_date    TYPE sy-datum.

  RANGES sel_date FOR sy-datum.

  sel_date-low = sy-datum - 90.
  sel_date-high = sy-datum.
  sel_date-sign = 'I'.
  sel_date-option = 'BT'.
  APPEND sel_date.

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
   WHERE a~kpiid_tx = 'MM05'
     AND l~spras = @sy-langu.                           "#EC CI_NOORDER

  MOVE it_crhd[] TO lt_crhd[].

*  CALL FUNCTION 'RP_CALC_DATE_IN_INTERVAL'
*    EXPORTING
*      date      = sy-datlo
*      days      = 0
*      months    = 6
*      years     = 0
*      signum    = '-'
*    IMPORTING
*      calc_date = wa_date.

* Calculate the KPI result for each WorkCentre and update return table
  SORT lt_crss BY werks arbpl.
  LOOP AT lt_crss INTO ls_crss.

    CLEAR: ls_results, lt_wo.

    IF ls_crss-objid IS INITIAL.
      SELECT SUM( ekpo~brtwr )
       FROM ekpo
       INNER JOIN ekko ON ekpo~ebeln = ekko~ebeln
       WHERE ekpo~werks = @ls_crss-werks
       AND ekpo~matnr IS NOT INITIAL
       AND ekko~aedat IN @sel_date
       AND ekpo~loekz <> 'X'
       AND ekpo~pstyp <> '9'
       INTO @DATA(results_cat).

      SELECT SUM( ekpo~brtwr )
       FROM ekpo
       INNER JOIN ekko ON ekpo~ebeln = ekko~ebeln
       WHERE ekpo~werks = @ls_crss-werks
       AND ekko~aedat IN @sel_date
       AND ekpo~loekz <> 'X'
       AND ekpo~pstyp <> '9'
       INTO @DATA(results_tot).

      ls_results-werks = ls_crss-werks.

      IF results_cat IS NOT INITIAL OR results_tot IS NOT INITIAL.
        ls_results-kpiresult = results_cat / ( results_tot ) * 100.
      ELSE.
        ls_results-kpiresult = 0.
      ENDIF.
      ls_results-denom   = results_tot.
      ls_results-numer   = results_cat.
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

        ls_results-werks = lwa_crhd-werks.
        ls_results-arbpl = lwa_crhd-arbpl.
        ls_results-ktext = lwa_crtx.

        IF results_cat IS NOT INITIAL OR results_tot IS NOT INITIAL.
          ls_results-kpiresult = results_cat / ( results_tot ) * 100.
        ELSE.
          ls_results-kpiresult = 0.
        ENDIF.
        ls_results-denom   = results_tot.
        ls_results-numer   = results_cat.
        ls_results-kpiid   = ls_kpiid_text-kpiid.
        ls_results-kpitext = ls_kpiid_text-kpitext.
        ls_results-date_calculated = sy-datlo.
        APPEND ls_results TO et_results.

        CLEAR: ls_results, lt_wo, results_cat, results_tot.

      ENDLOOP.
    ENDIF.
  ENDLOOP.

ENDFUNCTION.
