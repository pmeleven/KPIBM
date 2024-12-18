FUNCTION /pm11/fm_kpimm07.
*"----------------------------------------------------------------------
*"*"Local Interface:
*"  IMPORTING
*"     REFERENCE(IT_CRHD) TYPE  /PM11/KPIREP2T_CRHD
*"  EXPORTING
*"     REFERENCE(ET_RESULTS) TYPE  /PM11/KPIREP2T_RESULTS
*"----------------------------------------------------------------------

  TYPES: BEGIN OF ty_materials,
           mandt  TYPE marc-mandt,
           matnr  TYPE mara-matnr,
           werks  TYPE marc-werks,
           tabkey TYPE cdpos-tabkey,
         END OF ty_materials.

  DATA: ls_results           TYPE /pm11/kpirep2s_results,
        lt_materials         TYPE STANDARD TABLE OF ty_materials,
*        lt_updated_materials TYPE STANDARD TABLE OF ty_materials,
        lv_total_materials   TYPE /pm11/kpiresult,
        lv_updated_materials TYPE /pm11/kpiresult.

  RANGES sel_date FOR sy-datum.

  sel_date-low = sy-datum - 90.
  sel_date-high = sy-datum.
  sel_date-sign = 'I'.
  sel_date-option = 'BT'.
  APPEND sel_date.

* Get Description of Object ID
  DATA(lt_crhd) = it_crhd[].
  SORT lt_crhd BY werks.
  DELETE ADJACENT DUPLICATES FROM lt_crhd
                        COMPARING werks.

* Get KPI Description
  SELECT SINGLE l~kpiid,
                l~kpitext
    FROM /pm11/kpirptbml AS l
    INNER JOIN /pm11/kpirptbm AS a ON a~kpiid = l~kpiid
    INTO @DATA(ls_kpiid_text)
   WHERE a~kpiid_tx = 'MM07'
     AND l~spras = @sy-langu.                           "#EC CI_NOORDER

  LOOP AT lt_crhd INTO data(ls_crhd).
    CLEAR: lt_materials.
    SELECT marc~mandt,
           marc~matnr,
           marc~werks
      FROM marc
      WHERE marc~werks = @ls_crhd-werks
      INTO TABLE @lt_materials.

    SELECT SINGLE COUNT(*)
    FROM @lt_materials AS materials
    INTO @lv_total_materials.

    LOOP AT lt_materials ASSIGNING FIELD-SYMBOL(<fs_materials>).
*      CONCATENATE <fs_materials>-mandt <fs_materials>-matnr <fs_materials>-werks INTO <fs_materials>-tabkey.
      <fs_materials>-tabkey = <fs_materials>-mandt.
      <fs_materials>-tabkey+3 = <fs_materials>-matnr.
      <fs_materials>-tabkey+43 = <fs_materials>-werks.
    ENDLOOP.

    SELECT DISTINCT cdpos~objectid
      FROM cdpos
     INNER JOIN @lt_materials AS materials
        ON cdpos~objectid = materials~matnr
        and cdpos~tabkey = materials~tabkey
      LEFT OUTER JOIN cdhdr
        ON cdpos~objectclas = cdhdr~objectclas
       AND cdpos~objectid = cdhdr~objectid
       AND cdpos~changenr = cdhdr~changenr
     WHERE ( cdpos~fname = 'WEBAZ' OR cdpos~fname = 'PLIFZ' )
       AND cdhdr~udate in @sel_date
       AND cdpos~objectclas = 'MATERIAL'
     GROUP BY cdpos~objectid
      INTO TABLE @data(lt_updated_materials).

    SELECT SINGLE COUNT(*)
      FROM @lt_updated_materials AS updated_materials
      INTO @lv_updated_materials.

    ls_results-werks = ls_crhd-werks.

    IF lv_updated_materials IS NOT INITIAL OR lv_total_materials IS NOT INITIAL.
      ls_results-kpiresult = lv_updated_materials / ( lv_total_materials ) * 100.
    ELSE.
      ls_results-kpiresult = 0.
    ENDIF.
    ls_results-denom   = lv_total_materials.
    ls_results-numer   = lv_updated_materials.
    ls_results-kpiid   = ls_kpiid_text-kpiid.
    ls_results-kpitext = ls_kpiid_text-kpitext.
    ls_results-date_calculated = sy-datlo.
    APPEND ls_results TO et_results.

  ENDLOOP.

ENDFUNCTION.
