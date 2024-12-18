FUNCTION /PM11/FM_KPIBM03.
*"----------------------------------------------------------------------
*"*"Local Interface:
*"  IMPORTING
*"     REFERENCE(IT_CRHD) TYPE  /PM11/KPIREP2T_CRHD
*"  EXPORTING
*"     REFERENCE(ET_RESULTS) TYPE  /PM11/KPIREP2T_RESULTS
*"----------------------------------------------------------------------
  TYPES: BEGIN OF ty_material,
           matnr TYPE matnr,
           mtart TYPE mtart,
         END OF ty_material.

  TYPES: BEGIN OF ty_material_type,
           mtart TYPE mtart,
           counter TYPE /pm11/kpiresult,
         END OF ty_material_type.

  DATA: ls_results         TYPE /pm11/kpirep2s_results,
        ls_crss            TYPE /pm11/kpirep2s_crhd,
        lv_total_material  TYPE /pm11/kpiresult,
        lv_material_in_BOM TYPE /pm11/kpiresult,
        lv_current_material_type   TYPE MTART,
        lv_material_type   TYPE MTART,
        lt_material        TYPE STANDARD TABLE OF ty_material,
        lt_material_type   TYPE STANDARD TABLE OF ty_material_type,
        ls_material        TYPE ty_material.

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
   WHERE a~obj_name = '/PM11/FM_KPIBM03'
     AND l~spras = @sy-langu. "#EC CI_NOORDER

* Calculate the KPI result for each WorkCentre and update return table
    LOOP AT lt_crss INTO ls_crss.
      CLEAR: ls_results, lt_material, lt_material_type, lv_material_in_BOM, lv_total_material.

      SELECT DISTINCT marc~matnr, mara~mtart
        FROM marc
        JOIN mara
          ON marc~matnr = mara~matnr
        INTO TABLE @lt_material
       WHERE marc~werks = @ls_crss-werks.

      SELECT mara~mtart, COUNT( DISTINCT stpo~idnrk )
        FROM stpo
  INNER JOIN stas
          ON stpo~stlty = stas~stlty
         AND stpo~stlnr = stas~stlnr
         AND stpo~stlkn = stas~stlkn
  INNER JOIN stko
          ON stko~stlty = stas~stlty
         AND stko~stlnr = stas~stlnr
         AND stko~stlal = stas~stlal
  INNER JOIN @lt_material AS material
          ON stpo~idnrk = material~matnr
  LEFT OUTER JOIN mara
          ON stpo~idnrk = mara~matnr
       WHERE stko~wrkan = @ls_crss-werks
         AND stpo~lkenz <> 'X'
         AND stas~lkenz <> 'X'
         AND stko~loekz <> 'X'
         AND stko~lkenz <> 'X'
       GROUP BY mara~mtart
        INTO TABLE @lt_material_type.

      SORT lt_material_type BY mtart.

      LOOP AT lt_material INTO ls_material.
        lv_material_type = ls_material-mtart.

        IF lv_material_type = lv_current_material_type.
          CONTINUE.
        ENDIF.

        lv_current_material_type = lv_material_type.

        SELECT COUNT(*)
          FROM @lt_material AS material
         WHERE material~mtart = @lv_material_type
          INTO @lv_total_material.

        SELECT SINGLE counter
          FROM @lt_material_type AS material_type
         WHERE material_type~mtart = @lv_material_type
          INTO @lv_material_in_BOM.

        ls_results-werks = ls_crss-werks.
        ls_results-swerk = lv_material_type.

        IF lv_total_material IS NOT INITIAL.
          ls_results-kpiresult = lv_material_in_BOM / lv_total_material * 100.
        ELSE.
          ls_results-kpiresult = 0.
        ENDIF.
        ls_results-denom   = lv_total_material .
        ls_results-numer   = lv_material_in_BOM.
        ls_results-kpiid   = ls_kpiid_text-kpiid.
        ls_results-kpitext = ls_kpiid_text-kpitext.
        ls_results-date_calculated = sy-datlo.
        APPEND ls_results TO et_results.

        CLEAR: ls_results, lv_material_in_BOM, lv_total_material.
      ENDLOOP.

    ENDLOOP.

ENDFUNCTION.
