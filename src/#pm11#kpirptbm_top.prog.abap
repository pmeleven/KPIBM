*&---------------------------------------------------------------------*
*&  Include           /PM11/KPIRPTBM_TOP
*&---------------------------------------------------------------------*
TABLES: crhd, /pm11/kpirep2.

DATA: gt_output     TYPE        /pm11/kpirep2t_results,
      go_alv        TYPE REF TO cl_salv_table,
      gr_container  TYPE REF TO cl_gui_custom_container.
