*&---------------------------------------------------------------------*
*&  Include           /PM11/KPIRPTBM_SEL_SCRN
*&---------------------------------------------------------------------*
SELECTION-SCREEN BEGIN OF BLOCK b1.
SELECT-OPTIONS: s_werks FOR crhd-werks OBLIGATORY, "Plant
                s_arbpl FOR crhd-arbpl, "Work centres
                s_rpi   FOR /pm11/kpirep2-kpiid OBLIGATORY. "RPI
SELECTION-SCREEN END OF BLOCK b1.
