*&---------------------------------------------------------------------*
*&  Include           /PM11/KPIREP2_EVENTS
*&---------------------------------------------------------------------*
LOAD-OF-PROGRAM.



AT SELECTION-SCREEN OUTPUT.

  PERFORM f_default_dates.

AT SELECTION-SCREEN ON VALUE-REQUEST FOR s_rpi-low.

* Get the list of all RPIs for providing look up for
* for the LOW value of RPI
  PERFORM s_rpi_f4 CHANGING s_rpi-low.

AT SELECTION-SCREEN ON VALUE-REQUEST FOR s_rpi-high.

* Get the list of all RPIs for providing look up for
* for the HIGH value of RPI
  PERFORM s_rpi_f4 CHANGING s_rpi-high.

START-OF-SELECTION.

* Get the KPI Data from the respective FMs of KPIID.
  PERFORM get_data.

END-OF-SELECTION.

* Display the data in ALV format using FACTORY method.
  IF gt_output[] IS NOT INITIAL.
    PERFORM display_data.
  ENDIF.
  .
