*&---------------------------------------------------------------------*
*& Report z_abaptogit_heatmap
*&---------------------------------------------------------------------*
*& Program to collect heatmap of code changes for transport requests
*&---------------------------------------------------------------------*
REPORT z_abaptogit_heatmap.

PARAMETERS:
    p_pname TYPE devclass LOWER CASE OBLIGATORY,
    p_frdat TYPE d OBLIGATORY,
    p_todat TYPE d OBLIGATORY,
    p_file  TYPE string OBLIGATORY.

START-OF-SELECTION.

PERFORM f_heatmap.

FORM f_heatmap.
    WRITE / |Start heatmap at { sy-uzeit }|.
    DATA lo_pcrsch TYPE REF TO ZCL_HRPY_UTILITY_ABAPTOGIT_PCR.
    DATA lo_abaptogit TYPE REF TO ZCL_HRPY_UTILITY_ABAPTOGIT.
    CREATE OBJECT lo_pcrsch.
    CREATE OBJECT lo_abaptogit
        EXPORTING
            io_objpcrsch = lo_pcrsch
            iv_methpcrschall = 'GET_HR_SCHEMAPCRS'
            iv_methpcrschcnt = 'BUILD_PCRSCHEMA_CONTENT'.
    DATA lv_pname TYPE string.
    lv_pname = p_pname.
    lo_abaptogit->heatmap_trs(
        EXPORTING
            iv_packagenames = lv_pname
            iv_fromdat = p_frdat
            iv_todat = p_todat
            iv_resultfile = p_file
             ).
    WRITE / |Finish heatmap at { sy-uzeit }|.
ENDFORM.