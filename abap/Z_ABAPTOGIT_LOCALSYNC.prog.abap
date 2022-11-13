*&---------------------------------------------------------------------*
*& Report z_abaptogit_localsync
*&---------------------------------------------------------------------*
*& Program to download source code or statistics of ABAP objects for a system
*& in given package(s) to local disk file(s).
*&---------------------------------------------------------------------*
REPORT z_abaptogit_localsync.

PARAMETERS:
    " package name(s) split by comma
    p_pname     TYPE devclass LOWER CASE OBLIGATORY,
    " local folder to save downloaded ABAP object code files
    p_folder    TYPE string OBLIGATORY,
    " Sync mode: active/latest
    p_mode      TYPE string DEFAULT 'active' LOWER CASE OBLIGATORY,
    " TR ID to sync up to, if blank sync to default latest version 
    p_uttrid    TYPE string LOWER CASE.

START-OF-SELECTION.

PERFORM f_download.

FORM f_download.
    WRITE / |Sync mode { p_mode }|.
    WRITE / |Start download at { sy-uzeit }|.
    DATA lo_abaptogit TYPE REF TO ZCL_HRPY_UTILITY_ABAPTOGIT.
    CREATE OBJECT lo_abaptogit.
    DATA lv_packages TYPE string.
    lv_packages = p_pname.
    IF p_uttrid IS INITIAL.
        lo_abaptogit->get_packages_codes(
            EXPORTING
                iv_packages = lv_packages
                iv_folder = p_folder
                iv_mode = p_mode
                 ).
    ELSE.
        lo_abaptogit->get_packages_codes(
            EXPORTING
                iv_packages = lv_packages
                iv_folder = p_folder
                iv_mode = p_mode
                iv_uptotrid = p_uttrid
                 ).
    ENDIF.
    WRITE / |Finish download at { sy-uzeit }|.
ENDFORM.