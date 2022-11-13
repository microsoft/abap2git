*&---------------------------------------------------------------------*
*& Report z_abaptogit_catchupsync
*&---------------------------------------------------------------------*
*& Program to catch up transport requests of ABAP objects for a system
*& to Git repo
*&---------------------------------------------------------------------*
REPORT z_abaptogit_catchupsync.

parameters:
  p_pname type devclass LOWER CASE,
  p_orgid type string LOWER CASE,
  p_proj type string LOWER CASE,
  p_repoid type string LOWER CASE,
  p_prefix type string LOWER CASE DEFAULT 'users/system/',
  p_usrnam type string LOWER CASE,
  p_pat type string LOWER CASE.

START-OF-SELECTION.

PERFORM f_catchup.

FORM f_catchup.
    WRITE / |Start catchup at { sy-uzeit }|.
    DATA lo_abaptogit TYPE REF TO ZCL_UTILITY_ABAPTOGIT.
    CREATE OBJECT lo_abaptogit.
    lo_abaptogit->setup_ado(
        EXPORTING
            iv_username = p_usrnam
            iv_pat = p_pat
            iv_orgid = p_orgid
            iv_repoid = p_repoid
            iv_project = p_proj
             ).
    DATA lv_branch TYPE string.
    lo_abaptogit->prepare_landscape_branch( EXPORTING iv_prefix = p_prefix IMPORTING ev_branch = lv_branch ).
    DATA lv_pname TYPE string.
    lv_pname = p_pname.
    lo_abaptogit->catchup_trs( EXPORTING iv_branch = lv_branch iv_packagenames = lv_pname ).
    WRITE / |Finish catchup at { sy-uzeit }|.
ENDFORM.