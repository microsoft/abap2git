*&---------------------------------------------------------------------*
*& Report z_abaptogit_catchupsync
*&---------------------------------------------------------------------*
*& Program to catch up transport requests of ABAP objects for a system
*& to Git repo
*& this is to ensure the Git sync continuous after a break.
*& we will save the last successfully synced tr # in the local config file.
*&---------------------------------------------------------------------*
REPORT z_abaptogit_catchupsync.

parameters:
    " package name(s) split by comma
    p_pname type devclass LOWER CASE OBLIGATORY,
    " ADO organization name
    p_orgid type string LOWER CASE OBLIGATORY,
    " ADO project name
    p_proj type string LOWER CASE OBLIGATORY,
    " ADO Git repo GUID ID
    p_repoid type string LOWER CASE OBLIGATORY,
    " Git branch name prefix
    p_prefix type string LOWER CASE DEFAULT 'users/system/' OBLIGATORY,
    " ADO user name
    p_usrnam type string LOWER CASE OBLIGATORY,
    " ADO personal access token
    p_pat type string LOWER CASE OBLIGATORY,
    " Folder structure
    p_struct TYPE string DEFAULT 'eclipse' LOWER CASE OBLIGATORY.

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
    lo_abaptogit->catchup_trs( EXPORTING iv_branch = lv_branch iv_packagenames = lv_pname iv_folder_structure = p_struct ).
    WRITE / |Finish catchup at { sy-uzeit }|.
ENDFORM.