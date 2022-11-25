CLASS zcl_im__badi_abaptogit_sync DEFINITION
  PUBLIC
  FINAL
  CREATE PUBLIC .

  PUBLIC SECTION.

    INTERFACES if_ex_cts_request_check .

  PROTECTED SECTION.

  PRIVATE SECTION.

    METHODS perform_spotsync_abaptogit
      IMPORTING
        iv_package_names TYPE string
        iv_trid          TYPE trkorr.
ENDCLASS.

CLASS zcl_im__badi_abaptogit_sync IMPLEMENTATION.


  METHOD if_ex_cts_request_check~check_before_add_objects.
  ENDMETHOD.


  METHOD if_ex_cts_request_check~check_before_changing_owner.
  ENDMETHOD.


  METHOD if_ex_cts_request_check~check_before_creation.
  ENDMETHOD.


  METHOD if_ex_cts_request_check~check_before_release.

    TYPES:
        ty_devclass    TYPE STANDARD TABLE OF devclass WITH DEFAULT KEY,
        ty_list_string TYPE STANDARD TABLE OF string WITH DEFAULT KEY.

    DATA lv_package_names TYPE string.

    " K - workbench request only trying sync when workbench request is released
    DATA(lt_eligible_tr_types) = VALUE ty_list_string( ( `K` ) ).

    TRY.
        " Check only workbench transport request type
        IF NOT line_exists( lt_eligible_tr_types[ table_line = type ] ).
          EXIT.
        ENDIF.

        " no simple way to find out package an ABAP object belongs, leave to abap2git to determine

        " TODO: specify package names to sync ABAP objects in a TR
        DATA: lt_package_list TYPE ty_devclass.

        CONCATENATE LINES OF lt_package_list INTO lv_package_names SEPARATED BY ','.
        me->perform_spotsync_abaptogit( iv_package_names = lv_package_names  iv_trid = request ).

      CATCH cx_root INTO DATA(lx_root).

    ENDTRY.
  ENDMETHOD.


  METHOD if_ex_cts_request_check~check_before_release_slin.
  ENDMETHOD.

  METHOD perform_spotsync_abaptogit.

    " TODO: specify organization for ADO REST API
    DATA lv_orgid TYPE string.

    " TODO: specify Git repository ID for ADO REST API
    DATA lv_repoid TYPE string.

    " TODO: specify project name for ADO REST API
    DATA lv_project TYPE string.

    " TODO: specify the Git branch name prefix, usually users/system/,
    " for the branch holding the ABAP objects
    DATA lv_baseprefix TYPE string.

    " TODO: specify user name for ADO REST API
    DATA lv_username TYPE string.

    " TODO: specify personal access token of the user name above for ADO REST API
    DATA lv_pat TYPE string.

    DATA lo_abaptogit TYPE REF TO zcl_utility_abaptogit.
    CREATE OBJECT lo_abaptogit.
    lo_abaptogit->setup_ado(
        EXPORTING
            iv_username = lv_username
            iv_pat      = lv_pat
            iv_orgid    = lv_orgid
            iv_repoid   = lv_repoid
            iv_project  = lv_project
             ).
    DATA lv_trid TYPE string.
    lv_trid = iv_trid.
    lo_abaptogit->spotsync_tr(
        EXPORTING
            iv_trid = lv_trid
            iv_packagenames = iv_package_names
            iv_prefix = lv_baseprefix
             ).
  ENDMETHOD.

ENDCLASS.