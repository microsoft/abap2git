" Utility class to sync from ABAP objects in a transport request (TR)
" to Git repo with specific package(s) and branch by Azure DevOps (ADO) REST API
" Multiple packages will share the same Git repo
" Multiple SAP systems in the landscape of a service line will share the same Git repo
" in respective branches
" Following ABAP objects are included in sync-ing:
" Code: Class, Function Module, Program, Include, Test Class, Interface,
" Enhancement Object (hook implementation, class), Transformation,
" Dictionary: Data Table, Data Element, Domain, Lock Object, Search Help, Table Type, View
" SAPScript, Variant, Message Class, Web Dynpro
" HR/payroll schema/PCR, configuration changes, BRF plus as XML
" Following ABAP objects are not yet included in sync-ing:
" enhancement objects (others) and other objects.
" Deleted objects are not sync-ed since it's not found which package it was in.
" Two modes are suggested in sync-ing to Git repo:
" 1. Latest version mode, where latest version of an ABAP object, if any, is valued while
"    active version is not, this ensures the code in Git reflects the state of the one
"    in SAP when transporting to other systems in the landscape as the release process.
" 2. Active version mode, where active version, if any, otherwise latest version, of an
"    ABAP object is valued, this ensures the code in Git reflects the state of the one
"    in SAP in running the code.
" Latest version mode should be used for CI purpose since the quality of codes should be
" verified against the state to transport to integration/UAT/PROD system.
" Active version mode should be used for code review or exploring what are inside a given system
" (for static scan/analysis).
" ADO PAT requires privilege to have code read/write or full, identity read (to specify real author
" upon pushing ADO commit)
CLASS zcl_utility_abaptogit DEFINITION
  PUBLIC
  FINAL
  CREATE PUBLIC .

  PUBLIC SECTION.

    " list of TR IDs
    TYPES: tty_trids TYPE TABLE OF string.

    " list of users
    TYPES: tty_users TYPE TABLE OF string.

    " list of text results
    TYPES: tty_list TYPE TABLE OF string WITH DEFAULT KEY.

    " TR object update
    TYPES: BEGIN OF ty_obj_update,
             path TYPE string,
             date TYPE string,
             time TYPE string,
             type TYPE string,
           END OF ty_obj_update.
    TYPES: tty_obj_updates TYPE TABLE OF ty_obj_update WITH KEY path.

    " constructor
    " io_objtelemetry - class object for telemetry
    " iv_methtelemetry - method name for telemetry
    " io_objmetadata - class object for metadata
    " iv_methmetadata - method name for metadata
    " for telemetry, the method will be invoked with parameters
    " iv_message as string (for message content) and iv_kind as string (for category)
    " for metadata, the method will be invoked with parameters iv_pkg as string (for object package if any), iv_name as string (for object name) and iv_type as string (for object type) and return string as rv_metadata
    METHODS constructor
      IMPORTING
        io_objtelemetry  TYPE REF TO object OPTIONAL
        iv_methtelemetry TYPE string OPTIONAL
        io_objmetadata   TYPE REF TO object OPTIONAL
        iv_methmetadata  TYPE string OPTIONAL.

    " fetch HR/payroll schema/PCR language code lines into files
    " iv_folder - local folder name to save the files
    " iv_folder_structure - 'flat' or 'eclipse'
    METHODS get_hr_schemapcrs
      IMPORTING
                iv_folder           TYPE string
                iv_folder_structure TYPE string DEFAULT 'eclipse'
      RETURNING VALUE(rv_success)   TYPE abap_bool.

    " fetch source code lines into files for ABAP code objects in given package
    " iv_package - package name
    " iv_folder - local folder name to save the files
    " iv_mode - 'latest' or 'active'
    " iv_uptotrid - optional TR ID to fetch the version of each object no later than
    " iv_folder_structure - 'flat' or 'eclipse'
    METHODS get_package_codes
      IMPORTING
                iv_package          TYPE devclass
                iv_folder           TYPE string
                iv_mode             TYPE string
                iv_uptotrid         TYPE string OPTIONAL
                iv_folder_structure TYPE string DEFAULT 'eclipse'
      RETURNING VALUE(rv_success)   TYPE abap_bool.

    " fetch source code lines into files for ABAP code objects in given package list
    " iv_packages - package name list as string separated by comma
    " iv_folder - local folder name to save the files
    " iv_mode - 'latest' or 'active'
    " iv_uptotrid - optional TR ID to fetch the version of each object no later than
    " iv_folder_structure - 'flat' or 'eclipse'
    METHODS get_packages_codes
      IMPORTING
                iv_packages         TYPE string
                iv_folder           TYPE string
                iv_mode             TYPE string
                iv_uptotrid         TYPE string OPTIONAL
                iv_folder_structure TYPE string DEFAULT 'eclipse'
      RETURNING VALUE(rv_success)   TYPE abap_bool.

    " fetch source code lines into files for ABAP code objects in Y*/Z* packages
    " iv_folder - local folder name to save the files
    " iv_mode - 'latest' or 'active'
    " iv_uptotrid - optional TR ID to fetch the version of each object no later than
    " iv_folder_structure - 'flat' or 'eclipse'
    METHODS get_customize_packages_codes
      IMPORTING
                iv_folder           TYPE string
                iv_mode             TYPE string DEFAULT 'latest'
                iv_uptotrid         TYPE string OPTIONAL
                iv_folder_structure TYPE string DEFAULT 'eclipse'
      RETURNING VALUE(rv_success)   TYPE abap_bool.

    " fetch class info for ABAP class object (excluding unit tests) in specific package
    " iv_packagename - package name
    " iv_mode - 'latest' or 'active'
    " et_classes - class info list
    METHODS get_package_classes
        IMPORTING
            iv_packagename  TYPE devclass
            iv_mode         TYPE string
        EXPORTING
            et_classes      TYPE zcl_utility_abaptogit_tr=>tty_classes.

    " setup configs for ADO operations
    " iv_username - VSO user name as email address
    " iv_pat - VSO personal access token, could be generated from VSO portal per user with code change permission
    " iv_orgid - organization ID, like the name "org" in <org>.visualstudio.com
    " iv_repoid - Git repo ID
    " iv_project - project name in your VSO portal
    METHODS setup_ado
      IMPORTING
        iv_username TYPE string
        iv_pat      TYPE string
        iv_orgid    TYPE string
        iv_repoid   TYPE string
        iv_project  TYPE string.

    " spot sync ABAP objects in a TR
    " iv_trid - TR ID
    " iv_user - TR owner
    " iv_domain - email domain for TR owner
    " iv_packagenames - package names to include in commit, separated by comma
    " iv_prefix - branch prefix
    METHODS spotsync_tr
      IMPORTING
                iv_trid           TYPE string
                iv_user           TYPE string DEFAULT ''
                iv_domain         TYPE string DEFAULT ''
                iv_packagenames   TYPE string
                iv_prefix         TYPE string DEFAULT 'users/system/'
      RETURNING VALUE(rv_success) TYPE abap_bool.

    " prepare branch name for current landscape system
    " iv_prefix - branch prefix
    " ev_branch - branch name constructed
    METHODS prepare_landscape_branch
      IMPORTING
        iv_prefix TYPE string DEFAULT 'users/system/'
      EXPORTING
        ev_branch TYPE string.

    " sync TRs that not yet sync-ed to Git since last TR marked in repo sync status file
    " iv_branch - branch name to push the changes to
    " iv_packagenames - package names to include in commit, separated by comma
    " iv_domain - email domain for TR owner to specify real author of Git commit, blank means not to specify
    " iv_rootfolder - the root folder in Git local clone for ABAP objects to add to, shared by all packages in SAP, case sensitive
    " iv_folder_structure - 'flat' or 'eclipse'
    " iv_use_englist - engineer allow list file path to filter in customizing TRs, empty means not applicable
    " et_trids - TR IDs inspected regardless Git sync-ed or not
    " et_synctrids - TR IDs Git sync-ed
    METHODS catchup_trs
      IMPORTING
                iv_branch           TYPE string
                iv_packagenames     TYPE string
                iv_domain           TYPE string DEFAULT ''
                iv_rootfolder       TYPE string DEFAULT '/SRC/'
                iv_folder_structure TYPE string DEFAULT 'eclipse'
                iv_use_englist      TYPE string DEFAULT ''
      EXPORTING
                et_trids            TYPE tty_trids
                et_synctrids        TYPE tty_trids
      RETURNING VALUE(rv_success)   TYPE abap_bool.

    " get heatmap of code change stats in given date range
    " iv_packagenames - package names to include in commit, separated by comma
    " iv_fromdat - date to include from
    " iv_todat - date to include to
    " iv_mode - latest/active, blank means any
    " iv_resultfile - result file for heatmap
    " it_users - user filter, not applicable if empty
    METHODS heatmap_trs
      IMPORTING
                iv_packagenames   TYPE string
                iv_fromdat        TYPE d
                iv_todat          TYPE d
                iv_mode           TYPE string DEFAULT 'latest'
                iv_resultfile     TYPE string
                it_users          TYPE tty_users
      RETURNING VALUE(rv_success) TYPE abap_bool.

    " check if tasks of a TR are all released
    " iv_trid - TR ID
    " ev_released - whether tasks are all released
    METHODS is_trtasks_released
      IMPORTING
                iv_trid           TYPE string
      EXPORTING
                ev_released       TYPE abap_bool
      RETURNING VALUE(rv_success) TYPE abap_bool.

    " prepare branches for ADO pull request for the TR code review process
    " iv_trid - TR ID
    " iv_prefix - branch prefix
    " ev_prvbranch - private branch created
    " ev_slrelbranch - SL release branch
    " ev_activeprurl - existing PR URL
    " ev_firstpr - whether this is first PR for the TR
    METHODS prepare_pullrequest_branches
      IMPORTING
                iv_trid           TYPE string
                iv_prefix         TYPE string DEFAULT 'users/system/'
      EXPORTING
                ev_prvbranch      TYPE string
                ev_slrelbranch    TYPE string
                ev_activeprurl    TYPE string
                ev_firstpr        TYPE abap_bool
      RETURNING VALUE(rv_success) TYPE abap_bool.

    " create ADO pull request for the TR code review process
    " iv_packagenames - package names to include in commit, separated by comma
    " iv_trid - TR ID
    " iv_wid - ADO work item ID
    " iv_reviewer - reviewer to @ in PR description
    " iv_comment - Comment of the first commit
    " iv_domain - email domain for TR owner
    " iv_prefix - branch prefix
    " iv_curuser - use current user as Git commit author
    " it_excl_objs - exclusion list for object type
    " it_excl_tbls - exclusion list for configuration table names
    " ev_title - title of the TR
    " ev_newprurl - URL of PR creation
    METHODS create_pullrequest
      IMPORTING
                iv_packagenames   TYPE string
                iv_trid           TYPE string
                iv_wid            TYPE string
                iv_reviewer       TYPE string
                iv_comment        TYPE string DEFAULT ''
                iv_domain         TYPE string DEFAULT ''
                iv_prefix         TYPE string DEFAULT 'users/system'
                iv_curuser        TYPE abap_bool DEFAULT 'X'
                it_excl_objs      TYPE zcl_utility_abaptogit_tr=>tty_excl_list OPTIONAL
                it_excl_tbls      TYPE zcl_utility_abaptogit_tr=>tty_excl_list OPTIONAL
      EXPORTING
                ev_title          TYPE string
                ev_newprurl       TYPE string
      RETURNING VALUE(rv_success) TYPE abap_bool.

    " revise with code changes on the ADO pull request for the TR code review process
    " iv_packagenames - package names to include in commit, separated by comma
    " iv_trid - TR ID
    " iv_comment - comment on the code revision
    " iv_domain - email domain for TR owner
    " iv_prefix - branch prefix
    " iv_curuser - use current user as commit author
    " it_excl_objs - exclusion list for object type
    " it_excl_tbls - exclusion list for configuration table names
    METHODS revise_pullrequest
      IMPORTING
                iv_packagenames   TYPE string
                iv_trid           TYPE string
                iv_comment        TYPE string
                iv_domain         TYPE string DEFAULT ''
                iv_prefix         TYPE string DEFAULT 'users/system/'
                iv_curuser        TYPE abap_bool DEFAULT 'X'
                it_excl_objs      TYPE zcl_utility_abaptogit_tr=>tty_excl_list OPTIONAL
                it_excl_tbls      TYPE zcl_utility_abaptogit_tr=>tty_excl_list OPTIONAL
      RETURNING VALUE(rv_success) TYPE abap_bool.

    " get active or latest completed pull request for the TR
    " iv_trid - TR ID
    " iv_prefix - branch prefix
    " ev_prurl - URL of active PR
    METHODS get_related_pullrequest
      IMPORTING
                iv_trid           TYPE string
                iv_prefix         TYPE string DEFAULT 'users/system/'
      EXPORTING
                ev_prurl          TYPE string
      RETURNING VALUE(rv_success) TYPE abap_bool.

    " get latest update date/time among TR objects
    " iv_packagenames - package names to include in commit, separated by comma
    " iv_trid - TR ID
    " it_excl_objs - exclusion list for object type
    " it_excl_tbls - exclusion list for configuration table names
    " ev_owner - TR owner
    " ev_date - latest update date
    " ev_time - latest update time
    " et_items - TR object updates
    " ev_ossnote - TR has OSS note or not
    METHODS get_tr_object_latest_update
      IMPORTING
        iv_packagenames TYPE string
        iv_trid         TYPE string
        it_excl_objs    TYPE zcl_utility_abaptogit_tr=>tty_excl_list OPTIONAL
        it_excl_tbls    TYPE zcl_utility_abaptogit_tr=>tty_excl_list OPTIONAL
      EXPORTING
        ev_owner        TYPE string
        ev_date         TYPE d
        ev_time         TYPE t
        et_items        TYPE tty_obj_updates
        ev_ossnote      TYPE abap_bool.

    " get modifiable TRs from current user or given one
    " iv_user - given user to fetch TRs
    " et_trids - Modifiable TR IDs related to current user
    CLASS-METHODS get_modifiable_trs
      IMPORTING
        iv_user  TYPE string DEFAULT ''
      EXPORTING
        et_trids TYPE zcl_utility_abaptogit_ado=>tty_trids.

    " extract ADO work item ID from TR title, if any
    " iv_title - TR title
    " ev_wid - ADO work item ID if any
    CLASS-METHODS get_wid_from_title
      IMPORTING
        iv_title TYPE string
      EXPORTING
        ev_wid   TYPE string.

  PROTECTED SECTION.

  PRIVATE SECTION.

    CONSTANTS c_delim TYPE string VALUE '\'.
    CONSTANTS c_custtrfunc TYPE string VALUE 'W'.
    CONSTANTS c_wkbtrfunc TYPE string VALUE 'K'.
    CONSTANTS c_toctrfunc TYPE string VALUE 'T'.
    CONSTANTS c_relests TYPE string VALUE 'R'.

    " max difference in seconds for an imported TR to date time of now
    CONSTANTS c_importtr_diff_gap TYPE i VALUE 30.

    " code object used in downloading ABAP code object to local disk
    TYPES: BEGIN OF ts_code_object,
             obj_name  TYPE sobj_name,
             obj_type  TYPE trobjtype,
             obj_type2 TYPE trobjtype,
             fugr_name TYPE sobj_name,
             devclass  TYPE devclass,
           END OF ts_code_object.

    TYPES: BEGIN OF ts_code_object_wydc,
             obj_name  TYPE e071-obj_name,
             obj_type  TYPE trobjtype,
             obj_type2 TYPE trobjtype,
             fugr_name TYPE sobj_name,
             devclass  TYPE devclass,
           END OF ts_code_object_wydc.

    " helper objects for ADO and TR operations
    DATA oref_ado TYPE REF TO zcl_utility_abaptogit_ado.
    DATA oref_tr TYPE REF TO zcl_utility_abaptogit_tr.

    " telemetry callback
    DATA oref_telemetry TYPE REF TO object.
    DATA method_name_telemetry TYPE string.

    " save content to local disk file
    METHODS save_file
      IMPORTING
                iv_commit_object    TYPE zcl_utility_abaptogit_tr=>ts_commit_object
                iv_basefolder       TYPE string
                iv_folder_structure TYPE string
                iv_devclass         TYPE devclass
      EXPORTING
                ev_codefolder       TYPE string
                ev_filefolder       TYPE string
      CHANGING
                cht_filefolders     TYPE tty_list
                cht_filecontent     TYPE zcl_utility_abaptogit_tr=>tty_abaptext
      RETURNING VALUE(rv_success)   TYPE abap_bool.

    CLASS-METHODS get_utc_datetime
      IMPORTING
        iv_date TYPE d
        iv_time TYPE t
      EXPORTING
        ev_date TYPE d
        ev_time TYPE t.

    " wrapper to write telemetry with the callback registered
    METHODS write_telemetry
      IMPORTING
        iv_message TYPE string
        iv_kind    TYPE string DEFAULT 'error'.

ENDCLASS.



CLASS ZCL_UTILITY_ABAPTOGIT IMPLEMENTATION.


  METHOD catchup_trs.

    DATA lv_content TYPE string.
    DATA lv_sync_status TYPE zcl_utility_abaptogit_ado=>ts_sync_status.
    DATA lt_trids TYPE zcl_utility_abaptogit_ado=>tty_trids.
    DATA lt_commit_objects TYPE zcl_utility_abaptogit_tr=>tty_commit_object.
    DATA lv_owner TYPE string.
    DATA lv_comment TYPE string.
    DATA lv_rootfolder TYPE string.
    DATA lv_synccnt TYPE i.
    DATA lv_trtimestamp TYPE timestamp.
    DATA lv_nowtimestamp TYPE timestamp.
    DATA lv_diff TYPE tzntstmpl.

    lv_rootfolder = iv_rootfolder.
    TRANSLATE lv_rootfolder TO UPPER CASE.
    DATA(lv_itempath) = |{ lv_rootfolder }{ zcl_utility_abaptogit_ado=>c_sync_status_file }|.

    " fetch content of shared sync status file
    rv_success = me->oref_ado->get_item_ado(
        EXPORTING
            iv_branch = iv_branch
            iv_itempath = lv_itempath
        IMPORTING
            ev_content = lv_content
             ).
    CHECK rv_success = abap_true.

    " extract last sync TR ID
    rv_success = zcl_utility_abaptogit_ado=>load_sync_status(
       EXPORTING
           iv_filecontent = lv_content
       IMPORTING
           ev_sync_status = lv_sync_status
            ).
    CHECK rv_success = abap_true.

    " sync-ed in active mode can't be caught up since it's sync-ed to active/latest version of objects instead of TR
    IF lv_sync_status-mode = zcl_utility_abaptogit_tr=>c_active_version.
      me->write_telemetry( iv_message = |branch { iv_branch } is sync-ed in active mode| ).
      rv_success = abap_false.
      RETURN.
    ENDIF.

    TYPES: tty_englist TYPE TABLE OF string WITH DEFAULT KEY.
    TYPES: BEGIN OF ty_engallowlist,
             allowed_engineers TYPE tty_englist,
           END OF ty_engallowlist.
    DATA lv_engallowlist TYPE ty_engallowlist.

    IF iv_use_englist <> ''.
      lv_itempath = iv_use_englist.

      " fetch content of engineer allow list file
      rv_success = me->oref_ado->get_item_ado(
          EXPORTING
              iv_branch = iv_branch
              iv_itempath = lv_itempath
          IMPORTING
              ev_content = lv_content
               ).
      CHECK rv_success = abap_true.
      /ui2/cl_json=>deserialize(
          EXPORTING
              json = lv_content
          CHANGING
              data = lv_engallowlist
               ).
    ENDIF.

    " fetch TR IDs to sync since last one
    me->oref_ado->get_trs( EXPORTING iv_fromtrid = lv_sync_status-trid IMPORTING et_trids = lt_trids ).

    " push each TR ID to Git repo
    LOOP AT lt_trids INTO DATA(watrid).

      " an imported TR may be released before catchup starts but importing finishes after catchup finishes, the changes are missing in git commit
      " in this case the TR should be skipped, as well as TRs followed and let next catchup to carry the changes
      IF watrid-trid+0(3) <> sy-sysid.
        cl_abap_tstmp=>systemtstmp_syst2utc(
            EXPORTING
                syst_date = watrid-dat
                syst_time = watrid-tim
            IMPORTING
                utc_tstmp = lv_trtimestamp
             ).
        cl_abap_tstmp=>systemtstmp_syst2utc(
            EXPORTING
                syst_date = sy-datum
                syst_time = sy-uzeit
            IMPORTING
                utc_tstmp = lv_nowtimestamp
             ).
        cl_abap_tstmp=>subtract(
            EXPORTING
                tstmp1 = lv_nowtimestamp
                tstmp2 = lv_trtimestamp
            RECEIVING
                r_secs = lv_diff
             ).
        IF lv_diff < c_importtr_diff_gap.
          me->write_telemetry( iv_message = |Release time of import TR { watrid-trid } is too close with risk of changes missing in git commit| iv_kind = 'info' ).
          rv_success = abap_true.
          EXIT.
        ENDIF.
      ENDIF.

      " apply engineer allow list for TR owners if any
      IF iv_use_englist <> '' AND NOT line_exists( lv_engallowlist-allowed_engineers[ table_line = watrid-user ] ).
        me->write_telemetry( iv_message = |TR { watrid-trid } ({ watrid-user }) is not released by engineers in allow list| iv_kind = 'info' ).
        rv_success = abap_true.
        CONTINUE.
      ENDIF.

      CLEAR lt_commit_objects.
      rv_success = me->oref_tr->get_tr_commit_objects(
          EXPORTING
              iv_trid = watrid-trid
              iv_packagenames = iv_packagenames
          IMPORTING
              ev_owner = lv_owner
              ev_comment = lv_comment
          CHANGING
              it_commit_objects = lt_commit_objects
               ).
      CHECK rv_success = abap_true.
      rv_success = me->oref_ado->push_tr_commit_objects(
          EXPORTING
              iv_trid = watrid-trid
              iv_user = watrid-user
              iv_domain = iv_domain
              iv_branch = iv_branch
              iv_comment = lv_comment
              iv_rootfolder = iv_rootfolder
              it_commit_objects = lt_commit_objects
              iv_folder_structure = iv_folder_structure
          IMPORTING
              ev_synccnt = lv_synccnt
               ).
      CHECK rv_success = abap_true.
      IF et_trids IS SUPPLIED.
        APPEND watrid-trid TO et_trids.
      ENDIF.
      IF lv_synccnt = 0.
        me->write_telemetry( iv_message = |no push for TR { watrid-trid }| iv_kind = 'info' ).
      ELSE.
        IF et_synctrids IS SUPPLIED.
          APPEND watrid-trid TO et_synctrids.
        ENDIF.
        me->write_telemetry( iv_message = |caught up TR { watrid-trid } with { lv_synccnt } object(s) | iv_kind = 'info' ).
      ENDIF.
    ENDLOOP.

  ENDMETHOD.


  METHOD constructor.

    CREATE OBJECT me->oref_tr
      EXPORTING
        io_objtelemetry  = io_objtelemetry
        iv_methtelemetry = iv_methtelemetry
        io_objmetadata = io_objmetadata
        iv_methmetadata = iv_methmetadata.

    IF io_objtelemetry IS SUPPLIED.
      me->oref_telemetry = io_objtelemetry.
    ENDIF.

    IF iv_methtelemetry IS SUPPLIED.
      me->method_name_telemetry = iv_methtelemetry.
    ENDIF.

  ENDMETHOD.


  METHOD create_pullrequest.

    DATA lv_slrelbranch TYPE string.
    DATA lt_commit_objects TYPE zcl_utility_abaptogit_tr=>tty_commit_object.
    DATA lv_owner TYPE string.
    DATA lv_comment TYPE string.
    DATA lv_synccnt TYPE i.
    DATA lt_lines TYPE TABLE OF string.
    DATA lt_reviewers TYPE TABLE OF string.
    DATA lv_title TYPE string.
    DATA lv_description TYPE string.
    DATA lv_author TYPE string.
    DATA lv_reviewerid TYPE string.
    DATA lv_custtr TYPE abap_bool.

    " get active version of TR objects
    "BUGBUG: TO DO: we need to check the commit object time stamp
    "Do we want to keep each tr object separate or unify them?
    rv_success = me->oref_tr->get_tr_commit_objects(
        EXPORTING
            iv_trid = iv_trid
            iv_packagenames = iv_packagenames
            iv_mode = zcl_utility_abaptogit_tr=>c_active_version
            it_excl_objs = it_excl_objs
            it_excl_tbls = it_excl_tbls
        IMPORTING
            ev_owner = lv_owner
            ev_comment = lv_comment
            ev_custtr = lv_custtr
        CHANGING
            it_commit_objects = lt_commit_objects
             ).
    CHECK rv_success = abap_true.

    me->prepare_landscape_branch( EXPORTING iv_prefix = iv_prefix IMPORTING ev_branch = lv_slrelbranch ).
    DATA(lv_privatebranch) = |users/{ lv_owner }/TR/{ iv_trid }|.

    IF iv_wid IS NOT INITIAL.
      lv_comment = lv_comment && cl_abap_char_utilities=>cr_lf && iv_comment && cl_abap_char_utilities=>cr_lf && |#{ iv_wid }|.
    ENDIF.

    IF iv_reviewer IS NOT INITIAL.
      SPLIT iv_reviewer AT ',' INTO TABLE lt_reviewers.
      LOOP AT lt_reviewers INTO DATA(wareviewer).
        me->oref_ado->get_displayname_ado(
            EXPORTING
                iv_user = wareviewer
                iv_domain = iv_domain
            IMPORTING
                ev_id = lv_reviewerid
                 ).
        lv_comment = lv_comment && cl_abap_char_utilities=>cr_lf && |@<{ lv_reviewerid }>|.
      ENDLOOP.
    ENDIF.

    IF iv_curuser = abap_true.
      lv_author = sy-uname.
    ELSE.
      lv_author = lv_owner.
    ENDIF.

    " sync active version of TR objects to private branch
    rv_success = me->oref_ado->push_tr_commit_objects(
        EXPORTING
            iv_trid = iv_trid
            iv_user = lv_author
            iv_domain = iv_domain
            iv_branch = lv_privatebranch
            iv_comment = lv_comment
            iv_push_status = abap_false
            it_commit_objects = lt_commit_objects
        IMPORTING
            ev_synccnt = lv_synccnt
             ).
    CHECK rv_success = abap_true.

    IF lv_synccnt = 0.
      me->write_telemetry( iv_message = |TR { iv_trid } has no objects matching criteria to sync to private branch { lv_privatebranch }| iv_kind = 'info' ).
    ELSE.
      me->write_telemetry( iv_message = |push TR { iv_trid } objects ({ lv_synccnt }) to private branch { lv_privatebranch }| iv_kind = 'info' ).
    ENDIF.

    SPLIT lv_comment AT cl_abap_char_utilities=>cr_lf INTO TABLE lt_lines.
    lv_title = lt_lines[ 1 ].
    ev_title = lv_title.

    " get URL for pull request creation page
    rv_success = me->oref_ado->get_pullrequestcreate_url(
        EXPORTING
            iv_srcbranch = lv_privatebranch
            iv_tarbranch = lv_slrelbranch
        IMPORTING
            ev_prurl = ev_newprurl
             ).
    CHECK rv_success = abap_true.

  ENDMETHOD.


  METHOD get_customize_packages_codes.

    DATA lv_basefolder TYPE string.
    DATA lv_package TYPE devclass.

    IF iv_uptotrid IS SUPPLIED AND iv_mode <> zcl_utility_abaptogit_tr=>c_latest_version.
      me->write_telemetry( iv_message = |latest version required for up-to TR ID supplied| ).
      rv_success = abap_false.
      RETURN.
    ENDIF.

    IF iv_folder NP '*\'.
      lv_basefolder = iv_folder && '\'.
    ELSE.
      lv_basefolder = iv_folder.
    ENDIF.

    DATA lv_success TYPE abap_bool.
    DATA lt_packages TYPE STANDARD TABLE OF devclass.

    " fetch customization packages with initial Y or Z
    SELECT devclass INTO TABLE lt_packages FROM tdevc WHERE devclass LIKE 'Z%' OR devclass LIKE 'Y%'. "#EC CI_SGLSELECT

    " save objects in each package collected under the folder named as package name like \src\<package name>\
    LOOP AT lt_packages INTO DATA(wa).
      lv_package = wa.
      IF iv_uptotrid IS SUPPLIED.
        lv_success = me->get_package_codes(
            EXPORTING
                iv_package = lv_package
                iv_folder = lv_basefolder
                iv_mode = iv_mode
                iv_uptotrid = iv_uptotrid
                 ).
      ELSE.
        lv_success = me->get_package_codes(
            EXPORTING
                iv_package = lv_package
                iv_folder = lv_basefolder
                iv_mode = iv_mode
                 ).
      ENDIF.
      IF lv_success <> abap_true.
        me->write_telemetry( iv_message = |GET_CUSTOMIZE_PACKAGES_CODES has failure in package { lv_package }| ).
        rv_success = abap_false.
      ENDIF.
    ENDLOOP.

    " mark down status of last sync TR ID and update date time
    DATA(lv_file) = |{ lv_basefolder }{ zcl_utility_abaptogit_ado=>c_sync_status_file }|.
    IF iv_uptotrid IS SUPPLIED.
      rv_success = zcl_utility_abaptogit_ado=>save_sync_status(
          EXPORTING
              iv_mode = iv_mode
              iv_file = lv_file
              iv_trid = iv_uptotrid
               ).
    ELSE.
      rv_success = zcl_utility_abaptogit_ado=>save_sync_status(
          EXPORTING
              iv_mode = iv_mode
              iv_file = lv_file
               ).
    ENDIF.

  ENDMETHOD.


  METHOD get_hr_schemapcrs.
    DATA wacode TYPE ts_code_object.
    DATA lt_schemas TYPE STANDARD TABLE OF ts_code_object.
    DATA lt_pcrs TYPE STANDARD TABLE OF ts_code_object.
    DATA lt_filecontent TYPE zcl_utility_abaptogit_tr=>tty_abaptext.
    DATA lv_basefolder TYPE string.
    DATA lv_commit_object TYPE zcl_utility_abaptogit_tr=>ts_commit_object.
    DATA lv_code_name TYPE string.
    DATA lv_path TYPE string.
    DATA lv_folder TYPE rlgrap-filename.
    DATA lv_codefolder TYPE string.
    DATA lv_filefolder TYPE string.
    DATA lt_filefolders TYPE TABLE OF string WITH DEFAULT KEY.
    DATA lv_success TYPE abap_bool.
    DATA lv_schpcrname TYPE string.
    DATA lv_progcls TYPE t52ba-pwert.

    IF iv_folder NP '*\'.
      lv_basefolder = iv_folder && '\'.
    ELSE.
      lv_basefolder = iv_folder.
    ENDIF.

    " root folder for a package like \src\.schemapcr\
    lv_folder = |{ lv_basefolder }{ zcl_utility_abaptogit_tr=>c_schemapcr }{ c_delim }|.
    lv_codefolder = lv_folder.
    CALL FUNCTION 'GUI_CREATE_DIRECTORY'
      EXPORTING
        dirname = lv_folder
      EXCEPTIONS
        failed  = 1
        OTHERS = 2.
    IF sy-subrc <> 0.
      me->write_telemetry( iv_message = |GET_HR_SCHEMAPCRS creates existing folder { lv_folder }| ).
    ENDIF.

    " download schemas
    SELECT sname, 'PSCC', 'PSCC', ' ' INTO TABLE @lt_schemas FROM t52cc WHERE sname LIKE 'Z%' OR sname LIKE 'Y%'.

    LOOP AT lt_schemas INTO wacode.

      CLEAR lt_filecontent.

      lv_schpcrname = wacode-obj_name.
      me->oref_tr->build_schema_content_active(
          EXPORTING
              iv_schemaname = lv_schpcrname
          IMPORTING
              et_filecontent = lt_filecontent
               ).
      SELECT SINGLE pwert FROM t52ba INTO @lv_progcls WHERE potyp = 'SCHE' AND ponam = @lv_schpcrname AND pattr = 'PCL'. "#EC WARNOK

      lv_commit_object-objname = wacode-obj_name.
      lv_commit_object-objtype = 'PSCC'.
      lv_commit_object-progcls = lv_progcls.
      lv_code_name = zcl_utility_abaptogit_ado=>build_code_name(
          EXPORTING
              iv_commit_object = lv_commit_object
              iv_local_folder = abap_true
              iv_base_folder = lv_codefolder
              iv_folder_structure = iv_folder_structure
          IMPORTING
              ev_file_folder = lv_filefolder
               ).
      IF NOT line_exists( lt_filefolders[ table_line = lv_filefolder ] ).
        APPEND lv_filefolder TO lt_filefolders.
        lv_folder = lv_filefolder.
        CALL FUNCTION 'GUI_CREATE_DIRECTORY'
          EXPORTING
            dirname = lv_folder
          EXCEPTIONS
            failed = 1
            OTHERS  = 2. "#EC FB_RC
        IF sy-subrc <> 0.
          me->write_telemetry( iv_message = |GET_HR_SCHEMAPCRS creates existing folder { lv_folder }| ).
        ENDIF.
      ENDIF.
      lv_path = |{ lv_basefolder }{ zcl_utility_abaptogit_tr=>c_schemapcr }{ c_delim }{ lv_code_name }|.
      CALL FUNCTION 'GUI_DOWNLOAD'
        EXPORTING
          filename              = lv_path
          filetype              = 'ASC'
          write_field_separator = 'X'
        TABLES
          data_tab              = lt_filecontent
        EXCEPTIONS
          OTHERS                = 1.
      IF sy-subrc <> 0.
        me->write_telemetry( iv_message = |GET_HR_SCHEMAPCRS fails to save local file { lv_path }| ).
        rv_success = abap_false.
      ENDIF.

    ENDLOOP.

    CLEAR: lt_schemas, lt_filecontent.

    " download PCRs
    SELECT cname, 'PCYC', 'PCYC', ' ' INTO TABLE @lt_pcrs FROM t52ce WHERE cname LIKE 'Z%' OR cname LIKE 'Y%'.

    LOOP AT lt_pcrs INTO wacode.

      CLEAR lt_filecontent.

      lv_schpcrname = wacode-obj_name.
      me->oref_tr->build_pcr_content_active(
          EXPORTING
              iv_pcrname = lv_schpcrname
          IMPORTING
              et_filecontent = lt_filecontent
               ).
      SELECT SINGLE pwert FROM t52ba INTO @lv_progcls WHERE potyp = 'CYCL' AND ponam = @lv_schpcrname AND pattr = 'PCL'. "#EC WARNOK

      lv_commit_object-objname = wacode-obj_name.
      lv_commit_object-objtype = 'PCYC'.
      lv_commit_object-progcls = lv_progcls.
      lv_code_name = zcl_utility_abaptogit_ado=>build_code_name(
          EXPORTING
              iv_commit_object = lv_commit_object
              iv_local_folder = abap_true
              iv_base_folder = lv_codefolder
              iv_folder_structure = iv_folder_structure
          IMPORTING
              ev_file_folder = lv_filefolder
               ).
      IF NOT line_exists( lt_filefolders[ table_line = lv_filefolder ] ).
        APPEND lv_filefolder TO lt_filefolders.
        lv_folder = lv_filefolder.
        CALL FUNCTION 'GUI_CREATE_DIRECTORY'
          EXPORTING
            dirname = lv_folder
          EXCEPTIONS
            failed = 1
            OTHERS  = 2. "#EC FB_RC
        IF sy-subrc <> 0.
          me->write_telemetry( iv_message = |GET_HR_SCHEMAPCRS creates existing folder { lv_folder }| ).
        ENDIF.
      ENDIF.
      lv_path = |{ lv_basefolder }{ zcl_utility_abaptogit_tr=>c_schemapcr }{ c_delim }{ lv_code_name }|.

      CALL FUNCTION 'GUI_DOWNLOAD'
        EXPORTING
          filename              = lv_path
          filetype              = 'ASC'
          write_field_separator = 'X'
        TABLES
          data_tab              = lt_filecontent
        EXCEPTIONS
          OTHERS                = 1.
      IF sy-subrc <> 0.
        me->write_telemetry( iv_message = |GET_HR_SCHEMAPCRS fails to save local file { lv_path }| ).
        rv_success = abap_false.
      ENDIF.

    ENDLOOP.

  ENDMETHOD.


  METHOD get_modifiable_trs.
    DATA lv_user TYPE string.
    DATA lt_trids1 TYPE zcl_utility_abaptogit_ado=>tty_trids.
    DATA lt_trids2 TYPE zcl_utility_abaptogit_ado=>tty_trids.
    DATA lt_keys TYPE TABLE OF string WITH DEFAULT KEY.
    IF iv_user IS INITIAL.
      lv_user = sy-uname.
    ELSE.
      lv_user = iv_user.
    ENDIF.
    SELECT tr~trkorr, tr~as4user, tr~as4date, tr~as4time, tr~trfunction, tx~as4text
        FROM e070 AS ts INNER JOIN e070 AS tr ON ts~strkorr = tr~trkorr
        INNER JOIN e07t AS tx ON tx~trkorr = tr~trkorr
        INTO TABLE @lt_trids1
        WHERE ( tr~trfunction = @c_wkbtrfunc OR tr~trfunction = @c_custtrfunc OR tr~trfunction = @c_toctrfunc )
            AND tr~trstatus = 'D' AND ts~as4user = @lv_user.
    LOOP AT lt_trids1 ASSIGNING FIELD-SYMBOL(<fs1>).
      IF NOT line_exists( lt_keys[ table_line = <fs1>-trid ] ).
        APPEND <fs1>-trid TO lt_keys.
        APPEND <fs1> TO et_trids.
      ENDIF.
    ENDLOOP.
    SELECT tr~trkorr, tr~as4user, tr~as4date, tr~as4time, tr~trfunction, tx~as4text
        FROM e070 AS tr INNER JOIN e07t AS tx ON tx~trkorr = tr~trkorr
        INTO TABLE @lt_trids2
        WHERE ( tr~trfunction = @c_wkbtrfunc OR tr~trfunction = @c_custtrfunc OR tr~trfunction = @c_toctrfunc )
            AND tr~trstatus = 'D' AND tr~as4user = @lv_user.
    LOOP AT lt_trids2 ASSIGNING FIELD-SYMBOL(<fs2>).
      IF NOT line_exists( lt_keys[ table_line = <fs2>-trid ] ).
        APPEND <fs2>-trid TO lt_keys.
        APPEND <fs2> TO et_trids.
      ENDIF.
    ENDLOOP.
  ENDMETHOD.


  METHOD get_packages_codes.

    DATA lv_basefolder TYPE string.
    DATA lv_package TYPE devclass.

    IF iv_uptotrid IS SUPPLIED AND iv_mode <> zcl_utility_abaptogit_tr=>c_latest_version.
      me->write_telemetry( iv_message = |latest version required for up-to TR ID supplied| ).
      rv_success = abap_false.
      RETURN.
    ENDIF.

    IF iv_folder NP '*\'.
      lv_basefolder = iv_folder && '\'.
    ELSE.
      lv_basefolder = iv_folder.
    ENDIF.

    DATA lv_success TYPE abap_bool.
    DATA lt_packages TYPE TABLE OF string.
    SPLIT iv_packages AT ',' INTO TABLE lt_packages.

    " save objects in each package specified under the folder named as package name like \src\<package name>\
    LOOP AT lt_packages INTO DATA(wa).
      lv_package = wa.
      IF iv_uptotrid IS SUPPLIED.
        lv_success = me->get_package_codes(
            EXPORTING
                iv_package = lv_package
                iv_folder = lv_basefolder
                iv_mode = iv_mode
                iv_uptotrid = iv_uptotrid
                iv_folder_structure = iv_folder_structure
                 ).
      ELSE.
        lv_success = me->get_package_codes(
            EXPORTING
                iv_package = lv_package
                iv_folder = lv_basefolder
                iv_mode = iv_mode
                iv_folder_structure = iv_folder_structure
                 ).
      ENDIF.
      IF lv_success <> abap_true.
        me->write_telemetry( iv_message = |GET_PACKAGES_CODES has failure in package { lv_package }| ).
        rv_success = abap_false.
      ENDIF.
    ENDLOOP.

    " mark down status of last sync TR ID and update date time
    DATA(lv_file) = |{ lv_basefolder }{ zcl_utility_abaptogit_ado=>c_sync_status_file }|.
    IF iv_uptotrid IS SUPPLIED.
      rv_success = zcl_utility_abaptogit_ado=>save_sync_status(
          EXPORTING
              iv_mode = iv_mode
              iv_file = lv_file
              iv_trid = iv_uptotrid
               ).
    ELSE.
      rv_success = zcl_utility_abaptogit_ado=>save_sync_status(
          EXPORTING
              iv_mode = iv_mode
              iv_file = lv_file
               ).
    ENDIF.

  ENDMETHOD.


  METHOD get_package_codes.

    DATA wacode TYPE ts_code_object.
    DATA lv_trid TYPE trkorr.
    DATA lv_subpackage TYPE devclass.
    DATA lt_codes TYPE STANDARD TABLE OF ts_code_object.
    DATA lt_fmcodes TYPE STANDARD TABLE OF ts_code_object.
    DATA lt_fgfmcodes TYPE STANDARD TABLE OF ts_code_object.
    DATA lt_objversions TYPE zcl_utility_abaptogit_tr=>tty_version_no.
    DATA lv_objname TYPE versobjnam.
    DATA lv_objtype TYPE versobjtyp.
    DATA lv_objtype2 TYPE versobjtyp.
    DATA lv_fugrname TYPE versobjnam.
    DATA lt_fugrs TYPE STANDARD TABLE OF ts_code_object.
    DATA lt_progs TYPE STANDARD TABLE OF ts_code_object.
    DATA lt_msags TYPE STANDARD TABLE OF ts_code_object.
    DATA lt_wdyc TYPE STANDARD TABLE OF ts_code_object_wydc.
    DATA lt_scnnums TYPE STANDARD TABLE OF d020s-dnum.
    DATA lv_basefolder TYPE string.
    DATA lt_subpackages TYPE zcl_utility_abaptogit_tr=>tty_package.
    DATA lt_filecontent TYPE zcl_utility_abaptogit_tr=>tty_abaptext.
    DATA lv_filecontent TYPE string.
    DATA lt_tclsfilecontent TYPE zcl_utility_abaptogit_tr=>tty_abaptext.
    DATA lv_tclsname TYPE string.
    DATA lv_tclstype TYPE string.
    DATA lv_commit_object TYPE zcl_utility_abaptogit_tr=>ts_commit_object.
    DATA lv_code_name TYPE string.
    DATA lv_path TYPE string.
    DATA lv_folder TYPE rlgrap-filename.
    DATA lv_codefolder TYPE string.
    DATA lv_success TYPE abap_bool.
    DATA lv_objname3 TYPE e071-obj_name.
    DATA lv_version_no TYPE i.
    DATA lv_dat TYPE d.
    DATA lv_tim TYPE t.
    DATA lv_subc TYPE reposrc-subc.
    DATA lv_filefolder TYPE string.
    DATA lt_filefolders TYPE tty_list.
    DATA lv_objname_varx TYPE e071-obj_name.
    DATA lv_objname_form TYPE e071-obj_name.
    DATA lv_objname_msag TYPE e071-obj_name.
    DATA lv_objname_msad TYPE e071-obj_name.

    IF iv_uptotrid IS SUPPLIED AND iv_mode <> zcl_utility_abaptogit_tr=>c_latest_version.
      me->write_telemetry( iv_message = |latest version required for up-to TR ID supplied| ).
      rv_success = abap_false.
      RETURN.
    ENDIF.

    IF iv_folder NP '*\'.
      lv_basefolder = iv_folder && '\'.
    ELSE.
      lv_basefolder = iv_folder.
    ENDIF.

    " find all sub packages including the package to filter in
    me->oref_tr->get_subpackages(
        EXPORTING
            iv_package = iv_package
        IMPORTING
            et_packages = lt_subpackages
             ).

    LOOP AT lt_subpackages INTO lv_subpackage.
      SELECT obj_name, object, object, ' ', @lv_subpackage APPENDING TABLE @lt_codes FROM tadir
          WHERE devclass = @lv_subpackage AND pgmid = 'R3TR'. "#EC CI_SGLSELECT
    ENDLOOP.

    " collect FMs/includes in function groups into list of code objects to download
    LOOP AT lt_codes INTO wacode.
      lv_objtype = wacode-obj_type.
      IF lv_objtype = 'FUGR'.
        lv_objname = wacode-obj_name.
        " find function modules in the function group
        DATA(pname_filter) = 'SAPL' && lv_objname.
        CLEAR lt_fgfmcodes.
        SELECT funcname, 'FUNC', 'FUNC', @lv_objname, @wacode-devclass INTO TABLE @lt_fgfmcodes FROM tfdir WHERE pname = @pname_filter. "#EC CI_SGLSELECT
        APPEND LINES OF lt_fgfmcodes TO lt_fmcodes.
        " find includes in the function group
        pname_filter = 'L' && lv_objname && '%'.
        CLEAR lt_fgfmcodes.
        SELECT name, 'REPS', 'FUNC', @lv_objname, @wacode-devclass INTO TABLE @lt_fgfmcodes FROM trdir WHERE name LIKE @pname_filter. "#EC CI_SGLSELECT
        APPEND LINES OF lt_fgfmcodes TO lt_fmcodes.
      ENDIF.
    ENDLOOP.
    APPEND LINES OF lt_fmcodes TO lt_codes.

    IF iv_uptotrid IS SUPPLIED.
      SELECT SINGLE as4date INTO lv_dat FROM e070 WHERE trkorr = iv_uptotrid AND trfunction = c_wkbtrfunc AND trstatus = c_relests.
      IF sy-subrc <> 0.
        me->write_telemetry( iv_message = |{ iv_uptotrid } is not a released workbench transport request| ).
        rv_success = abap_false.
        RETURN.
      ENDIF.
      SELECT SINGLE as4time INTO lv_tim FROM e070 WHERE trkorr = iv_uptotrid.
    ENDIF.

    rv_success = abap_true.

    " download each object source code lines
    LOOP AT lt_codes INTO wacode.

      lv_objname = wacode-obj_name.
      lv_objtype = wacode-obj_type.
      lv_objtype2 = wacode-obj_type2.
      lv_fugrname = wacode-fugr_name.

      " function groups to be processed separately
      IF lv_fugrname IS NOT INITIAL.
        IF NOT line_exists( lt_fugrs[ obj_name = lv_fugrname devclass = wacode-devclass ] ).
          APPEND VALUE ts_code_object( obj_name = lv_fugrname devclass = wacode-devclass ) TO lt_fugrs.
        ENDIF.
      ENDIF.

      " screen and report text of programs to be processed separately
      IF lv_objtype = 'PROG'.
        IF NOT line_exists( lt_progs[ obj_name = lv_objname devclass = wacode-devclass ] ).
          APPEND VALUE ts_code_object( obj_name = lv_objname devclass = wacode-devclass ) TO lt_progs.
        ENDIF.
      ENDIF.

      " Message Class to be processed separately
      IF lv_objtype = 'MSAG'.
        IF NOT line_exists( lt_progs[ obj_name = lv_objname devclass = wacode-devclass ] ).
          APPEND VALUE ts_code_object( obj_name = lv_objname devclass = wacode-devclass ) TO lt_msags.
        ENDIF.
      ENDIF.

      " function group was processed to extract function modules
      CHECK lv_objtype <> 'FUGR'.

      " skip object like '...$01' in function group
      CHECK lv_objtype2 <> 'FUNC' OR lv_objname NS '$'.

      IF lv_objtype = 'FORM'.
        CLEAR lt_filecontent.
        lv_objname_form = lv_objname.
        lv_success = zcl_utility_abaptogit_fmt=>get_sapscript_content(
            EXPORTING
                iv_objname = lv_objname_form
            IMPORTING
                ev_filecontent = lv_filecontent
                et_filecontent = lt_filecontent
                 ).
        IF lv_success <> abap_true.
          me->write_telemetry( iv_message = |GET_PACKAGE_CODES fails to fetch object content for { lv_objname } type { lv_objtype }| ).
          rv_success = abap_false.
          CONTINUE.
        ENDIF.

        " save the object content to local disk file
        lv_commit_object-objname = lv_objname.
        lv_commit_object-objtype = lv_objtype.
        lv_commit_object-devclass = wacode-devclass.
        rv_success = me->save_file(
            EXPORTING
                iv_commit_object = lv_commit_object
                iv_basefolder = lv_basefolder
                iv_folder_structure = iv_folder_structure
                iv_devclass = wacode-devclass
            IMPORTING
                ev_codefolder = lv_codefolder
                ev_filefolder = lv_filefolder
            CHANGING
                cht_filefolders = lt_filefolders
                cht_filecontent = lt_filecontent
                 ).
        CONTINUE.
      ENDIF.

      " fetch object version
      CLEAR lt_objversions.
      lv_objname3 = lv_objname.

      IF iv_uptotrid IS SUPPLIED.
        " use date/time of up-to TR ID to constrain version to select if latest version
        lv_trid = iv_uptotrid.
        lv_success = me->oref_tr->get_versions_no(
            EXPORTING
                iv_objname = lv_objname3
                iv_objtype = lv_objtype
                iv_mode = iv_mode
                iv_trid = lv_trid
                iv_date = lv_dat
                iv_time = lv_tim
                iv_findtest = abap_true
            IMPORTING
                ev_version_no = lv_version_no
            CHANGING
                cht_objversions = lt_objversions
                ).
      ELSE.
        " no up-to TR ID constraint
        lv_success = me->oref_tr->get_versions_no(
            EXPORTING
                iv_objname = lv_objname3
                iv_objtype = lv_objtype
                iv_mode = iv_mode
                iv_findtest = abap_true
            IMPORTING
                ev_version_no = lv_version_no
            CHANGING
                cht_objversions = lt_objversions
                ).
      ENDIF.
      " the object may not be ABAP code thus not versions fetched
      CHECK lv_success = abap_true.
      CHECK lines( lt_objversions ) > 0.

      IF lv_objtype = 'TABL' OR lv_objtype = 'TABD'.

        CLEAR lt_filecontent.
        lv_success = me->oref_tr->build_data_table_content(
            EXPORTING
                iv_objname = lv_objname3
                iv_version = lt_objversions[ 1 ]-objversionno
            IMPORTING
                ev_filecontent = lv_filecontent
                et_filecontent = lt_filecontent
                ).
        IF lv_success <> abap_true.
          me->write_telemetry( iv_message = |GET_PACKAGE_CODES fails to fetch object content for { lv_objname3 } type { lv_objtype }| ).
          rv_success = abap_false.
          CONTINUE.
        ENDIF.

      ELSEIF lv_objtype = 'DTEL' OR lv_objtype = 'DTED'.

        CLEAR lt_filecontent.
        lv_success = me->oref_tr->get_dataelement_content(
            EXPORTING
                iv_objname = lt_objversions[ 1 ]-objname
                iv_version = lt_objversions[ 1 ]-objversionno
            IMPORTING
                ev_filecontent = lv_filecontent
                et_filecontent = lt_filecontent
                 ).
        IF lv_success <> abap_true.
          me->write_telemetry( iv_message = |GET_PACKAGE_CODES fails to fetch object content for { lv_objname3 } type { lv_objtype }| ).
          rv_success = abap_false.
          CONTINUE.
        ENDIF.

      ELSEIF lv_objtype = 'DOMA' OR lv_objtype = 'DOMD'.

        CLEAR lt_filecontent.
        lv_success = me->oref_tr->get_domain_content(
            EXPORTING
                iv_objname = lt_objversions[ 1 ]-objname
                iv_version = lt_objversions[ 1 ]-objversionno
            IMPORTING
                ev_filecontent = lv_filecontent
                et_filecontent = lt_filecontent
                 ).
        IF lv_success <> abap_true.
          me->write_telemetry( iv_message = |GET_PACKAGE_CODES fails to fetch object content for { lv_objname3 } type { lv_objtype }| ).
          rv_success = abap_false.
          CONTINUE.
        ENDIF.

      ELSEIF lv_objtype = 'ENQU' OR lv_objtype = 'ENQD'.

        CLEAR lt_filecontent.
        lv_success = me->oref_tr->get_lockobject_content(
            EXPORTING
                iv_objname = lt_objversions[ 1 ]-objname
                iv_version = lt_objversions[ 1 ]-objversionno
            IMPORTING
                ev_filecontent = lv_filecontent
                et_filecontent = lt_filecontent
                 ).
        IF lv_success <> abap_true.
          me->write_telemetry( iv_message = |GET_PACKAGE_CODES fails to fetch object content for { lv_objname3 } type { lv_objtype }| ).
          rv_success = abap_false.
          CONTINUE.
        ENDIF.

      ELSEIF lv_objtype = 'SHLP' OR lv_objtype = 'SHLD'.

        CLEAR lt_filecontent.
        lv_success = me->oref_tr->get_searchhelp_content(
            EXPORTING
                iv_objname = lt_objversions[ 1 ]-objname
                iv_version = lt_objversions[ 1 ]-objversionno
            IMPORTING
                ev_filecontent = lv_filecontent
                et_filecontent = lt_filecontent
                 ).
        IF lv_success <> abap_true.
          me->write_telemetry( iv_message = |GET_PACKAGE_CODES fails to fetch object content for { lv_objname3 } type { lv_objtype }| ).
          rv_success = abap_false.
          CONTINUE.
        ENDIF.

      ELSEIF lv_objtype = 'TTYP' OR lv_objtype = 'TTYD'.

        CLEAR lt_filecontent.
        lv_success = me->oref_tr->get_tabletype_content(
            EXPORTING
                iv_objname = lt_objversions[ 1 ]-objname
                iv_version = lt_objversions[ 1 ]-objversionno
            IMPORTING
                ev_filecontent = lv_filecontent
                et_filecontent = lt_filecontent
                 ).
        IF lv_success <> abap_true.
          me->write_telemetry( iv_message = |GET_PACKAGE_CODES fails to fetch object content for { lv_objname3 } type { lv_objtype }| ).
          rv_success = abap_false.
          CONTINUE.
        ENDIF.

      ELSEIF lv_objtype = 'VIEW' OR lv_objtype = 'VIED'.

        CLEAR lt_filecontent.
        lv_success = me->oref_tr->get_view_content(
            EXPORTING
                iv_objname = lt_objversions[ 1 ]-objname
                iv_version = lt_objversions[ 1 ]-objversionno
            IMPORTING
                ev_filecontent = lv_filecontent
                et_filecontent = lt_filecontent
                 ).
        IF lv_success <> abap_true.
          me->write_telemetry( iv_message = |GET_PACKAGE_CODES fails to fetch object content for { lv_objname3 } type { lv_objtype }| ).
          rv_success = abap_false.
          CONTINUE.
        ENDIF.

      ELSEIF lv_objtype = 'XSLT'.

        CLEAR lt_filecontent.
        lv_success = me->oref_tr->get_xslt_content(
            EXPORTING
                iv_objname = lt_objversions[ 1 ]-objname
                iv_version = lt_objversions[ 1 ]-objversionno
            IMPORTING
                ev_filecontent = lv_filecontent
                et_filecontent = lt_filecontent
                 ).
        IF lv_success <> abap_true.
          me->write_telemetry( iv_message = |GET_PACKAGE_CODES fails to fetch object content for { lv_objname3 } type { lv_objtype }| ).
          rv_success = abap_false.
          CONTINUE.
        ENDIF.

*      ELSEIF lv_objtype = 'MSAG'.
*        CLEAR lt_filecontent.
*        lv_success = me->oref_tr->build_single_message_content(
*            EXPORTING
*                iv_objname = lt_objversions[ 1 ]-objname
*                iv_version = lt_objversions[ 1 ]-objversionno
*            IMPORTING
*                ev_filecontent = lv_filecontent
*                et_filecontent = lt_filecontent
*                 ).
*        IF lv_success <> abap_true.
*          me->write_telemetry( iv_message = |GET_PACKAGE_CODES fails to fetch object content for { lv_objname3 } type { lv_objtype }| ).
*          rv_success = abap_false.
*          CONTINUE.
*        ENDIF.

      ELSEIF lv_objtype = 'WDYN'.
        " Get web dynpro controller by definition(wdyn)
        DATA lt_wdyc_name TYPE TABLE OF wdy_controller.
        DATA lv_wdyctemp TYPE ts_code_object_wydc.
        zcl_utility_abaptogit_fmt=>get_wdyc_by_wdyn( EXPORTING iv_wdyn_name = lv_objname3 IMPORTING et_wdyc_name = lt_wdyc_name ).
        LOOP AT lt_wdyc_name INTO DATA(lv_wdyc).
            DATA(lv_wdyc_name) = lv_objname3.
            lv_wdyc_name+30 = lv_wdyc-controller_name.
            lv_wdyctemp-obj_name = lv_wdyc_name.
            lv_wdyctemp-obj_type = 'WDYC'.
            lv_wdyctemp-fugr_name = lv_objname3.
            lv_wdyctemp-devclass = wacode-devclass.
            APPEND lv_wdyctemp TO lt_wdyc.
            CLEAR: lv_wdyc,lv_wdyctemp.
        ENDLOOP.

        lv_success = me->oref_tr->build_wdyd_json(
          EXPORTING
              iv_version = lt_objversions[ 1 ]-objversionno
              iv_objname = lt_objversions[ 1 ]-objname
          IMPORTING
              ev_filecontent = lv_filecontent
              et_filecontent = lt_filecontent
              ).
      ELSE.

        " construct object content (and test class content if any) as source code lines
        CLEAR lt_filecontent.
        CLEAR lt_tclsfilecontent.
        lv_success = me->oref_tr->build_code_content(
            EXPORTING
                iv_objname = lv_objname3
                iv_objtype = lv_objtype
                it_objversions = lt_objversions
            IMPORTING
                et_filecontent = lt_filecontent
                ev_tclsname = lv_tclsname
                ev_tclstype = lv_tclstype
                et_tclsfilecontent = lt_tclsfilecontent
                ).
        IF lv_success <> abap_true.
          me->write_telemetry( iv_message = |GET_PACKAGE_CODES fails to fetch code content for { lv_objname3 } type { lv_objtype }| ).
          rv_success = abap_false.
          CONTINUE.
        ENDIF.

      ENDIF.

      CLEAR lv_subc.
      SELECT SINGLE subc FROM reposrc INTO @lv_subc WHERE progname = @lv_objname3. "#EC WARNOK

      " save the object content to local disk file
      lv_commit_object-objname = lv_objname3.
      lv_commit_object-objtype = lv_objtype.
      lv_commit_object-objtype2 = lv_objtype2.
      lv_commit_object-fugr = lv_fugrname.
      lv_commit_object-subc = lv_subc.
      lv_commit_object-devclass = wacode-devclass.
      rv_success = me->save_file(
          EXPORTING
              iv_commit_object = lv_commit_object
              iv_basefolder = lv_basefolder
              iv_folder_structure = iv_folder_structure
              iv_devclass = wacode-devclass
          IMPORTING
              ev_codefolder = lv_codefolder
              ev_filefolder = lv_filefolder
          CHANGING
              cht_filefolders = lt_filefolders
              cht_filecontent = lt_filecontent
               ).

      " save the object content to local disk file for test class if any since it must be attached to a product class
      IF lines( lt_tclsfilecontent ) > 0.
        " following abapGit where class name is used for test class name instead of ====CCAU like one
        lv_commit_object-objname = lv_objname3.
        lv_commit_object-objtype = lv_tclstype.
        rv_success = me->save_file(
            EXPORTING
                iv_commit_object = lv_commit_object
                iv_basefolder = lv_basefolder
                iv_folder_structure = iv_folder_structure
                iv_devclass = wacode-devclass
            IMPORTING
                ev_codefolder = lv_codefolder
                ev_filefolder = lv_filefolder
            CHANGING
                cht_filefolders = lt_filefolders
                cht_filecontent = lt_tclsfilecontent
                 ).
      ENDIF.

    ENDLOOP.

    LOOP AT lt_fugrs INTO DATA(wafugr).

      CLEAR: lv_version_no, lt_objversions.
      lv_objname3 = |SAPL{ wafugr-obj_name }|.
      lv_objtype = 'REPS'.
      lv_success = me->oref_tr->get_versions_no(
          EXPORTING
              iv_objname = lv_objname3
              iv_objtype = lv_objtype
              iv_mode = iv_mode
              iv_trid = lv_trid
              iv_date = lv_dat
              iv_time = lv_tim
              iv_findtest = abap_false
          IMPORTING
              ev_version_no = lv_version_no
          CHANGING
              cht_objversions = lt_objversions
              ).
      CHECK lv_success = abap_true.
      CHECK lv_version_no > 0.

      CLEAR: lt_filecontent, lv_filecontent.
      rv_success = me->oref_tr->build_code_content(
          EXPORTING
              iv_objname = lv_objname3
              iv_objtype = lv_objtype
              it_objversions = lt_objversions
          IMPORTING
              et_filecontent = lt_filecontent
              ).
      CHECK rv_success = abap_true.

      lv_filecontent = concat_lines_of( table = lt_filecontent sep = cl_abap_char_utilities=>cr_lf ).
      lv_filecontent = lv_filecontent && cl_abap_char_utilities=>cr_lf.

      lv_objname3 = wafugr-obj_name.
      lv_objtype = 'FUGR'.
      CLEAR lv_commit_object.
      lv_commit_object-objname = lv_objname3.
      lv_commit_object-objtype = lv_objtype.
      lv_commit_object-devclass = wafugr-devclass.
      lv_success = me->save_file(
          EXPORTING
              iv_commit_object = lv_commit_object
              iv_basefolder = lv_basefolder
              iv_folder_structure = iv_folder_structure
              iv_devclass = wacode-devclass
          IMPORTING
              ev_codefolder = lv_codefolder
              ev_filefolder = lv_filefolder
          CHANGING
              cht_filefolders = lt_filefolders
              cht_filecontent = lt_filecontent
               ).

    ENDLOOP.

    LOOP AT lt_progs INTO DATA(waprog).

      "report texts of the program
      CLEAR: lv_version_no, lt_objversions.
      lv_objname3 = waprog-obj_name.
      lv_objtype = 'REPT'.
      lv_objtype2 = 'PROG'.
      lv_success = me->oref_tr->get_versions_no(
          EXPORTING
              iv_objname = lv_objname3
              iv_objtype = lv_objtype
              iv_mode = iv_mode
              iv_trid = lv_trid
              iv_date = lv_dat
              iv_time = lv_tim
              iv_findtest = abap_false
          IMPORTING
              ev_version_no = lv_version_no
          CHANGING
              cht_objversions = lt_objversions
              ).
      CHECK lv_success = abap_true.
      CHECK lines( lt_objversions ) > 0.

      CLEAR: lv_filecontent, lt_filecontent.
      me->oref_tr->get_rept_content(
          EXPORTING
              iv_version = lt_objversions[ 1 ]-objversionno
              iv_objname = lt_objversions[ 1 ]-objname
          IMPORTING
              ev_filecontent = lv_filecontent
              et_filecontent = lt_filecontent
               ).

      CLEAR lv_commit_object.
      lv_commit_object-objname = lv_objname3.
      lv_commit_object-objtype = lv_objtype.
      lv_commit_object-objtype2 = lv_objtype2.
      lv_commit_object-devclass = waprog-devclass.
      lv_success = me->save_file(
          EXPORTING
              iv_commit_object = lv_commit_object
              iv_basefolder = lv_basefolder
              iv_folder_structure = iv_folder_structure
              iv_devclass = wacode-devclass
          IMPORTING
              ev_codefolder = lv_codefolder
              ev_filefolder = lv_filefolder
          CHANGING
              cht_filefolders = lt_filefolders
              cht_filecontent = lt_filecontent
               ).

      " screens of the program
      CLEAR lt_scnnums.
      SELECT dnum FROM d020s INTO TABLE lt_scnnums WHERE prog = waprog-obj_name.
      LOOP AT lt_scnnums INTO DATA(wascnnum).

        CLEAR: lv_version_no, lt_objversions.
        lv_objname3 = |{ waprog-obj_name }{ wascnnum }|.
        lv_objtype = 'DYNP'.

        IF iv_uptotrid IS SUPPLIED.
          " use date/time of up-to TR ID to constrain version to select if latest version
          lv_trid = iv_uptotrid.
          lv_success = me->oref_tr->get_versions_no(
              EXPORTING
                  iv_objname = lv_objname3
                  iv_objtype = lv_objtype
                  iv_mode = iv_mode
                  iv_trid = lv_trid
                  iv_date = lv_dat
                  iv_time = lv_tim
                  iv_findtest = abap_false
              IMPORTING
                  ev_version_no = lv_version_no
              CHANGING
                  cht_objversions = lt_objversions
                  ).
        ELSE.
          " no up-to TR ID constraint
          lv_success = me->oref_tr->get_versions_no(
              EXPORTING
                  iv_objname = lv_objname3
                  iv_objtype = lv_objtype
                  iv_mode = iv_mode
                  iv_findtest = abap_true
              IMPORTING
                  ev_version_no = lv_version_no
              CHANGING
                  cht_objversions = lt_objversions
                  ).
        ENDIF.
        CHECK lv_success = abap_true.
        CHECK lines( lt_objversions ) > 0.

        CLEAR: lv_filecontent, lt_filecontent.
        lv_success = me->oref_tr->get_dynp_content(
            EXPORTING
                iv_objname = lt_objversions[ 1 ]-objname
                iv_version = lt_objversions[ 1 ]-objversionno
            IMPORTING
                ev_filecontent = lv_filecontent
                et_filecontent = lt_filecontent
                 ).
        IF lv_success <> abap_true.
          me->write_telemetry( iv_message = |GET_PACKAGE_CODES fails to fetch object content for { lv_objname3 } type { lv_objtype }| ).
          CONTINUE.
        ENDIF.

        CLEAR lv_commit_object.
        lv_commit_object-objname = lv_objname3.
        lv_commit_object-objtype = lv_objtype.
        lv_commit_object-devclass = waprog-devclass.
        rv_success = me->save_file(
            EXPORTING
                iv_commit_object = lv_commit_object
                iv_basefolder = lv_basefolder
                iv_folder_structure = iv_folder_structure
                iv_devclass = wacode-devclass
            IMPORTING
                ev_codefolder = lv_codefolder
                ev_filefolder = lv_filefolder
            CHANGING
                cht_filefolders = lt_filefolders
                cht_filecontent = lt_filecontent
                 ).

      ENDLOOP.

    ENDLOOP.

    LOOP AT lt_msags INTO DATA(wamsag).
      CLEAR: lv_version_no, lt_objversions.

      SELECT * FROM t100 INTO TABLE @DATA(lt_t100)
      WHERE arbgb        = @wamsag-obj_name.

      "handle MESS in MSAG
      LOOP AT lt_t100 INTO DATA(ls_t100).
        lv_objname3 = |{ ls_t100-arbgb }{ ls_t100-msgnr }|.
        lv_objtype = 'MESS'.
        CLEAR: lv_version_no, lt_objversions.
        IF iv_uptotrid IS SUPPLIED.
          " use date/time of up-to TR ID to constrain version to select if latest version
          lv_trid = iv_uptotrid.
          lv_success = me->oref_tr->get_versions_no(
              EXPORTING
                  iv_objname = lv_objname3
                  iv_objtype = lv_objtype
                  iv_mode = iv_mode
                  iv_trid = lv_trid
                  iv_date = lv_dat
                  iv_time = lv_tim
                  iv_findtest = abap_false
              IMPORTING
                  ev_version_no = lv_version_no
              CHANGING
                  cht_objversions = lt_objversions
                  ).
        ELSE.
          " no up-to TR ID constraint
          lv_success = me->oref_tr->get_versions_no(
              EXPORTING
                  iv_objname = lv_objname3
                  iv_objtype = lv_objtype
                  iv_mode = iv_mode
                  iv_findtest = abap_true
              IMPORTING
                  ev_version_no = lv_version_no
              CHANGING
                  cht_objversions = lt_objversions
                  ).
        ENDIF.

        CHECK lv_success = abap_true.
        CHECK lines( lt_objversions ) > 0.

        CLEAR: lv_filecontent, lt_filecontent.
        me->oref_tr->build_single_message_content(
          EXPORTING
              iv_version = lt_objversions[ 1 ]-objversionno
              iv_objname = lt_objversions[ 1 ]-objname
          IMPORTING
              ev_filecontent = lv_filecontent
              et_filecontent = lt_filecontent
               ).

        CLEAR lv_commit_object.
        lv_commit_object-objname = lv_objname3.
        lv_commit_object-objtype = lv_objtype.
        lv_commit_object-devclass = wamsag-devclass.
        lv_success = me->save_file(
            EXPORTING
                iv_commit_object = lv_commit_object
                iv_basefolder = lv_basefolder
                iv_folder_structure = iv_folder_structure
                iv_devclass = wacode-devclass
            IMPORTING
                ev_codefolder = lv_codefolder
                ev_filefolder = lv_filefolder
            CHANGING
                cht_filefolders = lt_filefolders
                cht_filecontent = lt_filecontent
                 ).
      ENDLOOP.

      " handle MSAD in MSAG
      lv_objname_msad = wamsag-obj_name.
      lv_objtype = 'MSAD'.
      CLEAR: lv_version_no, lt_objversions.
      IF iv_uptotrid IS SUPPLIED.
        " use date/time of up-to TR ID to constrain version to select if latest version
        lv_trid = iv_uptotrid.
        lv_success = me->oref_tr->get_versions_no(
            EXPORTING
                iv_objname = lv_objname_msad
                iv_objtype = lv_objtype
                iv_mode = iv_mode
                iv_trid = lv_trid
                iv_date = lv_dat
                iv_time = lv_tim
                iv_findtest = abap_false
            IMPORTING
                ev_version_no = lv_version_no
            CHANGING
                cht_objversions = lt_objversions
                ).
      ELSE.
        " no up-to TR ID constraint
        lv_success = me->oref_tr->get_versions_no(
            EXPORTING
                iv_objname = lv_objname_msad
                iv_objtype = lv_objtype
                iv_mode = iv_mode
                iv_findtest = abap_true
            IMPORTING
                ev_version_no = lv_version_no
            CHANGING
                cht_objversions = lt_objversions
                ).
      ENDIF.

      CHECK lv_success = abap_true.
      CHECK lines( lt_objversions ) > 0.

      CLEAR: lv_filecontent, lt_filecontent.
      me->oref_tr->build_msad_content(
        EXPORTING
            iv_version = lt_objversions[ 1 ]-objversionno
            iv_objname = lt_objversions[ 1 ]-objname
        IMPORTING
            ev_filecontent = lv_filecontent
            et_filecontent = lt_filecontent
             ).

      CLEAR lv_commit_object.
      lv_commit_object-objname = lv_objname_msad.
      lv_commit_object-objtype = lv_objtype.
      lv_commit_object-devclass = wamsag-devclass.
      lv_success = me->save_file(
          EXPORTING
              iv_commit_object = lv_commit_object
              iv_basefolder = lv_basefolder
              iv_folder_structure = iv_folder_structure
              iv_devclass = wacode-devclass
          IMPORTING
              ev_codefolder = lv_codefolder
              ev_filefolder = lv_filefolder
          CHANGING
              cht_filefolders = lt_filefolders
              cht_filecontent = lt_filecontent
               ).
    ENDLOOP.

    LOOP AT lt_wdyc INTO DATA(wawdyc).
      lv_objname3 = |{ wawdyc-obj_name }|.
      lv_objtype = 'WDYC'.
      CLEAR: lv_version_no, lt_objversions, lt_filecontent, lv_filecontent.
      lv_success = me->oref_tr->get_versions_no(
          EXPORTING
              iv_objname = lv_objname3
              iv_objtype = wawdyc-obj_type
              iv_mode = iv_mode
              iv_trid = lv_trid
              iv_date = lv_dat
              iv_time = lv_tim
              iv_findtest = abap_false
          IMPORTING
              ev_version_no = lv_version_no
          CHANGING
              cht_objversions = lt_objversions
              ).

      CHECK lv_success = abap_true.
      CHECK lv_version_no > 0.

      DATA lt_wdyc_content TYPE zcl_utility_abaptogit_tr=>tty_controller_content.
      DATA lv_objectname TYPE string.
      DATA lv_len TYPE i.
      DATA lv_controller_name TYPE string.
      DATA lv_component_name TYPE string.

      lv_objectname = lt_objversions[ 1 ]-objname.
      lv_len = strlen( lv_objectname ) - 30.
      lv_controller_name = lv_objectname+30(lv_len).
      lv_component_name = lv_objectname(30).
      CONDENSE lv_component_name.

      lv_success = me->oref_tr->build_wdyc_json(
      EXPORTING
          iv_version = lt_objversions[ 1 ]-objversionno
          iv_controllername = lv_controller_name
          iv_componentname = lv_component_name
          iv_mode = iv_mode
      IMPORTING
          et_filecontent = lt_wdyc_content
          ).

      CHECK lv_success = abap_true.
      lv_objname = |{ lv_component_name }_{ lv_controller_name }|.
      lv_commit_object-objname = lv_objname.
      lv_commit_object-objtype = lv_objtype.
      lv_commit_object-devclass = wafugr-devclass.
      LOOP AT lt_wdyc_content INTO DATA(lv_wdyc_content).
        SPLIT lv_wdyc_content-controller_content AT cl_abap_char_utilities=>newline INTO TABLE lt_filecontent.
        lv_commit_object-objtype2 = lv_wdyc_content-controller_content_type.
        lv_commit_object-fugr = lv_component_name.
        lv_success = me->save_file(
          EXPORTING
              iv_commit_object = lv_commit_object
              iv_basefolder = lv_basefolder
              iv_folder_structure = iv_folder_structure
              iv_devclass = wacode-devclass
          IMPORTING
              ev_codefolder = lv_codefolder
              ev_filefolder = lv_filefolder
          CHANGING
              cht_filefolders = lt_filefolders
              cht_filecontent = lt_filecontent
               ).
     ENDLOOP.
    ENDLOOP.
  ENDMETHOD.


  METHOD get_package_classes.

    DATA wacode TYPE ts_code_object.
    DATA lv_subpackage TYPE devclass.
    DATA lt_subpackages TYPE zcl_utility_abaptogit_tr=>tty_package.
    DATA lt_codes TYPE STANDARD TABLE OF ts_code_object.
    DATA lt_objversions TYPE zcl_utility_abaptogit_tr=>tty_version_no.
    DATA lv_version_no TYPE i.
    DATA lv_objname TYPE versobjnam.
    DATA lv_objtype TYPE versobjtyp.
    DATA lv_objname2 TYPE string.
    DATA lv_objname3 TYPE e071-obj_name.
    DATA lv_success TYPE abap_bool.

    " find all sub packages including the package to filter in
    me->oref_tr->get_subpackages(
        EXPORTING
            iv_package = iv_packagename
        IMPORTING
            et_packages = lt_subpackages
             ).

    LOOP AT lt_subpackages INTO lv_subpackage.
      SELECT obj_name, object, object, ' ', @lv_subpackage APPENDING TABLE @lt_codes FROM tadir
          WHERE devclass = @lv_subpackage AND pgmid = 'R3TR' AND object = 'CLAS'. "#EC CI_SGLSELECT
    ENDLOOP.

    LOOP AT lt_codes INTO wacode.

      lv_objname = wacode-obj_name.
      lv_objtype = wacode-obj_type.
      lv_objname2 = lv_objname.
      lv_objname3 = lv_objname.

      CLEAR lt_objversions.

      lv_success = me->oref_tr->get_versions_no(
          EXPORTING
              iv_objname = lv_objname3
              iv_objtype = lv_objtype
              iv_mode = iv_mode
              iv_findtest = abap_true
          IMPORTING
              ev_version_no = lv_version_no
          CHANGING
              cht_objversions = lt_objversions
           ).
      CHECK lv_success = abap_true.
      CHECK lines( lt_objversions ) > 0.

      me->oref_tr->get_class_info(
          EXPORTING
              iv_name = lv_objname2
              it_objversions = lt_objversions
          IMPORTING
              et_classes = et_classes
            ).

    ENDLOOP.

  ENDMETHOD.


  METHOD get_related_pullrequest.
    DATA lv_slrelbranch TYPE string.
    DATA lv_owner TYPE string.
    rv_success = me->oref_tr->get_tr_owner(
        EXPORTING
            iv_trid = iv_trid
        IMPORTING
            ev_owner = lv_owner
             ).
    CHECK rv_success = abap_true.
    me->prepare_landscape_branch( EXPORTING iv_prefix = iv_prefix IMPORTING ev_branch = lv_slrelbranch ).
    DATA(lv_privatebranch) = |users/{ lv_owner }/TR/{ iv_trid }|.
    rv_success = me->oref_ado->find_active_pullrequest(
        EXPORTING
            iv_srcbranch = lv_privatebranch
            iv_tarbranch = lv_slrelbranch
        IMPORTING
            ev_prurl = ev_prurl
             ).
    IF rv_success = abap_true AND ev_prurl IS NOT INITIAL.
      RETURN.
    ENDIF.
    rv_success = me->oref_ado->find_completed_pullrequest(
        EXPORTING
            iv_srcbranch = lv_privatebranch
            iv_tarbranch = lv_slrelbranch
        IMPORTING
            ev_prurl = ev_prurl
             ).
  ENDMETHOD.


  METHOD get_tr_object_latest_update.

    DATA lt_commit_objects TYPE zcl_utility_abaptogit_tr=>tty_commit_object.
    DATA lv_comment TYPE string.
    DATA lv_date TYPE d.
    DATA lv_time TYPE t.
    DATA lv_success TYPE abap_bool.
    DATA lv_codefolder TYPE string.

    " TR last update time will be self-updated when releasing the TR, even not successful
    lv_success = me->oref_tr->get_tr_commit_objects(
        EXPORTING
            iv_trid = iv_trid
            iv_packagenames = iv_packagenames
            iv_mode = zcl_utility_abaptogit_tr=>c_active_version
            iv_needcontent = abap_true
            it_excl_objs = it_excl_objs
            it_excl_tbls = it_excl_tbls
        IMPORTING
            ev_owner = ev_owner
            ev_comment = lv_comment
            ev_ossnote = ev_ossnote
        CHANGING
            it_commit_objects = lt_commit_objects
             ).
    CHECK lv_success = abap_true.

    LOOP AT lt_commit_objects ASSIGNING FIELD-SYMBOL(<fs>).
      get_utc_datetime(
          EXPORTING
              iv_date = <fs>-date
              iv_time = <fs>-time
          IMPORTING
              ev_date = <fs>-date
              ev_time = <fs>-time
               ).
      IF <fs>-date IS NOT INITIAL OR <fs>-time IS NOT INITIAL.
        IF <fs>-date > lv_date OR ( <fs>-date = lv_date AND <fs>-time > lv_time ).
          lv_date = <fs>-date.
          lv_time = <fs>-time.
        ENDIF.
      ENDIF.
      DATA(lv_code_name) = zcl_utility_abaptogit_ado=>build_code_name(
          EXPORTING
              iv_commit_object = <fs>
              iv_local_folder = abap_false
              iv_base_folder = lv_codefolder
               ).
      DATA(lv_filepath) = |/SRC/{ <fs>-devclass }/{ lv_code_name }|.
      APPEND VALUE ty_obj_update(
          path = lv_filepath
          date = |{ <fs>-date }|
          time = |{ <fs>-time }|
          type = <fs>-objtype )
          TO et_items.
    ENDLOOP.

    IF lv_date IS NOT INITIAL AND lv_time IS NOT INITIAL.
      ev_date = lv_date.
      ev_time = lv_time.
    ELSE.
      ev_date = '20000101'.
      ev_time = '000000'.
    ENDIF.

  ENDMETHOD.


  METHOD get_utc_datetime.
    DATA lv_tstmp TYPE timestamp.
    DATA lv_tstmptext TYPE string.
    DATA lv_dateutc TYPE string.
    DATA lv_timeutc TYPE string.
    IF iv_date = '00000000'.
      ev_date = iv_date.
      ev_time = iv_time.
      RETURN.
    ENDIF.
    CONVERT DATE iv_date TIME iv_time INTO TIME STAMP lv_tstmp TIME ZONE sy-zonlo.
    lv_tstmptext = lv_tstmp.
    lv_dateutc = |{ lv_tstmptext(8) }|.
    lv_timeutc = |{ lv_tstmptext+8 }|.
    ev_date = |{ lv_dateutc(4) }{ lv_dateutc+4(2) }{ lv_dateutc+6(2) }|.
    ev_time = |{ lv_timeutc(2) }{ lv_timeutc+2(2) }{ lv_timeutc+4(2) }|.
  ENDMETHOD.


  METHOD get_wid_from_title.
    DATA lt_parts_colon TYPE TABLE OF string.
    DATA lt_parts_dash TYPE TABLE OF string.
    SPLIT iv_title AT ':' INTO TABLE lt_parts_colon.
    CLEAR ev_wid.
    CHECK lines( lt_parts_colon ) > 1.
    SPLIT lt_parts_colon[ 1 ] AT '-' INTO TABLE lt_parts_dash.
    IF lines( lt_parts_dash ) > 1.
      ev_wid = lt_parts_dash[ lines( lt_parts_dash ) ].
    ELSE.
      ev_wid = lt_parts_dash[ 1 ].
    ENDIF.
    IF ev_wid CN '1234567890 '.
      CLEAR ev_wid.
    ENDIF.
  ENDMETHOD.


  METHOD heatmap_trs.

    DATA lt_trids TYPE zcl_utility_abaptogit_ado=>tty_trids.
    DATA lt_commit_objects TYPE zcl_utility_abaptogit_tr=>tty_commit_object.
    DATA lt_stats TYPE TABLE OF string INITIAL SIZE 0.
    DATA lv_insertions TYPE i.
    DATA lv_deletions TYPE i.
    DATA lv_objects TYPE i.
    DATA lv_tables TYPE i.
    DATA lv_rows TYPE i.

    rv_success = abap_true.

    zcl_utility_abaptogit_ado=>get_trs_daterange(
        EXPORTING
            iv_fromdat = iv_fromdat
            iv_todat = iv_todat
            iv_mode = iv_mode
        IMPORTING
            et_trids = lt_trids
             ).

    APPEND |System,User,TR,Function,Date,Time,Objects,Insertions,Deletions,Tables,Rows| TO lt_stats.

    LOOP AT lt_trids INTO DATA(wa_trid).
      IF lines( it_users ) > 0 AND NOT line_exists( it_users[ table_line = wa_trid-user ] ).
        CONTINUE.
      ENDIF.
      CLEAR: lt_commit_objects, lv_objects, lv_insertions, lv_deletions, lv_tables, lv_rows.
      me->oref_tr->get_tr_commit_objects(
          EXPORTING
              iv_trid = wa_trid-trid
              iv_packagenames = iv_packagenames
              iv_mode = iv_mode
              iv_deltastats = abap_true
          CHANGING
              it_commit_objects = lt_commit_objects
           ).
      LOOP AT lt_commit_objects INTO DATA(wa_co).
        lv_objects = lv_objects + 1.
        lv_insertions = lv_insertions + wa_co-insertions.
        lv_deletions = lv_deletions + wa_co-deletions.
        lv_tables = lv_tables + wa_co-tables.
        IF wa_co-rows > 0.
          lv_rows = lv_rows + wa_co-rows - 1.
        ENDIF.
      ENDLOOP.
      DATA(lv_date) = |{ wa_trid-dat+4(2) }/{ wa_trid-dat+6(2) }/{ wa_trid-dat(4) }|.
      DATA(lv_time) = |{ wa_trid-tim(2) }:{ wa_trid-tim+2(2) }:{ wa_trid-tim+4(2) }|.
      APPEND |{ sy-sysid },{ wa_trid-user },{ wa_trid-trid },{ wa_trid-func },{ lv_date },{ lv_time },{ lv_objects },{ lv_insertions },{ lv_deletions },{ lv_tables },{ lv_rows }| TO lt_stats.
    ENDLOOP.

    CALL FUNCTION 'GUI_DOWNLOAD'
      EXPORTING
        filename              = iv_resultfile
        filetype              = 'ASC'
        write_field_separator = 'X'
      TABLES
        data_tab              = lt_stats
      EXCEPTIONS
        OTHERS                = 1.
    IF sy-subrc <> 0.
      me->write_telemetry( iv_message = |HEATMAP_TRS fails to save local file { iv_resultfile }| ).
      rv_success = abap_false.
    ENDIF.

  ENDMETHOD.


  METHOD is_trtasks_released.
    DATA ls_info TYPE zcl_utility_abaptogit_tr=>ts_tr_info.
    rv_success = me->oref_tr->get_tr_info(
        EXPORTING
            iv_trid = iv_trid
        IMPORTING
            ev_info = ls_info
             ).
    CHECK rv_success = abap_true.
    LOOP AT ls_info-tasks INTO DATA(wa).
      IF wa-status <> 'R' AND wa-status <> 'N'.
        ev_released = abap_false.
        RETURN.
      ENDIF.
    ENDLOOP.
    ev_released = abap_true.
  ENDMETHOD.


  METHOD prepare_landscape_branch.
    " main branch name like users/system/<system id>
    ev_branch = |{ iv_prefix }{ sy-sysid }|.
  ENDMETHOD.


  METHOD prepare_pullrequest_branches.

    DATA lv_slrelbranch TYPE string.
    DATA lv_owner TYPE string.
    DATA lv_completedprurl TYPE string.

    " get TR owner
    rv_success = me->oref_tr->get_tr_owner(
        EXPORTING
            iv_trid = iv_trid
        IMPORTING
            ev_owner = lv_owner
             ).
    CHECK rv_success = abap_true.
    ev_prvbranch = |users/{ lv_owner }/TR/{ iv_trid }|.

    me->prepare_landscape_branch( EXPORTING iv_prefix = iv_prefix IMPORTING ev_branch = lv_slrelbranch ).
    ev_slrelbranch = lv_slrelbranch.

    " check if active PR for the branches exists
    rv_success = me->oref_ado->find_active_pullrequest(
        EXPORTING
            iv_srcbranch = ev_prvbranch
            iv_tarbranch = lv_slrelbranch
        IMPORTING
            ev_prurl = ev_activeprurl
             ).
    CHECK rv_success = abap_true.
    CHECK ev_activeprurl IS INITIAL.

    " check if there's any completed PR for the branches
    rv_success = me->oref_ado->find_completed_pullrequest(
        EXPORTING
            iv_srcbranch = ev_prvbranch
            iv_tarbranch = lv_slrelbranch
        IMPORTING
            ev_prurl = lv_completedprurl
             ).
    CHECK rv_success = abap_true.
    IF lv_completedprurl IS INITIAL.
      ev_firstpr = abap_true.
    ELSE.
      ev_firstpr = abap_false.
    ENDIF.

    " create private branch from SL release branch
    rv_success = me->oref_ado->create_branch(
        EXPORTING
            iv_basebranch = lv_slrelbranch
            iv_newbranch = ev_prvbranch
             ).
    CHECK rv_success = abap_true.
    me->write_telemetry( iv_message = |created private branch { ev_prvbranch }| iv_kind = 'info' ).

  ENDMETHOD.


  METHOD revise_pullrequest.

    DATA lv_slrelbranch TYPE string.
    DATA lt_commit_objects TYPE zcl_utility_abaptogit_tr=>tty_commit_object.
    DATA lv_owner TYPE string.
    DATA lv_comment TYPE string.
    DATA lv_synccnt TYPE i.
    DATA lv_author TYPE string.

    " get active version of TR objects
    rv_success = me->oref_tr->get_tr_commit_objects(
        EXPORTING
            iv_trid = iv_trid
            iv_packagenames = iv_packagenames
            iv_mode = zcl_utility_abaptogit_tr=>c_active_version
            it_excl_objs = it_excl_objs
            it_excl_tbls = it_excl_tbls
        IMPORTING
            ev_owner = lv_owner
            ev_comment = lv_comment
        CHANGING
            it_commit_objects = lt_commit_objects
             ).
    CHECK rv_success = abap_true.

    DATA(lv_privatebranch) = |users/{ lv_owner }/TR/{ iv_trid }|.

    IF iv_comment IS NOT INITIAL.
      lv_comment = iv_comment.
    ENDIF.

    IF iv_curuser = abap_true.
      lv_author = sy-uname.
    ELSE.
      lv_author = lv_owner.
    ENDIF.

    " sync active version of TR objects to private branch
    rv_success = me->oref_ado->push_tr_commit_objects(
        EXPORTING
            iv_trid = iv_trid
            iv_user = lv_author
            iv_domain = iv_domain
            iv_branch = lv_privatebranch
            iv_comment = lv_comment
            iv_push_status = abap_false
            it_commit_objects = lt_commit_objects
        IMPORTING
            ev_synccnt = lv_synccnt
             ).
    CHECK rv_success = abap_true.

    IF lv_synccnt = 0.
      me->write_telemetry( iv_message = |TR { iv_trid } has no objects matching criteria to sync to private branch { lv_privatebranch }| iv_kind = 'info' ).
      RETURN.
    ELSE.
      me->write_telemetry( iv_message = |push TR { iv_trid } objects ({ lv_synccnt }) to private branch { lv_privatebranch }| iv_kind = 'info' ).
    ENDIF.

    me->write_telemetry( iv_message = |continue on existing pull request process| iv_kind = 'info' ).

  ENDMETHOD.


  METHOD save_file.
    DATA lv_code_name TYPE string.
    DATA lv_path TYPE string.
    DATA lv_folder TYPE rlgrap-filename.
    DATA lv_filefolder TYPE string.
    rv_success = abap_true.
    ev_codefolder = |{ iv_basefolder }{ iv_devclass }{ c_delim }|.
    lv_folder = ev_codefolder.
    CALL FUNCTION 'GUI_CREATE_DIRECTORY'
      EXPORTING
        dirname = lv_folder
      EXCEPTIONS
        FAILED = 1
        OTHERS  = 2.
    IF sy-subrc <> 0.
      me->write_telemetry( iv_message = |SAVE_FILE creates existing folder { lv_folder }| ).
    ENDIF.
    lv_code_name = zcl_utility_abaptogit_ado=>build_code_name(
        EXPORTING
            iv_commit_object = iv_commit_object
            iv_local_folder = abap_true
            iv_base_folder = ev_codefolder
            iv_folder_structure = iv_folder_structure
        IMPORTING
            ev_file_folder = lv_filefolder
             ).
    IF NOT line_exists( cht_filefolders[ table_line = lv_filefolder ] ).
      APPEND lv_filefolder TO cht_filefolders.
      lv_folder = lv_filefolder.
      CALL FUNCTION 'GUI_CREATE_DIRECTORY'
        EXPORTING
          dirname = lv_folder
        EXCEPTIONS
          OTHERS  = 1. "#EC FB_RC
      IF sy-subrc <> 0.
          me->write_telemetry( iv_message = |SAVE_FILE creates existing folder { lv_folder }| ).
      ENDIF.
    ENDIF.
    lv_path = |{ iv_basefolder }{ iv_devclass }{ c_delim }{ lv_code_name }|.
    CALL FUNCTION 'GUI_DOWNLOAD'
      EXPORTING
        filename              = lv_path
        filetype              = 'ASC'
        write_field_separator = 'X'
      TABLES
        data_tab              = cht_filecontent
      EXCEPTIONS
        OTHERS                = 1.
    IF sy-subrc <> 0.
      me->write_telemetry( iv_message = |SAVE_FILE fails to save local file { lv_path }| ).
      rv_success = abap_false.
    ENDIF.
  ENDMETHOD.


  METHOD setup_ado.
    CREATE OBJECT me->oref_ado
      EXPORTING
        iv_username      = iv_username
        iv_pat           = iv_pat
        iv_orgid         = iv_orgid
        iv_repoid        = iv_repoid
        iv_project       = iv_project
        io_objtelemetry  = me->oref_telemetry
        iv_methtelemetry = me->method_name_telemetry.
  ENDMETHOD.


  METHOD spotsync_tr.

    DATA lt_commit_objects TYPE zcl_utility_abaptogit_tr=>tty_commit_object.
    DATA lv_owner TYPE string.
    DATA lv_comment TYPE string.
    DATA lv_basebranch TYPE string.
    DATA lv_synccnt TYPE i.

    me->write_telemetry( iv_message = |start spot sync: { sy-uzeit }| iv_kind = 'info' ).

    rv_success = me->oref_tr->get_tr_commit_objects(
        EXPORTING
            iv_trid = iv_trid
            iv_packagenames = iv_packagenames
        IMPORTING
            ev_owner = lv_owner
            ev_comment = lv_comment
        CHANGING
            it_commit_objects = lt_commit_objects
             ).
    CHECK rv_success = abap_true.

    me->write_telemetry( iv_message = |get TR { iv_trid } objects: { sy-uzeit }| iv_kind = 'info' ).

    me->prepare_landscape_branch( EXPORTING iv_prefix = iv_prefix IMPORTING ev_branch = lv_basebranch ).
    rv_success = me->oref_ado->push_tr_commit_objects(
        EXPORTING
            iv_trid = iv_trid
            iv_user = iv_user
            iv_domain = iv_domain
            iv_branch = lv_basebranch
            iv_comment = lv_comment
            it_commit_objects = lt_commit_objects
        IMPORTING
            ev_synccnt = lv_synccnt
             ).
    CHECK rv_success = abap_true.

    IF lv_synccnt = 0.
      me->write_telemetry( iv_message = |TR { iv_trid } has no objects matching criteria to sync to system branch { lv_basebranch }: { sy-uzeit }| iv_kind = 'info' ).
    ELSE.
      me->write_telemetry( iv_message = |push TR { iv_trid } objects ({ lv_synccnt }) to system branch { lv_basebranch }: { sy-uzeit }| iv_kind = 'info' ).
    ENDIF.

  ENDMETHOD.


  METHOD write_telemetry.
    IF me->oref_telemetry IS NOT INITIAL AND me->method_name_telemetry IS NOT INITIAL.
      DATA(oref) = me->oref_telemetry.
      DATA(meth) = me->method_name_telemetry.
      CALL METHOD oref->(meth)
        EXPORTING
          iv_message = iv_message
          iv_kind    = iv_kind.
    ELSE.
      WRITE / |{ iv_kind }: { iv_message }|.
    ENDIF.
  ENDMETHOD.
ENDCLASS.