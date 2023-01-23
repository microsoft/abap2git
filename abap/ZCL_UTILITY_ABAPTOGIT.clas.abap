" Utility class to sync from ABAP objects in a transport request (TR)
" to Git repo with specific package(s) and branch by Azure DevOps (ADO) REST API
" Multiple packages will share the same Git repo
" Multiple SAP systems in the landscape of a service line will share the same Git repo
" in respective branches
" Following ABAP objects are included in sync-ing:
" Class, Function Module, Program, Include, Test Class, Interface,
" Enhancement Object (hook implementation, class), Data Table,
" HR/payroll schema/PCR, configuration changes.
" Following ABAP objects are not yet included in sync-ing:
" Data dictionary objects (others), SAPScript, enhancement objects (others) and other objects.
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
" Active version mode should be used for exploring what are inside a given system (for
" static scan/analysis).
" ADO PAT requires privilege to have code read/write or full, identity read (to specify real author
" upon pushing ADO commit)
CLASS ZCL_UTILITY_ABAPTOGIT DEFINITION
  PUBLIC
  FINAL
  CREATE PUBLIC .

PUBLIC SECTION.

    " list of TR IDs
    TYPES: tty_trids TYPE TABLE OF string.

    " constructor
    " io_objtelemetry - class object for telemetry
    " iv_methtelemetry - method name for telemetry
    " for telemetry, the method will be invoked with parameters iv_message as string (for message content) and iv_kind as string (for category)
    METHODS constructor
        IMPORTING
            io_objtelemetry     TYPE REF TO object OPTIONAL
            iv_methtelemetry    TYPE string OPTIONAL.

    " fetch HR/payroll schema/PCR language code lines into files
    " iv_folder - local folder name to save the files
    " iv_folder_structure - 'flat' or 'eclipse'
    METHODS get_hr_schemapcrs
        IMPORTING
            iv_folder   TYPE string
            iv_folder_structure TYPE string DEFAULT 'eclipse'
        RETURNING VALUE(rv_success) TYPE string.

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
        RETURNING VALUE(rv_success) TYPE string.

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
        RETURNING VALUE(rv_success) TYPE string.

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
        RETURNING VALUE(rv_success) TYPE string.

    " setup configs for ADO operations
    " iv_username - VSO user name as email address
    " iv_pat - VSO personal access token, could be generated from VSO portal per user with code change permission
    " iv_orgid - organization ID, like the name "org" in <org>.visualstudio.com
    " iv_repoid - Git repo ID
    " iv_project - project name in your VSO portal
    METHODS setup_ado
        IMPORTING
            iv_username         TYPE string
            iv_pat              TYPE string
            iv_orgid            TYPE string
            iv_repoid           TYPE string
            iv_project          TYPE string.

    " spot sync ABAP objects in a TR
    " iv_trid - TR ID
    " iv_user - TR owner
    " iv_domain - email domain for TR owner
    " iv_packagenames - package names to include in commit, separated by comma
    " iv_prefix - branch prefix
    METHODS spotsync_tr
        IMPORTING
            iv_trid         TYPE string
            iv_user         TYPE string DEFAULT ''
            iv_domain       TYPE string DEFAULT ''
            iv_packagenames TYPE string
            iv_prefix       TYPE string DEFAULT 'users/system/'
        RETURNING VALUE(rv_success) TYPE string.

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
    " et_trids - TR IDs inspected regardless Git sync-ed or not
    " et_synctrids - TR IDs Git sync-ed
    METHODS catchup_trs
        IMPORTING
            iv_branch           TYPE string
            iv_packagenames     TYPE string
            iv_domain           TYPE string DEFAULT ''
            iv_rootfolder       TYPE string DEFAULT '/SRC/'
            iv_folder_structure TYPE string DEFAULT 'eclipse'
        EXPORTING
            et_trids            TYPE tty_trids
            et_synctrids        TYPE tty_trids
        RETURNING VALUE(rv_success) TYPE string.

    " get heatmap of code change stats in given date range
    " iv_packagenames - package names to include in commit, separated by comma
    " iv_fromdat - date to include from
    " iv_todat - date to include to
    METHODS heatmap_trs
        IMPORTING
            iv_packagenames TYPE string
            iv_fromdat      TYPE d
            iv_todat        TYPE d
            iv_resultfile   TYPE string
        RETURNING VALUE(rv_success) TYPE string.

PROTECTED SECTION.

PRIVATE SECTION.

    CONSTANTS c_delim TYPE string VALUE '\\'.

    " max difference in seconds for an imported TR to date time of now
    CONSTANTS c_importtr_diff_gap TYPE i VALUE 30.

    " code object used in downloading ABAP code object to local disk
    TYPES: BEGIN OF ts_code_object,
            obj_name    TYPE sobj_name,
            obj_type    TYPE trobjtype,
            obj_type2   TYPE trobjtype,
            fugr_name   TYPE sobj_name,
           END OF ts_code_object.

    " helper objects for ADO and TR operations
    DATA oref_ado TYPE REF TO ZCL_UTILITY_ABAPTOGIT_ADO.
    DATA oref_tr TYPE REF TO ZCL_UTILITY_ABAPTOGIT_TR.

    " telemetry callback
    DATA oref_telemetry TYPE REF TO object.
    DATA method_name_telemetry TYPE string.

    " wrapper to write telemetry with the callback registered
    METHODS write_telemetry
        IMPORTING
            iv_message  TYPE string
            iv_kind     TYPE string DEFAULT 'error'.

ENDCLASS.



CLASS ZCL_UTILITY_ABAPTOGIT IMPLEMENTATION.

  METHOD CONSTRUCTOR.

    CREATE OBJECT me->oref_tr
        EXPORTING
            io_objtelemetry = io_objtelemetry
            iv_methtelemetry = iv_methtelemetry.

    IF io_objtelemetry IS SUPPLIED.
        me->oref_telemetry = io_objtelemetry.
    ENDIF.

    IF iv_methtelemetry IS SUPPLIED.
        me->method_name_telemetry = iv_methtelemetry.
    ENDIF.

  ENDMETHOD.

  METHOD SETUP_ADO.
    CREATE OBJECT me->oref_ado
        EXPORTING
            iv_username = iv_username
            iv_pat = iv_pat
            iv_orgid = iv_orgid
            iv_repoid = iv_repoid
            iv_project = iv_project
            io_objtelemetry = me->oref_telemetry
            iv_methtelemetry = me->method_name_telemetry.
  ENDMETHOD.

  METHOD SPOTSYNC_TR.

    DATA lt_commit_objects TYPE ZCL_UTILITY_ABAPTOGIT_TR=>tty_commit_object.
    DATA lv_comment TYPE string.
    DATA lv_basebranch TYPE string.
    DATA lv_synccnt TYPE i.

    me->write_telemetry( iv_message = |start spot sync: { sy-uzeit }| iv_kind = 'info' ).

    rv_success = me->oref_tr->get_tr_commit_objects(
        EXPORTING
            iv_trid = iv_trid
            iv_packagenames = iv_packagenames
        IMPORTING
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

  METHOD CATCHUP_TRS.

    DATA lv_content TYPE string.
    DATA lv_sync_status TYPE ZCL_UTILITY_ABAPTOGIT_ADO=>ts_sync_status.
    DATA lt_trids TYPE ZCL_UTILITY_ABAPTOGIT_ADO=>tty_trids.
    DATA lt_commit_objects TYPE ZCL_UTILITY_ABAPTOGIT_TR=>tty_commit_object.
    DATA lv_comment TYPE string.
    DATA lv_rootfolder TYPE string.
    DATA lv_synccnt TYPE i.
    DATA lv_trtimestamp TYPE timestamp.
    DATA lv_nowtimestamp TYPE timestamp.
    DATA lv_diff TYPE tzntstmpl.

    lv_rootfolder = iv_rootfolder.
    TRANSLATE lv_rootfolder TO UPPER CASE.
    DATA(lv_itempath) = |{ lv_rootfolder }{ ZCL_UTILITY_ABAPTOGIT_ADO=>c_sync_status_file }|.

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
     rv_success = ZCL_UTILITY_ABAPTOGIT_ADO=>load_sync_status(
        EXPORTING
            iv_filecontent = lv_content
        IMPORTING
            ev_sync_status = lv_sync_status
             ).
    CHECK rv_success = abap_true.

    " sync-ed in active mode can't be caught up since it's sync-ed to active/latest version of objects instead of TR
    IF lv_sync_status-mode = ZCL_UTILITY_ABAPTOGIT_TR=>c_active_version.
        me->write_telemetry( iv_message = |branch { iv_branch } is sync-ed in active mode| ).
        rv_success = abap_false.
        RETURN.
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

        CLEAR lt_commit_objects.
        rv_success = me->oref_tr->get_tr_commit_objects(
            EXPORTING
                iv_trid = watrid-trid
                iv_packagenames = iv_packagenames
            IMPORTING
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

  METHOD HEATMAP_TRS.

    DATA lt_trids TYPE ZCL_UTILITY_ABAPTOGIT_ADO=>tty_trids.
    DATA lt_commit_objects TYPE ZCL_UTILITY_ABAPTOGIT_TR=>tty_commit_object.
    DATA lt_stats TYPE TABLE OF string INITIAL SIZE 0.
    DATA lv_insertions TYPE i.
    DATA lv_deletions TYPE i.
    DATA lv_tables TYPE i.
    DATA lv_rows TYPE i.

    rv_success = abap_true.

    ZCL_UTILITY_ABAPTOGIT_ADO=>get_trs_daterange(
        EXPORTING
            iv_fromdat = iv_fromdat
            iv_todat = iv_todat
        IMPORTING
            et_trids = lt_trids
             ).

    APPEND |System,TR,User,Date,Time,Insertions,Deletions,Tables,Rows| TO lt_stats.

    LOOP AT lt_trids INTO DATA(wa_trid).
        CLEAR: lt_commit_objects, lv_insertions, lv_deletions, lv_tables, lv_rows.
        me->oref_tr->get_tr_commit_objects(
            EXPORTING
                iv_trid = wa_trid-trid
                iv_packagenames = iv_packagenames
                iv_deltastats = abap_true
            CHANGING
                it_commit_objects = lt_commit_objects
             ).
        LOOP AT lt_commit_objects INTO DATA(wa_co).
            lv_insertions = lv_insertions + wa_co-insertions.
            lv_deletions = lv_deletions + wa_co-deletions.
            lv_tables = lv_tables + wa_co-tables.
            IF wa_co-rows > 0.
                lv_rows = lv_rows + wa_co-rows - 1.
            ENDIF.
        ENDLOOP.
        DATA(lv_date) = |{ wa_trid-dat+4(2) }/{ wa_trid-dat+6(2) }/{ wa_trid-dat(4) }|.
        DATA(lv_time) = |{ wa_trid-tim(2) }:{ wa_trid-tim+2(2) }:{ wa_trid-tim+4(2) }|.
        APPEND |{ sy-sysid },{ wa_trid-user },{ wa_trid-trid },{ lv_date },{ lv_time },{ lv_insertions },{ lv_deletions },{ lv_tables },{ lv_rows }| TO lt_stats.
    ENDLOOP.

    CALL FUNCTION 'GUI_DOWNLOAD'
        EXPORTING
            filename = iv_resultfile
            filetype = 'ASC'
            write_field_separator = 'X'
        TABLES
            data_tab = lt_stats
        EXCEPTIONS
            OTHERS = 1.
    IF sy-subrc <> 0.
        me->write_telemetry( iv_message = |HEATMAP_TRS fails to save local file { iv_resultfile }| ).
        rv_success = abap_false.
    ENDIF.

  ENDMETHOD.

  METHOD PREPARE_LANDSCAPE_BRANCH.
    " main branch name like users/system/<system id>
    ev_branch = |{ iv_prefix }{ sy-sysid }|.
  ENDMETHOD.

  METHOD GET_HR_SCHEMAPCRS.
    DATA wacode TYPE ts_code_object.
    DATA lt_schemas TYPE STANDARD TABLE OF ts_code_object.
    DATA lt_pcrs TYPE STANDARD TABLE OF ts_code_object.
    DATA lt_filecontent TYPE ZCL_UTILITY_ABAPTOGIT_TR=>tty_abaptext.
    DATA lv_basefolder TYPE string.
    DATA lv_commit_object TYPE ZCL_UTILITY_ABAPTOGIT_TR=>ts_commit_object.
    DATA lv_code_name TYPE string.
    DATA lv_path TYPE string.
    DATA lv_folder TYPE RLGRAP-FILENAME.
    DATA lv_codefolder TYPE string.
    DATA lv_filefolder TYPE string.
    DATA lt_filefolders TYPE TABLE OF string WITH DEFAULT KEY.
    DATA lv_success TYPE string.
    DATA lv_schpcrname TYPE string.
    DATA lv_progcls TYPE t52ba-pwert.

    IF iv_folder NP '*\'.
        lv_basefolder = iv_folder && '\'.
    ELSE.
        lv_basefolder = iv_folder.
    ENDIF.

    " root folder for a package like \src\.schemapcr\
    lv_folder = |{ lv_basefolder }{ ZCL_UTILITY_ABAPTOGIT_TR=>c_schemapcr }{ c_delim }|.
    lv_codefolder = lv_folder.
    CALL FUNCTION 'GUI_CREATE_DIRECTORY'
        EXPORTING
            dirname = lv_folder
        EXCEPTIONS
            FAILED = 1.
    IF sy-subrc <> 0.
        me->write_telemetry( iv_message = |GET_HR_SCHEMAPCRS fails to create folder { lv_folder }| ).
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
        SELECT SINGLE pwert FROM t52ba INTO @lv_progcls WHERE potyp = 'SCHE' AND ponam = @lv_schpcrname AND pattr = 'PCL'.  "#EC WARNOK

        lv_commit_object-objname = wacode-obj_name.
        lv_commit_object-objtype = 'PSCC'.
        lv_commit_object-progcls = lv_progcls.
        lv_code_name = ZCL_UTILITY_ABAPTOGIT_ADO=>build_code_name(
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
                    OTHERS = 1. "#EC FB_RC
        ENDIF.
        lv_path = |{ lv_basefolder }{ ZCL_UTILITY_ABAPTOGIT_TR=>c_schemapcr }{ c_delim }{ lv_code_name }|.
        CALL FUNCTION 'GUI_DOWNLOAD'
            EXPORTING
                filename = lv_path
                filetype = 'ASC'
                write_field_separator = 'X'
            TABLES
                data_tab = lt_filecontent
            EXCEPTIONS
                OTHERS = 1.
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
        SELECT SINGLE pwert FROM t52ba INTO @lv_progcls WHERE potyp = 'CYCL' AND ponam = @lv_schpcrname AND pattr = 'PCL'.  "#EC WARNOK

        lv_commit_object-objname = wacode-obj_name.
        lv_commit_object-objtype = 'PCYC'.
        lv_commit_object-progcls = lv_progcls.
        lv_code_name = ZCL_UTILITY_ABAPTOGIT_ADO=>build_code_name(
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
                    OTHERS = 1. "#EC FB_RC
        ENDIF.
        lv_path = |{ lv_basefolder }{ ZCL_UTILITY_ABAPTOGIT_TR=>c_schemapcr }{ c_delim }{ lv_code_name }|.
        CALL FUNCTION 'GUI_DOWNLOAD'
            EXPORTING
                filename = lv_path
                filetype = 'ASC'
                write_field_separator = 'X'
            TABLES
                data_tab = lt_filecontent
            EXCEPTIONS
                OTHERS = 1.
        IF sy-subrc <> 0.
            me->write_telemetry( iv_message = |GET_HR_SCHEMAPCRS fails to save local file { lv_path }| ).
            rv_success = abap_false.
        ENDIF.

    ENDLOOP.

  ENDMETHOD.

  METHOD GET_PACKAGE_CODES.

    DATA wacode TYPE ts_code_object.
    DATA lt_codes TYPE STANDARD TABLE OF ts_code_object.
    DATA lt_fmcodes TYPE STANDARD TABLE OF ts_code_object.
    DATA lt_fgfmcodes TYPE STANDARD TABLE OF ts_code_object.
    DATA lt_objversions TYPE ZCL_UTILITY_ABAPTOGIT_TR=>tty_version_no.
    DATA lv_objname TYPE versobjnam.
    DATA lv_objtype TYPE versobjtyp.
    DATA lv_objtype2 TYPE versobjtyp.
    DATA lv_fugrname TYPE versobjnam.
    DATA lv_basefolder TYPE string.
    DATA lt_filecontent TYPE ZCL_UTILITY_ABAPTOGIT_TR=>tty_abaptext.
    DATA lv_filecontent TYPE string.
    DATA lt_tclsfilecontent TYPE ZCL_UTILITY_ABAPTOGIT_TR=>tty_abaptext.
    DATA lv_tclsname TYPE string.
    DATA lv_tclstype TYPE string.
    DATA lv_commit_object TYPE ZCL_UTILITY_ABAPTOGIT_TR=>ts_commit_object.
    DATA lv_code_name TYPE string.
    DATA lv_path TYPE string.
    DATA lv_folder TYPE RLGRAP-FILENAME.
    DATA lv_codefolder TYPE string.
    DATA lv_success TYPE string.
    DATA lv_objname3 TYPE e071-obj_name.
    DATA lv_version_no TYPE i.
    DATA lv_dat TYPE d.
    DATA lv_tim TYPE t.
    DATA lv_subc TYPE reposrc-subc.
    DATA lv_filefolder TYPE string.
    DATA lt_filefolders TYPE TABLE OF string WITH DEFAULT KEY.

    IF iv_uptotrid IS SUPPLIED AND iv_mode <> ZCL_UTILITY_ABAPTOGIT_TR=>c_latest_version.
        me->write_telemetry( iv_message = |latest version required for up-to TR ID supplied| ).
        rv_success = abap_false.
        RETURN.
    ENDIF.

    IF iv_folder NP '*\'.
        lv_basefolder = iv_folder && '\'.
    ELSE.
        lv_basefolder = iv_folder.
    ENDIF.

    " root folder for a package like \src\<branch name>\
    lv_folder = |{ lv_basefolder }{ iv_package }{ c_delim }|.
    lv_codefolder = lv_folder.
    CALL FUNCTION 'GUI_CREATE_DIRECTORY'
        EXPORTING
            dirname = lv_folder
        EXCEPTIONS
            FAILED = 1.
    IF sy-subrc <> 0.
        me->write_telemetry( iv_message = |GET_PACKAGE_CODES fails to create folder { lv_folder }| ).
    ENDIF.

    SELECT obj_name, object, object, ' ' INTO TABLE @lt_codes FROM tadir WHERE devclass = @iv_package AND pgmid = 'R3TR'.   "#EC CI_SGLSELECT

    " collect FMs/includes in function groups into list of code objects to download
    LOOP AT lt_codes INTO wacode.
        lv_objtype = wacode-obj_type.
        IF lv_objtype = 'FUGR'.
            lv_objname = wacode-obj_name.
            " find function modules in the function group
            DATA(pname_filter) = 'SAPL' && lv_objname.
            CLEAR lt_fgfmcodes.
            SELECT funcname, 'FUNC', 'FUNC', @lv_objname INTO TABLE @lt_fgfmcodes FROM tfdir WHERE pname = @pname_filter.   "#EC CI_SGLSELECT
            APPEND LINES OF lt_fgfmcodes TO lt_fmcodes.
            " find includes in the function group
            pname_filter = 'L' && lv_objname && '%'.
            CLEAR lt_fgfmcodes.
            SELECT name, 'REPS', 'FUNC', @lv_objname INTO TABLE @lt_fgfmcodes FROM trdir WHERE name LIKE @pname_filter. "#EC CI_SGLSELECT
            APPEND LINES OF lt_fgfmcodes TO lt_fmcodes.
        ENDIF.
    ENDLOOP.
    APPEND LINES OF lt_fmcodes TO lt_codes.

    IF iv_uptotrid IS SUPPLIED.
        SELECT SINGLE as4date INTO lv_dat FROM e070 WHERE trkorr = iv_uptotrid AND trfunction = 'K' AND trstatus = 'R'.
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

        " function group was processed to extract function modules
        CHECK lv_objtype <> 'FUGR'.

        " skip object like '...$01' in function group
        CHECK lv_objtype2 <> 'FUNC' OR lv_objname NS '$'.

        " fetch object version
        CLEAR lt_objversions.
        lv_objname3 = lv_objname.

        IF iv_uptotrid IS SUPPLIED.
            " use date/time of up-to TR ID to constrain version to select if latest version
            lv_success = me->oref_tr->get_versions_no(
                EXPORTING
                    iv_objname = lv_objname3
                    iv_objtype = lv_objtype
                    iv_mode = iv_mode
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
        SELECT SINGLE subc FROM reposrc INTO @lv_subc WHERE progname = @lv_objname3.    "#EC WARNOK

        " save the object content to local disk file
        lv_commit_object-objname = lv_objname3.
        lv_commit_object-objtype = lv_objtype.
        lv_commit_object-objtype2 = lv_objtype2.
        lv_commit_object-fugr = lv_fugrname.
        lv_commit_object-subc = lv_subc.
        lv_code_name = ZCL_UTILITY_ABAPTOGIT_ADO=>build_code_name(
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
                    OTHERS = 1. "#EC FB_RC
        ENDIF.
        lv_path = |{ lv_basefolder }{ iv_package }{ c_delim }{ lv_code_name }|.
        CALL FUNCTION 'GUI_DOWNLOAD'
            EXPORTING
                filename = lv_path
                filetype = 'ASC'
                write_field_separator = 'X'
            TABLES
                data_tab = lt_filecontent
            EXCEPTIONS
                OTHERS = 1.
        IF sy-subrc <> 0.
            me->write_telemetry( iv_message = |GET_PACKAGE_CODES fails to save local file { lv_path }| ).
            rv_success = abap_false.
        ENDIF.

        " save the object content to local disk file for test class if any since it must be attached to a product class
        IF lines( lt_tclsfilecontent ) > 0.
            " following abapGit where class name is used for test class name instead of ====CCAU like one
            lv_commit_object-objname = lv_objname3.
            lv_commit_object-objtype = lv_tclstype.
            lv_code_name = ZCL_UTILITY_ABAPTOGIT_ADO=>build_code_name(
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
                        OTHERS = 1. "#EC FB_RC
            ENDIF.
            lv_path = |{ lv_basefolder }{ iv_package }{ c_delim }{ lv_code_name }|.
            CALL FUNCTION 'GUI_DOWNLOAD'
                EXPORTING
                    filename = lv_path
                    filetype = 'ASC'
                    write_field_separator = 'X'
                TABLES
                    data_tab = lt_tclsfilecontent
                EXCEPTIONS
                    OTHERS = 1.
            IF sy-subrc <> 0.
                me->write_telemetry( iv_message = |GET_PACKAGE_CODES fails to save local file { lv_path }| ).
                rv_success = abap_false.
            ENDIF.
        ENDIF.

    ENDLOOP.

  ENDMETHOD.

  METHOD GET_PACKAGES_CODES.

    DATA lv_basefolder TYPE string.
    DATA lv_package TYPE devclass.

    IF iv_uptotrid IS SUPPLIED AND iv_mode <> ZCL_UTILITY_ABAPTOGIT_TR=>c_latest_version.
        me->write_telemetry( iv_message = |latest version required for up-to TR ID supplied| ).
        rv_success = abap_false.
        RETURN.
    ENDIF.

    IF iv_folder NP '*\'.
        lv_basefolder = iv_folder && '\'.
    ELSE.
        lv_basefolder = iv_folder.
    ENDIF.

    DATA lv_success TYPE string.
    DATA lt_packages TYPE TABLE OF string.
    SPLIT iv_packages AT ',' INTO TABLE lt_packages.

    " save objects in each package specified under the folder named as package name like \src\<package name>\
    LOOP AT lt_packages INTO DATA(wa).
        lv_package = wa.
        IF iv_uptotrid IS SUPPLIED.
            lv_success = me->GET_PACKAGE_CODES(
                EXPORTING
                    iv_package = lv_package
                    iv_folder = lv_basefolder
                    iv_mode = iv_mode
                    iv_uptotrid = iv_uptotrid
                    iv_folder_structure = iv_folder_structure
                     ).
        ELSE.
            lv_success = me->GET_PACKAGE_CODES(
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
    DATA(lv_file) = |{ lv_basefolder }{ ZCL_UTILITY_ABAPTOGIT_ADO=>c_sync_status_file }|.
    IF iv_uptotrid IS SUPPLIED.
        rv_success = ZCL_UTILITY_ABAPTOGIT_ADO=>save_sync_status(
            EXPORTING
                iv_mode = iv_mode
                iv_file = lv_file
                iv_trid = iv_uptotrid
                 ).
    ELSE.
        rv_success = ZCL_UTILITY_ABAPTOGIT_ADO=>save_sync_status(
            EXPORTING
                iv_mode = iv_mode
                iv_file = lv_file
                 ).
    ENDIF.

  ENDMETHOD.

  METHOD GET_CUSTOMIZE_PACKAGES_CODES.

    DATA lv_basefolder TYPE string.
    DATA lv_package TYPE devclass.

    IF iv_uptotrid IS SUPPLIED AND iv_mode <> ZCL_UTILITY_ABAPTOGIT_TR=>c_latest_version.
        me->write_telemetry( iv_message = |latest version required for up-to TR ID supplied| ).
        rv_success = abap_false.
        RETURN.
    ENDIF.

    IF iv_folder NP '*\'.
        lv_basefolder = iv_folder && '\'.
    ELSE.
        lv_basefolder = iv_folder.
    ENDIF.

    DATA lv_success TYPE string.
    DATA lt_packages TYPE STANDARD TABLE OF devclass.

    " fetch customization packages with initial Y or Z
    SELECT devclass INTO TABLE lt_packages FROM tdevc WHERE devclass LIKE 'Z%' OR devclass LIKE 'Y%'.   "#EC CI_SGLSELECT

    " save objects in each package collected under the folder named as package name like \src\<package name>\
    LOOP AT lt_packages INTO DATA(wa).
        lv_package = wa.
        IF iv_uptotrid IS SUPPLIED.
            lv_success = me->GET_PACKAGE_CODES(
                EXPORTING
                    iv_package = lv_package
                    iv_folder = lv_basefolder
                    iv_mode = iv_mode
                    iv_uptotrid = iv_uptotrid
                     ).
        ELSE.
            lv_success = me->GET_PACKAGE_CODES(
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
    DATA(lv_file) = |{ lv_basefolder }{ ZCL_UTILITY_ABAPTOGIT_ADO=>c_sync_status_file }|.
    IF iv_uptotrid IS SUPPLIED.
        rv_success = ZCL_UTILITY_ABAPTOGIT_ADO=>save_sync_status(
            EXPORTING
                iv_mode = iv_mode
                iv_file = lv_file
                iv_trid = iv_uptotrid
                 ).
    ELSE.
        rv_success = ZCL_UTILITY_ABAPTOGIT_ADO=>save_sync_status(
            EXPORTING
                iv_mode = iv_mode
                iv_file = lv_file
                 ).
    ENDIF.

  ENDMETHOD.

  METHOD WRITE_TELEMETRY.
    IF me->oref_telemetry IS NOT INITIAL AND me->method_name_telemetry IS NOT INITIAL.
        DATA(oref) = me->oref_telemetry.
        DATA(meth) = me->method_name_telemetry.
        CALL METHOD oref->(meth)
            EXPORTING
                iv_message = iv_message
                iv_kind = iv_kind.
    ELSE.
        WRITE / |{ iv_kind }: { iv_message }|.
    ENDIF.
  ENDMETHOD.

ENDCLASS.