" Helper class to talk to ADO REST API for Git operations
" minor change
CLASS ZCL_UTILITY_ABAPTOGIT_ADO DEFINITION
  PUBLIC
  FINAL
  CREATE PUBLIC .


PUBLIC SECTION.

    " sync status file name to keep track on last sync-ed TR
    CONSTANTS:  c_sync_status_file   TYPE string VALUE '.sync_status.json'.

    CONSTANTS c_delim TYPE string VALUE '\'.
    CONSTANTS c_delimgit TYPE string VALUE '/'.

    " list of TR IDs to sync to Git
    TYPES: BEGIN OF ty_trid,
            trid TYPE string,
            user TYPE string,
            dat  TYPE d,
            tim  TYPE t,
            func TYPE string,
            titl TYPE string,
           END OF ty_trid.
    TYPES: tty_trids TYPE TABLE OF ty_trid.

    " payload of JSON file in Git repo root folder to mark down sync status
    TYPES: BEGIN OF ts_sync_status,
            trid        TYPE string,
            mode        TYPE string,
            updatedate  TYPE d,
            updatetime  TYPE t,
           END OF ts_sync_status.

    " constructor
    " iv_username - VSO user name as email address
    " iv_pat - VSO personal access token, could be generated from VSO portal per user with code change permission
    " iv_orgid - organization ID, like the name "org" in <org>.visualstudio.com
    " iv_repoid - Git repo ID
    " iv_project - project name in your VSO portal
    " io_objtelemetry - class object for telemetry
    " iv_methtelemetry - method name for telemetry
    " for telemetry, the method will be invoked with parameters iv_message as string (for message content) and iv_kind as string (for category)
    METHODS constructor
        IMPORTING
            iv_username         TYPE string
            iv_pat              TYPE string
            iv_orgid            TYPE string
            iv_repoid           TYPE string
            iv_project          TYPE string
            io_objtelemetry     TYPE REF TO object OPTIONAL
            iv_methtelemetry    TYPE string OPTIONAL.

    " push the ABAP objects to Git for a TR
    " iv_trid - TR ID to push to Git
    " iv_user - TR owner, blank means not to specify real author
    " iv_domain - TR owner email domain, blank means not to specify real author
    " iv_branch - branch name to push the changes to
    " iv_comment - commit comment retrieved from get_tr_commit_objects
    " iv_rootfolder - the root folder in Git local clone for ABAP objects to add to, shared by all packages in SAP
    " iv_folder_structure - 'flat' or 'eclipse'
    " iv_push_status - push status file or not
    " it_commit_objects - table of ABAP objects to commit to Git, retrieved from get_tr_commit_objects
    " ev_synccnt - count of objects to sync
    METHODS push_tr_commit_objects
        IMPORTING
            iv_trid             TYPE string
            iv_user             TYPE string DEFAULT ''
            iv_domain           TYPE string DEFAULT ''
            iv_branch           TYPE string
            iv_comment          TYPE string
            iv_rootfolder       TYPE string DEFAULT '/SRC/'
            iv_folder_structure TYPE string DEFAULT 'eclipse'
            iv_push_status      TYPE ABAP_BOOL DEFAULT 'X'
            it_commit_objects   TYPE ZCL_UTILITY_ABAPTOGIT_TR=>tty_commit_object
        EXPORTING
            ev_synccnt          TYPE i
        RETURNING VALUE(rv_success) TYPE abap_bool.

    " get IDs of TRs after a given TR, or ID of latest TR if not given
    " iv_fromtrid - TR ID to start sync-ing for its next TR, or indicate to get latest TR ID if not provided
    " et_trids - list of TR IDs retrieved
    CLASS-METHODS get_trs
        IMPORTING
            iv_fromtrid TYPE string OPTIONAL
        EXPORTING
            et_trids    TYPE tty_trids.

    " get IDs of TRs between a date range
    " iv_fromdat - date to include from
    " iv_todat - date to include to
    " et_trids - list of TR IDs retrieved
    CLASS-METHODS get_trs_daterange
        IMPORTING
            iv_fromdat  TYPE d
            iv_todat    TYPE d
        EXPORTING
            et_trids    TYPE tty_trids.

    " get IDs of workbench TRs since a date/time
    " iv_fromdat - date to include from
    " iv_fromtim - time to include from
    " et_trids - list of TR IDs retrieved
    CLASS-METHODS get_wbtrs
        IMPORTING
            iv_fromdat  TYPE d
            iv_fromtim  TYPE t
        EXPORTING
            et_trids    TYPE tty_trids.

    " construct source code file name in Git repo from ABAP code object name
    " iv_commit_object - ABAP object to commit
    " iv_local_folder - the file name is for local folder or not, if so folder structure may be built, otherwise just path name
    " iv_base_folder - the base folder including package name for a file to save
    " iv_folder_structure - 'flat' or 'eclipse'
    " ev_file_folder - folder of the file to create (to skip folder creation)
    " rv_name - file name to present the ABAP object in Git repo
    CLASS-METHODS build_code_name
        IMPORTING
            iv_commit_object    TYPE ZCL_UTILITY_ABAPTOGIT_TR=>ts_commit_object
            iv_local_folder     TYPE abap_bool
            iv_base_folder      TYPE string OPTIONAL
            iv_folder_structure TYPE string DEFAULT 'eclipse'
        EXPORTING
            ev_file_folder      TYPE string
        RETURNING VALUE(rv_name) TYPE string.

    " get sync status from the sync status file (by fetching item content from ADO REST API or local disk file)
    " iv_filecontent - file content of the sync status file
    " ev_sync_status - structure of the sync status
    CLASS-METHODS load_sync_status
        IMPORTING
            iv_filecontent  TYPE string
        EXPORTING
            ev_sync_status  TYPE ts_sync_status
        RETURNING VALUE(rv_success) TYPE abap_bool.

    " save constructed sync status file to local disk
    " iv_mode - active/latest version mode
    " iv_file - sync status file name
    " iv_trid - TR ID to specify in sync status
    CLASS-METHODS save_sync_status
        IMPORTING
            iv_mode TYPE string
            iv_file TYPE string
            iv_trid TYPE string OPTIONAL
        RETURNING VALUE(rv_success) TYPE abap_bool.

    " fetch item content by ADO REST API
    " iv_branch - branch name
    " iv_itempath - object path in Git repo
    " iv_read - read file content
    " ev_content - object content retrieved
    METHODS get_item_ado
        IMPORTING
            iv_branch   TYPE string
            iv_itempath TYPE string
            iv_read     TYPE abap_bool DEFAULT abap_true
        EXPORTING
            ev_content  TYPE string
        RETURNING VALUE(rv_success) TYPE abap_bool.

    " create a branch by ADO REST API
    " iv_basebranch - base branch name
    " iv_newbranch - new branch name
    METHODS create_branch
        IMPORTING
            iv_basebranch   TYPE string
            iv_newbranch    TYPE string
        RETURNING VALUE(rv_success) TYPE abap_bool.

    " find active PR for given source/target branch by ADO REST API
    " iv_srcbranch - source branch name
    " iv_tarbranch - target branch name
    " ev_prurl - active PR URL if any, otherwise empty string
    METHODS find_active_pullrequest
        IMPORTING
            iv_srcbranch    TYPE string
            iv_tarbranch    TYPE string
        EXPORTING
            ev_prurl        TYPE string
        RETURNING VALUE(rv_success) TYPE abap_bool.

    " find latest completed PR for given source/target branch by ADO REST API
    " iv_srcbranch - source branch name
    " iv_tarbranch - target branch name
    " ev_prurl - completed PR URL if any, otherwise empty string
    METHODS find_completed_pullrequest
        IMPORTING
            iv_srcbranch    TYPE string
            iv_tarbranch    TYPE string
        EXPORTING
            ev_prurl        TYPE string
        RETURNING VALUE(rv_success) TYPE abap_bool.

    " create a pull request by ADO REST API
    " iv_srcbranch - source branch name
    " iv_tarbranch - target branch name
    " iv_title - PR title
    " iv_description - PR description
    " iv_user - PR real owner user name
    " iv_domain - PR real owner domain
    " ev_prurl - URL of PR created
    METHODS create_pullrequest
        IMPORTING
            iv_srcbranch    TYPE string
            iv_tarbranch    TYPE string
            iv_title        TYPE string
            iv_description  TYPE string
            iv_user         TYPE string
            iv_domain       TYPE string
        EXPORTING
            ev_prurl        TYPE string
        RETURNING VALUE(rv_success) TYPE abap_bool.

    " generate a pull request creation URL to open from browser
    " iv_srcbranch - source branch name
    " iv_tarbranch - target branch name
    " ev_prurl - URL to create PR from browser
    METHODS get_pullrequestcreate_url
        IMPORTING
            iv_srcbranch    TYPE string
            iv_tarbranch    TYPE string
        EXPORTING
            ev_prurl        TYPE string
        RETURNING VALUE(rv_success) TYPE abap_bool.

PROTECTED SECTION.

PRIVATE SECTION.

    " payload for ADO REST API to create a push
    TYPES: BEGIN OF ts_item,
            path TYPE string,
           END OF ts_item.
    TYPES: BEGIN OF ts_newcontent,
            content     TYPE string,
            contentType TYPE i,
           END OF ts_newcontent.
    TYPES: BEGIN OF ts_change,
            changeType  TYPE i,
            item        TYPE ts_item,
            newContent  TYPE ts_newcontent,
           END OF ts_change.
    TYPES: tty_changes TYPE TABLE OF ts_change WITH KEY changeType.
    TYPES: BEGIN OF ts_user,
            email   TYPE string,
            name    TYPE string,
           END OF ts_user.
    TYPES: BEGIN OF ts_commit,
            changes TYPE tty_changes,
            comment TYPE string,
            author  TYPE ts_user,
           END OF ts_commit.
    TYPES: tty_commits TYPE TABLE OF ts_commit WITH KEY comment.
    TYPES: BEGIN OF ts_refupdate,
            name        TYPE string,
            oldObjectId TYPE string,
            newObjectId TYPE string,
           END OF ts_refupdate.
    TYPES: tty_refupdates TYPE TABLE OF ts_refupdate WITH KEY name.
    TYPES: BEGIN OF ts_push_json_req,
            commits     TYPE tty_commits,
            refUpdates  TYPE tty_refupdates,
           END OF ts_push_json_req.

    " payload for ADO REST API to create a PR
    TYPES: BEGIN OF ts_pr_merge_strategy,
            squash  TYPE string,
           END OF ts_pr_merge_strategy.
    TYPES: BEGIN OF ts_pr_completion_options,
            deleteSourceBranch  TYPE abap_bool,
            mergeStrategy       TYPE ts_pr_merge_strategy,
           END OF ts_pr_completion_options.
    TYPES: BEGIN OF ts_identityref,
            displayName TYPE string,
            id          TYPE string,
            uniqueName  TYPE string,
           END OF ts_identityref.
    TYPES: BEGIN OF ts_createpr_json_req,
            description         TYPE string,
            title               TYPE string,
            sourceRefName       TYPE string,
            targetRefName       TYPE string,
            createdBy           TYPE ts_identityref,
            completionOptions   TYPE ts_pr_completion_options,
           END OF ts_createpr_json_req.

    CONSTANTS: c_host                       TYPE string VALUE 'https://dev.azure.com/',
               c_hostid                     TYPE string VALUE 'https://vssps.dev.azure.com/',
               c_head                       TYPE string VALUE 'refs/heads/',
               c_folder_structure_eclipse   TYPE string VALUE 'eclipse',
               c_folder_structure_flat      TYPE string VALUE 'flat',
               c_empty_sha1                 TYPE string VALUE '0000000000000000000000000000000000000000'.

    CONSTANTS c_custtrfunc TYPE string VALUE 'W'.
    CONSTANTS c_wkbtrfunc TYPE string VALUE 'K'.
    CONSTANTS c_relests TYPE string VALUE 'R'.

    " credential for ADO REST APIs
    DATA username TYPE string.
    DATA pat TYPE string.

    " organization ID, like the name "org" in <org>.visualstudio.com
    DATA orgid TYPE string.
    " Git repo ID
    DATA repoid TYPE string.
    " project name like "OneITVSO"
    DATA project TYPE string.

    " telemetry callback
    DATA oref_telemetry TYPE REF TO object.
    DATA method_name_telemetry TYPE string.

    " construct sync status file content as a mark of which TR current branch sync to
    CLASS-METHODS build_sync_status
        IMPORTING
            iv_mode         TYPE string
            iv_trid         TYPE string OPTIONAL
        EXPORTING
            ev_filecontent  TYPE string.

    " fetch commit ID of a branch by ADO REST API
    METHODS get_commit_ado
        IMPORTING
            iv_branch   TYPE string
        EXPORTING
            ev_commitid TYPE string
        RETURNING VALUE(rv_success) TYPE abap_bool.

    " get repo name from ID by ADO REST API
    METHODS get_repo
        EXPORTING
            ev_projid   TYPE string
            ev_reponame TYPE string
        RETURNING VALUE(rv_success) TYPE abap_bool.

    " add a change to an ABAP object to ADO REST API body for push
    METHODS build_push_json
        IMPORTING
            iv_filename     TYPE string
            iv_filecontent  TYPE string
            iv_changetype   TYPE i
        CHANGING
            iv_commit       TYPE ts_commit.

    " fetch display name/id of a user for real author specified in commit
    METHODS get_displayname_ado
        IMPORTING
            iv_user     TYPE string
            iv_domain   TYPE string
        EXPORTING
            ev_id       TYPE string
            ev_name     TYPE string
        RETURNING VALUE(rv_success) TYPE abap_bool.

    " push changes of a TR to Git by ADO REST API
    METHODS push_ado
        IMPORTING
            iv_branch   TYPE string
            iv_commit   TYPE ts_commit
            iv_commitid TYPE string
        RETURNING VALUE(rv_success) TYPE abap_bool.

    " create HTTP client for ADO REST API
    METHODS create_http_client
        IMPORTING
            iv_url      TYPE string
            iv_username TYPE string
            iv_pat      TYPE string
            iv_accepts  TYPE string DEFAULT ''
        EXPORTING
            ei_http_client  TYPE REF TO if_http_client
            eo_rest_client  TYPE REF TO cl_rest_http_client
            ei_request      TYPE REF TO IF_REST_ENTITY.

    " make HTTP POST request for ADO REST API
    METHODS http_post
        IMPORTING
            io_rest_client  TYPE REF TO cl_rest_http_client
            ii_request      TYPE REF TO IF_REST_ENTITY
            iv_body         TYPE string
        EXPORTING
            ev_status       TYPE string
            ev_response     TYPE string.

    " make HTTP GET request for ADO REST API
    METHODS http_get
        IMPORTING
            io_rest_client  TYPE REF TO cl_rest_http_client
        EXPORTING
            ev_status       TYPE string
            ev_response     TYPE string.

    " wrapper to make HTTP POST request with object as POST body not yet serialized to JSON and response de-serialized
    METHODS http_post_json
        IMPORTING
            iv_path         TYPE string
            iv_username     TYPE string
            iv_pat          TYPE string
            iv_json         TYPE any
        EXPORTING
            ev_status       TYPE i
            et_entry_map    TYPE /ui5/cl_json_parser=>t_entry_map.

    " wrapper to make HTTP POST request with text POST body and response de-serialized
    METHODS http_post_json_text
        IMPORTING
            iv_host         TYPE string DEFAULT c_host
            iv_path         TYPE string
            iv_username     TYPE string
            iv_pat          TYPE string
            iv_accepts      TYPE string DEFAULT ''
            iv_json         TYPE string
        EXPORTING
            ev_status       TYPE i
            et_entry_map    TYPE /ui5/cl_json_parser=>t_entry_map.

    " wrapper to make HTTP POST request with response de-serialized
    METHODS http_get_json
        IMPORTING
            iv_host         TYPE string DEFAULT c_host
            iv_path         TYPE string
            iv_username     TYPE string
            iv_pat          TYPE string
            iv_log          TYPE abap_bool DEFAULT abap_true
        EXPORTING
            ev_status       TYPE i
            et_entry_map    TYPE /ui5/cl_json_parser=>t_entry_map.

    " wrapper to write telemetry with the callback registered
    METHODS write_telemetry
        IMPORTING
            iv_message  TYPE string
            iv_kind     TYPE string DEFAULT 'error'.

ENDCLASS.



CLASS ZCL_UTILITY_ABAPTOGIT_ADO IMPLEMENTATION.


  METHOD BUILD_CODE_NAME.

    DATA lv_folder TYPE RLGRAP-FILENAME.
    Data lv_type TYPE string.

    IF iv_commit_object-objtype = 'FUNC' OR iv_commit_object-objtype2 = 'FUNC'.

        " object in function group named as <function group name>.fugr.<object name>.abap, following abapGit
        IF iv_folder_structure = c_folder_structure_flat.
            rv_name = |{ iv_commit_object-fugr }.fugr.{ iv_commit_object-objname }.abap|.
        ELSEIF iv_folder_structure = c_folder_structure_eclipse.
            IF iv_commit_object-objname CP |L{ iv_commit_object-fugr }*|.
                " include case
                IF iv_local_folder = abap_true.
                    ev_file_folder = |{ iv_base_folder }Source Code Library{ c_delim }Function Groups{ c_delim }{ iv_commit_object-fugr }{ c_delim }Includes|.
                    rv_name = |Source Code Library{ c_delim }Function Groups{ c_delim }{ iv_commit_object-fugr }{ c_delim }Includes{ c_delim }{ iv_commit_object-objname }.abap|.
                ELSE.
                    rv_name = |Source Code Library{ c_delimgit }Function Groups{ c_delimgit }{ iv_commit_object-fugr }{ c_delimgit }Includes{ c_delimgit }{ iv_commit_object-objname }.abap|.
                ENDIF.
            ELSE.
                " function module case
                IF iv_local_folder = abap_true.
                    ev_file_folder = |{ iv_base_folder }Source Code Library{ c_delim }Function Groups{ c_delim }{ iv_commit_object-fugr }{ c_delim }Function Modules|.
                    rv_name = |Source Code Library{ c_delim }Function Groups{ c_delim }{ iv_commit_object-fugr }{ c_delim }Function Modules{ c_delim }{ iv_commit_object-objname }.abap|.
                ELSE.
                    rv_name = |Source Code Library{ c_delimgit }Function Groups{ c_delimgit }{ iv_commit_object-fugr }{ c_delimgit }Function Modules{ c_delimgit }{ iv_commit_object-objname }.abap|.
                ENDIF.
            ENDIF.
        ENDIF.

    ELSEIF iv_commit_object-objtype = 'FUGR'.

        " function group named as <function group name>.fugr.abap, not following abapGit
        IF iv_folder_structure = c_folder_structure_flat.
            rv_name = |{ iv_commit_object-objname }.fugr.abap|.
        ELSEIF iv_folder_structure = c_folder_structure_eclipse.
            IF iv_local_folder = abap_true.
                ev_file_folder = |{ iv_base_folder }Source Code Library{ c_delim }Function Groups{ c_delim }{ iv_commit_object-objname }|.
                rv_name = |Source Code Library{ c_delim }Function Groups{ c_delim }{ iv_commit_object-objname }{ c_delim }{ iv_commit_object-objname }.fugr.abap|.
            ELSE.
                rv_name = |Source Code Library{ c_delimgit }Function Groups{ c_delimgit }{ iv_commit_object-objname }{ c_delimgit }{ iv_commit_object-objname }.fugr.abap|.
            ENDIF.
        ENDIF.

    ELSEIF iv_commit_object-objtype = 'REPS' AND iv_commit_object-fugr IS NOT INITIAL.

        " object in function group named as <function group name>.fugr.<object name>.abap, following abapGit
        IF iv_folder_structure = c_folder_structure_flat.
            rv_name = |{ iv_commit_object-fugr }.fugr.{ iv_commit_object-objname }.abap|.
        ELSEIF iv_folder_structure = c_folder_structure_eclipse.
            IF iv_local_folder = abap_true.
                ev_file_folder = |{ iv_base_folder }Source Code Library{ c_delim }Function Groups{ c_delim }{ iv_commit_object-fugr }{ c_delim }Includes|.
                rv_name = |Source Code Library{ c_delim }Function Groups{ c_delim }{ iv_commit_object-fugr }{ c_delim }Includes{ c_delim }{ iv_commit_object-objname }.abap|.
            ELSE.
                rv_name = |Source Code Library{ c_delimgit }Function Groups{ c_delimgit }{ iv_commit_object-fugr }{ c_delimgit }Includes{ c_delimgit }{ iv_commit_object-objname }.abap|.
            ENDIF.
        ENDIF.

    ELSEIF iv_commit_object-objtype = 'CINC'.

        " test class named as <class name>.clas.testclasses.abap, following abapGit
        IF iv_folder_structure = c_folder_structure_flat.
            rv_name = |{ iv_commit_object-objname }.clas.testclasses.abap|.
        ELSEIF iv_folder_structure = c_folder_structure_eclipse.
            IF iv_local_folder = abap_true.
                ev_file_folder = |{ iv_base_folder }Source Code Library{ c_delim }Classes|.
                rv_name = |Source Code Library{ c_delim }Classes{ c_delim }{ iv_commit_object-objname }.clas.testclasses.abap|.
            ELSE.
                rv_name = |Source Code Library{ c_delimgit }Classes{ c_delimgit }{ iv_commit_object-objname }.clas.testclasses.abap|.
            ENDIF.
        ENDIF.

    ELSEIF iv_commit_object-objtype = 'ENHO'.

        " enhancement implementation named as <enhancement name>.enho.abap, not following abapGit
        IF iv_folder_structure = c_folder_structure_flat.
            rv_name = |{ iv_commit_object-objname }.enho.abap|.
        ELSEIF iv_folder_structure = c_folder_structure_eclipse.
            IF iv_local_folder = abap_true.
                ev_file_folder = |{ iv_base_folder }Enhancements{ c_delim }Enhancement Implementations|.
                rv_name = |Enhancements{ c_delim }Enhancement Implementations{ c_delim }{ iv_commit_object-objname }.enho.abap|.
            ELSE.
                rv_name = |Enhancements{ c_delimgit }Enhancement Implementations{ c_delimgit }{ iv_commit_object-objname }.enho.abap|.
            ENDIF.
        ENDIF.

    ELSEIF iv_commit_object-objtype = 'TABL' OR iv_commit_object-objtype = 'TABD'.

        " table object named as <table name>.tabl.json, not following abapGit
        IF iv_folder_structure = c_folder_structure_flat.
            rv_name = |{ iv_commit_object-objname }.tabl.json|.
        ELSEIF iv_folder_structure = c_folder_structure_eclipse.
            IF iv_local_folder = abap_true.
                ev_file_folder = |{ iv_base_folder }Dictionary{ c_delim }Data Tables|.
                rv_name = |Dictionary{ c_delim }Data Tables{ c_delim }{ iv_commit_object-objname }.tabl.json|.
            ELSE.
                rv_name = |Dictionary{ c_delimgit }Data Tables{ c_delimgit }{ iv_commit_object-objname }.tabl.json|.
            ENDIF.
        ENDIF.

    ELSEIF iv_commit_object-objtype = 'DTEL' OR iv_commit_object-objtype = 'DTED'.

        " data element object named as <object name>.dtel.json, not following abapGit
        IF iv_folder_structure = c_folder_structure_flat.
            rv_name = |{ iv_commit_object-objname }.dtel.json|.
        ELSEIF iv_folder_structure = c_folder_structure_eclipse.
            IF iv_local_folder = abap_true.
                ev_file_folder = |{ iv_base_folder }Dictionary{ c_delim }Data Elements|.
                rv_name = |Dictionary{ c_delim }Data Elements{ c_delim }{ iv_commit_object-objname }.dtel.json|.
            ELSE.
                rv_name = |Dictionary{ c_delimgit }Data Elements{ c_delimgit }{ iv_commit_object-objname }.dtel.json|.
            ENDIF.
        ENDIF.

    ELSEIF iv_commit_object-objtype = 'DOMA' OR iv_commit_object-objtype = 'DOMD'.

        " domain object named as <object name>.doma.json, not following abapGit
        IF iv_folder_structure = c_folder_structure_flat.
            rv_name = |{ iv_commit_object-objname }.doma.json|.
        ELSEIF iv_folder_structure = c_folder_structure_eclipse.
            IF iv_local_folder = abap_true.
                ev_file_folder = |{ iv_base_folder }Dictionary{ c_delim }Domains|.
                rv_name = |Dictionary{ c_delim }Domains{ c_delim }{ iv_commit_object-objname }.doma.json|.
            ELSE.
                rv_name = |Dictionary{ c_delimgit }Domains{ c_delimgit }{ iv_commit_object-objname }.doma.json|.
            ENDIF.
        ENDIF.

    ELSEIF iv_commit_object-objtype = 'ENQU' OR iv_commit_object-objtype = 'ENQD'.

        " lock object named as <object name>.enqu.json, not following abapGit
        IF iv_folder_structure = c_folder_structure_flat.
            rv_name = |{ iv_commit_object-objname }.enqu.json|.
        ELSEIF iv_folder_structure = c_folder_structure_eclipse.
            IF iv_local_folder = abap_true.
                ev_file_folder = |{ iv_base_folder }Dictionary{ c_delim }Lock Objects|.
                rv_name = |Dictionary{ c_delim }Lock Objects{ c_delim }{ iv_commit_object-objname }.enqu.json|.
            ELSE.
                rv_name = |Dictionary{ c_delimgit }Lock Objects{ c_delimgit }{ iv_commit_object-objname }.enqu.json|.
            ENDIF.
        ENDIF.

    ELSEIF iv_commit_object-objtype = 'SHLP' OR iv_commit_object-objtype = 'SHLD'.

        " search help object named as <object name>.shlp.json, not following abapGit
        IF iv_folder_structure = c_folder_structure_flat.
            rv_name = |{ iv_commit_object-objname }.shlp.json|.
        ELSEIF iv_folder_structure = c_folder_structure_eclipse.
            IF iv_local_folder = abap_true.
                ev_file_folder = |{ iv_base_folder }Dictionary{ c_delim }Srch Helps|.
                rv_name = |Dictionary{ c_delim }Srch Helps{ c_delim }{ iv_commit_object-objname }.shlp.json|.
            ELSE.
                rv_name = |Dictionary{ c_delimgit }Srch Helps{ c_delimgit }{ iv_commit_object-objname }.shlp.json|.
            ENDIF.
        ENDIF.

    ELSEIF iv_commit_object-objtype = 'TTYP' OR iv_commit_object-objtype = 'TTYD'.

        " table type object named as <object name>.ttyp.json, not following abapGit
        IF iv_folder_structure = c_folder_structure_flat.
            rv_name = |{ iv_commit_object-objname }.ttyp.json|.
        ELSEIF iv_folder_structure = c_folder_structure_eclipse.
            IF iv_local_folder = abap_true.
                ev_file_folder = |{ iv_base_folder }Dictionary{ c_delim }Table Types|.
                rv_name = |Dictionary{ c_delim }Table Types{ c_delim }{ iv_commit_object-objname }.ttyp.json|.
            ELSE.
                rv_name = |Dictionary{ c_delimgit }Table Types{ c_delimgit }{ iv_commit_object-objname }.ttyp.json|.
            ENDIF.
        ENDIF.

    ELSEIF iv_commit_object-objtype = 'VIEW' OR iv_commit_object-objtype = 'VIED'.

        " view object named as <object name>.view.json, not following abapGit
        IF iv_folder_structure = c_folder_structure_flat.
            rv_name = |{ iv_commit_object-objname }.view.json|.
        ELSEIF iv_folder_structure = c_folder_structure_eclipse.
            IF iv_local_folder = abap_true.
                ev_file_folder = |{ iv_base_folder }Dictionary{ c_delim }Views|.
                rv_name = |Dictionary{ c_delim }Views{ c_delim }{ iv_commit_object-objname }.view.json|.
            ELSE.
                rv_name = |Dictionary{ c_delimgit }Views{ c_delimgit }{ iv_commit_object-objname }.view.json|.
            ENDIF.
        ENDIF.

    ELSEIF iv_commit_object-objtype = 'XSLT'.

        " transformation XML object named as <object name>.xslt.json, not following abapGit
        IF iv_folder_structure = c_folder_structure_flat.
            rv_name = |{ iv_commit_object-objname }.xslt.json|.
        ELSEIF iv_folder_structure = c_folder_structure_eclipse.
            IF iv_local_folder = abap_true.
                ev_file_folder = |{ iv_base_folder }Source Code Library{ c_delim }Transformations|.
                rv_name = |Source Code Library{ c_delim }Transformations{ c_delim }{ iv_commit_object-objname }.xslt.json|.
            ELSE.
                rv_name = |Source Code Library{ c_delimgit }Transformations{ c_delimgit }{ iv_commit_object-objname }.xslt.json|.
            ENDIF.
        ENDIF.

     ELSEIF iv_commit_object-objtype = 'VARX' OR iv_commit_object-objtype = 'VARI'.

        " SAPSCRIPT FORM object named as <object name>.varx.txt
        IF iv_folder_structure = c_folder_structure_flat.
            rv_name = |{ iv_commit_object-objname }.varx.txt|.
        ELSEIF iv_folder_structure = c_folder_structure_eclipse.
             IF iv_local_folder = abap_true.
                ev_file_folder = |{ iv_base_folder }Report Variants{ c_delim }{ iv_commit_object-progcls }|.
                rv_name = |Report Variants{ c_delim }{ iv_commit_object-progcls }{ c_delim }{ iv_commit_object-objname }.varx.txt|.
            ELSE.
                rv_name = |Report Variants{ c_delimgit }{ iv_commit_object-progcls }{ c_delimgit }{ iv_commit_object-objname }.varx.txt|.
            ENDIF.
        ENDIF.

    ELSEIF iv_commit_object-objtype = 'FORM' OR iv_commit_object-objtype = 'TEXT'.

        " SAPSCRIPT FORM object named as <object name>.form.txt
        IF iv_folder_structure = c_folder_structure_flat.
            rv_name = |{ iv_commit_object-objname }.form.txt|.
        ELSEIF iv_folder_structure = c_folder_structure_eclipse.
            IF iv_local_folder = abap_true.
                ev_file_folder = |{ iv_base_folder }Source Code Library{ c_delim }Forms|.
                rv_name = |Source Code Library{ c_delim }Forms{ c_delim }{ iv_commit_object-objname }.form.txt|.
            ELSE.
                rv_name = |Source Code Library{ c_delimgit }Forms{ c_delimgit }{ iv_commit_object-objname }.form.txt|.
            ENDIF.
        ENDIF.

    ELSEIF iv_commit_object-objtype = 'DYNP'.

        " screen object named as <object name>.dynp.abap, not following abapGit
        IF iv_folder_structure = c_folder_structure_flat.
            rv_name = |{ iv_commit_object-objname }.dynp.abap|.
        ELSEIF iv_folder_structure = c_folder_structure_eclipse.
            IF iv_local_folder = abap_true.
                ev_file_folder = |{ iv_base_folder }Source Code Library{ c_delim }Programs|.
                rv_name = |Source Code Library{ c_delim }Programs{ c_delim }{ iv_commit_object-objname }.dynp.abap|.
            ELSE.
                rv_name = |Source Code Library{ c_delimgit }Programs{ c_delimgit }{ iv_commit_object-objname }.dynp.abap|.
            ENDIF.
        ENDIF.

    ELSEIF iv_commit_object-objtype = 'REPT'.

        " report text object named as <object name>.rept.txt, not following abapGit
        IF iv_folder_structure = c_folder_structure_flat.
            rv_name = |{ iv_commit_object-objname }.rept.txt|.
        ELSEIF iv_folder_structure = c_folder_structure_eclipse.
            DATA(lv_codetype) = 'Programs'.
            IF iv_commit_object-objtype2 = 'CLAS'.
                lv_codetype = 'Classes'.
            ENDIF.
            IF iv_local_folder = abap_true.
                ev_file_folder = |{ iv_base_folder }Source Code Library{ c_delim }{ lv_codetype }|.
                rv_name = |Source Code Library{ c_delim }{ lv_codetype }{ c_delim }{ iv_commit_object-objname }.rept.txt|.
            ELSE.
                rv_name = |Source Code Library{ c_delimgit }{ lv_codetype }{ c_delimgit }{ iv_commit_object-objname }.rept.txt|.
            ENDIF.
        ENDIF.

    ELSEIF iv_commit_object-objtype = 'TEXT'.

        " SAPScript object named as <object name>.text.txt, not following abapGit
        IF iv_folder_structure = c_folder_structure_flat.
            rv_name = |{ iv_commit_object-objname }.text.txt|.
        ELSEIF iv_folder_structure = c_folder_structure_eclipse.
            IF iv_local_folder = abap_true.
                ev_file_folder = |{ iv_base_folder }Source Code Library{ c_delim }Texts|.
                rv_name = |Source Code Library{ c_delim }Texts{ c_delim }{ iv_commit_object-objname }.text.txt|.
            ELSE.
                rv_name = |Source Code Library{ c_delimgit }Texts{ c_delimgit }{ iv_commit_object-objname }.text.txt|.
            ENDIF.
        ENDIF.

    ELSEIF iv_commit_object-objtype = 'PSCC'.

        " schema named as <schema name>.schm.txt
        IF iv_folder_structure = c_folder_structure_flat.
            rv_name = |{ iv_commit_object-objname }.schm.txt|.
        ELSEIF iv_folder_structure = c_folder_structure_eclipse.
            IF iv_local_folder = abap_true.
                ev_file_folder = |{ iv_base_folder }Schema{ c_delim }{ iv_commit_object-progcls }|.
                rv_name = |Schema{ c_delim }{ iv_commit_object-progcls }{ c_delim }{ iv_commit_object-objname }.schm.txt|.
            ELSE.
                rv_name = |Schema{ c_delimgit }{ iv_commit_object-progcls }{ c_delimgit }{ iv_commit_object-objname }.schm.txt|.
            ENDIF.
        ENDIF.

    ELSEIF iv_commit_object-objtype = 'PCYC'.

        " PCR named as <schema name>.pcr.txt
        IF iv_folder_structure = c_folder_structure_flat.
            rv_name = |{ iv_commit_object-objname }.pcr.txt|.
        ELSEIF iv_folder_structure = c_folder_structure_eclipse.
            IF iv_local_folder = abap_true.
                ev_file_folder = |{ iv_base_folder }PCR{ c_delim }{ iv_commit_object-progcls }|.
                rv_name = |PCR{ c_delim }{ iv_commit_object-progcls }{ c_delim }{ iv_commit_object-objname }.pcr.txt|.
            ELSE.
                rv_name = |PCR{ c_delimgit }{ iv_commit_object-progcls }{ c_delimgit }{ iv_commit_object-objname }.pcr.txt|.
            ENDIF.
        ENDIF.

    ELSEIF iv_commit_object-objtype = 'PROG'
        OR iv_commit_object-objtype = 'CLAS'
        OR iv_commit_object-objtype = 'INTF'
        OR iv_commit_object-objtype = 'REPS'.

        " others named as <object name>.<object type, PROG|CLAS|INTF|...>.abap, following abapGit
        IF iv_folder_structure = c_folder_structure_flat.
            rv_name = |{ iv_commit_object-objname }.{ iv_commit_object-objtype }.abap|.
        ELSEIF iv_folder_structure = c_folder_structure_eclipse.
            IF iv_commit_object-objtype = 'PROG'.
                IF iv_commit_object-subc = '1'.
                    lv_type = 'Programs'.
                ELSE.
                    lv_type = 'Includes'.
                ENDIF.
            ELSEIF iv_commit_object-objtype = 'CLAS'.
                lv_type = 'Classes'.
            ELSEIF iv_commit_object-objtype = 'INTF'.
                lv_type = 'Interfaces'.
            ELSEIF iv_commit_object-objtype = 'REPS'.
                lv_type = 'Includes'.
            ENDIF.
            IF iv_local_folder = abap_true.
                ev_file_folder = |{ iv_base_folder }Source Code Library{ c_delim }{ lv_type }|.
                rv_name = |Source Code Library{ c_delim }{ lv_type }{ c_delim }{ iv_commit_object-objname }.{ iv_commit_object-objtype }.abap|.
            ELSE.
                rv_name = |Source Code Library{ c_delimgit }{ lv_type }{ c_delimgit }{ iv_commit_object-objname }.{ iv_commit_object-objtype }.abap|.
            ENDIF.
        ENDIF.

    ELSE.

        " config change named as <table name>.<full|delta>.txt
        IF iv_folder_structure = c_folder_structure_flat.
            rv_name = |{ iv_commit_object-objname }.{ iv_commit_object-progcls }.txt|.
        ELSEIF iv_folder_structure = c_folder_structure_eclipse.
            IF iv_local_folder = abap_true.
                ev_file_folder = |{ iv_base_folder }{ iv_commit_object-devclass }{ c_delim }{ iv_commit_object-progcls }|.
                rv_name = |{ iv_commit_object-progcls }{ c_delim }{ iv_commit_object-objname }.{ iv_commit_object-progcls }.txt|.
            ELSE.
                rv_name = |{ iv_commit_object-progcls }{ c_delimgit }{ iv_commit_object-objname }.{ iv_commit_object-progcls }.txt|.
            ENDIF.
        ENDIF.

    ENDIF.

  ENDMETHOD.


  METHOD BUILD_PUSH_JSON.
    " https://learn.microsoft.com/en-us/rest/api/azure/devops/git/pushes/create?view=azure-devops-rest-7.1&tabs=HTTP
    DATA lv_change TYPE ts_change.
    lv_change-changetype = iv_changetype.
    lv_change-item-path = iv_filename.
    " an add or edit should have file content prepared, but not for deletion
    IF iv_changetype <> 16.
        lv_change-newContent-content = iv_filecontent.
        lv_change-newContent-contentType = 0.
    ENDIF.
    APPEND lv_change TO iv_commit-changes.
  ENDMETHOD.


  METHOD BUILD_SYNC_STATUS.
    DATA lv_sync_status TYPE ts_sync_status.
    DATA lt_trids TYPE tty_trids.
    DATA lr_json_serializer TYPE REF TO cl_trex_json_serializer.

    IF iv_trid IS SUPPLIED.
        lv_sync_status-trid = iv_trid.
    ELSEIF iv_mode = ZCL_UTILITY_ABAPTOGIT_TR=>c_latest_version.
        " in active version mode, it's not sync-ed to latest TR but active/latest version of objects
        get_trs( IMPORTING et_trids = lt_trids ).
        lv_sync_status-trid = lt_trids[ 1 ]-trid.
    ENDIF.

    lv_sync_status-mode = iv_mode.
    lv_sync_status-updatedate = sy-datum.
    lv_sync_status-updatetime = sy-uzeit.
    CREATE OBJECT lr_json_serializer EXPORTING data = lv_sync_status.
    lr_json_serializer->serialize( ).
    ev_filecontent = lr_json_serializer->get_data( ).
  ENDMETHOD.


  METHOD CONSTRUCTOR.

    me->username = iv_username.
    me->pat = iv_pat.
    me->orgid = iv_orgid.
    me->repoid = iv_repoid.
    me->project = iv_project.

    IF io_objtelemetry IS SUPPLIED.
        me->oref_telemetry = io_objtelemetry.
    ENDIF.

    IF iv_methtelemetry IS SUPPLIED.
        me->method_name_telemetry = iv_methtelemetry.
    ENDIF.

  ENDMETHOD.


  METHOD CREATE_BRANCH.
    " https://learn.microsoft.com/en-us/rest/api/azure/devops/git/refs/update-refs?view=azure-devops-rest-7.1&tabs=HTTP
    DATA lv_commitid TYPE string.
    DATA lt_refupdates TYPE tty_refupdates.
    DATA lv_status TYPE i.
    DATA lt_ret_data TYPE /ui5/cl_json_parser=>t_entry_map.
    rv_success = me->get_commit_ado(
        EXPORTING
            iv_branch = iv_basebranch
        IMPORTING
            ev_commitid = lv_commitid
             ).
    CHECK rv_success = abap_true.
    DATA(refPath) = |{ me->orgid }/_apis/git/repositories/{ me->repoid }/refs?api-version=7.1-preview.1|.
    APPEND VALUE ts_refupdate( name = |{ c_head }{ iv_newbranch }| oldObjectId = c_empty_sha1 newObjectId = lv_commitid ) TO lt_refupdates.
    me->HTTP_POST_JSON(
        EXPORTING
            iv_path = refPath
            iv_username = me->username
            iv_pat = me->pat
            iv_json = lt_refupdates
        IMPORTING
            ev_status = lv_status
            et_entry_map = lt_ret_data
             ).
    rv_success = abap_true.
    IF lv_status < 200 OR lv_status >= 300.
        me->write_telemetry( iv_message = |CREATE_BRANCH fails to create branch to Git { lv_status } for branch { iv_newbranch } on top of commit { lv_commitid }| ).
        rv_success = abap_false.
    ENDIF.
  ENDMETHOD.


  METHOD FIND_ACTIVE_PULLREQUEST.
    " https://learn.microsoft.com/en-us/rest/api/azure/devops/git/pull-requests/get-pull-requests?view=azure-devops-rest-7.1&tabs=HTTP
    DATA lv_status TYPE i.
    DATA lt_ret_data TYPE /ui5/cl_json_parser=>t_entry_map.
    DATA lv_prid TYPE string.
    DATA lv_reponame TYPE string.
    DATA(prPath) = |{ me->orgid }/_apis/git/repositories/{ me->repoid }/pullrequests?|
            && |searchCriteria.sourceRefName={ c_head }{ iv_srcbranch }&searchCriteria.targetRefName={ c_head }{ iv_tarbranch }&|
            && |searchCriteria.repositoryId={ me->repoid }&searchCriteria.sourceRepositoryId={ me->repoid }&|
            && 'searchCriteria.status=active&api-version=7.1-preview.1'.
    me->HTTP_GET_JSON(
        EXPORTING
            iv_path = prPath
            iv_username = me->username
            iv_pat = me->pat
        IMPORTING
            ev_status = lv_status
            et_entry_map = lt_ret_data
             ).
    rv_success = abap_true.
    IF lv_status < 200 OR lv_status >= 300.
        me->write_telemetry( iv_message = |FIND_ACTIVE_PULLREQUEST fails to get active PR from Git from { iv_srcbranch } to { iv_tarbranch }| ).
        rv_success = abap_false.
        RETURN.
    ENDIF.
    IF line_exists( lt_ret_data[ name = 'pullRequestId' parent = '/value/1' ] ).
        lv_prid = lt_ret_data[ name = 'pullRequestId' parent = '/value/1' ]-value.
        rv_success = me->get_repo(
            IMPORTING
                ev_reponame = lv_reponame
                 ).
        CHECK rv_success = abap_true.
        ev_prurl = |https://{ me->orgid }.visualstudio.com/{ me->project }/_git/{ lv_reponame }/pullrequest/{ lv_prid }|.
    ELSE.
        CLEAR ev_prurl.
    ENDIF.
  ENDMETHOD.


  METHOD FIND_COMPLETED_PULLREQUEST.
    " https://learn.microsoft.com/en-us/rest/api/azure/devops/git/pull-requests/get-pull-requests?view=azure-devops-rest-7.1&tabs=HTTP
    DATA lv_status TYPE i.
    DATA lt_ret_data TYPE /ui5/cl_json_parser=>t_entry_map.
    DATA lv_prid TYPE string.
    DATA lt_prids TYPE TABLE OF i WITH DEFAULT KEY.
    DATA lv_id TYPE i.
    DATA lv_reponame TYPE string.
    DATA(prPath) = |{ me->orgid }/_apis/git/repositories/{ me->repoid }/pullrequests?|
            && |searchCriteria.sourceRefName={ c_head }{ iv_srcbranch }&searchCriteria.targetRefName={ c_head }{ iv_tarbranch }&|
            && |searchCriteria.repositoryId={ me->repoid }&searchCriteria.sourceRepositoryId={ me->repoid }&|
            && 'searchCriteria.status=completed&api-version=7.1-preview.1'.
    me->HTTP_GET_JSON(
        EXPORTING
            iv_path = prPath
            iv_username = me->username
            iv_pat = me->pat
        IMPORTING
            ev_status = lv_status
            et_entry_map = lt_ret_data
             ).
    rv_success = abap_true.
    IF lv_status < 200 OR lv_status >= 300.
        me->write_telemetry( iv_message = |FIND_COMPLETED_PULLREQUEST fails to get completed PRs from Git from { iv_srcbranch } to { iv_tarbranch }| ).
        rv_success = abap_false.
        RETURN.
    ENDIF.
    DATA(lv_count) = lt_ret_data[ name = 'count' ]-value.
    CLEAR ev_prurl.
    IF lv_count = 0.
        RETURN.
    ELSE.
        DATA(lv_index) = 1.
        DO lv_count TIMES.
            lv_prid = lt_ret_data[ name = 'pullRequestId' parent = |/value/{ lv_index }| ]-value.
            lv_id = lv_prid.
            APPEND lv_id TO lt_prids.
            lv_index = lv_index + 1.
        ENDDO.
        SORT lt_prids DESCENDING.
        lv_prid = lt_prids[ 1 ].
        rv_success = me->get_repo(
            IMPORTING
                ev_reponame = lv_reponame
                 ).
        CHECK rv_success = abap_true.
        ev_prurl = |https://{ me->orgid }.visualstudio.com/{ me->project }/_git/{ lv_reponame }/pullrequest/{ lv_prid }|.
    ENDIF.
  ENDMETHOD.


  METHOD CREATE_PULLREQUEST.

    " known issue is createdby tag in post body doesn't help specify the PR owner to given user, say TR owner here
    " Github issue for reference https://github.com/Azure/azure-rest-api-specs/issues/10797
    " this leads to creator of the PR is always the user of me->username and me->pat, causing confusions in PR search and email notification

    " https://learn.microsoft.com/en-us/rest/api/azure/devops/git/pull-requests/create?view=azure-devops-rest-7.1&tabs=HTTP
    DATA lv_userid TYPE string.
    DATA lv_displayname TYPE string.
    DATA lt_json_req TYPE ts_createpr_json_req.
    DATA lv_status TYPE i.
    DATA lt_ret_data TYPE /ui5/cl_json_parser=>t_entry_map.
    DATA lv_prid TYPE string.
    DATA lv_reponame TYPE string.

    IF iv_user <> '' AND iv_domain <> ''.
        rv_success = me->get_displayname_ado(
            EXPORTING
                iv_user = iv_user
                iv_domain = iv_domain
            IMPORTING
                ev_id = lv_userid
                ev_name = lv_displayname
             ).
        IF rv_success = abap_true.
            lt_json_req-createdby-displayName = lv_displayname.
            lt_json_req-createdby-id = lv_userid.
            lt_json_req-createdby-uniquename = |{ iv_user }@{ iv_domain }|.
        ENDIF.
    ENDIF.

    lt_json_req-sourcerefname = |{ c_head }{ iv_srcbranch }|.
    lt_json_req-targetrefname = |{ c_head }{ iv_tarbranch }|.
    lt_json_req-title = iv_title.
    lt_json_req-description = iv_description.
    lt_json_req-completionoptions-deletesourcebranch = abap_true.
    lt_json_req-completionoptions-mergestrategy-squash = 'true'.

    DATA(prPath) = |{ me->orgid }/_apis/git/repositories/{ me->repoid }/pullrequests?supportsIterations=true&api-version=7.1-preview.1|.
    me->HTTP_POST_JSON(
        EXPORTING
            iv_path = prPath
            iv_username = me->username
            iv_pat = me->pat
            iv_json = lt_json_req
        IMPORTING
            ev_status = lv_status
            et_entry_map = lt_ret_data
             ).
    IF lv_status < 200 OR lv_status >= 300.
        me->write_telemetry( iv_message = |CREATE_PULLREQUEST fails to create PR to Git { lv_status } for branch { iv_tarbranch } from { iv_srcbranch }| ).
        rv_success = abap_false.
        RETURN.
    ENDIF.
    lv_prid = lt_ret_data[ name = 'pullRequestId' ]-value. "#EC CI_SORTSEQ

    rv_success = me->get_repo(
        IMPORTING
            ev_reponame = lv_reponame
             ).
    CHECK rv_success = abap_true.
    ev_prurl = |https://{ me->orgid }.visualstudio.com/{ me->project }/_git/{ lv_reponame }/pullrequest/{ lv_prid }|.

  ENDMETHOD.


  METHOD GET_COMMIT_ADO.
    " https://learn.microsoft.com/en-us/rest/api/azure/devops/git/commits/get-commits?view=azure-devops-rest-7.1&tabs=HTTP
    DATA lt_ret_data TYPE /ui5/cl_json_parser=>t_entry_map.
    DATA(commitPath) = |{ me->orgid }/_apis/git/repositories/{ me->repoid }/commits?searchCriteria.$top=1&searchCriteria.itemVersion.version={ iv_branch }&api-version=7.1-preview.1|.
    DATA lv_status TYPE i.
    ev_commitid = ''.
    me->HTTP_GET_JSON(
        EXPORTING
            iv_path = commitPath
            iv_username = me->username
            iv_pat = me->pat
        IMPORTING
            ev_status = lv_status
            et_entry_map = lt_ret_data
             ).
    rv_success = abap_true.
    IF lv_status < 200 OR lv_status >= 300.
        me->write_telemetry( iv_message = |GET_COMMIT_ADO fails to get commit from Git for branch { iv_branch }| ).
        rv_success = abap_false.
        RETURN.
    ENDIF.
    ev_commitId = lt_ret_data[ name = 'commitId' parent = '/value/1' ]-value.
  ENDMETHOD.


  METHOD GET_DISPLAYNAME_ADO.
    " https://learn.microsoft.com/en-us/rest/api/azure/devops/ims/identities/read-identities?view=azure-devops-rest-7.1&tabs=HTTP
    DATA lt_ret_data TYPE /ui5/cl_json_parser=>t_entry_map.
    DATA(idPath) = |{ me->orgid }/_apis/identities?searchFilter=General&filterValue={ iv_user }@{ iv_domain }&queryMembership=None&api-version=7.1-preview.1|.
    DATA lv_status TYPE i.
    ev_name = ''.
    me->HTTP_GET_JSON(
        EXPORTING
            iv_host = c_hostid
            iv_path = idPath
            iv_username = me->username
            iv_pat = me->pat
            iv_log = abap_true
        IMPORTING
            ev_status = lv_status
            et_entry_map = lt_ret_data
             ).
    rv_success = abap_true.
    IF lv_status < 200 OR lv_status >= 300.
        me->write_telemetry( iv_message = |GET_DISPLAYNAME_ADO fails to get ID display name from user { iv_user } domain { iv_domain } status { lv_status }| ).
        rv_success = abap_false.
        RETURN.
    ENDIF.
    IF lt_ret_data[ name = 'count' ]-value <> '1'.  "#EC CI_SORTSEQ
        me->write_telemetry( iv_message = |GET_DISPLAYNAME_ADO fails to get ID display name from user { iv_user } domain { iv_domain }| ).
        rv_success = abap_false.
        RETURN.
    ENDIF.
    ev_id = lt_ret_data[ name = 'id' parent = '/value/1' ]-value.    "#EC CI_SORTSEQ
    ev_name = lt_ret_data[ name = 'providerDisplayName' parent = '/value/1' ]-value.    "#EC CI_SORTSEQ
  ENDMETHOD.


  METHOD GET_ITEM_ADO.
    " https://learn.microsoft.com/en-us/rest/api/azure/devops/git/items/get?view=azure-devops-rest-7.1&tabs=HTTP
    DATA lt_ret_data TYPE /ui5/cl_json_parser=>t_entry_map.
    DATA lv_read TYPE string.
    IF iv_read = abap_true.
        lv_read = 'true'.
    ELSE.
        lv_read = 'false'.
    ENDIF.
    DATA(itemPath) = |{ me->orgid }/_apis/git/repositories/{ me->repoid }/items?path={ iv_itempath }&includeContent={ lv_read }&versionDescriptor.version={ iv_branch }&versionDescriptor.versionType=branch&api-version=7.1-preview.1|.
    DATA lv_status TYPE i.
    ev_content = ''.
    me->HTTP_GET_JSON(
        EXPORTING
            iv_path = itemPath
            iv_username = me->username
            iv_pat = me->pat
            iv_log = iv_read
        IMPORTING
            ev_status = lv_status
            et_entry_map = lt_ret_data
             ).
    rv_success = abap_true.
    IF lv_status < 200 OR lv_status >= 300.
        IF iv_read = abap_true.
            me->write_telemetry( iv_message = |GET_ITEM_ADO fails to get item content from Git for branch { iv_branch }, path { iv_itempath }| ).
        ENDIF.
        rv_success = abap_false.
        RETURN.
    ENDIF.
    IF iv_read = abap_true.
        ev_content = lt_ret_data[ name = 'content' ]-value. "#EC CI_SORTSEQ
    ENDIF.
  ENDMETHOD.


  METHOD GET_PULLREQUESTCREATE_URL.
    DATA lv_reponame TYPE string.
    rv_success = me->get_repo(
        IMPORTING
            ev_reponame = lv_reponame
             ).
    CHECK rv_success = abap_true.
    ev_prurl = |https://{ me->orgid }.visualstudio.com/{ me->project }/_git/{ lv_reponame }|
        && |/pullrequestcreate?sourceRef={ iv_srcbranch }&targetRef={ iv_tarbranch }&sourceRepositoryId={ me->repoid }&targetRepositoryId={ me->repoid }|.
  ENDMETHOD.


  METHOD GET_REPO.
    " https://learn.microsoft.com/en-us/rest/api/azure/devops/git/repositories/get-repository?view=azure-devops-rest-7.1&tabs=HTTP
    DATA lt_ret_data TYPE /ui5/cl_json_parser=>t_entry_map.
    DATA lv_status TYPE i.
    DATA(repoPath) = |{ me->orgid }/_apis/git/repositories/{ me->repoid }?api-version=7.1-preview.1|.
    me->HTTP_GET_JSON(
        EXPORTING
            iv_path = repoPath
            iv_username = me->username
            iv_pat = me->pat
            iv_log = abap_true
        IMPORTING
            ev_status = lv_status
            et_entry_map = lt_ret_data
             ).
    rv_success = abap_true.
    IF lv_status < 200 OR lv_status >= 300.
        me->write_telemetry( iv_message = |GET_REPO fails to get repo name from Git for repo ID { me->repoid }| ).
        rv_success = abap_false.
        RETURN.
    ENDIF.
    ev_projid = lt_ret_data[ parent = '/project' name = 'id' ]-value. "#EC CI_SORTSEQ
    ev_reponame = lt_ret_data[ name = 'name' ]-value. "#EC CI_SORTSEQ
  ENDMETHOD.


  METHOD GET_TRS.

    IF iv_fromtrid IS SUPPLIED.
        " fetch TRs later than given released TR (workbench or customizing)
        DATA lv_dat TYPE d.
        DATA lv_tim TYPE t.
        SELECT SINGLE as4date INTO lv_dat FROM e070 WHERE trkorr = iv_fromtrid.
        SELECT SINGLE as4time INTO lv_tim FROM e070 WHERE trkorr = iv_fromtrid.
        SELECT trkorr, as4user, as4date, as4time, trfunction INTO TABLE @et_trids FROM e070
            WHERE ( trfunction = @c_custtrfunc OR trfunction = @c_wkbtrfunc ) AND trstatus = @c_relests AND ( as4date > @lv_dat OR ( as4date = @lv_dat AND as4time > @lv_tim ) ).
        SORT et_trids BY dat tim.
    ELSE.
        " fetch latest released TR (workbench or customizing)
        DATA maxdat TYPE d.
        SELECT MAX( as4date ) FROM e070 INTO maxdat WHERE ( trfunction = c_custtrfunc OR trfunction = c_wkbtrfunc ) AND trstatus = c_relests.
        SELECT trkorr, as4user, as4date, as4time, trfunction FROM e070 INTO TABLE @et_trids
            WHERE ( trfunction = @c_custtrfunc OR trfunction = @c_wkbtrfunc ) AND trstatus = @c_relests AND as4date = @maxdat.
        IF lines( et_trids ) > 1.
            SORT et_trids BY tim DESCENDING.
            DATA(lv_count) = lines( et_trids ).
            DELETE et_trids FROM 2 TO lv_count.
        ENDIF.
    ENDIF.

  ENDMETHOD.


  METHOD GET_TRS_DATERANGE.

    SELECT trkorr, as4user, as4date, as4time, trfunction INTO TABLE @et_trids FROM e070
        WHERE ( trfunction = @c_custtrfunc OR trfunction = @c_wkbtrfunc ) AND trstatus = @c_relests AND as4date >= @iv_fromdat AND as4date <= @iv_todat.
    SORT et_trids BY dat tim.

  ENDMETHOD.


  METHOD GET_WBTRS.

    SELECT trkorr INTO TABLE @et_trids FROM e070
        WHERE trfunction = @c_wkbtrfunc AND trstatus = @c_relests
            AND ( as4date > @iv_fromdat OR ( as4date = @iv_fromdat AND as4time >= @iv_fromtim ) ).
    SORT et_trids BY dat tim.

  ENDMETHOD.


  METHOD CREATE_HTTP_CLIENT.

    DATA lo_http_client TYPE REF TO if_http_client.
    DATA lo_rest_client TYPE REF TO cl_rest_http_client.
    DATA lv_accept TYPE string.

    cl_http_client=>create_by_url(
        EXPORTING
            url                = iv_url
        IMPORTING
            client             = lo_http_client
        EXCEPTIONS
            argument_not_found = 1
            plugin_not_active  = 2
            internal_error     = 3
        OTHERS             = 4 ).
    CHECK sy-subrc = 0.

    " basic auth with user name and password (personal access token) for ADO REST APIs
    DATA(lv_auth) = cl_http_utility=>encode_base64( |{ iv_username }:{ iv_pat }| ).
    lv_auth = |Basic { lv_auth }|.
    lo_http_client->request->set_header_field( name = 'authorization' value = lv_auth ).
    lv_accept = |{ if_rest_media_type=>gc_appl_json }{ iv_accepts }|.
    lo_http_client->request->set_header_field( name = 'Accept' value = lv_accept ).
    lo_http_client->propertytype_logon_popup = lo_http_client->co_disabled.
    lo_http_client->request->set_version( if_http_request=>co_protocol_version_1_1 ).

    CREATE OBJECT lo_rest_client EXPORTING io_http_client = lo_http_client.

    DATA(lo_request) = lo_rest_client->if_rest_client~create_request_entity( ).
    lo_request->set_content_type( iv_media_type = if_rest_media_type=>gc_appl_json ).

    ei_http_client = lo_http_client.
    eo_rest_client = lo_rest_client.
    ei_request = lo_request.

  ENDMETHOD.


  METHOD HTTP_GET.
    TRY.
        io_rest_client->if_rest_resource~get( ).
        DATA(lo_response) = io_rest_client->if_rest_client~get_response_entity( ).
        ev_status = lo_response->get_header_field( '~status_code' ).
        ev_response = lo_response->get_string_data( ).
    CATCH CX_REST_CLIENT_EXCEPTION.
        ev_status = '503'.
        ev_response = ''.
    ENDTRY.
  ENDMETHOD.


  METHOD HTTP_GET_JSON.

    DATA li_http_client TYPE REF TO if_http_client.
    DATA lo_rest_client TYPE REF TO cl_rest_http_client.
    DATA li_request TYPE REF TO IF_REST_ENTITY.
    DATA(lv_url) = |{ iv_host }{ iv_path }|.
    DATA lv_status TYPE string.
    DATA lv_response TYPE string.
    DATA lo_parse TYPE REF TO /ui5/cl_json_parser.
    DATA lv_statuscode TYPE i.

    me->create_http_client(
        EXPORTING
            iv_url = lv_url
            iv_username = iv_username
            iv_pat = iv_pat
        IMPORTING
            ei_http_client = li_http_client
            eo_rest_client = lo_rest_client
            ei_request = li_request
            ).

    me->http_get(
        EXPORTING
            io_rest_client = lo_rest_client
        IMPORTING
            ev_status = lv_status
            ev_response = lv_response
            ).
    lo_rest_client->if_rest_client~close( ).

    lv_statuscode = lv_status.

    IF et_entry_map IS SUPPLIED.
        CLEAR et_entry_map.
    ENDIF.

    IF ev_status IS SUPPLIED.
        ev_status = lv_statuscode.
    ENDIF.

    IF lv_statuscode < 200 OR lv_statuscode >= 300.
        IF iv_log = abap_true.
            me->write_telemetry( iv_message = |HTTP_GET_JSON HTTP GET status code { lv_status } for { lv_url }| ).
            me->write_telemetry( iv_message = |response { lv_response }| ).
        ENDIF.
        RETURN.
    ENDIF.

    IF et_entry_map IS SUPPLIED.
        CREATE OBJECT lo_parse.
        lo_parse->parse( json = lv_response ).
        et_entry_map = lo_parse->m_entries.
    ENDIF.

  ENDMETHOD.


  METHOD HTTP_POST.
    TRY.
        ii_request->set_string_data( iv_body ).
        io_rest_client->if_rest_resource~post( ii_request ).
        DATA(lo_response) = io_rest_client->if_rest_client~get_response_entity( ).
        ev_status = lo_response->get_header_field( '~status_code' ).
        ev_response = lo_response->get_string_data( ).
    CATCH CX_REST_CLIENT_EXCEPTION.
        ev_status = '503'.
        ev_response = ''.
    ENDTRY.
  ENDMETHOD.


  METHOD HTTP_POST_JSON.
    DATA lr_json_serializer TYPE REF TO cl_trex_json_serializer.
    CREATE OBJECT lr_json_serializer EXPORTING data = iv_json.
    lr_json_serializer->serialize( ).
    DATA(lv_body) = lr_json_serializer->get_data( ).
    me->http_post_json_text(
        EXPORTING
            iv_path = iv_path
            iv_username = iv_username
            iv_pat = iv_pat
            iv_json = lv_body
        IMPORTING
            ev_status = ev_status
            et_entry_map = et_entry_map
             ).
  ENDMETHOD.


  METHOD HTTP_POST_JSON_TEXT.

    DATA li_http_client TYPE REF TO if_http_client.
    DATA lo_rest_client TYPE REF TO cl_rest_http_client.
    DATA li_request TYPE REF TO IF_REST_ENTITY.
    DATA(lv_url) = |{ iv_host }{ iv_path }|.
    DATA lv_status TYPE string.
    DATA lv_response TYPE string.
    DATA lo_parse TYPE REF TO /ui5/cl_json_parser.
    DATA lv_statuscode TYPE i.

    me->create_http_client(
        EXPORTING
            iv_url = lv_url
            iv_username = iv_username
            iv_pat = iv_pat
            iv_accepts = iv_accepts
        IMPORTING
            ei_http_client = li_http_client
            eo_rest_client = lo_rest_client
            ei_request = li_request
            ).

    me->http_post(
        EXPORTING
            io_rest_client = lo_rest_client
            ii_request = li_request
            iv_body = iv_json
        IMPORTING
            ev_status = lv_status
            ev_response = lv_response
            ).
    lo_rest_client->if_rest_client~close( ).

    lv_statuscode = lv_status.

    IF ev_status IS SUPPLIED.
        ev_status = lv_statuscode.
    ENDIF.

    IF et_entry_map IS SUPPLIED.
        CLEAR et_entry_map.
    ENDIF.

    IF et_entry_map IS SUPPLIED.
        CREATE OBJECT lo_parse.
        lo_parse->parse( json = lv_response ).
        et_entry_map = lo_parse->m_entries.
    ENDIF.

    IF lv_statuscode < 200 OR lv_statuscode >= 300.
        me->write_telemetry( iv_message = |HTTP_POST_JSON HTTP POST status code { lv_status } for { lv_url }| ).
        me->write_telemetry( iv_message = |response { lv_response }| ).
    ENDIF.

  ENDMETHOD.


  METHOD LOAD_SYNC_STATUS.
    TRY.
        DATA lo_parse TYPE REF TO /ui5/cl_json_parser.
        CREATE OBJECT lo_parse.
        lo_parse->parse( json = iv_filecontent ).
        DATA(lt_ret_data) = lo_parse->m_entries.
        ev_sync_status-trid = lt_ret_data[ name = 'trid' ]-value.   "#EC CI_SORTSEQ
        ev_sync_status-mode = lt_ret_data[ name = 'mode' ]-value.   "#EC CI_SORTSEQ
        ev_sync_status-updatedate = lt_ret_data[ name = 'updatedate' ]-value.   "#EC CI_SORTSEQ
        ev_sync_status-updatetime = lt_ret_data[ name = 'updatetime' ]-value.   "#EC CI_SORTSEQ
        rv_success = abap_true.
    CATCH /ui5/CX_VFS_ERROR.
        rv_success = abap_false.
    ENDTRY.
  ENDMETHOD.


  METHOD PUSH_ADO.
    " https://learn.microsoft.com/en-us/rest/api/azure/devops/git/pushes/create?view=azure-devops-rest-7.1&tabs=HTTP
    DATA(lv_branch) = |{ c_head }{ iv_branch }|.
    DATA(createPushPath) = |{ me->orgid }/_apis/git/repositories/{ me->repoid }/pushes?api-version=7.1-preview.2|.
    DATA lv_json_req TYPE ts_push_json_req.
    DATA lv_status TYPE i.
    DATA lt_ret_data TYPE /ui5/cl_json_parser=>t_entry_map.
    APPEND iv_commit TO lv_json_req-commits.
    APPEND VALUE ts_refupdate( name = lv_branch oldObjectId = iv_commitid ) TO lv_json_req-refUpdates.
    me->HTTP_POST_JSON(
        EXPORTING
            iv_path = createPushPath
            iv_username = me->username
            iv_pat = me->pat
            iv_json = lv_json_req
        IMPORTING
            ev_status = lv_status
            et_entry_map = lt_ret_data
             ).
    rv_success = abap_true.
    IF lv_status < 200 OR lv_status >= 300.
        me->write_telemetry( iv_message = |PUSH_ADO fails to push to Git { lv_status } for branch { lv_branch } on top of commit { iv_commitid }| ).
        rv_success = abap_false.
    ENDIF.
  ENDMETHOD.


  METHOD PUSH_TR_COMMIT_OBJECTS.

    DATA lv_commit_object TYPE ZCL_UTILITY_ABAPTOGIT_TR=>ts_commit_object.
    DATA lv_commit TYPE ts_commit.
    DATA lv_commitid TYPE string.
    DATA lv_userid TYPE string.
    DATA lv_displayname TYPE string.
    DATA lv_changetype TYPE i.
    DATA lv_syncfilecontent TYPE string.
    DATA lv_rootfolder TYPE string.
    DATA lv_synccnt TYPE i.
    DATA lv_success TYPE abap_bool.

    lv_rootfolder = iv_rootfolder.
    TRANSLATE lv_rootfolder TO UPPER CASE.

    " construct commit object list payload for push ADO REST call
    LOOP AT it_commit_objects INTO lv_commit_object.

        " change type for add/edit/delete
        IF lv_commit_object-delflag <> ' '.
            lv_changetype = 16.
        ELSEIF lv_commit_object-verno > 1.
            lv_changetype = 2.
        ELSE.
            lv_changetype = 1.
        ENDIF.

        DATA(lv_code_name) = build_code_name(
            EXPORTING
                iv_commit_object = lv_commit_object
                iv_local_folder = abap_false
                iv_folder_structure = iv_folder_structure
                 ).
        DATA(lv_filepath) = |{ lv_rootfolder }{ lv_commit_object-devclass }{ c_delimgit }{ lv_code_name }|.

        " config change delta/full file may exist
        lv_success = me->get_item_ado(
            EXPORTING
                iv_branch = iv_branch
                iv_itempath = lv_filepath
                iv_read = abap_false
                 ).
        IF lv_success = abap_true.
            lv_changetype = 2.
        ELSE.
            lv_changetype = 1.
        ENDIF.

        " add the ABAP object change to the changes section of the payload
        me->build_push_json(
            EXPORTING
                iv_filename = lv_filepath
                iv_filecontent = lv_commit_object-filecontent
                iv_changetype = lv_changetype
            CHANGING
                iv_commit = lv_commit
             ).

        lv_synccnt = lv_synccnt + 1.

    ENDLOOP.

    IF ev_synccnt IS SUPPLIED.
        ev_synccnt = lv_synccnt.
    ENDIF.

    IF lv_synccnt = 0.
        rv_success = abap_true.
        RETURN.
    ENDIF.

    IF iv_push_status = abap_true.
        " update sync status file with the TR id
        build_sync_status(
            EXPORTING
                iv_mode = ZCL_UTILITY_ABAPTOGIT_TR=>c_latest_version
                iv_trid = iv_trid
            IMPORTING
                ev_filecontent = lv_syncfilecontent
                 ).
        DATA(lv_syncstatuspath) = |{ lv_rootfolder }{ c_sync_status_file }|.

        " sync status file may not exist
        lv_success = me->get_item_ado(
            EXPORTING
                iv_branch = iv_branch
                iv_itempath = lv_syncstatuspath
                iv_read = abap_false
                 ).
        IF lv_success = abap_true.
            lv_changetype = 2.
        ELSE.
            lv_changetype = 1.
        ENDIF.

        me->build_push_json(
            EXPORTING
                iv_filename = lv_syncstatuspath
                iv_filecontent = lv_syncfilecontent
                iv_changetype = lv_changetype
            CHANGING
                iv_commit = lv_commit
                 ).
    ENDIF.

    lv_commit-comment = iv_comment.

    " specify real author of the commit if needed
    IF iv_user <> '' AND iv_domain <> ''.
        rv_success = me->get_displayname_ado(
            EXPORTING
                iv_user = iv_user
                iv_domain = iv_domain
            IMPORTING
                ev_id = lv_userid
                ev_name = lv_displayname
             ).
        lv_commit-author-email = |{ iv_user }@{ iv_domain }|.
        IF rv_success = abap_true.
            lv_commit-author-name = lv_displayname.
        ELSE.
            lv_commit-author-name = iv_user.
        ENDIF.
    ENDIF.

    me->write_telemetry( iv_message = |{ lv_synccnt } objects to push for TR { iv_trid }| iv_kind = 'info' ).

    " fetch the head commit ID for given branch
    rv_success = me->get_commit_ado(
        EXPORTING
            iv_branch = iv_branch
        IMPORTING
            ev_commitid = lv_commitid
             ).
    CHECK rv_success = abap_true.

    " push the changes to Git by ADO REST call
    rv_success = me->push_ado(
        EXPORTING
            iv_branch = iv_branch
            iv_commit = lv_commit
            iv_commitid = lv_commitid
             ).

  ENDMETHOD.


  METHOD SAVE_SYNC_STATUS.

    DATA lv_json TYPE string.
    DATA lt_filecontent TYPE TABLE OF string.

    IF iv_trid IS SUPPLIED.
        build_sync_status(
            EXPORTING
                iv_mode = iv_mode
                iv_trid = iv_trid
            IMPORTING
                ev_filecontent = lv_json
                 ).
    ELSE.
        build_sync_status(
            EXPORTING
                iv_mode = iv_mode
            IMPORTING
                ev_filecontent = lv_json
                 ).
    ENDIF.

    APPEND lv_json TO lt_filecontent.
    CALL FUNCTION 'GUI_DOWNLOAD'
          EXPORTING
            filename = iv_file
            filetype = 'ASC'
            write_field_separator = 'X'
          TABLES
            data_tab = lt_filecontent
          EXCEPTIONS
            OTHERS = 1.
    rv_success = abap_true.
    IF sy-subrc <> 0.
        rv_success = abap_false.
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