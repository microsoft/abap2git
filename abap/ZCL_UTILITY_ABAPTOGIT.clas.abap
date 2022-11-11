" Utility class to sync from ABAP objects in a transport request (TR)
" to Git repo with specific package(s) and branch
" Multiple packages will share the same Git repo
" Multiple SAP systems in the landscape of a service line will share the same Git repo
" in respective branches
" CI branch could be created for a TR and build pipeline triggered on it
" Following ABAP objects are included in sync-ing:
" Class, Function Module, Program, Include, Test Class.
" Following ABAP objects are not yet included in sync-ing:
" Data table related objects, enhancement objects.
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
CLASS ZCL_UTILITY_ABAPTOGIT DEFINITION
  PUBLIC
  FINAL
  CREATE PUBLIC .

PUBLIC SECTION.

    " type for communicating ABAP objects to sync to Git from SAP
    TYPES: BEGIN OF ts_commit_object,
            devclass    TYPE string,
            objname     TYPE string,
            objtype     TYPE string,
            objtype2    TYPE string,
            fugr        TYPE string,
            delflag     TYPE string,
            verno       TYPE i,
            filecontent TYPE string,
           END OF ts_commit_object.
    TYPES: tty_commit_object TYPE TABLE OF ts_commit_object.

    " constructor
    " iv_username - VSO user name as email address
    " iv_pat - VSO personal access token, could be generated from VSO portal per user
    " iv_orgid - organization ID, like the name "org" in <org>.visualstudio.com
    " iv_repoid - Git repo ID
    " iv_project - project name
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

    " fetch source code lines into files for ABAP code objects in given package
    " iv_package - package name
    " iv_folder - local folder name to save the files
    " iv_mode - 'latest' or 'active'
    METHODS get_package_codes
        IMPORTING
            iv_package  TYPE devclass
            iv_folder   TYPE string
            iv_mode     TYPE string
        RETURNING VALUE(rv_success) TYPE string.

    " fetch source code lines into files for ABAP code objects in given package list
    " iv_packages - package name list as string separated by comma
    " iv_folder - local folder name to save the files
    " iv_mode - 'latest' or 'active'
    METHODS get_packages_codes
        IMPORTING
            iv_packages TYPE string
            iv_folder   TYPE string
            iv_mode     TYPE string
        RETURNING VALUE(rv_success) TYPE string.

    " fetch source code lines into files for ABAP code objects in Y*/Z* packages
    " iv_folder - local folder name to save the files
    " iv_mode - 'latest' or 'active'
    METHODS get_customize_packages_codes
        IMPORTING
            iv_folder   TYPE string
            iv_mode     TYPE string DEFAULT 'latest'
        RETURNING VALUE(rv_success) TYPE string.

    " spot sync ABAP objects in a TR
    " iv_trid - TR ID
    " iv_packagenames - package names to include in commit, separated by comma
    " iv_pipelineid - ADO build pipeline ID
    " iv_prefix - branch prefix, suggest to use pattern 'users/<service line>/<purpose ci|develop>/'
    METHODS spotsync_tr
        IMPORTING
            iv_trid         TYPE string
            iv_packagenames TYPE string
            iv_pipelineid   TYPE string
            iv_prefix       TYPE string DEFAULT 'users/ci/'
        RETURNING VALUE(rv_success) TYPE string.

    " fetch ABAP objects from SAP to commit to Git for a TR
    " iv_trid - TR ID
    " iv_packagenames - package names to include in commit, separated by comma
    " ev_comment - commit comment
    " it_commit_objects - table of ABAP objects to commit to Git including name, type, file content, add/update/delete status
    METHODS get_tr_commit_objects
        IMPORTING
            iv_trid             TYPE string
            iv_packagenames     TYPE string
        EXPORTING
            ev_comment          TYPE string
        CHANGING
            it_commit_objects   TYPE tty_commit_object
        RETURNING VALUE(rv_success) TYPE string.

    " push the ABAP objects to Git for a TR
    " iv_trid - TR ID to push to Git
    " iv_branch - branch name to push the changes to
    " iv_comment - commit comment retrieved from get_tr_commit_objects
    " iv_rootfolder - the root folder in Git local clone for ABAP objects to add to, shared by all packages in SAP
    " it_commit_objects - table of ABAP objects to commit to Git, retrieved from get_tr_commit_objects
    METHODS push_tr_commit_objects
        IMPORTING
            iv_trid             TYPE string
            iv_branch           TYPE string
            iv_comment          TYPE string
            iv_rootfolder       TYPE string DEFAULT '/src/'
            it_commit_objects   TYPE tty_commit_object
        RETURNING VALUE(rv_success) TYPE string.

    " prepare branch name for current landscape system
    " iv_prefix - branch prefix
    " ev_branch - branch name constructed
    METHODS prepare_landscape_branch
        IMPORTING
            iv_prefix TYPE string DEFAULT 'users/ci/'
        EXPORTING
            ev_branch TYPE string.

    " prepare branch for CI of given TR ID
    " iv_trid - TR ID to prepare CI branch
    " iv_baseprefix - branch prefix for base branch
    " iv_ciprefix - branch prefix for CI branch
    " ev_branch - branch name constructed and prepared
    METHODS prepare_ci_branch
        IMPORTING
            iv_trid         TYPE string
            iv_baseprefix   TYPE string DEFAULT 'users/ci/'
            iv_ciprefix     TYPE string DEFAULT 'users/'
        EXPORTING
            ev_branch       TYPE string
        RETURNING VALUE(rv_success) TYPE string.

    " cleanup the branch for CI of given TR ID
    " iv_trid - TR ID for the CI branch
    " iv_prefix - branch prefix
    METHODS cleanup_ci_branch
        IMPORTING
            iv_trid TYPE string
            iv_prefix TYPE string DEFAULT 'users/'
        RETURNING VALUE(rv_success) TYPE string.

    " kick off a run for specified ADO pipeline on the given CI branch
    " iv_pipelineid - ADO pipeline ID to run CI, could be found in variable system.definitionId
    " iv_branch - branch name to run CI
    " ev_runid - run ID kicked off
    METHODS kickoff_pipeline_run_ado
        IMPORTING
            iv_pipelineid   TYPE string
            iv_branch       TYPE string
        EXPORTING
            ev_runid        TYPE string
        RETURNING VALUE(rv_success) TYPE string.

    " sync TRs that not yet sync-ed to Git since last TR marked in repo sync status file
    " iv_branch - branch name to push the changes to
    " iv_packagenames - package names to include in commit, separated by comma
    " iv_rootfolder - the root folder in Git local clone for ABAP objects to add to, shared by all packages in SAP
    METHODS catchup_trs
        IMPORTING
            iv_branch       TYPE string
            iv_packagenames TYPE string
            iv_rootfolder   TYPE string DEFAULT '/src/'
        RETURNING VALUE(rv_success) TYPE string.

PROTECTED SECTION.

PRIVATE SECTION.

    CONSTANTS: c_host               TYPE string VALUE 'https://dev.azure.com/',
               c_head               TYPE string VALUE 'refs/heads/',
               c_sync_status_file   TYPE string VALUE '.sync_status.json',
               c_null_objectid_ref  TYPE string VALUE '0000000000000000000000000000000000000000',
               c_latest_version     TYPE string VALUE 'latest',
               c_active_version     TYPE string VALUE 'active'.

    " source lines of an ABAP object
    TYPES: tty_abaptext TYPE TABLE OF ABAPTXT255 INITIAL SIZE 0.

    TYPES: BEGIN OF ts_version_no,
           objname      TYPE versobjnam,
           objtype      TYPE versobjtyp,
           objversionno TYPE versno,
           END OF ts_version_no.
    TYPES: tty_version_no TYPE STANDARD TABLE OF ts_version_no.

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
    TYPES: BEGIN OF ts_commit,
            changes TYPE tty_changes,
            comment TYPE string,
           END OF ts_commit.
    TYPES: tty_commits TYPE TABLE OF ts_commit WITH KEY comment.
    TYPES: BEGIN OF ts_refupdate,
            name        TYPE string,
            oldObjectId TYPE string,
           END OF ts_refupdate.
    TYPES: tty_refupdates TYPE TABLE OF ts_refupdate WITH KEY name.
    TYPES: BEGIN OF ts_push_json_req,
            commits     TYPE tty_commits,
            refUpdates  TYPE tty_refupdates,
           END OF ts_push_json_req.
    TYPES: BEGIN OF ts_updaterefs_json_req,
            name        TYPE string,
            oldObjectId TYPE string,
            newObjectId TYPE string,
           END OF ts_updaterefs_json_req.
    TYPES: BEGIN OF ts_self,
            refName TYPE string,
           END OF ts_self.
    TYPES: BEGIN OF ts_repository,
            self TYPE ts_self,
           END OF ts_repository.
    TYPES: BEGIN OF ts_resource,
            repositories TYPE ts_repository,
           END OF ts_resource.
    TYPES: BEGIN OF ts_run_json_req,
            resources TYPE ts_resource,
           END OF ts_run_json_req.

    " code object used in downloading ABAP code object to local disk
    TYPES: BEGIN OF ts_code_object,
            obj_name    TYPE sobj_name,
            obj_type    TYPE trobjtype,
            obj_type2   TYPE trobjtype,
            fugr_name   TYPE sobj_name,
           END OF ts_code_object.

    " payload of JSON file in Git repo root folder to mark down sync status
    TYPES: BEGIN OF ts_sync_status,
            trid        TYPE string,
            mode        TYPE string,
            updatedate  TYPE d,
            updatetime  TYPE t,
           END OF ts_sync_status.

    " list of TR IDs to sync to Git
    TYPES: tty_trids TYPE TABLE OF string.

    " credential for ADO REST APIs
    DATA username TYPE string.
    DATA pat TYPE string.

    " organization ID, like the name "org" in <org>.visualstudio.com
    DATA orgid TYPE string.
    " Git repo ID
    DATA repoid TYPE string.
    " project name
    DATA project TYPE string.

    " telemetry callback
    DATA oreftelemetry TYPE REF TO object.
    DATA methtelemetry TYPE string.

    " get function group name of an object in a function group
    METHODS get_fugr
        IMPORTING
            iv_objname  TYPE string
        EXPORTING
            ev_fugrname TYPE string.

    " get source code lines and count of them
    METHODS get_code_lines
        IMPORTING
            iv_version  TYPE versno
            iv_objname  TYPE versobjnam
            iv_objtype  TYPE versobjtyp
            iv_logdest  TYPE rfcdest
        EXPORTING
            linecount   TYPE i
            abaptext    TYPE tty_abaptext
        RETURNING VALUE(rv_success) TYPE string.

    METHODS build_code_content
        IMPORTING
            iv_objname          TYPE e071-obj_name
            iv_objtype          TYPE e071-object
            it_objversions      TYPE tty_version_no
        EXPORTING
            et_filecontent      TYPE tty_abaptext
            ev_tclsname         TYPE string
            ev_tclstype         TYPE string
            et_tclsfilecontent  TYPE tty_abaptext
        RETURNING VALUE(rv_success) TYPE string.

    " construct source code file name in Git repo from ABAP code object name
    METHODS build_code_name
        IMPORTING
            iv_commit_object    TYPE ts_commit_object
        RETURNING VALUE(rv_name) TYPE string.

    " get ABAP object version number (to fetch specific version's code lines)
    METHODS get_versions_no
        IMPORTING
            iv_objname      TYPE e071-obj_name
            iv_objtype      TYPE e071-object
            iv_mode         TYPE string
            iv_date         TYPE d OPTIONAL
            iv_time         TYPE t OPTIONAL
        EXPORTING
            ev_version_no   TYPE i
        CHANGING
            it_objversions  TYPE tty_version_no OPTIONAL
        RETURNING VALUE(rv_success) TYPE string.

    " get class object version (for public/protected/private sections in definition and method implementations)
    METHODS get_class_versions_no
        IMPORTING
            iv_objname  TYPE e071-obj_name
            iv_objtype  TYPE e071-object
            iv_mode     TYPE string
            iv_date     TYPE d OPTIONAL
            iv_time     TYPE t OPTIONAL
        CHANGING
            it_objversions  TYPE tty_version_no OPTIONAL
        RETURNING VALUE(r_version_no) TYPE i.

    " get valued version to no later than specific time stamp if any given latest/active version mode
    METHODS get_valued_version
        IMPORTING
            iv_mode     TYPE string
            iv_date     TYPE d OPTIONAL
            iv_time     TYPE t OPTIONAL
        EXPORTING
            ev_versno   TYPE versno
            ev_verscnt  TYPE i
        CHANGING
            cht_vers    TYPE vrsd_tab
        RETURNING VALUE(rv_success) TYPE string.

    " get versions of an ABAP object
    METHODS get_versions
        IMPORTING
            iv_objname      TYPE e071-obj_name
            iv_objtype      TYPE e071-object
        CHANGING
            it_vers         TYPE vrsd_tab
        RETURNING VALUE(rv_success) TYPE string.

    " get methods of a class object
    METHODS get_class_methods
        IMPORTING
            iv_classname    TYPE classname
        CHANGING
            cht_methods     TYPE abap_methdescr_tab.

    " get IDs of TRs after a given TR, or ID of latest TR if not given
    METHODS get_trs
        IMPORTING
            iv_fromtrid   TYPE string OPTIONAL
        EXPORTING
            et_trids      TYPE tty_trids.

    " construct sync status file content as a mark of which TR current branch sync to
    METHODS build_sync_status
        IMPORTING
            iv_mode         TYPE string
            iv_trid         TYPE string OPTIONAL
        EXPORTING
            ev_filecontent  TYPE string.

    " save constructed sync status file to local disk
    METHODS save_sync_status
        IMPORTING
            iv_mode TYPE string
            iv_file TYPE string
        RETURNING VALUE(rv_success) TYPE string.

    " get sync status from the sync status file (by fetching item content from ADO REST API or local disk file)
    METHODS load_sync_status
        IMPORTING
            iv_filecontent  TYPE string
        EXPORTING
            ev_sync_status  TYPE ts_sync_status
        RETURNING VALUE(rv_success) TYPE string.

    " fetch item content by ADO REST API
    METHODS get_item_ado
        IMPORTING
            iv_branch   TYPE string
            iv_itempath TYPE string
        EXPORTING
            ev_content  TYPE string
        RETURNING VALUE(rv_success) TYPE string.

    " fetch commit ID of a branch by ADO REST API
    METHODS get_commit_ado
        IMPORTING
            iv_branch   TYPE string
        EXPORTING
            ev_commitid TYPE string
        RETURNING VALUE(rv_success) TYPE string.

    " add a change to an ABAP object to ADO REST API body for push
    METHODS build_push_json
        IMPORTING
            iv_filename     TYPE string
            iv_filecontent  TYPE string
            iv_changetype   TYPE i
        CHANGING
            iv_commit       TYPE ts_commit.

    " push changes of a TR to Git by ADO REST API
    METHODS push_ado
        IMPORTING
            iv_branch   TYPE string
            iv_commit   TYPE ts_commit
            iv_commitid TYPE string
        RETURNING VALUE(rv_success) TYPE string.

    " create private branch for a TR from "main" branch (users/ci/abc like branch) by ADO REST API
    METHODS create_branch_ado
        IMPORTING
            iv_branch   TYPE string
            iv_commitid TYPE string
        RETURNING VALUE(rv_success) TYPE string.

    " delete private branch created above
    METHODS delete_branch_ado
        IMPORTING
            iv_branch   TYPE string
            iv_commitid TYPE string
        RETURNING VALUE(rv_success) TYPE string.

    " create HTTP client for ADO REST API
    METHODS create_http_client
        IMPORTING
            iv_url      TYPE string
            iv_username TYPE string
            iv_pat      TYPE string
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

    " wrapper to make HTTP POST request with response de-serialized
    METHODS http_get_json
        IMPORTING
            iv_path         TYPE string
            iv_username     TYPE string
            iv_pat          TYPE string
        EXPORTING
            ev_status       TYPE i
            et_entry_map    TYPE /ui5/cl_json_parser=>t_entry_map.

    " wrapper to write telemetry with the callback registered
    METHODS write_telemetry
        IMPORTING
            iv_message  TYPE string
            iv_kind     TYPE string DEFAULT 'error'.

ENDCLASS.

CLASS ZCL_UTILITY_ABAPTOGIT IMPLEMENTATION.

  METHOD CONSTRUCTOR.

    me->username = iv_username.
    me->pat = iv_pat.
    me->orgid = iv_orgid.
    me->repoid = iv_repoid.
    me->project = iv_project.

    IF io_objtelemetry IS SUPPLIED.
        me->oreftelemetry = io_objtelemetry.
    ENDIF.

    IF iv_methtelemetry IS SUPPLIED.
        me->methtelemetry = iv_methtelemetry.
    ENDIF.

  ENDMETHOD.

  METHOD SPOTSYNC_TR.

    DATA lt_commit_objects TYPE tty_commit_object.
    DATA lv_comment TYPE string.
    DATA lv_basebranch TYPE string.
    DATA lv_cibranch TYPE string.
    DATA lv_runid TYPE string.

    me->write_telemetry( iv_message = |start spot sync: { sy-uzeit }| iv_kind = 'info' ).

    rv_success = me->get_tr_commit_objects(
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
    rv_success = me->push_tr_commit_objects(
        EXPORTING
            iv_trid = iv_trid
            iv_branch = lv_basebranch
            iv_comment = lv_comment
            it_commit_objects = lt_commit_objects
             ).
    CHECK rv_success = abap_true.

    me->write_telemetry( iv_message = |push TR { iv_trid } objects to base branch { lv_basebranch }: { sy-uzeit }| iv_kind = 'info' ).

    rv_success = me->prepare_ci_branch(
        EXPORTING
            iv_trid = iv_trid
            iv_baseprefix = iv_prefix
        IMPORTING
            ev_branch = lv_cibranch
             ).
    CHECK rv_success = abap_true.

    me->write_telemetry( iv_message = |create CI branch { lv_cibranch }: { sy-uzeit }| iv_kind = 'info' ).

    rv_success = me->kickoff_pipeline_run_ado(
        EXPORTING
            iv_pipelineid = iv_pipelineid
            iv_branch = lv_cibranch
        IMPORTING
            ev_runid = lv_runid
             ).

    me->write_telemetry( iv_message = |kick off CI build { lv_runid } for pipeline { iv_pipelineid }: { sy-uzeit }| iv_kind = 'info' ).

  ENDMETHOD.

  METHOD GET_TR_COMMIT_OBJECTS.

    DATA ld_cs_request TYPE TRWBO_REQUEST.
    DATA lv_trkorr TYPE TRKORR.
    FIELD-SYMBOLS <fs_cs_request_object> LIKE LINE OF ld_cs_request-objects.
    DATA lt_objversions TYPE tty_version_no.
    DATA lt_filecontent TYPE tty_abaptext.
    DATA lt_tclsfilecontent TYPE tty_abaptext.
    DATA lt_packagenames TYPE TABLE OF string.
    DATA lv_objtype2 TYPE string.
    DATA lv_devclass TYPE string.
    DATA lv_delflag TYPE c.
    DATA lv_fugr TYPE string.
    DATA lv_filecontent TYPE string.
    DATA lv_tclsname TYPE string.
    DATA lv_tclstype TYPE string.
    DATA lv_tclsfilecontent TYPE string.
    DATA lv_version_no TYPE i.
    DATA lv_funcname TYPE string.

    rv_success = abap_true.

    " fetch objects in a TR
    lv_trkorr = iv_trid.
    CALL FUNCTION 'TR_READ_REQUEST'
      EXPORTING
        iv_read_e070 =               abap_true
        iv_read_e07t =               abap_true
        iv_read_e070c =              abap_true
        iv_read_e070m =              abap_true
        iv_read_objs_keys =          abap_true
        iv_read_attributes =         abap_true
        iv_trkorr =                  lv_trkorr
      CHANGING
        cs_request =                 ld_cs_request
      EXCEPTIONS
        ERROR_OCCURED =              1
        NO_AUTHORIZATION =           2.
    IF sy-subrc <> 0.
        me->write_telemetry( iv_message = |GET_TR_COMMIT_OBJECTS fails to call TR_READ_REQUEST subrc { sy-subrc }| ).
        rv_success = abap_false.
        EXIT.
    ENDIF.

    " only support TR
    IF ld_cs_request-h-trfunction <> 'K'.
        me->write_telemetry( iv_message = |GET_TR_COMMIT_OBJECTS meets request type { ld_cs_request-h-trfunction }| ).
        rv_success = abap_false.
        EXIT.
    ENDIF.

    " construct Git commit description carrying TR ID, owner and original description
    " the Git commit can't has committer as the TR owner, instead, on-behalf-of the owner by user name/PAT specified
    ev_comment = |[TR # { iv_trid }] [OWNER: { ld_cs_request-h-as4user }] DESCRIPTION: { ld_cs_request-h-as4text }|.

    SPLIT iv_packagenames AT ',' INTO TABLE lt_packagenames.

    " process objects in the TR
    LOOP AT ld_cs_request-objects ASSIGNING <fs_cs_request_object> WHERE object <> 'RELE'.

        DATA(lv_objname) = <fs_cs_request_object>-obj_name.
        DATA(lv_objtype) = <fs_cs_request_object>-object.

        CLEAR lv_objtype2.

        " find out which package the ABAP object belongs to
        SELECT SINGLE devclass INTO lv_devclass FROM tadir
            WHERE object = lv_objtype AND obj_name = lv_objname.

        " is the object in one of the packages specified?
        CHECK line_exists( lt_packagenames[ table_line = lv_devclass ] ).

        " find out whether it's a deletion for the object
        SELECT SINGLE delflag INTO lv_delflag FROM tadir
            WHERE object = lv_objtype AND obj_name = lv_objname.

        " fetch object content if not a deletion for the object
        IF lv_delflag = ' '.

            " fetch versions no later than given TR date/time
            CLEAR lt_objversions.
            rv_success = me->get_versions_no(
                EXPORTING
                    iv_objname = lv_objname
                    iv_objtype = lv_objtype
                    iv_mode = c_latest_version
                    iv_date = ld_cs_request-h-as4date
                    iv_time = ld_cs_request-h-as4time
                IMPORTING
                    ev_version_no = lv_version_no
                CHANGING
                    it_objversions = lt_objversions
                    ).
            CHECK rv_success = abap_true.
            CHECK lv_version_no > 0.

            " fetch function group name in case of function module
            IF lv_objtype = 'FUGR'.
                lv_funcname = lv_objname.
                lv_objtype2 = 'FUNC'.
                me->get_fugr( EXPORTING iv_objname = lv_funcname IMPORTING ev_fugrname = lv_fugr ).
            ENDIF.

            " construct object content (and test class content if any)
            CLEAR lt_filecontent.
            CLEAR lt_tclsfilecontent.
            CLEAR lv_tclsfilecontent.
            rv_success = me->build_code_content(
                EXPORTING
                    iv_objname = lv_objname
                    iv_objtype = lv_objtype
                    it_objversions = lt_objversions
                IMPORTING
                    et_filecontent = lt_filecontent
                    ev_tclsname = lv_tclsname
                    ev_tclstype = lv_tclstype
                    et_tclsfilecontent = lt_tclsfilecontent
                    ).
            CHECK rv_success = abap_true.

            lv_filecontent = concat_lines_of( table = lt_filecontent sep = CL_ABAP_CHAR_UTILITIES=>CR_LF ).

            " test class
            IF lines( lt_tclsfilecontent ) > 0.
                lv_tclsfilecontent = concat_lines_of( table = lt_tclsfilecontent sep = CL_ABAP_CHAR_UTILITIES=>CR_LF ).
                TRANSLATE lv_tclsname TO UPPER CASE.
                APPEND VALUE ts_commit_object(
                    devclass = lv_devclass
                    objname = lv_objname
                    objtype = lv_tclstype
                    objtype2 = lv_objtype2
                    fugr = lv_fugr
                    delflag = lv_delflag
                    verno = lv_version_no
                    filecontent = lv_tclsfilecontent
                    ) TO it_commit_objects.
            ENDIF.

        ENDIF.

        TRANSLATE lv_objname TO UPPER CASE.

        APPEND VALUE ts_commit_object(
            devclass = lv_devclass
            objname = lv_objname
            objtype = lv_objtype
            objtype2 = lv_objtype2
            fugr = lv_fugr
            delflag = lv_delflag
            verno = lv_version_no
            filecontent = lv_filecontent
            ) TO it_commit_objects.

    ENDLOOP.

  ENDMETHOD.

  METHOD PUSH_TR_COMMIT_OBJECTS.

    DATA lv_commit_object TYPE ts_commit_object.
    DATA lv_commit TYPE ts_commit.
    DATA lv_commitid TYPE string.
    DATA lv_changetype TYPE i.
    DATA lv_syncfilecontent TYPE string.
    DATA lv_rootfolder TYPE string.
    DATA lv_synccnt TYPE i.

    lv_rootfolder = iv_rootfolder.
    TRANSLATE lv_rootfolder TO UPPER CASE.

    " fetch the head commit ID for given branch
    rv_success = me->get_commit_ado(
        EXPORTING
            iv_branch = iv_branch
        IMPORTING
            ev_commitid = lv_commitid
             ).
    CHECK rv_success = abap_true.

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

        DATA(lv_code_name) = me->build_code_name( lv_commit_object ).
        DATA(lv_filepath) = |{ lv_rootfolder }{ lv_commit_object-devclass }/{ lv_code_name }|.

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

    " update sync status file with the TR id
    me->build_sync_status(
        EXPORTING
            iv_mode = c_latest_version
            iv_trid = iv_trid
        IMPORTING
            ev_filecontent = lv_syncfilecontent
             ).
    DATA(lv_syncstatuspath) = |{ lv_rootfolder }{ c_sync_status_file }|.
    me->build_push_json(
        EXPORTING
            iv_filename = lv_syncstatuspath
            iv_filecontent = lv_syncfilecontent
            iv_changetype = 2
        CHANGING
            iv_commit = lv_commit
             ).

    lv_commit-comment = iv_comment.

    me->write_telemetry( iv_message = |{ lv_synccnt } objects to push for TR { iv_trid }| iv_kind = 'info' ).

    " push the changes to Git by ADO REST call
    rv_success = me->push_ado(
        EXPORTING
            iv_branch = iv_branch
            iv_commit = lv_commit
            iv_commitid = lv_commitid
             ).

  ENDMETHOD.

  METHOD CATCHUP_TRS.

    DATA lv_content TYPE string.
    DATA lv_sync_status TYPE ts_sync_status.
    DATA lt_trids TYPE tty_trids.
    DATA lt_commit_objects TYPE tty_commit_object.
    DATA lv_comment TYPE string.
    DATA lv_rootfolder TYPE string.

    lv_rootfolder = iv_rootfolder.
    TRANSLATE lv_rootfolder TO UPPER CASE.
    DATA(lv_itempath) = |{ lv_rootfolder }{ c_sync_status_file }|.

    " fetch content of shared sync status file
    rv_success = me->get_item_ado(
        EXPORTING
            iv_branch = iv_branch
            iv_itempath = lv_itempath
        IMPORTING
            ev_content = lv_content
             ).
    CHECK rv_success = abap_true.

    " extract last sync TR ID
     rv_success = me->load_sync_status(
        EXPORTING
            iv_filecontent = lv_content
        IMPORTING
            ev_sync_status = lv_sync_status
             ).
    CHECK rv_success = abap_true.

    " sync-ed in active mode can't be caught up since it's sync-ed to active/latest version of objects instead of TR
    IF lv_sync_status-mode = c_active_version.
        me->write_telemetry( iv_message = |branch { iv_branch } is sync-ed in active mode| ).
        rv_success = abap_false.
        EXIT.
    ENDIF.

    " fetch TR IDs to sync since last one
    me->get_trs( EXPORTING iv_fromtrid = lv_sync_status-trid IMPORTING et_trids = lt_trids ).

    " push each TR ID to Git repo
    LOOP AT lt_trids INTO DATA(watrid).
        CLEAR lt_commit_objects.
        rv_success = me->get_tr_commit_objects(
            EXPORTING
                iv_trid = watrid
                iv_packagenames = iv_packagenames
            IMPORTING
                ev_comment = lv_comment
            CHANGING
                it_commit_objects = lt_commit_objects
                 ).
        CHECK rv_success = abap_true.
        rv_success = me->push_tr_commit_objects(
            EXPORTING
                iv_trid = watrid
                iv_branch = iv_branch
                iv_comment = lv_comment
                iv_rootfolder = iv_rootfolder
                it_commit_objects = lt_commit_objects
                 ).
        CHECK rv_success = abap_true.
        me->write_telemetry( iv_message = |caught up TR { watrid }| iv_kind = 'info' ).
    ENDLOOP.

  ENDMETHOD.

  METHOD PREPARE_LANDSCAPE_BRANCH.
    " main branch name like users/ci/<system id>
    ev_branch = |{ iv_prefix }{ sy-sysid }|.
  ENDMETHOD.

  METHOD PREPARE_CI_BRANCH.

    DATA ld_cs_request TYPE TRWBO_REQUEST.
    DATA lv_trkorr TYPE TRKORR.
    DATA lv_frombranch TYPE string.
    DATA lv_commitid TYPE string.

    rv_success = abap_true.

    " fetch attributes of TR
    lv_trkorr = iv_trid.
    CALL FUNCTION 'TR_READ_REQUEST'
      EXPORTING
        iv_read_e070 =               abap_true
        iv_read_e07t =               abap_true
        iv_read_e070c =              abap_true
        iv_read_e070m =              abap_true
        iv_read_objs_keys =          abap_false
        iv_read_attributes =         abap_true
        iv_trkorr =                  lv_trkorr
      CHANGING
        cs_request =                 ld_cs_request
      EXCEPTIONS
        ERROR_OCCURED =              1
        NO_AUTHORIZATION =           2.
    IF sy-subrc <> 0.
        me->write_telemetry( iv_message = |PREPARE_CI_BRANCH fails to call TR_READ_REQUEST subrc { sy-subrc }| ).
        rv_success = abap_false.
        EXIT.
    ENDIF.

    " private branch name like users/<alias>/<TR id>
    ev_branch = |{ iv_ciprefix }{ ld_cs_request-h-as4user }/{ iv_trid }|.

    " from branch as "main" branch for the system
    " to branch as private branch for the user and TR
    me->prepare_landscape_branch( EXPORTING iv_prefix = iv_baseprefix IMPORTING ev_branch = lv_frombranch ).
    rv_success = me->get_commit_ado(
        EXPORTING
            iv_branch = lv_frombranch
        IMPORTING
            ev_commitid = lv_commitid ).
    CHECK rv_success = abap_true.

    rv_success = me->create_branch_ado(
        EXPORTING
            iv_branch = ev_branch
            iv_commitid = lv_commitid
            ).

  ENDMETHOD.

  METHOD CLEANUP_CI_BRANCH.

    DATA ld_cs_request TYPE TRWBO_REQUEST.
    DATA lv_trkorr TYPE TRKORR.
    DATA lv_commitid TYPE string.

    " fetch attributes of TR
    lv_trkorr = iv_trid.
    CALL FUNCTION 'TR_READ_REQUEST'
      EXPORTING
        iv_read_e070 =               abap_true
        iv_read_e07t =               abap_true
        iv_read_e070c =              abap_true
        iv_read_e070m =              abap_true
        iv_read_objs_keys =          abap_false
        iv_read_attributes =         abap_true
        iv_trkorr =                  lv_trkorr
      CHANGING
        cs_request =                 ld_cs_request
      EXCEPTIONS
        ERROR_OCCURED =              1
        NO_AUTHORIZATION =           2.
    IF sy-subrc <> 0.
        me->write_telemetry( iv_message = |CLEANUP_CI_BRANCH fails to call TR_READ_REQUEST subrc { sy-subrc }| ).
        rv_success = abap_false.
        EXIT.
    ENDIF.

    DATA(lv_branch) = |{ iv_prefix }{ ld_cs_request-h-as4user }/{ iv_trid }|.

    " commit ID of the private branch needed for ADO REST API
    rv_success = me->get_commit_ado(
        EXPORTING
            iv_branch = lv_branch
        IMPORTING
            ev_commitid = lv_commitid
             ).
    CHECK rv_success = abap_true.

    rv_success = me->delete_branch_ado(
        EXPORTING
            iv_branch = lv_branch
            iv_commitid = lv_commitid
            ).

  ENDMETHOD.

  METHOD KICKOFF_PIPELINE_RUN_ADO.
    " https://learn.microsoft.com/en-us/rest/api/azure/devops/pipelines/runs/run-pipeline?view=azure-devops-rest-7.1
    DATA(runPath) = |{ me->orgid }/{ me->project }/_apis/pipelines/{ iv_pipelineid }/runs?api-version=7.1-preview.1|.
    DATA(lv_branch) = |{ c_head }{ iv_branch }|.
    DATA lv_status TYPE i.
    DATA lt_ret_data TYPE /ui5/cl_json_parser=>t_entry_map.
    DATA ls_json_req TYPE ts_run_json_req.
    ls_json_req-resources-repositories-self-refName = lv_branch.
    me->HTTP_POST_JSON(
        EXPORTING
            iv_path = runPath
            iv_username = me->username
            iv_pat = me->pat
            iv_json = ls_json_req
        IMPORTING
            ev_status = lv_status
            et_entry_map = lt_ret_data
             ).
    IF lv_status < 200 OR lv_status >= 300.
        me->write_telemetry( iv_message = |KICKOFF_PIPELINE_RUN_ADO fails to create a run for branch { lv_branch }| ).
        rv_success = abap_false.
        EXIT.
    ENDIF.
    rv_success = abap_true.
    ev_runid = lt_ret_data[ name = 'id' ]-value.
  ENDMETHOD.

  METHOD GET_PACKAGE_CODES.

    DATA wacode TYPE ts_code_object.
    DATA lt_codes TYPE STANDARD TABLE OF ts_code_object.
    DATA lt_fmcodes TYPE STANDARD TABLE OF ts_code_object.
    DATA lt_fgfmcodes TYPE STANDARD TABLE OF ts_code_object.
    DATA lt_objversions TYPE tty_version_no.
    DATA lv_objname TYPE versobjnam.
    DATA lv_objtype TYPE versobjtyp.
    DATA lv_objtype2 TYPE versobjtyp.
    DATA lv_fugrname TYPE versobjnam.
    DATA lv_basefolder TYPE string.
    DATA lt_filecontent TYPE tty_abaptext.
    DATA lt_tclsfilecontent TYPE tty_abaptext.
    DATA lv_tclsname TYPE string.
    DATA lv_tclstype TYPE string.
    DATA lv_commit_object TYPE ts_commit_object.
    DATA lv_code_name TYPE string.
    DATA lv_path TYPE string.
    DATA lv_folder TYPE RLGRAP-FILENAME.
    DATA lv_success TYPE string.
    DATA lv_objname3 TYPE e071-obj_name.
    DATA lv_version_no TYPE i.

    IF iv_folder NP '*\'.
        lv_basefolder = iv_folder && '\'.
    ELSE.
        lv_basefolder = iv_folder.
    ENDIF.

    " root folder for a package like \src\<branch name>\
    lv_folder = |{ lv_basefolder }{ iv_package }\\|.
    CALL FUNCTION 'GUI_CREATE_DIRECTORY'
        EXPORTING
            dirname = lv_folder
        EXCEPTIONS
            FAILED = 1.
    IF sy-subrc <> 0.
        me->write_telemetry( iv_message = |GET_PACKAGE_CODES fails to create folder { lv_folder }| ).
    ENDIF.

    " collect FMs/includes in function groups into list of code objects to download
    SELECT obj_name, object, object, ' ' INTO TABLE @lt_codes FROM tadir WHERE devclass = @iv_package AND pgmid = 'R3TR'.
    LOOP AT lt_codes INTO wacode.
        lv_objtype = wacode-obj_type.
        IF lv_objtype = 'FUGR'.
            lv_objname = wacode-obj_name.
            " find function modules in the function group
            DATA(pname_filter) = 'SAPL' && lv_objname.
            CLEAR lt_fgfmcodes.
            SELECT funcname, 'FUNC', 'FUNC', @lv_objname INTO TABLE @lt_fgfmcodes FROM tfdir WHERE pname = @pname_filter.
            APPEND LINES OF lt_fgfmcodes TO lt_fmcodes.
            " find includes in the function group
            pname_filter = 'L' && lv_objname && '%'.
            CLEAR lt_fgfmcodes.
            SELECT name, 'REPS', 'FUNC', @lv_objname INTO TABLE @lt_fgfmcodes FROM trdir WHERE name LIKE @pname_filter.
            APPEND LINES OF lt_fgfmcodes TO lt_fmcodes.
        ENDIF.
    ENDLOOP.
    APPEND LINES OF lt_fmcodes TO lt_codes.

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
        lv_success = me->get_versions_no(
            EXPORTING
                iv_objname = lv_objname3
                iv_objtype = lv_objtype
                iv_mode = iv_mode
            IMPORTING
                ev_version_no = lv_version_no
            CHANGING
                it_objversions = lt_objversions
                ).
        " the object may not be ABAP code thus not versions fetched
        CHECK lv_success = abap_true.
        CHECK lines( lt_objversions ) > 0.

        " construct object content (and test class content if any) as source code lines
        CLEAR lt_filecontent.
        CLEAR lt_tclsfilecontent.
        lv_success = me->build_code_content(
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

        " save the object content to local disk file
        lv_commit_object-objname = lv_objname3.
        lv_commit_object-objtype = lv_objtype.
        lv_commit_object-objtype2 = lv_objtype2.
        lv_commit_object-fugr = lv_fugrname.
        lv_code_name = me->build_code_name( lv_commit_object ).
        lv_path = |{ lv_basefolder }{ iv_package }\\{ lv_code_name }|.
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

        " save the object content to local disk file for test class if any
        IF lines( lt_tclsfilecontent ) > 0.
            " following abapGit where class name is used for test class name instead of ====CCAU like one
            lv_commit_object-objname = lv_objname3.
            lv_commit_object-objtype = lv_tclstype.
            lv_code_name = me->build_code_name( lv_commit_object ).
            lv_path = |{ lv_basefolder }{ iv_package }\\{ lv_code_name }|.
            CALL FUNCTION 'GUI_DOWNLOAD'
                  EXPORTING
                    filename = lv_path
                    filetype = 'ASC'
                    write_field_separator = 'X'
                  TABLES
                    data_tab = lt_tclsfilecontent.
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
        lv_success = me->GET_PACKAGE_CODES(
            EXPORTING
                iv_package = lv_package
                iv_folder = lv_basefolder
                iv_mode = iv_mode
                 ).
        IF lv_success <> abap_true.
            me->write_telemetry( iv_message = |GET_PACKAGES_CODES has failure in package { lv_package }| ).
            rv_success = abap_false.
        ENDIF.
    ENDLOOP.

    " mark down status of last sync TR ID and update date time
    DATA(lv_file) = |{ lv_basefolder }{ c_sync_status_file }|.
    rv_success = me->SAVE_SYNC_STATUS( EXPORTING iv_mode = iv_mode iv_file = lv_file ).

  ENDMETHOD.

  METHOD GET_CUSTOMIZE_PACKAGES_CODES.

    DATA lv_basefolder TYPE string.
    DATA lv_package TYPE devclass.

    IF iv_folder NP '*\'.
        lv_basefolder = iv_folder && '\'.
    ELSE.
        lv_basefolder = iv_folder.
    ENDIF.

    DATA lv_success TYPE string.
    DATA lt_packages TYPE STANDARD TABLE OF devclass.

    " fetch customization packages with initial Y or Z
    SELECT devclass INTO TABLE lt_packages FROM tdevc WHERE devclass LIKE 'Z%' OR devclass LIKE 'Y%'.

    " save objects in each package collected under the folder named as package name like \src\<package name>\
    LOOP AT lt_packages INTO DATA(wa).
        lv_package = wa.
        lv_success = me->GET_PACKAGE_CODES(
            EXPORTING
                iv_package = lv_package
                iv_folder = lv_basefolder
                iv_mode = iv_mode
                 ).
        IF lv_success <> abap_true.
            me->write_telemetry( iv_message = |GET_CUSTOMIZE_PACKAGES_CODES has failure in package { lv_package }| ).
            rv_success = abap_false.
        ENDIF.
    ENDLOOP.

    " mark down status of last sync TR ID and update date time
    DATA(lv_file) = |{ lv_basefolder }{ c_sync_status_file }|.
    rv_success = me->SAVE_SYNC_STATUS( EXPORTING iv_mode = iv_mode iv_file = lv_file ).

  ENDMETHOD.

  METHOD GET_FUGR.

    DATA lv_pname_filter TYPE string.
    SELECT SINGLE pname INTO @lv_pname_filter FROM tfdir WHERE funcname = @iv_objname.
    ev_fugrname = lv_pname_filter+4.    " SAPL... as prefix for names in pname field

  ENDMETHOD.

  METHOD GET_CODE_LINES.

      DATA: lv_no_release_transformation TYPE svrs_bool.
      DATA: trdir_new TYPE TABLE OF TRDIR INITIAL SIZE 1.
      DATA: smodilog_new TYPE table of smodilog.

      linecount = 0.

      IF iv_logdest = space OR iv_logdest = 'NONE'.
        lv_no_release_transformation = abap_true.
      ELSE.
        lv_no_release_transformation = abap_false.
      ENDIF.

      CALL FUNCTION 'SVRS_GET_REPS_FROM_OBJECT'
        EXPORTING
          destination                  = iv_logdest
          object_name                  = iv_objname
          object_type                  = iv_objtype
          versno                       = iv_version
          iv_no_release_transformation = lv_no_release_transformation
        TABLES
          repos_tab                    = abaptext
          trdir_tab                    = trdir_new
          vsmodilog                    = smodilog_new
        EXCEPTIONS
          no_version                   = 01.
      IF sy-subrc <> 0.
         me->write_telemetry( iv_message = |GET_CODE_LINES fails in fetching code lines for { iv_objname } type { iv_objtype } version { iv_version }| ).
         rv_success = abap_false.
         EXIT.
      ENDIF.

      linecount = lines( abaptext ).
      rv_success = abap_true.

  ENDMETHOD.

  METHOD BUILD_CODE_CONTENT.

    DATA lt_abaptext TYPE tty_abaptext.
    DATA lv_firstmethod TYPE c VALUE abap_true.
    DATA lv_linecount TYPE i.
    DATA lv_success TYPE string.
    DATA lv_hasmethod TYPE c VALUE abap_false.
    FIELD-SYMBOLS <fstxt> TYPE ABAPTXT255.
    DATA: textline TYPE string,
          abaptextline TYPE string.
    DATA: tclstextline TYPE string,
          tclsabaptextline TYPE string.

    rv_success = abap_true.

    " fetch code lines for each version object
    " for class object there may be multiple for methods, test class, public/protected/private sections
    " others only one
    LOOP AT it_objversions INTO DATA(waver).

        CLEAR lt_abaptext.

        lv_success = me->get_code_lines(
            EXPORTING
                iv_version = waver-objversionno
                iv_objname = waver-objname
                iv_objtype = waver-objtype
                iv_logdest = ''
            IMPORTING
                linecount = lv_linecount
                abaptext = lt_abaptext
                ).
        IF lv_success = abap_false.
            rv_success = abap_false.
            EXIT.
        ENDIF.

        IF iv_objtype = 'CLAS'.
            " methods are added to class implementation clause
            IF waver-objtype = 'METH'.
                lv_hasmethod = abap_true.
                IF lv_firstmethod = abap_true.
                    textline = |ENDCLASS.|.
                    APPEND textline TO et_filecontent.
                    textline = ''.
                    APPEND textline TO et_filecontent.
                    textline = ''.
                    APPEND textline TO et_filecontent.
                    textline = |CLASS { iv_objname } IMPLEMENTATION.|.
                    APPEND textline TO et_filecontent.
                    textline = ''.
                    APPEND textline TO et_filecontent.
                ENDIF.
                lv_firstmethod = abap_false.
                LOOP AT lt_abaptext ASSIGNING <fstxt>.
                    abaptextline = <fstxt>.
                    textline = |{ abaptextline }|.
                    APPEND textline TO et_filecontent.
                ENDLOOP.
                textline = ''.
                APPEND textline TO et_filecontent.
            ELSEIF waver-objtype = 'CINC'.
                " test class is a stand-alone file to save
                ev_tclsname = waver-objname.
                ev_tclstype = waver-objtype.
                LOOP AT lt_abaptext ASSIGNING <fstxt>.
                    tclsabaptextline = <fstxt>.
                    tclstextline = |{ tclsabaptextline }|.
                    APPEND tclstextline TO et_tclsfilecontent.
                ENDLOOP.
            ELSE.
                LOOP AT lt_abaptext ASSIGNING <fstxt>.
                    abaptextline = <fstxt>.
                    textline = |{ abaptextline }|.
                    APPEND textline TO et_filecontent.
                ENDLOOP.
            ENDIF.
        ELSE.
            LOOP AT lt_abaptext ASSIGNING <fstxt>.
                abaptextline = <fstxt>.
                textline = |{ abaptextline }|.
                APPEND textline TO et_filecontent.
            ENDLOOP.
        ENDIF.
    ENDLOOP.

    CHECK rv_success = abap_true.

    " auto padding for class object at the end
    IF iv_objtype = 'CLAS'.
        textline = |ENDCLASS.|.
        APPEND textline TO et_filecontent.

        " class has no method lines, but class implementation portion should still be added.
        IF lv_hasmethod = abap_false.
            textline = ''.
            APPEND textline TO et_filecontent.
            textline = |CLASS { iv_objname } IMPLEMENTATION.|.
            APPEND textline TO et_filecontent.
            textline = |ENDCLASS.|.
            APPEND textline TO et_filecontent.
        ENDIF.
    ENDIF.

  ENDMETHOD.

  METHOD BUILD_CODE_NAME.
    IF iv_commit_object-objtype = 'FUNC' OR iv_commit_object-objtype2 = 'FUNC'.
        " object in function group named as <function group name>.fugr.<object name>.abap, following abapGit
        rv_name = |{ iv_commit_object-fugr }.fugr.{ iv_commit_object-objname }.abap|.
    ELSEIF iv_commit_object-objtype = 'CINC'.
        " test class named as <class name>.clas.testclasses.abap, following abapGit
        rv_name = |{ iv_commit_object-objname }.clas.testclasses.abap|.
    ELSE.
        " others named as <object name>.<object type, PROG|CLAS|INTF|...>.abap, following abapGit
        rv_name = |{ iv_commit_object-objname }.{ iv_commit_object-objtype }.abap|.
    ENDIF.
  ENDMETHOD.

  METHOD GET_VERSIONS_NO.

    DATA wa_ver TYPE VRSD.
    DATA lt_vers TYPE vrsd_tab.
    DATA wa_objversion TYPE ts_version_no.
    DATA lv_versno TYPE versno.

    IF iv_objtype = 'FUNC'.
        " function module case
        CLEAR lt_vers.
        rv_success = me->get_versions(
            EXPORTING
                iv_objname = iv_objname
                iv_objtype = 'FUNC'
            CHANGING
                it_vers = lt_vers
                 ).
        CHECK rv_success = abap_true.
        rv_success = me->get_valued_version(
            EXPORTING
                iv_mode = iv_mode
                iv_date = iv_date
                iv_time = iv_time
            IMPORTING
                ev_versno = lv_versno
                ev_verscnt = ev_version_no
            CHANGING
                cht_vers = lt_vers
                 ).
        IF rv_success = abap_true AND it_objversions IS SUPPLIED.
            wa_objversion-objversionno = lv_versno.
            wa_objversion-objname = iv_objname.
            wa_objversion-objtype = 'FUNC'.
            APPEND wa_objversion TO it_objversions.
        ENDIF.
    ELSEIF iv_objtype = 'CLAS'.
        " class object case, multiple versions may return including public/protected/private sections and methods
        ev_version_no = me->get_class_versions_no(
            EXPORTING
                iv_objname = iv_objname
                iv_objtype = iv_objtype
                iv_mode = iv_mode
                iv_date = iv_date
                iv_time = iv_time
            CHANGING
                it_objversions = it_objversions
                 ).
        rv_success = abap_true.
    ELSEIF iv_objtype = 'PROG'.
        " program/include case
        CLEAR lt_vers.
        rv_success = me->get_versions(
            EXPORTING
                iv_objname = iv_objname
                iv_objtype = 'REPS'
            CHANGING
                it_vers = lt_vers
                 ).
        CHECK rv_success = abap_true.
        rv_success = me->get_valued_version(
            EXPORTING
                iv_mode = iv_mode
                iv_date = iv_date
                iv_time = iv_time
            IMPORTING
                ev_versno = lv_versno
                ev_verscnt = ev_version_no
            CHANGING
                cht_vers = lt_vers
                 ).
        IF rv_success = abap_true AND it_objversions IS SUPPLIED.
            wa_objversion-objversionno = lv_versno.
            wa_objversion-objname = iv_objname.
            wa_objversion-objtype = 'REPS'.
            APPEND wa_objversion TO it_objversions.
        ENDIF.
    ELSEIF iv_objtype = 'REPS'.
        " include case
        CLEAR lt_vers.
        rv_success = me->get_versions(
            EXPORTING
                iv_objname = iv_objname
                iv_objtype = 'REPS'
            CHANGING
                it_vers = lt_vers
                 ).
        CHECK rv_success = abap_true.
        rv_success = me->get_valued_version(
            EXPORTING
                iv_mode = iv_mode
                iv_date = iv_date
                iv_time = iv_time
            IMPORTING
                ev_versno = lv_versno
                ev_verscnt = ev_version_no
            CHANGING
                cht_vers = lt_vers
                 ).
        IF rv_success = abap_true AND it_objversions IS SUPPLIED.
            wa_objversion-objversionno = lv_versno.
            wa_objversion-objname = iv_objname.
            wa_objversion-objtype = 'REPS'.
            APPEND wa_objversion TO it_objversions.
        ENDIF.
    ELSEIF iv_objtype = 'INTF'.
        " interface case
        CLEAR lt_vers.
        rv_success = me->get_versions(
            EXPORTING
                iv_objname = iv_objname
                iv_objtype = 'INTF'
            CHANGING
                it_vers = lt_vers
                 ).
        CHECK rv_success = abap_true.
        rv_success = me->get_valued_version(
            EXPORTING
                iv_mode = iv_mode
                iv_date = iv_date
                iv_time = iv_time
            IMPORTING
                ev_versno = lv_versno
                ev_verscnt = ev_version_no
            CHANGING
                cht_vers = lt_vers
                 ).
        IF rv_success = abap_true AND it_objversions IS SUPPLIED.
            wa_objversion-objversionno = lv_versno.
            wa_objversion-objname = iv_objname.
            wa_objversion-objtype = 'INTF'.
            APPEND wa_objversion TO it_objversions.
        ENDIF.
    ELSE.
        rv_success = abap_false.
        ev_version_no = 0.
    ENDIF.

  ENDMETHOD.

  METHOD GET_VALUED_VERSION.

    DATA lv_mode TYPE string.
    lv_mode = iv_mode.
    TRANSLATE lv_mode TO LOWER CASE.

    SORT cht_vers DESCENDING BY versno.

    IF iv_date IS SUPPLIED AND iv_time IS SUPPLIED AND iv_date IS NOT INITIAL AND iv_time IS NOT INITIAL.
        " remove version later than given time, keep active version
        DELETE cht_vers WHERE versno <> 0 AND ( datum > iv_date OR ( datum = iv_date AND zeit > iv_time ) ).
    ENDIF.

    IF lines( cht_vers ) = 0.
        rv_success = abap_false.
        EXIT.
    ENDIF.

    IF lv_mode = c_latest_version.
        " exclude active version
        DELETE cht_vers WHERE versno = 0.
        ev_verscnt = lines( cht_vers ).
        IF ev_verscnt = 0.
            " no latest released version available
            rv_success = abap_false.
            EXIT.
        ELSE.
            " at least one released version available, use it
            ev_versno = cht_vers[ 1 ]-versno.
            rv_success = abap_true.
        ENDIF.
    ELSEIF lv_mode = c_active_version.
        ev_verscnt = lines( cht_vers ).
        IF line_exists( cht_vers[ versno = 0 ] ).
            " active version available, use it
            ev_versno = 0.
            rv_success = abap_true.
        ELSE.
            " use latest released version since active one unavailable
            ev_versno = cht_vers[ 1 ]-versno.
            rv_success = abap_true.
        ENDIF.
    ELSE.
        me->write_telemetry( iv_message = |invalid mode { iv_mode }| ).
        rv_success = abap_false.
    ENDIF.

  ENDMETHOD.

  METHOD GET_CLASS_VERSIONS_NO.

    DATA lv_versno TYPE versno.
    DATA lv_verscnt TYPE i.
    DATA lv_objname TYPE e071-obj_name.
    DATA lv_classname TYPE classname.
    DATA lt_methods TYPE abap_methdescr_tab.
    DATA lv_methodname TYPE versobjnam.
    DATA wa_ver TYPE VRSD.
    DATA lt_vers TYPE vrsd_tab.
    DATA wa_objversion TYPE ts_version_no.
    FIELD-SYMBOLS <fsmethod> LIKE LINE OF lt_methods.
    DATA lv_CLSKEY TYPE SEOCLSKEY.
    DATA lv_LIMU TYPE SEOK_LIMU.
    DATA lv_INCTYPE TYPE CHAR3.
    DATA lv_progm TYPE PROGRAMM.
    DATA lv_testclassname TYPE versobjnam.
    DATA lv_success TYPE string.

    lv_classname = iv_objname.
    r_version_no = 1.

    " public section's version
    CLEAR lt_vers.
    me->get_versions(
        EXPORTING
            iv_objname = iv_objname
            iv_objtype = 'CPUB'
        CHANGING
            it_vers = lt_vers
             ).
    lv_success = me->get_valued_version(
        EXPORTING
            iv_mode = iv_mode
            iv_date = iv_date
            iv_time = iv_time
        IMPORTING
            ev_versno = lv_versno
            ev_verscnt = lv_verscnt
        CHANGING
            cht_vers = lt_vers
             ).
    IF lv_success = abap_true AND it_objversions IS SUPPLIED.
        wa_objversion-objversionno = lv_versno.
        wa_objversion-objname = iv_objname.
        wa_objversion-objtype = 'CPUB'.
        APPEND wa_objversion TO it_objversions.
    ENDIF.
    IF lv_verscnt > r_version_no.
        r_version_no = lv_verscnt.
    ENDIF.

    " protected section's version
    CLEAR lt_vers.
    me->get_versions(
        EXPORTING
            iv_objname = iv_objname
            iv_objtype = 'CPRT'
        CHANGING
            it_vers = lt_vers
             ).
    lv_success = me->get_valued_version(
        EXPORTING
            iv_mode = iv_mode
            iv_date = iv_date
            iv_time = iv_time
        IMPORTING
            ev_versno = lv_versno
            ev_verscnt = lv_verscnt
        CHANGING
            cht_vers = lt_vers
             ).
    IF lv_success = abap_true AND it_objversions IS SUPPLIED.
        wa_objversion-objversionno = lv_versno.
        wa_objversion-objname = iv_objname.
        wa_objversion-objtype = 'CPRT'.
        APPEND wa_objversion TO it_objversions.
    ENDIF.
    IF lv_verscnt > r_version_no.
        r_version_no = lv_verscnt.
    ENDIF.

    " private section's version
    CLEAR lt_vers.
    me->get_versions(
        EXPORTING
            iv_objname = iv_objname
            iv_objtype = 'CPRI'
        CHANGING
            it_vers = lt_vers
             ).
    lv_success = me->get_valued_version(
        EXPORTING
            iv_mode = iv_mode
            iv_date = iv_date
            iv_time = iv_time
        IMPORTING
            ev_versno = lv_versno
            ev_verscnt = lv_verscnt
        CHANGING
            cht_vers = lt_vers
             ).
    IF lv_success = abap_true AND it_objversions IS SUPPLIED.
        wa_objversion-objversionno = lv_versno.
        wa_objversion-objname = iv_objname.
        wa_objversion-objtype = 'CPRI'.
        APPEND wa_objversion TO it_objversions.
    ENDIF.
    IF lv_verscnt > r_version_no.
        r_version_no = lv_verscnt.
    ENDIF.

    " each method's version
    me->get_class_methods(
        EXPORTING
            iv_classname = lv_classname
        CHANGING
            cht_methods = lt_methods
             ).
    LOOP AT lt_methods ASSIGNING <fsmethod>.
        lv_methodname(30) = lv_classname.
        lv_methodname+30  = <fsmethod>-name.
        CLEAR lt_vers.
        lv_objname = lv_methodname.
        me->get_versions(
            EXPORTING
                iv_objname = lv_objname
                iv_objtype = 'METH'
            CHANGING
                it_vers = lt_vers
                 ).
        lv_success = me->get_valued_version(
            EXPORTING
                iv_mode = iv_mode
                iv_date = iv_date
                iv_time = iv_time
            IMPORTING
                ev_versno = lv_versno
                ev_verscnt = lv_verscnt
            CHANGING
                cht_vers = lt_vers
                 ).
        IF lv_success = abap_true AND it_objversions IS SUPPLIED.
            wa_objversion-objversionno = lv_versno.
            wa_objversion-objname = lv_methodname.
            wa_objversion-objtype = 'METH'.
            APPEND wa_objversion TO it_objversions.
        ENDIF.
        IF lv_verscnt > r_version_no.
            r_version_no = lv_verscnt.
        ENDIF.
    ENDLOOP.

    " test class of the class if any
    lv_CLSKEY = lv_classname.
    lv_LIMU = 'CINC'.
    lv_INCTYPE = 'AU'.
    CALL FUNCTION 'SEO_CLASS_GET_INCLUDE_BY_NAME'
        EXPORTING
            clskey = lv_CLSKEY
            limu = lv_LIMU
            inctype = lv_INCTYPE
        IMPORTING
            progname = lv_progm.

    lv_testclassname = lv_progm.

    lv_objname = lv_testclassname.
    CLEAR lt_vers.
    me->get_versions(
        EXPORTING
            iv_objname = lv_objname
            iv_objtype = 'CINC'
        CHANGING
            it_vers = lt_vers
             ).
    lv_success = me->get_valued_version(
        EXPORTING
            iv_mode = iv_mode
            iv_date = iv_date
            iv_time = iv_time
        IMPORTING
            ev_versno = lv_versno
            ev_verscnt = lv_verscnt
        CHANGING
            cht_vers = lt_vers
             ).
    IF lv_success = abap_true AND it_objversions IS SUPPLIED.
        wa_objversion-objversionno = lv_versno.
        wa_objversion-objname = lv_objname.
        wa_objversion-objtype = 'CINC'.
        APPEND wa_objversion TO it_objversions.
    ENDIF.
    IF lv_verscnt > r_version_no.
        r_version_no = lv_verscnt.
    ENDIF.

  ENDMETHOD.

  METHOD GET_CLASS_METHODS.

    DATA: lcl_obj TYPE REF TO cl_abap_objectdescr,
          lp_descr_ref TYPE REF TO CL_ABAP_TYPEDESCR.
    TRY.
        cl_abap_objectdescr=>describe_by_name(
            EXPORTING
                p_name = iv_classname
            RECEIVING
                P_DESCR_REF = lp_descr_ref
            EXCEPTIONS
                TYPE_NOT_FOUND = 1
                ).
        IF sy-subrc = 0.
            lcl_obj ?= lp_descr_ref.
            cht_methods = lcl_obj->methods.
        ENDIF.
    CATCH CX_SY_RTTI_SYNTAX_ERROR.
        me->write_telemetry( iv_message = |GET_CLASS_METHODS fails in fetching class methods for class { iv_classname }| ).
    ENDTRY.

  ENDMETHOD.

  METHOD GET_VERSIONS.

      DATA: lv_rfcdest TYPE RFCDEST,
            lt_versno TYPE STANDARD TABLE OF vrsn,
            lt_text_vers TYPE TABLE OF e07t,
            lt_vers_obj  TYPE TABLE OF vrsd,
            lt_text_obj TYPE TABLE OF e07t,
            lv_objname TYPE versobjnam,
            lv_objtype TYPE versobjtyp.

      lv_objname = iv_objname.
      lv_objtype = iv_objtype.

      CALL FUNCTION 'SVRS_GET_VERSION_DIRECTORY_46'
        EXPORTING
          destination  = lv_rfcdest
          objtype      = lv_objtype
          objname      = lv_objname
        TABLES
          lversno_list = lt_versno
          version_list = it_vers
        EXCEPTIONS
          no_entry     = 1
          OTHERS       = 2.
      IF sy-subrc <> 0.
        " a class method may not have entry thus sy-subrc as 1
        " other exceptions should be reported
        IF sy-subrc <> 1.
            me->write_telemetry( iv_message = |GET_VERSIONS fails with { lv_objname } { lv_objtype } subrc { sy-subrc }| ).
            rv_success = abap_false.
            EXIT.
        ENDIF.
      ENDIF.

      CALL FUNCTION 'GET_E07T_DATA_46'
        EXPORTING
          destination           = lv_rfcdest
          mode                  = 'V'
        TABLES
          version_list          = it_vers
          e07t_vrs              = lt_text_vers
          e07t_obj              = lt_text_obj
          object_list           = lt_vers_obj
        EXCEPTIONS
          system_failure        = 1
          communication_failure = 2.
      IF sy-subrc <> 0.
        me->write_telemetry( iv_message = |GET_VERSIONS fails in calling GET_E07T_DATA_46 subrc { sy-subrc }| ).
        rv_success = abap_false.
        EXIT.
      ENDIF.

      rv_success = abap_true.

  ENDMETHOD.

  METHOD GET_TRS.

    IF iv_fromtrid IS SUPPLIED.
        " fetch TRs later than given released TR
        DATA lv_dat TYPE d.
        DATA lv_tim TYPE t.
        SELECT SINGLE as4date INTO lv_dat FROM e070 WHERE trkorr = iv_fromtrid.
        SELECT SINGLE as4time INTO lv_tim FROM e070 WHERE trkorr = iv_fromtrid.
        SELECT trkorr INTO TABLE @et_trids FROM e070
            WHERE trfunction = 'K' AND trstatus = 'R' AND ( as4date > @lv_dat OR ( as4date = @lv_dat AND as4time > @lv_tim ) )
            ORDER BY as4date DESCENDING, as4time DESCENDING.
    ELSE.
        " fetch latest released TR
        SELECT trkorr FROM e070 INTO TABLE @et_trids UP TO 1 ROWS
            WHERE trfunction = 'K' AND trstatus = 'R'
            ORDER BY as4date DESCENDING, as4time DESCENDING.
    ENDIF.

  ENDMETHOD.

  METHOD BUILD_SYNC_STATUS.
    DATA lv_sync_status TYPE ts_sync_status.
    DATA lt_trids TYPE tty_trids.
    DATA lr_json_serializer TYPE REF TO cl_trex_json_serializer.

    IF iv_trid IS SUPPLIED.
        lv_sync_status-trid = iv_trid.
    ELSEIF iv_mode = c_latest_version.
        " in active version mode, it's not sync-ed to latest TR but active/latest version of objects
        me->get_trs( IMPORTING et_trids = lt_trids ).
        lv_sync_status-trid = lt_trids[ 1 ].
    ENDIF.

    lv_sync_status-mode = iv_mode.
    lv_sync_status-updatedate = sy-datum.
    lv_sync_status-updatetime = sy-uzeit.
    CREATE OBJECT lr_json_serializer EXPORTING data = lv_sync_status.
    lr_json_serializer->serialize( ).
    ev_filecontent = lr_json_serializer->get_data( ).
  ENDMETHOD.

  METHOD SAVE_SYNC_STATUS.
    DATA lv_json TYPE string.
    DATA lt_filecontent TYPE TABLE OF string.
    me->build_sync_status( EXPORTING iv_mode = iv_mode IMPORTING ev_filecontent = lv_json ).
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
        me->write_telemetry( iv_message = |SAVE_SYNC_STATUS fails in saving local file subrc { sy-subrc }| ).
        rv_success = abap_false.
    ENDIF.
  ENDMETHOD.

  METHOD LOAD_SYNC_STATUS.
    TRY.
        DATA lo_parse TYPE REF TO /ui5/cl_json_parser.
        CREATE OBJECT lo_parse.
        lo_parse->parse( json = iv_filecontent ).
        DATA(lt_ret_data) = lo_parse->m_entries.
        ev_sync_status-trid = lt_ret_data[ name = 'trid' ]-value.
        ev_sync_status-mode = lt_ret_data[ name = 'mode' ]-value.
        ev_sync_status-updatedate = lt_ret_data[ name = 'updatedate' ]-value.
        ev_sync_status-updatetime = lt_ret_data[ name = 'updatetime' ]-value.
        rv_success = abap_true.
    CATCH /ui5/CX_VFS_ERROR.
        me->write_telemetry( iv_message = 'LOAD_SYNC_STATUS fails to parse sync status file' ).
        rv_success = abap_false.
    ENDTRY.
  ENDMETHOD.

  METHOD GET_ITEM_ADO.
    " https://learn.microsoft.com/en-us/rest/api/azure/devops/git/items/get?view=azure-devops-rest-7.1&tabs=HTTP
    DATA lt_ret_data TYPE /ui5/cl_json_parser=>t_entry_map.
    DATA(itemPath) = |{ me->orgid }/_apis/git/repositories/{ me->repoid }/items?path={ iv_itempath }&includeContent=true&versionDescriptor.version={ iv_branch }&versionDescriptor.versionType=branch&api-version=7.1-preview.1|.
    DATA lv_status TYPE i.
    ev_content = ''.
    me->HTTP_GET_JSON(
        EXPORTING
            iv_path = itemPath
            iv_username = me->username
            iv_pat = me->pat
        IMPORTING
            ev_status = lv_status
            et_entry_map = lt_ret_data
             ).
    rv_success = abap_true.
    IF lv_status < 200 OR lv_status >= 300.
        me->write_telemetry( iv_message = |GET_ITEM_ADO fails to get item content from Git for branch { iv_branch }, path { iv_itempath }| ).
        rv_success = abap_false.
        EXIT.
    ENDIF.
    ev_content = lt_ret_data[ name = 'content' ]-value.
  ENDMETHOD.

  METHOD GET_COMMIT_ADO.
    " https://learn.microsoft.com/en-us/rest/api/azure/devops/git/commits/get?view=azure-devops-rest-7.1&tabs=HTTP
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
        EXIT.
    ENDIF.
    ev_commitId = lt_ret_data[ name = 'commitId' parent = '/value/1' ]-value.
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

  METHOD PUSH_ADO.
    " https://learn.microsoft.com/en-us/rest/api/azure/devops/git/pushes/create?view=azure-devops-rest-7.1&tabs=HTTP
    DATA(lv_branch) = |{ c_head }{ iv_branch }|.
    DATA(createPushPath) = |{ me->orgid }/_apis/git/repositories/{ me->repoid }/pushes?api-version=7.1-preview.2|.
    DATA lv_json_req TYPE ts_push_json_req.
    DATA lv_status TYPE i.
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
             ).
    rv_success = abap_true.
    IF lv_status < 200 OR lv_status >= 300.
        me->write_telemetry( iv_message = |PUSH_ADO fails to push to Git for branch { lv_branch } on top of commit { iv_commitid }| ).
        rv_success = abap_false.
    ENDIF.
  ENDMETHOD.

  METHOD CREATE_BRANCH_ADO.
    " https://learn.microsoft.com/en-us/rest/api/azure/devops/git/refs/update-refs?view=azure-devops-rest-7.1&tabs=HTTP
    DATA(lv_branch) = |{ c_head }{ iv_branch }|.
    DATA(createBranchPath) = |{ me->orgid }/_apis/git/repositories/{ me->repoid }/refs?api-version=7.1-preview.1|.
    DATA lt_json_req TYPE TABLE OF ts_updaterefs_json_req.
    DATA lv_status TYPE i.
    APPEND VALUE ts_updaterefs_json_req( name = lv_branch oldobjectid = c_null_objectid_ref newobjectid = iv_commitid ) TO lt_json_req.
    me->HTTP_POST_JSON(
        EXPORTING
            iv_path = createBranchPath
            iv_username = me->username
            iv_pat = me->pat
            iv_json = lt_json_req
        IMPORTING
            ev_status = lv_status
             ).
    rv_success = abap_true.
    IF lv_status < 200 OR lv_status >= 300.
        me->write_telemetry( iv_message = |CREATE_BRANCH_ADO fails to create branch to Git for branch { lv_branch } on top of commit { iv_commitid }| ).
        rv_success = abap_false.
    ENDIF.
  ENDMETHOD.

  METHOD DELETE_BRANCH_ADO.
    " https://learn.microsoft.com/en-us/rest/api/azure/devops/git/refs/update-refs?view=azure-devops-rest-7.1&tabs=HTTP
    DATA(lv_branch) = |{ c_head }{ iv_branch }|.
    DATA(deleteBranchPath) = |{ me->orgid }/_apis/git/repositories/{ me->repoid }/refs?api-version=7.1-preview.1|.
    DATA lt_json_req TYPE TABLE OF ts_updaterefs_json_req.
    DATA lv_status TYPE i.
    APPEND VALUE ts_updaterefs_json_req( name = lv_branch oldobjectid = iv_commitid newobjectid = c_null_objectid_ref ) TO lt_json_req.
    me->HTTP_POST_JSON(
        EXPORTING
            iv_path = deleteBranchPath
            iv_username = me->username
            iv_pat = me->pat
            iv_json = lt_json_req
        IMPORTING
            ev_status = lv_status
             ).
    rv_success = abap_true.
    IF lv_status < 200 OR lv_status >= 300.
        me->write_telemetry( iv_message = |DELETE_BRANCH_ADO fails to delete branch to Git for branch { lv_branch } on top of commit { iv_commitid }| ).
        rv_success = abap_false.
    ENDIF.
  ENDMETHOD.

  METHOD CREATE_HTTP_CLIENT.

    DATA  lo_http_client TYPE REF TO if_http_client.
    DATA lo_rest_client TYPE REF TO cl_rest_http_client.

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

    " basic auth with user name and password (personal access token) for ADO REST APIs
    DATA(lv_auth) = cl_http_utility=>encode_base64( |{ iv_username }:{ iv_pat }| ).
    lv_auth = |Basic { lv_auth }|.
    lo_http_client->request->set_header_field( name = 'authorization' value = lv_auth ).
    lo_http_client->request->set_header_field( name = 'Accept' value = if_rest_media_type=>gc_appl_json ).
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
    io_rest_client->if_rest_resource~get( ).
    DATA(lo_response) = io_rest_client->if_rest_client~get_response_entity( ).
    ev_status = lo_response->get_header_field( '~status_code' ).
    ev_response = lo_response->get_string_data( ).
  ENDMETHOD.

  METHOD HTTP_POST.
    ii_request->set_string_data( iv_body ).
    io_rest_client->if_rest_resource~post( ii_request ).
    DATA(lo_response) = io_rest_client->if_rest_client~get_response_entity( ).
    ev_status = lo_response->get_header_field( '~status_code' ).
    ev_response = lo_response->get_string_data( ).
  ENDMETHOD.

  METHOD HTTP_GET_JSON.

    DATA li_http_client TYPE REF TO if_http_client.
    DATA lo_rest_client TYPE REF TO cl_rest_http_client.
    DATA li_request TYPE REF TO IF_REST_ENTITY.
    DATA(lv_url) = |{ c_host }{ iv_path }|.
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
        me->write_telemetry( iv_message = |HTTP_GET_JSON HTTP GET for { lv_url } status code { lv_status }, response { lv_response }| ).
        EXIT.
    ENDIF.

    IF et_entry_map IS SUPPLIED.
        CREATE OBJECT lo_parse.
        lo_parse->parse( json = lv_response ).
        et_entry_map = lo_parse->m_entries.
    ENDIF.

  ENDMETHOD.

  METHOD HTTP_POST_JSON.

    DATA lr_json_serializer TYPE REF TO cl_trex_json_serializer.
    DATA li_http_client TYPE REF TO if_http_client.
    DATA lo_rest_client TYPE REF TO cl_rest_http_client.
    DATA li_request TYPE REF TO IF_REST_ENTITY.
    DATA(lv_url) = |{ c_host }{ iv_path }|.
    DATA lv_status TYPE string.
    DATA lv_response TYPE string.
    DATA lo_parse TYPE REF TO /ui5/cl_json_parser.
    DATA lv_statuscode TYPE i.

    CREATE OBJECT lr_json_serializer EXPORTING data = iv_json.
    lr_json_serializer->serialize( ).
    DATA(lv_body) = lr_json_serializer->get_data( ).

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

    me->http_post(
        EXPORTING
            io_rest_client = lo_rest_client
            ii_request = li_request
            iv_body = lv_body
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

    IF lv_statuscode < 200 OR lv_statuscode >= 300.
        me->write_telemetry( iv_message = |HTTP_POST_JSON HTTP POST for { lv_url } status code { lv_status }, response { lv_response }| ).
        EXIT.
    ENDIF.

    IF et_entry_map IS SUPPLIED.
        CREATE OBJECT lo_parse.
        lo_parse->parse( json = lv_response ).
        et_entry_map = lo_parse->m_entries.
    ENDIF.

  ENDMETHOD.

  METHOD WRITE_TELEMETRY.
    IF me->oreftelemetry IS NOT INITIAL AND me->methtelemetry IS NOT INITIAL.
        DATA(oref) = me->oreftelemetry.
        DATA(meth) = me->methtelemetry.
        CALL METHOD oref->(meth)
            EXPORTING
                iv_message = iv_message
                iv_kind = iv_kind.
    ELSE.
        WRITE / |{ iv_kind }: { iv_message }|.
    ENDIF.
  ENDMETHOD.

ENDCLASS.