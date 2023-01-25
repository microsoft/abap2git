" Helper class to talk to SAP transport request and ABAP/config objects operations
CLASS ZCL_UTILITY_ABAPTOGIT_TR DEFINITION
  PUBLIC
  FINAL
  CREATE PUBLIC .

PUBLIC SECTION.

    " latest and active version mode
    CONSTANTS: c_latest_version     TYPE string VALUE 'latest',
               c_active_version     TYPE string VALUE 'active',
               c_schemapcr          TYPE string VALUE '.schemapcr',
               c_config             TYPE string VALUE '.config'.

    " TR info
    TYPES: BEGIN OF ts_change_object,
           name TYPE string,
           type TYPE string,
           END OF ts_change_object.
    TYPES: tty_change_object TYPE TABLE OF ts_change_object WITH KEY name.
    TYPES: BEGIN OF ts_tr_info,
           id       TYPE string,
           owner    TYPE string,
           desc     TYPE string,
           tasks    TYPE TABLE OF string WITH DEFAULT KEY,
           func     TYPE string,
           objects  TYPE tty_change_object,
           END OF ts_tr_info.

    " version information of an ABAP object to fetch file content
    TYPES: BEGIN OF ts_version_no,
           objname      TYPE versobjnam,
           objtype      TYPE versobjtyp,
           objversionno TYPE versno,
           END OF ts_version_no.
    TYPES: tty_version_no TYPE STANDARD TABLE OF ts_version_no.

    " type for communicating ABAP objects to sync to Git from SAP
    TYPES: BEGIN OF ts_commit_object,
            devclass    TYPE string,
            objname     TYPE string,
            objtype     TYPE string,
            objtype2    TYPE string,
            fugr        TYPE string,
            progcls     TYPE string,
            subc        TYPE string,
            delflag     TYPE string,
            verno       TYPE i,
            filecontent TYPE string,
            insertions  TYPE i,
            deletions   TYPE i,
            rows        TYPE i,
            tables      TYPE i,
           END OF ts_commit_object.
    TYPES: tty_commit_object TYPE TABLE OF ts_commit_object.

    " source lines of an ABAP object
    TYPES: tty_abaptext TYPE TABLE OF ABAPTXT255 INITIAL SIZE 0.

    " cache for function group to package name mappings
    TYPES: BEGIN OF ts_fugr_devclass,
            fugr        TYPE string,
            devclass    TYPE string,
           END OF ts_fugr_devclass.
    TYPES: tty_fugr_devclass TYPE TABLE OF ts_fugr_devclass.

    " package list
    TYPES: tty_package TYPE TABLE OF string.

    " constructor
    " io_objtelemetry - class object for telemetry
    " iv_methtelemetry - method name for telemetry
    " for telemetry, the method will be invoked with parameters iv_message as string (for message content) and iv_kind as string (for category)
    METHODS constructor
        IMPORTING
            io_objtelemetry     TYPE REF TO object OPTIONAL
            iv_methtelemetry    TYPE string OPTIONAL.

    " fetch TR info for a TR
    " iv_trid - TR ID
    " ev_info - TR info
    METHODS get_tr_info
        IMPORTING
            iv_trid TYPE string
        EXPORTING
            ev_info TYPE ts_tr_info
        RETURNING VALUE(rv_success) TYPE string.

    " fetch ABAP objects from SAP to commit to Git for a TR
    " iv_trid - TR ID
    " iv_packagenames - package names to include in commit, separated by comma
    " iv_maxrow - max rows for config table full snapshot
    " iv_maxfulltable - max tables with full snapshot, 0 means disabled
    " iv_deltastats - conduct heatmap stats for code changes or not
    " ev_comment - commit comment
    " it_commit_objects - table of ABAP objects to commit to Git including name, type, file content, add/update/delete status
    METHODS get_tr_commit_objects
        IMPORTING
            iv_trid             TYPE string
            iv_packagenames     TYPE string
            iv_maxrow           TYPE i DEFAULT 1000
            iv_maxfulltable     TYPE i DEFAULT 0
            iv_deltastats       TYPE abap_bool DEFAULT abap_false
        EXPORTING
            ev_comment          TYPE string
        CHANGING
            it_commit_objects   TYPE tty_commit_object
        RETURNING VALUE(rv_success) TYPE string.

    " get ABAP object version number (to fetch specific version's code lines)
    " iv_objname - ABAP object name from table TADIR
    " iv_objtype - ABAP object type from table TADIR
    " iv_mode - active/latest version mode
    " iv_date/iv_time - date and time of versions no later than to select
    " iv_findtest - require to find test class of a product class if applicable
    " ev_version_no - count of versions selected
    " cht_objversions - object versions selected
    METHODS get_versions_no
        IMPORTING
            iv_objname      TYPE e071-obj_name
            iv_objtype      TYPE e071-object
            iv_mode         TYPE string
            iv_date         TYPE d OPTIONAL
            iv_time         TYPE t OPTIONAL
            iv_findtest     TYPE abap_bool
        EXPORTING
            ev_version_no   TYPE i
        CHANGING
            cht_objversions TYPE tty_version_no OPTIONAL
        RETURNING VALUE(rv_success) TYPE string.

    " construct ABAP object code content
    " iv_objname - ABAP object name from table TADIR
    " iv_objtype - ABAP object type from table TADIR
    " it_objversions - object versions
    " et_filecontent - file content lines
    " ev_tclsname - test class name
    " ev_tclstype - test class type
    " et_tclsfilecontent - test class file content lines
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

    " construct ABAP data table object description content
    " iv_objname - ABAP object name from table TADIR
    " iv_version - object version
    " ev_filecontent - file content
    " et_filecontent - file content lines
    METHODS build_data_table_content
        IMPORTING
            iv_objname          TYPE e071-obj_name
            iv_version          TYPE versno
        EXPORTING
            ev_filecontent      TYPE string
            et_filecontent      TYPE tty_abaptext
        RETURNING VALUE(rv_success) TYPE string.

    " remark: OOB program RPDASC00 also dumps schema/PCR but takes background job privilege to run
    " and then fetch spools log from WRITE output and wait for job finish -- complicated and error prone
    " remark: upon releasing customizing TR the snapshot of schema/PCR is kept though SAP doesn't keep
    " all old versions, it's reliable to keep the snapshot as Git commit at the same time to reflect
    " what version the TR keeps and transports later

    " construct HR/payroll schema language code content
    " iv_schemaname - schema name
    " iv_indented - indented for instruction part or not
    " et_filecontent - schema code content lines
    METHODS build_schema_content_active
        IMPORTING
            iv_schemaname   TYPE string
            iv_indented     TYPE abap_bool DEFAULT abap_true
        EXPORTING
            et_filecontent  TYPE tty_abaptext.

    " construct HR/payroll personnel calculation rule code content
    " iv_pcrname - PCR name
    " et_filecontent - PCR code content lines
    METHODS build_pcr_content_active
        IMPORTING
            iv_pcrname      TYPE string
        EXPORTING
            et_filecontent  TYPE tty_abaptext.

    " find all hierarchical parent packages of a package
    " iv_package - package name as subpackage
    " et_packages - package list of parent packages including iv_package
    METHODS get_parentpackages
        IMPORTING
            iv_package  TYPE string
        EXPORTING
            et_packages TYPE tty_package.

    " find all hierarchical sub packages of a package
    " iv_package - package name as root package
    " et_packages - package list of sub packages including iv_package
    METHODS get_subpackages
        IMPORTING
            iv_package  TYPE devclass
        EXPORTING
            et_packages TYPE tty_package.

PROTECTED SECTION.

PRIVATE SECTION.

    CONSTANTS c_en TYPE spras VALUE 'E'.

    " structure for data table description
    TYPES: BEGIN OF ty_dd02v,
            tabname TYPE string,
            ddlanguage TYPE string,
            tabclass TYPE string,
            clidep TYPE string,
            ddtext TYPE string,
            mainflag TYPE string,
            contflag TYPE string,
            shlpexi TYPE string,
           END OF ty_dd02v.
    TYPES: BEGIN OF ty_data_table_field,
            fieldname TYPE string,
            keyflag TYPE string,
            rollname TYPE string,
            adminfield TYPE string,
            datatype TYPE string,
            leng TYPE i,
            decimals TYPE i,
            notnull TYPE string,
            ddtext TYPE string,
            domname TYPE string,
            shlporigin TYPE string,
            comptype TYPE string,
           END OF ty_data_table_field.
    TYPES: tty_data_table_field TYPE TABLE OF ty_data_table_field WITH DEFAULT KEY.
    TYPES: BEGIN OF ty_data_table_desc,
            dd02v TYPE ty_dd02v,
            dd03v TYPE tty_data_table_field,
           END OF ty_data_table_desc.

    " telemetry callback
    DATA oref_telemetry TYPE REF TO object.
    DATA method_name_telemetry TYPE string.

    " process table config change case
    METHODS build_table_config_change
        IMPORTING
            iv_cs_request       TYPE TRWBO_REQUEST
            iv_objname          TYPE e071-obj_name
            iv_objtype          TYPE e071-object
            iv_deltastats       TYPE abap_bool DEFAULT abap_false
            iv_maxrow           TYPE i DEFAULT 1000
            iv_maxfulltable     TYPE i DEFAULT 10
        CHANGING
            it_commit_objects   TYPE tty_commit_object.

    " process logical config change case
    METHODS build_logical_config_change
        IMPORTING
            iv_cs_request       TYPE TRWBO_REQUEST
            iv_object           TYPE e071
            iv_objname          TYPE e071-obj_name
            iv_objtype          TYPE e071-object
            iv_deltastats       TYPE abap_bool DEFAULT abap_false
            iv_maxrow           TYPE i DEFAULT 1000
            iv_maxfulltable     TYPE i DEFAULT 10
        CHANGING
            it_commit_objects   TYPE tty_commit_object.

    " experiment: extract config changes from audit log
    METHODS build_config_log
        IMPORTING
            iv_trid     TYPE e070-trkorr
            iv_tabname  TYPE string
            iv_fromdate TYPE d
            iv_fromtime TYPE t
            iv_todate   TYPE d
            iv_totime   TYPE t
            iv_user     TYPE string
        EXPORTING
            abaptext    TYPE tty_abaptext
        RETURNING VALUE(rv_success) TYPE string.

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

    " get source code lines for enhancement implementation
    METHODS get_code_lines_enho
        IMPORTING
            iv_version  TYPE versno
            iv_objname  TYPE versobjnam
            iv_logdest  TYPE rfcdest
        EXPORTING
            abaptext    TYPE tty_abaptext
        RETURNING VALUE(rv_success) TYPE string.

    " fetch table schema
    METHODS get_table_schema
        IMPORTING
            iv_objname  TYPE e071-obj_name
            iv_version  TYPE versno
            iv_escape   TYPE abap_bool DEFAULT abap_true
        EXPORTING
            ev_desc     TYPE ty_data_table_desc
        RETURNING VALUE(rv_success) TYPE string.

    " build config table delta-row lines
    METHODS get_config_delta_lines
        IMPORTING
            iv_tabname      TYPE e071k-objname
            iv_mtabname     TYPE e071k-mastername
            iv_objtype      TYPE e071-object
            iv_request      TYPE TRWBO_REQUEST
            iv_deltastats   TYPE abap_bool DEFAULT abap_false
        EXPORTING
            abaptext        TYPE tty_abaptext
            ev_rows         TYPE i
        RETURNING VALUE(rv_success) TYPE string.

    " build config table all-row lines
    METHODS get_config_table_lines
        IMPORTING
            iv_tabname  TYPE e071k-objname
            iv_request  TYPE TRWBO_REQUEST
            iv_tsv      TYPE abap_bool
            iv_maxrow   TYPE i DEFAULT 1000
        EXPORTING
            abaptext    TYPE tty_abaptext
        RETURNING VALUE(rv_success) TYPE string.

    " build config table row lines for header
    METHODS get_config_lines_header
        IMPORTING
            iv_tabname  TYPE e071k-objname
            iv_request  TYPE TRWBO_REQUEST
            iv_tsv      TYPE abap_bool DEFAULT abap_false
        EXPORTING
            ev_desc     TYPE ty_data_table_desc
        CHANGING
            abaptext    TYPE tty_abaptext
        RETURNING VALUE(rv_success) TYPE string.

    " extract packed number text from raw row line
    METHODS build_packnumber_text
        IMPORTING
            iv_line TYPE string
            iv_off  TYPE i
            iv_leng TYPE i
            iv_dec  TYPE i
        EXPORTING
            ev_text TYPE string
            ev_leng TYPE i.

    " get TR creation timestamp
    METHODS get_tr_creation
        IMPORTING
            iv_trid     TYPE e070-trkorr
        EXPORTING
            ev_date     TYPE d
            ev_time     TYPE t
        RETURNING VALUE(rv_success) TYPE string.

    " get class object version (for public/protected/private sections in definition and method implementations)
    METHODS get_class_versions_no
        IMPORTING
            iv_objname      TYPE e071-obj_name
            iv_objtype      TYPE e071-object
            iv_mode         TYPE string
            iv_date         TYPE d OPTIONAL
            iv_time         TYPE t OPTIONAL
            iv_findtest     TYPE abap_bool DEFAULT abap_true
        CHANGING
            cht_objversions TYPE tty_version_no OPTIONAL
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

    " fetch delta stats of an ABAP object given two versions
    CLASS-METHODS get_delta
        IMPORTING
            version_new     TYPE versno
            objname_new     TYPE versobjnam
            objtype_new     TYPE versobjtyp
            logdest_new     TYPE rfcdest
            version_old     TYPE versno
            objname_old     TYPE versobjnam
            objtype_old     TYPE versobjtyp
            logdest_old     TYPE rfcdest
        EXPORTING
            no_delta        TYPE c
            insertions      TYPE i
            deletions       TYPE i.

    " fetch ABAP code object source code lines of a given version
    CLASS-METHODS get_initial
        IMPORTING
            iv_version TYPE versno
            iv_objname TYPE versobjnam
            iv_objtype TYPE versobjtyp
            iv_logdest TYPE rfcdest
        EXPORTING
            no_delta TYPE c
            linecount TYPE i.

    " parse column values from row text
    METHODS get_column_values
        IMPORTING
            iv_data_table_desc  TYPE ty_data_table_desc
            iv_line             TYPE string
        CHANGING
            cht_filecontent     TYPE tty_abaptext.

    " get methods of a class object
    METHODS get_class_methods
        IMPORTING
            iv_classname    TYPE classname
        CHANGING
            cht_methods     TYPE abap_methdescr_tab.

    " wrapper to write telemetry with the callback registered
    METHODS write_telemetry
        IMPORTING
            iv_message  TYPE string
            iv_kind     TYPE string DEFAULT 'error'.

ENDCLASS.



CLASS ZCL_UTILITY_ABAPTOGIT_TR IMPLEMENTATION.


  METHOD CONSTRUCTOR.

    IF io_objtelemetry IS SUPPLIED.
        me->oref_telemetry = io_objtelemetry.
    ENDIF.

    IF iv_methtelemetry IS SUPPLIED.
        me->method_name_telemetry = iv_methtelemetry.
    ENDIF.

  ENDMETHOD.


  METHOD GET_TR_INFO.

    DATA ld_cs_request TYPE TRWBO_REQUEST.
    DATA lv_trkorr TYPE TRKORR.
    DATA lv_task TYPE string.
    DATA lv_taskid TYPE string.
    DATA lv_taskdesc TYPE string.
    DATA lt_tasks TYPE TABLE OF string.
    DATA lt_taskfields TYPE TABLE OF string.
    DATA ls_change_object TYPE ts_change_object.
    FIELD-SYMBOLS <fs_cs_request_object> LIKE LINE OF ld_cs_request-objects.

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
        me->write_telemetry( iv_message = |GET_TR_INFO fails to call TR_READ_REQUEST with '{ lv_trkorr }' subrc { sy-subrc }| ).
        rv_success = abap_false.
        RETURN.
    ENDIF.

    ev_info-id = iv_trid.
    ev_info-func = ld_cs_request-h-trfunction.
    ev_info-owner = ld_cs_request-h-as4user.
    ev_info-desc = ld_cs_request-h-as4text.

    " fetch tasks in a TR
    SELECT obj_name FROM e071 INTO TABLE @lt_tasks WHERE trkorr = @lv_trkorr AND object = 'RELE' AND pgmid = 'CORR'.

    " append all tasks' description to the commit description
    LOOP AT lt_tasks INTO lv_task.
        CLEAR lt_taskfields.
        SPLIT lv_task AT ' ' INTO TABLE lt_taskfields.
        lv_taskid = lt_taskfields[ 1 ].
        SELECT SINGLE as4text FROM e07t INTO @lv_taskdesc WHERE trkorr = @lv_taskid.    "#EC WARNOK
        APPEND |{ lv_task } { lv_taskdesc }| TO ev_info-tasks.
    ENDLOOP.

    LOOP AT ld_cs_request-objects ASSIGNING <fs_cs_request_object> WHERE object <> 'RELE'.
        CLEAR ls_change_object.
        ls_change_object-name = <fs_cs_request_object>-obj_name.
        ls_change_object-type = <fs_cs_request_object>-object.
        APPEND ls_change_object TO ev_info-objects.
    ENDLOOP.

  ENDMETHOD.


  METHOD GET_TR_COMMIT_OBJECTS.

    DATA ld_cs_request TYPE TRWBO_REQUEST.
    DATA lv_trkorr TYPE TRKORR.
    FIELD-SYMBOLS <fs_cs_request_object> LIKE LINE OF ld_cs_request-objects.
    FIELD-SYMBOLS <fs_cs_request_key> LIKE LINE OF ld_cs_request-keys.
    DATA lt_objversions TYPE tty_version_no.
    DATA lt_filecontent TYPE tty_abaptext.
    DATA lt_tclsfilecontent TYPE tty_abaptext.
    DATA lt_packagenames TYPE TABLE OF string.
    DATA lv_packagename TYPE string.
    DATA lv_objname2 TYPE string.
    DATA lv_objtype2 TYPE string.
    DATA lv_devclass TYPE string.
    DATA lv_fugr TYPE string.
    DATA lv_filecontent TYPE string.
    DATA lv_tclsname TYPE string.
    DATA lv_tclstype TYPE string.
    DATA lv_tclsfilecontent TYPE string.
    DATA lv_version_no TYPE i.
    DATA lv_funcname TYPE string.
    DATA lt_tasks TYPE TABLE OF string.
    DATA lt_commentlines TYPE TABLE OF string.
    DATA lt_taskids TYPE TABLE OF string.
    DATA lv_task TYPE string.
    DATA lv_taskid TYPE string.
    DATA lv_taskdesc TYPE string.
    DATA lt_taskfields TYPE TABLE OF string.
    DATA lt_tasktexts TYPE TABLE OF string.
    DATA lv_programm TYPE PROGRAMM.
    DATA lv_classkey TYPE SEOCLSKEY.
    DATA lv_classname TYPE tadir-obj_name.
    DATA lv_haspackage TYPE abap_bool.
    DATA lt_parentpackages TYPE tty_package.
    DATA lv_foundparentpackage TYPE abap_bool.
    DATA lt_objname_parts TYPE TABLE OF string.
    DATA lt_classes TYPE TABLE OF string.
    DATA lt_fugrs TYPE TABLE OF ts_fugr_devclass.
    DATA lv_cdate TYPE cdate.
    DATA lv_udate TYPE aedat.
    DATA lv_progcls TYPE t52ba-pwert.
    DATA lv_subc TYPE reposrc-subc.
    DATA lv_objverprev TYPE versno.
    DATA lv_insertions TYPE i.
    DATA lv_deletions TYPE i.
    DATA lv_no_delta TYPE c.
    DATA lv_total_insertions TYPE i.
    DATA lv_total_deletions TYPE i.
    DATA lv_success TYPE string.

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
        me->write_telemetry( iv_message = |GET_TR_COMMIT_OBJECTS fails to call TR_READ_REQUEST with '{ lv_trkorr }' subrc { sy-subrc }| ).
        rv_success = abap_false.
        RETURN.
    ENDIF.

    " only support workbench/customizing TR
    IF ld_cs_request-h-trfunction <> 'K' AND ld_cs_request-h-trfunction <> 'W'.
        me->write_telemetry( iv_message = |GET_TR_COMMIT_OBJECTS meets request type '{ ld_cs_request-h-trfunction }'| ).
        rv_success = abap_false.
        RETURN.
    ENDIF.

    " only support released one
    IF ld_cs_request-h-trstatus <> 'R'.
        me->write_telemetry( iv_message = |GET_TR_COMMIT_OBJECTS meets request status '{ ld_cs_request-h-trstatus }'| ).
        rv_success = abap_false.
        RETURN.
    ENDIF.

    " construct Git commit description carrying TR ID, owner and original description
    " the Git commit can't has committer as the TR owner, instead, on-behalf-of the owner by user name/PAT specified
    APPEND |{ iv_trid }\|{ ld_cs_request-h-as4user }\|{ ld_cs_request-h-as4text }| TO lt_commentlines.

    " fetch tasks in a TR
    SELECT obj_name FROM e071 INTO TABLE @lt_tasks WHERE trkorr = @lv_trkorr AND object = 'RELE' AND pgmid = 'CORR'.

    " append all tasks' description to the commit description
    LOOP AT lt_tasks INTO lv_task.
        CLEAR lt_taskfields.
        SPLIT lv_task AT ' ' INTO TABLE lt_taskfields.
        lv_taskid = lt_taskfields[ 1 ].
        SELECT SINGLE as4text FROM e07t INTO @lv_taskdesc WHERE trkorr = @lv_taskid.    "#EC WARNOK
        APPEND |{ lv_task } { lv_taskdesc }| TO lt_tasktexts.
    ENDLOOP.

    APPEND LINES OF lt_tasktexts TO lt_commentlines.

    " commit description now contains TR description plus all tasks' description each line
    ev_comment = concat_lines_of( table = lt_commentlines sep = CL_ABAP_CHAR_UTILITIES=>CR_LF ).

    CLEAR: lt_taskids, lt_taskfields, lt_tasktexts.

    SPLIT iv_packagenames AT ',' INTO TABLE lt_packagenames.

    " process objects in the TR
    LOOP AT ld_cs_request-objects ASSIGNING <fs_cs_request_object> WHERE pgmid <> 'CORR'.

        DATA(lv_objname) = <fs_cs_request_object>-obj_name.
        DATA(lv_objtype) = <fs_cs_request_object>-object.

        " table config change object
        IF lv_objtype = 'TABU'
            OR lv_objtype = 'CDAT'
            OR lv_objtype = 'VDAT'
            OR lv_objtype = 'TDAT'.
            me->build_table_config_change(
                EXPORTING
                    iv_cs_request = ld_cs_request
                    iv_objname = <fs_cs_request_object>-obj_name
                    iv_objtype = <fs_cs_request_object>-object
                    iv_deltastats = iv_deltastats
                    iv_maxrow = iv_maxrow
                    iv_maxfulltable = iv_maxfulltable
                CHANGING
                    it_commit_objects = it_commit_objects
                     ).
            CONTINUE.
        ENDIF.

        " schema/PCR object
        IF lv_objtype = 'PSCC' OR lv_objtype = 'PCYC'.

            CLEAR lt_filecontent.
            lv_objname2 = lv_objname.
            me->build_schema_content_active(
                EXPORTING
                    iv_schemaname = lv_objname2
                IMPORTING
                    et_filecontent = lt_filecontent
                     ).

            IF lv_objtype = 'PSCC'.
                SELECT SINGLE cdate FROM t52cc INTO @lv_cdate WHERE sname = @lv_objname.
                SELECT SINGLE udate FROM t52cc INTO @lv_udate WHERE sname = @lv_objname.
                SELECT SINGLE pwert FROM t52ba INTO @lv_progcls WHERE potyp = 'SCHE' AND ponam = @lv_objname AND pattr = 'PCL'. "#EC WARNOK
            ELSE.
                SELECT SINGLE cdate FROM t52ce INTO @lv_cdate WHERE cname = @lv_objname.
                SELECT SINGLE udate FROM t52ce INTO @lv_udate WHERE cname = @lv_objname.
                SELECT SINGLE pwert FROM t52ba INTO @lv_progcls WHERE potyp = 'CYCL' AND ponam = @lv_objname AND pattr = 'PCL'. "#EC WARNOK
            ENDIF.

            " schema/PCR has no reliable version upon releasing TR, have to use dates to determine add or update
            IF lv_udate IS INITIAL OR lv_cdate = lv_udate.
                lv_version_no = 1.
            ELSE.
                lv_version_no = 2.
            ENDIF.

            " stitch to string from source code lines
            lv_filecontent = concat_lines_of( table = lt_filecontent sep = CL_ABAP_CHAR_UTILITIES=>CR_LF ).

            " align with GUI_DOWNLOAD which adds a blank line
            lv_filecontent = lv_filecontent && CL_ABAP_CHAR_UTILITIES=>CR_LF.

            TRANSLATE lv_objname TO UPPER CASE.

            APPEND VALUE ts_commit_object(
                devclass = c_schemapcr
                objname = lv_objname
                objtype = lv_objtype
                objtype2 = lv_objtype
                fugr = ''
                progcls = lv_progcls
                delflag = abap_false
                verno = lv_version_no
                filecontent = lv_filecontent
                ) TO it_commit_objects.

            CONTINUE.

        ENDIF.

        CLEAR: lv_objtype2, lv_devclass.

        IF lv_objtype = 'CINC'
            OR lv_objtype = 'CLSD'
            OR lv_objtype = 'CLAS'
            OR lv_objtype = 'CPUB'
            OR lv_objtype = 'CPRO'
            OR lv_objtype = 'CPRI'
            OR lv_objtype = 'METH'.

            " a test class, class definition, public/protected/private section and method will not be located in table tadir
            " need to find the product class it belongs to and then find the package the product class belongs to

            IF lv_objtype = 'CINC'.
                " test class name to product class name
                lv_programm = lv_objname.
                CALL FUNCTION 'SEO_CLASS_GET_NAME_BY_INCLUDE'
                    EXPORTING
                        progname = lv_programm
                    IMPORTING
                        clskey = lv_classkey.   "#EC FB_OLDED
                lv_classname = lv_classkey.
            ELSE.
                IF lv_objtype = 'METH'.
                    " class name <spaces> method name pattern
                    CLEAR lt_objname_parts.
                    SPLIT lv_objname AT ' ' INTO TABLE lt_objname_parts.
                    lv_classname = lt_objname_parts[ 1 ].
                ELSE.
                    " for CLSD/CPUB/CPRO/CPRI case class name is provided
                    lv_classname = lv_objname.
                ENDIF.

                IF line_exists( lt_classes[ table_line = lv_classname ] ).
                    " this class has been processed, skip
                    CONTINUE.
                ELSE.
                    " use the class name instead and ensure it's processed only one time
                    APPEND lv_classname TO lt_classes.
                    lv_objname = lv_classname.
                    lv_objtype = 'CLAS'.
                ENDIF.
            ENDIF.

            " find out which package the class belongs to
            SELECT SINGLE devclass INTO lv_devclass FROM tadir
                WHERE object = 'CLAS' AND obj_name = lv_classname.  "#EC WARNOK "#EC CI_SGLSELECT

        ELSEIF lv_objtype = 'FUNC'.

            " function module case, find out function group and then the package function group belongs to
            lv_funcname = lv_objname.
            me->get_fugr( EXPORTING iv_objname = lv_funcname IMPORTING ev_fugrname = lv_fugr ).

            " use cache for function group objects
            IF line_exists( lt_fugrs[ fugr = lv_fugr ] ).
                lv_devclass = lt_fugrs[ fugr = lv_fugr ]-devclass.
            ELSE.
                SELECT SINGLE devclass FROM tadir INTO lv_devclass
                    WHERE obj_name = lv_fugr AND object = 'FUGR'.   "#EC WARNOK "#EC CI_SGLSELECT
                APPEND VALUE ts_fugr_devclass( fugr = lv_fugr devclass = lv_devclass ) TO lt_fugrs.
            ENDIF.

        ELSEIF lv_objtype = 'REPS'.

            " could be repair of an object
            " need to search object type to replace 'REPS'
            SELECT SINGLE object FROM tadir INTO lv_objtype
                WHERE obj_name = lv_objname.    "#EC WARNOK "#EC CI_SGLSELECT
            IF sy-subrc = 0.

                " found, use the right type of the object
                " find out which package the ABAP object belongs to
                SELECT SINGLE devclass INTO lv_devclass FROM tadir
                    WHERE object = lv_objtype AND obj_name = lv_objname.    "#EC WARNOK "#EC CI_SGLSELECT

            ELSE.

                " function include case, extract function group name and then the package function group belongs to
                " function include name as L + function group name + 3 characters
                IF strlen( lv_objname ) < 4.
                    lv_fugr = ''.
                ELSE.
                    lv_fugr = substring( val = lv_objname off = 1 len = strlen( lv_objname ) - 1 - 3 ).
                ENDIF.

                " use cache for function group objects
                IF line_exists( lt_fugrs[ fugr = lv_fugr ] ).
                    lv_devclass = lt_fugrs[ fugr = lv_fugr ]-devclass.
                ELSE.
                    SELECT SINGLE devclass FROM tadir INTO lv_devclass
                        WHERE obj_name = lv_fugr AND object = 'FUGR'.   "#EC WARNOK "#EC CI_SGLSELECT
                    APPEND VALUE ts_fugr_devclass( fugr = lv_fugr devclass = lv_devclass ) TO lt_fugrs.
                ENDIF.

            ENDIF.

        ELSEIF lv_objtype = 'PROG' OR lv_objtype = 'INTF' OR lv_objtype = 'ENHO'.

            " program (include), interface, enhancement implementation case

            " find out which package the ABAP object belongs to
            SELECT SINGLE devclass INTO lv_devclass FROM tadir
                WHERE object = lv_objtype AND obj_name = lv_objname.    "#EC WARNOK "#EC CI_SGLSELECT

        ELSEIF lv_objtype = 'TABD' OR lv_objtype = 'TABL'.

            " data table case

            " find out which package the ABAP object belongs to
            SELECT SINGLE devclass INTO lv_devclass FROM tadir
                WHERE object = 'TABL' AND obj_name = lv_objname.    "#EC WARNOK "#EC CI_SGLSELECT

        ELSE.

            " all other config change objects
            me->build_logical_config_change(
                EXPORTING
                    iv_cs_request = ld_cs_request
                    iv_object = <fs_cs_request_object>
                    iv_objname = lv_objname
                    iv_objtype = lv_objtype
                    iv_deltastats = iv_deltastats
                    iv_maxrow = iv_maxrow
                    iv_maxfulltable = iv_maxfulltable
                CHANGING
                    it_commit_objects = it_commit_objects
                     ).

            CONTINUE.

        ENDIF.

        " ABAP source code or table schema cases

        lv_haspackage = abap_true.
        IF sy-subrc <> 0.
            " the ABAP object is not found from tadir, it may be a deleted object
            lv_haspackage = abap_false.
        ENDIF.

        " fetch versions no later than given TR date/time
        CLEAR lt_objversions.
        lv_success = me->get_versions_no(
            EXPORTING
                iv_objname = lv_objname
                iv_objtype = lv_objtype
                iv_mode = c_latest_version
                iv_date = ld_cs_request-h-as4date
                iv_time = ld_cs_request-h-as4time
                iv_findtest = abap_false
            IMPORTING
                ev_version_no = lv_version_no
            CHANGING
                cht_objversions = lt_objversions
                ).
        IF lv_success = abap_false AND lv_haspackage = abap_false.
            " can't locate this object in table tadir and versions neither
            " fail to find which package it belongs to and can't place it to ADO push payload
            me->write_telemetry( iv_message = |deleted object { lv_objname } type { lv_objtype } can't be processed without package name available| ).
            CONTINUE.
        ENDIF.

        " is the object in (sub package of) one of the packages specified?
        CLEAR lt_parentpackages.
        me->get_parentpackages(
            EXPORTING
                iv_package = lv_devclass
            IMPORTING
                et_packages = lt_parentpackages
                 ).
        lv_foundparentpackage = abap_false.
        LOOP AT lt_parentpackages INTO DATA(waparentpackage).
            IF line_exists( lt_packagenames[ table_line = waparentpackage ] ).
                lv_foundparentpackage = abap_true.
                EXIT.
            ENDIF.
        ENDLOOP.
        CHECK lv_foundparentpackage = abap_true.

        " is there any version found?
        CHECK lv_version_no > 0.

        " fetch function group name in case of function module
        IF lv_objtype = 'FUGR'.
            lv_funcname = lv_objname.
            lv_objtype2 = 'FUNC'.
            me->get_fugr( EXPORTING iv_objname = lv_funcname IMPORTING ev_fugrname = lv_fugr ).
        ENDIF.

        " construct object content (and test class content won't be provided given not required to in fetching version above)
        IF lv_objtype = 'TABL' OR lv_objtype = 'TABD'.
            rv_success = me->build_data_table_content(
                EXPORTING
                    iv_objname = lv_objname
                    iv_version = lt_objversions[ 1 ]-objversionno
                IMPORTING
                    ev_filecontent = lv_filecontent
                    ).
            CHECK rv_success = abap_true.
        ELSE.

            CLEAR: lv_total_insertions, lv_total_deletions.

            IF iv_deltastats = abap_true.

                LOOP AT lt_objversions INTO DATA(wa_objversion).
                    CLEAR: lv_insertions, lv_deletions.
                    IF wa_objversion-objversionno >= 2.
                        lv_objverprev = wa_objversion-objversionno - 1.
                        me->get_delta(
                            EXPORTING
                                version_new = wa_objversion-objversionno
                                objname_new = wa_objversion-objname
                                objtype_new = wa_objversion-objtype
                                logdest_new = ''
                                version_old = lv_objverprev
                                objname_old = wa_objversion-objname
                                objtype_old = wa_objversion-objtype
                                logdest_old = ''
                            IMPORTING
                                no_delta = lv_no_delta
                                insertions = lv_insertions
                                deletions = lv_deletions
                                 ).
                    ELSEIF wa_objversion-objversionno = 1.
                        me->get_initial(
                            EXPORTING
                                iv_version = wa_objversion-objversionno
                                iv_objname = wa_objversion-objname
                                iv_objtype = wa_objversion-objtype
                                iv_logdest = ''
                            IMPORTING
                                no_delta = lv_no_delta
                                linecount = lv_insertions
                                 ).
                        IF lv_no_delta = abap_true.
                            lv_insertions = 0.
                            lv_deletions = 0.
                        ELSE.
                            lv_deletions = 0.
                        ENDIF.
                    ENDIF.
                    lv_total_insertions = lv_total_insertions + lv_insertions.
                    lv_total_deletions = lv_total_deletions + lv_deletions.
                ENDLOOP.

            ELSE.

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

                " stitch to string from source code lines
                lv_filecontent = concat_lines_of( table = lt_filecontent sep = CL_ABAP_CHAR_UTILITIES=>CR_LF ).

                " align with GUI_DOWNLOAD which adds a blank line
                lv_filecontent = lv_filecontent && CL_ABAP_CHAR_UTILITIES=>CR_LF.

            ENDIF.

        ENDIF.

        IF lv_objtype = 'CINC'.
            " following abapGit where class name is used for test class name instead of ====CCAU like one
            lv_objname = lv_classname.
        ENDIF.

        CLEAR lv_subc.
        SELECT SINGLE subc FROM reposrc INTO @lv_subc WHERE progname = @lv_objname. "#EC WARNOK

        TRANSLATE lv_objname TO UPPER CASE.

        IF line_exists( it_commit_objects[ devclass = lv_devclass objname = lv_objname objtype = lv_objtype objtype2 = lv_objtype2 fugr = lv_fugr ] ).
            CONTINUE.
        ENDIF.

        APPEND VALUE ts_commit_object(
            devclass = lv_devclass
            objname = lv_objname
            objtype = lv_objtype
            objtype2 = lv_objtype2
            fugr = lv_fugr
            subc = lv_subc
            delflag = abap_false
            verno = lv_version_no
            filecontent = lv_filecontent
            insertions = lv_total_insertions
            deletions = lv_total_deletions
            ) TO it_commit_objects.

    ENDLOOP.

  ENDMETHOD.


  METHOD BUILD_TABLE_CONFIG_CHANGE.

    FIELD-SYMBOLS <fs_cs_request_key> LIKE LINE OF iv_cs_request-keys.
    DATA lt_filecontent TYPE tty_abaptext.
    DATA lv_objname TYPE string.
    DATA lv_objtype TYPE string.
    DATA lv_fulltablecount TYPE i.
    DATA lv_filecontent TYPE string.
    DATA lv_rows TYPE i.
    DATA lv_success TYPE string.

    LOOP AT iv_cs_request-keys ASSIGNING <fs_cs_request_key> WHERE mastertype = iv_objtype AND mastername = iv_objname.

        lv_objtype = iv_objtype.
        lv_objname = <fs_cs_request_key>-objname.
        TRANSLATE lv_objname TO UPPER CASE.

        IF line_exists( it_commit_objects[ objname = lv_objname ] ).
            CONTINUE.
        ENDIF.

        CLEAR lt_filecontent.
        lv_success = me->get_config_delta_lines(
            EXPORTING
                iv_tabname = <fs_cs_request_key>-objname
                iv_mtabname = <fs_cs_request_key>-mastername
                iv_objtype = iv_objtype
                iv_request = iv_cs_request
                iv_deltastats = iv_deltastats
            IMPORTING
                abaptext = lt_filecontent
                ev_rows = lv_rows
             ).
        IF lv_success <> abap_true.
            me->write_telemetry( iv_message = |fail to get config delta lines for { <fs_cs_request_key>-objname }| ).
            CONTINUE.
        ENDIF.

        IF iv_deltastats = abap_false.
            lv_filecontent = concat_lines_of( table = lt_filecontent sep = CL_ABAP_CHAR_UTILITIES=>CR_LF ).
            " align with GUI_DOWNLOAD which adds a blank line
            lv_filecontent = lv_filecontent && CL_ABAP_CHAR_UTILITIES=>CR_LF.
        ENDIF.

        APPEND VALUE ts_commit_object(
            devclass = c_config
            objname = lv_objname
            objtype = lv_objtype
            objtype2 = lv_objtype
            fugr = ''
            progcls = 'delta'
            delflag = abap_false
            verno = 1
            filecontent = lv_filecontent
            tables = 1
            rows = lv_rows
            ) TO it_commit_objects.

        " skip if too many full tables sync-ed already, massive full tables may exceed memory limit and fail the process
        CLEAR lv_fulltablecount.
        LOOP AT it_commit_objects TRANSPORTING NO FIELDS WHERE progcls = 'full' OR progcls = 'fulltsv'.
            lv_fulltablecount = lv_fulltablecount + 1.
        ENDLOOP.
        IF lv_fulltablecount >= iv_maxfulltable.
            CONTINUE.
        ENDIF.

        " human readable version
        CLEAR lt_filecontent.
        lv_success = me->get_config_table_lines(
            EXPORTING
                iv_tabname = <fs_cs_request_key>-objname
                iv_request = iv_cs_request
                iv_tsv = abap_false
                iv_maxrow = iv_maxrow
            IMPORTING
                abaptext = lt_filecontent
             ).
        IF lv_success <> abap_true.
            me->write_telemetry( iv_message = |fail to get config full lines for { <fs_cs_request_key>-objname }| ).
            CONTINUE.
        ENDIF.

        lv_filecontent = concat_lines_of( table = lt_filecontent sep = CL_ABAP_CHAR_UTILITIES=>CR_LF ).

        " align with GUI_DOWNLOAD which adds a blank line
        lv_filecontent = lv_filecontent && CL_ABAP_CHAR_UTILITIES=>CR_LF.

        APPEND VALUE ts_commit_object(
            devclass = c_config
            objname = lv_objname
            objtype = lv_objtype
            objtype2 = lv_objtype
            fugr = ''
            progcls = 'full'
            delflag = abap_false
            verno = 1
            filecontent = lv_filecontent
            ) TO it_commit_objects.

        " TSV version
        CLEAR lt_filecontent.
        lv_success = me->get_config_table_lines(
            EXPORTING
                iv_tabname = <fs_cs_request_key>-objname
                iv_request = iv_cs_request
                iv_tsv = abap_true
                iv_maxrow = iv_maxrow
            IMPORTING
                abaptext = lt_filecontent
             ).
        IF lv_success <> abap_true.
            me->write_telemetry( iv_message = |fail to get config full lines for { <fs_cs_request_key>-objname }| ).
            CONTINUE.
        ENDIF.

        lv_filecontent = concat_lines_of( table = lt_filecontent sep = CL_ABAP_CHAR_UTILITIES=>CR_LF ).

        " align with GUI_DOWNLOAD which adds a blank line
        lv_filecontent = lv_filecontent && CL_ABAP_CHAR_UTILITIES=>CR_LF.

        APPEND VALUE ts_commit_object(
            devclass = c_config
            objname = lv_objname
            objtype = lv_objtype
            objtype2 = lv_objtype
            fugr = ''
            progcls = 'fulltsv'
            delflag = abap_false
            verno = 1
            filecontent = lv_filecontent
            ) TO it_commit_objects.

    ENDLOOP.

  ENDMETHOD.


  METHOD BUILD_LOGICAL_CONFIG_CHANGE.

    DATA lv_objname_cto TYPE ob_object.
    DATA lv_objtype_cto TYPE ob_typ.
    DATA ls_objecttext TYPE objt.
    DATA lt_object_piecelist TYPE TABLE OF objs.
    DATA lt_objects TYPE tr_objects.
    DATA lt_keys TYPE tr_keys.
    DATA ls_data_table_desc TYPE ty_data_table_desc.
    DATA lt_filecontent TYPE tty_abaptext.
    DATA lt_filecontent_dummy TYPE tty_abaptext.
    TYPES: line(5000) TYPE c.
    DATA lv_tabname TYPE dd02l-tabname.
    DATA lt_entry_tab TYPE TABLE OF line.
    DATA lv_line TYPE string.
    DATA lv_row TYPE i.
    FIELD-SYMBOLS <fs_entry> TYPE line.
    DATA lv_filecontent TYPE string.
    FIELD-SYMBOLS <fs_commit_object> TYPE ts_commit_object.
    DATA lv_append TYPE abap_bool.
    DATA lv_fulltablecount TYPE i.
    DATA lv_success TYPE string.

    CALL FUNCTION 'CTO_E071_GET_OBJECT'
        EXPORTING
            is_e071            = iv_object
        IMPORTING
            ev_objectname      = lv_objname_cto
            ev_objecttype      = lv_objtype_cto
        EXCEPTIONS
            no_sobj_object     = 1
            object_not_defined = 2
            OTHERS             = 3.
    IF sy-subrc <> 0.
        me->write_telemetry( iv_message = |fails to call CTO_E071_GET_OBJECT with subrc { sy-subrc } for object type { iv_object-object }| ).
        RETURN.
    ENDIF.

    " get objects in a TR object line
    CALL FUNCTION 'CTO_OBJECT_GET'
        EXPORTING
            iv_objectname      = lv_objname_cto
            iv_objecttype      = lv_objtype_cto
            iv_sel_objt        = abap_true
            iv_sel_objs        = abap_true
      IMPORTING
            es_objt            = ls_objecttext
      TABLES
            tt_objs            = lt_object_piecelist
      EXCEPTIONS
            object_not_defined = 1
            OTHERS             = 2.
    IF sy-subrc <> 0.
        me->write_telemetry( iv_message = |fails to call CTO_OBJECT_GET with subrc { sy-subrc }| ).
        RETURN.
    ENDIF.

    " get keys of tables
    CALL FUNCTION 'RESOLVE_LOGICAL_OBJECT'
      EXPORTING
        e071_entry        = iv_object
        iv_client         = sy-mandt
      TABLES
        e071k_tab         = lt_keys
        e071_tab          = lt_objects
      EXCEPTIONS
        no_logical_object = 1
        OTHERS            = 1.
    IF sy-subrc <> 0.
        me->write_telemetry( iv_message = |fails to call RESOLVE_LOGICAL_OBJECT with subrc { sy-subrc }| ).
        RETURN.
    ENDIF.

    LOOP AT lt_keys INTO DATA(wakey).

        CLEAR: ls_data_table_desc, lt_filecontent, lt_filecontent_dummy, lt_entry_tab, lv_filecontent.

        " multiple object lines may refer to the same tables
        IF line_exists( it_commit_objects[ objname = wakey-objname ] ).
            ASSIGN it_commit_objects[ objname = wakey-objname objtype = wakey-object ] TO <fs_commit_object>.
            lv_row = <fs_commit_object>-rows.
            lv_success = me->get_config_lines_header(
                EXPORTING
                    iv_tabname = wakey-objname
                    iv_request = iv_cs_request
                IMPORTING
                    ev_desc = ls_data_table_desc
                CHANGING
                    abaptext = lt_filecontent_dummy
                 ).
            CHECK lv_success = abap_true.
            lv_append = abap_true.
        ELSE.
            lv_row = 1.
            lv_success = me->get_config_lines_header(
                EXPORTING
                    iv_tabname = wakey-objname
                    iv_request = iv_cs_request
                IMPORTING
                    ev_desc = ls_data_table_desc
                CHANGING
                    abaptext = lt_filecontent
                 ).
            CHECK lv_success = abap_true.
            IF iv_deltastats = abap_false.
                APPEND 'Rows:' TO lt_filecontent.
                APPEND '' TO lt_filecontent.
            ENDIF.
            lv_append = abap_false.
        ENDIF.

        " get texts of each row's column values
        lv_tabname = wakey-objname.
        CALL FUNCTION 'SRTT_TABLE_GET_BY_KEYLIST'
            EXPORTING
                tabname = lv_tabname
                mtype = wakey-mastertype
                mtabname = wakey-mastername
            TABLES
                e071k_tab = lt_keys
                entry_tab = lt_entry_tab
            EXCEPTIONS
                OTHERS = 1.
        IF sy-subrc <> 0.
            me->write_telemetry( iv_message = |BUILD_LOGICAL_CONFIG_CHANGE fails in SRTT_TABLE_GET_BY_KEYLIST call with { lv_tabname }| ).
        ENDIF.

        " extract field values from entry line text
        IF lines( lt_entry_tab ) = 0.
            IF iv_deltastats = abap_false.
                " use keys only
                APPEND |Row { lv_row }| TO lt_filecontent.
                lv_line = wakey-tabkey.
                me->get_column_values(
                    EXPORTING
                        iv_data_table_desc = ls_data_table_desc
                        iv_line = lv_line
                     CHANGING
                        cht_filecontent = lt_filecontent
                         ).
                APPEND '' TO lt_filecontent.
            ENDIF.
            lv_row = lv_row + 1.
        ELSE.
            " full column values besides keys
            LOOP AT lt_entry_tab ASSIGNING <fs_entry>.
                IF iv_deltastats = abap_false.
                    APPEND |Row { lv_row }| TO lt_filecontent.
                    lv_line = <fs_entry>.
                    me->get_column_values(
                        EXPORTING
                            iv_data_table_desc = ls_data_table_desc
                            iv_line = lv_line
                         CHANGING
                            cht_filecontent = lt_filecontent
                             ).
                    APPEND '' TO lt_filecontent.
                ENDIF.
                lv_row = lv_row + 1.
            ENDLOOP.
        ENDIF.

        IF iv_deltastats = abap_false.
            IF lv_append = abap_true.
                lv_filecontent = <fs_commit_object>-filecontent && concat_lines_of( table = lt_filecontent sep = CL_ABAP_CHAR_UTILITIES=>CR_LF ).
            ELSE.
                lv_filecontent = concat_lines_of( table = lt_filecontent sep = CL_ABAP_CHAR_UTILITIES=>CR_LF ).
            ENDIF.
            " align with GUI_DOWNLOAD which adds a blank line
            lv_filecontent = lv_filecontent && CL_ABAP_CHAR_UTILITIES=>CR_LF.
         ENDIF.

        IF lv_append = abap_true.
            <fs_commit_object>-rows = lv_row.
            <fs_commit_object>-filecontent = lv_filecontent.
        ELSE.
            APPEND VALUE ts_commit_object(
                devclass = c_config
                objname = wakey-objname
                objtype = wakey-object
                objtype2 = wakey-object
                fugr = ''
                progcls = 'delta'
                delflag = abap_false
                verno = 1
                filecontent = lv_filecontent
                tables = 1
                rows = lv_row
                ) TO it_commit_objects.
        ENDIF.

        " skip if too many full tables sync-ed already, massive full tables may exceed memory limit and fail the process
        CLEAR lv_fulltablecount.
        LOOP AT it_commit_objects TRANSPORTING NO FIELDS WHERE progcls = 'full' OR progcls = 'fulltsv'.
            lv_fulltablecount = lv_fulltablecount + 1.
        ENDLOOP.
        IF lv_fulltablecount >= iv_maxfulltable.
            CONTINUE.
        ENDIF.

        IF lv_append = abap_true.
            CONTINUE.
        ENDIF.

        " human readable version
        CLEAR lt_filecontent.
        lv_success = me->get_config_table_lines(
            EXPORTING
                iv_tabname = lv_tabname
                iv_request = iv_cs_request
                iv_tsv = abap_false
                iv_maxrow = iv_maxrow
            IMPORTING
                abaptext = lt_filecontent
             ).
        IF lv_success <> abap_true.
            me->write_telemetry( iv_message = |fail to get config full lines for { lv_tabname }| ).
            CONTINUE.
        ENDIF.

        lv_filecontent = concat_lines_of( table = lt_filecontent sep = CL_ABAP_CHAR_UTILITIES=>CR_LF ).

        " align with GUI_DOWNLOAD which adds a blank line
        lv_filecontent = lv_filecontent && CL_ABAP_CHAR_UTILITIES=>CR_LF.

        APPEND VALUE ts_commit_object(
            devclass = c_config
            objname = wakey-objname
            objtype = wakey-object
            objtype2 = wakey-object
            fugr = ''
            progcls = 'full'
            delflag = abap_false
            verno = 1
            filecontent = lv_filecontent
            ) TO it_commit_objects.

        " TSV version
        CLEAR lt_filecontent.
        lv_success = me->get_config_table_lines(
            EXPORTING
                iv_tabname = lv_tabname
                iv_request = iv_cs_request
                iv_tsv = abap_true
                iv_maxrow = iv_maxrow
            IMPORTING
                abaptext = lt_filecontent
             ).
        IF lv_success <> abap_true.
            me->write_telemetry( iv_message = |fail to get config full lines for { lv_tabname }| ).
            CONTINUE.
        ENDIF.

        lv_filecontent = concat_lines_of( table = lt_filecontent sep = CL_ABAP_CHAR_UTILITIES=>CR_LF ).

        " align with GUI_DOWNLOAD which adds a blank line
        lv_filecontent = lv_filecontent && CL_ABAP_CHAR_UTILITIES=>CR_LF.

        APPEND VALUE ts_commit_object(
            devclass = c_config
            objname = wakey-objname
            objtype = wakey-object
            objtype2 = wakey-object
            fugr = ''
            progcls = 'fulltsv'
            delflag = abap_false
            verno = 1
            filecontent = lv_filecontent
            ) TO it_commit_objects.

    ENDLOOP.

  ENDMETHOD.


  METHOD GET_FUGR.

    DATA lv_pname_filter TYPE string.
    SELECT SINGLE pname INTO @lv_pname_filter FROM tfdir WHERE funcname = @iv_objname.
    IF strlen( lv_pname_filter ) < 4.
        ev_fugrname = ''.
    ELSE.
        ev_fugrname = lv_pname_filter+4.    " SAPL... as prefix for names in pname field
    ENDIF.

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
         RETURN.
      ENDIF.

      linecount = lines( abaptext ).
      rv_success = abap_true.

  ENDMETHOD.


  METHOD GET_CODE_LINES_ENHO.

    DATA lv_enhancement_id TYPE ENHNAME.
    DATA r_vers TYPE REF TO IF_ENH_TOOL.
    DATA lv_state type r3state.
    DATA lv_tooltype TYPE enhtooltype.
    DATA:   l_clas_data   TYPE ENHCLASSMETHDATA,
            l_fugr_data   TYPE ENHFUGRDATA,
            l_hook_data   TYPE ENH_HOOK_ADMIN.

    rv_success = abap_false.

    lv_enhancement_id = iv_objname.

    TRY.
        CALL METHOD cl_enh_factory=>get_enhancement
            EXPORTING
                enhancement_id = lv_enhancement_id
                versno = iv_version
                rfcdestination = iv_logdest
            RECEIVING
                enhancement = r_vers.
    CATCH CX_ENH_IO_ERROR.
        RETURN.
    CATCH CX_ENH_ROOT.
        RETURN.
    ENDTRY.

    cl_enh_cache=>refresh_enh_cache( enhname = lv_enhancement_id ).
    lv_tooltype = r_vers->get_tool( ).

    lv_state = 'A'.

    CASE lv_tooltype.

        WHEN 'CLASENH' OR 'INTFENH'.
            TRY.
                CALL METHOD r_vers->if_enh_object~get_data
                    EXPORTING
                        version = lv_state
                    IMPORTING
                        DATA    = l_clas_data.
                APPEND LINES OF l_clas_data-enh_eimpsource TO abaptext.
                LOOP AT l_clas_data-enh_methsources INTO DATA(wa_enh_meth).
                    APPEND '' TO abaptext.
                    APPEND '' TO abaptext.
                    APPEND LINES OF wa_enh_meth-source TO abaptext.
                ENDLOOP.
                rv_success = abap_true.
            CATCH CX_ENH_NO_VALID_INPUT_TYPE .
                RETURN.
            ENDTRY.
        WHEN 'FUGRENH'.
            TRY.
                CALL METHOD r_vers->if_enh_object~get_data
                    EXPORTING
                        version = lv_state
                    IMPORTING
                        DATA    = l_fugr_data.
                " TODO function group case
            CATCH CX_ENH_NO_VALID_INPUT_TYPE .
                RETURN.
            ENDTRY.
        WHEN 'BADI_IMPL'.
            " BAdI case has no code to produce
            RETURN.
        WHEN 'HOOK_IMPL'.
            TRY.
                CALL METHOD r_vers->if_enh_object~get_data
                    EXPORTING
                        version = lv_state
                    IMPORTING
                        DATA    = l_hook_data.
                 " stitch all enhancement ID source code lines to one file
                 LOOP AT l_hook_data-hook_impls INTO DATA(wa_hook_impl).
                    APPEND |ENHANCEMENT { wa_hook_impl-id } { iv_objname }.| TO abaptext.
                    APPEND LINES OF wa_hook_impl-source TO abaptext.
                    APPEND |ENDENHANCEMENT.| TO abaptext.
                 ENDLOOP.
                 APPEND '' TO abaptext.
                 rv_success = abap_true.
            CATCH CX_ENH_NO_VALID_INPUT_TYPE .
                RETURN.
            ENDTRY.
        WHEN OTHERS.
            RETURN.
    ENDCASE.

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

        IF waver-objtype = 'ENHO'.
            lv_success = me->get_code_lines_enho(
                EXPORTING
                    iv_version = waver-objversionno
                    iv_objname = waver-objname
                    iv_logdest = ''
                IMPORTING
                    abaptext = lt_abaptext
                    ).
        ELSE.
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
        ENDIF.
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


  METHOD GET_TABLE_SCHEMA.

    DATA:
        DD02TV_TAB TYPE TABLE OF DD02TV,
        DD02V_TAB TYPE TABLE OF DD02V,
        DD03TV_TAB TYPE TABLE OF DD03TV,
        DD03V_TAB TYPE TABLE OF DD03V,
        DD05V_TAB TYPE TABLE OF DD05V,
        DD08TV_TAB TYPE TABLE OF DD08TV,
        DD08V_TAB TYPE TABLE OF DD08V,
        DD35V_TAB TYPE TABLE OF DD35V,
        DD36V_TAB TYPE TABLE OF DD36V.
    DATA DD03P_TAB_A TYPE TABLE OF DD03P.
    DATA lv_objname TYPE VRSD-OBJNAME.
    DATA lv_tabname TYPE DD02L-TABNAME.
    DATA ls_data_table_field TYPE ty_data_table_field.
    DATA lv_eddtext TYPE abap_bool VALUE abap_true.

    rv_success = abap_false.

    lv_objname = iv_objname.
    CALL FUNCTION 'SVRS_GET_VERSION_TABD_40'
        EXPORTING
            object_name = lv_objname
            versno      = iv_version
        TABLES
            dd02v_tab   = dd02v_tab
            dd03v_tab   = dd03v_tab
            dd05v_tab   = dd05v_tab
            dd08v_tab   = dd08v_tab
            dd35v_tab   = dd35v_tab
            dd36v_tab   = dd36v_tab
            dd02tv_tab  = dd02tv_tab
            dd03tv_tab  = dd03tv_tab
            dd08tv_tab  = dd08tv_tab
        EXCEPTIONS
            no_version  = 01
            OTHERS = 02.
    IF sy-subrc <> 0.
        me->write_telemetry( iv_message = |BUILD_DATA_TABLE_CONTENT fails in fetching data table header for { iv_objname } version { iv_version }| ).
        RETURN.
    ENDIF.

    IF lines( dd02v_tab ) = 0.
        me->write_telemetry( iv_message = |BUILD_DATA_TABLE_CONTENT fails in fetching data table header for { iv_objname } with enough version| ).
        RETURN.
    ENDIF.

    " get English (default German) field description
    lv_tabname = iv_objname.
    CALL FUNCTION 'DD_TABL_GET'
        EXPORTING
            langu = 'E'
            tabl_name = lv_tabname
            withtext = abap_true
        TABLES
            DD03P_TAB_A = DD03P_TAB_A
        EXCEPTIONS
            ACCESS_FAILURE = 1.
    IF sy-subrc <> 0.
        lv_eddtext = abap_false.
    ENDIF.

    ev_desc-dd02v-tabname = dd02v_tab[ 1 ]-tabname.
    ev_desc-dd02v-ddlanguage = dd02v_tab[ 1 ]-ddlanguage.
    ev_desc-dd02v-tabclass = dd02v_tab[ 1 ]-tabclass.
    ev_desc-dd02v-clidep = dd02v_tab[ 1 ]-clidep.
    IF line_exists( dd02tv_tab[ ddlanguage = 'E' ] ).
        ev_desc-dd02v-ddtext = dd02tv_tab[ ddlanguage = 'E' ]-ddtext.
    ELSE.
        ev_desc-dd02v-ddtext = dd02v_tab[ 1 ]-ddtext.
    ENDIF.
    ev_desc-dd02v-mainflag = dd02v_tab[ 1 ]-mainflag.
    ev_desc-dd02v-contflag = dd02v_tab[ 1 ]-contflag.
    ev_desc-dd02v-shlpexi = dd02v_tab[ 1 ]-shlpexi.

    LOOP AT dd03v_tab INTO DATA(wa_dd03v).
        CLEAR ls_data_table_field.
        ls_data_table_field-fieldname = wa_dd03v-fieldname.
        ls_data_table_field-keyflag = wa_dd03v-keyflag.
        ls_data_table_field-rollname = wa_dd03v-rollname.
        ls_data_table_field-adminfield = wa_dd03v-adminfield.
        ls_data_table_field-datatype = wa_dd03v-datatype.
        ls_data_table_field-leng = wa_dd03v-leng.
        ls_data_table_field-decimals = wa_dd03v-decimals.
        ls_data_table_field-notnull = wa_dd03v-notnull.
        IF lv_eddtext = abap_true AND line_exists( DD03P_TAB_A[ fieldname = wa_dd03v-fieldname ] ).
            ls_data_table_field-ddtext = DD03P_TAB_A[ fieldname = wa_dd03v-fieldname ]-ddtext.
        ELSE.
            ls_data_table_field-ddtext = wa_dd03v-ddtext.
        ENDIF.
        IF iv_escape = abap_true.
            REPLACE ALL OCCURRENCES OF ',' IN ls_data_table_field-ddtext WITH '`'.
        ENDIF.
        ls_data_table_field-domname = wa_dd03v-domname.
        ls_data_table_field-shlporigin = wa_dd03v-shlporigin.
        ls_data_table_field-comptype = wa_dd03v-comptype.
        APPEND ls_data_table_field TO ev_desc-dd03v.
    ENDLOOP.

    rv_success = abap_true.

  ENDMETHOD.


  METHOD BUILD_DATA_TABLE_CONTENT.

    DATA ls_data_table_desc TYPE ty_data_table_desc.

    rv_success = me->get_table_schema(
        EXPORTING
            iv_objname = iv_objname
            iv_version = iv_version
        IMPORTING
            ev_desc = ls_data_table_desc
             ).
    CHECK rv_success = abap_true.

    /UI2/CL_JSON=>serialize(
        EXPORTING
            !data = ls_data_table_desc
            pretty_name = /ui2/cl_json=>pretty_mode-low_case
        RECEIVING
            r_json = ev_filecontent
             ).

    " beautify a bit with line breaks for code diff benefit
    REPLACE ALL OCCURRENCES OF ',' IN ev_filecontent WITH |,{ CL_ABAP_CHAR_UTILITIES=>CR_LF }|.
    LOOP AT ls_data_table_desc-dd03v ASSIGNING FIELD-SYMBOL(<fs>).
        REPLACE ALL OCCURRENCES OF '`' IN <fs>-ddtext WITH ','.
    ENDLOOP.

    SPLIT ev_filecontent AT CL_ABAP_CHAR_UTILITIES=>CR_LF INTO TABLE et_filecontent.

    " align with GUI_DOWNLOAD which adds a blank line
    ev_filecontent = ev_filecontent && CL_ABAP_CHAR_UTILITIES=>CR_LF.

    rv_success = abap_true.

  ENDMETHOD.


  METHOD BUILD_SCHEMA_CONTENT_ACTIVE.

    TYPES ts_comment TYPE t52c3.
    DATA lv_ltext TYPE t52cc_t-ltext.
    DATA ls_desc TYPE t52cc.
    DATA lv_cnt TYPE t52ba-pwert.
    DATA lt_comments TYPE TABLE OF ts_comment.
    DATA lv_line TYPE c LENGTH 72.
    DATA lv_text TYPE string.
    DATA lv_maxindent TYPE i.
    DATA lv_indent TYPE i.
    DATA lv_funco TYPE string.

    " fetch schema's meta data
    SELECT SINGLE * FROM t52cc INTO @ls_desc WHERE sname = @iv_schemaname.
    SELECT SINGLE pwert FROM t52ba INTO @lv_cnt WHERE potyp = 'SCHE' AND ponam = @iv_schemaname AND pattr = 'CNT'.  "#EC WARNOK

    " fetch schema's description text
    SELECT SINGLE ltext FROM t52cc_t INTO @lv_ltext WHERE sname = @iv_schemaname.   "#EC WARNOK "#EC CI_SGLSELECT

    " meta data for this schema, don't want to bother to add an additional XML/JSON description file
    APPEND |* Schema                : { iv_schemaname }| TO et_filecontent.
    APPEND |* Description           : { lv_ltext }| TO et_filecontent.
    APPEND |* Executable            : { ls_desc-execu }| TO et_filecontent.
    APPEND |* Country Grouping      : { lv_cnt }| TO et_filecontent.
    APPEND |* Owner                 : { ls_desc-respu }| TO et_filecontent.
    APPEND |* Creation Date         : { ls_desc-cdate }| TO et_filecontent.
    APPEND |* Only Changed by Owner : { ls_desc-execu }| TO et_filecontent.
    APPEND |* Version               : { ls_desc-uvers }| TO et_filecontent.
    APPEND |* Last Changed By       : { ls_desc-uname }| TO et_filecontent.
    APPEND |* Last Changed Date     : { ls_desc-udate }| TO et_filecontent.
    APPEND |* Last Changed Time     : { ls_desc-utime }| TO et_filecontent.

    " fetch schema instructions' comments
    SELECT * FROM t52c3 INTO TABLE @lt_comments
        WHERE schem = @iv_schemaname AND spras = @c_en.

    IF iv_indented = abap_true.

        " calculate max indents
        lv_indent = 0.
        lv_maxindent = 0.
        SELECT * FROM t52c1 INTO @DATA(wa_instr1) WHERE schem = @iv_schemaname.
            IF wa_instr1-delet = '*'.
                CONTINUE.
            ENDIF.
            CASE wa_instr1-funco.
                WHEN 'IF' OR 'BINI' OR 'BEND' OR 'BDAY' OR 'LPBEG'.
                    lv_indent = lv_indent + 2.
                    IF lv_maxindent < lv_indent.
                        lv_maxindent = lv_indent.
                    ENDIF.
                WHEN 'ENDIF' OR 'EINI' OR 'EEND' OR 'EDAY' OR 'LPEND'.
                    lv_indent = lv_indent - 2.
                WHEN OTHERS.
                    " no action
            ENDCASE.
        ENDSELECT.

        " instruction table header
        DATA lv_header TYPE string VALUE 'Par1  Par2  Par3  Par4  D Text'.
        DO lv_maxindent TIMES.
            lv_header = ` ` && lv_header.
        ENDDO.
        lv_header = `Func. ` && lv_header.
        APPEND lv_header TO et_filecontent.

        lv_indent = 0.

        " fetch schema instructions
        SELECT * FROM t52c1 INTO @DATA(wa_instr2) WHERE schem = @iv_schemaname.
            CLEAR lv_line.
            lv_line+0 = wa_instr2-parm1.
            lv_line+6 = wa_instr2-parm2.
            lv_line+12 = wa_instr2-parm3.
            lv_line+18 = wa_instr2-parm4.
            lv_line+24 = wa_instr2-delet.
            lv_line+26 = lt_comments[ seqno = wa_instr2-textid ]-scdes.
            CLEAR lv_funco.
            lv_funco = wa_instr2-funco.
            CASE wa_instr2-funco.
                WHEN 'IF' OR 'BINI' OR 'BEND' OR 'BDAY' OR 'LPBEG'.
                    DO lv_indent TIMES.
                        lv_funco = ` ` && lv_funco.
                    ENDDO.
                    IF wa_instr2-delet <> '*'.
                        lv_indent = lv_indent + 2.
                    ENDIF.
                WHEN 'ELSE'.
                    IF wa_instr2-delet <> '*'.
                        lv_indent = lv_indent - 2.
                    ENDIF.
                    DO lv_indent TIMES.
                        lv_funco = ` ` && lv_funco.
                    ENDDO.
                    IF wa_instr2-delet <> '*'.
                        lv_indent = lv_indent + 2.
                    ENDIF.
                WHEN 'ENDIF' OR 'EINI' OR 'EEND' OR 'EDAY' OR 'LPEND'.
                    IF wa_instr2-delet <> '*'.
                        lv_indent = lv_indent - 2.
                    ENDIF.
                    DO lv_indent TIMES.
                        lv_funco = ` ` && lv_funco.
                    ENDDO.
                WHEN OTHERS.
                    DO lv_indent TIMES.
                        lv_funco = ` ` && lv_funco.
                    ENDDO.
            ENDCASE.
            DATA(lv_length) = strlen( lv_funco ).
            DO lv_maxindent + 6 - lv_length TIMES.
                lv_funco = lv_funco && ` `.
            ENDDO.
            APPEND |{ lv_funco }{ lv_line }| TO et_filecontent.
        ENDSELECT.

    ELSE.

        " instruction table header
        APPEND 'Func. Par1  Par2  Par3  Par4  D Text' TO et_filecontent.

        " fetch schema instructions
        SELECT * FROM t52c1 INTO @DATA(wa_instr3) WHERE schem = @iv_schemaname.
            CLEAR lv_line.
            lv_line+0 = wa_instr3-funco.
            lv_line+6 = wa_instr3-parm1.
            lv_line+12 = wa_instr3-parm2.
            lv_line+18 = wa_instr3-parm3.
            lv_line+24 = wa_instr3-parm4.
            lv_line+30 = wa_instr3-delet.
            lv_line+32 = lt_comments[ seqno = wa_instr3-textid ]-scdes.
            APPEND lv_line TO et_filecontent.
        ENDSELECT.

    ENDIF.

  ENDMETHOD.


  METHOD BUILD_PCR_CONTENT_ACTIVE.

    DATA lv_line TYPE c LENGTH 91.
    DATA lv_index TYPE i.
    DATA lv_text TYPE string.
    DATA lv_t TYPE c.
    DATA lv_ltext TYPE t52ce_t-ltext.
    DATA ls_desc TYPE t52ce.
    DATA lv_esg TYPE string.
    DATA lv_wgt TYPE string.
    DATA lv_cnt TYPE t52ba-pwert.
    DATA lv_first TYPE c VALUE abap_true.

    " fetch PCR's meta data
    SELECT SINGLE * FROM t52ce INTO @ls_desc WHERE cname = @iv_pcrname.
    SELECT SINGLE pwert FROM t52ba INTO @lv_cnt WHERE potyp = 'CYCL' AND ponam = @iv_pcrname AND pattr = 'CNT'. "#EC WARNOK

    " fetch PCR's description text
    SELECT SINGLE ltext FROM t52ce_t INTO @lv_ltext WHERE cname = @iv_pcrname AND sprsl = @c_en.

    " meta data for this PCR, don't want to bother to add an additional XML/JSON description file
    APPEND |* PCR                   : { iv_pcrname }| TO et_filecontent.
    APPEND |* Description           : { lv_ltext }| TO et_filecontent.
    APPEND |* Country Grouping      : { lv_cnt }| TO et_filecontent.
    APPEND |* Owner                 : { ls_desc-respu }| TO et_filecontent.
    APPEND |* Creation Date         : { ls_desc-cdate }| TO et_filecontent.
    APPEND |* Only Changed by Owner : { ls_desc-relea }| TO et_filecontent.
    APPEND |* Last Changed By       : { ls_desc-uname }| TO et_filecontent.
    APPEND |* Last Changed Date     : { ls_desc-udate }| TO et_filecontent.
    APPEND |* Last Changed Time     : { ls_desc-utime }| TO et_filecontent.

    " fetch PCR instructions
    lv_index = 1.
    SELECT * FROM t52c5 INTO @DATA(wa_instr) WHERE ccycl = @iv_pcrname.
        IF lv_first = abap_true OR wa_instr-abart <> lv_esg OR wa_instr-lgart <> lv_wgt.
            APPEND '' TO et_filecontent.
            " show ES group and wage type
            APPEND |* ES Group { wa_instr-abart }, Wage/Time Type { wa_instr-lgart }| TO et_filecontent.
            " instruction table header
            APPEND 'Var.Key CL T Operation  Operation  Operation  Operation  Operation  Operation *' TO et_filecontent.
            lv_first = abap_false.
            lv_esg = wa_instr-abart.
            lv_wgt = wa_instr-lgart.
            lv_index = 1.
        ENDIF.

        CLEAR lv_line.
        lv_line+0 = wa_instr-vargt.
        lv_line+8 = wa_instr-seqno.
        lv_text = wa_instr-vinfo.
        IF strlen( lv_text ) > 0.
            lv_t = lv_text+0(1).
            lv_text = lv_text+1.
        ELSE.
            lv_t = space.
        ENDIF.
        lv_line+11 = lv_t.
        lv_line+13 = lv_text.
        APPEND lv_line TO et_filecontent.
        lv_index = lv_index + 1.
    ENDSELECT.

  ENDMETHOD.


  METHOD GET_CONFIG_LINES_HEADER.

    DATA lv_objname TYPE  e071-obj_name.
    DATA lv_version_no TYPE i.
    DATA lt_objversions TYPE tty_version_no.
    DATA wafield TYPE ty_data_table_field.
    " 30 field name (max length) round up to 4 tabs x 8 plus a colon
    DATA lv_field(33) TYPE c.
    DATA lv_field_desc TYPE string.
    DATA lv_strlen TYPE i.
    DATA lv_header TYPE string.

    rv_success = abap_false.

    " fetch table version
    lv_objname = iv_tabname.
    IF lv_objname CP 'Z*' OR lv_objname CP 'Y*'.
        rv_success = me->get_versions_no(
            EXPORTING
                iv_objname = lv_objname
                iv_objtype = 'TABL'
                iv_mode = c_latest_version
                iv_date = iv_request-h-as4date
                iv_time = iv_request-h-as4time
                iv_findtest = abap_false
            IMPORTING
                ev_version_no = lv_version_no
            CHANGING
                cht_objversions = lt_objversions
                ).
    ELSE.
        rv_success = me->get_versions_no(
            EXPORTING
                iv_objname = lv_objname
                iv_objtype = 'TABL'
                iv_mode = c_active_version
                iv_findtest = abap_false
            IMPORTING
                ev_version_no = lv_version_no
            CHANGING
                cht_objversions = lt_objversions
                ).
    ENDIF.
    CHECK rv_success = abap_true.
    CHECK lv_version_no > 0.

    " fetch table schema for column specs
    rv_success = me->get_table_schema(
        EXPORTING
            iv_objname = lv_objname
            iv_version = lt_objversions[ 1 ]-objversionno
            iv_escape = abap_false
        IMPORTING
            ev_desc = ev_desc
             ).
    CHECK rv_success = abap_true.

    " write file header for table and fields
    APPEND 'Table header part is auto generated and not part of engineer changes to the row(s).' TO abaptext.
    APPEND |Table { iv_tabname }: { ev_desc-dd02v-ddtext }| TO abaptext.
    APPEND |Changed in TR { iv_request-h-trkorr } at { iv_request-h-as4date } { iv_request-h-as4time }| TO abaptext.

    IF iv_tsv = abap_true.
        LOOP AT ev_desc-dd03v INTO wafield.
            lv_header = lv_header && wafield-fieldname && cl_abap_char_utilities=>horizontal_tab.
        ENDLOOP.
        APPEND lv_header TO abaptext.
    ELSE.
        APPEND 'Fields:' TO abaptext.
        LOOP AT ev_desc-dd03v INTO wafield.
            lv_strlen = strlen( wafield-fieldname ).
            lv_field = '                                :'.
            lv_field+0(lv_strlen) = wafield-fieldname.
            lv_field_desc = wafield-ddtext.
            APPEND |    { lv_field } { lv_field_desc }| TO abaptext.
        ENDLOOP.
        APPEND '' TO abaptext.
    ENDIF.

  ENDMETHOD.


  METHOD GET_CONFIG_DELTA_LINES.

    TYPES: line(5000) TYPE c.
    DATA lv_tabname TYPE dd02l-tabname.
    DATA lt_entry_tab TYPE TABLE OF line.
    DATA ls_data_table_desc TYPE ty_data_table_desc.
    FIELD-SYMBOLS <fs_entry> TYPE line.
    DATA lv_entry TYPE string.

    rv_success = me->get_config_lines_header(
        EXPORTING
            iv_tabname = iv_tabname
            iv_request = iv_request
        IMPORTING
            ev_desc = ls_data_table_desc
        CHANGING
            abaptext = abaptext
         ).
    CHECK rv_success = abap_true.

    IF iv_deltastats = abap_false.
        APPEND 'Rows:' TO abaptext.
        APPEND '' TO abaptext.
    ENDIF.

    " fetch config changes as table rows
    lv_tabname = iv_tabname.
    CALL FUNCTION 'SRTT_TABLE_GET_BY_KEYLIST'
        EXPORTING
            tabname = lv_tabname
            mtype = iv_objtype
            mtabname = iv_mtabname
        TABLES
            e071k_tab = iv_request-keys
            entry_tab = lt_entry_tab
        EXCEPTIONS
            OTHERS = 1.
    IF sy-subrc <> 0.
        me->write_telemetry( iv_message = |GET_CONFIG_DELTA_LINES fails in SRTT_TABLE_GET_BY_KEYLIST call with { iv_tabname }| ).
        rv_success = abap_false.
        RETURN.
    ENDIF.

    " extract field values from entry line text
    ev_rows = 1.
    LOOP AT lt_entry_tab ASSIGNING <fs_entry>.
        IF iv_deltastats = abap_false.
            APPEND |Row { ev_rows }| TO abaptext.
            lv_entry = <fs_entry>.
            me->get_column_values(
                EXPORTING
                    iv_data_table_desc = ls_data_table_desc
                    iv_line = lv_entry
                 CHANGING
                    cht_filecontent = abaptext
                     ).
            APPEND '' TO abaptext.
        ENDIF.
        ev_rows = ev_rows + 1.
    ENDLOOP.

    rv_success = abap_true.

  ENDMETHOD.


  METHOD GET_CONFIG_TABLE_LINES.

    DATA lv_tabname TYPE dd02l-tabname.
    DATA lt_entry_tab TYPE TABLE OF line.
    DATA ls_data_table_desc TYPE ty_data_table_desc.
    DATA wafield TYPE ty_data_table_field.
    DATA lv_row TYPE i.
    DATA lv_field(33) TYPE c.
    DATA lv_strlen TYPE i.
    DATA lv_rowcount TYPE i.
    DATA lv_fieldname TYPE string.
    FIELD-SYMBOLS:
        <fs_table> TYPE ANY TABLE,
        <fs_struct> TYPE ANY,
        <fs_field> TYPE ANY.
    DATA: lo_typedesc    TYPE REF TO cl_abap_typedescr,
          lo_tabtype     TYPE REF TO cl_abap_tabledescr,
          lo_struct_type TYPE REF TO cl_abap_structdescr,
          lr_data        TYPE REF TO data,
          lt_comp_tab    TYPE cl_abap_structdescr=>component_table,
          ls_comp_fld    TYPE cl_abap_structdescr=>component.
    DATA lv_value TYPE string.
    DATA lv_line TYPE string.

    " prepare for dynamic SQL query for all rows
    CALL METHOD cl_abap_typedescr=>describe_by_name
        EXPORTING
            p_name = iv_tabname
        RECEIVING
            p_descr_ref = lo_typedesc
        EXCEPTIONS
            TYPE_NOT_FOUND = 1.
    IF sy-subrc <> 0.
        me->write_telemetry( iv_message = |GET_CONFIG_TABLE_LINES fails to find type info of { iv_tabname }| ).
        rv_success = abap_false.
        RETURN.
    ENDIF.

    lv_tabname = iv_tabname.
    SELECT FROM (lv_tabname) FIELDS COUNT( * ) INTO @lv_rowcount.
    IF lv_rowcount > iv_maxrow.
        me->write_telemetry( iv_message = |GET_CONFIG_TABLE_LINES met table { iv_tabname } with { lv_rowcount } rows, more than { iv_maxrow }| ).
    ENDIF.

    rv_success = me->get_config_lines_header(
        EXPORTING
            iv_tabname = iv_tabname
            iv_request = iv_request
            iv_tsv = iv_tsv
        IMPORTING
            ev_desc = ls_data_table_desc
        CHANGING
            abaptext = abaptext
         ).
    CHECK rv_success = abap_true.

    lo_struct_type ?= lo_typedesc.
    lt_comp_tab = lo_struct_type->get_components( ).
    lo_struct_type = cl_abap_structdescr=>create( lt_comp_tab ).
    lo_tabtype = cl_abap_tabledescr=>create( lo_struct_type ).
    CREATE DATA lr_data TYPE HANDLE lo_tabtype.
    ASSIGN lr_data->* TO <fs_table>.

    " dynamic query for given table name
    SELECT * FROM (lv_tabname) INTO CORRESPONDING FIELDS OF TABLE <fs_table> UP TO iv_maxrow ROWS .

    IF iv_tsv = abap_true.
        LOOP AT <fs_table> ASSIGNING FIELD-SYMBOL(<fs_row1>).
            CLEAR lv_line.
            LOOP AT ls_data_table_desc-dd03v INTO wafield.
                lv_fieldname = wafield-fieldname.
                ASSIGN COMPONENT lv_fieldname OF STRUCTURE <fs_row1> TO <fs_field>.
                IF sy-subrc <> 0.
                    lv_value = ''.
                ELSE.
                    lv_value = <fs_field>.
                ENDIF.
                lv_line = lv_line && |{ lv_value }| && CL_ABAP_CHAR_UTILITIES=>horizontal_tab.
            ENDLOOP.
            APPEND lv_line TO abaptext.
        ENDLOOP.
        IF lv_rowcount > iv_maxrow.
            APPEND '...' TO abaptext.
        ENDIF.
    ELSE.
        APPEND 'Rows:' TO abaptext.
        APPEND '' TO abaptext.
        lv_row = 1.
        LOOP AT <fs_table> ASSIGNING FIELD-SYMBOL(<fs_row2>).
            APPEND |Row { lv_row }| TO abaptext.
            APPEND '' TO abaptext.
            LOOP AT ls_data_table_desc-dd03v INTO wafield.
                lv_fieldname = wafield-fieldname.
                ASSIGN COMPONENT lv_fieldname OF STRUCTURE <fs_row2> TO <fs_field>.
                IF sy-subrc <> 0.
                    lv_value = ''.
                ELSE.
                    lv_value = <fs_field>.
                ENDIF.
                lv_strlen = strlen( wafield-fieldname ).
                lv_field = '                                :'.
                lv_field+0(lv_strlen) = wafield-fieldname.
                APPEND |    { lv_field } { lv_value }| TO abaptext.
            ENDLOOP.
            APPEND '' TO abaptext.
            lv_row = lv_row + 1.
        ENDLOOP.
        IF lv_rowcount > iv_maxrow.
            APPEND '...' TO abaptext.
        ENDIF.
    ENDIF.

    rv_success = abap_true.

  ENDMETHOD.


  METHOD BUILD_CONFIG_LOG.

    DATA lv_tabname TYPE e071-obj_name.
    DATA ls_data_table_desc TYPE ty_data_table_desc.
    DATA lv_row TYPE i.
    FIELD-SYMBOLS <fs_entry> TYPE line.
    DATA lv_entry TYPE dbtablog.
    DATA lv_string TYPE string.
    DATA fromday TYPE d.
    DATA fromtime TYPE t.
    DATA lt_log_list TYPE STPRT_LOG_STABLE_TYPE.
    DATA lt_obj_cnt_list TYPE STPRT_OBJ_APP_CNT_LIST_TYPE.
    DATA lt_user_list TYPE stprt_user_name_list_type.

    APPEND VALUE stprt_user_name_sel_line_type( sign = 'I' option = 'EQ' low = iv_user high = '' ) TO lt_user_list.

    CALL FUNCTION 'DBLOG_READ_WITH_STATISTIC'
        EXPORTING
            from_day = iv_fromdate
            from_time = iv_fromtime
            to_day = iv_todate
            to_time = iv_totime
            user_list = lt_user_list
        CHANGING
            log_list = lt_log_list
            obj_cnt_list = lt_obj_cnt_list
        EXCEPTIONS
            archive_access_error = 1
            no_archives_found    = 2
            OTHERS               = 3.
    CHECK sy-subrc = 0.

    lv_tabname = iv_tabname.
    rv_success = me->get_table_schema(
        EXPORTING
            iv_objname = lv_tabname
            iv_version = 0
            iv_escape = abap_false
        IMPORTING
            ev_desc = ls_data_table_desc
             ).
    CHECK rv_success = abap_true.

    DATA unicode_2_abap TYPE REF TO cl_abap_conv_in_ce.
    FIELD-SYMBOLS <fsx> TYPE x.

    lv_entry = lt_log_list[ tabname = lv_tabname ]. "#EC CI_SORTSEQ
    DATA datalen TYPE i.
    datalen = lv_entry-dataln.
    ASSIGN lv_entry-logdata(datalen) TO <fsx>.

    unicode_2_abap = cl_abap_conv_in_ce=>create( encoding = '4102' ).
    unicode_2_abap->convert(
        EXPORTING input = <fsx>
        IMPORTING data  = lv_string
         ).

    APPEND 'Rows:' TO abaptext.
    APPEND '' TO abaptext.
    me->get_column_values(
        EXPORTING
            iv_data_table_desc = ls_data_table_desc
            iv_line = lv_string
         CHANGING
            cht_filecontent = abaptext
             ).
    APPEND '' TO abaptext.

  ENDMETHOD.


  METHOD BUILD_PACKNUMBER_TEXT.

    DATA lv_leng TYPE i.
    DATA lv_index TYPE i.
    DATA lv_char_val TYPE char1.
    DATA lv_int_val TYPE i.
    DATA lv_int_val_digit TYPE i.
    DATA lv_mask1 TYPE x LENGTH 2 VALUE '00F0'.
    DATA lv_mask2 TYPE x LENGTH 2 VALUE '000F'.
    DATA lv_mask3 TYPE x LENGTH 2 VALUE 'F000'.
    DATA lv_mask4 TYPE x LENGTH 2 VALUE '0F00'.
    DATA lv_int_x TYPE x LENGTH 2.
    DATA lv_digit TYPE c.
    DATA lv_packnumber TYPE string.

    lv_leng = iv_leng + 1.

    " round up to multiply of 4
    IF lv_leng MOD 4 <> 0.
        lv_leng = ( ( lv_leng DIV 4 ) + 1 ) * 4.
    ENDIF.

    " bytes needed
    lv_leng = lv_leng DIV 4.

    lv_index = iv_off.
    DO lv_leng TIMES.
        " character to its integer value
        CHECK lv_index + 1 <= strlen( iv_line ).

        lv_char_val = iv_line+lv_index(1).
        CL_TREX_CHAR_UTILITY=>char_to_int(
            EXPORTING
                char_val = lv_char_val
            RECEIVING
                int_val = lv_int_val
                 ).

        " 1st digit
        lv_int_x = lv_int_val.
        lv_int_x = lv_int_x BIT-AND lv_mask1.
        lv_int_x = lv_int_x DIV 16.
        lv_int_val_digit = lv_int_x.
        lv_digit = lv_int_val_digit.
        lv_packnumber = lv_packnumber && lv_digit.

        " 2nd digit
        lv_int_x = lv_int_val.
        lv_int_x = lv_int_x BIT-AND lv_mask2.
        lv_int_val_digit = lv_int_x.
        lv_digit = lv_int_val_digit.
        lv_packnumber = lv_packnumber && lv_digit.

        " 3rd digit
        lv_int_x = lv_int_val.
        lv_int_x = lv_int_x BIT-AND lv_mask3.
        lv_int_x = lv_int_x DIV 4096.
        lv_int_val_digit = lv_int_x.
        lv_digit = lv_int_val_digit.
        lv_packnumber = lv_packnumber && lv_digit.

        IF sy-index = lv_leng.
            " sign +/-
            lv_int_x = lv_int_val.
            lv_int_x = lv_int_x BIT-AND lv_mask4.
            lv_int_x = lv_int_x DIV 256.
            lv_int_val_digit = lv_int_x.
            IF lv_int_val_digit <> 12.
                lv_packnumber = '-' && lv_packnumber.
            ENDIF.
        ELSE.
            " 4th digit
            lv_int_x = lv_int_val.
            lv_int_x = lv_int_x BIT-AND lv_mask4.
            lv_int_x = lv_int_x DIV 256.
            lv_int_val_digit = lv_int_x.
            lv_digit = lv_int_val_digit.
            lv_packnumber = lv_packnumber && lv_digit.
        ENDIF.

        lv_index = lv_index + 1.
    ENDDO.

    " apply decimal dot
    IF iv_dec IS NOT INITIAL.
        DATA(lv_strlen) = strlen( lv_packnumber ).
        DATA(lv_left) = lv_strlen - iv_dec.
        lv_packnumber = |{ lv_packnumber+0(lv_left) }.{ lv_packnumber+lv_left(iv_dec) }|.
    ENDIF.

    ev_text = lv_packnumber.
    ev_leng = lv_leng.

  ENDMETHOD.


  METHOD GET_TR_CREATION.

    DATA lv_cant_open TYPE c.
    DATA lv_logfile TYPE tstrf01-file.
    DATA lv_sysname TYPE tcesyst-sysname.
    DATA lv_timestamp TYPE tstamp.
    DATA lv_client TYPE t000-mandt.
    DATA lt_lines TYPE TABLE OF trlog.
    DATA lv_line TYPE string.
    DATA lt_parts TYPE TABLE OF string.

    rv_success = abap_false.

    lv_sysname = iv_trid+0(3).
    CALL FUNCTION 'STRF_OPEN_PROT'
          EXPORTING
            access     = 'R'
            acttype    = 'Z'
            dirtype    = 'T'
            trkorr     = iv_trid
            sysname    = lv_sysname
          IMPORTING
            cant_open  = lv_cant_open
            file       = lv_logfile
          EXCEPTIONS
            wrong_call = 8.
    IF sy-subrc <> 0 OR lv_cant_open = abap_true.
        me->write_telemetry( iv_message = |BUILD_CONFIG_LOG fails to call STRF_OPEN_PROT { sy-subrc }| ).
        RETURN.
    ENDIF.

    CALL FUNCTION 'TRINT_READ_LOG'
        EXPORTING
            IV_LOG_TYPE     = 'FILE'
            IV_LOGNAME_FILE = lv_logfile
            IV_TIMESTAMP    = lv_timestamp
            IV_CLIENT       = lv_client
            IV_LANGUAGE     = 'E'
        TABLES
            ET_LINES        = lt_lines
        EXCEPTIONS
            OTHERS          = 1.
    IF sy-subrc <> 0.
        me->write_telemetry( iv_message = |BUILD_CONFIG_LOG fails to call TRINT_READ_LOG { sy-subrc }| ).
        RETURN.
    ENDIF.

    LOOP AT lt_lines INTO DATA(wa) WHERE !class = 'TK'.
        IF wa-line CP |*{ iv_trid }*|.
            CONTINUE.
        ENDIF.
        lv_line = wa-line.
        EXIT.
    ENDLOOP.

    SPLIT lv_line AT space INTO TABLE lt_parts.
    ev_date = lt_parts[ 1 ].
    ev_time = lt_parts[ 2 ].

    rv_success = abap_true.

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
        IF rv_success = abap_true AND cht_objversions IS SUPPLIED.
            wa_objversion-objversionno = lv_versno.
            wa_objversion-objname = iv_objname.
            wa_objversion-objtype = 'FUNC'.
            APPEND wa_objversion TO cht_objversions.
        ENDIF.
    ELSEIF iv_objtype = 'CLAS'.
        " class object case, multiple versions may return including public/protected/private sections and methods
        " if test class required, its versions will be fetched together
        ev_version_no = me->get_class_versions_no(
            EXPORTING
                iv_objname = iv_objname
                iv_objtype = iv_objtype
                iv_mode = iv_mode
                iv_date = iv_date
                iv_time = iv_time
                iv_findtest = iv_findtest
            CHANGING
                cht_objversions = cht_objversions
                 ).
        rv_success = abap_true.
    ELSEIF iv_objtype = 'CINC'.
        " in TR commit scenario test class will be requested separately
        CLEAR lt_vers.
        rv_success = me->get_versions(
            EXPORTING
                iv_objname = iv_objname
                iv_objtype = 'CINC'
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
        IF rv_success = abap_true AND cht_objversions IS SUPPLIED.
            wa_objversion-objversionno = lv_versno.
            wa_objversion-objname = iv_objname.
            wa_objversion-objtype = 'CINC'.
            APPEND wa_objversion TO cht_objversions.
        ENDIF.
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
        IF rv_success = abap_true AND cht_objversions IS SUPPLIED.
            wa_objversion-objversionno = lv_versno.
            wa_objversion-objname = iv_objname.
            wa_objversion-objtype = 'REPS'.
            APPEND wa_objversion TO cht_objversions.
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
        IF rv_success = abap_true AND cht_objversions IS SUPPLIED.
            wa_objversion-objversionno = lv_versno.
            wa_objversion-objname = iv_objname.
            wa_objversion-objtype = 'REPS'.
            APPEND wa_objversion TO cht_objversions.
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
        IF rv_success = abap_true AND cht_objversions IS SUPPLIED.
            wa_objversion-objversionno = lv_versno.
            wa_objversion-objname = iv_objname.
            wa_objversion-objtype = 'INTF'.
            APPEND wa_objversion TO cht_objversions.
        ENDIF.
    ELSEIF iv_objtype = 'ENHO'.
        " enhancement implementation case
        CLEAR lt_vers.
        rv_success = me->get_versions(
            EXPORTING
                iv_objname = iv_objname
                iv_objtype = 'ENHO'
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
        IF rv_success = abap_true AND cht_objversions IS SUPPLIED.
            wa_objversion-objversionno = lv_versno.
            wa_objversion-objname = iv_objname.
            wa_objversion-objtype = 'ENHO'.
            APPEND wa_objversion TO cht_objversions.
        ENDIF.
    ELSEIF iv_objtype = 'TABL' OR iv_objtype = 'TABD'.
        " data table case
        CLEAR lt_vers.
        rv_success = me->get_versions(
            EXPORTING
                iv_objname = iv_objname
                iv_objtype = 'TABD'
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
        IF rv_success = abap_true AND cht_objversions IS SUPPLIED.
            wa_objversion-objversionno = lv_versno.
            wa_objversion-objname = iv_objname.
            wa_objversion-objtype = 'TABD'.
            APPEND wa_objversion TO cht_objversions.
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
        " remove version later than given time, keep active/inactive version
        DELETE cht_vers WHERE versno <> 0 AND versno <> 99999 AND ( datum > iv_date OR ( datum = iv_date AND zeit >= iv_time ) ).
    ENDIF.

    IF lines( cht_vers ) = 0.
        rv_success = abap_false.
        RETURN.
    ENDIF.

    IF lv_mode = c_latest_version.
        " exclude active/inactive version
        DELETE cht_vers WHERE versno = 0 OR versno = 99999.
        ev_verscnt = lines( cht_vers ).
        IF ev_verscnt = 0.
            " no latest released version available
            rv_success = abap_false.
            RETURN.
        ELSE.
            " at least one released version available, use it
            ev_versno = cht_vers[ 1 ]-versno.
            rv_success = abap_true.
        ENDIF.
    ELSEIF lv_mode = c_active_version.
        " exclude inactive version
        DELETE cht_vers WHERE versno = 99999.
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
    IF lv_success = abap_true AND cht_objversions IS SUPPLIED.
        wa_objversion-objversionno = lv_versno.
        wa_objversion-objname = iv_objname.
        wa_objversion-objtype = 'CPUB'.
        APPEND wa_objversion TO cht_objversions.
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
    IF lv_success = abap_true AND cht_objversions IS SUPPLIED.
        wa_objversion-objversionno = lv_versno.
        wa_objversion-objname = iv_objname.
        wa_objversion-objtype = 'CPRT'.
        APPEND wa_objversion TO cht_objversions.
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
    IF lv_success = abap_true AND cht_objversions IS SUPPLIED.
        wa_objversion-objversionno = lv_versno.
        wa_objversion-objname = iv_objname.
        wa_objversion-objtype = 'CPRI'.
        APPEND wa_objversion TO cht_objversions.
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
        IF lv_success = abap_true AND cht_objversions IS SUPPLIED.
            wa_objversion-objversionno = lv_versno.
            wa_objversion-objname = lv_methodname.
            wa_objversion-objtype = 'METH'.
            APPEND wa_objversion TO cht_objversions.
        ENDIF.
        IF lv_verscnt > r_version_no.
            r_version_no = lv_verscnt.
        ENDIF.
    ENDLOOP.

    IF iv_findtest = abap_true.

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
                progname = lv_progm.    "#EC FB_OLDED

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
        IF lv_success = abap_true AND cht_objversions IS SUPPLIED.
            wa_objversion-objversionno = lv_versno.
            wa_objversion-objname = lv_objname.
            wa_objversion-objtype = 'CINC'.
            APPEND wa_objversion TO cht_objversions.
        ENDIF.
        IF lv_verscnt > r_version_no.
            r_version_no = lv_verscnt.
        ENDIF.

    ENDIF.

  ENDMETHOD.


  METHOD GET_DELTA.

    CONSTANTS lv_ignore_case_differences TYPE c value abap_true.
    CONSTANTS lv_comp_mode TYPE i VALUE 1.
    DATA: lv_no_release_transformation TYPE svrs_bool.
    DATA: abaptext_new TYPE TABLE OF ABAPTXT255 INITIAL SIZE 0.
    DATA: abaptext_old TYPE TABLE OF ABAPTXT255 INITIAL SIZE 0.
    DATA: trdir_new TYPE TABLE OF TRDIR INITIAL SIZE 1.
    DATA: trdir_old TYPE TABLE OF TRDIR INITIAL SIZE 1.
    DATA: abaptext_delta TYPE TABLE OF vXABAPT255 INITIAL SIZE 0.
    DATA: trdir_delta TYPE TABLE OF XTRDIR INITIAL SIZE 1.
    DATA: smodilog_new TYPE TABLE OF smodilog,
          smodilog_old TYPE TABLE OF smodilog.
    DATA: lv_addlines TYPE i VALUE 0,
          lv_updatelines TYPE i VALUE 0,
          lv_deletelines TYPE i VALUE 0.

    no_delta = abap_false.

    IF logdest_new = space OR logdest_new = 'NONE'.
        lv_no_release_transformation = abap_true.
    ELSE.
        lv_no_release_transformation = abap_false.
    ENDIF.

    " fetch one version's source code lines
    CALL FUNCTION 'SVRS_GET_REPS_FROM_OBJECT'
        EXPORTING
            destination                  = logdest_new
            object_name                  = objname_new
            object_type                  = objtype_new
            versno                       = version_new
            iv_no_release_transformation = lv_no_release_transformation
        TABLES
            repos_tab                    = abaptext_new
            trdir_tab                    = trdir_new
            vsmodilog                    = smodilog_new
        EXCEPTIONS
            no_version                   = 01.

      IF sy-subrc <> 0.
          no_delta = abap_true.
          RETURN.
      ENDIF.

      IF logdest_old = space OR logdest_old = 'NONE'.
          lv_no_release_transformation = abap_true.
      ELSE.
          lv_no_release_transformation = abap_false.
      ENDIF.

      " fetch the other version's source code lines
      CALL FUNCTION 'SVRS_GET_REPS_FROM_OBJECT'
          EXPORTING
              destination                  = logdest_old
              object_name                  = objname_old
              object_type                  = objtype_old
              versno                       = version_old
              iv_no_release_transformation = lv_no_release_transformation
          TABLES
              repos_tab                    = abaptext_old
              trdir_tab                    = trdir_old
              vsmodilog                    = smodilog_old
          EXCEPTIONS
              no_version                   = 01.

      IF sy-subrc <> 0.
          no_delta = abap_true.
          RETURN.
      ENDIF.

      " compare both versions' source code lines
      CALL FUNCTION 'SVRS_COMPUTE_DELTA_REPS'
          EXPORTING
              compare_mode            = lv_comp_mode
              ignore_case_differences = lv_ignore_case_differences
          TABLES
              texttab_old             = abaptext_old
              texttab_new             = abaptext_new
              trdirtab_old            = trdir_old
              trdirtab_new            = trdir_new
              trdir_delta             = trdir_delta
              text_delta              = abaptext_delta.

      READ TABLE abaptext_delta INDEX 1 TRANSPORTING NO FIELDS.
      IF sy-subrc <> 0.
         no_delta = abap_true.
      ENDIF.

      FIELD-SYMBOLS <fs> LIKE LINE OF abaptext_delta.
      LOOP AT abaptext_delta ASSIGNING <fs>.
          IF <fs>-vrsflag = 'I'.
              lv_addlines = lv_addlines + 1.
          ELSEIF <fs>-vrsflag = 'U'.
              lv_updatelines = lv_updatelines + 1.
          ELSEIF <fs>-vrsflag = 'D'.
              lv_deletelines = lv_deletelines + 1.
          ENDIF.
      ENDLOOP.

      insertions = lv_addlines + lv_updatelines.
      deletions = lv_updatelines + lv_deletelines.

  ENDMETHOD.


  METHOD GET_INITIAL.

    DATA lv_no_release_transformation TYPE svrs_bool.
    DATA trdir_new TYPE TABLE OF TRDIR INITIAL SIZE 1.
    DATA smodilog_new TYPE TABLE OF smodilog.
    DATA abaptext TYPE tty_abaptext.

    no_delta = abap_false.

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
        no_delta = abap_true.
        RETURN.
    ENDIF.

    linecount = lines( abaptext ).

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
            RETURN.
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
        RETURN.
      ENDIF.

      rv_success = abap_true.

  ENDMETHOD.


  METHOD GET_COLUMN_VALUES.

    DATA wafield TYPE ty_data_table_field.
    DATA lv_lineoffset TYPE i.
    DATA lv_field(33) TYPE c.
    DATA lv_value TYPE string.
    DATA lv_leng TYPE i.
    DATA lv_strlen TYPE i.

    LOOP AT iv_data_table_desc-dd03v INTO wafield WHERE keyflag = abap_true.
        CLEAR lv_value.
        lv_strlen = strlen( wafield-fieldname ).
        lv_field = '                                :'.
        lv_field+0(lv_strlen) = wafield-fieldname.
        IF wafield-datatype = 'DEC'.
            " packed number
            me->build_packnumber_text(
                EXPORTING
                    iv_line = iv_line
                    iv_off = lv_lineoffset
                    iv_leng = wafield-leng
                    iv_Dec = wafield-decimals
                IMPORTING
                    ev_text = lv_value
                    ev_leng = lv_leng ).
            lv_lineoffset = lv_lineoffset + lv_leng.
        ELSE.
            " other text like fields
            IF wafield-leng > 0.
                IF lv_lineoffset + wafield-leng <= strlen( iv_line ).
                    lv_value = iv_line+lv_lineoffset(wafield-leng).
                ELSE.
                    lv_leng = strlen( iv_line ) - lv_lineoffset.
                    IF lv_leng > 0.
                        lv_value = iv_line+lv_lineoffset(lv_leng).
                    ELSE.
                        lv_value = ''.
                    ENDIF.
                ENDIF.
                lv_lineoffset = lv_lineoffset + wafield-leng.
            ENDIF.
        ENDIF.
        APPEND |    { lv_field } { lv_value }| TO cht_filecontent.
    ENDLOOP.

  ENDMETHOD.


  METHOD GET_PARENTPACKAGES.

    DATA lv_parentcl TYPE parentcl.
    DATA lv_childcl TYPE devclass.
    lv_childcl = iv_package.
    WHILE lv_childcl IS NOT INITIAL.
        SELECT SINGLE parentcl FROM tdevc INTO lv_parentcl WHERE devclass = lv_childcl.
        APPEND lv_childcl TO et_packages.
        lv_childcl = lv_parentcl.
    ENDWHILE.

  ENDMETHOD.


  METHOD GET_SUBPACKAGES.

    " breath first search for sub packages of each package
    DATA lt_queue TYPE tty_package.
    DATA lt_children TYPE tty_package.
    DATA lv_current TYPE devclass.
    APPEND iv_package TO lt_queue.
    WHILE lines( lt_queue ) > 0.
        lv_current = lt_queue[ 1 ].
        CLEAR lt_children.
        SELECT devclass FROM tdevc INTO TABLE @lt_children WHERE parentcl = @lv_current.
        APPEND LINES OF lt_children TO lt_queue.
        APPEND lv_current TO et_packages.
        DELETE lt_queue FROM 1.
    ENDWHILE.

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