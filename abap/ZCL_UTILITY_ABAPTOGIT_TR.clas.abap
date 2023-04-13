" Helper class to talk to SAP transport request and ABAP/config objects operations
CLASS zcl_utility_abaptogit_tr DEFINITION
  PUBLIC
  FINAL
  CREATE PUBLIC .

  PUBLIC SECTION.

    " latest and active version mode
    CONSTANTS: c_latest_version        TYPE string VALUE 'latest',
               c_active_version        TYPE string VALUE 'active',
               c_schemapcr             TYPE string VALUE '.schemapcr',
               c_config                TYPE string VALUE '.config',
               c_varx                  TYPE string VALUE '.varx'.

    " TR info
    TYPES: BEGIN OF ts_change_object,
             name TYPE string,
             type TYPE string,
           END OF ts_change_object.
    TYPES: BEGIN OF ts_task,
             id     TYPE string,
             title  TYPE string,
             status TYPE string,
           END OF ts_task.
    TYPES: tty_change_object TYPE TABLE OF ts_change_object WITH KEY name.
    TYPES: BEGIN OF ts_tr_info,
             id      TYPE string,
             owner   TYPE string,
             desc    TYPE string,
             tasks   TYPE TABLE OF ts_task WITH KEY id,
             func    TYPE string,
             objects TYPE tty_change_object,
           END OF ts_tr_info.

    " version information of an ABAP object to fetch file content
    TYPES: BEGIN OF ts_version_no,
             objname      TYPE versobjnam,
             objtype      TYPE versobjtyp,
             objversionno TYPE versno,
             date         TYPE d,
             time         TYPE t,
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
             prog        TYPE string,
             verno       TYPE i,
             filecontent TYPE string,
             insertions  TYPE i,
             deletions   TYPE i,
             rows        TYPE i,
             tables      TYPE i,
             date        TYPE d,
             time        TYPE t,
           END OF ts_commit_object.
    TYPES: tty_commit_object TYPE TABLE OF ts_commit_object.

    " source lines of an ABAP object
    TYPES: tty_abaptext TYPE TABLE OF abaptxt255 INITIAL SIZE 0.

    " cache for function group to package name mappings
    TYPES: BEGIN OF ts_fugr_devclass,
             fugr     TYPE string,
             devclass TYPE string,
           END OF ts_fugr_devclass.
    TYPES: tty_fugr_devclass TYPE TABLE OF ts_fugr_devclass.

    " cache for program to package name mappings
    TYPES: BEGIN OF ts_prog_devclass,
             prog     TYPE string,
             devclass TYPE string,
           END OF ts_prog_devclass.
    TYPES: tty_prog_devclass TYPE TABLE OF ts_prog_devclass.

    " package list
    TYPES: tty_package TYPE TABLE OF string.

    " object type histogram
    TYPES: BEGIN OF ts_objtype_hist,
             pgmid   TYPE e071-pgmid,
             objtype TYPE e071-object,
             trkorr  TYPE e070-trkorr,
           END OF ts_objtype_hist.
    TYPES: tty_objtype_hist TYPE TABLE OF ts_objtype_hist.

    " constructor
    " io_objtelemetry - class object for telemetry
    " iv_methtelemetry - method name for telemetry
    " for telemetry, the method will be invoked with parameters iv_message as string (for message content) and iv_kind as string (for category)
    METHODS constructor
      IMPORTING
        io_objtelemetry  TYPE REF TO object OPTIONAL
        iv_methtelemetry TYPE string OPTIONAL.

    " fetch TR info for a TR
    " iv_trid - TR ID
    " ev_info - TR info
    METHODS get_tr_info
      IMPORTING
                iv_trid           TYPE string
      EXPORTING
                ev_info           TYPE ts_tr_info
      RETURNING VALUE(rv_success) TYPE abap_bool.

    " fetch ABAP objects from SAP to commit to Git for a TR
    " iv_trid - TR ID
    " iv_packagenames - package names to include in commit, separated by comma
    " iv_maxrow - max rows for config table full snapshot
    " iv_maxfulltable - max tables with full snapshot, 0 means disabled
    " iv_deltastats - conduct heatmap stats for code changes or not
    " iv_mode - last or active version, by default latest as usual sync scenario, active for Git PR experience integration
    " iv_needcontent - fetch TR object content or not
    " ev_owner - TR owner
    " ev_comment - commit comment
    " ev_date - TR last changed date
    " ev_time - TR last changed time
    " it_commit_objects - table of ABAP objects to commit to Git including name, type, file content, add/update/delete status
    METHODS get_tr_commit_objects
      IMPORTING
                iv_trid           TYPE string
                iv_packagenames   TYPE string
                iv_maxrow         TYPE i DEFAULT 1000
                iv_maxfulltable   TYPE i DEFAULT 0
                iv_deltastats     TYPE abap_bool DEFAULT abap_false
                iv_mode           TYPE string DEFAULT c_latest_version
                iv_needcontent    TYPE abap_bool DEFAULT abap_true
      EXPORTING
                ev_owner          TYPE string
                ev_comment        TYPE string
                ev_date           TYPE d
                ev_time           TYPE t
      CHANGING
                it_commit_objects TYPE tty_commit_object
      RETURNING VALUE(rv_success) TYPE abap_bool.

    " get TR owner
    " iv_trid - TR ID
    " ev_owner - TR owner
    METHODS get_tr_owner
      IMPORTING
                iv_trid           TYPE string
      EXPORTING
                ev_owner          TYPE string
      RETURNING VALUE(rv_success) TYPE abap_bool.

    " get ABAP object version number (to fetch specific version's code lines)
    " iv_objname - ABAP object name from table TADIR
    " iv_objtype - ABAP object type from table TADIR
    " iv_mode - active/latest version mode
    " iv_trid - TR ID of the version matching or no later to select
    " iv_date/iv_time - date and time of versions no later than to select
    " iv_findtest - require to find test class of a product class if applicable
    " ev_version_no - count of versions selected
    " ev_date - update date of version selected
    " ev_time - update time of version selected
    " cht_objversions - object versions selected
    METHODS get_versions_no
      IMPORTING
                iv_objname        TYPE e071-obj_name
                iv_objtype        TYPE e071-object
                iv_mode           TYPE string
                iv_trid           TYPE trkorr OPTIONAL
                iv_date           TYPE d OPTIONAL
                iv_time           TYPE t OPTIONAL
                iv_findtest       TYPE abap_bool
      EXPORTING
                ev_version_no     TYPE i
                ev_date           TYPE d
                ev_time           TYPE t
      CHANGING
                cht_objversions   TYPE tty_version_no OPTIONAL
      RETURNING VALUE(rv_success) TYPE abap_bool.

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
                iv_objname         TYPE e071-obj_name
                iv_objtype         TYPE e071-object
                it_objversions     TYPE tty_version_no
      EXPORTING
                et_filecontent     TYPE tty_abaptext
                ev_tclsname        TYPE string
                ev_tclstype        TYPE string
                et_tclsfilecontent TYPE tty_abaptext
      RETURNING VALUE(rv_success)  TYPE abap_bool.

    " construct ABAP data table object description content
    " iv_objname - ABAP object name from table TADIR
    " iv_version - object version
    " ev_filecontent - file content
    " et_filecontent - file content lines
    METHODS build_data_table_content
      IMPORTING
                iv_objname        TYPE e071-obj_name
                iv_version        TYPE versno
      EXPORTING
                ev_filecontent    TYPE string
                et_filecontent    TYPE tty_abaptext
      RETURNING VALUE(rv_success) TYPE abap_bool.

    " get content of data element
    " iv_objname - ABAP object name from table TADIR
    " iv_version - object version
    " ev_filecontent - file content
    " et_filecontent - file content lines
    METHODS get_dataelement_content
      IMPORTING
                iv_version        TYPE versno
                iv_objname        TYPE versobjnam
                iv_logdest        TYPE rfcdest DEFAULT ''
      EXPORTING
                ev_filecontent    TYPE string
                et_filecontent    TYPE tty_abaptext
      RETURNING VALUE(rv_success) TYPE abap_bool.

    " get content of domain
    " iv_objname - ABAP object name from table TADIR
    " iv_version - object version
    " ev_filecontent - file content
    " et_filecontent - file content lines
    METHODS get_domain_content
      IMPORTING
                iv_version        TYPE versno
                iv_objname        TYPE versobjnam
                iv_logdest        TYPE rfcdest DEFAULT ''
      EXPORTING
                ev_filecontent    TYPE string
                et_filecontent    TYPE tty_abaptext
      RETURNING VALUE(rv_success) TYPE abap_bool.

    " get content of lock object
    " iv_objname - ABAP object name from table TADIR
    " iv_version - object version
    " ev_filecontent - file content
    " et_filecontent - file content lines
    METHODS get_lockobject_content
      IMPORTING
                iv_version        TYPE versno
                iv_objname        TYPE versobjnam
                iv_logdest        TYPE rfcdest DEFAULT ''
      EXPORTING
                ev_filecontent    TYPE string
                et_filecontent    TYPE tty_abaptext
      RETURNING VALUE(rv_success) TYPE abap_bool.

    " get content of search help object
    " iv_objname - ABAP object name from table TADIR
    " iv_version - object version
    " ev_filecontent - file content
    " et_filecontent - file content lines
    METHODS get_searchhelp_content
      IMPORTING
                iv_version        TYPE versno
                iv_objname        TYPE versobjnam
                iv_logdest        TYPE rfcdest DEFAULT ''
      EXPORTING
                ev_filecontent    TYPE string
                et_filecontent    TYPE tty_abaptext
      RETURNING VALUE(rv_success) TYPE abap_bool.

    " get content of table type
    " iv_objname - ABAP object name from table TADIR
    " iv_version - object version
    " ev_filecontent - file content
    " et_filecontent - file content lines
    METHODS get_tabletype_content
      IMPORTING
                iv_version        TYPE versno
                iv_objname        TYPE versobjnam
                iv_logdest        TYPE rfcdest DEFAULT ''
      EXPORTING
                ev_filecontent    TYPE string
                et_filecontent    TYPE tty_abaptext
      RETURNING VALUE(rv_success) TYPE abap_bool.

    " get content of transformation XML
    " iv_objname - ABAP object name from table TADIR
    " iv_version - object version
    " ev_filecontent - file content
    " et_filecontent - file content lines
    METHODS get_xslt_content
      IMPORTING
                iv_version        TYPE versno
                iv_objname        TYPE versobjnam
                iv_logdest        TYPE rfcdest DEFAULT ''
      EXPORTING
                ev_filecontent    TYPE string
                et_filecontent    TYPE tty_abaptext
      RETURNING VALUE(rv_success) TYPE abap_bool.

    " get content of report text
    " iv_objname - ABAP object name from table TADIR
    " iv_version - object version
    " ev_filecontent - file content
    " et_filecontent - file content lines
    METHODS get_rept_content
      IMPORTING
                iv_version        TYPE versno
                iv_objname        TYPE versobjnam
                iv_logdest        TYPE rfcdest DEFAULT ''
      EXPORTING
                ev_filecontent    TYPE string
                et_filecontent    TYPE tty_abaptext
      RETURNING VALUE(rv_success) TYPE abap_bool.

    " get content of view
    " iv_objname - ABAP object name from table TADIR
    " iv_version - object version
    " ev_filecontent - file content
    " et_filecontent - file content lines
    METHODS get_view_content
      IMPORTING
                iv_version        TYPE versno
                iv_objname        TYPE versobjnam
                iv_logdest        TYPE rfcdest DEFAULT ''
      EXPORTING
                ev_filecontent    TYPE string
                et_filecontent    TYPE tty_abaptext
      RETURNING VALUE(rv_success) TYPE abap_bool.

    " get content of screen
    " iv_objname - ABAP object name from table TADIR
    " iv_version - object version
    " iv_needcontent - need screen content or not
    " ev_prog - program name
    " ev_devclass - package name
    " ev_filecontent - file content
    " et_filecontent - file content lines
    METHODS get_dynp_content
      IMPORTING
                iv_version        TYPE versno
                iv_objname        TYPE versobjnam
                iv_logdest        TYPE rfcdest DEFAULT ''
                iv_needcontent    TYPE abap_bool DEFAULT 'X'
      EXPORTING
                ev_prog           TYPE string
                ev_devclass       TYPE string
                ev_filecontent    TYPE string
                et_filecontent    TYPE tty_abaptext
      RETURNING VALUE(rv_success) TYPE abap_bool.

    " get content of Report Variants
    " iv_objname - ABAP object name
    " iv_version - object version
    " ev_filecontent - file content
    " et_filecontent - file content lines
    METHODS get_varx_content
      IMPORTING
*            iv_version      TYPE versno
                iv_objname        TYPE e071-obj_name
*            iv_logdest      TYPE rfcdest DEFAULT ''
*            iv_needcontent  TYPE abap_bool DEFAULT 'X'
      EXPORTING
*            ev_filecontent  TYPE string
                et_filecontent    TYPE tty_abaptext
      RETURNING VALUE(rv_success) TYPE abap_bool.

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
        iv_schemaname  TYPE string
        iv_indented    TYPE abap_bool DEFAULT abap_true
      EXPORTING
        et_filecontent TYPE tty_abaptext.

    " construct HR/payroll personnel calculation rule code content
    " iv_pcrname - PCR name
    " et_filecontent - PCR code content lines
    METHODS build_pcr_content_active
      IMPORTING
        iv_pcrname     TYPE string
      EXPORTING
        et_filecontent TYPE tty_abaptext.

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

    METHODS get_objtype_hist
      IMPORTING
        iv_fromdate TYPE d
      EXPORTING
        et_hist     TYPE tty_objtype_hist.

  PROTECTED SECTION.

  PRIVATE SECTION.

    CONSTANTS c_en TYPE spras VALUE 'E'.
    CONSTANTS c_custtrfunc TYPE string VALUE 'W'.
    CONSTANTS c_wkbtrfunc TYPE string VALUE 'K'.
    CONSTANTS c_toctrfunc TYPE string VALUE 'T'.
    CONSTANTS c_relests TYPE string VALUE 'R'.

    " structure for data table description
    TYPES: BEGIN OF ty_dd02v,
             as4user    TYPE string,
             as4date    TYPE string,
             as4time    TYPE string,
             tabname    TYPE string,
             ddlanguage TYPE string,
             tabclass   TYPE string,
             clidep     TYPE string,
             ddtext     TYPE string,
             mainflag   TYPE string,
             contflag   TYPE string,
             shlpexi    TYPE string,
           END OF ty_dd02v.
    TYPES: BEGIN OF ty_data_table_field,
             fieldname  TYPE string,
             keyflag    TYPE string,
             rollname   TYPE string,
             adminfield TYPE string,
             datatype   TYPE string,
             leng       TYPE i,
             decimals   TYPE i,
             notnull    TYPE string,
             ddtext     TYPE string,
             domname    TYPE string,
             shlporigin TYPE string,
             comptype   TYPE string,
           END OF ty_data_table_field.
    TYPES: tty_data_table_field TYPE TABLE OF ty_data_table_field WITH DEFAULT KEY.
    TYPES: BEGIN OF ty_data_table_desc,
             dd02v TYPE ty_dd02v,
             dd03v TYPE tty_data_table_field,
           END OF ty_data_table_desc.
    " structure for data element description
    TYPES: BEGIN OF ty_data_element_desc,
             as4user    TYPE string,
             as4date    TYPE string,
             as4time    TYPE string,
             rollname   TYPE string,
             ddlanguage TYPE string,
             domname    TYPE string,
             ddtext     TYPE string,
             datatype   TYPE string,
             leng       TYPE i,
             decimals   TYPE i,
             outputlen  TYPE i,
             headlen    TYPE i,
             scrlen1    TYPE i,
             scrlen2    TYPE i,
             scrlen3    TYPE i,
             reptext    TYPE string,
             scrtext_s  TYPE string,
             scrtext_m  TYPE string,
             scrtext_l  TYPE string,
             refkind    TYPE string,
             lowercase  TYPE string,
           END OF ty_data_element_desc.
    " structure for domain description
    TYPES: BEGIN OF ty_domain_desc,
             as4user    TYPE string,
             as4date    TYPE string,
             as4time    TYPE string,
             ddlanguage TYPE string,
             domname    TYPE string,
             ddtext     TYPE string,
             datatype   TYPE string,
             leng       TYPE i,
             decimals   TYPE i,
             outputlen  TYPE i,
             entitytab  TYPE string,
           END OF ty_domain_desc.
    " structure for table type description
    TYPES: BEGIN OF ty_dd40v,
             as4user    TYPE string,
             as4date    TYPE string,
             as4time    TYPE string,
             ddlanguage TYPE string,
             typename   TYPE string,
             ddtext     TYPE string,
             datatype   TYPE string,
             rowtype    TYPE string,
             rowkind    TYPE string,
             keykind    TYPE string,
             keyfdcount TYPE i,
             typelen    TYPE i,
             ttypkind   TYPE string,
           END OF ty_dd40v.
    TYPES: BEGIN OF ty_dd42v,
             seckeyname TYPE string,
             keyfdpos   TYPE i,
             rowtypepos TYPE i,
             keyfield   TYPE string,
           END OF ty_dd42v.
    TYPES: BEGIN OF ty_dd43v,
             seckeyname     TYPE string,
             ddlanguage     TYPE string,
             seckeyunique   TYPE string,
             accessmode     TYPE string,
             kind           TYPE string,
             keydescription TYPE string,
           END OF ty_dd43v.
    TYPES: tty_dd42v TYPE TABLE OF ty_dd42v WITH DEFAULT KEY.
    TYPES: tty_dd43v TYPE TABLE OF ty_dd43v WITH DEFAULT KEY.
    TYPES: BEGIN OF ty_table_type_desc,
             dd40v TYPE ty_dd40v,
             dd42v TYPE tty_dd42v,
             dd43v TYPE tty_dd43v,
           END OF ty_table_type_desc.
    " structure for view description
    TYPES: BEGIN OF ty_dd25v,
             as4user    TYPE string,
             as4date    TYPE string,
             as4time    TYPE string,
             ddlanguage TYPE string,
             viewname   TYPE string,
             ddtext     TYPE string,
             aggtype    TYPE string,
             dbrefname  TYPE string,
             viewref    TYPE string,
           END OF ty_dd25v.
    TYPES: BEGIN OF ty_dd26v,
             tabname    TYPE string,
             tabpos     TYPE i,
             fortabname TYPE string,
             forfield   TYPE string,
             fordir     TYPE string,
           END OF ty_dd26v.
    TYPES: BEGIN OF ty_dd27v,
             viewfield   TYPE string,
             objpos      TYPE i,
             tabname     TYPE string,
             keyflag     TYPE string,
             rollname    TYPE string,
             rdonly      TYPE string,
             dbviewfield TYPE string,
             enqmode     TYPE string,
           END OF ty_dd27v.
    TYPES: BEGIN OF ty_dd28v,
             condname  TYPE string,
             position  TYPE i,
             tabname   TYPE string,
             negation  TYPE string,
             operator  TYPE string,
             constants TYPE string,
             contline  TYPE string,
             and_or    TYPE string,
             offset    TYPE i,
             flength   TYPE i,
             joperator TYPE string,
           END OF ty_dd28v.
    TYPES: tty_dd26v TYPE TABLE OF ty_dd26v WITH DEFAULT KEY.
    TYPES: tty_dd27v TYPE TABLE OF ty_dd27v WITH DEFAULT KEY.
    TYPES: tty_dd28v TYPE TABLE OF ty_dd28v WITH DEFAULT KEY.
    TYPES: BEGIN OF ty_view_desc,
             dd25v TYPE ty_dd25v,
             dd26v TYPE tty_dd26v,
             dd27v TYPE tty_dd27v,
             dd28v TYPE tty_dd28v,
           END OF ty_view_desc.
    " structure for lock object description
    TYPES: BEGIN OF ty_lock_object_desc,
             dd25v TYPE ty_dd25v,
             dd26v TYPE tty_dd26v,
             dd27v TYPE tty_dd27v,
           END OF ty_lock_object_desc.
    " structure for search help description
    TYPES: BEGIN OF ty_dd30v,
             as4user    TYPE string,
             as4date    TYPE string,
             as4time    TYPE string,
             ddlanguage TYPE string,
             shlpname   TYPE string,
             ddtext     TYPE string,
             issimple   TYPE string,
             selmethod  TYPE string,
             dialogtype TYPE string,
           END OF ty_dd30v.
    TYPES: BEGIN OF ty_dd31v,
             subshlp    TYPE string,
             shposition TYPE i,
             viashlp    TYPE string,
             hideflag   TYPE string,
           END OF ty_dd31v.
    TYPES: BEGIN OF ty_dd32v,
             fieldname  TYPE string,
             flposition TYPE i,
             rollname   TYPE string,
             shlpinput  TYPE string,
             shlpoutput TYPE string,
             shlpselpos TYPE i,
             shlpseldis TYPE string,
           END OF ty_dd32v.
    TYPES: BEGIN OF ty_dd33v,
             fieldname TYPE string,
             subshlp   TYPE string,
             subfield  TYPE string,
           END OF ty_dd33v.
    TYPES: tty_dd31v TYPE TABLE OF ty_dd31v WITH DEFAULT KEY.
    TYPES: tty_dd32v TYPE TABLE OF ty_dd32v WITH DEFAULT KEY.
    TYPES: tty_dd33v TYPE TABLE OF ty_dd33v WITH DEFAULT KEY.
    TYPES: BEGIN OF ty_search_help_desc,
             dd30v TYPE ty_dd30v,
             dd31v TYPE tty_dd31v,
             dd32v TYPE tty_dd32v,
             dd33v TYPE tty_dd33v,
           END OF ty_search_help_desc.

    " telemetry callback
    DATA oref_telemetry TYPE REF TO object.
    DATA method_name_telemetry TYPE string.

    " process table config change case
    METHODS build_table_config_change
      IMPORTING
        iv_cs_request     TYPE trwbo_request
        iv_objname        TYPE e071-obj_name
        iv_objtype        TYPE e071-object
        iv_deltastats     TYPE abap_bool DEFAULT abap_false
        iv_maxrow         TYPE i DEFAULT 1000
        iv_maxfulltable   TYPE i DEFAULT 10
      CHANGING
        it_commit_objects TYPE tty_commit_object.

    " process sapscript change case
    METHODS build_sapscript_change
      IMPORTING
        iv_cs_request     TYPE trwbo_request
        iv_devclass       TYPE string
        iv_object         TYPE e071
        iv_objname        TYPE e071-obj_name
        iv_objtype        TYPE e071-object
        iv_deltastats     TYPE abap_bool DEFAULT abap_false
        iv_maxrow         TYPE i DEFAULT 1000
        iv_maxfulltable   TYPE i DEFAULT 10
      CHANGING
        it_commit_objects TYPE tty_commit_object.

    " process logical config change case
    METHODS build_logical_config_change
      IMPORTING
        iv_cs_request     TYPE trwbo_request
        iv_object         TYPE e071
        iv_objname        TYPE e071-obj_name
        iv_objtype        TYPE e071-object
        iv_deltastats     TYPE abap_bool DEFAULT abap_false
        iv_maxrow         TYPE i DEFAULT 1000
        iv_maxfulltable   TYPE i DEFAULT 10
      CHANGING
        it_commit_objects TYPE tty_commit_object.

    " process report variant change case
    METHODS build_report_variant_change
      IMPORTING
        iv_cs_request     TYPE trwbo_request
        iv_objname        TYPE e071-obj_name
        iv_objtype        TYPE e071-object
        iv_deltastats     TYPE abap_bool DEFAULT abap_false
        iv_maxrow         TYPE i DEFAULT 1000
        iv_maxfulltable   TYPE i DEFAULT 10
      CHANGING
        it_commit_objects TYPE tty_commit_object.

    " experiment: extract config changes from audit log
    METHODS build_config_log
      IMPORTING
                iv_trid           TYPE e070-trkorr
                iv_tabname        TYPE string
                iv_fromdate       TYPE d
                iv_fromtime       TYPE t
                iv_todate         TYPE d
                iv_totime         TYPE t
                iv_user           TYPE string
      EXPORTING
                abaptext          TYPE tty_abaptext
      RETURNING VALUE(rv_success) TYPE abap_bool.

    " get function group name of an object in a function group
    METHODS get_fugr
      IMPORTING
        iv_objname  TYPE string
      EXPORTING
        ev_fugrname TYPE string.

    " get source code lines and count of them
    METHODS get_code_lines
      IMPORTING
                iv_version        TYPE versno
                iv_objname        TYPE versobjnam
                iv_objtype        TYPE versobjtyp
                iv_logdest        TYPE rfcdest
      EXPORTING
                linecount         TYPE i
                abaptext          TYPE tty_abaptext
      RETURNING VALUE(rv_success) TYPE abap_bool.

    " get source code lines for enhancement implementation
    METHODS get_code_lines_enho
      IMPORTING
                iv_version        TYPE versno
                iv_objname        TYPE versobjnam
                iv_logdest        TYPE rfcdest
      EXPORTING
                abaptext          TYPE tty_abaptext
      RETURNING VALUE(rv_success) TYPE abap_bool.

    " fetch table schema
    METHODS get_table_schema
      IMPORTING
                iv_objname        TYPE e071-obj_name
                iv_version        TYPE versno
                iv_escape         TYPE abap_bool DEFAULT abap_true
      EXPORTING
                ev_desc           TYPE ty_data_table_desc
      RETURNING VALUE(rv_success) TYPE abap_bool.

    " build config table delta-row lines
    METHODS get_config_delta_lines
      IMPORTING
                iv_tabname        TYPE e071k-objname
                iv_mtabname       TYPE e071k-mastername
                iv_objtype        TYPE e071-object
                iv_request        TYPE trwbo_request
                iv_deltastats     TYPE abap_bool DEFAULT abap_false
      EXPORTING
                abaptext          TYPE tty_abaptext
                ev_rows           TYPE i
      RETURNING VALUE(rv_success) TYPE abap_bool.

    " build config table all-row lines
    METHODS get_config_table_lines
      IMPORTING
                iv_tabname        TYPE e071k-objname
                iv_request        TYPE trwbo_request
                iv_tsv            TYPE abap_bool
                iv_maxrow         TYPE i DEFAULT 1000
      EXPORTING
                abaptext          TYPE tty_abaptext
      RETURNING VALUE(rv_success) TYPE abap_bool.

    " build config table row lines for header
    METHODS get_config_lines_header
      IMPORTING
                iv_tabname        TYPE e071k-objname
                iv_request        TYPE trwbo_request
                iv_tsv            TYPE abap_bool DEFAULT abap_false
      EXPORTING
                ev_desc           TYPE ty_data_table_desc
      CHANGING
                abaptext          TYPE tty_abaptext
      RETURNING VALUE(rv_success) TYPE abap_bool.

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

    " get versions of an ABAP object
    METHODS get_versions
      IMPORTING
                iv_objname        TYPE e071-obj_name
                iv_objtype        TYPE e071-object
      CHANGING
                it_vers           TYPE vrsd_tab
      RETURNING VALUE(rv_success) TYPE abap_bool.

    " get TR creation timestamp
    METHODS get_tr_creation
      IMPORTING
                iv_trid           TYPE e070-trkorr
      EXPORTING
                ev_date           TYPE d
                ev_time           TYPE t
      RETURNING VALUE(rv_success) TYPE abap_bool.

    " get code content or heatmap stats
    METHODS get_code_content_or_heatmap
      IMPORTING
                iv_objname          TYPE e071-obj_name
                iv_objtype          TYPE e071-object
                it_objversions      TYPE tty_version_no
                iv_needcontent      TYPE abap_bool
                iv_deltastats       TYPE abap_bool
      EXPORTING
                et_filecontent      TYPE tty_abaptext
                ev_filecontent      TYPE string
                ev_total_insertions TYPE i
                ev_total_deletions  TYPE i
      RETURNING VALUE(rv_success)   TYPE string.


    " get class object version (for public/protected/private sections in definition and method implementations)
    METHODS get_class_versions_no
      IMPORTING
                iv_objname          TYPE e071-obj_name
                iv_objtype          TYPE e071-object
                iv_mode             TYPE string
                iv_trid             TYPE trkorr
                iv_date             TYPE d OPTIONAL
                iv_time             TYPE t OPTIONAL
                iv_findtest         TYPE abap_bool DEFAULT abap_true
      EXPORTING
                ev_date             TYPE d
                ev_time             TYPE t
      CHANGING
                cht_objversions     TYPE tty_version_no OPTIONAL
      RETURNING VALUE(r_version_no) TYPE i.

    " helper for get versions number
    METHODS get_versions_no_helper
      IMPORTING
                iv_objname        TYPE e071-obj_name
                iv_objtype        TYPE e071-object
                iv_mode           TYPE string
                iv_trid           TYPE trkorr OPTIONAL
                iv_date           TYPE d OPTIONAL
                iv_time           TYPE t OPTIONAL
      EXPORTING
                ev_version_no     TYPE i
                ev_versno         TYPE versno
                ev_date           TYPE d
                ev_time           TYPE t
      CHANGING
                cht_vers          TYPE vrsd_tab
                cht_objversions   TYPE tty_version_no OPTIONAL
      RETURNING VALUE(rv_success) TYPE abap_bool.

    " get valued version to no later than specific time stamp if any given latest/active version mode
    METHODS get_valued_version
      IMPORTING
                iv_mode           TYPE string
                iv_trid           TYPE trkorr OPTIONAL
                iv_date           TYPE d
                iv_time           TYPE t
      EXPORTING
                ev_versno         TYPE versno
                ev_verscnt        TYPE i
                ev_date           TYPE d
                ev_time           TYPE t
      CHANGING
                cht_vers          TYPE vrsd_tab
      RETURNING VALUE(rv_success) TYPE abap_bool.

    " fetch delta stats of an ABAP object given two versions
    CLASS-METHODS get_delta
      IMPORTING
        version_new TYPE versno
        objname_new TYPE versobjnam
        objtype_new TYPE versobjtyp
        logdest_new TYPE rfcdest
        version_old TYPE versno
        objname_old TYPE versobjnam
        objtype_old TYPE versobjtyp
        logdest_old TYPE rfcdest
      EXPORTING
        no_delta    TYPE c
        insertions  TYPE i
        deletions   TYPE i.

    " fetch ABAP code object source code lines of a given version
    CLASS-METHODS get_initial
      IMPORTING
        iv_version TYPE versno
        iv_objname TYPE versobjnam
        iv_objtype TYPE versobjtyp
        iv_logdest TYPE rfcdest
      EXPORTING
        no_delta   TYPE c
        linecount  TYPE i.

    " parse column values from row text
    METHODS get_column_values
      IMPORTING
        iv_data_table_desc TYPE ty_data_table_desc
        iv_line            TYPE string
      CHANGING
        cht_filecontent    TYPE tty_abaptext.

    " get other non-key column values from table
    METHODS get_nonkey_column_values
      IMPORTING
        iv_data_table_desc TYPE ty_data_table_desc
        iv_criteria        TYPE string
      EXPORTING
        abaptext           TYPE tty_abaptext.

    " get methods of a class object
    METHODS get_class_methods
      IMPORTING
        iv_classname TYPE classname
      CHANGING
        cht_methods  TYPE abap_methdescr_tab.

    " wrapper to write telemetry with the callback registered
    METHODS write_telemetry
      IMPORTING
        iv_message TYPE string
        iv_kind    TYPE string DEFAULT 'error'.

ENDCLASS.



CLASS zcl_utility_abaptogit_tr IMPLEMENTATION.


  METHOD constructor.

    IF io_objtelemetry IS SUPPLIED.
      me->oref_telemetry = io_objtelemetry.
    ENDIF.

    IF iv_methtelemetry IS SUPPLIED.
      me->method_name_telemetry = iv_methtelemetry.
    ENDIF.

  ENDMETHOD.


  METHOD get_tr_info.

    DATA ld_cs_request TYPE trwbo_request.
    DATA lv_trkorr TYPE trkorr.
    DATA lv_tasktitle TYPE string.
    DATA lv_taskid TYPE string.
    DATA lv_taskdesc TYPE string.
    DATA lt_tasktitles TYPE TABLE OF string.
    DATA lt_taskfields TYPE TABLE OF string.
    DATA ls_task TYPE ts_task.
    DATA lt_tasks TYPE TABLE OF ts_task.
    DATA ls_change_object TYPE ts_change_object.
    FIELD-SYMBOLS <fs_cs_request_object> LIKE LINE OF ld_cs_request-objects.

    rv_success = abap_true.

    " fetch objects in a TR
    lv_trkorr = iv_trid.
    CALL FUNCTION 'TR_READ_REQUEST'
      EXPORTING
        iv_read_e070       = abap_true
        iv_read_e07t       = abap_true
        iv_read_e070c      = abap_true
        iv_read_e070m      = abap_true
        iv_read_objs_keys  = abap_true
        iv_read_attributes = abap_true
        iv_trkorr          = lv_trkorr
      CHANGING
        cs_request         = ld_cs_request
      EXCEPTIONS
        error_occured      = 1
        no_authorization   = 2.
    IF sy-subrc <> 0.
      me->write_telemetry( iv_message = |GET_TR_INFO fails to call TR_READ_REQUEST with '{ lv_trkorr }' subrc { sy-subrc }| ).
      rv_success = abap_false.
      RETURN.
    ENDIF.

    ev_info-id = iv_trid.
    ev_info-func = ld_cs_request-h-trfunction.
    ev_info-owner = ld_cs_request-h-as4user.
    ev_info-desc = ld_cs_request-h-as4text.

    " fetch task titles in a TR
    SELECT obj_name FROM e071 INTO TABLE @lt_tasktitles WHERE trkorr = @lv_trkorr AND object = 'RELE' AND pgmid = 'CORR'.

    " fetch task id and status in a TR
    SELECT trkorr, ' ', trstatus FROM e070 INTO TABLE @lt_tasks WHERE strkorr = @lv_trkorr.

    " append all tasks' description to the commit description
    LOOP AT lt_tasktitles INTO lv_tasktitle.
      CLEAR lt_taskfields.
      SPLIT lv_tasktitle AT ' ' INTO TABLE lt_taskfields.
      lv_taskid = lt_taskfields[ 1 ].
      SELECT SINGLE as4text FROM e07t INTO @lv_taskdesc WHERE trkorr = @lv_taskid. "#EC WARNOK
      CLEAR ls_task.
      ls_task-id = lv_taskid.
      ls_task-title = |{ lv_tasktitle } { lv_taskdesc }|.
      IF line_exists( lt_tasks[ id = lv_taskid ] ).
        ls_task-status = lt_tasks[ id = lv_taskid ]-status.
      ENDIF.
      APPEND ls_task TO ev_info-tasks.
    ENDLOOP.
    LOOP AT lt_tasks INTO ls_task.
      IF NOT line_exists( ev_info-tasks[ id = ls_task-id ] ).
        APPEND ls_task TO ev_info-tasks.
      ENDIF.
    ENDLOOP.

    LOOP AT ld_cs_request-objects ASSIGNING <fs_cs_request_object> WHERE object <> 'RELE'.
      CLEAR ls_change_object.
      ls_change_object-name = <fs_cs_request_object>-obj_name.
      ls_change_object-type = <fs_cs_request_object>-object.
      APPEND ls_change_object TO ev_info-objects.
    ENDLOOP.

  ENDMETHOD.


  METHOD get_tr_owner.
    DATA ld_cs_request TYPE trwbo_request.
    DATA lv_trkorr TYPE trkorr.
    rv_success = abap_true.
    lv_trkorr = iv_trid.
    CALL FUNCTION 'TR_READ_REQUEST'
      EXPORTING
        iv_read_e070       = abap_true
        iv_read_e07t       = abap_false
        iv_read_e070c      = abap_false
        iv_read_e070m      = abap_false
        iv_read_objs_keys  = abap_false
        iv_read_attributes = abap_false
        iv_trkorr          = lv_trkorr
      CHANGING
        cs_request         = ld_cs_request
      EXCEPTIONS
        error_occured      = 1
        no_authorization   = 2.
    IF sy-subrc <> 0.
      me->write_telemetry( iv_message = |GET_TR_OWNER fails to call TR_READ_REQUEST with '{ lv_trkorr }' subrc { sy-subrc }| ).
      rv_success = abap_false.
      RETURN.
    ENDIF.
    ev_owner = ld_cs_request-h-as4user.
  ENDMETHOD.


  METHOD get_tr_commit_objects.

    DATA ld_cs_request TYPE trwbo_request.
    DATA lv_trkorr TYPE trkorr.
    DATA lv_tasktrkorr TYPE trkorr.
    DATA lt_objects TYPE trwbo_t_e071.
    FIELD-SYMBOLS <fs_cs_request_object> LIKE LINE OF ld_cs_request-objects.
    FIELD-SYMBOLS <fs_cs_request_key> LIKE LINE OF ld_cs_request-keys.
    DATA lt_objversions TYPE tty_version_no.
    DATA wa_objversion TYPE ts_version_no.
    DATA lt_filecontent TYPE tty_abaptext.
    DATA lt_tclsfilecontent TYPE tty_abaptext.
    DATA lt_packagenames TYPE TABLE OF string.
    DATA lv_packagename TYPE string.
    DATA lt_fgfmcodes TYPE TABLE OF string.
    DATA lt_fginccodes TYPE TABLE OF string.
    DATA lv_objname2 TYPE string.
    DATA lv_objtype2 TYPE string.
    DATA lv_devclass TYPE string.
    DATA ls_tadir TYPE tadir.
    DATA lv_progname TYPE string.
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
    DATA lv_programm TYPE programm.
    DATA lv_classkey TYPE seoclskey.
    DATA lv_classname TYPE tadir-obj_name.
    DATA lv_haspackage TYPE abap_bool.
    DATA lt_parentpackages TYPE tty_package.
    DATA lv_foundparentpackage TYPE abap_bool.
    DATA lt_objname_parts TYPE TABLE OF string.
    DATA lt_classes TYPE TABLE OF string.
    DATA lt_fugrs TYPE tty_fugr_devclass.
    DATA lt_progs TYPE tty_prog_devclass.
    DATA lt_scnnums TYPE STANDARD TABLE OF d020s-dnum.
    DATA lv_cdate TYPE cdate.
    DATA lv_udate TYPE aedat.
    DATA lv_progcls TYPE t52ba-pwert.
    DATA lv_subc TYPE reposrc-subc.
    DATA lv_objverprev TYPE versno.
    DATA lv_verdate TYPE d.
    DATA lv_vertime TYPE t.
    DATA lv_insertions TYPE i.
    DATA lv_deletions TYPE i.
    DATA lv_no_delta TYPE c.
    DATA lv_total_insertions TYPE i.
    DATA lv_total_deletions TYPE i.
    DATA lv_success TYPE abap_bool.

    rv_success = abap_true.

    " fetch objects in a TR
    lv_trkorr = iv_trid.
    CALL FUNCTION 'TR_READ_REQUEST'
      EXPORTING
        iv_read_e070       = abap_true
        iv_read_e07t       = abap_true
        iv_read_e070c      = abap_true
        iv_read_e070m      = abap_true
        iv_read_objs_keys  = abap_true
        iv_read_attributes = abap_true
        iv_trkorr          = lv_trkorr
      CHANGING
        cs_request         = ld_cs_request
      EXCEPTIONS
        error_occured      = 1
        no_authorization   = 2.
    IF sy-subrc <> 0.
      me->write_telemetry( iv_message = |GET_TR_COMMIT_OBJECTS fails to call TR_READ_REQUEST with '{ lv_trkorr }' subrc { sy-subrc }| ).
      rv_success = abap_false.
      RETURN.
    ENDIF.

    ev_owner = ld_cs_request-h-as4user.
    ev_date = ld_cs_request-h-as4date.
    ev_time = ld_cs_request-h-as4time.

    " only support workbench/customizing/transport of copies TR
    IF ld_cs_request-h-trfunction <> c_wkbtrfunc AND ld_cs_request-h-trfunction <> c_custtrfunc AND ld_cs_request-h-trfunction <> c_toctrfunc.
      me->write_telemetry( iv_message = |GET_TR_COMMIT_OBJECTS meets request type '{ ld_cs_request-h-trfunction }'| ).
      rv_success = abap_false.
      RETURN.
    ENDIF.

    " only support released one if latest version asked
    IF iv_mode = c_latest_version AND ld_cs_request-h-trstatus <> c_relests.
      me->write_telemetry( iv_message = |GET_TR_COMMIT_OBJECTS meets request status '{ ld_cs_request-h-trstatus }'| ).
      rv_success = abap_false.
      RETURN.
    ENDIF.

    " construct Git commit description carrying TR ID, owner and original description
    " the Git commit can't have committer as the TR owner, instead, on-behalf-of the owner by user name/PAT specified
    APPEND |{ iv_trid }\|{ ld_cs_request-h-as4user }\|{ ld_cs_request-h-as4text }| TO lt_commentlines.

    " active version for modifiable TR has no objects connected and tasks connected
    " need to enumerate tasks and then extract objects from each task
    IF iv_mode = c_active_version.

      " fetch tasks in a modifiable TR
      SELECT trkorr FROM e070 INTO TABLE @lt_tasks WHERE strkorr = @lv_trkorr.

      LOOP AT lt_tasks INTO DATA(watrkorr).
        lv_tasktrkorr = watrkorr.
        CLEAR ld_cs_request.
        CALL FUNCTION 'TR_READ_REQUEST'
          EXPORTING
            iv_read_e070       = abap_true
            iv_read_e07t       = abap_true
            iv_read_e070c      = abap_true
            iv_read_e070m      = abap_true
            iv_read_objs_keys  = abap_true
            iv_read_attributes = abap_true
            iv_trkorr          = lv_tasktrkorr
          CHANGING
            cs_request         = ld_cs_request
          EXCEPTIONS
            error_occured      = 1
            no_authorization   = 2.
        IF sy-subrc <> 0.
          me->write_telemetry( iv_message = |GET_TR_COMMIT_OBJECTS fails to call TR_READ_REQUEST with '{ lv_tasktrkorr }' subrc { sy-subrc }| ).
          rv_success = abap_false.
          RETURN.
        ENDIF.
        " object may duplicate among tasks
        LOOP AT ld_cs_request-objects ASSIGNING <fs_cs_request_object>.
          IF NOT line_exists( lt_objects[ obj_name = <fs_cs_request_object>-obj_name ] ).
            APPEND <fs_cs_request_object> TO lt_objects.
          ENDIF.
        ENDLOOP.
      ENDLOOP.

    ELSE.

      " fetch tasks in a released TR
      SELECT obj_name FROM e071 INTO TABLE @lt_tasks WHERE trkorr = @lv_trkorr AND object = 'RELE' AND pgmid = 'CORR'.

      APPEND LINES OF ld_cs_request-objects TO lt_objects.

    ENDIF.

    " append all tasks' description to the commit description
    LOOP AT lt_tasks INTO lv_task.
      CLEAR lt_taskfields.
      SPLIT lv_task AT ' ' INTO TABLE lt_taskfields.
      lv_taskid = lt_taskfields[ 1 ].
      SELECT SINGLE as4text FROM e07t INTO @lv_taskdesc WHERE trkorr = @lv_taskid. "#EC WARNOK
      APPEND |{ lv_task } { lv_taskdesc }| TO lt_tasktexts.
    ENDLOOP.

    APPEND LINES OF lt_tasktexts TO lt_commentlines.

    " commit description now contains TR description plus all tasks' description each line
    ev_comment = concat_lines_of( table = lt_commentlines sep = cl_abap_char_utilities=>cr_lf ).

    CLEAR: lt_taskids, lt_taskfields, lt_tasktexts.

    SPLIT iv_packagenames AT ',' INTO TABLE lt_packagenames.

    " process objects in the TR
    LOOP AT lt_objects ASSIGNING <fs_cs_request_object> WHERE pgmid <> 'CORR'.

      DATA(lv_objname) = <fs_cs_request_object>-obj_name.
      DATA(lv_objtype) = <fs_cs_request_object>-object.

      " table config change object
      IF lv_objtype = 'TABU'
          OR lv_objtype = 'CDAT'
          OR lv_objtype = 'VDAT'
          OR lv_objtype = 'TDAT'.
        IF iv_needcontent = abap_true.
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
        ENDIF.
        CONTINUE.
      ENDIF.

      " schema/PCR object
      IF lv_objtype = 'PSCC' OR lv_objtype = 'PCYC'.

        CLEAR lt_filecontent.

        IF iv_needcontent = abap_true.

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
          lv_filecontent = concat_lines_of( table = lt_filecontent sep = cl_abap_char_utilities=>cr_lf ).

          " align with GUI_DOWNLOAD which adds a blank line
          lv_filecontent = lv_filecontent && cl_abap_char_utilities=>cr_lf.

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

        ENDIF.

        CONTINUE.

      ENDIF.

      IF lv_objtype = 'VARX' OR lv_objtype = 'VARI'.
        " handle VARX report variant case
        me->build_report_variant_change(
            EXPORTING
                iv_cs_request = ld_cs_request
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
              clskey   = lv_classkey.   "#EC FB_OLDED
          lv_classname = lv_classkey.
        ELSE.
          IF lv_objtype = 'METH'.
            " class name <spaces> method name pattern
            CLEAR lt_objname_parts.
            SPLIT lv_objname AT ' ' INTO TABLE lt_objname_parts.
            lv_classname = lt_objname_parts[ 1 ].
            IF strlen( lv_classname ) > 30.
              lv_classname = lv_objname+0(30).
            ENDIF.
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
            WHERE object = 'CLAS' AND obj_name = lv_classname. "#EC WARNOK "#EC CI_SGLSELECT

      ELSEIF lv_objtype = 'FUNC'.

        " function module case, find out function group and then the package function group belongs to
        lv_funcname = lv_objname.
        me->get_fugr( EXPORTING iv_objname = lv_funcname IMPORTING ev_fugrname = lv_fugr ).

        " use cache for function group objects
        IF line_exists( lt_fugrs[ fugr = lv_fugr ] ).
          lv_devclass = lt_fugrs[ fugr = lv_fugr ]-devclass.
        ELSE.
          SELECT SINGLE devclass FROM tadir INTO lv_devclass
              WHERE obj_name = lv_fugr AND object = 'FUGR'. "#EC WARNOK "#EC CI_SGLSELECT
          APPEND VALUE ts_fugr_devclass( fugr = lv_fugr devclass = lv_devclass ) TO lt_fugrs.
        ENDIF.

      ELSEIF lv_objtype = 'FUGR'.

        " function group, firstly created, may contain FMs/includes and not listed in TR object list
        lv_fugr = lv_objname.

        " use cache for function group objects
        IF line_exists( lt_fugrs[ fugr = lv_fugr ] ).
          lv_devclass = lt_fugrs[ fugr = lv_fugr ]-devclass.
        ELSE.
          SELECT SINGLE devclass FROM tadir INTO lv_devclass
              WHERE obj_name = lv_fugr AND object = 'FUGR'. "#EC WARNOK "#EC CI_SGLSELECT
          APPEND VALUE ts_fugr_devclass( fugr = lv_fugr devclass = lv_devclass ) TO lt_fugrs.
        ENDIF.

        " function group itself to be processed separately
        CONTINUE.

      ELSEIF lv_objtype = 'REPS'.

        " could be repair of an object
        " need to search object type to replace 'REPS'
        SELECT SINGLE object FROM tadir INTO lv_objtype
            WHERE obj_name = lv_objname.  "#EC WARNOK "#EC CI_SGLSELECT
        IF sy-subrc = 0.

          " found, use the right type of the object
          " find out which package the ABAP object belongs to
          SELECT SINGLE devclass INTO lv_devclass FROM tadir
              WHERE object = lv_objtype AND obj_name = lv_objname. "#EC WARNOK "#EC CI_SGLSELECT

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
                WHERE obj_name = lv_fugr AND object = 'FUGR'. "#EC WARNOK "#EC CI_SGLSELECT
            APPEND VALUE ts_fugr_devclass( fugr = lv_fugr devclass = lv_devclass ) TO lt_fugrs.
          ENDIF.

        ENDIF.

      ELSEIF lv_objtype = 'PROG'.

          " program (include) case
          " find out which package the ABAP object belongs to

          " use cache for function group objects
          IF line_exists( lt_progs[ prog = lv_objname ] ).
            lv_devclass = lt_progs[ prog = lv_objname ]-devclass.
          ELSE.
              SELECT SINGLE devclass INTO lv_devclass FROM tadir
                  WHERE object = lv_objtype AND obj_name = lv_objname. "#EC WARNOK "#EC CI_SGLSELECT
              APPEND VALUE ts_prog_devclass( prog = lv_objname devclass = lv_devclass ) TO lt_progs.
          ENDIF.

      ELSEIF lv_objtype = 'INTF' OR lv_objtype = 'ENHO'.
        " interface, enhancement implementation case
        " find out which package the ABAP object belongs to
        SELECT SINGLE devclass INTO lv_devclass FROM tadir
            WHERE object = lv_objtype AND obj_name = lv_objname. "#EC WARNOK "#EC CI_SGLSELECT
      ELSEIF lv_objtype = 'TABD' OR lv_objtype = 'TABL'.
        " data table case
        " find out which package the ABAP object belongs to
        SELECT SINGLE devclass INTO lv_devclass FROM tadir
            WHERE object = 'TABL' AND obj_name = lv_objname. "#EC WARNOK "#EC CI_SGLSELECT
      ELSEIF lv_objtype = 'DTED' OR lv_objtype = 'DTEL'.
        " data element case
        " find out which package the ABAP object belongs to
        SELECT SINGLE devclass INTO lv_devclass FROM tadir
            WHERE object = 'DTEL' AND obj_name = lv_objname. "#EC WARNOK "#EC CI_SGLSELECT
      ELSEIF lv_objtype = 'DOMA' OR lv_objtype = 'DOMD'.
        " domain case
        " find out which package the ABAP object belongs to
        SELECT SINGLE devclass INTO lv_devclass FROM tadir
            WHERE object = 'DOMA' AND obj_name = lv_objname. "#EC WARNOK "#EC CI_SGLSELECT
      ELSEIF lv_objtype = 'ENQU' OR lv_objtype = 'ENQD'.
        " lock object case
        " find out which package the ABAP object belongs to
        SELECT SINGLE devclass INTO lv_devclass FROM tadir
            WHERE object = 'ENQU' AND obj_name = lv_objname. "#EC WARNOK "#EC CI_SGLSELECT
      ELSEIF lv_objtype = 'SHLP' OR lv_objtype = 'SHLD'.
        " search help type case
        " find out which package the ABAP object belongs to
        SELECT SINGLE devclass INTO lv_devclass FROM tadir
            WHERE object = 'SHLP' AND obj_name = lv_objname. "#EC WARNOK "#EC CI_SGLSELECT
      ELSEIF lv_objtype = 'TTYP' OR lv_objtype = 'TTYD'.
        " table type case
        " find out which package the ABAP object belongs to
        SELECT SINGLE devclass INTO lv_devclass FROM tadir
            WHERE object = 'TTYP' AND obj_name = lv_objname. "#EC WARNOK "#EC CI_SGLSELECT
      ELSEIF lv_objtype = 'VIEW' OR lv_objtype = 'VIED'.
        " view case
        " find out which package the ABAP object belongs to
        SELECT SINGLE devclass INTO lv_devclass FROM tadir
            WHERE object = 'VIEW' AND obj_name = lv_objname. "#EC WARNOK "#EC CI_SGLSELECT
      ELSEIF lv_objtype = 'DYNP'.
        " to be handled later
      ELSEIF lv_objtype = 'XSLT'.
        " transformation XML case
        " find out which package the ABAP object belongs to
        SELECT SINGLE devclass INTO lv_devclass FROM tadir
            WHERE object = 'XSLT' AND obj_name = lv_objname. "#EC WARNOK "#EC CI_SGLSELECT
      ELSEIF lv_objtype = 'REPT'.

        " report text case
        " find out which package the ABAP object belongs to
        CLEAR lt_objname_parts.
        SPLIT lv_objname AT '=' INTO TABLE lt_objname_parts.
        lv_classname = lt_objname_parts[ 1 ].
        IF strlen( lv_classname ) > 30.
            lv_classname = lv_objname+0(30).
        ENDIF.
        CLEAR ls_tadir.
        SELECT SINGLE * INTO ls_tadir FROM tadir
            WHERE obj_name = lv_classname. "#EC WARNOK "#EC CI_SGLSELECT
        lv_devclass = ls_tadir-devclass.
        lv_objtype2 = ls_tadir-object.

      ELSEIF lv_objtype = 'FORM' OR lv_objtype = 'TEXT'.

        "SAPScript case
        " find out which package the ABAP object belongs to
        SELECT SINGLE devclass INTO lv_devclass FROM tadir
            WHERE object = 'FORM' AND obj_name = lv_objname. "#EC WARNOK "#EC CI_SGLSELECT
        IF iv_needcontent = abap_true.
          me->build_sapscript_change(
              EXPORTING
                  iv_cs_request = ld_cs_request
                  iv_object = <fs_cs_request_object>
                  iv_devclass = lv_devclass
                  iv_objname = lv_objname
                  iv_objtype = lv_objtype
                  iv_deltastats = iv_deltastats
                  iv_maxrow = iv_maxrow
                  iv_maxfulltable = iv_maxfulltable
              CHANGING
                  it_commit_objects = it_commit_objects
                   ).
        ENDIF.

        CONTINUE.

      ELSEIF lv_objtype = 'NOTE' OR lv_objtype = 'CINS' OR lv_objtype = 'SOTT' OR lv_objtype = 'SOTS' OR lv_objtype = 'SOTU'.

        " skip for OSS note, correction, concept texts
        CONTINUE.

      ELSE.

        IF iv_needcontent = abap_true.
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
        ENDIF.

        CONTINUE.

      ENDIF.

      " ABAP source code or table schema cases

      lv_haspackage = abap_true.
      IF sy-subrc <> 0.
        " the ABAP object is not found from tadir, it may be a deleted object
        lv_haspackage = abap_false.
      ENDIF.

      " fetch versions no later than given TR date/time
      CLEAR: lv_version_no, lt_objversions.
      lv_success = me->get_versions_no(
          EXPORTING
              iv_objname = lv_objname
              iv_objtype = lv_objtype
              iv_mode = iv_mode
              iv_trid = ld_cs_request-h-trkorr
              iv_date = ld_cs_request-h-as4date
              iv_time = ld_cs_request-h-as4time
              iv_findtest = abap_false
          IMPORTING
              ev_version_no = lv_version_no
              ev_date = lv_verdate
              ev_time = lv_vertime
          CHANGING
              cht_objversions = lt_objversions
              ).
      IF lv_success = abap_false AND lv_haspackage = abap_false.
        " can't locate this object in table tadir and versions neither
        " fail to find which package it belongs to and can't place it to ADO push payload
        me->write_telemetry( iv_message = |deleted object { lv_objname } type { lv_objtype } can't be processed without package name available| ).
        CONTINUE.
      ENDIF.

      " screen has program name for package name
      CLEAR lv_progname.
      IF lv_version_no > 0 AND lv_objtype = 'DYNP'.
        rv_success = me->get_dynp_content(
            EXPORTING
                iv_version = lt_objversions[ 1 ]-objversionno
                iv_objname = lt_objversions[ 1 ]-objname
                iv_needcontent = abap_false
            IMPORTING
                ev_prog = lv_progname
                ev_devclass = lv_devclass
                 ).
        CHECK rv_success = abap_true.
      ENDIF.

      " is the object in (sub package of) one of the packages specified?
      " only applied in latest mode
      IF iv_mode = c_latest_version.
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
      ENDIF.

      " is there any version found?
      CHECK lv_version_no > 0.

      " fetch function group name in case of function module
      IF lv_objtype = 'FUGR'.
        lv_funcname = lv_objname.
        lv_objtype2 = 'FUNC'.
        me->get_fugr( EXPORTING iv_objname = lv_funcname IMPORTING ev_fugrname = lv_fugr ).
      ENDIF.

      CLEAR: lt_filecontent, lv_filecontent.

      IF iv_needcontent = abap_true.

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
        ELSEIF lv_objtype = 'DTED' OR lv_objtype = 'DTEL'.
          IF iv_deltastats = abap_false.
            me->get_dataelement_content(
                EXPORTING
                    iv_version = lt_objversions[ 1 ]-objversionno
                    iv_objname = lt_objversions[ 1 ]-objname
                IMPORTING
                    ev_filecontent = lv_filecontent
                    et_filecontent = lt_filecontent
                     ).
          ENDIF.
        ELSEIF lv_objtype = 'DOMA' OR lv_objtype = 'DOMD'.
          IF iv_deltastats = abap_false.
            me->get_domain_content(
                EXPORTING
                    iv_version = lt_objversions[ 1 ]-objversionno
                    iv_objname = lt_objversions[ 1 ]-objname
                IMPORTING
                    ev_filecontent = lv_filecontent
                    et_filecontent = lt_filecontent
                     ).
          ENDIF.
        ELSEIF lv_objtype = 'ENQU' OR lv_objtype = 'ENQD'.
          IF iv_deltastats = abap_false.
            me->get_lockobject_content(
                EXPORTING
                    iv_version = lt_objversions[ 1 ]-objversionno
                    iv_objname = lt_objversions[ 1 ]-objname
                IMPORTING
                    ev_filecontent = lv_filecontent
                    et_filecontent = lt_filecontent
                     ).
          ENDIF.
        ELSEIF lv_objtype = 'SHLP' OR lv_objtype = 'SHLD'.
          IF iv_deltastats = abap_false.
            me->get_searchhelp_content(
                EXPORTING
                    iv_version = lt_objversions[ 1 ]-objversionno
                    iv_objname = lt_objversions[ 1 ]-objname
                IMPORTING
                    ev_filecontent = lv_filecontent
                    et_filecontent = lt_filecontent
                     ).
          ENDIF.
        ELSEIF lv_objtype = 'TTYP' OR lv_objtype = 'TTYD'.
          IF iv_deltastats = abap_false.
            me->get_tabletype_content(
                EXPORTING
                    iv_version = lt_objversions[ 1 ]-objversionno
                    iv_objname = lt_objversions[ 1 ]-objname
                IMPORTING
                    ev_filecontent = lv_filecontent
                    et_filecontent = lt_filecontent
                     ).
          ENDIF.
        ELSEIF lv_objtype = 'VIEW' OR lv_objtype = 'VIED'.
          IF iv_deltastats = abap_false.
            me->get_view_content(
                EXPORTING
                    iv_version = lt_objversions[ 1 ]-objversionno
                    iv_objname = lt_objversions[ 1 ]-objname
                IMPORTING
                    ev_filecontent = lv_filecontent
                    et_filecontent = lt_filecontent
                     ).
          ENDIF.
        ELSEIF lv_objtype = 'DYNP'.
          IF iv_deltastats = abap_false.
            me->get_dynp_content(
                EXPORTING
                    iv_version = lt_objversions[ 1 ]-objversionno
                    iv_objname = lt_objversions[ 1 ]-objname
                IMPORTING
                    ev_filecontent = lv_filecontent
                    et_filecontent = lt_filecontent
                     ).
          ENDIF.
        ELSEIF lv_objtype = 'XSLT'.
          IF iv_deltastats = abap_false.
            me->get_xslt_content(
                EXPORTING
                    iv_version = lt_objversions[ 1 ]-objversionno
                    iv_objname = lt_objversions[ 1 ]-objname
                IMPORTING
                    ev_filecontent = lv_filecontent
                    et_filecontent = lt_filecontent
                     ).
          ENDIF.
        ELSE.

          CLEAR: lv_total_insertions, lv_total_deletions.

          IF iv_deltastats = abap_true.

            LOOP AT lt_objversions INTO wa_objversion.
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
            lv_filecontent = concat_lines_of( table = lt_filecontent sep = cl_abap_char_utilities=>cr_lf ).

            " align with GUI_DOWNLOAD which adds a blank line
            lv_filecontent = lv_filecontent && cl_abap_char_utilities=>cr_lf.

          ENDIF.

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
          prog = lv_progname
          verno = lv_version_no
          filecontent = lv_filecontent
          insertions = lv_total_insertions
          deletions = lv_total_deletions
          date = lv_verdate
          time = lv_vertime
          ) TO it_commit_objects.

    ENDLOOP.

    " function groups collected
    LOOP AT lt_fugrs INTO DATA(wafugr).

      lv_fugr = wafugr-fugr.
      lv_devclass = wafugr-devclass.

      " is the object in (sub package of) one of the packages specified?
      " only applied in latest mode
      IF iv_mode = c_latest_version.
        CLEAR lt_parentpackages.
        me->get_parentpackages(
            EXPORTING
                iv_package = lv_devclass
            IMPORTING
                et_packages = lt_parentpackages
                 ).
        lv_foundparentpackage = abap_false.
        LOOP AT lt_parentpackages INTO DATA(waparentpackage2).
          IF line_exists( lt_packagenames[ table_line = waparentpackage2 ] ).
            lv_foundparentpackage = abap_true.
            EXIT.
          ENDIF.
        ENDLOOP.
        CHECK lv_foundparentpackage = abap_true.
      ENDIF.

      " function group object itself

      CLEAR: lv_version_no, lt_objversions.
      lv_objname = |SAPL{ wafugr-fugr }|.
      lv_objtype = 'REPS'.
      lv_success = me->get_versions_no(
          EXPORTING
              iv_objname = lv_objname
              iv_objtype = lv_objtype
              iv_mode = iv_mode
              iv_trid = ld_cs_request-h-trkorr
              iv_date = ld_cs_request-h-as4date
              iv_time = ld_cs_request-h-as4time
              iv_findtest = abap_false
          IMPORTING
              ev_version_no = lv_version_no
              ev_date = lv_verdate
              ev_time = lv_vertime
          CHANGING
              cht_objversions = lt_objversions
              ).
      IF lv_success = abap_true AND lv_version_no > 0.
        CLEAR: lt_filecontent, lv_filecontent.
        lv_success = me->get_code_content_or_heatmap(
            EXPORTING
                iv_objname = lv_objname
                iv_objtype = lv_objtype
                it_objversions = lt_objversions
                iv_needcontent = iv_needcontent
                iv_deltastats = abap_false
            IMPORTING
                et_filecontent = lt_filecontent
                ev_filecontent = lv_filecontent
                ev_total_insertions = lv_total_insertions
                ev_total_deletions = lv_total_deletions
                    ).
        IF lv_success = abap_true.
          lv_objname = wafugr-fugr.
          lv_objtype = 'FUGR'.
          TRANSLATE lv_objname TO UPPER CASE.
          IF NOT line_exists( it_commit_objects[ devclass = lv_devclass objname = lv_objname objtype = lv_objtype ] ).
            APPEND VALUE ts_commit_object(
                devclass = lv_devclass
                objname = lv_objname
                objtype = lv_objtype
                delflag = abap_false
                verno = lv_version_no
                filecontent = lv_filecontent
                date = lv_verdate
                time = lv_vertime
                ) TO it_commit_objects.
          ENDIF.
        ENDIF.
      ENDIF.

      " FMs and includes of the function group

      DATA(pname_filter) = |SAPL{ lv_fugr }|.
      CLEAR: lt_fgfmcodes, lt_fginccodes.
      SELECT funcname INTO TABLE @lt_fgfmcodes FROM tfdir WHERE pname = @pname_filter.
      pname_filter = |L{ lv_fugr }%|.
      SELECT name INTO TABLE @lt_fginccodes FROM trdir WHERE name LIKE @pname_filter.

      LOOP AT lt_fgfmcodes INTO DATA(wafgfm).

        lv_objname = wafgfm.
        lv_objtype = 'FUNC'.
        lv_objtype2 = 'FUNC'.

        " fetch versions no later than given TR date/time
        CLEAR: lv_version_no, lt_objversions.
        lv_success = me->get_versions_no(
            EXPORTING
                iv_objname = lv_objname
                iv_objtype = lv_objtype
                iv_mode = iv_mode
                iv_trid = ld_cs_request-h-trkorr
                iv_date = ld_cs_request-h-as4date
                iv_time = ld_cs_request-h-as4time
                iv_findtest = abap_false
            IMPORTING
                ev_version_no = lv_version_no
                ev_date = lv_verdate
                ev_time = lv_vertime
            CHANGING
                cht_objversions = lt_objversions
                ).
        IF lv_success = abap_false.
          " can't locate this object in table tadir and versions neither
          " fail to find which package it belongs to and can't place it to ADO push payload
          me->write_telemetry( iv_message = |deleted object { lv_objname } type { lv_objtype } can't be processed without package name available| ).
          CONTINUE.
        ENDIF.

        CLEAR: lt_filecontent, lv_filecontent.
        lv_success = me->get_code_content_or_heatmap(
            EXPORTING
                iv_objname = lv_objname
                iv_objtype = lv_objtype
                it_objversions = lt_objversions
                iv_needcontent = iv_needcontent
                iv_deltastats = iv_deltastats
            IMPORTING
                et_filecontent = lt_filecontent
                ev_filecontent = lv_filecontent
                ev_total_insertions = lv_total_insertions
                ev_total_deletions = lv_total_deletions
                    ).
        CHECK lv_success = abap_true.

        TRANSLATE lv_objname TO UPPER CASE.

        IF line_exists( it_commit_objects[ devclass = lv_devclass objname = lv_objname objtype = lv_objtype fugr = lv_fugr ] ).
          CONTINUE.
        ENDIF.

        APPEND VALUE ts_commit_object(
            devclass = lv_devclass
            objname = lv_objname
            objtype = lv_objtype
            objtype2 = lv_objtype2
            fugr = lv_fugr
            delflag = abap_false
            verno = lv_version_no
            filecontent = lv_filecontent
            insertions = lv_total_insertions
            deletions = lv_total_deletions
            date = lv_verdate
            time = lv_vertime
            ) TO it_commit_objects.

      ENDLOOP.

      LOOP AT lt_fginccodes INTO DATA(wafginc).

        lv_objname = wafginc.
        lv_objtype = 'REPS'.
        lv_objtype2 = 'FUNC'.

        " fetch versions no later than given TR date/time
        CLEAR: lv_version_no, lt_objversions.
        lv_success = me->get_versions_no(
            EXPORTING
                iv_objname = lv_objname
                iv_objtype = lv_objtype
                iv_mode = iv_mode
                iv_trid = ld_cs_request-h-trkorr
                iv_date = ld_cs_request-h-as4date
                iv_time = ld_cs_request-h-as4time
                iv_findtest = abap_false
            IMPORTING
                ev_version_no = lv_version_no
                ev_date = lv_verdate
                ev_time = lv_vertime
            CHANGING
                cht_objversions = lt_objversions
                ).
        IF lv_success = abap_false.
          " can't locate this object in table tadir and versions neither
          " fail to find which package it belongs to and can't place it to ADO push payload
          me->write_telemetry( iv_message = |deleted object { lv_objname } type { lv_objtype } can't be processed without package name available| ).
          CONTINUE.
        ENDIF.

        CLEAR: lt_filecontent, lv_filecontent.
        lv_success = me->get_code_content_or_heatmap(
            EXPORTING
                iv_objname = lv_objname
                iv_objtype = lv_objtype
                it_objversions = lt_objversions
                iv_needcontent = iv_needcontent
                iv_deltastats = iv_deltastats
            IMPORTING
                et_filecontent = lt_filecontent
                ev_filecontent = lv_filecontent
                ev_total_insertions = lv_total_insertions
                ev_total_deletions = lv_total_deletions
                    ).
        CHECK lv_success = abap_true.

        TRANSLATE lv_objname TO UPPER CASE.

        IF line_exists( it_commit_objects[ devclass = lv_devclass objname = lv_objname objtype = lv_objtype fugr = lv_fugr ] ).
          CONTINUE.
        ENDIF.

        APPEND VALUE ts_commit_object(
            devclass = lv_devclass
            objname = lv_objname
            objtype = lv_objtype
            objtype2 = lv_objtype2
            fugr = lv_fugr
            delflag = abap_false
            verno = lv_version_no
            filecontent = lv_filecontent
            insertions = lv_total_insertions
            deletions = lv_total_deletions
            date = lv_verdate
            time = lv_vertime
            ) TO it_commit_objects.

      ENDLOOP.

    ENDLOOP.

    LOOP AT lt_progs INTO DATA(waprog).

      " report texts of the program
      CLEAR: lv_version_no, lt_objversions.
      lv_objname = waprog-prog.
      lv_objtype = 'REPT'.
      lv_objtype2 = 'PROG'.
      lv_success = me->get_versions_no(
          EXPORTING
              iv_objname = lv_objname
              iv_objtype = lv_objtype
              iv_mode = iv_mode
              iv_trid = ld_cs_request-h-trkorr
              iv_date = ld_cs_request-h-as4date
              iv_time = ld_cs_request-h-as4time
              iv_findtest = abap_false
          IMPORTING
              ev_version_no = lv_version_no
              ev_date = lv_verdate
              ev_time = lv_vertime
          CHANGING
              cht_objversions = lt_objversions
              ).
      CHECK lv_success = abap_true.
      CHECK lines( lt_objversions ) > 0.

      IF iv_deltastats = abap_false.
          CLEAR: lv_filecontent, lt_filecontent.
          me->get_rept_content(
              EXPORTING
                  iv_version = lt_objversions[ 1 ]-objversionno
                  iv_objname = lt_objversions[ 1 ]-objname
              IMPORTING
                  ev_filecontent = lv_filecontent
                  et_filecontent = lt_filecontent
                   ).

          TRANSLATE lv_objname TO UPPER CASE.

          IF line_exists( it_commit_objects[ devclass = lv_devclass objname = lv_objname objtype = lv_objtype ] ).
              CONTINUE.
          ENDIF.

          APPEND VALUE ts_commit_object(
              devclass = lv_devclass
              objname = lv_objname
              objtype = 'REPT'
              objtype2 = lv_objtype2
              delflag = abap_false
              verno = lv_version_no
              filecontent = lv_filecontent
              date = lv_verdate
              time = lv_vertime
              ) TO it_commit_objects.
      ENDIF.

      " screens of the program
      CLEAR lt_scnnums.
      SELECT dnum FROM d020s INTO TABLE lt_scnnums WHERE prog = waprog-prog.
      LOOP AT lt_scnnums INTO DATA(wascnnum).

        CLEAR: lv_version_no, lt_objversions.
        lv_objname = |{ waprog-prog }{ wascnnum }|.
        lv_objtype = 'DYNP'.

        lv_success = me->get_versions_no(
            EXPORTING
                iv_objname = lv_objname
                iv_objtype = lv_objtype
                iv_mode = iv_mode
                iv_trid = ld_cs_request-h-trkorr
                iv_date = ld_cs_request-h-as4date
                iv_time = ld_cs_request-h-as4time
                iv_findtest = abap_false
            IMPORTING
                ev_version_no = lv_version_no
                ev_date = lv_verdate
                ev_time = lv_vertime
            CHANGING
                cht_objversions = lt_objversions
                ).
        CHECK lv_success = abap_true.
        CHECK lines( lt_objversions ) > 0.

        CLEAR: lv_filecontent, lt_filecontent.
        lv_success = me->get_dynp_content(
            EXPORTING
                iv_objname = lt_objversions[ 1 ]-objname
                iv_version = lt_objversions[ 1 ]-objversionno
            IMPORTING
                ev_filecontent = lv_filecontent
                et_filecontent = lt_filecontent
                 ).
        CHECK lv_success = abap_true.

        TRANSLATE lv_objname TO UPPER CASE.

        IF line_exists( it_commit_objects[ devclass = lv_devclass objname = lv_objname objtype = lv_objtype ] ).
            CONTINUE.
        ENDIF.

        APPEND VALUE ts_commit_object(
            devclass = lv_devclass
            objname = lv_objname
            objtype = 'DYNP'
            delflag = abap_false
            verno = lv_version_no
            filecontent = lv_filecontent
            date = lv_verdate
            time = lv_vertime
            ) TO it_commit_objects.

      ENDLOOP.

    ENDLOOP.

  ENDMETHOD.

  METHOD build_table_config_change.

    FIELD-SYMBOLS <fs_cs_request_key> LIKE LINE OF iv_cs_request-keys.
    DATA lt_filecontent TYPE tty_abaptext.
    DATA lv_objname TYPE string.
    DATA lv_objtype TYPE string.
    DATA lv_fulltablecount TYPE i.
    DATA lv_filecontent TYPE string.
    DATA lv_rows TYPE i.
    DATA lv_success TYPE abap_bool.

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
        lv_filecontent = concat_lines_of( table = lt_filecontent sep = cl_abap_char_utilities=>cr_lf ).
        " align with GUI_DOWNLOAD which adds a blank line
        lv_filecontent = lv_filecontent && cl_abap_char_utilities=>cr_lf.
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

      lv_filecontent = concat_lines_of( table = lt_filecontent sep = cl_abap_char_utilities=>cr_lf ).

      " align with GUI_DOWNLOAD which adds a blank line
      lv_filecontent = lv_filecontent && cl_abap_char_utilities=>cr_lf.

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

      lv_filecontent = concat_lines_of( table = lt_filecontent sep = cl_abap_char_utilities=>cr_lf ).

      " align with GUI_DOWNLOAD which adds a blank line
      lv_filecontent = lv_filecontent && cl_abap_char_utilities=>cr_lf.

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

  METHOD build_sapscript_change.

    DATA lv_filecontent  TYPE string.
    DATA lt_filecontent TYPE tty_abaptext.
    DATA lv_success TYPE abap_bool.
    DATA lv_objname TYPE string.
    DATA lv_objtype TYPE string VALUE 'FORM'.
    DATA lt_form_versions TYPE TABLE OF itcformver.
    DATA lv_form TYPE thead-tdform.

    lv_success = ZCL_UTILITY_ABAPTOGIT_FMT=>get_sapscript_content(
        EXPORTING
            iv_objname = iv_objname
        IMPORTING
            et_filecontent = lt_filecontent
             ).

    IF lv_success <> abap_true.
        me->write_telemetry( iv_message = |fail to get sapscript for { iv_objname }| ).
        RETURN.
    ENDIF.

    lv_filecontent = concat_lines_of( table = lt_filecontent sep = cl_abap_char_utilities=>cr_lf ).

    " align with GUI_DOWNLOAD which adds a blank line
    lv_filecontent = lv_filecontent && cl_abap_char_utilities=>cr_lf.

    IF line_exists( it_commit_objects[ devclass = iv_devclass objname = iv_objname objtype = lv_objtype ] ).
        RETURN.
    ENDIF.

    APPEND VALUE ts_commit_object(
        devclass = iv_devclass
        objname = lv_objname
        objtype = lv_objtype
        objtype2 = lv_objtype
        fugr = ''
        progcls = 'full'
        delflag = abap_false
        verno = 1
        filecontent = lv_filecontent
        ) TO it_commit_objects.

  ENDMETHOD.

  METHOD build_logical_config_change.

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
    DATA lv_success TYPE abap_bool.

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
          tabname   = lv_tabname
          mtype     = wakey-mastertype
          mtabname  = wakey-mastername
        TABLES
          e071k_tab = lt_keys
          entry_tab = lt_entry_tab
        EXCEPTIONS
          OTHERS    = 1.
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
          lv_filecontent = <fs_commit_object>-filecontent && concat_lines_of( table = lt_filecontent sep = cl_abap_char_utilities=>cr_lf ).
        ELSE.
          lv_filecontent = concat_lines_of( table = lt_filecontent sep = cl_abap_char_utilities=>cr_lf ).
        ENDIF.
        " align with GUI_DOWNLOAD which adds a blank line
        lv_filecontent = lv_filecontent && cl_abap_char_utilities=>cr_lf.
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

      lv_filecontent = concat_lines_of( table = lt_filecontent sep = cl_abap_char_utilities=>cr_lf ).

      " align with GUI_DOWNLOAD which adds a blank line
      lv_filecontent = lv_filecontent && cl_abap_char_utilities=>cr_lf.

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

      lv_filecontent = concat_lines_of( table = lt_filecontent sep = cl_abap_char_utilities=>cr_lf ).

      " align with GUI_DOWNLOAD which adds a blank line
      lv_filecontent = lv_filecontent && cl_abap_char_utilities=>cr_lf.

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

  METHOD build_report_variant_change.
    DATA lv_filecontent  TYPE string.
    DATA lt_filecontent TYPE tty_abaptext.
    DATA lv_success TYPE abap_bool.
    DATA lv_objname TYPE string.
    DATA lv_objtype TYPE string.

    lv_success = me->get_varx_content(
        EXPORTING
            iv_objname = iv_objname
        IMPORTING
            et_filecontent = lt_filecontent
             ).

    IF lv_success <> abap_true.
      me->write_telemetry( iv_message = |fail to get report variants for { iv_objname }| ).
      EXIT.
    ENDIF.

    lv_filecontent = concat_lines_of( table = lt_filecontent sep = cl_abap_char_utilities=>cr_lf ).

    " align with GUI_DOWNLOAD which adds a blank line
    lv_filecontent = lv_filecontent && cl_abap_char_utilities=>cr_lf.

    APPEND VALUE ts_commit_object(
        devclass = c_varx
        objname = lv_objname
        objtype = lv_objtype
        objtype2 = lv_objtype
        fugr = ''
        progcls = 'full'
        delflag = abap_false
        verno = 1
        filecontent = lv_filecontent
        ) TO it_commit_objects.
  ENDMETHOD.

  METHOD get_fugr.

    DATA lv_pname_filter TYPE string.
    SELECT SINGLE pname INTO @lv_pname_filter FROM tfdir WHERE funcname = @iv_objname.
    IF strlen( lv_pname_filter ) < 4.
      ev_fugrname = ''.
    ELSE.
      ev_fugrname = lv_pname_filter+4.    " SAPL... as prefix for names in pname field
    ENDIF.

  ENDMETHOD.


  METHOD get_dataelement_content.

    DATA lv_no_release_transformation TYPE svrs_bool.
    DATA lv_info_line TYPE abaptext-line.
    DATA smodilog_new TYPE TABLE OF smodilog.
    DATA smodisrc_new TYPE TABLE OF smodisrc.
    DATA lt_dd04tv_tab TYPE TABLE OF dd04tv.
    DATA lt_dd04v_tab TYPE TABLE OF dd04v.
    DATA ls_data_element_desc TYPE ty_data_element_desc.
    DATA ls_dd04v TYPE dd04v.
    DATA lv_ddtext TYPE string.

    IF iv_logdest = space OR iv_logdest = 'NONE'.
      lv_no_release_transformation = abap_true.
    ELSE.
      lv_no_release_transformation = abap_false.
    ENDIF.

    CALL FUNCTION 'SVRS_GET_VERSION_DTED_40'
      EXPORTING
        destination                  = ''
        object_name                  = iv_objname
        versno                       = iv_version
        iv_no_release_transformation = lv_no_release_transformation
      IMPORTING
        info_line                    = lv_info_line
      TABLES
        vsmodisrc                    = smodisrc_new
        dd04tv_tab                   = lt_dd04tv_tab
        dd04v_tab                    = lt_dd04v_tab
        vsmodilog                    = smodilog_new
      EXCEPTIONS
        no_version                   = 1
        OTHERS                       = 2.
    IF sy-subrc <> 0.
      me->write_telemetry( iv_message = |GET_DATAELEMENT_CONTENT fails in fetching content lines for { iv_objname } version { iv_version }| ).
      rv_success = abap_false.
      RETURN.
    ENDIF.
    IF lines( lt_dd04v_tab ) = 0.
      me->write_telemetry( iv_message = |GET_DATAELEMENT_CONTENT fetched zero content lines for { iv_objname } version { iv_version }| ).
      rv_success = abap_false.
      RETURN.
    ENDIF.

    ls_dd04v = lt_dd04v_tab[ 1 ].
    ls_data_element_desc-as4user = ls_dd04v-as4user.
    ls_data_element_desc-as4date = ls_dd04v-as4date.
    ls_data_element_desc-as4time = ls_dd04v-as4time.
    ls_data_element_desc-rollname = ls_dd04v-rollname.
    ls_data_element_desc-ddlanguage = ls_dd04v-ddlanguage.
    ls_data_element_desc-domname = ls_dd04v-domname.
    IF line_exists( lt_dd04tv_tab[ ddlanguage = 'E' ] ).
      ls_data_element_desc-ddtext = lt_dd04tv_tab[ ddlanguage = 'E' ]-ddtext.
    ELSE.
      ls_data_element_desc-ddtext = ls_dd04v-ddtext.
    ENDIF.
    ls_data_element_desc-datatype = ls_dd04v-datatype.
    ls_data_element_desc-leng = ls_dd04v-leng.
    ls_data_element_desc-decimals = ls_dd04v-decimals.
    ls_data_element_desc-outputlen = ls_dd04v-outputlen.
    ls_data_element_desc-headlen = ls_dd04v-headlen.
    ls_data_element_desc-scrlen1 = ls_dd04v-scrlen1.
    ls_data_element_desc-scrlen2 = ls_dd04v-scrlen2.
    ls_data_element_desc-scrlen3 = ls_dd04v-scrlen3.
    ls_data_element_desc-reptext = ls_dd04v-reptext.
    ls_data_element_desc-scrtext_s = ls_dd04v-scrtext_s.
    ls_data_element_desc-scrtext_m = ls_dd04v-scrtext_m.
    ls_data_element_desc-scrtext_l = ls_dd04v-scrtext_l.

    /ui2/cl_json=>serialize(
        EXPORTING
            !data = ls_data_element_desc
            pretty_name = /ui2/cl_json=>pretty_mode-low_case
        RECEIVING
            r_json = ev_filecontent
             ).

    " beautify a bit with line breaks for code diff benefit
    REPLACE ALL OCCURRENCES OF ',' IN ev_filecontent WITH |,{ cl_abap_char_utilities=>cr_lf }|.

    SPLIT ev_filecontent AT cl_abap_char_utilities=>cr_lf INTO TABLE et_filecontent.

    " align with GUI_DOWNLOAD which adds a blank line
    ev_filecontent = ev_filecontent && cl_abap_char_utilities=>cr_lf.

    rv_success = abap_true.

  ENDMETHOD.


  METHOD get_domain_content.

    DATA lv_no_release_transformation TYPE svrs_bool.
    DATA lv_info_line TYPE abaptext-line.
    DATA smodilog_new TYPE TABLE OF smodilog.
    DATA smodisrc_new TYPE TABLE OF smodisrc.
    DATA lt_dd01tv_tab TYPE TABLE OF dd01tv.
    DATA lt_dd01v_tab TYPE TABLE OF dd01v.
    DATA lt_dd07tv_tab TYPE TABLE OF dd07tv.
    DATA lt_dd07v_tab TYPE TABLE OF dd07v.
    DATA ls_domain_desc TYPE ty_domain_desc.
    DATA ls_dd01v TYPE dd01v.

    IF iv_logdest = space OR iv_logdest = 'NONE'.
      lv_no_release_transformation = abap_true.
    ELSE.
      lv_no_release_transformation = abap_false.
    ENDIF.

    CALL FUNCTION 'SVRS_GET_VERSION_DOMD_40'
      EXPORTING
        destination                  = ''
        object_name                  = iv_objname
        versno                       = iv_version
        iv_no_release_transformation = lv_no_release_transformation
      IMPORTING
        info_line                    = lv_info_line
      TABLES
        vsmodisrc                    = smodisrc_new
        dd01tv_tab                   = lt_dd01tv_tab
        dd01v_tab                    = lt_dd01v_tab
        dd07tv_tab                   = lt_dd07tv_tab
        dd07v_tab                    = lt_dd07v_tab
        vsmodilog                    = smodilog_new
      EXCEPTIONS
        no_version                   = 1
        OTHERS                       = 2.
    IF sy-subrc <> 0.
      me->write_telemetry( iv_message = |GET_DOMAIN_CONTENT fails in fetching content lines for { iv_objname } version { iv_version }| ).
      rv_success = abap_false.
      RETURN.
    ENDIF.
    IF lines( lt_dd01v_tab ) = 0.
      me->write_telemetry( iv_message = |GET_DOMAIN_CONTENT fetched zero content lines for { iv_objname } version { iv_version }| ).
      rv_success = abap_false.
      RETURN.
    ENDIF.

    ls_dd01v = lt_dd01v_tab[ 1 ].
    ls_domain_desc-as4user = ls_dd01v-as4user.
    ls_domain_desc-as4date = ls_dd01v-as4date.
    ls_domain_desc-as4time = ls_dd01v-as4time.
    ls_domain_desc-domname = ls_dd01v-domname.
    ls_domain_desc-ddlanguage = ls_dd01v-ddlanguage.
    IF line_exists( lt_dd01tv_tab[ ddlanguage = 'E' ] ).
      ls_domain_desc-ddtext = lt_dd01tv_tab[ ddlanguage = 'E' ]-ddtext.
    ELSE.
      ls_domain_desc-ddtext = ls_dd01v-ddtext.
    ENDIF.
    ls_domain_desc-datatype = ls_dd01v-datatype.
    ls_domain_desc-leng = ls_dd01v-leng.
    ls_domain_desc-decimals = ls_dd01v-decimals.
    ls_domain_desc-outputlen = ls_dd01v-outputlen.
    ls_domain_desc-entitytab = ls_dd01v-entitytab.

    /ui2/cl_json=>serialize(
        EXPORTING
            !data = ls_domain_desc
            pretty_name = /ui2/cl_json=>pretty_mode-low_case
        RECEIVING
            r_json = ev_filecontent
             ).

    " beautify a bit with line breaks for code diff benefit
    REPLACE ALL OCCURRENCES OF ',' IN ev_filecontent WITH |,{ cl_abap_char_utilities=>cr_lf }|.

    SPLIT ev_filecontent AT cl_abap_char_utilities=>cr_lf INTO TABLE et_filecontent.

    " align with GUI_DOWNLOAD which adds a blank line
    ev_filecontent = ev_filecontent && cl_abap_char_utilities=>cr_lf.

    rv_success = abap_true.

  ENDMETHOD.


  METHOD get_lockobject_content.

    DATA lv_no_release_transformation TYPE svrs_bool.
    DATA lv_info_line TYPE abaptext-line.
    DATA smodilog_new TYPE TABLE OF smodilog.
    DATA smodisrc_new TYPE TABLE OF smodisrc.
    DATA lt_dd25tv_tab TYPE TABLE OF dd25tv.
    DATA lt_dd25v_tab TYPE TABLE OF dd25v.
    DATA lt_dd26v_tab TYPE TABLE OF dd26v.
    DATA lt_dd27v_tab TYPE TABLE OF dd27v.
    DATA ls_lock_object_desc TYPE ty_lock_object_desc.
    DATA ls_dd25v TYPE dd25v.
    DATA ls_dd26v TYPE ty_dd26v.
    DATA ls_dd27v TYPE ty_dd27v.

    IF iv_logdest = space OR iv_logdest = 'NONE'.
      lv_no_release_transformation = abap_true.
    ELSE.
      lv_no_release_transformation = abap_false.
    ENDIF.

    CALL FUNCTION 'SVRS_GET_VERSION_ENQD_40'
      EXPORTING
        destination                  = ''
        object_name                  = iv_objname
        versno                       = iv_version
        iv_no_release_transformation = lv_no_release_transformation
      IMPORTING
        info_line                    = lv_info_line
      TABLES
        vsmodisrc                    = smodisrc_new
        dd25tv_tab                   = lt_dd25tv_tab
        dd25v_tab                    = lt_dd25v_tab
        dd26v_tab                    = lt_dd26v_tab
        dd27v_tab                    = lt_dd27v_tab
        vsmodilog                    = smodilog_new
      EXCEPTIONS
        no_version                   = 1
        OTHERS                       = 2.
    IF sy-subrc <> 0.
      me->write_telemetry( iv_message = |GET_LOCKOBJECT_CONTENT fails in fetching content lines for { iv_objname } version { iv_version }| ).
      rv_success = abap_false.
      RETURN.
    ENDIF.
    IF lines( lt_dd25v_tab ) = 0.
      me->write_telemetry( iv_message = |GET_LOCKOBJECT_CONTENT fetched zero content lines for { iv_objname } version { iv_version }| ).
      rv_success = abap_false.
      RETURN.
    ENDIF.

    ls_dd25v = lt_dd25v_tab[ 1 ].
    ls_lock_object_desc-dd25v-as4user = ls_dd25v-as4user.
    ls_lock_object_desc-dd25v-as4date = ls_dd25v-as4date.
    ls_lock_object_desc-dd25v-as4time = ls_dd25v-as4time.
    ls_lock_object_desc-dd25v-viewname = ls_dd25v-viewname.
    ls_lock_object_desc-dd25v-ddlanguage = ls_dd25v-ddlanguage.
    IF line_exists( lt_dd25tv_tab[ ddlanguage = 'E' ] ).
      ls_lock_object_desc-dd25v-ddtext = lt_dd25tv_tab[ ddlanguage = 'E' ]-ddtext.
    ELSE.
      ls_lock_object_desc-dd25v-ddtext = ls_dd25v-ddtext.
    ENDIF.
    LOOP AT lt_dd26v_tab INTO DATA(wa26).
      CLEAR ls_dd26v.
      ls_dd26v-tabname = wa26-tabname.
      ls_dd26v-tabpos = wa26-tabpos.
      ls_dd26v-fortabname = wa26-fortabname.
      ls_dd26v-forfield = wa26-forfield.
      ls_dd26v-fordir = wa26-fordir.
      APPEND ls_dd26v TO ls_lock_object_desc-dd26v.
    ENDLOOP.

    LOOP AT lt_dd27v_tab INTO DATA(wa27).
      CLEAR ls_dd27v.
      ls_dd27v-viewfield = wa27-viewfield.
      ls_dd27v-objpos = wa27-objpos.
      ls_dd27v-tabname = wa27-tabname.
      ls_dd27v-keyflag = wa27-keyflag.
      ls_dd27v-rollname = wa27-rollname.
      ls_dd27v-rdonly = wa27-rdonly.
      ls_dd27v-dbviewfield = wa27-dbviewfield.
      ls_dd27v-enqmode = wa27-enqmode.
      APPEND ls_dd27v TO ls_lock_object_desc-dd27v.
    ENDLOOP.

    /ui2/cl_json=>serialize(
        EXPORTING
            !data = ls_lock_object_desc
            pretty_name = /ui2/cl_json=>pretty_mode-low_case
        RECEIVING
            r_json = ev_filecontent
             ).

    " beautify a bit with line breaks for code diff benefit
    REPLACE ALL OCCURRENCES OF ',' IN ev_filecontent WITH |,{ cl_abap_char_utilities=>cr_lf }|.

    SPLIT ev_filecontent AT cl_abap_char_utilities=>cr_lf INTO TABLE et_filecontent.

    " align with GUI_DOWNLOAD which adds a blank line
    ev_filecontent = ev_filecontent && cl_abap_char_utilities=>cr_lf.

    rv_success = abap_true.

  ENDMETHOD.


  METHOD get_searchhelp_content.

    DATA lv_no_release_transformation TYPE svrs_bool.
    DATA lv_info_line TYPE abaptext-line.
    DATA smodilog_new TYPE TABLE OF smodilog.
    DATA smodisrc_new TYPE TABLE OF smodisrc.
    DATA lt_dd30tv_tab TYPE TABLE OF dd30tv.
    DATA lt_dd30v_tab TYPE TABLE OF dd30v.
    DATA lt_dd31v_tab TYPE TABLE OF dd31v.
    DATA lt_dd32v_tab TYPE TABLE OF dd32v.
    DATA lt_dd33v_tab TYPE TABLE OF dd33v.
    DATA ls_search_help_desc TYPE ty_search_help_desc.
    DATA ls_dd30v TYPE dd30v.
    DATA ls_dd31v TYPE ty_dd31v.
    DATA ls_dd32v TYPE ty_dd32v.
    DATA ls_dd33v TYPE ty_dd33v.

    IF iv_logdest = space OR iv_logdest = 'NONE'.
      lv_no_release_transformation = abap_true.
    ELSE.
      lv_no_release_transformation = abap_false.
    ENDIF.

    CALL FUNCTION 'SVRS_GET_VERSION_SHLD_40'
      EXPORTING
        destination                  = ''
        object_name                  = iv_objname
        versno                       = iv_version
        iv_no_release_transformation = lv_no_release_transformation
      IMPORTING
        info_line                    = lv_info_line
      TABLES
        vsmodisrc                    = smodisrc_new
        dd30tv_tab                   = lt_dd30tv_tab
        dd30v_tab                    = lt_dd30v_tab
        dd31v_tab                    = lt_dd31v_tab
        dd32v_tab                    = lt_dd32v_tab
        dd33v_tab                    = lt_dd33v_tab
        vsmodilog                    = smodilog_new
      EXCEPTIONS
        no_version                   = 1
        OTHERS                       = 2.
    IF sy-subrc <> 0.
      me->write_telemetry( iv_message = |GET_SEARCHHELP_CONTENT fails in fetching content lines for { iv_objname } version { iv_version }| ).
      rv_success = abap_false.
      RETURN.
    ENDIF.
    IF lines( lt_dd30v_tab ) = 0.
      me->write_telemetry( iv_message = |GET_SEARCHHELP_CONTENT fetched zero content lines for { iv_objname } version { iv_version }| ).
      rv_success = abap_false.
      RETURN.
    ENDIF.

    ls_dd30v = lt_dd30v_tab[ 1 ].
    ls_search_help_desc-dd30v-as4user = ls_dd30v-as4user.
    ls_search_help_desc-dd30v-as4date = ls_dd30v-as4date.
    ls_search_help_desc-dd30v-as4time = ls_dd30v-as4time.
    ls_search_help_desc-dd30v-shlpname = ls_dd30v-shlpname.
    ls_search_help_desc-dd30v-ddlanguage = ls_dd30v-ddlanguage.
    ls_search_help_desc-dd30v-issimple = ls_dd30v-issimple.
    ls_search_help_desc-dd30v-selmethod = ls_dd30v-selmethod.
    ls_search_help_desc-dd30v-dialogtype = ls_dd30v-dialogtype.
    IF line_exists( lt_dd30tv_tab[ ddlanguage = 'E' ] ).
      ls_search_help_desc-dd30v-ddtext = lt_dd30tv_tab[ ddlanguage = 'E' ]-ddtext.
    ELSE.
      ls_search_help_desc-dd30v-ddtext = ls_dd30v-ddtext.
    ENDIF.

    LOOP AT lt_dd31v_tab INTO DATA(wa31).
      CLEAR ls_dd31v.
      ls_dd31v-subshlp = wa31-subshlp.
      ls_dd31v-shposition = wa31-shposition.
      ls_dd31v-viashlp = wa31-viashlp.
      ls_dd31v-hideflag = wa31-hideflag.
      APPEND ls_dd31v TO ls_search_help_desc-dd31v.
    ENDLOOP.

    LOOP AT lt_dd32v_tab INTO DATA(wa32).
      CLEAR ls_dd32v.
      ls_dd32v-fieldname = wa32-fieldname.
      ls_dd32v-flposition = wa32-flposition.
      ls_dd32v-rollname = wa32-rollname.
      ls_dd32v-shlpinput = wa32-shlpinput.
      ls_dd32v-shlpoutput = wa32-shlpoutput.
      ls_dd32v-shlpselpos = wa32-shlpselpos.
      ls_dd32v-shlpseldis = wa32-shlpseldis.
      APPEND ls_dd32v TO ls_search_help_desc-dd32v.
    ENDLOOP.

    LOOP AT lt_dd33v_tab INTO DATA(wa33).
      CLEAR ls_dd33v.
      ls_dd33v-fieldname = wa33-fieldname.
      ls_dd33v-subshlp = wa33-subshlp.
      ls_dd33v-subfield = wa33-subfield.
      APPEND ls_dd33v TO ls_search_help_desc-dd33v.
    ENDLOOP.

    /ui2/cl_json=>serialize(
        EXPORTING
            !data = ls_search_help_desc
            pretty_name = /ui2/cl_json=>pretty_mode-low_case
        RECEIVING
            r_json = ev_filecontent
             ).

    " beautify a bit with line breaks for code diff benefit
    REPLACE ALL OCCURRENCES OF ',' IN ev_filecontent WITH |,{ cl_abap_char_utilities=>cr_lf }|.

    SPLIT ev_filecontent AT cl_abap_char_utilities=>cr_lf INTO TABLE et_filecontent.

    " align with GUI_DOWNLOAD which adds a blank line
    ev_filecontent = ev_filecontent && cl_abap_char_utilities=>cr_lf.

    rv_success = abap_true.

  ENDMETHOD.


  METHOD get_tabletype_content.

    DATA lv_no_release_transformation TYPE svrs_bool.
    DATA lv_info_line TYPE abaptext-line.
    DATA smodilog_new TYPE TABLE OF smodilog.
    DATA smodisrc_new TYPE TABLE OF smodisrc.
    DATA lt_dd40tv_tab TYPE TABLE OF dd40tv.
    DATA lt_dd40v_tab TYPE TABLE OF dd40v.
    DATA lt_dd42v_tab TYPE TABLE OF dd42v.
    DATA lt_dd43tv_tab TYPE TABLE OF dd43t.
    DATA lt_dd43v_tab TYPE TABLE OF dd43v.
    DATA ls_table_type_desc TYPE ty_table_type_desc.
    DATA ls_dd40v TYPE dd40v.
    DATA ls_dd42v TYPE ty_dd42v.
    DATA ls_dd43v TYPE ty_dd43v.

    IF iv_logdest = space OR iv_logdest = 'NONE'.
      lv_no_release_transformation = abap_true.
    ELSE.
      lv_no_release_transformation = abap_false.
    ENDIF.

    CALL FUNCTION 'SVRS_GET_VERSION_TTYD_40'
      EXPORTING
        destination                  = ''
        object_name                  = iv_objname
        versno                       = iv_version
        iv_no_release_transformation = lv_no_release_transformation
      IMPORTING
        info_line                    = lv_info_line
      TABLES
        vsmodisrc                    = smodisrc_new
        dd40tv_tab                   = lt_dd40tv_tab
        dd40v_tab                    = lt_dd40v_tab
        dd42v_tab                    = lt_dd42v_tab
        dd43tv_tab                   = lt_dd43tv_tab
        dd43v_tab                    = lt_dd43v_tab
        vsmodilog                    = smodilog_new
      EXCEPTIONS
        no_version                   = 1
        OTHERS                       = 2.
    IF sy-subrc <> 0.
      me->write_telemetry( iv_message = |GET_TABLETYPE_CONTENT fails in fetching content lines for { iv_objname } version { iv_version }| ).
      rv_success = abap_false.
      RETURN.
    ENDIF.
    IF lines( lt_dd40v_tab ) = 0.
      me->write_telemetry( iv_message = |GET_TABLETYPE_CONTENT fetched zero content lines for { iv_objname } version { iv_version }| ).
      rv_success = abap_false.
      RETURN.
    ENDIF.

    ls_dd40v = lt_dd40v_tab[ 1 ].
    ls_table_type_desc-dd40v-as4user = ls_dd40v-as4user.
    ls_table_type_desc-dd40v-as4date = ls_dd40v-as4date.
    ls_table_type_desc-dd40v-as4time = ls_dd40v-as4time.
    ls_table_type_desc-dd40v-typename = ls_dd40v-typename.
    ls_table_type_desc-dd40v-ddlanguage = ls_dd40v-ddlanguage.
    IF line_exists( lt_dd40tv_tab[ ddlanguage = 'E' ] ).
      ls_table_type_desc-dd40v-ddtext = lt_dd40tv_tab[ ddlanguage = 'E' ]-ddtext.
    ELSE.
      ls_table_type_desc-dd40v-ddtext = ls_dd40v-ddtext.
    ENDIF.
    ls_table_type_desc-dd40v-datatype = ls_dd40v-datatype.
    ls_table_type_desc-dd40v-rowtype = ls_dd40v-rowtype.
    ls_table_type_desc-dd40v-rowkind = ls_dd40v-rowkind.
    ls_table_type_desc-dd40v-keykind = ls_dd40v-keykind.
    ls_table_type_desc-dd40v-keyfdcount = ls_dd40v-keyfdcount.
    ls_table_type_desc-dd40v-typelen = ls_dd40v-typelen.
    ls_table_type_desc-dd40v-ttypkind = ls_dd40v-ttypkind.

    LOOP AT lt_dd42v_tab INTO DATA(wa42).
      CLEAR ls_dd42v.
      ls_dd42v-seckeyname = wa42-seckeyname.
      ls_dd42v-keyfdpos = wa42-keyfdpos.
      ls_dd42v-keyfield = wa42-keyfield.
      ls_dd42v-rowtypepos = wa42-rowtypepos.
      APPEND ls_dd42v TO ls_table_type_desc-dd42v.
    ENDLOOP.

    LOOP AT lt_dd43v_tab INTO DATA(wa43).
      CLEAR ls_dd43v.
      ls_dd43v-seckeyname = wa43-seckeyname.
      ls_dd43v-seckeyunique = wa43-seckeyunique.
      ls_dd43v-ddlanguage = wa43-ddlanguage.
      ls_dd43v-kind = wa43-kind.
      ls_dd43v-accessmode = wa43-accessmode.
      ls_dd43v-keydescription = wa43-keydescription.
      APPEND ls_dd42v TO ls_table_type_desc-dd42v.
    ENDLOOP.

    /ui2/cl_json=>serialize(
        EXPORTING
            !data = ls_table_type_desc
            pretty_name = /ui2/cl_json=>pretty_mode-low_case
        RECEIVING
            r_json = ev_filecontent
             ).

    " beautify a bit with line breaks for code diff benefit
    REPLACE ALL OCCURRENCES OF ',' IN ev_filecontent WITH |,{ cl_abap_char_utilities=>cr_lf }|.

    SPLIT ev_filecontent AT cl_abap_char_utilities=>cr_lf INTO TABLE et_filecontent.

    " align with GUI_DOWNLOAD which adds a blank line
    ev_filecontent = ev_filecontent && cl_abap_char_utilities=>cr_lf.

    rv_success = abap_true.

  ENDMETHOD.


  METHOD get_view_content.

    DATA lv_no_release_transformation TYPE svrs_bool.
    DATA lv_info_line TYPE abaptext-line.
    DATA smodilog_new TYPE TABLE OF smodilog.
    DATA smodisrc_new TYPE TABLE OF smodisrc.
    DATA lt_dd25tv_tab TYPE TABLE OF dd25tv.
    DATA lt_dd25v_tab TYPE TABLE OF dd25v.
    DATA lt_dd26v_tab TYPE TABLE OF dd26v.
    DATA lt_dd27v_tab TYPE TABLE OF dd27v.
    DATA lt_dd28v_tab TYPE TABLE OF dd28v.
    DATA ls_view_desc TYPE ty_view_desc.
    DATA ls_dd25v TYPE dd25v.
    DATA ls_dd26v TYPE ty_dd26v.
    DATA ls_dd27v TYPE ty_dd27v.
    DATA ls_dd28v TYPE ty_dd28v.

    IF iv_logdest = space OR iv_logdest = 'NONE'.
      lv_no_release_transformation = abap_true.
    ELSE.
      lv_no_release_transformation = abap_false.
    ENDIF.

    CALL FUNCTION 'SVRS_GET_VERSION_VIED_40'
      EXPORTING
        destination                  = ''
        object_name                  = iv_objname
        versno                       = iv_version
        iv_no_release_transformation = lv_no_release_transformation
      IMPORTING
        info_line                    = lv_info_line
      TABLES
        vsmodisrc                    = smodisrc_new
        dd25tv_tab                   = lt_dd25tv_tab
        dd25v_tab                    = lt_dd25v_tab
        dd26v_tab                    = lt_dd26v_tab
        dd27v_tab                    = lt_dd27v_tab
        dd28v_tab                    = lt_dd28v_tab
        vsmodilog                    = smodilog_new
      EXCEPTIONS
        no_version                   = 1
        OTHERS                       = 2.
    IF sy-subrc <> 0.
      me->write_telemetry( iv_message = |GET_VIEW_CONTENT fails in fetching content lines for { iv_objname } version { iv_version }| ).
      rv_success = abap_false.
      RETURN.
    ENDIF.
    IF lines( lt_dd25v_tab ) = 0.
      me->write_telemetry( iv_message = |GET_VIEW_CONTENT fetched zero content lines for { iv_objname } version { iv_version }| ).
      rv_success = abap_false.
      RETURN.
    ENDIF.

    ls_dd25v = lt_dd25v_tab[ 1 ].
    ls_view_desc-dd25v-as4user = ls_dd25v-as4user.
    ls_view_desc-dd25v-as4date = ls_dd25v-as4date.
    ls_view_desc-dd25v-as4time = ls_dd25v-as4time.
    ls_view_desc-dd25v-viewname = ls_dd25v-viewname.
    ls_view_desc-dd25v-ddlanguage = ls_dd25v-ddlanguage.
    IF line_exists( lt_dd25tv_tab[ ddlanguage = 'E' ] ).
      ls_view_desc-dd25v-ddtext = lt_dd25tv_tab[ ddlanguage = 'E' ]-ddtext.
    ELSE.
      ls_view_desc-dd25v-ddtext = ls_dd25v-ddtext.
    ENDIF.

    LOOP AT lt_dd26v_tab INTO DATA(wa26).
      CLEAR ls_dd26v.
      ls_dd26v-tabname = wa26-tabname.
      ls_dd26v-tabpos = wa26-tabpos.
      ls_dd26v-fortabname = wa26-fortabname.
      ls_dd26v-forfield = wa26-forfield.
      ls_dd26v-fordir = wa26-fordir.
      APPEND ls_dd26v TO ls_view_desc-dd26v.
    ENDLOOP.

    LOOP AT lt_dd27v_tab INTO DATA(wa27).
      CLEAR ls_dd27v.
      ls_dd27v-viewfield = wa27-viewfield.
      ls_dd27v-objpos = wa27-objpos.
      ls_dd27v-tabname = wa27-tabname.
      ls_dd27v-keyflag = wa27-keyflag.
      ls_dd27v-rollname = wa27-rollname.
      ls_dd27v-rdonly = wa27-rdonly.
      ls_dd27v-dbviewfield = wa27-dbviewfield.
      ls_dd27v-enqmode = wa27-enqmode.
      APPEND ls_dd27v TO ls_view_desc-dd27v.
    ENDLOOP.

    LOOP AT lt_dd28v_tab INTO DATA(wa28).
      CLEAR ls_dd28v.
      ls_dd28v-condname = wa28-condname.
      ls_dd28v-position = wa28-position.
      ls_dd28v-tabname = wa28-tabname.
      ls_dd28v-negation = wa28-negation.
      ls_dd28v-operator = wa28-operator.
      ls_dd28v-constants = wa28-constants.
      ls_dd28v-contline = wa28-contline.
      ls_dd28v-and_or = wa28-and_or.
      ls_dd28v-offset = wa28-offset.
      ls_dd28v-flength = wa28-flength.
      ls_dd28v-joperator = wa28-joperator.
      APPEND ls_dd28v TO ls_view_desc-dd28v.
    ENDLOOP.

    /ui2/cl_json=>serialize(
        EXPORTING
            !data = ls_view_desc
            pretty_name = /ui2/cl_json=>pretty_mode-low_case
        RECEIVING
            r_json = ev_filecontent
             ).

    " beautify a bit with line breaks for code diff benefit
    REPLACE ALL OCCURRENCES OF ',' IN ev_filecontent WITH |,{ cl_abap_char_utilities=>cr_lf }|.

    SPLIT ev_filecontent AT cl_abap_char_utilities=>cr_lf INTO TABLE et_filecontent.

    " align with GUI_DOWNLOAD which adds a blank line
    ev_filecontent = ev_filecontent && cl_abap_char_utilities=>cr_lf.

    rv_success = abap_true.

  ENDMETHOD.


  METHOD get_dynp_content.

    DATA lv_no_release_transformation TYPE svrs_bool.
    DATA lv_info_line TYPE abaptext-line.
    DATA smodilog_new TYPE TABLE OF smodilog.
    DATA smodisrc_new TYPE TABLE OF smodisrc.
    DATA lt_d020s_tab TYPE TABLE OF d020s.
    DATA lt_d020t_tab TYPE TABLE OF d020t.
    DATA lt_d021s_tab TYPE TABLE OF d021se.
    DATA lt_d021t_tab TYPE TABLE OF d021t.
    DATA lt_d022s_tab TYPE TABLE OF d022s.
    DATA lt_d023s_tab TYPE TABLE OF d023s.
    DATA ls_d020s TYPE d020s.

    IF iv_logdest = space OR iv_logdest = 'NONE'.
      lv_no_release_transformation = abap_true.
    ELSE.
      lv_no_release_transformation = abap_false.
    ENDIF.

    CALL FUNCTION 'SVRS_GET_VERSION_DYNP_40'
      EXPORTING
        destination                  = ''
        object_name                  = iv_objname
        versno                       = iv_version
        iv_no_release_transformation = lv_no_release_transformation
      IMPORTING
        info_line                    = lv_info_line
      TABLES
        vsmodisrc                    = smodisrc_new
        d020s_tab                    = lt_d020s_tab
        d020t_tab                    = lt_d020t_tab
        d021s_tab                    = lt_d021s_tab
        d021t_tab                    = lt_d021t_tab
        d022s_tab                    = lt_d022s_tab
        d023s_tab                    = lt_d023s_tab
        vsmodilog                    = smodilog_new
      EXCEPTIONS
        no_version                   = 1
        OTHERS                       = 2.
    IF sy-subrc <> 0.
      me->write_telemetry( iv_message = |GET_DYNP_CONTENT fails in fetching content lines for { iv_objname } version { iv_version }| ).
      rv_success = abap_false.
      RETURN.
    ENDIF.
    IF lines( lt_d020s_tab ) = 0.
      me->write_telemetry( iv_message = |GET_DYNP_CONTENT fetched zero content lines for { iv_objname } version { iv_version }| ).
      rv_success = abap_false.
      RETURN.
    ENDIF.

    ls_d020s = lt_d020s_tab[ 1 ].

    SELECT SINGLE devclass FROM tadir INTO ev_devclass WHERE
      object = 'DYNP' AND obj_name = ls_d020s-prog. "#EC WARNOK "#EC CI_SGLSELECT

    ev_prog = ls_d020s-prog.

    IF iv_needcontent = abap_true.
      APPEND |* Program:        { ls_d020s-prog }| TO et_filecontent.
      APPEND |* Language:       { ls_d020s-spra }| TO et_filecontent.
      APPEND |* Screen number:  { ls_d020s-dnum }| TO et_filecontent.
      APPEND |* Screen type:    { ls_d020s-type }| TO et_filecontent.
      APPEND |* VALP:           { ls_d020s-valp }| TO et_filecontent.
      APPEND |* CUAN:           { ls_d020s-cuan }| TO et_filecontent.
      APPEND |* Next dynpro:    { ls_d020s-fnum }| TO et_filecontent.
      APPEND |* Cursor position:{ ls_d020s-cupo }| TO et_filecontent.
      APPEND |* Screen group:   { ls_d020s-dgrp }| TO et_filecontent.
      APPEND |* BZMX:           { ls_d020s-bzmx }| TO et_filecontent.
      APPEND |* BZBR:           { ls_d020s-bzbr }| TO et_filecontent.
      APPEND |* MILI:           { ls_d020s-mili }| TO et_filecontent.
      APPEND |* MICO:           { ls_d020s-mico }| TO et_filecontent.
      APPEND |* MALI:           { ls_d020s-mali }| TO et_filecontent.
      APPEND |* MACO:           { ls_d020s-maco }| TO et_filecontent.
      APPEND |* NOLI:           { ls_d020s-noli }| TO et_filecontent.
      APPEND |* NOCO:           { ls_d020s-noco }| TO et_filecontent.
      APPEND |* Date Generation:{ ls_d020s-dgen }| TO et_filecontent.
      APPEND |* Time Generation:{ ls_d020s-tgen }| TO et_filecontent.

      APPEND '' TO et_filecontent.
      APPEND '* Screen dynpro fields below' TO et_filecontent.
      LOOP AT lt_d021s_tab ASSIGNING FIELD-SYMBOL(<fs_d021s>).
        APPEND |* Field name:   { <fs_d021s>-fnam }| TO et_filecontent.
        APPEND |*   TYPE:         { <fs_d021s>-type }| TO et_filecontent.
        APPEND |*   LINE:         { <fs_d021s>-line }| TO et_filecontent.
        APPEND |*   COLN:         { <fs_d021s>-coln }| TO et_filecontent.
        APPEND |*   DIDX:         { <fs_d021s>-didx }| TO et_filecontent.
        APPEND |*   FLG1:         { <fs_d021s>-flg1 }| TO et_filecontent.
        APPEND |*   FLG2:         { <fs_d021s>-flg2 }| TO et_filecontent.
        APPEND |*   FLG3:         { <fs_d021s>-flg3 }| TO et_filecontent.
        APPEND |*   FILL:         { <fs_d021s>-fill }| TO et_filecontent.
        APPEND |*   FMB1:         { <fs_d021s>-fmb1 }| TO et_filecontent.
        APPEND |*   FMB2:         { <fs_d021s>-fmb2 }| TO et_filecontent.
        APPEND |*   COLR:         { <fs_d021s>-colr }| TO et_filecontent.
        APPEND |*   LENG:         { <fs_d021s>-leng }| TO et_filecontent.
        APPEND |*   LTYP:         { <fs_d021s>-ltyp }| TO et_filecontent.
        APPEND |*   LANF:         { <fs_d021s>-lanf }| TO et_filecontent.
        APPEND |*   LBLK:         { <fs_d021s>-lblk }| TO et_filecontent.
        APPEND |*   LREP:         { <fs_d021s>-lrep }| TO et_filecontent.
        APPEND |*   FMKY:         { <fs_d021s>-fmky }| TO et_filecontent.
        APPEND |*   PAID:         { <fs_d021s>-paid }| TO et_filecontent.
        APPEND |*   AUTH:         { <fs_d021s>-auth }| TO et_filecontent.
        APPEND |*   WNAM:         { <fs_d021s>-wnam }| TO et_filecontent.
        APPEND |*   DMAC:         { <fs_d021s>-dmac }| TO et_filecontent.
        APPEND |*   GRP1:         { <fs_d021s>-grp1 }| TO et_filecontent.
        APPEND |*   GRP2:         { <fs_d021s>-grp2 }| TO et_filecontent.
        APPEND |*   GRP3:         { <fs_d021s>-grp3 }| TO et_filecontent.
        APPEND |*   GRP4:         { <fs_d021s>-grp4 }| TO et_filecontent.
        APPEND |*   ITYP:         { <fs_d021s>-ityp }| TO et_filecontent.
        APPEND |*   AGLT:         { <fs_d021s>-aglt }| TO et_filecontent.
        APPEND |*   ADEZ:         { <fs_d021s>-adez }| TO et_filecontent.
        APPEND |*   STXT:         { <fs_d021s>-stxt }| TO et_filecontent.
        APPEND |*   RES1:         { <fs_d021s>-res1 }| TO et_filecontent.
        APPEND |*   RES2:         { <fs_d021s>-res2 }| TO et_filecontent.
      ENDLOOP.

      APPEND '' TO et_filecontent.
      APPEND '* Screen code below' TO et_filecontent.
      LOOP AT lt_d022s_tab ASSIGNING FIELD-SYMBOL(<fs_d022s>).
        APPEND <fs_d022s>-line TO et_filecontent.
      ENDLOOP.

      ev_filecontent = concat_lines_of( table = et_filecontent sep = cl_abap_char_utilities=>cr_lf ).
    ENDIF.

    rv_success = abap_true.

  ENDMETHOD.


  METHOD get_xslt_content.

    DATA lv_no_release_transformation TYPE svrs_bool.
    DATA lv_info_line TYPE abaptext-line.
    DATA smodilog_new TYPE TABLE OF smodilog.
    DATA smodisrc_new TYPE TABLE OF smodisrc.
    DATA lt_o2xattr_tab TYPE TABLE OF o2xsltattr.
    DATA lt_o2p_tab TYPE TABLE OF o2pageline.
    DATA lt_o2xtext_tab TYPE TABLE OF o2xslttext.

    IF iv_logdest = space OR iv_logdest = 'NONE'.
      lv_no_release_transformation = abap_true.
    ELSE.
      lv_no_release_transformation = abap_false.
    ENDIF.

    CALL FUNCTION 'SVRS_GET_VERSION_XSLT_40'
      EXPORTING
        destination                  = ''
        object_name                  = iv_objname
        versno                       = iv_version
        iv_no_release_transformation = lv_no_release_transformation
      IMPORTING
        info_line                    = lv_info_line
      TABLES
        vsmodisrc                    = smodisrc_new
        xsatt_tab                    = lt_o2xattr_tab
        xssrc_tab                    = lt_o2p_tab
        xstxt_tab                    = lt_o2xtext_tab
        mdlog_tab                    = smodilog_new
      EXCEPTIONS
        no_version                   = 1
        OTHERS                       = 2.
    IF sy-subrc <> 0.
      me->write_telemetry( iv_message = |GET_XSLT_CONTENT fails in fetching content lines for { iv_objname } version { iv_version }| ).
      rv_success = abap_false.
      RETURN.
    ENDIF.

    LOOP AT lt_o2p_tab ASSIGNING FIELD-SYMBOL(<fs>).
      APPEND <fs>-line TO et_filecontent.
    ENDLOOP.

    ev_filecontent = concat_lines_of( table = et_filecontent sep = cl_abap_char_utilities=>cr_lf ).

    " align with GUI_DOWNLOAD which adds a blank line
    ev_filecontent = ev_filecontent && cl_abap_char_utilities=>cr_lf.

    rv_success = abap_true.

  ENDMETHOD.


  METHOD get_rept_content.

    DATA lv_no_release_transformation TYPE svrs_bool.
    DATA lv_info_line TYPE abaptext-line.
    DATA smodilog_new TYPE TABLE OF smodilog.
    DATA smodisrc_new TYPE TABLE OF smodisrc.
    DATA lt_repot_tab TYPE TABLE OF textpoolt.
    DATA lt_trdir_tab TYPE TABLE OF trdir.

    IF iv_logdest = space OR iv_logdest = 'NONE'.
      lv_no_release_transformation = abap_true.
    ELSE.
      lv_no_release_transformation = abap_false.
    ENDIF.

    CALL FUNCTION 'SVRS_GET_VERSION_REPT_40'
      EXPORTING
        destination                  = ''
        object_name                  = iv_objname
        versno                       = iv_version
        iv_no_release_transformation = lv_no_release_transformation
      IMPORTING
        info_line                    = lv_info_line
      TABLES
        vsmodisrc                    = smodisrc_new
        repot_tab                    = lt_repot_tab
        trdir_tab                    = lt_trdir_tab
        vsmodilog                    = smodilog_new
      EXCEPTIONS
        no_version                   = 1
        OTHERS                       = 2.
    IF sy-subrc <> 0.
      me->write_telemetry( iv_message = |GET_REPT_CONTENT fails in fetching content lines for { iv_objname } version { iv_version }| ).
      rv_success = abap_false.
      RETURN.
    ENDIF.

    LOOP AT lt_repot_tab ASSIGNING FIELD-SYMBOL(<fs>).
      APPEND |{ <fs>-id } { <fs>-key } { <fs>-lang }: { <fs>-entry }| TO et_filecontent.
    ENDLOOP.

    ev_filecontent = concat_lines_of( table = et_filecontent sep = cl_abap_char_utilities=>cr_lf ).

    " align with GUI_DOWNLOAD which adds a blank line
    ev_filecontent = ev_filecontent && cl_abap_char_utilities=>cr_lf.

    rv_success = abap_true.

  ENDMETHOD.

  METHOD get_varx_content.
    DATA: lv_objnamestring TYPE string.
    DATA: lv_itab         TYPE TABLE OF string.
    DATA: lv_report_str   TYPE string.
    DATA: lv_variant_str  TYPE string.
    DATA: lv_report       TYPE  vari_reprt.
    DATA: lv_variant      TYPE  variant.
    DATA: lt_valuetab     TYPE TABLE OF rsparams.

    lv_objnamestring = iv_objname.
    SPLIT lv_objnamestring AT space INTO lv_report_str lv_variant_str.

    CONDENSE lv_report_str.
    CONDENSE lv_variant_str.

    lv_report = lv_report_str.
    lv_variant = lv_variant_str.
    DATA:
        valuetab     TYPE TABLE OF rsparams.

    TRY.
        CALL FUNCTION 'RS_VARIANT_CONTENTS'
          EXPORTING
            report               = lv_report
            variant              = lv_variant
          TABLES
            valutab              = lt_valuetab
          EXCEPTIONS
            variant_non_existent = 1
            variant_obsolete     = 2
            OTHERS               = 3.
        IF sy-subrc <> 0.
          " Implement suitable error handling here
        ENDIF.

*        APPEND |selname kind sign option low high| TO et_filecontent.
        LOOP AT lt_valuetab INTO DATA(lv_row).
          CONCATENATE
            lv_row-selname
            lv_row-kind
            lv_row-sign
            lv_row-option
            lv_row-low
            INTO DATA(lv_row_str) SEPARATED BY space.
          APPEND |{ lv_row_str }| TO et_filecontent.
        ENDLOOP.
*        et_filecontent = lt_valuetab.
        rv_success = abap_true.
      CATCH cx_root.
    ENDTRY.
  ENDMETHOD.

  METHOD get_code_lines.

    DATA: lv_no_release_transformation TYPE svrs_bool.
    DATA: trdir_new TYPE TABLE OF trdir INITIAL SIZE 1.
    DATA: smodilog_new TYPE TABLE OF smodilog.

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
        no_version                   = 1
        OTHERS                       = 2.
    IF sy-subrc <> 0.
      me->write_telemetry( iv_message = |GET_CODE_LINES fails in fetching code lines for { iv_objname } type { iv_objtype } version { iv_version }| ).
      rv_success = abap_false.
      RETURN.
    ENDIF.

    linecount = lines( abaptext ).
    rv_success = abap_true.

  ENDMETHOD.


  METHOD get_code_lines_enho.

    DATA lv_enhancement_id TYPE enhname.
    DATA r_vers TYPE REF TO if_enh_tool.
    DATA lv_state TYPE r3state.
    DATA lv_tooltype TYPE enhtooltype.
    DATA: l_clas_data TYPE enhclassmethdata,
          l_fugr_data TYPE enhfugrdata,
          l_hook_data TYPE enh_hook_admin.

    rv_success = abap_false.

    lv_enhancement_id = iv_objname.

    TRY.
        CALL METHOD cl_enh_factory=>get_enhancement
          EXPORTING
            enhancement_id = lv_enhancement_id
            versno         = iv_version
            rfcdestination = iv_logdest
          RECEIVING
            enhancement    = r_vers.
      CATCH cx_enh_io_error.
        RETURN.
      CATCH cx_enh_root.
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
                data    = l_clas_data.
            APPEND LINES OF l_clas_data-enh_eimpsource TO abaptext.
            LOOP AT l_clas_data-enh_methsources INTO DATA(wa_enh_meth).
              APPEND '' TO abaptext.
              APPEND '' TO abaptext.
              APPEND LINES OF wa_enh_meth-source TO abaptext.
            ENDLOOP.
            rv_success = abap_true.
          CATCH cx_enh_no_valid_input_type .
            RETURN.
        ENDTRY.
      WHEN 'FUGRENH'.
        TRY.
            CALL METHOD r_vers->if_enh_object~get_data
              EXPORTING
                version = lv_state
              IMPORTING
                data    = l_fugr_data.
            " TODO function group case
          CATCH cx_enh_no_valid_input_type .
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
                data    = l_hook_data.
            " stitch all enhancement ID source code lines to one file
            LOOP AT l_hook_data-hook_impls INTO DATA(wa_hook_impl).
              APPEND |ENHANCEMENT { wa_hook_impl-id } { iv_objname }.| TO abaptext.
              APPEND LINES OF wa_hook_impl-source TO abaptext.
              APPEND |ENDENHANCEMENT.| TO abaptext.
            ENDLOOP.
            APPEND '' TO abaptext.
            rv_success = abap_true.
          CATCH cx_enh_no_valid_input_type .
            RETURN.
        ENDTRY.
      WHEN OTHERS.
        RETURN.
    ENDCASE.

  ENDMETHOD.


  METHOD build_code_content.

    DATA lt_abaptext TYPE tty_abaptext.
    DATA lv_firstmethod TYPE c VALUE abap_true.
    DATA lv_linecount TYPE i.
    DATA lv_success TYPE abap_bool.
    DATA lv_hasmethod TYPE c VALUE abap_false.
    FIELD-SYMBOLS <fstxt> TYPE abaptxt255.
    DATA: textline     TYPE string,
          abaptextline TYPE string.
    DATA: tclstextline     TYPE string,
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


  METHOD get_table_schema.

    DATA:
      dd02tv_tab TYPE TABLE OF dd02tv,
      dd02v_tab  TYPE TABLE OF dd02v,
      dd03tv_tab TYPE TABLE OF dd03tv,
      dd03v_tab  TYPE TABLE OF dd03v,
      dd05v_tab  TYPE TABLE OF dd05v,
      dd08tv_tab TYPE TABLE OF dd08tv,
      dd08v_tab  TYPE TABLE OF dd08v,
      dd35v_tab  TYPE TABLE OF dd35v,
      dd36v_tab  TYPE TABLE OF dd36v.
    DATA dd03p_tab_a TYPE TABLE OF dd03p.
    DATA lv_objname TYPE vrsd-objname.
    DATA lv_tabname TYPE dd02l-tabname.
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
        OTHERS      = 02.
    IF sy-subrc <> 0.
      me->write_telemetry( iv_message = |GET_TABLE_SCHEMA fails in fetching data table header for { iv_objname } version { iv_version }| ).
      RETURN.
    ENDIF.

    IF lines( dd02v_tab ) = 0.
      me->write_telemetry( iv_message = |GET_TABLE_SCHEMA fails in fetching data table header for { iv_objname } with enough version| ).
      RETURN.
    ENDIF.

    " get English (default German) field description
    lv_tabname = iv_objname.
    CALL FUNCTION 'DD_TABL_GET'
      EXPORTING
        langu          = 'E'
        tabl_name      = lv_tabname
        withtext       = abap_true
      TABLES
        dd03p_tab_a    = dd03p_tab_a
      EXCEPTIONS
        access_failure = 1.
    IF sy-subrc <> 0.
      lv_eddtext = abap_false.
    ENDIF.

    ev_desc-dd02v-tabname = dd02v_tab[ 1 ]-tabname.
    ev_desc-dd02v-ddlanguage = dd02v_tab[ 1 ]-ddlanguage.
    ev_desc-dd02v-tabclass = dd02v_tab[ 1 ]-tabclass.
    ev_desc-dd02v-clidep = dd02v_tab[ 1 ]-clidep.
    ev_desc-dd02v-as4user = dd02v_tab[ 1 ]-as4user.
    ev_desc-dd02v-as4date = dd02v_tab[ 1 ]-as4date.
    ev_desc-dd02v-as4time = dd02v_tab[ 1 ]-as4time.
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
      IF lv_eddtext = abap_true AND line_exists( dd03p_tab_a[ fieldname = wa_dd03v-fieldname ] ).
        ls_data_table_field-ddtext = dd03p_tab_a[ fieldname = wa_dd03v-fieldname ]-ddtext.
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


  METHOD build_data_table_content.

    DATA ls_data_table_desc TYPE ty_data_table_desc.

    rv_success = me->get_table_schema(
        EXPORTING
            iv_objname = iv_objname
            iv_version = iv_version
        IMPORTING
            ev_desc = ls_data_table_desc
             ).
    CHECK rv_success = abap_true.

    /ui2/cl_json=>serialize(
        EXPORTING
            !data = ls_data_table_desc
            pretty_name = /ui2/cl_json=>pretty_mode-low_case
        RECEIVING
            r_json = ev_filecontent
             ).

    " beautify a bit with line breaks for code diff benefit
    REPLACE ALL OCCURRENCES OF ',' IN ev_filecontent WITH |,{ cl_abap_char_utilities=>cr_lf }|.
    LOOP AT ls_data_table_desc-dd03v ASSIGNING FIELD-SYMBOL(<fs>).
      REPLACE ALL OCCURRENCES OF '`' IN <fs>-ddtext WITH ','.
    ENDLOOP.

    SPLIT ev_filecontent AT cl_abap_char_utilities=>cr_lf INTO TABLE et_filecontent.

    " align with GUI_DOWNLOAD which adds a blank line
    ev_filecontent = ev_filecontent && cl_abap_char_utilities=>cr_lf.

    rv_success = abap_true.

  ENDMETHOD.


  METHOD build_schema_content_active.

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
    SELECT SINGLE pwert FROM t52ba INTO @lv_cnt WHERE potyp = 'SCHE' AND ponam = @iv_schemaname AND pattr = 'CNT'. "#EC WARNOK

    " fetch schema's description text
    SELECT SINGLE ltext FROM t52cc_t INTO @lv_ltext WHERE sname = @iv_schemaname. "#EC WARNOK "#EC CI_SGLSELECT

    " meta data for this schema, don't want to bother to add an additional XML/JSON description file
    APPEND '* Schema header part is auto generated and not part of engineer changes.' TO et_filecontent.
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


  METHOD build_pcr_content_active.

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
    APPEND '* PCR header part is auto generated and not part of engineer changes.' TO et_filecontent.
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


  METHOD get_config_lines_header.

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
              iv_trid = iv_request-h-trkorr
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
    APPEND 'Field marked with X is key field.' TO abaptext.
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
        APPEND |    { lv_field } { wafield-keyflag } { lv_field_desc }| TO abaptext.
      ENDLOOP.
      APPEND '' TO abaptext.
    ENDIF.

  ENDMETHOD.


  METHOD get_config_delta_lines.

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
        tabname   = lv_tabname
        mtype     = iv_objtype
        mtabname  = iv_mtabname
      TABLES
        e071k_tab = iv_request-keys
        entry_tab = lt_entry_tab
      EXCEPTIONS
        OTHERS    = 1.
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


  METHOD get_config_table_lines.

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
      <fs_table>  TYPE ANY TABLE,
      <fs_struct> TYPE any,
      <fs_field>  TYPE any.
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
        p_name         = iv_tabname
      RECEIVING
        p_descr_ref    = lo_typedesc
      EXCEPTIONS
        type_not_found = 1.
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
          lv_line = lv_line && |{ lv_value }| && cl_abap_char_utilities=>horizontal_tab.
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


  METHOD build_config_log.

    DATA lv_tabname TYPE e071-obj_name.
    DATA ls_data_table_desc TYPE ty_data_table_desc.
    DATA lv_row TYPE i.
    FIELD-SYMBOLS <fs_entry> TYPE line.
    DATA lv_entry TYPE dbtablog.
    DATA lv_string TYPE string.
    DATA fromday TYPE d.
    DATA fromtime TYPE t.
    DATA lt_log_list TYPE stprt_log_stable_type.
    DATA lt_obj_cnt_list TYPE stprt_obj_app_cnt_list_type.
    DATA lt_user_list TYPE stprt_user_name_list_type.

    APPEND VALUE stprt_user_name_sel_line_type( sign = 'I' option = 'EQ' low = iv_user high = '' ) TO lt_user_list.

    CALL FUNCTION 'DBLOG_READ_WITH_STATISTIC'
      EXPORTING
        from_day             = iv_fromdate
        from_time            = iv_fromtime
        to_day               = iv_todate
        to_time              = iv_totime
        user_list            = lt_user_list
      CHANGING
        log_list             = lt_log_list
        obj_cnt_list         = lt_obj_cnt_list
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

    lv_entry = lt_log_list[ tabname = lv_tabname ].     "#EC CI_SORTSEQ
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


  METHOD build_packnumber_text.

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
      cl_trex_char_utility=>char_to_int(
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


  METHOD get_tr_creation.

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
        iv_log_type     = 'FILE'
        iv_logname_file = lv_logfile
        iv_timestamp    = lv_timestamp
        iv_client       = lv_client
        iv_language     = 'E'
      TABLES
        et_lines        = lt_lines
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


  METHOD get_versions_no_helper.

    DATA ls_objversion TYPE ts_version_no.

    CLEAR cht_vers.
    rv_success = me->get_versions(
        EXPORTING
            iv_objname = iv_objname
            iv_objtype = iv_objtype
        CHANGING
            it_vers = cht_vers
             ).
    CHECK rv_success = abap_true.
    rv_success = me->get_valued_version(
        EXPORTING
            iv_mode = iv_mode
            iv_trid = iv_trid
            iv_date = iv_date
            iv_time = iv_time
        IMPORTING
            ev_versno = ev_versno
            ev_verscnt = ev_version_no
            ev_date = ev_date
            ev_time = ev_time
        CHANGING
            cht_vers = cht_vers
             ).
    IF rv_success = abap_true AND cht_objversions IS SUPPLIED.
      ls_objversion-objversionno = ev_versno.
      ls_objversion-objname = iv_objname.
      ls_objversion-objtype = iv_objtype.
      ls_objversion-date = ev_date.
      ls_objversion-time = ev_time.
      APPEND ls_objversion TO cht_objversions.
    ENDIF.

  ENDMETHOD.


  METHOD get_versions_no.

    DATA wa_ver TYPE vrsd.
    DATA lt_vers TYPE vrsd_tab.
    DATA wa_objversion TYPE ts_version_no.
    DATA lv_versno TYPE versno.
    DATA lv_objtype TYPE e071-object.

    CLEAR ev_version_no.

    IF iv_objtype = 'CLAS'.
      " class object case, multiple versions may return including public/protected/private sections and methods
      " if test class required, its versions will be fetched together
      ev_version_no = me->get_class_versions_no(
          EXPORTING
              iv_objname = iv_objname
              iv_objtype = iv_objtype
              iv_mode = iv_mode
              iv_trid = iv_trid
              iv_date = iv_date
              iv_time = iv_time
              iv_findtest = iv_findtest
          IMPORTING
              ev_date = ev_date
              ev_time = ev_time
          CHANGING
              cht_objversions = cht_objversions
               ).
      rv_success = abap_true.
      RETURN.
    ENDIF.

    IF iv_objtype = 'FUNC'.
      " function module case
      lv_objtype = 'FUNC'.
    ELSEIF iv_objtype = 'CINC'.
      " in TR commit scenario test class will be requested separately
      lv_objtype = 'CINC'.
    ELSEIF iv_objtype = 'PROG'.
      " program/include case
      lv_objtype = 'REPS'.
    ELSEIF iv_objtype = 'REPS'.
      " include case
      lv_objtype = 'REPS'.
    ELSEIF iv_objtype = 'INTF'.
      " interface case
      lv_objtype = 'INTF'.
    ELSEIF iv_objtype = 'ENHO'.
      " enhancement implementation case
      lv_objtype = 'ENHO'.
    ELSEIF iv_objtype = 'TABL' OR iv_objtype = 'TABD'.
      " data table case
      lv_objtype = 'TABD'.
    ELSEIF iv_objtype = 'DTEL' OR iv_objtype = 'DTED'.
      " data table case
      lv_objtype = 'DTED'.
    ELSEIF iv_objtype = 'DOMA' OR iv_objtype = 'DOMD'.
      " data table case
      lv_objtype = 'DOMD'.
    ELSEIF iv_objtype = 'ENQU' OR iv_objtype = 'ENQD'.
      " lock object case
      lv_objtype = 'ENQD'.
    ELSEIF iv_objtype = 'SHLP' OR iv_objtype = 'SHLD'.
      " search help object case
      lv_objtype = 'SHLD'.
    ELSEIF iv_objtype = 'TTYP' OR iv_objtype = 'TTYD'.
      " data table case
      lv_objtype = 'TTYD'.
    ELSEIF iv_objtype = 'VIEW' OR iv_objtype = 'VIED'.
      " view case
      lv_objtype = 'VIED'.
    ELSEIF iv_objtype = 'DYNP'.
      " view case
      lv_objtype = 'DYNP'.
    ELSEIF iv_objtype = 'REPT'.
      " report text case
      lv_objtype = 'REPT'.
    ELSEIF iv_objtype = 'XSLT'.
      " view case
      lv_objtype = 'XSLT'.
    ELSE.
      rv_success = abap_false.
      ev_version_no = 0.
      CLEAR: ev_date, ev_time.
      RETURN.
    ENDIF.

    rv_success = me->get_versions_no_helper(
        EXPORTING
            iv_objname = iv_objname
            iv_objtype = lv_objtype
            iv_mode = iv_mode
            iv_trid = iv_trid
            iv_date = iv_date
            iv_time = iv_time
        IMPORTING
            ev_version_no = ev_version_no
            ev_versno = lv_versno
            ev_date = ev_date
            ev_time = ev_time
        CHANGING
            cht_vers = lt_vers
            cht_objversions = cht_objversions
             ).
  ENDMETHOD.


  METHOD get_valued_version.

    DATA lv_mode TYPE string.
    lv_mode = iv_mode.
    TRANSLATE lv_mode TO LOWER CASE.

    SORT cht_vers DESCENDING BY versno.

    IF lv_mode <> c_active_version AND iv_date IS NOT INITIAL AND iv_time IS NOT INITIAL.
      " remove version not matching TR ID and later than given time, keep active/inactive version
      DELETE cht_vers WHERE versno <> 0 AND versno <> 99999 AND korrnum <> iv_trid AND ( datum > iv_date OR ( datum = iv_date AND zeit >= iv_time ) ).
    ENDIF.

    CLEAR: ev_date, ev_time.

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
      ENDIF.
      " at least one released version available, use it
      ev_versno = cht_vers[ 1 ]-versno.
      ev_date = cht_vers[ 1 ]-datum.
      ev_time = cht_vers[ 1 ]-zeit.
      rv_success = abap_true.
    ELSEIF lv_mode = c_active_version.
      " exclude inactive version
      DELETE cht_vers WHERE versno = 99999.
      ev_verscnt = lines( cht_vers ).
      IF ev_verscnt = 0.
        rv_success = abap_false.
        RETURN.
      ENDIF.
      IF line_exists( cht_vers[ versno = 0 ] ).
        " active version available, use it
        ev_versno = 0.
        ev_date = cht_vers[ versno = 0 ]-datum.
        ev_time = cht_vers[ versno = 0 ]-zeit.
        rv_success = abap_true.
      ELSE.
        " use latest released version since active one unavailable
        ev_versno = cht_vers[ 1 ]-versno.
        ev_date = cht_vers[ 1 ]-datum.
        ev_time = cht_vers[ 1 ]-zeit.
        rv_success = abap_true.
      ENDIF.
    ELSE.
      me->write_telemetry( iv_message = |invalid mode { iv_mode }| ).
      rv_success = abap_false.
    ENDIF.

  ENDMETHOD.


  METHOD get_code_content_or_heatmap.

    DATA wa_objversion TYPE ts_version_no.
    DATA lt_tclsfilecontent TYPE tty_abaptext.
    DATA lv_tclsname TYPE string.
    DATA lv_tclstype TYPE string.
    DATA lv_tclsfilecontent TYPE string.
    DATA lv_insertions TYPE i.
    DATA lv_deletions TYPE i.
    DATA lv_objverprev TYPE versno.
    DATA lv_no_delta TYPE c.

    CLEAR: et_filecontent, ev_filecontent.

    IF iv_needcontent = abap_true.

      CLEAR: ev_total_insertions, ev_total_deletions.

      IF iv_deltastats = abap_true.

        LOOP AT it_objversions INTO wa_objversion.
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
          ev_total_insertions = ev_total_insertions + lv_insertions.
          ev_total_deletions = ev_total_deletions + lv_deletions.
        ENDLOOP.

      ELSE.

        CLEAR et_filecontent.
        CLEAR lt_tclsfilecontent.
        CLEAR lv_tclsfilecontent.
        rv_success = me->build_code_content(
            EXPORTING
                iv_objname = iv_objname
                iv_objtype = iv_objtype
                it_objversions = it_objversions
            IMPORTING
                et_filecontent = et_filecontent
                ev_tclsname = lv_tclsname
                ev_tclstype = lv_tclstype
                et_tclsfilecontent = lt_tclsfilecontent
                ).
        CHECK rv_success = abap_true.

        " stitch to string from source code lines
        ev_filecontent = concat_lines_of( table = et_filecontent sep = cl_abap_char_utilities=>cr_lf ).

        " align with GUI_DOWNLOAD which adds a blank line
        ev_filecontent = ev_filecontent && cl_abap_char_utilities=>cr_lf.

      ENDIF.

    ENDIF.
  ENDMETHOD.


  METHOD get_class_versions_no.

    DATA lv_versno TYPE versno.
    DATA lv_verscnt TYPE i.
    DATA lv_objname TYPE e071-obj_name.
    DATA lv_classname TYPE classname.
    DATA lt_methods TYPE abap_methdescr_tab.
    DATA lv_methodname TYPE versobjnam.
    DATA wa_ver TYPE vrsd.
    DATA lt_vers TYPE vrsd_tab.
    DATA wa_objversion TYPE ts_version_no.
    FIELD-SYMBOLS <fsmethod> LIKE LINE OF lt_methods.
    DATA lv_clskey TYPE seoclskey.
    DATA lv_limu TYPE seok_limu.
    DATA lv_inctype TYPE char3.
    DATA lv_progm TYPE programm.
    DATA lv_testclassname TYPE versobjnam.
    DATA lv_date TYPE d.
    DATA lv_time TYPE t.
    DATA lv_latestdate TYPE d.
    DATA lv_latesttime TYPE t.
    DATA lv_success TYPE abap_bool.

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
            iv_trid = iv_trid
            iv_date = iv_date
            iv_time = iv_time
        IMPORTING
            ev_versno = lv_versno
            ev_verscnt = lv_verscnt
            ev_date = lv_date
            ev_time = lv_time
        CHANGING
            cht_vers = lt_vers
             ).
    IF lv_success = abap_true AND cht_objversions IS SUPPLIED.
      wa_objversion-objversionno = lv_versno.
      wa_objversion-objname = iv_objname.
      wa_objversion-objtype = 'CPUB'.
      wa_objversion-date = lv_date.
      wa_objversion-time = lv_time.
      APPEND wa_objversion TO cht_objversions.
      IF lv_date > lv_latestdate OR ( lv_date = lv_latestdate AND lv_time > lv_latesttime ).
        lv_latestdate = lv_date.
        lv_latesttime = lv_time.
      ENDIF.
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
            iv_trid = iv_trid
            iv_date = iv_date
            iv_time = iv_time
        IMPORTING
            ev_versno = lv_versno
            ev_verscnt = lv_verscnt
            ev_date = lv_date
            ev_time = lv_time
        CHANGING
            cht_vers = lt_vers
             ).
    IF lv_success = abap_true AND cht_objversions IS SUPPLIED.
      wa_objversion-objversionno = lv_versno.
      wa_objversion-objname = iv_objname.
      wa_objversion-objtype = 'CPRT'.
      wa_objversion-date = lv_date.
      wa_objversion-time = lv_time.
      APPEND wa_objversion TO cht_objversions.
      IF lv_date > lv_latestdate OR ( lv_date = lv_latestdate AND lv_time > lv_latesttime ).
        lv_latestdate = lv_date.
        lv_latesttime = lv_time.
      ENDIF.
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
            iv_trid = iv_trid
            iv_date = iv_date
            iv_time = iv_time
        IMPORTING
            ev_versno = lv_versno
            ev_verscnt = lv_verscnt
            ev_date = lv_date
            ev_time = lv_time
        CHANGING
            cht_vers = lt_vers
             ).
    IF lv_success = abap_true AND cht_objversions IS SUPPLIED.
      wa_objversion-objversionno = lv_versno.
      wa_objversion-objname = iv_objname.
      wa_objversion-objtype = 'CPRI'.
      wa_objversion-date = lv_date.
      wa_objversion-time = lv_time.
      APPEND wa_objversion TO cht_objversions.
      IF lv_date > lv_latestdate OR ( lv_date = lv_latestdate AND lv_time > lv_latesttime ).
        lv_latestdate = lv_date.
        lv_latesttime = lv_time.
      ENDIF.
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
              iv_trid = iv_trid
              iv_date = iv_date
              iv_time = iv_time
          IMPORTING
              ev_versno = lv_versno
              ev_verscnt = lv_verscnt
              ev_date = lv_date
              ev_time = lv_time
          CHANGING
              cht_vers = lt_vers
               ).
      IF lv_success = abap_true AND cht_objversions IS SUPPLIED.
        wa_objversion-objversionno = lv_versno.
        wa_objversion-objname = lv_methodname.
        wa_objversion-objtype = 'METH'.
        wa_objversion-date = lv_date.
        wa_objversion-time = lv_time.
        APPEND wa_objversion TO cht_objversions.
        IF lv_date > lv_latestdate OR ( lv_date = lv_latestdate AND lv_time > lv_latesttime ).
          lv_latestdate = lv_date.
          lv_latesttime = lv_time.
        ENDIF.
      ENDIF.
      IF lv_verscnt > r_version_no.
        r_version_no = lv_verscnt.
      ENDIF.
    ENDLOOP.

    IF iv_findtest = abap_true.

      " test class of the class if any
      lv_clskey = lv_classname.
      lv_limu = 'CINC'.
      lv_inctype = 'AU'.
      CALL FUNCTION 'SEO_CLASS_GET_INCLUDE_BY_NAME'
        EXPORTING
          clskey   = lv_clskey
          limu     = lv_limu
          inctype  = lv_inctype
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
              iv_trid = iv_trid
              iv_date = iv_date
              iv_time = iv_time
          IMPORTING
              ev_versno = lv_versno
              ev_verscnt = lv_verscnt
              ev_date = lv_date
              ev_time = lv_time
          CHANGING
              cht_vers = lt_vers
               ).
      IF lv_success = abap_true AND cht_objversions IS SUPPLIED.
        wa_objversion-objversionno = lv_versno.
        wa_objversion-objname = lv_objname.
        wa_objversion-objtype = 'CINC'.
        wa_objversion-date = lv_date.
        wa_objversion-time = lv_time.
        APPEND wa_objversion TO cht_objversions.
        IF lv_date > lv_latestdate OR ( lv_date = lv_latestdate AND lv_time > lv_latesttime ).
          lv_latestdate = lv_date.
          lv_latesttime = lv_time.
        ENDIF.
      ENDIF.
      IF lv_verscnt > r_version_no.
        r_version_no = lv_verscnt.
      ENDIF.

    ENDIF.

    ev_date = lv_latestdate.
    ev_time = lv_latesttime.

  ENDMETHOD.


  METHOD get_delta.

    CONSTANTS lv_ignore_case_differences TYPE c VALUE abap_true.
    CONSTANTS lv_comp_mode TYPE i VALUE 1.
    DATA: lv_no_release_transformation TYPE svrs_bool.
    DATA: abaptext_new TYPE TABLE OF abaptxt255 INITIAL SIZE 0.
    DATA: abaptext_old TYPE TABLE OF abaptxt255 INITIAL SIZE 0.
    DATA: trdir_new TYPE TABLE OF trdir INITIAL SIZE 1.
    DATA: trdir_old TYPE TABLE OF trdir INITIAL SIZE 1.
    DATA: abaptext_delta TYPE TABLE OF vxabapt255 INITIAL SIZE 0.
    DATA: trdir_delta TYPE TABLE OF xtrdir INITIAL SIZE 1.
    DATA: smodilog_new TYPE TABLE OF smodilog,
          smodilog_old TYPE TABLE OF smodilog.
    DATA: lv_addlines    TYPE i VALUE 0,
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


  METHOD get_initial.

    DATA lv_no_release_transformation TYPE svrs_bool.
    DATA trdir_new TYPE TABLE OF trdir INITIAL SIZE 1.
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


  METHOD get_class_methods.

    DATA: lcl_obj      TYPE REF TO cl_abap_objectdescr,
          lp_descr_ref TYPE REF TO cl_abap_typedescr.
    TRY.
        cl_abap_objectdescr=>describe_by_name(
            EXPORTING
                p_name = iv_classname
            RECEIVING
                p_descr_ref = lp_descr_ref
            EXCEPTIONS
                type_not_found = 1
                ).
        IF sy-subrc = 0.
          lcl_obj ?= lp_descr_ref.
          cht_methods = lcl_obj->methods.
        ENDIF.
      CATCH cx_sy_rtti_syntax_error.
        me->write_telemetry( iv_message = |GET_CLASS_METHODS fails in fetching class methods for class { iv_classname }| ).
    ENDTRY.

  ENDMETHOD.


  METHOD get_versions.

    DATA: lv_rfcdest   TYPE rfcdest,
          lt_versno    TYPE STANDARD TABLE OF vrsn,
          lt_text_vers TYPE TABLE OF e07t,
          lt_vers_obj  TYPE TABLE OF vrsd,
          lt_text_obj  TYPE TABLE OF e07t,
          lv_objname   TYPE versobjnam,
          lv_objtype   TYPE versobjtyp.

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


  METHOD get_column_values.

    DATA wafield TYPE ty_data_table_field.
    DATA lv_lineoffset TYPE i.
    DATA lv_field(33) TYPE c.
    DATA lv_value TYPE string.
    DATA lv_leng TYPE i.
    DATA lv_strlen TYPE i.
    DATA lt_conditions TYPE TABLE OF string.
    DATA lv_criteria TYPE string.

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
                iv_dec = wafield-decimals
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
      IF wafield-fieldname NA '.&'.
        APPEND |{ wafield-fieldname } = '{ lv_value }'| TO lt_conditions.
      ENDIF.
    ENDLOOP.

    lv_criteria = concat_lines_of( table = lt_conditions sep = ` AND ` ).

    me->get_nonkey_column_values(
        EXPORTING
            iv_data_table_desc = iv_data_table_desc
            iv_criteria = lv_criteria
        IMPORTING
            abaptext = cht_filecontent
             ).

  ENDMETHOD.


  METHOD get_nonkey_column_values.

    DATA lt_entry_tab TYPE TABLE OF line.
    DATA wafield TYPE ty_data_table_field.
    DATA lv_field(33) TYPE c.
    DATA lv_strlen TYPE i.
    DATA lv_fieldname TYPE string.
    FIELD-SYMBOLS:
      <fs_table>  TYPE ANY TABLE,
      <fs_struct> TYPE any,
      <fs_field>  TYPE any.
    DATA: lo_typedesc    TYPE REF TO cl_abap_typedescr,
          lo_tabtype     TYPE REF TO cl_abap_tabledescr,
          lo_struct_type TYPE REF TO cl_abap_structdescr,
          lr_data        TYPE REF TO data,
          lt_comp_tab    TYPE cl_abap_structdescr=>component_table.
    DATA lv_value TYPE string.

    " prepare for dynamic SQL query for all rows
    CALL METHOD cl_abap_typedescr=>describe_by_name
      EXPORTING
        p_name         = iv_data_table_desc-dd02v-tabname
      RECEIVING
        p_descr_ref    = lo_typedesc
      EXCEPTIONS
        type_not_found = 1.
    IF sy-subrc <> 0.
      me->write_telemetry( iv_message = |GET_CONFIG_TABLE_LINES fails to find type info of { iv_data_table_desc-dd02v-tabname }| ).
      RETURN.
    ENDIF.

    lo_struct_type ?= lo_typedesc.
    lt_comp_tab = lo_struct_type->get_components( ).
    lo_struct_type = cl_abap_structdescr=>create( lt_comp_tab ).
    lo_tabtype = cl_abap_tabledescr=>create( lo_struct_type ).
    CREATE DATA lr_data TYPE HANDLE lo_tabtype.
    ASSIGN lr_data->* TO <fs_table>.

    TRY.
        " dynamic query for given table name
        SELECT * FROM (iv_data_table_desc-dd02v-tabname) INTO CORRESPONDING FIELDS OF TABLE <fs_table> UP TO 1 ROWS WHERE (iv_criteria).

        LOOP AT <fs_table> ASSIGNING FIELD-SYMBOL(<fs_row2>).
          LOOP AT iv_data_table_desc-dd03v INTO wafield WHERE keyflag = abap_false.
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
        ENDLOOP.
      CATCH cx_root.
    ENDTRY.

  ENDMETHOD.

  METHOD get_parentpackages.

    DATA lv_parentcl TYPE parentcl.
    DATA lv_childcl TYPE devclass.
    lv_childcl = iv_package.
    WHILE lv_childcl IS NOT INITIAL.
      SELECT SINGLE parentcl FROM tdevc INTO lv_parentcl WHERE devclass = lv_childcl.
      APPEND lv_childcl TO et_packages.
      lv_childcl = lv_parentcl.
    ENDWHILE.

  ENDMETHOD.


  METHOD get_subpackages.

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
      DELETE lt_queue FROM 1 TO 1.
    ENDWHILE.

  ENDMETHOD.


  METHOD get_objtype_hist.
    SELECT tt~pgmid, tt~object, tr~trkorr
        FROM e070 AS ts INNER JOIN e070 AS tr ON ts~strkorr = tr~trkorr
        INNER JOIN e071 AS tt ON tt~trkorr = ts~trkorr
        INTO TABLE @et_hist
        WHERE ( tr~trfunction = 'K' OR tr~trfunction = 'W' )
            AND tr~trstatus = 'R' AND tr~as4date > @iv_fromdate
            AND NOT ( tt~object = 'RELE' AND tt~pgmid = 'CORR' ).
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