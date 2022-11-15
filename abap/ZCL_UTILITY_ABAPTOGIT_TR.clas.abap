" Helper class to talk to SAP transport request and ABAP objects operations
CLASS ZCL_UTILITY_ABAPTOGIT_TR DEFINITION
  PUBLIC
  FINAL
  CREATE PUBLIC .

PUBLIC SECTION.

    " latest and active version mode
    CONSTANTS: c_latest_version     TYPE string VALUE 'latest',
               c_active_version     TYPE string VALUE 'active'.

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
            delflag     TYPE string,
            verno       TYPE i,
            filecontent TYPE string,
           END OF ts_commit_object.
    TYPES: tty_commit_object TYPE TABLE OF ts_commit_object.

    " source lines of an ABAP object
    TYPES: tty_abaptext TYPE TABLE OF ABAPTXT255 INITIAL SIZE 0.

    " constructor
    " io_objtelemetry - class object for telemetry
    " iv_methtelemetry - method name for telemetry
    " for telemetry, the method will be invoked with parameters iv_message as string (for message content) and iv_kind as string (for category)
    METHODS constructor
        IMPORTING
            io_objtelemetry     TYPE REF TO object OPTIONAL
            iv_methtelemetry    TYPE string OPTIONAL.

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
            iv_findtest     LIKE abap_true
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

PROTECTED SECTION.

PRIVATE SECTION.

    " telemetry callback
    DATA oref_telemetry TYPE REF TO object.
    DATA method_name_telemetry TYPE string.

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

    " get class object version (for public/protected/private sections in definition and method implementations)
    METHODS get_class_versions_no
        IMPORTING
            iv_objname      TYPE e071-obj_name
            iv_objtype      TYPE e071-object
            iv_mode         TYPE string
            iv_date         TYPE d OPTIONAL
            iv_time         TYPE t OPTIONAL
            iv_findtest     LIKE abap_true DEFAULT abap_true
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

  METHOD GET_TR_COMMIT_OBJECTS.

    DATA ld_cs_request TYPE TRWBO_REQUEST.
    DATA lv_trkorr TYPE TRKORR.
    FIELD-SYMBOLS <fs_cs_request_object> LIKE LINE OF ld_cs_request-objects.
    DATA lt_objversions TYPE tty_version_no.
    DATA lt_filecontent TYPE tty_abaptext.
    DATA lt_tclsfilecontent TYPE tty_abaptext.
    DATA lt_packagenames TYPE TABLE OF string.
    DATA lv_packagename TYPE string.
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
    DATA lv_haspackage LIKE abap_true.
    DATA lt_objname_parts TYPE TABLE OF string.
    DATA lt_classes TYPE TABLE OF string.
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
    APPEND |[TR # { iv_trid }] [OWNER: { ld_cs_request-h-as4user }] DESCRIPTION: { ld_cs_request-h-as4text }| TO lt_commentlines.

    " fetch tasks in a TR
    SELECT obj_name FROM e071 INTO TABLE @lt_tasks WHERE trkorr = @lv_trkorr AND object = 'RELE' AND pgmid = 'CORR'.

    " append all tasks' description to the commit description
    LOOP AT lt_tasks INTO lv_task.
        CLEAR lt_taskfields.
        SPLIT lv_task AT ' ' INTO TABLE lt_taskfields.
        lv_taskid = lt_taskfields[ 1 ].
        SELECT SINGLE as4text FROM e07t INTO @lv_taskdesc WHERE trkorr = @lv_taskid.
        APPEND |{ lv_task } { lv_taskdesc }| TO lt_tasktexts.
    ENDLOOP.

    APPEND LINES OF lt_tasktexts TO lt_commentlines.

    " commit description now contains TR description plus all tasks' description each line
    ev_comment = concat_lines_of( table = lt_commentlines sep = CL_ABAP_CHAR_UTILITIES=>CR_LF ).

    CLEAR: lt_taskids, lt_taskfields, lt_tasktexts.

    SPLIT iv_packagenames AT ',' INTO TABLE lt_packagenames.

    " process objects in the TR
    LOOP AT ld_cs_request-objects ASSIGNING <fs_cs_request_object> WHERE object <> 'RELE'.

        DATA(lv_objname) = <fs_cs_request_object>-obj_name.
        DATA(lv_objtype) = <fs_cs_request_object>-object.

        CLEAR: lv_objtype2, lv_devclass.

        IF lv_objtype = 'CINC'
            OR lv_objtype = 'CLSD'
            OR lv_objtype = 'CPUB'
            OR lv_objtype = 'CPRT'
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
                        clskey = lv_classkey.
                lv_classname = lv_classkey.
            ELSE.
                IF lv_objtype = 'METH'.
                    " class name <spaces> method name pattern
                    CLEAR lt_objname_parts.
                    SPLIT lv_objname AT ' ' INTO TABLE lt_objname_parts.
                    lv_classname = lt_objname_parts[ 1 ].
                ELSE.
                    " for CLSD/CPUB/CPRT/CPRI case class name is provided
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
                WHERE object = 'CLAS' AND obj_name = lv_classname.

        ELSEIF lv_objtype = 'FUNC'.

            " function module case, find out function group and then the package function group belongs to
            lv_funcname = lv_objname.
            me->get_fugr( EXPORTING iv_objname = lv_funcname IMPORTING ev_fugrname = lv_fugr ).
            SELECT SINGLE devclass FROM tadir INTO lv_devclass
                WHERE obj_name = lv_fugr AND object = 'FUGR'.

        ELSEIF lv_objtype = 'REPS'.

            " function include case, extract function group name and then the package function group belongs to
            " function include name as L + function group name + 3 characters
            lv_fugr = substring( val = lv_objname off = 1 len = strlen( lv_objname ) - 1 - 3 ).
            SELECT SINGLE devclass FROM tadir INTO lv_devclass
                WHERE obj_name = lv_fugr AND object = 'FUGR'.

        ELSEIF lv_objtype = 'PROG' OR lv_objtype = 'INTF'.

            " program (include), interface case

            " find out which package the ABAP object belongs to
            SELECT SINGLE devclass INTO lv_devclass FROM tadir
                WHERE object = lv_objtype AND obj_name = lv_objname.

        ELSE.

            me->write_telemetry( iv_message = |unsupported type { lv_objtype } for object { lv_objname }| iv_kind = 'info' ).

        ENDIF.

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

        " is the object in one of the packages specified?
        CHECK line_exists( lt_packagenames[ table_line = lv_devclass ] ).

        " is there any version found?
        CHECK lv_version_no > 0.

        " fetch function group name in case of function module
        IF lv_objtype = 'FUGR'.
            lv_funcname = lv_objname.
            lv_objtype2 = 'FUNC'.
            me->get_fugr( EXPORTING iv_objname = lv_funcname IMPORTING ev_fugrname = lv_fugr ).
        ENDIF.

        " construct object content (and test class content won't be provided given not required to in fetching version above)
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

        IF lv_objtype = 'CINC'.
            " following abapGit where class name is used for test class name instead of ====CCAU like one
            lv_objname = lv_classname.
        ENDIF.

        TRANSLATE lv_objname TO UPPER CASE.

        APPEND VALUE ts_commit_object(
            devclass = lv_devclass
            objname = lv_objname
            objtype = lv_objtype
            objtype2 = lv_objtype2
            fugr = lv_fugr
            delflag = abap_false
            verno = lv_version_no
            filecontent = lv_filecontent
            ) TO it_commit_objects.

    ENDLOOP.

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