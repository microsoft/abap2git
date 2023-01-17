" Utility class to execute ABAP Unit Test (AUT) suite and collect code coverage
" OOB program RS_AUCV_RUNNER either reports unit test result and code coverage in IDE
" or sends email with unit test result only, SCI rule ABAP Unit runs unit tests only
" without code coverage reported
" This class works together with abap2git to fetch allowed list in Git repo file defining
" classes with real harmless test class testing system-to-test without read/write access to SAP
" data thus stable enough to repetitively run agnostic to SAP data updates by other engineers
" and configuration changes. It produces result structure with unit test results and
" statement/procedure/branch code coverage like OOB offerings so that one could further
" report with customized email format, dump to storage or display UI with customized outlook
CLASS ZCL_UTILITY_AUCV_RUNNER DEFINITION
  PUBLIC
  FINAL
  CREATE PUBLIC .

  PUBLIC section.

    " test and code coverage result of a unit test run
    TYPES: BEGIN OF ts_method_result,
           name     TYPE string,
           pass     TYPE abap_bool,
           text     TYPE string,
           END OF ts_method_result.
    TYPES: tty_method_result TYPE TABLE OF ts_method_result WITH KEY name.
    TYPES: BEGIN OF ts_test_result,
           name     TYPE string,
           sut      TYPE string,
           total    TYPE i,
           pass     TYPE i,
           fail     TYPE i,
           methods  TYPE tty_method_result,
           END OF ts_test_result.
    TYPES: tty_test_result TYPE TABLE OF ts_test_result WITH KEY name.
    TYPES: BEGIN OF ts_statement,
           start_line   TYPE i,
           start_column TYPE i,
           end_line     TYPE i,
           end_column   TYPE i,
           hit          TYPE abap_bool,
           END OF ts_statement.
    TYPES: tty_statement TYPE TABLE OF ts_statement WITH KEY start_line start_column.
    TYPES: BEGIN OF ts_cov_method_result,
           name         TYPE string,
           total        TYPE i,
           hit          TYPE i,
           miss         TYPE i,
           percent      TYPE DECFLOAT16,
           statements   TYPE tty_statement,
           END OF ts_cov_method_result.
    TYPES: tty_cov_method_result TYPE TABLE OF ts_cov_method_result WITH KEY name.
    TYPES: BEGIN OF ts_cov_result,
           name         TYPE string,
           total        TYPE i,
           hit          TYPE i,
           miss         TYPE i,
           percent      TYPE DECFLOAT16,
           methods      TYPE tty_cov_method_result,
           END OF ts_cov_result.
    TYPES: tty_cov_result TYPE TABLE OF ts_cov_result WITH KEY name.
    TYPES: BEGIN OF ts_aucv_result,
           tests    TYPE tty_test_result,
           brancovs TYPE tty_cov_result,
           proccovs TYPE tty_cov_result,
           stmtcovs TYPE tty_cov_result,
           startdat TYPE string,
           starttim TYPE string,
           stopdat  TYPE string,
           stoptim  TYPE string,
           END OF ts_aucv_result.

    " allow class list as system-to-test
    TYPES: tty_allowed_classes TYPE STANDARD TABLE OF string WITH DEFAULT KEY.

    " run specific classes' unit tests specified by Git repo file
    " iv_branch - Git branch following abap2git protocol
    " iv_username - VSO user name as email address
    " iv_pat - VSO personal access token, could be generated from VSO portal per user with code change permission
    " iv_orgid - organization ID, like the name "org" in <org>.visualstudio.com
    " iv_repoid - Git repo ID
    " iv_project - project name in your VSO portal
    " ev_result - unit test and code coverage result of the test run
    CLASS-METHODS run_with_listfile
        IMPORTING
            iv_branch       TYPE string
            iv_username     TYPE string
            iv_pat          TYPE string
            iv_orgid        TYPE string
            iv_repoid       TYPE string
            iv_project      TYPE string
        EXPORTING
            ev_result       TYPE ts_aucv_result.

    " run specific classes' unit tests specified by internal table
    " it_allowlist - allowed class names to run unit tests
    " ev_result - unit test and code coverage result of the test run
    CLASS-METHODS run_with_list
        IMPORTING
            it_allowlist    TYPE tty_allowed_classes
        EXPORTING
            ev_result       TYPE ts_aucv_result.

  PROTECTED section.

  PRIVATE section.

    " Git repo file for AUT run settings
    CONSTANTS co_runsettings TYPE string VALUE '/nonSap/tools/aut.runsettings.json'.

    " AUT run settings JSON format
    TYPES: BEGIN OF ty_runsettings,
           allowed_classes TYPE tty_allowed_classes,
           END OF ty_runsettings.

    " fetch run settings file content from ADO
    CLASS-METHODS get_runsettings
        IMPORTING
            iv_branch       TYPE string
            iv_username     TYPE string
            iv_pat          TYPE string
            iv_orgid        TYPE string
            iv_repoid       TYPE string
            iv_project      TYPE string
        EXPORTING
            ev_runsettings  TYPE ty_runsettings
        RETURNING VALUE(rv_success) TYPE string.

    " fetch method code coverage metrics
    CLASS-METHODS get_methods_coverage
        IMPORTING
            ioref_Coverage_Result   TYPE REF TO if_Scv_Result
        EXPORTING
            et_methods              TYPE tty_cov_method_result.

ENDCLASS.

CLASS ZCL_UTILITY_AUCV_RUNNER IMPLEMENTATION.

  METHOD RUN_WITH_LISTFILE.

    DATA ls_runsettings TYPE ty_runsettings.
    DATA lv_result TYPE string.

    " fetch unit test run settings from Git branch
    lv_result = GET_RUNSETTINGS(
        EXPORTING
            iv_branch = iv_branch
            iv_username = iv_username
            iv_pat = iv_pat
            iv_orgid = iv_orgid
            iv_repoid = iv_repoid
            iv_project = iv_project
        IMPORTING
            ev_runsettings = ls_runsettings
             ).
    CHECK lv_result = abap_true.

    run_with_list(
        EXPORTING
            it_allowlist = ls_runsettings-allowed_classes
        IMPORTING
            ev_result = ev_result
             ).

  ENDMETHOD.

  METHOD RUN_WITH_LIST.

    DATA lt_classes TYPE RANGE OF seoaliases-clsname.
    DATA oref_listener TYPE REF TO ZCL_UTILITY_AUTLISTENER.
    DATA lt_programs TYPE ZCL_UTILITY_AUTLISTENER=>ty_programs.
    DATA lt_syntax_errors TYPE ZCL_UTILITY_AUTLISTENER=>ty_syntax_errors.
    DATA oref_au_factory TYPE REF TO cl_aunit_factory.
    DATA oref_task TYPE REF TO cl_Aucv_Task.
    DATA lv_converted_Key TYPE cl_Aucv_Task=>ty_Object_Directory_Element.
    DATA lt_converted_Keys TYPE cl_Aucv_Task=>ty_Object_Directory_Elements.
    DATA oref_Coverage_Measurement TYPE REF TO if_Scv_Measurement.
    DATA oref_Coverage_Result TYPE REF TO if_Scv_Result.
    DATA lv_class TYPE string.
    DATA lv_cov_name TYPE string.
    DATA lt_coverages TYPE IF_SCV_COVERAGE=>TAB.
    DATA oref_coverage TYPE REF TO if_scv_coverage.
    DATA lv_cov_id TYPE string.
    DATA lv_coverage TYPE ts_cov_result.
    DATA lt_stats TYPE ZCL_UTILITY_AUTLISTENER=>ty_statistics.
    DATA lv_testclass TYPE ts_test_result.
    DATA lv_method TYPE ts_method_result.

    FIELD-SYMBOLS:
        <fs_program> TYPE ZCL_UTILITY_AUTLISTENER=>ty_program,
        <fs_test_class> TYPE ZCL_UTILITY_AUTLISTENER=>ty_test_class,
        <fs_test_method> TYPE ZCL_UTILITY_AUTLISTENER=>ty_test_method.

    " prepare for listener program list
    LOOP AT it_allowlist INTO lv_class.
        APPEND VALUE #( sign = 'I' option = 'EQ' low = lv_class ) TO lt_classes.
    ENDLOOP.

    " query class objects from allow list
    SELECT                                              ##TOO_MANY_ITAB_FIELDS
        object as obj_type                             "#EC CI_GENBUFF
        obj_name
        devclass as package
    FROM tadir
    APPENDING CORRESPONDING FIELDS OF TABLE lt_programs
    WHERE
        pgmid = 'R3TR' AND
        object = 'CLAS' AND
        obj_name in lt_classes.

    " look up test class for each system-to-test class
    LOOP AT lt_programs ASSIGNING <fs_program>.
        <fs_program>-name = cl_aunit_prog_info=>tadir_to_progname(
            obj_type = <fs_program>-obj_type
            obj_name = <fs_program>-obj_name
             ).
    ENDLOOP.

    " create unit test result listener
    CREATE OBJECT oref_au_factory.
    oref_listener = ZCL_UTILITY_AUTLISTENER=>create_listener(
      i_programs = lt_programs
      i_syntax_errors = lt_syntax_errors
      i_au_factory = oref_au_factory
       ).

    " prepare for unit test run task
    oref_task = cl_Aucv_Task=>create(
        i_Listener = oref_listener
        i_Measure_Coverage = abap_true
        i_Max_Risk_Level = 11
        i_Max_Duration_Category = 12
         ).

    LOOP AT it_allowlist INTO lv_class.
      lv_converted_key-object = 'CLAS'.
      lv_converted_key-obj_Name = lv_class.
      APPEND lv_converted_key TO lt_converted_Keys.
    ENDLOOP.

    oref_task->add_Associated_unit_Tests( lt_converted_Keys ).

    " execute unit tests
    oref_task->run( if_Aunit_Task=>c_Run_Mode-catch_Short_Dump ).

    " collect code coverage result
    TRY.

        oref_Coverage_Measurement = oref_task->get_coverage_measurement( ).

        LOOP AT lt_programs ASSIGNING <fs_program>.
            oref_Coverage_Result = oref_Coverage_Measurement->build_program_result( <fs_program>-name ).
            lv_cov_name = <fs_program>-obj_name.
            lt_coverages = oref_Coverage_Result->get_coverages( ).
            LOOP AT lt_coverages INTO oref_coverage.
                CLEAR lv_coverage.
                lv_coverage-name = lv_cov_name.
                lv_coverage-total = oref_coverage->get_total( ).
                lv_coverage-hit = oref_coverage->get_executed( ).
                lv_coverage-miss = oref_coverage->get_not_executed( ).
                lv_coverage-percent = oref_coverage->get_percentage( ).
                lv_cov_id = oref_coverage->type->id.
                IF lv_cov_id = 'BRAN'.
                    APPEND lv_coverage to ev_result-brancovs.
                ELSEIF lv_cov_id = 'PROC'.
                    APPEND lv_coverage to ev_result-proccovs.
                ELSE.
                    get_methods_coverage(
                        EXPORTING
                            ioref_Coverage_Result = oref_Coverage_Result
                        IMPORTING
                            et_methods = lv_coverage-methods
                         ).
                    APPEND lv_coverage to ev_result-stmtcovs.
                ENDIF.
                CLEAR oref_coverage.
            ENDLOOP.
            CLEAR lt_coverages.
            CLEAR oref_Coverage_Result.
        ENDLOOP.

    CATCH cx_scv_execution_error INTO DATA(lx_error).
        EXIT.
    ENDTRY.

    " collect test result
    oref_listener->get_result(
        IMPORTING
            e_programs = lt_programs
            e_statistic = lt_stats
             ).
    LOOP AT lt_programs ASSIGNING <fs_program>.
        LOOP AT <fs_program>-test_classes ASSIGNING <fs_test_class>.
            CLEAR lv_testclass.
            lv_testclass-name = <fs_test_class>-name.
            lv_testclass-sut = <fs_program>-obj_name.
            LOOP AT <fs_test_class>-test_methods ASSIGNING <fs_test_method>.
                CLEAR lv_method.
                lv_method-name = <fs_test_method>-name.
                IF <fs_test_method>-alert IS INITIAL.
                    lv_method-pass = abap_true.
                    ADD 1 TO lv_testclass-pass.
                ELSE.
                    lv_method-pass = abap_false.
                    lv_method-text = <fs_test_method>-alert-description.
                    ADD 1 TO lv_testclass-fail.
                ENDIF.
                ADD 1 TO lv_testclass-total.
                APPEND lv_method TO lv_testclass-methods.
            ENDLOOP.
            APPEND lv_testclass TO ev_result-tests.
        ENDLOOP.
    ENDLOOP.

  ENDMETHOD.

  METHOD GET_RUNSETTINGS.
    DATA oref_ado TYPE REF TO ZCL_UTILITY_ABAPTOGIT_ADO.
    DATA lv_runsettings TYPE string.
    CREATE OBJECT oref_ado
        EXPORTING
            iv_username = iv_username
            iv_pat = iv_pat
            iv_orgid = iv_orgid
            iv_repoid = iv_repoid
            iv_project = iv_project.
    rv_success = oref_ado->get_item_ado(
        EXPORTING
            iv_branch = iv_branch
            iv_itempath = co_runsettings
        IMPORTING
            ev_content = lv_runsettings
             ).
    CHECK rv_success = abap_true.
    /ui2/cl_json=>deserialize(
        EXPORTING
            json = lv_runsettings
        CHANGING
            data = ev_runsettings
             ).
  ENDMETHOD.

  METHOD GET_METHODS_COVERAGE.
    DATA ls_method TYPE ts_cov_method_result.
    DATA ls_statement TYPE ts_statement.
    DATA(oref_root_node) = ioref_Coverage_Result->get_root_node( ).
    CHECK oref_root_node->has_children( ) = abap_true.
    DATA(lt_children) = oref_root_node->get_children( ).
    LOOP AT lt_children INTO DATA(wachild).
        CHECK wachild->has_children( ) = abap_true.
        DATA(lt_methods) = wachild->get_children( ).
        LOOP AT lt_methods INTO DATA(wamethod).
            CLEAR ls_method.
            LOOP AT wamethod->get_coverages( ) INTO DATA(wacov).
                DATA(lv_cov_id) = wacov->type->id.
                CHECK lv_cov_id = 'STAT'.
                ls_method-name = wamethod->name.
                ls_method-total = wacov->get_total( ).
                ls_method-hit = wacov->get_executed( ).
                ls_method-miss = wacov->get_not_executed( ).
                ls_method-percent = wacov->get_percentage( ).
            ENDLOOP.
            APPEND ls_method TO et_methods.
        ENDLOOP.
    ENDLOOP.
  ENDMETHOD.

ENDCLASS.
