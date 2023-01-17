" AUT test runner listener to collect unit test result
" working together with class ZCL_UTILITY_AUCV_RUNNER
" Mostly from RS_AUCV_RUNNER excluding skipping, timeout, time statistics and alert level logics
CLASS ZCL_UTILITY_AUTLISTENER DEFINITION
  PUBLIC
  FINAL
  CREATE PUBLIC.

  PUBLIC SECTION.

    TYPES: BEGIN OF ty_context,
        package          TYPE string,
        program          TYPE string,
        obj_type         TYPE string,
        obj_name         TYPE string,
        test_class       TYPE string,
        test_method      TYPE string,
        adt_resource_uri TYPE string,
      END OF ty_context.

    TYPES: ty_alert_level TYPE c LENGTH 6.

    TYPES: BEGIN OF ty_alert,
        context     TYPE ty_context,
        kind        TYPE string,
        description TYPE string,
        level       TYPE ty_alert_level,
        apply_zebra TYPE abap_bool,
      END OF ty_alert.
    TYPES: ty_alerts TYPE STANDARD TABLE OF ty_alert WITH DEFAULT KEY.

    TYPES: BEGIN OF ty_test_method,
        name  TYPE string,
        alert TYPE ty_alert,
        BEGIN OF state,
          has_been_started TYPE abap_bool,
          has_been_skipped TYPE abap_bool,
        END OF state,
      END OF ty_test_method.
    TYPES: ty_methods TYPE STANDARD TABLE OF ty_test_method WITH NON-UNIQUE KEY name.

    TYPES: BEGIN OF ty_test_class,
        name         TYPE string,
        handle       TYPE REF TO if_aunit_test_class_handle,
        test_methods TYPE ty_methods,
        BEGIN OF state,
          has_been_started           TYPE abap_bool,
          BEGIN OF issue,
            has_been_skipped TYPE abap_bool,
            has_rt_failure   TYPE abap_bool,
            has_timeout      TYPE abap_bool,
            has_failure      TYPE abap_bool,
          END OF issue,
          count_exec_methods         TYPE i,
          count_skipped_methods      TYPE i,
          count_skipped_over_methods TYPE i,
          count_no_permission        TYPE i,
        END OF state,
      END OF ty_test_class.
    TYPES: ty_test_classes TYPE STANDARD TABLE OF ty_test_class WITH NON-UNIQUE KEY name.

    TYPES: BEGIN OF ty_program,
        name             TYPE progname,
        obj_type         TYPE tadir-object,
        obj_name         TYPE tadir-obj_name,
        package          TYPE tadir-devclass,
        adt_resource_uri TYPE string,
        test_classes     TYPE ty_test_classes,
        is_permitted     TYPE abap_Bool,
        BEGIN OF state,
          has_been_started TYPE abap_bool,
          has_issue        TYPE abap_bool,
        END OF state,
      END OF ty_program.

    TYPES: ty_programs TYPE STANDARD TABLE OF ty_program WITH NON-UNIQUE KEY obj_name obj_type
        WITH unique sorted KEY sorted components obj_name obj_type.

    TYPES: BEGIN OF ty_syntax_error,
        obj_type         TYPE tadir-object,
        obj_name         TYPE tadir-obj_name,
        adt_resource_uri TYPE string,
        message          TYPE string,
        line             TYPE i,
        token            TYPE string,
      END OF ty_syntax_error.
    TYPES: ty_syntax_errors TYPE STANDARD TABLE OF ty_syntax_error WITH KEY obj_name obj_type.

    TYPES: BEGIN OF ty_stat_package,
        name     TYPE string,
        new_line TYPE abap_bool,
        BEGIN OF state,
          executed_classes TYPE i,
          executed_methods TYPE i,
          skipped_methods  TYPE i,
        END OF state,
      END OF ty_stat_package.

    TYPES: ty_stat_packages TYPE STANDARD TABLE OF ty_stat_package WITH NON-UNIQUE KEY name.

    TYPES: BEGIN OF ty_statistics,
        cnt_packages      TYPE i,
        cnt_programs      TYPE i,
        cnt_test_classes  TYPE i,
        cnt_test_methods  TYPE i,

        BEGIN OF cnt_method,
          passed         TYPE i,
          with_fatal     TYPE i,
          with_critical  TYPE i,
          with_tolerable TYPE i,
          skipped        TYPE i,
        END OF cnt_method,

        " message
        has_timeout_only TYPE abap_bool,
        BEGIN OF cnt_failure,
          syntax_error  TYPE i,
          fatal         TYPE i,
          critical      TYPE i,
          tolerable     TYPE i,
          total         TYPE i,
        END OF cnt_failure,

        packages          TYPE ty_stat_packages,

      END OF ty_statistics.

    INTERFACES: if_aunit_listener.

    CLASS-METHODS create_listener
        IMPORTING   i_programs      TYPE ty_programs
                    i_syntax_errors TYPE ty_syntax_errors OPTIONAL
                    i_au_factory    TYPE REF TO cl_aunit_factory
        RETURNING VALUE(result)     TYPE REF TO ZCL_UTILITY_AUTLISTENER.

    METHODS get_result
        EXPORTING   e_programs  TYPE ty_programs
                    e_statistic TYPE ty_statistics.

  PRIVATE SECTION.

    METHODS:
      init,

      handle_failure
        IMPORTING   i_kind        TYPE string
                    i_ref_failure TYPE REF TO if_aunit_info_failure,

      initialize_program_entry
        IMPORTING   i_program_name TYPE csequence,

      finish_statistics,

      add_devc_uri_syntax_errors,

      extract_test_class.

    CONSTANTS:
      BEGIN OF c_on_miss,
        ignore TYPE c LENGTH 6 VALUE 'ignore' ##no_Text,
        assert TYPE c LENGTH 6 VALUE 'assert' ##no_Text,
        create TYPE c LENGTH 6 VALUE 'create' ##no_Text,
      END OF c_on_miss.

    CONSTANTS:
      BEGIN OF c_kind,
        assert_failure TYPE string VALUE 'Assertion Failure' ##no_Text,
        warning        TYPE string VALUE 'Warning' ##no_Text,
        cx_failure     TYPE string VALUE 'Exception' ##no_Text,
        rt_failure     TYPE string VALUE 'Runtime Abortion' ##no_Text,
        skipped        TYPE string VALUE 'Skipped' ##no_Text,
        timeout        TYPE string VALUE 'Timeout' ##no_Text,
      END OF c_kind.

    CONSTANTS:
      BEGIN OF c_level,
        skipped   TYPE ty_alert_level VALUE '1-SKIP' ##no_Text,
        tolerable TYPE ty_alert_level VALUE '2-TOLE' ##no_Text,
        critical  TYPE ty_alert_level VALUE '3-CRIT' ##no_Text,
        fatal     TYPE ty_alert_level VALUE '4-FATA' ##no_Text,
      END OF c_level.

    DATA:
      f_programs      TYPE ty_programs,
      f_syntax_errors TYPE ty_syntax_errors,
      f_statistic     TYPE ty_statistics,
      f_alerts        TYPE ty_alerts,
      f_test_context  TYPE ty_context,
      f_au_factory    TYPE REF TO cl_aunit_factory,
      f_text_api      TYPE REF TO if_aunit_text_description.

ENDCLASS.



CLASS ZCL_UTILITY_AUTLISTENER IMPLEMENTATION.

  METHOD create_listener.

    DATA: oref_listener TYPE REF TO ZCL_UTILITY_AUTLISTENER.

    create object oref_listener.
    oref_listener->f_programs = i_programs.
    oref_listener->f_syntax_errors = i_syntax_errors.
    oref_listener->f_statistic-cnt_failure-syntax_error = lines( i_syntax_errors ).

    IF ( i_au_factory IS BOUND ).
      oref_listener->f_au_factory =  i_au_factory.
    ELSE.
      create object oref_listener->f_au_factory.
    ENDIF.
    oref_listener->init( ).
    oref_listener->extract_test_class( ).
    result = oref_listener.

  ENDMETHOD.

  METHOD add_devc_uri_syntax_errors.
    DATA:
      devclass TYPE tadir-devclass,
      package  TYPE ty_stat_package,
      escaped_name TYPE string.
    FIELD-SYMBOLS:
      <syntax_error> TYPE ty_syntax_error.

    check me->f_syntax_errors IS NOT INITIAL.
    LOOP AT me->f_syntax_errors ASSIGNING <syntax_error>.
      SELECT SINGLE devclass
        FROM  tadir INTO devclass
         WHERE
           pgmid     = 'R3TR'     AND
           object    = <syntax_error>-obj_type   AND
           obj_name  = <syntax_error>-obj_name. "#EC CI_GENBUFF
      READ TABLE me->f_statistic-packages
        WITH KEY name = devclass transporting NO FIELDS.
      IF ( 0 ne sy-subrc ).
        package-name = devclass.
        INSERT package INTO TABLE me->f_statistic-packages.
        ADD 1 TO me->f_statistic-cnt_packages.
      ENDIF.
    ENDLOOP.
  ENDMETHOD.

  METHOD init.
    FIELD-SYMBOLS:
      <test_class>  TYPE ty_test_class,
      <test_method> TYPE ty_test_method,
      <program>     TYPE ty_program.

    me->f_text_api = me->f_au_factory->get_text_converter( language = 'E' ).

    LOOP AT me->f_programs ASSIGNING <program>.
      CLEAR <program>-state.
      LOOP AT <program>-test_classes ASSIGNING <test_class>.
        CLEAR <test_class>-state.
        LOOP AT <test_class>-test_methods ASSIGNING <test_method>.
          CLEAR <test_method>-state.
        ENDLOOP.
      ENDLOOP.
    ENDLOOP.
  ENDMETHOD.


  METHOD if_aunit_listener~task_start.
    CLEAR:
      me->f_test_context,
      me->f_statistic,
      me->f_alerts.
  ENDMETHOD.


  METHOD if_aunit_listener~program_start.
    DATA:
      descr        TYPE if_aunit_text_description=>ty_s_description,
      package      TYPE ty_stat_package,
      message_text TYPE string.
    FIELD-SYMBOLS:
      <program>      TYPE ty_program.

    CLEAR me->f_test_context-test_METHOD.

    descr = info->get_description( ).
    READ TABLE descr-params index 1 INTO
       me->f_test_context-program.

    initialize_program_entry(  me->f_test_context-program ).

    READ TABLE me->f_Programs
      ASSIGNING <program> WITH KEY
        name = me->f_Test_Context-program.

    <program>-state-has_been_started = abap_true.
    me->f_test_context-obj_name = <program>-obj_name.
    me->f_test_context-obj_type = <program>-obj_type.
    me->f_test_context-package =  <program>-package.

    ADD 1 TO me->f_statistic-cnt_programs.
    READ TABLE me->f_statistic-packages
      WITH KEY name = <program>-package transporting NO FIELDS.
    IF ( 0 ne sy-subrc ).
      package-name = <program>-package.
      INSERT package INTO TABLE me->f_statistic-packages.
    ENDIF.
  ENDMETHOD.


  METHOD if_aunit_listener~class_start.
    DATA:
      descr         TYPE if_aunit_text_description=>ty_s_description.
    FIELD-SYMBOLS:
      <test_class> TYPE ty_test_class,
      <program>    TYPE ty_program.

    CLEAR me->f_test_context-test_class.
    CLEAR me->f_test_context-test_METHOD.

    descr = info->get_description( ).
    READ TABLE descr-params
      index 1 INTO me->f_test_context-test_class.

    READ TABLE me->f_Programs
      ASSIGNING <program> WITH KEY
        name = me->f_Test_Context-program.

    READ TABLE <program>-test_Classes ASSIGNING <test_Class>
      WITH KEY name = me->f_Test_Context-test_Class.
    IF sy-subrc <> 0.
        INSERT INITIAL line INTO TABLE <program>-test_Classes ASSIGNING <test_Class>.
        <test_Class>-name = me->f_Test_Context-test_Class.
    ENDIF.

    <test_class>-state-has_been_started = abap_true.
    ADD 1 TO me->f_statistic-cnt_test_classes.
  ENDMETHOD.


  METHOD if_aunit_listener~method_start.
    DATA:
      descr         TYPE if_aunit_text_description=>ty_s_description.
    FIELD-SYMBOLS:
      <test_method> TYPE ty_test_method,
      <test_class>  TYPE ty_test_class,
      <program>     TYPE ty_program.

    CLEAR me->f_test_context-test_METHOD.

    descr = info->get_description( ).
    READ TABLE descr-params index 1 INTO
       me->f_test_context-test_METHOD.

    READ TABLE me->f_Programs
      ASSIGNING <program> WITH KEY
        name = me->f_Test_Context-program.

    READ TABLE <program>-test_Classes ASSIGNING <test_Class>
      WITH KEY name = me->f_Test_Context-test_Class.
    IF sy-subrc <> 0.
        INSERT INITIAL line INTO TABLE <program>-test_Classes ASSIGNING <test_Class>.
        <test_Class>-name = me->f_Test_Context-test_Class.
    ENDIF.

    READ TABLE <test_Class>-test_Methods ASSIGNING <test_Method>
      WITH KEY name = me->f_Test_Context-test_Method.
    IF sy-subrc <> 0.
        INSERT INITIAL line INTO TABLE <test_Class>-test_Methods ASSIGNING <test_Method>.
        <test_Method>-name = me->f_Test_Context-test_Method.
    ENDIF.

    <test_method>-state-has_been_started = abap_true.
    ADD 1 TO <test_class>-state-count_exec_methods.
  ENDMETHOD.


  METHOD if_aunit_listener~method_end.
    FIELD-SYMBOLS:
      <program>     TYPE ty_program,
      <test_class>  TYPE ty_test_class,
      <test_method> TYPE ty_test_method.

    READ TABLE me->f_Programs
      ASSIGNING <program> WITH KEY
        name = me->f_Test_Context-program.

    READ TABLE <program>-test_Classes ASSIGNING <test_Class>
      WITH KEY name = me->f_Test_Context-test_Class.
    IF sy-subrc <> 0.
        INSERT INITIAL line INTO TABLE <program>-test_Classes ASSIGNING <test_Class>.
        <test_Class>-name = me->f_Test_Context-test_Class.
    ENDIF.

    READ TABLE <test_Class>-test_Methods ASSIGNING <test_Method>
      WITH KEY name = me->f_Test_Context-test_Method.
    IF sy-subrc <> 0.
        INSERT INITIAL line INTO TABLE <test_Class>-test_Methods ASSIGNING <test_Method>.
        <test_Method>-name = me->f_Test_Context-test_Method.
    ENDIF.

    IF ( <test_method>-alert IS INITIAL ).
      ADD 1 TO me->f_statistic-cnt_method-passed.
    ELSE.
      CASE <test_method>-alert-level.
        WHEN c_level-skipped.
          ADD 1 TO me->f_statistic-cnt_method-skipped.
          ADD 1 TO <test_class>-state-count_skipped_methods.
        WHEN c_level-fatal.
          ADD 1 TO me->f_statistic-cnt_method-with_fatal.
        WHEN c_level-critical.
          ADD 1 TO me->f_statistic-cnt_method-with_critical.
        WHEN c_level-tolerable.
          ADD 1 TO me->f_statistic-cnt_method-with_tolerable.
        WHEN OTHERS.
          CLEAR sy-subrc.
      ENDCASE.
    ENDIF.

    CLEAR:
      me->f_test_context-test_METHOD.
  ENDMETHOD.


  METHOD if_aunit_listener~class_end.
    FIELD-SYMBOLS:
      <program>     TYPE ty_program,
      <test_class>  TYPE ty_test_class,
      <test_method> TYPE ty_test_METHOD.

    READ TABLE me->f_Programs
      ASSIGNING <program> WITH KEY
        name = me->f_Test_Context-program.

    READ TABLE <program>-test_Classes ASSIGNING <test_Class>
      WITH KEY name = me->f_Test_Context-test_Class.
    IF sy-subrc <> 0.
        INSERT INITIAL line INTO TABLE <program>-test_Classes ASSIGNING <test_Class>.
        <test_Class>-name = me->f_Test_Context-test_Class.
    ENDIF.

    CASE abap_true.
      WHEN <test_class>-state-issue-has_been_skipped.
        LOOP AT <test_class>-test_methods ASSIGNING <test_method>
          WHERE
            state-has_been_started = abap_false.
          <test_method>-state-has_been_skipped = abap_true.
          ADD 1 TO me->f_statistic-cnt_method-skipped.
          ADD 1 TO <test_class>-state-count_skipped_methods.
          ADD 1 TO <test_class>-state-count_skipped_over_methods.
        ENDLOOP.
      WHEN <test_class>-state-issue-has_rt_failure or
           <test_class>-state-issue-has_timeout.
        IF ( <test_class>-state-count_exec_methods IS INITIAL ).
          " a runtime error or timeout cancels TO test sand box
          " without further information.
          " Unless the framework IS sick AT least one test method
          " must have been started. Align statistics accordingly.
          ADD 1 TO <test_class>-state-count_exec_methods.
          ADD 1 TO me->f_statistic-cnt_test_methods.
        ENDIF.
    ENDCASE.

    CLEAR:
      me->f_test_context-test_class,
      me->f_test_context-test_METHOD.
  ENDMETHOD.


  METHOD if_aunit_listener~program_end.
    FIELD-SYMBOLS:
      <program>    TYPE ty_program,
      <package>    TYPE ty_stat_package,
      <test_class> TYPE ty_test_class.

    READ TABLE me->f_Programs
      ASSIGNING <program> WITH KEY
        name = me->f_Test_Context-program.

    CLEAR me->f_test_context.
    READ TABLE me->f_statistic-packages[]
      ASSIGNING <package>
      WITH KEY name = <program>-package.
    ASSERT <package> IS ASSIGNED.

    LOOP AT <program>-test_classes[]
      ASSIGNING <test_class>
      WHERE state-has_been_started = abap_true.
      <package>-state-executed_classes =
        <package>-state-executed_classes + 1.
      <package>-state-executed_methods =
        <package>-state-executed_methods + <test_class>-state-count_exec_methods.
      <package>-state-skipped_methods =
        <package>-state-skipped_methods + <test_class>-state-count_skipped_methods.
    ENDLOOP.
  ENDMETHOD.


  METHOD if_aunit_listener~task_end.
    DELETE ADJACENT DUPLICATES FROM me->f_statistic-packages.
    finish_statistics( ).
  ENDMETHOD.


  METHOD handle_failure.
    DATA:
      adjust_method_totals TYPE abap_bool,
      native_level         TYPE aunit_level,
      alert                TYPE ty_alert,
      descr                TYPE if_aunit_text_description=>ty_s_description,
      count                TYPE i.
    FIELD-SYMBOLS:
      <program>     TYPE ty_program,
      <test_class>  TYPE ty_test_class,
      <test_method> TYPE ty_test_method.

    READ TABLE me->f_Programs
      ASSIGNING <program> WITH KEY
        name = me->f_Test_Context-program.

    READ TABLE <program>-test_Classes ASSIGNING <test_Class>
      WITH KEY name = me->f_Test_Context-test_Class.
    IF sy-subrc <> 0.
        INSERT INITIAL line INTO TABLE <program>-test_Classes ASSIGNING <test_Class>.
        <test_Class>-name = me->f_Test_Context-test_Class.
    ENDIF.

    READ TABLE <test_Class>-test_Methods ASSIGNING <test_Method>
      WITH KEY name = me->f_Test_Context-test_Method.
    IF sy-subrc <> 0.
        INSERT INITIAL line INTO TABLE <test_Class>-test_Methods ASSIGNING <test_Method>.
        <test_Method>-name = me->f_Test_Context-test_Method.
    ENDIF.

    IF ( <program> IS ASSIGNED ).
      <program>-state-has_issue = abap_true.
    ENDIF.
    IF ( <test_class> IS ASSIGNED ).
      <test_class>-state-issue-has_failure = abap_true.
    ENDIF.

    descr = i_ref_failure->get_header_description( ).
    alert-kind = i_kind.
    alert-context = me->f_test_context.
    alert-description = me->f_text_api->get_string( descr ).

    IF i_kind = c_kind-skipped.
      IF ( <test_method> IS ASSIGNED ).
        <test_method>-state-has_been_skipped = abap_true.
        <test_class>-state-issue-has_been_skipped = abap_true.
      elseif ( <test_class>  IS ASSIGNED ).
        <test_class>-state-issue-has_been_skipped = abap_true.
      ENDIF.
    ELSE.
      native_level = i_ref_failure->get_level( ).
      CASE native_level.
        WHEN if_aunit_constants=>fatal.
          ADD 1 TO me->f_statistic-cnt_failure-fatal.
        WHEN if_aunit_constants=>critical.
          ADD 1 TO me->f_statistic-cnt_failure-critical.
        WHEN OTHERS.
          ADD 1 TO me->f_statistic-cnt_failure-tolerable.
      ENDCASE.
      IF ( <test_method> IS NOT ASSIGNED AND <test_class> IS ASSIGNED ).
        " a timeout or runtime abortion IS reported on class level
        " however it happened within a METHOD. Fix totals in such a CASE
        " BY the assumption that AT least one method has been executed
        CASE i_kind.
          WHEN c_kind-rt_failure.
            <test_class>-state-issue-has_rt_failure = abap_true.
            adjust_method_totals = abap_true.
          WHEN c_kind-timeout.
            <test_class>-state-issue-has_timeout = abap_true.
            adjust_method_totals = abap_true.
          WHEN OTHERS.
            adjust_method_totals = abap_false.
        ENDCASE.
        IF ( abap_true =  adjust_method_totals ).
          CASE native_level.
            WHEN if_aunit_constants=>fatal.
              ADD 1 TO me->f_statistic-cnt_method-with_fatal.
            WHEN if_aunit_constants=>critical.
              ADD 1 TO me->f_statistic-cnt_method-with_critical.
            WHEN OTHERS.
              ADD 1 TO me->f_statistic-cnt_method-with_tolerable.
          ENDCASE.
        ENDIF.
      ENDIF.
    ENDIF.

    count = lines( me->f_alerts ).
    count = count MOD 2.
    IF ( 1 = count ).
      alert-apply_zebra = abap_true.
    ENDIF.

    INSERT alert INTO TABLE me->f_alerts[].

    IF <test_method> IS ASSIGNED.
      IF <test_method>-alert-level < alert-level.
        <test_method>-alert = alert.
      ENDIF.
    ENDIF.

  ENDMETHOD.


  METHOD if_aunit_listener~assert_failure.
    DATA:
      header TYPE if_aunit_text_description=>ty_s_description.
    header = failure->get_header_description( ).
    CASE header-id.
      WHEN OTHERS.
        handle_failure( i_kind = c_kind-assert_failure i_ref_failure = failure ).

    ENDCASE.
  ENDMETHOD.


  METHOD if_aunit_listener~warning.
    DATA:
      header TYPE if_aunit_text_description=>ty_s_description.
    header = warning->get_header_description( ).
    CASE header-id.
      WHEN OTHERS.
        handle_failure( i_kind = c_kind-warning i_ref_failure = warning ).

    ENDCASE.
  ENDMETHOD.


  METHOD if_aunit_listener~cx_failure.
    handle_failure( i_kind = c_kind-cx_failure i_ref_failure = failure ).
  ENDMETHOD.


  METHOD if_aunit_listener~rt_failure.
    handle_failure( i_kind = c_kind-rt_failure i_ref_failure = failure ).
  ENDMETHOD.


  METHOD if_aunit_listener~execution_event ##needed.
  ENDMETHOD.


  METHOD initialize_program_entry.
    DATA:
      escaped_name TYPE string,
      program      TYPE ty_program.
    FIELD-SYMBOLS:
      <program> TYPE ty_program.

    READ TABLE me->f_programs WITH KEY
      name = i_program_name
      ASSIGNING <program>.
    IF ( 0 ne sy-subrc ).
      program-name = i_program_name.
      cl_aunit_prog_info=>progname_to_tadir(
        EXPORTING
          progname = program-name
        IMPORTING
          obj_name = program-obj_name
          obj_type = program-obj_type ).
      INSERT program INTO TABLE me->f_programs ASSIGNING <program>.
    ENDIF.
  ENDMETHOD.


  METHOD finish_statistics.
    DATA:
      fraction_of_4       TYPE i,
      nbr_of_fill_entries TYPE i.
    FIELD-SYMBOLS:
      <test_class> TYPE ty_test_class,
      <program>    TYPE ty_program,
      <package>    TYPE ty_stat_package.

    me->f_statistic-cnt_failure-syntax_error = lines( me->f_syntax_errors ).
    add_devc_uri_syntax_errors( ).

    LOOP AT me->f_programs ASSIGNING <program> WHERE state IS NOT INITIAL.
      ADD 1 TO me->f_statistic-cnt_programs.
      LOOP AT <program>-test_classes ASSIGNING <test_class> WHERE state IS NOT INITIAL.
        ADD 1 TO me->f_statistic-cnt_test_classes.
        LOOP AT <test_class>-test_methods transporting NO FIELDS WHERE state IS NOT INITIAL.
          ADD 1 TO me->f_statistic-cnt_test_methods.
        ENDLOOP.
      ENDLOOP.
    ENDLOOP.

    me->f_statistic-cnt_packages = lines( me->f_statistic-packages ).

    me->f_statistic-cnt_failure-total =
      me->f_statistic-cnt_failure-critical +
      me->f_statistic-cnt_failure-fatal +
      me->f_statistic-cnt_failure-tolerable.

    " prepare package data to ease rendering
    SORT me->f_statistic-packages BY name ASCENDING.
    IF ( me->f_statistic-packages IS NOT INITIAL ).
      LOOP AT me->f_statistic-packages ASSIGNING <package>.
        fraction_of_4 = sy-tabix  MOD 4.
        IF ( 1 eq fraction_of_4 AND sy-tabix <> 1 ).
          <package>-new_line = abap_true.
        ENDIF.
      ENDLOOP.
      nbr_of_fill_entries = ( 4 - fraction_of_4 ) MOD 4.
      DO nbr_of_fill_entries TIMES.
        INSERT INITIAL line INTO TABLE me->f_statistic-packages.
      enddo.
    ENDIF.
  ENDMETHOD.

  METHOD EXTRACT_TEST_CLASS.

    DATA:
      syntax_error       TYPE ty_syntax_error,
      method_names       TYPE saunit_t_methods,
      method_name        TYPE saunit_d_method,
      method             TYPE ty_test_method,
      test_class         TYPE ty_test_class,
      test_class_handles
        TYPE if_aunit_test_class_handle=>ty_t_testclass_handles.
    FIELD-SYMBOLS:
      <program> TYPE ty_program.

    LOOP AT me->f_programs ASSIGNING <program>.
      test_class_handles =
         me->f_au_factory->get_test_class_handles(
           obj_type = <program>-obj_type
           obj_name = <program>-obj_name  ).

      CLEAR test_class.
      LOOP AT test_class_handles INTO test_class-handle.

        test_class-name = test_class-handle->get_class_name( ).
        method_names = test_class-handle->get_test_methods( ).

        LOOP AT method_names INTO method-name.
          INSERT method INTO TABLE test_class-test_methods.
        ENDLOOP.

        INSERT test_class INTO TABLE <program>-test_classes.
        CLEAR test_class.
      ENDLOOP.

    ENDLOOP.

  ENDMETHOD.

  METHOD GET_RESULT.
    e_programs = me->f_programs.
    e_statistic = me->f_statistic.
  ENDMETHOD.

ENDCLASS.