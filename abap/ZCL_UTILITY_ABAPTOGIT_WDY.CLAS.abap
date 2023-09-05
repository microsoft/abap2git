CLASS zcl_hrpy_utility_abaptogit_wdy DEFINITION
  PUBLIC
  FINAL
  CREATE PRIVATE

  GLOBAL FRIENDS cl_wdy_md_adt_code_wizard
                 cl_wdy_md_adt_component
                 cl_wdy_md_adt_ctlr_source_util
                 cl_wdy_md_adt_view
                 cl_wdy_md_adt_window
                 cl_wdy_md_adt_interface_view .

  PUBLIC SECTION.

    INTERFACES if_wdy_md_adt_controller .

    CLASS-METHODS get_instance_by_key
      IMPORTING
        !component_name  TYPE csequence
        !controller_name TYPE csequence
        !version         TYPE r3state DEFAULT 'I'
      RETURNING
        VALUE(result)    TYPE REF TO if_wdy_md_adt_controller
      RAISING
        cx_wdy_md_not_existing .
    INTERFACE if_wdy_md_adt_controller LOAD .
    CLASS-METHODS get_instance_by_data
      IMPORTING
        !controller_data            TYPE if_wdy_md_adt_controller=>gty_s_controller
        !source_implementation_part TYPE rswsourcet OPTIONAL
        !adt_view                   TYPE REF TO if_wdy_md_adt_abstract_view OPTIONAL
      RETURNING
        VALUE(result)               TYPE REF TO if_wdy_md_adt_controller
      RAISING
        cx_wdy_md_not_existing
        cx_wdy_md_adt_exception .
    CLASS-METHODS create
      IMPORTING
        !component_name  TYPE csequence
        !controller_name TYPE csequence
        !description     TYPE wdy_md_description OPTIONAL
      RETURNING
        VALUE(result)    TYPE REF TO if_wdy_md_adt_controller
      RAISING
        cx_wdy_md_already_existing
        cx_wdy_md_adt_exception .
    CLASS-METHODS get_instance_by_version
      IMPORTING
        !version_number  TYPE vrsd-versno
        !destination     TYPE rfcdes-rfcdest OPTIONAL
        !component_name  TYPE csequence
        !controller_name TYPE csequence
        !adt_view        TYPE REF TO if_wdy_md_adt_abstract_view OPTIONAL "Required for source code retrieval
      RETURNING
        VALUE(result)    TYPE REF TO if_wdy_md_adt_controller
      RAISING
        cx_wdy_md_not_existing .
    CLASS-METHODS check_existency
      IMPORTING
        !component_name  TYPE csequence
        !controller_name TYPE csequence
        !active          TYPE wdy_boolean
      RETURNING
        VALUE(result)    TYPE wdy_boolean .
  PROTECTED SECTION.
  PRIVATE SECTION.

    ALIASES gty_s_action
      FOR if_wdy_md_adt_controller~gty_s_action .
    ALIASES gty_s_attribute
      FOR if_wdy_md_adt_controller~gty_s_attribute .
    ALIASES gty_s_context_attribute
      FOR if_wdy_md_adt_controller~gty_s_context_attribute .
    ALIASES gty_s_context_mapping
      FOR if_wdy_md_adt_controller~gty_s_context_mapping .
    ALIASES gty_s_context_node
      FOR if_wdy_md_adt_controller~gty_s_context_node .
    ALIASES gty_s_controller
      FOR if_wdy_md_adt_controller~gty_s_controller .
    ALIASES gty_s_controller_usage
      FOR if_wdy_md_adt_controller~gty_s_controller_usage .
    ALIASES gty_s_event
      FOR if_wdy_md_adt_controller~gty_s_event .
    ALIASES gty_s_event_handler
      FOR if_wdy_md_adt_controller~gty_s_event_handler .
    ALIASES gty_s_exception
      FOR if_wdy_md_adt_controller~gty_s_exception .
    ALIASES gty_s_method
      FOR if_wdy_md_adt_controller~gty_s_method .
    ALIASES gty_s_parameter
      FOR if_wdy_md_adt_controller~gty_s_parameter .
    ALIASES gty_s_supply_function
      FOR if_wdy_md_adt_controller~gty_s_supply_function .
    ALIASES gty_t_exception
      FOR if_wdy_md_adt_controller~gty_t_exception .
    ALIASES gty_t_parameter
      FOR if_wdy_md_adt_controller~gty_t_parameter .

    DATA m_controller_key TYPE wdy_md_controller_key .
    DATA m_langu TYPE langu .
    DATA m_reader TYPE REF TO if_wdy_md_controller_reader .
    DATA m_version TYPE r3state .
    DATA m_compos TYPE wdy_ctlr_compo_table .
    DATA m_compo_txts TYPE wdy_ctlr_compot_table .
    DATA m_params TYPE wdy_ctlr_param_table .
    DATA m_param_txts TYPE wdy_ctlr_paramt_table .
    DATA m_exceptions TYPE wdy_ctlr_exc_table .
    DATA m_exception_txts TYPE wdy_ctlr_exct_table .
    DATA m_adt_view_ref TYPE REF TO cl_wdy_md_adt_abstract_view .
    CONSTANTS c_node_type_recursion TYPE wdy_md_object_name VALUE 'CL_WDY_MD_CTX_RECURSION_NODE'. "#EC NOTEXT
    CONSTANTS c_node_type_value TYPE wdy_md_object_name VALUE 'CL_WDY_MD_CONTEXT_VALUE_NODE'. "#EC NOTEXT
    CONSTANTS c_attr_type_value TYPE wdy_md_object_name VALUE 'CL_WDY_MD_CTX_VALUE_ATTRIBUTE'. "#EC NOTEXT
    DATA m_controller_data TYPE gty_s_controller .
    DATA m_implementation TYPE rswsourcet .                                                 " .
    TYPE-POOLS abap .
    DATA m_is_interface_ctlr TYPE abap_bool .
    DATA m_is_comp_interface TYPE abap_bool .
    DATA m_called_by_unit_test TYPE abap_bool .
    DATA m_version_number TYPE vrsd-versno .

    INTERFACE if_wdy_md_adt_controller LOAD .
    METHODS add_predef_methods
      CHANGING
        !methods TYPE if_wdy_md_adt_controller=>gty_t_method .
    CLASS-METHODS raise_adt_exception_by_msg
      IMPORTING
        !previous TYPE REF TO cx_root OPTIONAL
      RAISING
        cx_wdy_md_adt_exception .
    METHODS convert_ctx_help_to_md
      IMPORTING
        !p_attribute    TYPE gty_s_context_attribute
      CHANGING
        !p_md_attribute TYPE wdy_ctx_attrib .
    METHODS convert_ctx_help_to_extern
      IMPORTING
        !p_md_attribute TYPE wdy_ctx_attrib
      CHANGING
        !p_attribute    TYPE gty_s_context_attribute .
    METHODS check_node_positions
      IMPORTING
        !p_parent_node_name TYPE if_wdy_md_adt_controller=>gty_context_node_name
      RAISING
        cx_wdy_md_adt_exception .
    METHODS check_format_properties
      IMPORTING
        !p_attribute TYPE wdy_ctx_attrib
      RAISING
        cx_wdy_md_adt_exception .
    METHODS check_attrib_positions
      IMPORTING
        !p_node TYPE if_wdy_md_adt_controller=>gty_s_context_node
      RAISING
        cx_wdy_md_adt_exception .
    TYPE-POOLS saboo .
    METHODS check_missing_method_defs
      IMPORTING
        !p_method_impls TYPE saboo_method_impl_tab
      RAISING
        cx_wdy_md_adt_exception .
    METHODS split_implementation
      EXPORTING
        !p_method_impls TYPE saboo_method_impl_tab
      RAISING
        cx_wdy_md_adt_exception .
    CLASS-METHODS create_internal
      IMPORTING
        !p_controller_key          TYPE wdy_controller_key
        !p_controller_type         TYPE wdy_md_controller_type
        !p_description             TYPE wdy_md_description OPTIONAL
        !p_for_new_component       TYPE abap_bool DEFAULT space
      RETURNING
        VALUE(p_ref_md_controller) TYPE REF TO if_wdy_md_controller
      RAISING
        cx_wdy_md_already_existing
        cx_wdy_md_adt_exception .
    METHODS modify_code_body
      IMPORTING
        !p_method_impls TYPE saboo_method_impl_tab
        !p_method_name  TYPE csequence
      CHANGING
        !p_code_body    TYPE string
      RAISING
        cx_wdy_md_adt_exception .
    METHODS convert_supply_funcs_to_md
      IMPORTING
        !p_method_impls       TYPE saboo_method_impl_tab OPTIONAL
      CHANGING
        !p_controller_data_md TYPE wdy_md_controller_meta_data
      RAISING
        cx_wdy_md_adt_exception .
    METHODS convert_methods_to_md
      IMPORTING
        !p_method_impls       TYPE saboo_method_impl_tab OPTIONAL
      CHANGING
        !p_controller_data_md TYPE wdy_md_controller_meta_data
      RAISING
        cx_wdy_md_adt_exception .
    METHODS convert_event_handler_to_md
      IMPORTING
        !p_method_impls       TYPE saboo_method_impl_tab OPTIONAL
      CHANGING
        !p_controller_data_md TYPE wdy_md_controller_meta_data
      RAISING
        cx_wdy_md_adt_exception .
    METHODS convert_events_to_md
      CHANGING
        !p_controller_data_md TYPE wdy_md_controller_meta_data
      RAISING
        cx_wdy_md_adt_exception .
    METHODS convert_ctlr_usage_to_md
      CHANGING
        !p_controller_data_md TYPE wdy_md_controller_meta_data
      RAISING
        cx_wdy_md_adt_exception .
    METHODS convert_controller_data_to_md
      EXPORTING
        !p_controller_data_md TYPE wdy_md_controller_meta_data
      RAISING
        cx_wdy_md_adt_exception .
    METHODS convert_context_to_md
      CHANGING
        !p_controller_data_md TYPE wdy_md_controller_meta_data
      RAISING
        cx_wdy_md_adt_exception .
    METHODS convert_attributes_to_md
      CHANGING
        !p_controller_data_md TYPE wdy_md_controller_meta_data
      RAISING
        cx_wdy_md_adt_exception .
    METHODS convert_actions_to_md
      CHANGING
        !p_controller_data_md TYPE wdy_md_controller_meta_data
      RAISING
        cx_wdy_md_adt_exception .
    METHODS convert_exception_to_md
      IMPORTING
        !p_cmpname        TYPE csequence
        !p_exceptions     TYPE gty_t_exception
      CHANGING
        !p_exceptions_md  TYPE wdy_ctlr_exc_table
        !p_exceptionst_md TYPE wdy_ctlr_exct_table
      RAISING
        cx_wdy_md_adt_exception .
    METHODS convert_parameter_to_md
      IMPORTING
        !p_cmpname        TYPE csequence
        !p_parameters     TYPE gty_t_parameter
      CHANGING
        !p_parameters_md  TYPE wdy_ctlr_param_table
        !p_parameterst_md TYPE wdy_ctlr_paramt_table
      RAISING
        cx_wdy_md_adt_exception .
    METHODS constructor
      IMPORTING
        !p_controller_key  TYPE wdy_md_controller_key
        !p_version         TYPE r3state OPTIONAL
        !p_langu           TYPE langu
        !p_adt_view        TYPE REF TO if_wdy_md_adt_abstract_view OPTIONAL
        !p_controller_data TYPE if_wdy_md_adt_controller=>gty_s_controller OPTIONAL
        !p_implementation  TYPE rswsourcet OPTIONAL
        !p_version_number  TYPE vrsd-versno OPTIONAL
        !p_destination     TYPE rfcdes-rfcdest OPTIONAL .
    METHODS load_compos_internal .
    METHODS get_exceptions
      IMPORTING
        !p_cmpname    TYPE wdy_md_object_name
      RETURNING
        VALUE(result) TYPE gty_t_exception .
    METHODS get_parameters
      IMPORTING
        !p_cmpname    TYPE csequence
      RETURNING
        VALUE(result) TYPE gty_t_parameter .
    METHODS get_controller_ref
      IMPORTING
        !create_predef_methods TYPE abap_bool DEFAULT ' '
      RETURNING
        VALUE(result)          TYPE REF TO if_wdy_md_controller
      RAISING
        cx_wdy_md_not_existing
        cx_wdy_md_adt_exception .
    TYPE-POOLS svrs2 .
    METHODS prepare_save
      EXPORTING
        !p_new_data_vrs TYPE svrs2_wdyc
        !p_delta        TYPE svrs2_xwdyc
      RAISING
        cx_wdy_md_adt_exception .
ENDCLASS.



CLASS zcl_hrpy_utility_abaptogit_wdy IMPLEMENTATION.


  METHOD add_predef_methods.
* make sure that this method is only called if NO predefined "WDDO..." method exists at all!
    DATA: l_ref_controller   TYPE REF TO if_wdy_md_controller,
          l_method_map       TYPE REF TO cl_object_map,
          l_param_map        TYPE REF TO cl_object_map,
          l_method           TYPE REF TO if_wdy_md_method,
          l_parameter        TYPE REF TO if_wdy_md_parameter,
          l_method_def       TYPE gty_s_method,
          l_parameter_def    TYPE gty_s_parameter.
    FIELD-SYMBOLS: <f_method_entry> TYPE cl_object_map=>ty_map_element,
                   <f_param_entry> TYPE cl_object_map=>ty_map_element.

    TRY.
        "get MD reference and create the predefined methods
        l_ref_controller = me->get_controller_ref( create_predef_methods = abap_true ).

        "append the predefined methods to the method table
        l_method_map ?= l_ref_controller->get_methods( ).
        CHECK l_method_map IS BOUND.
        LOOP AT l_method_map->map ASSIGNING <f_method_entry>.
          TRY.
              l_method ?= <f_method_entry>-value.
            CATCH cx_sy_move_cast_error.
              CONTINUE.
          ENDTRY.
          IF l_method IS BOUND.
            l_method_def-cmpname = l_method->if_wdy_md_object~get_name( ).
            TRANSLATE l_method_def-cmpname TO UPPER CASE. "#EC SYNTCHAR

            " append only predefined methods!!!
            IF cl_wdy_wb_vc_proc_support=>is_predef_method( l_method_def-cmpname ) = abap_true.
              l_method_def-description   = l_method->if_wdy_md_object~get_description( ).
              l_method_def-is_intf_item  = ' '.
              l_method_def-is_predefined = abap_true.
              l_method_def-code_body     = l_method->get_code_body( ).
              CLEAR l_method_def-parameters.

              "parameter
              l_param_map ?= l_method->get_parameters( ).
              IF l_param_map IS BOUND.
                LOOP AT l_param_map->map ASSIGNING <f_param_entry>.
                  CLEAR l_parameter.
                  TRY.
                      l_parameter ?= <f_param_entry>-value.
                    CATCH cx_sy_move_cast_error.
                      CONTINUE.
                  ENDTRY.
                  IF l_parameter IS BOUND.
                    l_parameter_def-parameter_name   = l_parameter->get_name( ).
                    l_parameter_def-declaration_type = l_parameter->get_declaration_type( ).
                    l_parameter_def-abap_typing      = l_parameter->get_abap_typing( ).
                    l_parameter_def-abap_type        = l_parameter->get_abap_type( ).
                    l_parameter_def-optional         = l_parameter->get_optional( ).
                    l_parameter_def-default_value    = l_parameter->get_default_value( ).
                    l_parameter_def-description      = l_parameter->if_wdy_md_object~get_description( ).
                    APPEND l_parameter_def TO l_method_def-parameters.
                  ENDIF.
                ENDLOOP.
              ENDIF.

              "no exceptions
              APPEND l_method_def TO methods.
            ENDIF.

          ENDIF.
        ENDLOOP.

      CATCH cx_wdy_md_exception cx_wdy_md_adt_exception. "#EC NOHANDLER
    ENDTRY.
  ENDMETHOD.


  METHOD check_attrib_positions.
    DATA: l_pos_tab  TYPE STANDARD TABLE OF i,
          l_pos      TYPE i,
          l_msg_str  TYPE string.                           "#EC NEEDED
    FIELD-SYMBOLS: <f_attr>        TYPE gty_s_context_attribute.

    LOOP AT p_node-attributes ASSIGNING <f_attr>.
      IF <f_attr>-attrib_position < 1.
        MESSAGE e053(swdp_wdy_md_adt) WITH p_node-node_name INTO l_msg_str.
        me->raise_adt_exception_by_msg( ).
      ELSE.
        APPEND <f_attr>-attrib_position TO l_pos_tab.
      ENDIF.
    ENDLOOP.
    SORT l_pos_tab.
    LOOP AT l_pos_tab INTO l_pos.
      IF l_pos <> sy-tabix.
        MESSAGE e053(swdp_wdy_md_adt) WITH p_node-node_name INTO l_msg_str.
        me->raise_adt_exception_by_msg( ).
      ENDIF.
    ENDLOOP.
  ENDMETHOD.


  METHOD check_existency.
    result = cl_wdy_md_controller=>check_existency(
        component_name  = component_name
        controller_name = controller_name
        active          = active ).
  ENDMETHOD.


  METHOD check_format_properties.
    DATA l_msg_str TYPE string.                             "#EC NEEDED

    IF NOT p_attribute-null_as_blank IS INITIAL.
      IF cl_wdy_wb_context_checker=>check_attr_format_property(
           format_struc_field = 'NULL_AS_BLANK'
           attribute_type = p_attribute-abap_type ) = abap_false.
        MESSAGE e030(swdp_wdy_md_adt) WITH 'NULL_AS_BLANK' p_attribute-node_name p_attribute-attribute_name INTO l_msg_str.
        me->raise_adt_exception_by_msg( ).
      ENDIF.
    ENDIF.

    IF NOT p_attribute-sign_pos IS INITIAL.
      IF cl_wdy_wb_context_checker=>check_attr_format_property(
           format_struc_field = 'SIGN_POS'
           attribute_type = p_attribute-abap_type ) = abap_false.
        MESSAGE e030(swdp_wdy_md_adt) WITH 'SIGN_POS' p_attribute-node_name p_attribute-attribute_name INTO l_msg_str.
        me->raise_adt_exception_by_msg( ).
      ENDIF.
    ENDIF.

    IF NOT p_attribute-condense IS INITIAL.
      IF cl_wdy_wb_context_checker=>check_attr_format_property(
           format_struc_field = 'CONDENSE'
           attribute_type = p_attribute-abap_type ) = abap_false.
        MESSAGE e030(swdp_wdy_md_adt) WITH 'CONDENSE' p_attribute-node_name p_attribute-attribute_name INTO l_msg_str.
        me->raise_adt_exception_by_msg( ).
      ENDIF.
    ENDIF.

    IF NOT p_attribute-short_time IS INITIAL.
      IF cl_wdy_wb_context_checker=>check_attr_format_property(
           format_struc_field = 'SHORT_TIME'
           attribute_type = p_attribute-abap_type ) = abap_false.
        MESSAGE e030(swdp_wdy_md_adt) WITH 'SHORT_TIME' p_attribute-node_name p_attribute-attribute_name INTO l_msg_str.
        me->raise_adt_exception_by_msg( ).
      ENDIF.
    ENDIF.

    IF NOT p_attribute-date_format IS INITIAL.
      IF cl_wdy_wb_context_checker=>check_attr_format_property(
           format_struc_field = 'DATE_FORMAT'
           attribute_type = p_attribute-abap_type ) = abap_false.
        MESSAGE e030(swdp_wdy_md_adt) WITH 'DATE_FORMAT' p_attribute-node_name p_attribute-attribute_name INTO l_msg_str.
        me->raise_adt_exception_by_msg( ).
      ENDIF.
    ENDIF.
  ENDMETHOD.


  METHOD check_missing_method_defs.
    DATA: l_msg_str       TYPE string.                      "#EC NEEDED
    FIELD-SYMBOLS: <f_method_impls> TYPE saboo_method_impl.

    IF p_method_impls IS INITIAL.
      MESSAGE e504(swdp_wb_tool) WITH text-e18 INTO l_msg_str.
      me->raise_adt_exception_by_msg( ).
    ENDIF.

    LOOP AT p_method_impls ASSIGNING <f_method_impls>.
      CHECK <f_method_impls>-mtdkey-cpdname <> 'CONSTRUCTOR'.
      READ TABLE m_controller_data-methods TRANSPORTING NO FIELDS
           WITH KEY cmpname = <f_method_impls>-mtdkey-cpdname. "#EC WARNOK
      IF sy-subrc <> 0.
        READ TABLE m_controller_data-supply_functions TRANSPORTING NO FIELDS
             WITH KEY cmpname = <f_method_impls>-mtdkey-cpdname. "#EC WARNOK
        IF sy-subrc <> 0.
          READ TABLE m_controller_data-event_handler TRANSPORTING NO FIELDS
               WITH KEY cmpname = <f_method_impls>-mtdkey-cpdname. "#EC WARNOK
          IF sy-subrc <> 0.
            MESSAGE e170(swdp_wb_tool) WITH <f_method_impls>-mtdkey-cpdname INTO l_msg_str.
            me->raise_adt_exception_by_msg( ).
          ENDIF.
        ENDIF.
      ENDIF.
    ENDLOOP.

  ENDMETHOD.


  METHOD check_node_positions.
    DATA: l_pos_tab  TYPE STANDARD TABLE OF i,
          l_pos      TYPE i,
          l_msg_str  TYPE string.                           "#EC NEEDED
    FIELD-SYMBOLS: <f_node>        TYPE gty_s_context_node.

    LOOP AT m_controller_data-context ASSIGNING <f_node>
         WHERE parent_node_name = p_parent_node_name.
      IF <f_node>-node_position < 1.
        MESSAGE e054(swdp_wdy_md_adt) WITH p_parent_node_name INTO l_msg_str.
        me->raise_adt_exception_by_msg( ).
      ELSE.
        APPEND <f_node>-node_position TO l_pos_tab.
      ENDIF.
      me->check_node_positions( p_parent_node_name = <f_node>-node_name ).
      me->check_attrib_positions( p_node = <f_node> ).
    ENDLOOP.
    SORT l_pos_tab.
    LOOP AT l_pos_tab INTO l_pos.
      IF l_pos <> sy-tabix.
        MESSAGE e054(swdp_wdy_md_adt) WITH p_parent_node_name INTO l_msg_str.
        me->raise_adt_exception_by_msg( ).
      ENDIF.
    ENDLOOP.
  ENDMETHOD.


  METHOD constructor.
    DATA: l_comp_type      TYPE wdy_md_component_type,
          l_controller_key TYPE wdy_md_controller_key,
          l_reader_db      TYPE REF TO cl_wdy_md_controller_reader_db,
          l_reader_vrs     TYPE REF TO cl_wdy_md_ctlr_reader_vrs.

    m_controller_key = p_controller_key.
    m_version = p_version.
    m_version_number = p_version_number.
    m_langu = p_langu.
    m_adt_view_ref ?= p_adt_view.
    m_controller_data = p_controller_data.
    m_implementation = p_implementation.
    l_controller_key = m_controller_key.

    SELECT SINGLE type FROM wdy_component INTO l_comp_type  "#EC WARNOK
           WHERE component_name = p_controller_key-component_name. "#EC CI_GENBUFF

    IF l_comp_type = wdyn_component_type_interface.
      m_is_comp_interface = abap_true.
    ELSEIF m_controller_key-controller_name = wdyn_interface_controller_name.
      m_is_interface_ctlr = abap_true.
      l_controller_key-controller_name = wdyn_component_controller_name.
    ENDIF.

    IF m_controller_data IS INITIAL AND p_version_number IS INITIAL.    "=>load from db
      CREATE OBJECT l_reader_db
        EXPORTING
          version        = m_version
          controller_key = l_controller_key
          langu          = m_langu.
      m_reader = l_reader_db.

    ELSEIF NOT p_version_number IS INITIAL.    "=>load from version database
      CREATE OBJECT l_reader_vrs
        EXPORTING
          version        = p_version_number
          controller_key = l_controller_key
          langu          = m_langu
          destination    = p_destination.
      m_reader = l_reader_vrs.
    ENDIF.

  ENDMETHOD.


  METHOD convert_actions_to_md.
    DATA: l_compo      TYPE wdy_ctlr_compo,
          l_compot     TYPE wdy_ctlr_compot,
          l_msg_str    TYPE string,                         "#EC NEEDED
          l_name       TYPE wdy_md_object_name,
          l_evhan_name TYPE wdy_md_object_name,
          l_domvalue   TYPE domvalue_l.
    FIELD-SYMBOLS: <f_action>  TYPE gty_s_action,
                   <f_ev_handler> TYPE gty_s_event_handler.

    IF m_controller_data-definition-controller_type <> wdyn_ctlr_type_view AND
       NOT m_controller_data-actions IS INITIAL.
      MESSAGE e504(swdp_wb_tool) WITH text-e04 text-e05 INTO l_msg_str.
      me->raise_adt_exception_by_msg( ).
    ENDIF.

* actions
    LOOP AT m_controller_data-actions ASSIGNING <f_action>.
      IF <f_action>-cmpname IS INITIAL.
        MESSAGE e045(swdp_wdy_md_adt) INTO l_msg_str.
        me->raise_adt_exception_by_msg( ).
      ENDIF.
*   check compo name is unique
      READ TABLE p_controller_data_md-controller_components TRANSPORTING NO FIELDS
           WITH KEY cmpname = <f_action>-cmpname.
      IF sy-subrc = 0.
        MESSAGE e605(swdp_wb_tool)
          WITH <f_action>-cmpname m_controller_data-definition-controller_name INTO l_msg_str.
        me->raise_adt_exception_by_msg( ).
      ENDIF.
*   check name
      l_name = <f_action>-cmpname.
      cl_wdy_wb_checker=>check_component_name( EXPORTING p_name = l_name
                                                         p_is_proc = 'X'
                                               EXCEPTIONS OTHERS = 1 ).
      IF sy-subrc <> 0.
        me->raise_adt_exception_by_msg( ).
      ENDIF.
*   check eventhandler existency
      l_evhan_name = cl_wdy_wb_action_ed=>get_method_name_for_action( <f_action>-cmpname ).
      READ TABLE m_controller_data-event_handler ASSIGNING <f_ev_handler> WITH KEY cmpname = l_evhan_name.
      IF sy-subrc <> 0 OR (  "missing eventhandler
         ( NOT <f_ev_handler>-ref_component IS INITIAL AND
         <f_ev_handler>-ref_component <> m_controller_data-definition-component_name ) OR
           " The below line is commented because copy of a view while saving passes through this logic
           " hence the reference name and definition name will be different as the copy source will be different
           " the destination. Customer CSN: 002075129500001285522018
*         <f_ev_handler>-ref_ctlr_name <> m_controller_data-definition-controller_name OR
         <f_ev_handler>-ref_cmpname <> <f_action>-cmpname ).
        MESSAGE e504(swdp_wb_tool) WITH text-e04 text-002 <f_action>-cmpname INTO l_msg_str.
        me->raise_adt_exception_by_msg( ).
      ENDIF.
*   check action type
      IF NOT <f_action>-action_type IS INITIAL.
        l_domvalue = <f_action>-action_type.
        IF cl_wdy_wb_vc_checker=>check_dom_value( p_value   = l_domvalue
                                                  p_domname = 'WDY_WB_VC_ACTION_TYPE' ) IS INITIAL.
          MESSAGE e521(swdp_wb_tool) WITH  <f_action>-action_type <f_action>-cmpname INTO l_msg_str.
          me->raise_adt_exception_by_msg( ).
        ENDIF.
      ENDIF.

      CLEAR: l_compo, l_compot.
      MOVE-CORRESPONDING <f_action> TO l_compo.
      l_compo-component_name = m_controller_data-definition-component_name.
      l_compo-controller_name = m_controller_data-definition-controller_name.
      TRANSLATE l_compo-cmpname TO UPPER CASE.
      "l_compo-version = 'I'.
      l_compo-cmptype = wdyn_ctlr_compo_action.
      "cmp_position ???
      l_compo-display_name = l_compo-cmpname.
      APPEND l_compo TO p_controller_data_md-controller_components.

      MOVE-CORRESPONDING <f_action> TO l_compot.
      l_compot-component_name = m_controller_data-definition-component_name.
      l_compot-controller_name = m_controller_data-definition-controller_name.
      l_compot-langu = m_langu.
      APPEND l_compot TO p_controller_data_md-controller_component_texts.

      SORT <f_action>-parameters BY parameter_name.
      me->convert_parameter_to_md(
           EXPORTING p_cmpname    = <f_action>-cmpname
                     p_parameters = <f_action>-parameters
           CHANGING  p_parameters_md  = p_controller_data_md-controller_parameters
                     p_parameterst_md = p_controller_data_md-controller_parameter_texts ).
    ENDLOOP.

  ENDMETHOD.


  METHOD convert_attributes_to_md.
    DATA: l_compo     TYPE wdy_ctlr_compo,
          l_compot    TYPE wdy_ctlr_compot,
          l_domvalue  TYPE domvalue_l,
          l_msg_str   TYPE string.                          "#EC NEEDED
    FIELD-SYMBOLS: <f_attr>  TYPE gty_s_attribute.

    IF ( m_controller_data-definition-controller_type = wdyn_ctlr_type_intf_view OR
         m_controller_data-definition-controller_type = wdyn_ctlr_type_cmp_intf ) AND
       NOT m_controller_data-attributes IS INITIAL.
      MESSAGE e504(swdp_wb_tool) WITH text-e04 text-e06 INTO l_msg_str.
      me->raise_adt_exception_by_msg( ).
    ENDIF.

    LOOP AT m_controller_data-attributes ASSIGNING <f_attr>.
      IF <f_attr>-cmpname IS INITIAL.
        MESSAGE e038(swdp_wdy_md_adt) INTO l_msg_str.
        me->raise_adt_exception_by_msg( ).
      ENDIF.
*   check compo name is unique
      READ TABLE p_controller_data_md-controller_components TRANSPORTING NO FIELDS
           WITH KEY cmpname = <f_attr>-cmpname.
      IF sy-subrc = 0.
        MESSAGE e605(swdp_wb_tool)
          WITH <f_attr>-cmpname m_controller_data-definition-controller_name INTO l_msg_str.
        me->raise_adt_exception_by_msg( ).
      ENDIF.
*   check name
      cl_wdy_wb_checker=>check_component_name( EXPORTING p_name = <f_attr>-cmpname
                                                         p_is_proc = 'X'
                                               EXCEPTIONS OTHERS = 1 ).
      IF sy-subrc <> 0.
        me->raise_adt_exception_by_msg( ).
      ENDIF.
      IF <f_attr>-abap_type IS INITIAL.
        MESSAGE e039(swdp_wdy_md_adt) WITH <f_attr>-cmpname INTO l_msg_str.
        me->raise_adt_exception_by_msg( ).
      ENDIF.
      IF NOT <f_attr>-abap_typing IS INITIAL.
        l_domvalue = <f_attr>-abap_typing.
        IF cl_wdy_wb_vc_checker=>check_dom_value( p_value   = l_domvalue
                                                  p_domname = 'WDY_MD_ABAP_TYPING_ENUM' ) IS INITIAL.
          MESSAGE e059(swdp_wdy_md_adt)
            WITH <f_attr>-abap_typing 'WDY_MD_ABAP_TYPING_ENUM' text-004 <f_attr>-cmpname INTO l_msg_str.
          me->raise_adt_exception_by_msg( ).
        ENDIF.
      ENDIF.
      CLEAR: l_compo, l_compot.
      MOVE-CORRESPONDING <f_attr> TO l_compo.
      l_compo-component_name = m_controller_data-definition-component_name.
      l_compo-controller_name = m_controller_data-definition-controller_name.
      TRANSLATE l_compo-cmpname TO UPPER CASE.
      "l_compo-version = 'I'.
      l_compo-cmptype = wdyn_ctlr_compo_attribute.
      IF <f_attr>-is_public = 'X'.
        IF m_controller_data-definition-controller_type = wdyn_ctlr_type_view.
          MESSAGE e060(swdp_wdy_md_adt) WITH <f_attr>-cmpname INTO l_msg_str.
          me->raise_adt_exception_by_msg( ).
        ENDIF.
        l_compo-visibility = wdyn_visibility_public.
        l_compo-read_only = 'X'.
      ELSE.
        l_compo-visibility = wdyn_visibility_private.
        l_compo-read_only = space.
      ENDIF.
      "cmp_position ???
      l_compo-display_name = l_compo-cmpname.
      APPEND l_compo TO p_controller_data_md-controller_components.

      MOVE-CORRESPONDING <f_attr> TO l_compot.
      l_compot-component_name = m_controller_data-definition-component_name.
      l_compot-controller_name = m_controller_data-definition-controller_name.
      l_compot-langu = m_langu.
      APPEND l_compot TO p_controller_data_md-controller_component_texts.
    ENDLOOP.

  ENDMETHOD.


  METHOD convert_context_to_md.
    DATA: l_node      TYPE wdy_ctx_node,
          l_attr      TYPE wdy_ctx_attrib,
          l_map       TYPE wdy_ctx_mapping,
          l_domvalue  TYPE domvalue_l,
          l_msg_str   TYPE string.                          "#EC NEEDED
    FIELD-SYMBOLS: <f_node>        TYPE gty_s_context_node,
                   <f_parent_node> TYPE gty_s_context_node,
                   <f_attr>        TYPE gty_s_context_attribute,
                   <f_map>         TYPE gty_s_context_mapping.

    IF m_controller_data-definition-controller_type = wdyn_ctlr_type_intf_view AND
       NOT m_controller_data-context IS INITIAL.
      MESSAGE e504(swdp_wb_tool) WITH text-e04 text-e19 INTO l_msg_str.
      me->raise_adt_exception_by_msg( ).
    ENDIF.

* convert nodes and attributes
    LOOP AT m_controller_data-context ASSIGNING <f_node>.
*   elementary checks
      IF <f_node>-node_name IS INITIAL.
        MESSAGE e040(swdp_wdy_md_adt) INTO l_msg_str.
        me->raise_adt_exception_by_msg( ).
      ENDIF.
      IF <f_node>-node_name <> 'CONTEXT' AND <f_node>-parent_node_name IS INITIAL.
        MESSAGE e041(swdp_wdy_md_adt) WITH <f_node>-node_name INTO l_msg_str.
        me->raise_adt_exception_by_msg( ).
      ENDIF.
*   check no duplicates
      READ TABLE p_controller_data_md-context_nodes TRANSPORTING NO FIELDS
           WITH KEY node_name = <f_node>-node_name.
      IF sy-subrc = 0.
        MESSAGE e212(swdp_wb_tool) WITH <f_node>-node_name INTO l_msg_str.
        me->raise_adt_exception_by_msg( ).
      ENDIF.
*   parent node existency / is_intf_item
      IF <f_node>-node_name <> 'CONTEXT'.
        READ TABLE m_controller_data-context ASSIGNING <f_parent_node>
             WITH KEY node_name = <f_node>-parent_node_name.
        IF sy-subrc <> 0.
          MESSAGE e800(swdp_wb_tool) WITH <f_node>-parent_node_name <f_node>-node_name INTO l_msg_str.
          me->raise_adt_exception_by_msg( ).
        ELSE.
          IF ( <f_node>-is_intf_item = 'X' AND
              ( m_controller_key-controller_name <> wdyn_component_controller_name AND
                m_controller_key-controller_name <> wdyn_interface_controller_name ) ) OR
             ( <f_node>-is_intf_item = space AND
               m_controller_key-controller_name = wdyn_interface_controller_name ) OR
            ( <f_node>-is_intf_item <> <f_parent_node>-is_intf_item AND
              <f_node>-parent_node_name <> 'CONTEXT' ).
            MESSAGE e031(swdp_wdy_md_adt) WITH <f_node>-node_name INTO l_msg_str.
            me->raise_adt_exception_by_msg( ).
          ENDIF.
        ENDIF.

*   check domain values
        IF NOT <f_node>-cardinality IS INITIAL.
          l_domvalue = <f_node>-cardinality.
          IF cl_wdy_wb_vc_checker=>check_dom_value( p_value   = l_domvalue
                                                    p_domname = 'WDY_MD_CO_CARDINALITY' ) IS INITIAL.
            MESSAGE e059(swdp_wdy_md_adt)
              WITH <f_node>-cardinality 'WDY_MD_CO_CARDINALITY' text-005 <f_node>-node_name INTO l_msg_str.
            me->raise_adt_exception_by_msg( ).
          ENDIF.
        ENDIF.
        IF NOT <f_node>-selection IS INITIAL.
          l_domvalue = <f_node>-selection.
          IF cl_wdy_wb_vc_checker=>check_dom_value( p_value   = l_domvalue
                                                    p_domname = 'WDY_MD_SELECTION' ) IS INITIAL.
            MESSAGE e059(swdp_wdy_md_adt)
              WITH <f_node>-selection 'WDY_MD_SELECTION' text-005 <f_node>-node_name INTO l_msg_str.
            me->raise_adt_exception_by_msg( ).
          ENDIF.
        ENDIF.
      ENDIF.

      CLEAR l_node.
      MOVE-CORRESPONDING <f_node> TO l_node.
      l_node-component_name = m_controller_data-definition-component_name.
      l_node-controller_name = m_controller_data-definition-controller_name.
      "l_node-version = 'I'.
      TRANSLATE l_node-node_name TO UPPER CASE.
      IF <f_node>-is_recursion_node = 'X'.
        l_node-node_type = c_node_type_recursion.
        CLEAR l_node-collection_type.
      ELSE.
        l_node-node_type = c_node_type_value.
        l_node-collection_type = wdyn_ctxt_type_coll_list.  "unused in runtime, therefore not maintainable on UI
      ENDIF.
      l_node-display_name = l_node-node_name.
      APPEND l_node TO p_controller_data_md-context_nodes.

*   convert attributes
      LOOP AT <f_node>-attributes ASSIGNING <f_attr>.
*     elementary checks
        IF <f_attr>-attribute_name IS INITIAL.
          MESSAGE e050(swdp_wdy_md_adt) WITH <f_node>-node_name INTO l_msg_str.
          me->raise_adt_exception_by_msg( ).
        ENDIF.
        IF <f_attr>-abap_type IS INITIAL.
          MESSAGE e051(swdp_wdy_md_adt) WITH <f_attr>-attribute_name <f_node>-node_name INTO l_msg_str.
          me->raise_adt_exception_by_msg( ).
        ENDIF.
        IF NOT <f_attr>-abap_typing IS INITIAL.
          l_domvalue = <f_attr>-abap_typing.
          IF cl_wdy_wb_vc_checker=>check_dom_value( p_value   = l_domvalue
                                                    p_domname = 'WDY_MD_ABAP_TYPING_ENUM' ) IS INITIAL.
            MESSAGE e059(swdp_wdy_md_adt)
              WITH <f_attr>-abap_typing 'WDY_MD_ABAP_TYPING_ENUM' text-006 <f_attr>-attribute_name INTO l_msg_str.
            me->raise_adt_exception_by_msg( ).
          ENDIF.
        ENDIF.
        IF NOT <f_attr>-default_value IS INITIAL AND <f_attr>-abap_typing = wdyn_ctxt_type_typing_ref_to.
          MESSAGE e052(swdp_wdy_md_adt) WITH <f_node>-node_name <f_attr>-attribute_name INTO l_msg_str.
          me->raise_adt_exception_by_msg( ).
        ENDIF.

        CLEAR l_attr.
        MOVE-CORRESPONDING <f_attr> TO l_attr.
        l_attr-component_name = m_controller_data-definition-component_name.
        l_attr-controller_name = m_controller_data-definition-controller_name.
        l_attr-node_name = l_node-node_name.
        "l_attr-version = 'I'.
        TRANSLATE l_attr-attribute_name TO UPPER CASE.
        l_attr-attribute_type = c_attr_type_value.
        l_attr-display_name = l_attr-attribute_name.
        me->convert_ctx_help_to_md( EXPORTING p_attribute    = <f_attr>
                                    CHANGING  p_md_attribute = l_attr ).
        me->check_format_properties( EXPORTING p_attribute = l_attr ).
        APPEND l_attr TO p_controller_data_md-context_attributes.
      ENDLOOP.
    ENDLOOP.

* check consistency of position field content for nodes and attributes
* (this is done after the conversion to await the result of the elementary checks)
    me->check_node_positions( p_parent_node_name = 'CONTEXT' ).

* convert mapping
    IF ( m_controller_data-definition-controller_type = wdyn_ctlr_type_intf_view OR
         m_controller_data-definition-controller_type = wdyn_ctlr_type_cmp_intf ) AND
       NOT m_controller_data-context_mapping IS INITIAL.
      MESSAGE e504(swdp_wb_tool) WITH text-e04 text-e11 INTO l_msg_str.
      me->raise_adt_exception_by_msg( ).
    ENDIF.

    LOOP AT m_controller_data-context_mapping ASSIGNING <f_map>.
*   elementary checks
      IF <f_map>-ctx_mapp_name IS INITIAL.   "check that ctx_mapp_name is set (guid)
        MESSAGE e504(swdp_wb_tool) WITH text-e02 INTO l_msg_str.
        me->raise_adt_exception_by_msg( ).
      ENDIF.
      IF <f_map>-ctlr_usage_name IS INITIAL.
        MESSAGE e048(swdp_wdy_md_adt) WITH <f_map>-own_node_name INTO l_msg_str.
        me->raise_adt_exception_by_msg( ).
      ENDIF.
      IF <f_map>-map_node_name IS INITIAL.
        MESSAGE e055(swdp_wdy_md_adt) WITH <f_map>-own_node_name INTO l_msg_str.
        me->raise_adt_exception_by_msg( ).
      ENDIF.
*   check node existency
      READ TABLE m_controller_data-context TRANSPORTING NO FIELDS
           WITH KEY node_name = <f_map>-own_node_name.      "#EC WARNOK
      IF sy-subrc <> 0.
        MESSAGE e543(swdp_wb_tool) WITH <f_map>-own_node_name INTO l_msg_str.
        me->raise_adt_exception_by_msg( ).
      ENDIF.

      CLEAR l_map.
      MOVE-CORRESPONDING <f_map> TO l_map.
      l_map-component_name = m_controller_data-definition-component_name.
      l_map-controller_name = m_controller_data-definition-controller_name.
      "l_map-version = 'I'.
      l_map-mapping_type = wdyn_ctxt_type_map_type_co_se.
      APPEND l_map TO p_controller_data_md-context_mappings.
    ENDLOOP.

  ENDMETHOD.


  METHOD convert_controller_data_to_md.
    DATA: l_decription   TYPE wdy_controllert,
          l_method_impls TYPE saboo_method_impl_tab.

    ASSERT NOT m_controller_data IS INITIAL.

    IF NOT m_implementation IS INITIAL.
      me->split_implementation( IMPORTING p_method_impls = l_method_impls ).
      me->check_missing_method_defs( EXPORTING p_method_impls = l_method_impls ).
      "else.   the coding is passed in the code bodies
    ENDIF.

* definition
    MOVE-CORRESPONDING m_controller_data-definition TO p_controller_data_md-definition.
    "p_controller_data_md-definition-version = 'I'.
    p_controller_data_md-definition-display_name = m_controller_data-definition-controller_name.
    p_controller_data_md-definition-context = 'CONTEXT'.
* short description
    l_decription-component_name = m_controller_data-definition-component_name.
    l_decription-controller_name = m_controller_data-definition-controller_name.
    l_decription-langu = m_langu.
    l_decription-description = m_controller_data-definition-description.
    APPEND l_decription TO p_controller_data_md-descriptions.

* controller_usages
    me->convert_ctlr_usage_to_md( CHANGING p_controller_data_md = p_controller_data_md ).

* controller_components (events, actions, methods, ...)
    me->convert_attributes_to_md( CHANGING p_controller_data_md = p_controller_data_md ).

    me->convert_events_to_md( CHANGING p_controller_data_md = p_controller_data_md ).

    me->convert_actions_to_md( CHANGING p_controller_data_md = p_controller_data_md ).

    me->convert_methods_to_md( EXPORTING p_method_impls      = l_method_impls
                               CHANGING  p_controller_data_md = p_controller_data_md ).

    me->convert_supply_funcs_to_md( EXPORTING p_method_impls      = l_method_impls
                                    CHANGING  p_controller_data_md = p_controller_data_md ).

    me->convert_event_handler_to_md( EXPORTING p_method_impls      = l_method_impls
                                     CHANGING  p_controller_data_md = p_controller_data_md ).
* context
    me->convert_context_to_md( CHANGING p_controller_data_md = p_controller_data_md ).

  ENDMETHOD.


  METHOD convert_ctlr_usage_to_md.
    DATA: l_usage     TYPE wdy_ctlr_usage,
          l_used_comp TYPE wdy_component_name,
          l_comp_type TYPE wdy_md_component_type,
          l_msg_str   TYPE string.                          "#EC NEEDED
    FIELD-SYMBOLS: <f_usage> TYPE gty_s_controller_usage.

    IF ( m_controller_data-definition-controller_type = wdyn_ctlr_type_intf_view OR
         m_controller_data-definition-controller_type = wdyn_ctlr_type_cmp_intf ) AND
       NOT m_controller_data-controller_usages IS INITIAL.
      MESSAGE e504(swdp_wb_tool) WITH text-e04 text-e07 INTO l_msg_str.
      me->raise_adt_exception_by_msg( ).
    ENDIF.

    LOOP AT m_controller_data-controller_usages ASSIGNING <f_usage>.
*   check ctlr_usage_name is set (GUID)
      IF <f_usage>-ctlr_usage_name IS INITIAL.
        MESSAGE e504(swdp_wb_tool) WITH text-e01 INTO l_msg_str.
        me->raise_adt_exception_by_msg( ).
      ENDIF.
      IF <f_usage>-component_usage IS INITIAL AND <f_usage>-used_controller IS INITIAL.
        MESSAGE e504(swdp_wb_tool) WITH text-e16 INTO l_msg_str.
        me->raise_adt_exception_by_msg( ).
      ENDIF.
      CLEAR l_usage.
      MOVE-CORRESPONDING <f_usage> TO l_usage.
      l_usage-component_name = m_controller_data-definition-component_name.
      l_usage-controller_name = m_controller_data-definition-controller_name.
      "l_usage-version = 'I'.
      IF NOT l_usage-component_usage IS INITIAL AND
         l_usage-used_controller = wdyn_interface_controller_name.
        SELECT SINGLE used_component FROM wdy_compo_usage INTO l_used_comp
          WHERE component_name   = l_usage-component_name AND
                compo_usage_name = l_usage-component_usage.
        IF sy-subrc = 0 AND NOT l_used_comp IS INITIAL.
          SELECT SINGLE type FROM wdy_component INTO l_comp_type
            WHERE component_name = l_used_comp.         "#EC CI_GENBUFF
          IF sy-subrc = 0 AND l_comp_type IS INITIAL.  "Component, not interface
            l_usage-used_controller = wdyn_component_controller_name.
          ENDIF.
        ENDIF.
      ENDIF.
      APPEND l_usage TO p_controller_data_md-controller_usages.
    ENDLOOP.

  ENDMETHOD.


  METHOD convert_ctx_help_to_extern.
    DATA: l_attrib TYPE wdy_vc_co_node.

    MOVE-CORRESPONDING p_md_attribute TO l_attrib.

    cl_wdy_wb_context_utilities=>convert_f4_to_extern(
      EXPORTING
        context_node        = l_attrib
      IMPORTING
        ctx_value_help_id   = p_attribute-value_help_id
        ctx_value_help_mode = p_attribute-value_help_mode
        ctx_value_help_type = p_attribute-value_help_type_text ).

  ENDMETHOD.


  METHOD convert_ctx_help_to_md.

    cl_wdy_wb_context_utilities=>convert_f4_to_intern(
      EXPORTING
        ctx_value_help_id   = p_attribute-value_help_id
        ctx_value_help_mode = p_attribute-value_help_mode
      IMPORTING
        api_value_help_id   = p_md_attribute-value_help_id
        api_value_help_mode = p_md_attribute-value_help_mode ).

  ENDMETHOD.


  METHOD convert_events_to_md.
    DATA: l_compo     TYPE wdy_ctlr_compo,
          l_compot    TYPE wdy_ctlr_compot,
          l_msg_str   TYPE string,                          "#EC NEEDED
          l_name      TYPE wdy_md_object_name.
    FIELD-SYMBOLS: <f_event>  TYPE gty_s_event.

    IF ( m_controller_data-definition-controller_type = wdyn_ctlr_type_view OR
         m_controller_data-definition-controller_type = wdyn_ctlr_type_intf_view ) AND
       NOT m_controller_data-events IS INITIAL.
      MESSAGE e504(swdp_wb_tool) WITH text-e04 text-e08 INTO l_msg_str.
      me->raise_adt_exception_by_msg( ).
    ENDIF.

    LOOP AT m_controller_data-events ASSIGNING <f_event>.
*   elementary checks
      IF <f_event>-cmpname IS INITIAL.
        MESSAGE e042(swdp_wdy_md_adt) INTO l_msg_str.
        me->raise_adt_exception_by_msg( ).
      ENDIF.

*   check compo name is unique
      READ TABLE p_controller_data_md-controller_components TRANSPORTING NO FIELDS
           WITH KEY cmpname = <f_event>-cmpname.
      IF sy-subrc = 0.
        MESSAGE e605(swdp_wb_tool) WITH <f_event>-cmpname m_controller_data-definition-controller_name INTO l_msg_str.
        me->raise_adt_exception_by_msg( ).
      ENDIF.
*   check name
      l_name = <f_event>-cmpname.
      cl_wdy_wb_checker=>check_component_name( EXPORTING p_name = l_name
                                                         p_is_proc = ' '
                                               EXCEPTIONS OTHERS = 1 ).
      IF sy-subrc <> 0.
        me->raise_adt_exception_by_msg( ).
      ENDIF.

*   is_intf_item
      IF ( <f_event>-is_intf_item = 'X' AND
          ( m_controller_key-controller_name <> wdyn_component_controller_name AND
            m_controller_key-controller_name <> wdyn_interface_controller_name ) ) OR
         ( <f_event>-is_intf_item = space AND
            m_controller_key-controller_name = wdyn_interface_controller_name ).
        MESSAGE e032(swdp_wdy_md_adt) WITH  <f_event>-cmpname INTO l_msg_str.
        me->raise_adt_exception_by_msg( ).
      ENDIF.

      CLEAR: l_compo, l_compot.
      MOVE-CORRESPONDING <f_event> TO l_compo.
      l_compo-component_name = m_controller_data-definition-component_name.
      l_compo-controller_name = m_controller_data-definition-controller_name.
      TRANSLATE l_compo-cmpname TO UPPER CASE.
      "l_compo-version = 'I'.
      l_compo-cmptype = wdyn_ctlr_compo_event.
      "cmp_position ???
      l_compo-display_name = l_compo-cmpname.
      APPEND l_compo TO p_controller_data_md-controller_components.

      MOVE-CORRESPONDING <f_event> TO l_compot.
      l_compot-component_name = m_controller_data-definition-component_name.
      l_compot-controller_name = m_controller_data-definition-controller_name.
      l_compot-langu = m_langu.
      APPEND l_compot TO p_controller_data_md-controller_component_texts.

      SORT <f_event>-parameters BY parameter_name.
      me->convert_parameter_to_md(
           EXPORTING p_cmpname    = <f_event>-cmpname
                     p_parameters = <f_event>-parameters
           CHANGING  p_parameters_md = p_controller_data_md-controller_parameters
                     p_parameterst_md = p_controller_data_md-controller_parameter_texts ).
    ENDLOOP.

  ENDMETHOD.


  METHOD convert_event_handler_to_md.
    DATA: l_compo     TYPE wdy_ctlr_compo,
          l_compot    TYPE wdy_ctlr_compot,
          l_name      TYPE wdy_md_object_name,
          l_msg_str   TYPE string.                          "#EC NEEDED
    DATA: l_cust_evh  TYPE abap_bool,
          l_comp_type TYPE wdy_md_component_type.
    FIELD-SYMBOLS: <f_evhan>  TYPE gty_s_event_handler.

    LOOP AT m_controller_data-event_handler ASSIGNING <f_evhan>.
      clear l_cust_evh.
*   elementary checks
      IF <f_evhan>-cmpname IS INITIAL.
        MESSAGE e043(swdp_wdy_md_adt) INTO l_msg_str.
        me->raise_adt_exception_by_msg( ).
      ENDIF.

*   check compo name is unique
      READ TABLE p_controller_data_md-controller_components TRANSPORTING NO FIELDS
           WITH KEY cmpname = <f_evhan>-cmpname.
      IF sy-subrc = 0.
        MESSAGE e605(swdp_wb_tool)
          WITH <f_evhan>-cmpname m_controller_data-definition-controller_name INTO l_msg_str.
        me->raise_adt_exception_by_msg( ).
      ENDIF.
*   check handler for action/inplug
      IF cl_wdy_wb_method_ed=>is_action_method_name( <f_evhan>-cmpname ) = 'X'.
        l_name = <f_evhan>-cmpname+8.
        READ TABLE m_controller_data-actions TRANSPORTING NO FIELDS WITH KEY cmpname = l_name. "#EC WARNOK
        IF sy-subrc <> 0 OR (   "action does not exist
           NOT <f_evhan>-event_source IS INITIAL OR   "inconsistent eventhandler data for action
           " The below line is commented because copy of a view while saving passes through this logic
           " hence the reference name and definition name will be different as the copy source will be different
           " the destination. Customer CSN: 002075129500001285522018
*           <f_evhan>-ref_ctlr_name <> m_controller_data-definition-controller_name OR
           <f_evhan>-ref_cmpname   <> l_name OR
           NOT <f_evhan>-ref_view_name IS INITIAL OR
           NOT <f_evhan>-ref_plug_name IS INITIAL ).
          MESSAGE e504(swdp_wb_tool) WITH text-e04 text-003 <f_evhan>-cmpname INTO l_msg_str.
          me->raise_adt_exception_by_msg( ).
        ENDIF.
      ELSEIF cl_wdy_wb_method_ed=>is_plug_method_name( <f_evhan>-cmpname ) = 'X'.
        "existency cannot be checked here
        l_name = <f_evhan>-cmpname+6.
        IF NOT <f_evhan>-event_source  IS INITIAL OR   "inconsistent eventhandler data for plug
           NOT <f_evhan>-ref_component IS INITIAL OR
           NOT <f_evhan>-ref_ctlr_name IS INITIAL OR
           NOT <f_evhan>-ref_cmpname   IS INITIAL OR
            <f_evhan>-ref_view_name IS INITIAL OR
           " The below lines are commented because copy of a view while saving passes through this logic
           " hence the reference name and definition name will be different as the copy source will be different
           " the destination. Customer CSN: 002075129500001285522018
*           <f_evhan>-ref_view_name <> m_controller_data-definition-controller_name OR
*           <f_evhan>-ref_ctlr_name <> m_controller_data-definition-controller_name OR
           <f_evhan>-ref_plug_name <> l_name.
          MESSAGE e504(swdp_wb_tool) WITH text-e04 text-003 <f_evhan>-cmpname INTO l_msg_str.
          me->raise_adt_exception_by_msg( ).
        ENDIF.
      ELSE. "custom eventhandler
        l_cust_evh = abap_true.
        cl_wdy_wb_checker=>check_component_name( EXPORTING p_name = <f_evhan>-cmpname
                                                           p_is_proc = 'X'
                                                 EXCEPTIONS OTHERS = 1 ).
        IF sy-subrc <> 0.
          me->raise_adt_exception_by_msg( ).
        ENDIF.

*     is_intf_item
        IF ( <f_evhan>-is_intf_item = 'X' AND
            ( m_controller_key-controller_name <> wdyn_component_controller_name AND
              m_controller_key-controller_name <> wdyn_interface_controller_name ) ) OR
           ( <f_evhan>-is_intf_item = space AND
              m_controller_key-controller_name = wdyn_interface_controller_name ).
          MESSAGE e032(swdp_wdy_md_adt) WITH  <f_evhan>-cmpname INTO l_msg_str.
          me->raise_adt_exception_by_msg( ).
        ENDIF.


        IF NOT <f_evhan>-ref_view_name IS INITIAL OR
           NOT <f_evhan>-ref_plug_name IS INITIAL.
          MESSAGE e504(swdp_wb_tool) WITH text-e04 text-003 <f_evhan>-cmpname INTO l_msg_str.
          me->raise_adt_exception_by_msg( ).
        ENDIF.

*     event_source
        IF NOT <f_evhan>-ref_component IS INITIAL AND
           <f_evhan>-ref_component <> m_controller_data-definition-component_name AND  "foreign component
           <f_evhan>-event_source IS INITIAL.
          MESSAGE e504(swdp_wb_tool) WITH text-e04 text-003 <f_evhan>-cmpname INTO l_msg_str.
          me->raise_adt_exception_by_msg( ).
        ENDIF.
      ENDIF.

      CLEAR: l_compo, l_compot.
      MOVE-CORRESPONDING <f_evhan> TO l_compo.
      l_compo-component_name = m_controller_data-definition-component_name.
      l_compo-controller_name = m_controller_data-definition-controller_name.
      TRANSLATE l_compo-cmpname TO UPPER CASE.
      l_compo-cmptype = wdyn_ctlr_compo_evhandler.
      l_compo-visibility = wdyn_visibility_public.

      IF l_cust_evh = abap_true and NOT l_compo-ref_component IS INITIAL AND
         l_compo-ref_ctlr_name = wdyn_interface_controller_name.
        IF l_compo-ref_component <> m_controller_data-definition-component_name.
          SELECT SINGLE type FROM wdy_component INTO l_comp_type
            WHERE component_name = l_compo-ref_component AND version = 'A'.
          IF sy-subrc = 0 AND l_comp_type IS INITIAL.  "Component, not interface
            l_compo-ref_ctlr_name = wdyn_component_controller_name.
          ENDIF.
        ELSE. "Definitely a component
            l_compo-ref_ctlr_name = wdyn_component_controller_name.
        ENDIF.
      ENDIF.

      me->modify_code_body( EXPORTING p_method_name = l_compo-cmpname
                                      p_method_impls = p_method_impls
                            CHANGING  p_code_body = l_compo-code_body ).
      l_compo-display_name = l_compo-cmpname.
      APPEND l_compo TO p_controller_data_md-controller_components.

      MOVE-CORRESPONDING <f_evhan> TO l_compot.
      l_compot-component_name = m_controller_data-definition-component_name.
      l_compot-controller_name = m_controller_data-definition-controller_name.
      l_compot-langu = m_langu.
      APPEND l_compot TO p_controller_data_md-controller_component_texts.

      SORT <f_evhan>-parameters BY parameter_name.
      me->convert_parameter_to_md(
           EXPORTING p_cmpname    = <f_evhan>-cmpname
                     p_parameters = <f_evhan>-parameters
           CHANGING  p_parameters_md  = p_controller_data_md-controller_parameters
                     p_parameterst_md = p_controller_data_md-controller_parameter_texts ).
    ENDLOOP.

  ENDMETHOD.


  METHOD convert_exception_to_md.
    DATA: l_exception  TYPE wdy_ctlr_exc,
          l_exceptiont TYPE wdy_ctlr_exct,
          l_msg_str    TYPE string.                         "#EC NEEDED
    FIELD-SYMBOLS: <f_exception> TYPE gty_s_exception.

    l_exception-component_name = m_controller_data-definition-component_name.
    l_exception-controller_name = m_controller_data-definition-controller_name.
    l_exception-method_name = p_cmpname.
    "l_exception-version = 'I'.
    MOVE-CORRESPONDING l_exception TO l_exceptiont.
    l_exceptiont-langu = m_langu.

    LOOP AT p_exceptions ASSIGNING <f_exception>.
      IF <f_exception>-exception_name IS INITIAL.
        MESSAGE e044(swdp_wdy_md_adt) INTO l_msg_str.
        me->raise_adt_exception_by_msg( ).
      ENDIF.

*   check exception name is unique
      READ TABLE p_exceptions_md TRANSPORTING NO FIELDS
           WITH KEY method_name = p_cmpname
                    exception_name = <f_exception>-exception_name.
      IF sy-subrc = 0.
        MESSAGE e480(swdp_wb_tool) WITH <f_exception>-exception_name INTO l_msg_str.
        me->raise_adt_exception_by_msg( ).
      ENDIF.

      l_exception-exception_name = <f_exception>-exception_name.
      TRANSLATE l_exception-exception_name TO UPPER CASE.
      APPEND l_exception TO p_exceptions_md.

      l_exceptiont-exception_name = l_exception-exception_name.
      l_exceptiont-description = <f_exception>-description.
      APPEND l_exceptiont TO p_exceptionst_md.
    ENDLOOP.
  ENDMETHOD.


  METHOD convert_methods_to_md.
    DATA: l_compo     TYPE wdy_ctlr_compo,
          l_compot    TYPE wdy_ctlr_compot,
          l_msg_str   TYPE string.                          "#EC NEEDED
    FIELD-SYMBOLS: <f_method>  TYPE gty_s_method.

    IF m_controller_data-definition-controller_type = wdyn_ctlr_type_intf_view AND
       NOT m_controller_data-methods IS INITIAL.
      MESSAGE e504(swdp_wb_tool) WITH text-e04 text-e10 INTO l_msg_str.
      me->raise_adt_exception_by_msg( ).
    ENDIF.

    LOOP AT m_controller_data-methods ASSIGNING <f_method>.
*   elementary checks
      IF <f_method>-cmpname IS INITIAL.
        MESSAGE e043(swdp_wdy_md_adt) INTO l_msg_str.
        me->raise_adt_exception_by_msg( ).
      ENDIF.
*   check compo name is unique
      READ TABLE p_controller_data_md-controller_components TRANSPORTING NO FIELDS
           WITH KEY cmpname = <f_method>-cmpname.
      IF sy-subrc = 0.
        MESSAGE e605(swdp_wb_tool) WITH <f_method>-cmpname m_controller_data-definition-controller_name INTO l_msg_str.
        me->raise_adt_exception_by_msg( ).
      ENDIF.

*   check name
      IF cl_wdy_wb_vc_proc_support=>is_predef_method( <f_method>-cmpname ) = space.
        cl_wdy_wb_checker=>check_component_name( EXPORTING p_name = <f_method>-cmpname
                                                           p_is_proc = 'X'
                                                 EXCEPTIONS OTHERS = 1 ).
        IF sy-subrc <> 0.
          me->raise_adt_exception_by_msg( ).
        ENDIF.

*   is_intf_item
        IF ( <f_method>-is_intf_item = 'X' AND
            ( m_controller_key-controller_name <> wdyn_component_controller_name AND
              m_controller_key-controller_name <> wdyn_interface_controller_name ) ) OR
           ( <f_method>-is_intf_item = space AND
              m_controller_key-controller_name = wdyn_interface_controller_name ).
          MESSAGE e032(swdp_wdy_md_adt) WITH  <f_method>-cmpname INTO l_msg_str.
          me->raise_adt_exception_by_msg( ).
        ENDIF.

      ELSE.  "predef method
        IF <f_method>-is_intf_item = 'X'.
          MESSAGE e032(swdp_wdy_md_adt) WITH  <f_method>-cmpname INTO l_msg_str.
          me->raise_adt_exception_by_msg( ).
        ENDIF.
      ENDIF.

      CLEAR: l_compo, l_compot.
      MOVE-CORRESPONDING <f_method> TO l_compo.
      l_compo-component_name = m_controller_data-definition-component_name.
      l_compo-controller_name = m_controller_data-definition-controller_name.
      TRANSLATE l_compo-cmpname TO UPPER CASE.
      "l_compo-version = 'I'.
      l_compo-cmptype = wdyn_ctlr_compo_method.
      "cmp_position ???
      l_compo-visibility = wdyn_visibility_public.
      me->modify_code_body( EXPORTING p_method_name = l_compo-cmpname
                                      p_method_impls = p_method_impls
                            CHANGING  p_code_body = l_compo-code_body ).
      IF l_compo-code_body IS INITIAL OR l_compo-code_body = space.
        MESSAGE e034(swdp_wdy_md_adt) WITH  <f_method>-cmpname INTO l_msg_str.
        me->raise_adt_exception_by_msg( ).
      ENDIF.
      l_compo-display_name = l_compo-cmpname.
      APPEND l_compo TO p_controller_data_md-controller_components.

      MOVE-CORRESPONDING <f_method> TO l_compot.
      l_compot-component_name = m_controller_data-definition-component_name.
      l_compot-controller_name = m_controller_data-definition-controller_name.
      l_compot-langu = m_langu.
      APPEND l_compot TO p_controller_data_md-controller_component_texts.

      SORT <f_method>-parameters BY parameter_name.
      me->convert_parameter_to_md(
           EXPORTING p_cmpname    = <f_method>-cmpname
                     p_parameters = <f_method>-parameters
           CHANGING  p_parameters_md  = p_controller_data_md-controller_parameters
                     p_parameterst_md = p_controller_data_md-controller_parameter_texts ).

      me->convert_exception_to_md(
           EXPORTING p_cmpname    = <f_method>-cmpname
                     p_exceptions = <f_method>-exceptions
           CHANGING  p_exceptions_md  = p_controller_data_md-controller_exceptions
                     p_exceptionst_md = p_controller_data_md-controller_exception_texts ).
    ENDLOOP.

  ENDMETHOD.


  METHOD convert_parameter_to_md.
    DATA: l_param      TYPE wdy_ctlr_param,
          l_paramt     TYPE wdy_ctlr_paramt,
          l_domvalue   TYPE domvalue_l,
          l_pos        TYPE i,
          l_msg_str    TYPE string.                         "#EC NEEDED
    FIELD-SYMBOLS: <f_param> TYPE gty_s_parameter.

    l_param-component_name = m_controller_data-definition-component_name.
    l_param-controller_name = m_controller_data-definition-controller_name.
    l_param-cmpname = p_cmpname.
    "l_param-version = 'I'.
    l_paramt-component_name = m_controller_data-definition-component_name.
    l_paramt-controller_name = m_controller_data-definition-controller_name.
    l_paramt-cmpname = p_cmpname.
    l_paramt-langu = m_langu.

    LOOP AT p_parameters ASSIGNING <f_param>.
      l_pos = sy-tabix.
*   elementary checks
      IF <f_param>-parameter_name IS INITIAL.
        MESSAGE e037(swdp_wdy_md_adt) WITH p_cmpname INTO l_msg_str.
        me->raise_adt_exception_by_msg( ).
      ENDIF.
      IF <f_param>-parameter_name(2) = 'WD'.
        MESSAGE e027(swdp_wb_tool) WITH <f_param>-parameter_name 'WD' INTO l_msg_str.
        me->raise_adt_exception_by_msg( ).
      ENDIF.
      IF <f_param>-abap_type IS INITIAL.
        MESSAGE e036(swdp_wdy_md_adt) WITH p_cmpname <f_param>-parameter_name INTO l_msg_str.
        me->raise_adt_exception_by_msg( ).
      ENDIF.
      IF NOT <f_param>-abap_typing IS INITIAL.
        l_domvalue = <f_param>-abap_typing.
        IF cl_wdy_wb_vc_checker=>check_dom_value( p_value   = l_domvalue
                                                  p_domname = 'WDY_MD_ABAP_TYPING_ENUM' ) IS INITIAL.
          MESSAGE e059(swdp_wdy_md_adt)
            WITH <f_param>-abap_typing 'WDY_MD_ABAP_TYPING_ENUM' text-007 <f_param>-parameter_name INTO l_msg_str.
          me->raise_adt_exception_by_msg( ).
        ENDIF.
      ENDIF.
      IF NOT <f_param>-declaration_type IS INITIAL.
        l_domvalue = <f_param>-declaration_type.
        IF cl_wdy_wb_vc_checker=>check_dom_value( p_value   = l_domvalue
                                                  p_domname = 'WDY_MD_DECLARATION_TYPE_ENUM' ) IS INITIAL.
          MESSAGE e059(swdp_wdy_md_adt)
            WITH <f_param>-declaration_type 'WDY_MD_DECLARATION_TYPE_ENUM' text-007 <f_param>-parameter_name INTO l_msg_str.
          me->raise_adt_exception_by_msg( ).
        ENDIF.
      ENDIF.
      IF ( <f_param>-optional = 'X' OR NOT <f_param>-default_value IS INITIAL ) AND
         ( <f_param>-declaration_type <> wdyn_declare_type_importing AND <f_param>-declaration_type <> wdyn_declare_type_changing ).
        MESSAGE e029(swdp_wdy_md_adt) WITH <f_param>-parameter_name INTO l_msg_str.
        me->raise_adt_exception_by_msg( ).
      ENDIF.

*   check parameter name is unique
      READ TABLE p_parameters_md TRANSPORTING NO FIELDS
           WITH KEY cmpname = p_cmpname
                    parameter_name = <f_param>-parameter_name.
      IF sy-subrc = 0.
        MESSAGE e016(swdp_wb_tool) WITH <f_param>-parameter_name INTO l_msg_str.
        me->raise_adt_exception_by_msg( ).
      ENDIF.

      MOVE-CORRESPONDING <f_param> TO l_param.
      TRANSLATE l_param-parameter_name TO UPPER CASE.
      l_param-display_name = l_param-parameter_name.
      l_param-param_position = l_pos.
      APPEND l_param TO p_parameters_md.

      l_paramt-parameter_name = l_param-parameter_name.
      l_paramt-description = <f_param>-description.
      APPEND l_paramt TO p_parameterst_md.
    ENDLOOP.

  ENDMETHOD.


  METHOD convert_supply_funcs_to_md.
    DATA: l_compo     TYPE wdy_ctlr_compo,
          l_compot    TYPE wdy_ctlr_compot,
          l_msg_str   TYPE string.                          "#EC NEEDED
    FIELD-SYMBOLS: <f_supply_func>  TYPE gty_s_supply_function.

    IF ( m_controller_data-definition-controller_type = wdyn_ctlr_type_intf_view OR
         m_controller_data-definition-controller_type = wdyn_ctlr_type_cmp_intf ) AND
       NOT m_controller_data-supply_functions IS INITIAL.
      MESSAGE e504(swdp_wb_tool) WITH text-e04 text-e09 INTO l_msg_str.
      me->raise_adt_exception_by_msg( ).
    ENDIF.

    LOOP AT m_controller_data-supply_functions ASSIGNING <f_supply_func>.
*   elementary checks
      IF <f_supply_func>-cmpname IS INITIAL.
        MESSAGE e043(swdp_wdy_md_adt) INTO l_msg_str.
        me->raise_adt_exception_by_msg( ).
      ENDIF.
*   check compo name is unique
      READ TABLE p_controller_data_md-controller_components TRANSPORTING NO FIELDS
           WITH KEY cmpname = <f_supply_func>-cmpname.
      IF sy-subrc = 0.
        MESSAGE e605(swdp_wb_tool) WITH <f_supply_func>-cmpname m_controller_data-definition-controller_name INTO l_msg_str.
        me->raise_adt_exception_by_msg( ).
      ENDIF.
*   check name
      cl_wdy_wb_checker=>check_component_name( EXPORTING p_name = <f_supply_func>-cmpname
                                                         p_is_proc = 'X'
                                               EXCEPTIONS OTHERS = 1 ).
      IF sy-subrc <> 0.
        me->raise_adt_exception_by_msg( ).
      ENDIF.

      CLEAR: l_compo, l_compot.
      MOVE-CORRESPONDING <f_supply_func> TO l_compo.
      l_compo-component_name = m_controller_data-definition-component_name.
      l_compo-controller_name = m_controller_data-definition-controller_name.
      TRANSLATE l_compo-cmpname TO UPPER CASE.
      "l_compo-version = 'I'.
      l_compo-cmptype = wdyn_ctlr_compo_supplyfunc.
      l_compo-visibility = wdyn_visibility_public.
      "cmp_position ???
      me->modify_code_body( EXPORTING p_method_name = l_compo-cmpname
                                      p_method_impls = p_method_impls
                            CHANGING  p_code_body = l_compo-code_body ).
      l_compo-display_name = l_compo-cmpname.
      APPEND l_compo TO p_controller_data_md-controller_components.

      MOVE-CORRESPONDING <f_supply_func> TO l_compot.
      l_compot-component_name = m_controller_data-definition-component_name.
      l_compot-controller_name = m_controller_data-definition-controller_name.
      l_compot-langu = m_langu.
      APPEND l_compot TO p_controller_data_md-controller_component_texts.

      SORT <f_supply_func>-parameters BY parameter_name.
      me->convert_parameter_to_md(
           EXPORTING p_cmpname    = <f_supply_func>-cmpname
                     p_parameters = <f_supply_func>-parameters
           CHANGING  p_parameters_md  = p_controller_data_md-controller_parameters
                     p_parameterst_md = p_controller_data_md-controller_parameter_texts ).
    ENDLOOP.

  ENDMETHOD.


  METHOD create.
    DATA: l_controller_key TYPE wdy_controller_key.

    l_controller_key-component_name = component_name.
    l_controller_key-controller_name = controller_name.
    TRANSLATE l_controller_key TO UPPER CASE.

    create_internal( p_controller_key  = l_controller_key
                     p_controller_type = wdyn_ctlr_type_custom
                     p_description     = description ).

    CREATE OBJECT result TYPE zcl_hrpy_utility_abaptogit_wdy
      EXPORTING
        p_controller_key = l_controller_key
        p_version        = 'I'
        p_langu          = sy-langu.

  ENDMETHOD.


  METHOD create_internal.
    DATA: l_md_controller  TYPE REF TO if_wdy_md_controller,
          l_md_view        TYPE REF TO if_wdy_md_abstract_view,
          l_msg_str        TYPE string,                     "#EC NEEDED
          l_masterl        TYPE langu,
          l_md_excp        TYPE REF TO cx_wdy_md_exception.

    IF p_for_new_component IS INITIAL.
*   component/interface existency
      IF p_controller_type = wdyn_ctlr_type_intf_view.
        IF cl_wdy_md_component_intf_def=>check_existency( name = p_controller_key-component_name ) IS INITIAL.
          MESSAGE e187(swdp_wb_tool) WITH p_controller_key-component_name INTO l_msg_str.
          raise_adt_exception_by_msg( ).
        ENDIF.
      ELSE.
        IF cl_wdy_md_component=>check_existency( name = p_controller_key-component_name ) IS INITIAL.
          MESSAGE e001(swdp_wb_tool) WITH p_controller_key-component_name INTO l_msg_str.
          raise_adt_exception_by_msg( ).
        ENDIF.
*   generation limits
        cl_wdy_wb_checker=>check_limits(
          EXPORTING
            p_compname    = p_controller_key-component_name
          EXCEPTIONS
            maxl_reached  = 1
            maxh_reached  = 2
            limit_reached = 3 ).
        IF sy-subrc >= 2.
          MESSAGE e277(swdp_wb_tool) INTO l_msg_str.
          raise_adt_exception_by_msg( ).
        ENDIF.
      ENDIF.
    ENDIF.

* language check
    SELECT SINGLE masterlang FROM tadir INTO l_masterl
      WHERE pgmid    = 'R3TR' AND
            object   = 'WDYN' AND
            obj_name = p_controller_key-component_name.
    IF l_masterl <> sy-langu.
      MESSAGE e504(swdp_wb_tool) WITH text-e14 INTO l_msg_str.
      raise_adt_exception_by_msg( ).
    ENDIF.

* controller name
    IF strlen( p_controller_key-controller_name ) > 20.
      MESSAGE e504(swdp_wb_tool) WITH text-e17 INTO l_msg_str.
      raise_adt_exception_by_msg( ).
    ENDIF.
    cl_wdy_wb_checker=>check_lockable_object_name(
          EXPORTING  p_name = p_controller_key-controller_name
                     p_is_subobject = 'X'
          EXCEPTIONS invalid_name = 1 ).
    IF sy-subrc <> 0.
      raise_adt_exception_by_msg( ).
    ELSEIF p_controller_key-controller_name = p_controller_key-component_name AND
         ( p_controller_type <> wdyn_ctlr_type_window AND p_controller_type <> wdyn_ctlr_type_intf_view ).
      MESSAGE e005(swdp_wb_tool) WITH text-001 INTO l_msg_str.
      raise_adt_exception_by_msg( ).
    ELSEIF p_controller_key-controller_name = wdyvc_emptyview_name.
      MESSAGE e113(swdp_wb_tool) WITH wdyvc_emptyview_name INTO l_msg_str.
      raise_adt_exception_by_msg( ).
    ENDIF.

* controller existency
    IF cl_wdy_md_controller=>check_existency(
       component_name  = p_controller_key-component_name
       controller_name = p_controller_key-controller_name ) = abap_true.
      RAISE EXCEPTION TYPE cx_wdy_md_already_existing
        EXPORTING
          textid = cx_wdy_md_already_existing=>controller_already_existing.
    ENDIF.
* check that controller does not exists original in enhancement
    IF cl_wdy_enh_helper=>get_orig_enh_of_controller( p_controller_key ) = abap_true.
      MESSAGE e010(swdp_wb_tool) WITH p_controller_key-controller_name INTO l_msg_str.
      raise_adt_exception_by_msg( ).
    ENDIF.

    TRY.
        CASE p_controller_type.
          WHEN wdyn_ctlr_type_view.
            l_md_view = cl_wdy_md_view=>create_complete(
              component_name = p_controller_key-component_name
              view_name      = p_controller_key-controller_name
              suppress_access_permission = 'X' ).
            l_md_controller = l_md_view->get_view_controller( ).

          WHEN wdyn_ctlr_type_window.
            l_md_view = cl_wdy_md_window=>create_complete(
              component_name = p_controller_key-component_name
              window_name    = p_controller_key-controller_name
              suppress_access_permission = 'X' ).
            l_md_controller = l_md_view->get_view_controller( ).

          WHEN wdyn_ctlr_type_custom.
            l_md_controller = cl_wdy_md_controller=>create_complete(
              component_name             = p_controller_key-component_name
              controller_name            = p_controller_key-controller_name
              controller_type            = p_controller_type
              suppress_access_permission = 'X' ).

          WHEN wdyn_ctlr_type_intf_view.
            l_md_view = cl_wdy_md_interface_view=>create_complete(
              component_name = p_controller_key-component_name
              view_name      = p_controller_key-controller_name
              suppress_access_permission = 'X' ).
            l_md_controller = l_md_view->get_view_controller( ).

          WHEN OTHERS.
            MESSAGE e308(swdp_wb_tool) INTO l_msg_str.
            raise_adt_exception_by_msg( ).
        ENDCASE.
        IF l_md_view IS BOUND.
          IF p_description IS NOT INITIAL.
            l_md_view->if_wdy_md_object~set_description( description = p_description ).
          ENDIF.
          l_md_view->save_to_database( EXPORTING suppress_corr = abap_true ).
        ENDIF.
        IF l_md_controller IS BOUND.
          IF p_description IS NOT INITIAL.
            l_md_controller->if_wdy_md_object~set_description( description = p_description ).
          ENDIF.
          l_md_controller->save_to_database( EXPORTING suppress_corr = abap_true ).
        ENDIF.

      CATCH cx_wdy_md_not_existing cx_wdy_md_enqueue_failure cx_wdy_md_corr_cancelled
            cx_wdy_md_permission_failure  cx_wdy_md_access_exception cx_wdy_md_save_exception
        INTO l_md_excp.
        "should never occur
        MESSAGE e504(swdp_wb_tool) WITH l_md_excp->get_text( ) INTO l_msg_str.
        raise_adt_exception_by_msg( previous = l_md_excp ).
    ENDTRY.

    IF p_controller_type <> wdyn_ctlr_type_intf_view AND
       p_controller_type <> wdyn_ctlr_type_cmp_intf.
      cl_wdy_wb_method_ed=>create_predef_methods( p_ref_controller = l_md_controller ).
      TRY.
          l_md_controller->save_to_database( suppress_corr = 'X' ).
        CATCH cx_wdy_md_exception INTO l_md_excp.
          "should never occur
          MESSAGE e504(swdp_wb_tool) WITH l_md_excp->get_text( ) INTO l_msg_str.
          raise_adt_exception_by_msg( previous = l_md_excp ).
      ENDTRY.
    ENDIF.

    p_ref_md_controller = l_md_controller.
  ENDMETHOD.


  METHOD get_controller_ref.
    DATA: l_controller_md TYPE wdy_md_controller_meta_data,
          l_reader_vrs    TYPE REF TO cl_wdy_md_ctlr_reader_vrs,
          l_ref_view      TYPE REF TO if_wdy_md_abstract_view,
          l_complete      TYPE abap_bool,
          l_ctlr_name     TYPE wdy_controller_name,
          l_msg_str       TYPE string.                      "#EC NEEDED

    l_complete = 'X'.
    l_ctlr_name = m_controller_key-controller_name.

    TRY.
        l_reader_vrs ?= m_reader.
      CATCH cx_sy_move_cast_error.
    ENDTRY.

    IF m_controller_data IS INITIAL AND l_reader_vrs IS INITIAL. "load from database
      TRY.
          IF m_is_interface_ctlr = abap_true.
            CLEAR l_complete.
            IF l_ctlr_name = wdyn_interface_controller_name.
              l_ctlr_name = wdyn_component_controller_name.
            ENDIF.
          ENDIF.
          result = cl_wdy_md_controller=>get_object_by_key(
              component_name             = m_controller_key-component_name
              controller_name            = l_ctlr_name
              version                    = m_version
              get_all_items              = l_complete
              suppress_access_permission = 'X'  "done by AIE
              ).
          IF create_predef_methods = abap_true.
            cl_wdy_wb_method_ed=>create_predef_methods( p_ref_controller = result ).
          ENDIF.
        CATCH cx_wdy_md_permission_failure. "cannot occur
          RETURN.
      ENDTRY.

    ELSE.
      IF l_reader_vrs IS BOUND.
        l_controller_md = l_reader_vrs->get_metadata_inst( ).
      ELSE.
        me->convert_controller_data_to_md( IMPORTING p_controller_data_md = l_controller_md ).
      ENDIF.
      result = cl_wdy_md_controller=>get_object_by_metadata(
          controller_key      = m_controller_key
          controller_metadata = l_controller_md ).
      IF result IS INITIAL.
        MESSAGE e000(swdp_wb_tool) WITH space 'zcl_hrpy_utility_abaptogit_wdy=>GET_CONTROLLER_REF' INTO l_msg_str.
        me->raise_adt_exception_by_msg( ).
      ENDIF.
    ENDIF.

  ENDMETHOD.


  METHOD get_exceptions.
    DATA: l_exception      TYPE gty_s_exception.
    FIELD-SYMBOLS: <f_exception>     TYPE wdy_ctlr_exc,
                   <f_exception_txt> TYPE wdy_ctlr_exct.

    LOOP AT m_exceptions ASSIGNING <f_exception> WHERE method_name = p_cmpname.
      CLEAR l_exception.
      MOVE-CORRESPONDING <f_exception> TO l_exception.
*   excpetion short description
      READ TABLE m_exception_txts ASSIGNING <f_exception_txt> BINARY SEARCH
            WITH KEY method_name = p_cmpname
                     exception_name = l_exception-exception_name
                     langu = m_langu.
      IF sy-subrc = 0.
        l_exception-description = <f_exception_txt>-description.
      ENDIF.
      APPEND l_exception TO result.
    ENDLOOP.

  ENDMETHOD.


  METHOD get_instance_by_data.
    DATA: l_controller_key TYPE wdy_controller_key,
          l_ctlr_type      TYPE wdy_md_controller_type,
          l_msg_str        TYPE string,                     "#EC NEEDED
          l_dummy          TYPE r3state.

    ASSERT NOT controller_data IS INITIAL.

    l_controller_key-component_name = controller_data-definition-component_name.
    l_controller_key-controller_name = controller_data-definition-controller_name.

    IF cl_wdy_md_controller=>check_existency( component_name  = l_controller_key-component_name
                                              controller_name = l_controller_key-controller_name ) IS INITIAL.
*   do not support this factory for the virtual interface controller
      RAISE EXCEPTION TYPE cx_wdy_md_not_existing
        EXPORTING
          object_name = l_controller_key-controller_name
          textid      = cx_wdy_md_not_existing=>controller_not_existing.
    ENDIF.

* basic consistency checks
    SELECT SINGLE controller_type FROM wdy_controller INTO l_ctlr_type "#EC WARNOK
      WHERE component_name = l_controller_key-component_name AND
            controller_name = l_controller_key-controller_name.
    IF l_ctlr_type <> controller_data-definition-controller_type.
      MESSAGE e504(swdp_wb_tool) WITH text-e15 INTO l_msg_str.
      raise_adt_exception_by_msg( ).
    ENDIF.
    IF ( l_controller_key-controller_name = wdyn_component_controller_name ) AND
       ( controller_data-definition-controller_type <> wdyn_ctlr_type_component ).
      MESSAGE e504(swdp_wb_tool) WITH text-e15 INTO l_msg_str.
      raise_adt_exception_by_msg( ).
    ELSEIF ( l_controller_key-controller_name = wdyn_interface_controller_name ) AND
           ( controller_data-definition-controller_type <> wdyn_ctlr_type_cmp_intf ).
      MESSAGE e504(swdp_wb_tool) WITH text-e15 INTO l_msg_str.
      raise_adt_exception_by_msg( ).
    ELSEIF ( controller_data-definition-runtime_object IS INITIAL ) AND (
           ( controller_data-definition-controller_type = wdyn_ctlr_type_component ) OR
           ( controller_data-definition-controller_type = wdyn_ctlr_type_cmp_intf ) ).
      MESSAGE e504(swdp_wb_tool) WITH text-e15 INTO l_msg_str.
      raise_adt_exception_by_msg( ).
    ENDIF.

    SELECT SINGLE version FROM wdy_view INTO l_dummy        "#EC WARNOK
      WHERE component_name = l_controller_key-component_name AND
            view_name = l_controller_key-controller_name.
    IF sy-subrc = 0 AND (
      ( controller_data-definition-controller_type = wdyn_ctlr_type_component ) OR
      ( controller_data-definition-controller_type = wdyn_ctlr_type_cmp_intf ) OR
      ( controller_data-definition-controller_type = wdyn_ctlr_type_custom ) ).
*   single controller -> no corresponding view allowed
      MESSAGE e504(swdp_wb_tool) WITH text-e15 INTO l_msg_str.
      raise_adt_exception_by_msg( ).
    ELSEIF sy-subrc <> 0 AND (
      ( controller_data-definition-controller_type = wdyn_ctlr_type_view ) OR
      ( controller_data-definition-controller_type = wdyn_ctlr_type_window ) OR
      ( controller_data-definition-controller_type = wdyn_ctlr_type_intf_view ) ).
*   view/window controller without corresponding view/window
      MESSAGE e504(swdp_wb_tool) WITH text-e15 INTO l_msg_str.
      raise_adt_exception_by_msg( ).
    ENDIF.

    IF ( controller_data-definition-controller_type = wdyn_ctlr_type_intf_view OR
         controller_data-definition-controller_type = wdyn_ctlr_type_cmp_intf ) AND
        NOT source_implementation_part IS INITIAL.
      MESSAGE e504(swdp_wb_tool) WITH text-e04 text-e12 INTO l_msg_str.
      raise_adt_exception_by_msg( ).
    ENDIF.

    CREATE OBJECT result TYPE zcl_hrpy_utility_abaptogit_wdy
      EXPORTING
        p_controller_key  = l_controller_key
        p_version         = 'I'
        p_langu           = sy-langu
        p_adt_view        = adt_view
        p_controller_data = controller_data
        p_implementation  = source_implementation_part.

  ENDMETHOD.


  METHOD get_instance_by_key.
    DATA: l_controller_key TYPE wdy_controller_key.

    l_controller_key-component_name = component_name.
    l_controller_key-controller_name = controller_name.
    TRANSLATE l_controller_key TO UPPER CASE.   "#EC NOTEXT

    IF cl_wdy_md_controller=>check_existency( component_name  = l_controller_key-component_name
                                              controller_name = l_controller_key-controller_name ) IS INITIAL.
      IF l_controller_key-controller_name <> wdyn_interface_controller_name OR
         cl_wdy_md_controller=>check_existency( component_name  = l_controller_key-component_name
                                                controller_name = wdyn_component_controller_name ) IS INITIAL.
*     in case the virtual interface controller is requested check the component controller existency
        RAISE EXCEPTION TYPE cx_wdy_md_not_existing
          EXPORTING
            object_name = l_controller_key-controller_name
            textid      = cx_wdy_md_not_existing=>controller_not_existing.
      ENDIF.
    ENDIF.

    CREATE OBJECT result TYPE zcl_hrpy_utility_abaptogit_wdy
      EXPORTING
        p_controller_key = l_controller_key
        p_version        = version
        p_langu          = sy-langu.


  ENDMETHOD.


  METHOD get_instance_by_version.
    DATA: l_controller_key TYPE wdy_controller_key.

    l_controller_key-component_name = component_name.
    l_controller_key-controller_name = controller_name.
    TRANSLATE l_controller_key TO UPPER CASE.   "#EC NOTEXT

    IF cl_wdy_md_controller=>check_existency( component_name  = l_controller_key-component_name
                                              controller_name = l_controller_key-controller_name ) IS INITIAL.
      RAISE EXCEPTION TYPE cx_wdy_md_not_existing
        EXPORTING
          object_name = l_controller_key-controller_name
          textid      = cx_wdy_md_not_existing=>controller_not_existing.
    ENDIF.

    CREATE OBJECT result TYPE zcl_hrpy_utility_abaptogit_wdy
      EXPORTING
        p_controller_key = l_controller_key
        p_version_number = version_number
        p_langu          = sy-langu
        p_destination    = destination
        p_adt_view       = adt_view.

  ENDMETHOD.


  METHOD get_parameters.
    DATA: l_param      TYPE gty_s_parameter.
    FIELD-SYMBOLS: <f_param>     TYPE wdy_ctlr_param,
                   <f_param_txt> TYPE wdy_ctlr_paramt.

    LOOP AT m_params ASSIGNING <f_param> WHERE cmpname = p_cmpname.
      CLEAR l_param.
      MOVE-CORRESPONDING <f_param> TO l_param.
*   parameter short description
      READ TABLE m_param_txts ASSIGNING <f_param_txt> BINARY SEARCH
            WITH KEY cmpname = p_cmpname
                     parameter_name = l_param-parameter_name
                     langu = m_langu.
      IF sy-subrc = 0.
        l_param-description = <f_param_txt>-description.
      ENDIF.
      APPEND l_param TO result.
    ENDLOOP.

  ENDMETHOD.


  METHOD if_wdy_md_adt_controller~check.
    DATA: l_msg_str          TYPE string,                   "#EC NEEDED
          l_saved            TYPE abap_bool,
          l_ref_controller   TYPE REF TO if_wdy_md_controller,
          l_ref_checklist    TYPE REF TO cl_wb_checklist.
    DATA: l_obj_name         TYPE e071-obj_name,
          l_vers             TYPE r3state,
          l_working_item     TYPE flag,
          l_object_ref       TYPE REF TO if_wdy_md_object,
          l_md_excp          TYPE REF TO cx_wdy_md_permission_failure.

    IF m_is_interface_ctlr = abap_true.
      MESSAGE e504(swdp_wb_tool) WITH text-e13 INTO l_msg_str.
      me->raise_adt_exception_by_msg( ).
    ENDIF.

    IF m_controller_data IS INITIAL.
      l_saved = 'X'.
    ELSEIF ( m_controller_data-definition-controller_type = wdyn_ctlr_type_view OR
             m_controller_data-definition-controller_type = wdyn_ctlr_type_intf_view OR
             m_controller_data-definition-controller_type = wdyn_ctlr_type_window ) AND
            NOT m_adt_view_ref IS BOUND.
      MESSAGE e504(swdp_wb_tool) WITH text-e03 INTO l_msg_str.
      me->raise_adt_exception_by_msg( ).
    ENDIF.

    l_ref_controller = me->get_controller_ref( ).

    IF m_is_comp_interface = abap_false.
      CALL FUNCTION 'WDY_WB_LOCAL_CHECK'
        EXPORTING
          p_ref_controller = l_ref_controller
          p_is_saved       = l_saved
          p_called_by_aie  = 'X'
        CHANGING
          p_wb_checklist   = l_ref_checklist.


    ELSE. "component interface definition

      l_obj_name = m_controller_key-component_name.
      CALL FUNCTION 'RS_OBJECT_IN_WORKING_AREA'
        EXPORTING
          object              = wdyn_limu_component_definition
          obj_name            = l_obj_name
          mode                = 'D'
        IMPORTING
          object_is_work_item = l_working_item.
      IF l_working_item IS INITIAL.
        l_vers = wdyvc_version_active.
      ELSE.
        l_vers = wdyvc_version_inactive.
      ENDIF.

      TRY.
          l_object_ref = cl_wdy_md_component_intf_def=>get_object_by_key(
                         name = m_controller_key-component_name
                         version = l_vers ).
        CATCH cx_wdy_md_permission_failure INTO l_md_excp.
          MESSAGE e504(swdp_wb_tool) WITH l_md_excp->get_text( ) INTO l_msg_str.
          me->raise_adt_exception_by_msg( previous = l_md_excp ).
      ENDTRY.

      CALL METHOD cl_wdy_wb_vc_checker=>check_metadata
        EXPORTING
          p_ref_controller = l_ref_controller
          p_ref_component  = l_object_ref
          p_all_errors     = 'X'
          p_with_intf_impl = 'X'
          p_full_check     = 'X'
          p_called_by      = wdyn_s_check_environment-aie
        CHANGING
          p_wb_checklist   = l_ref_checklist.
    ENDIF.

    result = l_ref_checklist.

  ENDMETHOD.


  METHOD if_wdy_md_adt_controller~delete.
    DATA: l_ref_md_controller TYPE REF TO if_wdy_md_controller,
          l_md_excp           TYPE REF TO cx_wdy_md_exception,
          l_msg_str           TYPE string.                  "#EC NEEDED

    IF m_is_interface_ctlr = abap_true.
      MESSAGE e504(swdp_wb_tool) WITH text-e13 INTO l_msg_str.
      me->raise_adt_exception_by_msg( ).
    ENDIF.

    l_ref_md_controller = me->get_controller_ref( ).

* view/window controller have to be deleted via view/window functionality
    IF l_ref_md_controller->get_type( ) <> wdyn_ctlr_type_custom.
      MESSAGE e049(swdp_wb_tool) INTO l_msg_str.
      me->raise_adt_exception_by_msg( ).
    ENDIF.

    TRY.
        l_ref_md_controller->delete( ).
        l_ref_md_controller->save_to_database( suppress_corr = 'X' ).

      CATCH cx_wdy_md_exception INTO l_md_excp.
        "should never occur
        MESSAGE e504(swdp_wb_tool) WITH l_md_excp->get_text( ) INTO l_msg_str.
        me->raise_adt_exception_by_msg( previous = l_md_excp ).
    ENDTRY.

  ENDMETHOD.


  METHOD if_wdy_md_adt_controller~load_actions.
    DATA: l_action     TYPE gty_s_action.
    FIELD-SYMBOLS: <f_compo>     TYPE wdy_ctlr_compo,
                   <f_compo_txt> TYPE wdy_ctlr_compot.

    IF m_is_interface_ctlr = abap_true. RETURN. ENDIF.

    IF NOT m_controller_data IS INITIAL.
      actions = m_controller_data-actions.

    ELSE.
      me->load_compos_internal( ).

      LOOP AT m_compos ASSIGNING <f_compo> WHERE cmptype = wdyn_ctlr_compo_action.
        CLEAR l_action.
        MOVE-CORRESPONDING <f_compo> TO l_action.
*   short description
        READ TABLE m_compo_txts ASSIGNING <f_compo_txt> BINARY SEARCH
                WITH KEY cmpname = l_action-cmpname
                         langu = m_langu.
        IF sy-subrc = 0.
          l_action-description = <f_compo_txt>-description.
        ENDIF.
*   parameter
        l_action-parameters = me->get_parameters( p_cmpname = l_action-cmpname ).

        APPEND l_action TO actions.
      ENDLOOP.
    ENDIF.
  ENDMETHOD.


  METHOD if_wdy_md_adt_controller~load_attributes.
    DATA: l_attr       TYPE gty_s_attribute.
    FIELD-SYMBOLS: <f_compo>     TYPE wdy_ctlr_compo,
                   <f_compo_txt> TYPE wdy_ctlr_compot.

    IF m_is_interface_ctlr = abap_true. RETURN. ENDIF.

    IF NOT m_controller_data IS INITIAL.
      attributes = m_controller_data-attributes.

    ELSE.
      me->load_compos_internal( ).

      LOOP AT m_compos ASSIGNING <f_compo> WHERE cmptype = wdyn_ctlr_compo_attribute.
        CLEAR l_attr.
        MOVE-CORRESPONDING <f_compo> TO l_attr.
        IF <f_compo>-visibility = wdyn_visibility_public.
          l_attr-is_public = 'X'.
        ENDIF.
*   short description
        READ TABLE m_compo_txts ASSIGNING <f_compo_txt> BINARY SEARCH
              WITH KEY cmpname = l_attr-cmpname
                       langu = m_langu.
        IF sy-subrc = 0.
          l_attr-description = <f_compo_txt>-description.
        ENDIF.

        APPEND l_attr TO attributes.
      ENDLOOP.
    ENDIF.
  ENDMETHOD.


  METHOD if_wdy_md_adt_controller~load_context.
    DATA: l_nodes      TYPE wdy_ctx_node_table,
          l_attributes TYPE wdy_ctx_attrib_table,
          l_mappings   TYPE wdy_ctx_mapping_table,
          l_node       TYPE gty_s_context_node,
          l_attr       TYPE gty_s_context_attribute,
          l_mapping    TYPE gty_s_context_mapping.
    FIELD-SYMBOLS: <f_node> TYPE wdy_ctx_node,
                   <f_attr> TYPE wdy_ctx_attrib,
                   <f_map>  TYPE wdy_ctx_mapping.

    IF NOT m_controller_data IS INITIAL.
      context = m_controller_data-context.
      mapping = m_controller_data-context_mapping.

    ELSE.
      l_nodes = m_reader->get_context_nodes( ).
      l_attributes = m_reader->get_context_attributes( ).
      l_mappings = m_reader->get_context_mappings( ).

* nodes
      LOOP AT l_nodes ASSIGNING <f_node>.
        IF m_is_interface_ctlr = abap_true.
          CHECK <f_node>-is_intf_item = 'X'.
        ENDIF.
        CLEAR l_node.
        MOVE-CORRESPONDING <f_node> TO l_node.
        IF ( <f_node>-is_intf_item = 'X' AND
            ( m_controller_key-controller_name <> wdyn_component_controller_name AND
              m_controller_key-controller_name <> wdyn_interface_controller_name ) ).
          CLEAR l_node-is_intf_item.
        ENDIF.

        IF <f_node>-node_type = c_node_type_recursion.
          l_node-is_recursion_node = 'X'.
        ENDIF.

*   attributes
        LOOP AT l_attributes ASSIGNING <f_attr> WHERE node_name = <f_node>-node_name.
          CLEAR l_attr.
          MOVE-CORRESPONDING <f_attr> TO l_attr.
          me->convert_ctx_help_to_extern( EXPORTING p_md_attribute = <f_attr>
                                          CHANGING  p_attribute    = l_attr ).
          APPEND l_attr TO l_node-attributes.
        ENDLOOP.
        SORT l_node-attributes BY attrib_position.

        APPEND l_node TO context.
      ENDLOOP.
      SORT context BY node_position.

* mapping
      IF m_is_interface_ctlr = abap_false.
        SORT l_nodes BY node_name.
        LOOP AT l_mappings ASSIGNING <f_map>.
          CLEAR l_mapping.
          READ TABLE l_nodes TRANSPORTING NO FIELDS
            WITH KEY node_name = <f_map>-own_node_name
            BINARY SEARCH.
          IF sy-subrc = 0.
            MOVE-CORRESPONDING <f_map> TO l_mapping.
            APPEND l_mapping TO mapping.
          ENDIF.
        ENDLOOP.
        SORT mapping BY ctlr_usage_name.
      ENDIF.
    ENDIF.
  ENDMETHOD.


  METHOD if_wdy_md_adt_controller~load_controller.

    me->if_wdy_md_adt_controller~load_definition(
     IMPORTING definition = controller_data-definition ).
    me->if_wdy_md_adt_controller~load_controller_usages(
      IMPORTING controller_usages = controller_data-controller_usages ).
    me->if_wdy_md_adt_controller~load_context(
       IMPORTING context = controller_data-context
                 mapping = controller_data-context_mapping ).
    me->if_wdy_md_adt_controller~load_actions(
       IMPORTING actions = controller_data-actions ).
    me->if_wdy_md_adt_controller~load_attributes(
      IMPORTING attributes = controller_data-attributes ).
    me->if_wdy_md_adt_controller~load_events(
      IMPORTING events = controller_data-events ).
    me->if_wdy_md_adt_controller~load_methods(
      IMPORTING methods = controller_data-methods
                event_handler = controller_data-event_handler
                supply_functions = controller_data-supply_functions ).

  ENDMETHOD.


  METHOD if_wdy_md_adt_controller~load_controller_usages.
    DATA: l_ctlr_usages   TYPE wdy_ctlr_usage_table,
          l_ctlr_usage    TYPE gty_s_controller_usage.
    FIELD-SYMBOLS: <f_usage> TYPE wdy_ctlr_usage.

    IF m_is_interface_ctlr = abap_true. RETURN. ENDIF.

    IF NOT m_controller_data IS INITIAL.
      controller_usages = m_controller_data-controller_usages.

    ELSE.
      l_ctlr_usages = m_reader->get_controller_usages( ).
      LOOP AT l_ctlr_usages ASSIGNING <f_usage>.
        CLEAR l_ctlr_usage.
        MOVE-CORRESPONDING <f_usage> TO l_ctlr_usage.
        IF NOT l_ctlr_usage-component_usage IS INITIAL AND
           l_ctlr_usage-used_controller = wdyn_component_controller_name.
          l_ctlr_usage-used_controller = wdyn_interface_controller_name.
        ENDIF.
        APPEND l_ctlr_usage TO controller_usages.
      ENDLOOP.
    ENDIF.
  ENDMETHOD.


  METHOD if_wdy_md_adt_controller~load_definition.
    DATA: l_controller TYPE wdy_controller,
          l_descr_tab TYPE wdy_controllert_table.
    FIELD-SYMBOLS: <f_descr> TYPE wdy_controllert.

    IF NOT m_controller_data IS INITIAL.
      definition = m_controller_data-definition.

    ELSE.
      l_controller = m_reader->get_definition( ).
      MOVE-CORRESPONDING l_controller TO definition.
      "different name for virtual interface controller of a component (INTERFACECONTROLLER)
      definition-controller_name = m_controller_key-controller_name.

      l_descr_tab = m_reader->get_descriptions( ).
      READ TABLE l_descr_tab WITH KEY langu = m_langu ASSIGNING <f_descr>.
      IF sy-subrc = 0.
        definition-description = <f_descr>-description.
      ENDIF.
    ENDIF.
  ENDMETHOD.


  METHOD if_wdy_md_adt_controller~load_events.
    DATA: l_event      TYPE gty_s_event.
    FIELD-SYMBOLS: <f_compo>     TYPE wdy_ctlr_compo,
                   <f_compo_txt> TYPE wdy_ctlr_compot.

    IF NOT m_controller_data IS INITIAL.
      events = m_controller_data-events.

    ELSE.
      me->load_compos_internal( ).

      LOOP AT m_compos ASSIGNING <f_compo> WHERE cmptype = wdyn_ctlr_compo_event.
        IF m_is_interface_ctlr = abap_true.
          CHECK <f_compo>-is_intf_item = 'X'.
        ENDIF.
        CLEAR l_event.
        MOVE-CORRESPONDING <f_compo> TO l_event.
*   short description
        READ TABLE m_compo_txts ASSIGNING <f_compo_txt> BINARY SEARCH
                WITH KEY cmpname = l_event-cmpname
                         langu = m_langu.
        IF sy-subrc = 0.
          l_event-description = <f_compo_txt>-description.
        ENDIF.

*   parameter
        l_event-parameters = me->get_parameters( p_cmpname = l_event-cmpname ).

        APPEND l_event TO events.
      ENDLOOP.
    ENDIF.
  ENDMETHOD.


  METHOD if_wdy_md_adt_controller~load_methods.
    DATA: l_method          TYPE gty_s_method,
          l_supply_function TYPE gty_s_supply_function,
          l_event_handler   TYPE gty_s_event_handler,
          l_predef_methods_exists TYPE abap_bool.
    FIELD-SYMBOLS: <f_compo>     TYPE wdy_ctlr_compo,
                   <f_compo_txt> TYPE wdy_ctlr_compot.

    IF NOT m_controller_data IS INITIAL.
      methods = m_controller_data-methods.
      supply_functions = m_controller_data-supply_functions.
      event_handler = m_controller_data-event_handler.

    ELSE.
      me->load_compos_internal( ).

* methods
      LOOP AT m_compos ASSIGNING <f_compo> WHERE cmptype = wdyn_ctlr_compo_method.
        IF m_is_interface_ctlr = abap_true.
          CHECK <f_compo>-is_intf_item = 'X'.
        ENDIF.
        CLEAR l_method.
        MOVE-CORRESPONDING <f_compo> TO l_method.
*   short description
        READ TABLE m_compo_txts ASSIGNING <f_compo_txt> BINARY SEARCH
                WITH KEY cmpname = l_method-cmpname
                         langu = m_langu.
        IF sy-subrc = 0.
          l_method-description = <f_compo_txt>-description.
        ENDIF.
        l_method-is_predefined = cl_wdy_wb_vc_proc_support=>is_predef_method( l_method-cmpname ).
        IF l_method-is_predefined = abap_true.
          l_predef_methods_exists = abap_true.
        ENDIF.
*   parameter
        l_method-parameters = me->get_parameters( p_cmpname = l_method-cmpname ).
        l_method-exceptions = me->get_exceptions( p_cmpname = l_method-cmpname ).

        APPEND l_method TO methods.
      ENDLOOP.

      IF m_is_interface_ctlr = abap_true. RETURN. ENDIF.

* supply functions
      LOOP AT m_compos ASSIGNING <f_compo> WHERE cmptype = wdyn_ctlr_compo_supplyfunc.
        CLEAR l_supply_function.
        MOVE-CORRESPONDING <f_compo> TO l_supply_function.
*   short description
        READ TABLE m_compo_txts ASSIGNING <f_compo_txt> BINARY SEARCH
                WITH KEY cmpname = l_supply_function-cmpname
                         langu = m_langu.
        IF sy-subrc = 0.
          l_supply_function-description = <f_compo_txt>-description.
        ENDIF.

*   parameter
        l_supply_function-parameters = me->get_parameters( p_cmpname = l_supply_function-cmpname ).

        APPEND l_supply_function TO supply_functions.
      ENDLOOP.

* event handler
      LOOP AT m_compos ASSIGNING <f_compo> WHERE cmptype = wdyn_ctlr_compo_evhandler.
        IF m_is_interface_ctlr = abap_true.
          CHECK <f_compo>-is_intf_item = 'X'.
        ENDIF.
        CLEAR l_event_handler.
        MOVE-CORRESPONDING <f_compo> TO l_event_handler.
*   short description
        READ TABLE m_compo_txts ASSIGNING <f_compo_txt> BINARY SEARCH
                WITH KEY cmpname = l_event_handler-cmpname
                         langu = m_langu.
        IF sy-subrc = 0.
          l_event_handler-description = <f_compo_txt>-description.
        ENDIF.

*   parameter
        l_event_handler-parameters = me->get_parameters( p_cmpname = l_event_handler-cmpname ).

        APPEND l_event_handler TO event_handler.
      ENDLOOP.

*     for very old objects the predefined methods may not exist; create them now
      IF l_predef_methods_exists = abap_false AND
         m_is_comp_interface = abap_false     AND
         m_is_interface_ctlr = abap_false.
        me->add_predef_methods( CHANGING methods = methods ).
      ENDIF.

    ENDIF.
  ENDMETHOD.


  METHOD if_wdy_md_adt_controller~load_source.
* TODO: for performance reasons the reader should be reused!!!!
    DATA: l_ref_controller   TYPE REF TO if_wdy_md_controller,
          l_dummy_version    TYPE r3state,
          l_create_predef_methods TYPE abap_bool.
    DATA: l_def_info         TYPE wdyrt_gen_info_type,
          l_imp_info         TYPE wdyrt_gen_info_type,
          l_generation_error TYPE REF TO cx_wdy_rg,
          l_dynamic_exc      TYPE REF TO cx_dynamic_check,
          l_msg_str          TYPE string,                   "#EC NEEDED
          l_line_nr          TYPE i,
          l_lines            TYPE i.

*   for very old objects the predefined methods may not exist; only create them on load from DB!
    l_create_predef_methods = abap_false.
    IF m_version_number IS INITIAL AND m_reader IS BOUND.
      IF m_compos IS INITIAL.
        SELECT SINGLE version FROM wdy_ctlr_compo INTO l_dummy_version
          WHERE component_name  = m_controller_key-component_name  AND
                controller_name = m_controller_key-controller_name AND
                cmpname = wdyvc_do_init.
        IF sy-subrc <> 0.
          l_create_predef_methods = abap_true.
        ENDIF.
      ELSE.
        l_create_predef_methods = abap_true.
        LOOP AT m_compos TRANSPORTING NO FIELDS WHERE cmpname(4) = 'WDDO'.
          l_create_predef_methods = abap_false.
          EXIT.
        ENDLOOP.
      ENDIF.
    ENDIF.
    l_ref_controller = me->get_controller_ref( create_predef_methods = l_create_predef_methods ).

    TRY.
        l_def_info =
          cl_wdy_rg_source_code_tool=>gen_inner_definition(
            controller =                  l_ref_controller
            component_runtime_classname = 'CL_DUMMY_CLASS'
            application_code_only =       abap_true ).
      CATCH cx_wdy_rg INTO l_generation_error.
        MESSAGE e504(swdp_wb_tool) WITH l_generation_error->get_text( ) INTO l_msg_str.
        me->raise_adt_exception_by_msg( previous = l_generation_error ).
      CATCH cx_sy_ref_is_initial INTO l_dynamic_exc.
        MESSAGE e504(swdp_wb_tool) WITH l_dynamic_exc->get_text( ) INTO l_msg_str.
        me->raise_adt_exception_by_msg( previous = l_dynamic_exc ).
    ENDTRY.


* delete definition of framework class
    l_line_nr = 0.
    DESCRIBE TABLE l_def_info-source LINES l_lines.
    SEARCH l_def_info-source FOR 'endclass.'.               "#EC NOTEXT
    IF sy-subrc = 0.
      l_line_nr = sy-tabix + 1.
    ENDIF.
    IF l_line_nr > 0.
      DELETE l_def_info-source FROM l_line_nr TO l_lines.
    ENDIF.

    TRY.
        l_imp_info =
          cl_wdy_rg_source_code_tool=>gen_inner_implementation(
            controller =                  l_ref_controller
            component_runtime_classname = 'CL_DUMMY_CLASS'  "#EC NOTEXT
            application_code_only =       abap_true ).
      CATCH cx_wdy_rg INTO l_generation_error.
        MESSAGE e504(swdp_wb_tool) WITH l_generation_error->get_text( ) INTO l_msg_str.
        me->raise_adt_exception_by_msg( previous = l_generation_error ).
      CATCH cx_sy_ref_is_initial INTO l_dynamic_exc.
        MESSAGE e504(swdp_wb_tool) WITH l_dynamic_exc->get_text( ) INTO l_msg_str.
        me->raise_adt_exception_by_msg( previous = l_dynamic_exc ).
    ENDTRY.

    definition_part = l_def_info-source.
    implementation_part = l_imp_info-source.

  ENDMETHOD.


  METHOD if_wdy_md_adt_controller~save.
    DATA: l_data       TYPE svrs2_wdyc,
          l_delta      TYPE svrs2_xwdyc,
          l_corrnr     TYPE trkorr,  "dummy
          l_msg_str    TYPE string,                         "#EC NEEDED
          l_masterl    TYPE langu,
          l_package    TYPE devclass,
          l_md_excp    TYPE REF TO cx_wdy_md_exception.

    IF m_is_interface_ctlr = abap_true OR
       m_controller_key-controller_name = wdyn_empty_view_name.
      MESSAGE e504(swdp_wb_tool) WITH text-e13 INTO l_msg_str.
      me->raise_adt_exception_by_msg( ).
    ENDIF.
    SELECT SINGLE masterlang devclass FROM tadir INTO (l_masterl, l_package)
      WHERE pgmid    = 'R3TR' AND
            object   = 'WDYN' AND
            obj_name = m_controller_key-component_name.
    IF l_masterl <> m_langu.
      MESSAGE e504(swdp_wb_tool) WITH text-e14 INTO l_msg_str.
      me->raise_adt_exception_by_msg( ).
    ENDIF.

    me->prepare_save( IMPORTING p_new_data_vrs = l_data    "will raise exception in case of inconsistencies
                                p_delta        = l_delta ).

* save the view
    IF ( m_controller_data-definition-controller_type = wdyn_ctlr_type_view OR
         m_controller_data-definition-controller_type = wdyn_ctlr_type_intf_view OR
         m_controller_data-definition-controller_type = wdyn_ctlr_type_window ).
      IF NOT m_adt_view_ref IS BOUND.
        IF m_called_by_unit_test IS INITIAL.
          MESSAGE e504(swdp_wb_tool) WITH text-e03 INTO l_msg_str.
          me->raise_adt_exception_by_msg( ).
        ENDIF.
      ELSE.

      ENDIF.
    ENDIF.

* save the controller delta
    TRY.
    CALL METHOD cl_wdy_md_controller=>recover_version
      EXPORTING
        controller_key            = m_controller_key
        delta                     = l_delta
        recovered_controller_data = l_data
        mode                      = wdyn_vrs_recovery_aie
      CHANGING
        corrnr                    = l_corrnr.
    CATCH cx_wdy_md_exception INTO l_md_excp.
      RAISE EXCEPTION TYPE cx_wdy_md_adt_exception
            EXPORTING previous = l_md_excp.
    ENDTRY.

  ENDMETHOD.


  METHOD load_compos_internal.

    CHECK m_compos IS INITIAL.
    m_compos     = m_reader->get_controller_components( ).
    m_compo_txts = m_reader->get_controller_component_texts( ).
    SORT m_compo_txts BY cmpname langu.

* parameter
    m_params = m_reader->get_controller_parameters( ).
    SORT m_params BY cmpname parameter_name.
    m_param_txts = m_reader->get_controller_parameter_texts( ).
    SORT m_param_txts BY cmpname parameter_name langu.

* exceptions
    m_exceptions = m_reader->get_controller_exceptions( ).
    SORT m_exceptions BY method_name exception_name.
    m_exception_txts = m_reader->get_controller_exception_texts( ).
    SORT m_exception_txts BY method_name exception_name langu.

  ENDMETHOD.


  METHOD modify_code_body.
    DATA: l_source     TYPE rswsourcet,
          l_pre_comment  TYPE rswsourcet,
          l_post_comment TYPE rswsourcet,
          l_new_source   TYPE rswsourcet,
          lv_begin_or_end type string,
          l_meth_line    TYPE string,
          l_meth_COMMENT TYPE string,
          l_meth_begin TYPE string,
          l_meth_end   TYPE string,
          l_src_line   TYPE string,
          l_line       TYPE seo_section_source_line,
          l_nr         TYPE i,
          l_last_indx  TYPE i,
          l_msg_str    TYPE string.                         "#EC NEEDED
    FIELD-SYMBOLS: <f_method> TYPE saboo_method_impl.

    IF p_method_impls IS INITIAL.
      RETURN.  "no implementation given -> let code body untouched
    ENDIF.

    READ TABLE p_method_impls ASSIGNING <f_method> WITH KEY mtdkey-cpdname = p_method_name.
    IF sy-subrc <> 0.
      MESSAGE e170(swdp_wb_tool) WITH p_method_name INTO l_msg_str.
      me->raise_adt_exception_by_msg( ).
    ENDIF.
    l_new_source = <f_method>-implementation.

* original method code body
    l_source = cl_wdy_wb_vc_proc_support=>convert_strng_to_source_static( p_string = p_code_body ).
    IF l_source IS INITIAL.
      CONCATENATE 'method' p_method_name '.' INTO l_src_line SEPARATED BY space. "#EC NOTEXT
      APPEND l_src_line TO l_source.
      l_src_line = 'endmethod.'.                            "#EC NOTEXT
      APPEND l_src_line TO l_source.
    ENDIF.

    lv_begin_or_end ='METHOD'. " variable lv_beign_or_end contains three values
                                     " 'METHOD'(when parsing source for comments before METHOD statement  )
                                     " 'INMETHOD' (   when parsing source between method and endMethod )
                                     " 'ENDMETHOD'(when parsing source for comments after ENDMETHOD statement )
    LOOP AT l_source  INTO l_meth_line.
      TRANSLATE l_meth_line TO UPPER CASE.
      CONDENSE l_meth_line.
      IF lv_begin_or_end eq 'METHOD'.       " If condition to record the comments before start of the method
        IF strlen( l_meth_line ) GE 6 AND l_meth_line+0(6) EQ 'METHOD'.      " Length check is needed to compare for method
          READ TABLE l_source INDEX sy-tabix INTO l_meth_begin.
          lv_begin_or_end = 'INMETHOD'.
        ELSE.
          READ TABLE l_source INDEX sy-tabix INTO l_meth_comment.
          APPEND l_meth_comment TO l_pre_comment.
        ENDIF.

      ELSEIF lv_begin_or_end eq 'INMETHOD'. " Condition to find ENDMETHOD statement
          IF strlen( l_meth_line ) GE 10 AND l_meth_line+0(10) EQ 'ENDMETHOD.'.     " Length check is needed to compare for method
            READ TABLE l_source INDEX sy-tabix INTO l_meth_end.
            lv_begin_or_end = 'ENDMETHOD'.
          ENDIF.
      ELSE.                                  " Else condition to record the comments after ENDMETHOD statement

        READ TABLE l_source INDEX sy-tabix INTO l_meth_comment.
        APPEND l_meth_comment TO l_post_comment.

        ENDIF.
    ENDLOOP.

    IF lines( l_new_source ) > 0.
      READ TABLE l_new_source INDEX 1 INTO l_line.
      IF l_line IS INITIAL.   "source-scan creates emtpy first lines
        DELETE l_new_source INDEX 1.
      ELSEIF l_line CS '"#EC NEEDED'.
        IF  sy-fdpos = 0 OR
          ( sy-fdpos > 0 AND l_line(sy-fdpos) IS INITIAL ).
          DELETE l_new_source INDEX 1.
        ENDIF.
      ENDIF.
    ENDIF.

    CLEAR l_source.
    CLEAR l_meth_line.
    LOOP AT l_pre_comment INTO l_meth_line.     " Adding comments before start of method
      APPEND l_meth_line TO l_source.
    ENDLOOP.
    APPEND l_meth_begin TO l_source.            " Adding Method statement
    LOOP AT l_new_source INTO l_meth_line.      " Adding implementation
      APPEND l_meth_line TO l_source.
    ENDLOOP.

* scan does not remove the endmethod statement in case of a missing '.' before
* handle this special case here
    l_nr = lines( l_source ).
    l_last_indx = 0.
    DO.
      CHECK l_nr > 0.
      READ TABLE l_source INDEX l_nr INTO l_line.
      IF l_line IS INITIAL.
        l_nr = l_nr - 1.
      ELSEIF l_line CS 'ENDMETHOD.' AND
             sy-fdpos = 0.      "ENDMETHOD statement already exists
        l_last_indx = l_nr.
        EXIT.  "DO
      ELSE.
        EXIT.   "DO
      ENDIF.
    ENDDO.
    IF l_last_indx > 0.
      DELETE l_source FROM l_last_indx.
    ENDIF.
*
    APPEND l_meth_end TO l_source.             "endmethod statement

    LOOP AT l_post_comment INTO l_meth_line.     " Adding comments after end of method
      APPEND l_meth_line TO l_source.
    ENDLOOP.

    p_code_body = cl_wdy_wb_vc_proc_support=>convert_source_to_strng_static( p_source = l_source ).

  ENDMETHOD.


  METHOD prepare_save.
* see program WDY_RECOVER_VERSION
    DATA: l_new_data_md      TYPE wdy_md_controller_meta_data,
          l_new_data_vrs     TYPE svrs2_versionable_object,  "data from AIE
          l_save_data_vrs    TYPE svrs2_versionable_object, "data from DB
          l_delta            TYPE svrs2_xversionable_object,
          l_ctlr_compo_vrs   TYPE wdy_ctlr_compo_vrs,
          l_ctlr_source_vrs  TYPE wdy_ctlr_compo_source_vrs,
          l_ctlr_compo_delta TYPE vxwdy_ctlr_compo_vrs,
          l_source           TYPE rswsourcet.
    DATA: l_inactive_exists     TYPE abap_bool,
          l_object_is_work_item TYPE abap_bool,
          l_objname             TYPE trobj_name,
          l_objtype             TYPE trobjtype,
          l_version             TYPE r3state,
          l_t100_key            TYPE scx_t100key.
    FIELD-SYMBOLS: <f_ctlr_compo_src_delta> TYPE vxwdy_ctlr_compo_source_vrs,
                   <f_ctlr_compo_vrs>       TYPE wdy_ctlr_compo_vrs,
                   <f_ctlr_compo_md>        TYPE wdy_ctlr_compo.

    l_new_data_vrs-objtype = 'WDYC'.
    l_new_data_vrs-objname = m_controller_key.
    l_save_data_vrs = l_new_data_vrs.

* convert AIE format into MD format
    me->convert_controller_data_to_md( IMPORTING p_controller_data_md = l_new_data_md ).

* convert MD format into VRS format
    l_new_data_md-definition-changedby = sy-uname.
    l_new_data_md-definition-changedon = sy-datum.
    APPEND l_new_data_md-definition TO l_new_data_vrs-wdyc-defin.
    l_new_data_vrs-wdyc-descr = l_new_data_md-descriptions[].
    l_new_data_vrs-wdyc-cusag = l_new_data_md-controller_usages[].
* l_new_data_vrs-wdyc-ccomp                              "special format
* l_new_data_vrs-wdyc-ccoms                              "   "      "
    l_new_data_vrs-wdyc-ccomt = l_new_data_md-controller_component_texts[].
    l_new_data_vrs-wdyc-cpara = l_new_data_md-controller_parameters[].
    l_new_data_vrs-wdyc-cpart = l_new_data_md-controller_parameter_texts[].
    l_new_data_vrs-wdyc-excp  = l_new_data_md-controller_exceptions[].
    l_new_data_vrs-wdyc-excpt = l_new_data_md-controller_exception_texts[].
    l_new_data_vrs-wdyc-cnode = l_new_data_md-context_nodes[].
    l_new_data_vrs-wdyc-cattr = l_new_data_md-context_attributes[].
    l_new_data_vrs-wdyc-cmapp = l_new_data_md-context_mappings[].

* special convertion for compos (in particular methods with source code) into VRS format
    LOOP AT l_new_data_md-controller_components ASSIGNING <f_ctlr_compo_md>.
*   definition
      MOVE-CORRESPONDING <f_ctlr_compo_md> TO l_ctlr_compo_vrs.
      APPEND l_ctlr_compo_vrs TO l_new_data_vrs-wdyc-ccomp.
      IF <f_ctlr_compo_md>-code_body IS INITIAL.  "no method
        CONTINUE.
      ENDIF.
*   source code
      CLEAR: l_source, l_ctlr_source_vrs.
      MOVE-CORRESPONDING <f_ctlr_compo_md> TO l_ctlr_source_vrs.
      CALL METHOD cl_o2_helper=>convert_string_to_table
        EXPORTING
          p_input = <f_ctlr_compo_md>-code_body
        IMPORTING
          p_table = l_source.
      LOOP AT l_source INTO l_ctlr_source_vrs-source_line.
        l_ctlr_source_vrs-line_number = sy-tabix.
        APPEND l_ctlr_source_vrs TO l_new_data_vrs-wdyc-ccoms.
      ENDLOOP.
    ENDLOOP.

    IF ( m_controller_data-definition-controller_type = wdyn_ctlr_type_view OR
         m_controller_data-definition-controller_type = wdyn_ctlr_type_intf_view OR
         m_controller_data-definition-controller_type = wdyn_ctlr_type_window ).
      l_objtype = 'WDYV'. "for view controller no inactive controller entry exists
    ELSE.
      l_objtype = 'WDYC'.
    ENDIF.
    l_objname = m_controller_key.
    CALL FUNCTION 'RS_OBJECT_IN_WORKING_AREA'
      EXPORTING
        object                  = l_objtype
        obj_name                = l_objname
        mode                    = 'D'
      IMPORTING
        object_is_work_item     = l_object_is_work_item
        object_inactive_version = l_inactive_exists.
    IF l_inactive_exists IS INITIAL AND l_object_is_work_item IS INITIAL.
      l_version = 'A'.
    ELSE.
      l_version = 'I'.
    ENDIF.

    FIELD-SYMBOLS: <f_defin> TYPE wdy_controller,
                   <f_cusag> TYPE wdy_ctlr_usage,
                   <f_ccomp> TYPE wdy_ctlr_compo_vrs,
                   <f_cpara> TYPE wdy_ctlr_param,
                   <f_excp>  TYPE wdy_ctlr_exc,
                   <f_cnode> TYPE wdy_ctx_node,
                   <f_cattr> TYPE wdy_ctx_attrib,
                   <f_cmapp> TYPE wdy_ctx_mapping.
* get saved version from DB
    CALL FUNCTION 'WDYC_GET_OBJECT'
      EXPORTING
        controller_key               = m_controller_key
        r3state                      = l_version
        get_all_translations         = ''
*    importing
*       object                       = controller_metadata
      TABLES
        definition                   = l_save_data_vrs-wdyc-defin
        descriptions                 = l_save_data_vrs-wdyc-descr
        controller_usages            = l_save_data_vrs-wdyc-cusag
        controller_components        = l_save_data_vrs-wdyc-ccomp
        controller_component_sources = l_save_data_vrs-wdyc-ccoms
        controller_component_texts   = l_save_data_vrs-wdyc-ccomt
        controller_parameters        = l_save_data_vrs-wdyc-cpara
        controller_parameter_texts   = l_save_data_vrs-wdyc-cpart
        controller_exceptions        = l_save_data_vrs-wdyc-excp
        controller_exception_texts   = l_save_data_vrs-wdyc-excpt
        context_nodes                = l_save_data_vrs-wdyc-cnode
        context_attributes           = l_save_data_vrs-wdyc-cattr
        context_mappings             = l_save_data_vrs-wdyc-cmapp
      EXCEPTIONS
        not_existing                 = 1.
    IF sy-subrc <> 0.
      l_t100_key-msgid = 'SWDP_WB_TOOL'.
      l_t100_key-msgno = '471'.
      l_t100_key-attr1 = m_controller_key-component_name.
      l_t100_key-attr2 = m_controller_key-controller_name.
      RAISE EXCEPTION TYPE cx_wdy_md_adt_exception
        EXPORTING
          textid = l_t100_key.
    ENDIF.

    LOOP AT l_save_data_vrs-wdyc-defin ASSIGNING <f_defin>.
      CLEAR <f_defin>-version.
    ENDLOOP.
    LOOP AT l_save_data_vrs-wdyc-cusag ASSIGNING <f_cusag>.
      CLEAR <f_cusag>-version.
    ENDLOOP.
    LOOP AT l_save_data_vrs-wdyc-ccomp ASSIGNING <f_ccomp>.
      CLEAR <f_ccomp>-version.
    ENDLOOP.
    LOOP AT l_save_data_vrs-wdyc-cpara ASSIGNING <f_cpara>.
      CLEAR <f_cpara>-version.
    ENDLOOP.
    LOOP AT l_save_data_vrs-wdyc-excp ASSIGNING <f_excp>.
      CLEAR <f_excp>-version.
    ENDLOOP.
    LOOP AT l_save_data_vrs-wdyc-cnode ASSIGNING <f_cnode>.
      CLEAR <f_cnode>-version.
    ENDLOOP.
    LOOP AT l_save_data_vrs-wdyc-cattr ASSIGNING <f_cattr>.
      CLEAR <f_cattr>-version.
    ENDLOOP.
    LOOP AT l_save_data_vrs-wdyc-cmapp ASSIGNING <f_cmapp>.
      CLEAR <f_cmapp>-version.
    ENDLOOP.

* now compare those two structures
* the resulting delta is used to update the metadata accordingly
    CALL FUNCTION 'SVRS_MAKE_OBJECT_DELTA '
      EXPORTING
        obj_old              = l_save_data_vrs
        obj_new              = l_new_data_vrs
      CHANGING
        delta                = l_delta
      EXCEPTIONS
        inconsistent_objects = 0.
* !!!ATTENTION!!!
* in case of a modification, i.e. vrsflag = 'U', the delta information contains the OLD data

* special handling for methods:
* if the source code of a method has differences, the corresponding source code structures
* are contained within the delta, but not the definition itself.
* to force the update, the method definition has also to be inserted into the delta and
* the vrsflag has to be set correctly to 'U'!

* add the method def, if a delta in the source body exists
    LOOP AT l_delta-wdyc-ccoms ASSIGNING <f_ctlr_compo_src_delta>.
      READ TABLE l_delta-wdyc-ccomp TRANSPORTING NO FIELDS
           WITH KEY cmpname = <f_ctlr_compo_src_delta>-cmpname.
      IF sy-subrc <> 0.  " not yet in delta
        READ TABLE l_new_data_vrs-wdyc-ccomp ASSIGNING <f_ctlr_compo_vrs>
             WITH KEY cmpname = <f_ctlr_compo_src_delta>-cmpname.
        IF sy-subrc = 0.  " should always occur
          MOVE-CORRESPONDING <f_ctlr_compo_vrs> TO l_ctlr_compo_delta.
          l_ctlr_compo_delta-vrsflag = 'U'.
          APPEND l_ctlr_compo_delta TO l_delta-wdyc-ccomp.
        ENDIF.
      ENDIF.
    ENDLOOP.

    p_new_data_vrs = l_new_data_vrs-wdyc.
    p_delta        = l_delta-wdyc.
  ENDMETHOD.


  METHOD raise_adt_exception_by_msg.
    DATA: l_t100_key  TYPE scx_t100key.

    l_t100_key-msgid = sy-msgid.
    l_t100_key-msgno = sy-msgno.
    l_t100_key-attr1 = sy-msgv1.
    l_t100_key-attr2 = sy-msgv2.
    l_t100_key-attr3 = sy-msgv3.
    l_t100_key-attr4 = sy-msgv4.
    RAISE EXCEPTION TYPE cx_wdy_md_adt_exception
      EXPORTING
        textid   = l_t100_key
        previous = previous.
  ENDMETHOD.


  METHOD split_implementation.
    DATA: l_implementation TYPE saboo_sourt,
          l_vseo_tabs      TYPE saboo_vseot,
          l_msg_str        TYPE string.                     "#EC NEEDED

    l_implementation = m_implementation.
    CALL FUNCTION 'SCAN_ABAP_OBJECTS_CLASSES'
      CHANGING
        vseo_tabs                   = l_vseo_tabs
        method_impls                = p_method_impls
        sourc_tab                   = l_implementation
      EXCEPTIONS
        scan_abap_source_error      = 1
        scan_abap_src_line_too_long = 2
        OTHERS                      = 3.

    IF sy-subrc <> 0.
      MESSAGE e392(swdp_wb_tool) INTO l_msg_str.
      me->raise_adt_exception_by_msg( ).
    ENDIF.

  ENDMETHOD.
ENDCLASS.