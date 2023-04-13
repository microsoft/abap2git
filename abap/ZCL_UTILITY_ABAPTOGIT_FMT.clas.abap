" Helper class to generate text for SAP object types
class ZCL_UTILITY_ABAPTOGIT_FMT definition
  public
  final
  create public .

public section.

    TYPES: tty_abaptext TYPE TABLE OF abaptxt255 INITIAL SIZE 0.

    " get content of SAPScript
    " iv_objname - ABAP object name
    " ev_filecontent - file content
    " et_filecontent - file content lines
    CLASS-METHODS get_sapscript_content
        IMPORTING
            iv_objname        TYPE e071-obj_name
        EXPORTING
            ev_filecontent    TYPE string
            et_filecontent    TYPE tty_abaptext
        RETURNING VALUE(rv_success) TYPE abap_bool.

protected section.

private section.

    CONSTANTS:
          t_meaning     VALUE '1',                  "#EC NOTEXT Bedeutung
          t_standard    VALUE '2',                  "#EC NOTEXT Standardattribute
          t_print       VALUE '3',                  "#EC NOTEXT Druckattribute
          t_pagecounter VALUE '4',                  "#EC NOTEXT Seitenzähler
          t_pagewins    VALUE '5',                  "#EC NOTEXT Seitenfenster
          t_title       VALUE '6',                  "#EC NOTEXT Titel
          t_after_title VALUE '7'.                  "#EC NOTEXT Nach Titel

    TYPES: BEGIN OF ts_result,
             id(8),
             item(20),
             attr(20),
             value(61),
             vol       LIKE sy-index,
           END OF ts_result.

    TYPES: tty_result TYPE TABLE OF ts_result WITH DEFAULT KEY.
    TYPES: tty_dfies TYPE TABLE OF dfies WITH DEFAULT KEY.
    TYPES: tty_itctg TYPE TABLE OF itctg WITH DEFAULT KEY.
    TYPES: tty_itcth TYPE TABLE OF itcth WITH DEFAULT KEY.
    TYPES: tty_itctw TYPE TABLE OF itctw WITH DEFAULT KEY.
    TYPES: tty_tline TYPE TABLE OF tline WITH DEFAULT KEY.
    TYPES: tty_itcdp TYPE TABLE OF itcdp WITH DEFAULT KEY.
    TYPES: tty_itcdq TYPE TABLE OF itcdq WITH DEFAULT KEY.
    TYPES: tty_itcds TYPE TABLE OF itcds WITH DEFAULT KEY.

    TYPES: BEGIN OF ty_string_array,
             name TYPE string,
           END OF ty_string_array.

    " retrieve header content from FORM object in SAPSCRIPT
    CLASS-METHODS get_text_header_content
        IMPORTING
            is_header       TYPE thead
        CHANGING
            cht_filecontent TYPE tty_abaptext.

    " retrieve header content from FORM object in SAPSCRIPT
    CLASS-METHODS get_text_form_header_content
        IMPORTING
            is_header       TYPE thead
            is_form_header  TYPE itcta
        CHANGING
            cht_filecontent TYPE tty_abaptext.

    " retrieve string content from FORM object in SAPSCRIPT
    CLASS-METHODS get_text_string_content
        IMPORTING
            it_strings      TYPE tty_itcds
        CHANGING
            cht_filecontent TYPE tty_abaptext.

    " retrieve paragraph content from FORM object in SAPSCRIPT
    CLASS-METHODS get_text_paragraph_content
      IMPORTING
        it_paragraphs  TYPE tty_itcdp
        it_tabs        TYPE tty_itcdq
      CHANGING
        cht_filecontent TYPE tty_abaptext.

    " retrieve windows content from FORM object in SAPSCRIPT
    CLASS-METHODS get_text_windows_content
      IMPORTING
        it_windows     TYPE tty_itctw
      CHANGING
        cht_filecontent TYPE tty_abaptext.

    " retrieve page content from FORM object in SAPSCRIPT
    CLASS-METHODS get_text_page_content
      IMPORTING
        it_pages        TYPE tty_itctg
        it_page_windows TYPE tty_itcth
      CHANGING
        cht_filecontent  TYPE tty_abaptext.

    " retrieve header content from FORM object in SAPSCRIPT
    CLASS-METHODS get_text_txtelement_content
      IMPORTING
        it_windows     TYPE tty_itctw
        it_form_lines  TYPE tty_tline
      CHANGING
        cht_filecontent TYPE tty_abaptext.

ENDCLASS.


CLASS ZCL_UTILITY_ABAPTOGIT_FMT IMPLEMENTATION.

  METHOD get_sapscript_content.

    DATA lv_form TYPE itcta-tdform.
    DATA ls_form_header TYPE itcta.
    DATA lv_found TYPE abap_bool.
    DATA ls_header TYPE thead.
    DATA lv_olanguage TYPE sy-langu.
    DATA lt_form_lines TYPE TABLE OF tline.
    DATA lt_pages TYPE TABLE OF itctg.
    DATA lt_page_windows TYPE TABLE OF itcth.
    DATA lt_paragraphs TYPE TABLE OF itcdp.
    DATA lt_strings TYPE TABLE OF itcds.
    DATA lt_tabs TYPE TABLE OF itcdq.
    DATA lt_windows TYPE TABLE OF itctw.
    DATA lv_transtat TYPE string.
    DATA lv_status TYPE string.
    DATA: lv_rcode LIKE sy-subrc.

    lv_form = iv_objname.
    CALL FUNCTION 'READ_FORM'
      EXPORTING
        form         = lv_form
      IMPORTING
        form_header  = ls_form_header
        found        = lv_found
        header       = ls_header
        olanguage    = lv_olanguage
      TABLES
        form_lines   = lt_form_lines
        pages        = lt_pages
        page_windows = lt_page_windows
        paragraphs   = lt_paragraphs
        strings      = lt_strings
        tabs         = lt_tabs
        windows      = lt_windows.
    IF lv_found = abap_false.
        rv_success = abap_false.
        RETURN.
    ENDIF.

    get_text_header_content(
        EXPORTING
            is_header = ls_header
        CHANGING
            cht_filecontent = et_filecontent
             ).

    " form header
    APPEND '' TO et_filecontent.
    APPEND |{ space WIDTH = 80 PAD = '-' }| TO et_filecontent.
    APPEND 'FORMHEADER' TO et_filecontent.
    APPEND |{ space WIDTH = 80 PAD = '-' }| TO et_filecontent.
    get_text_form_header_content(
        EXPORTING
            is_header = ls_header
            is_form_header = ls_form_header
        CHANGING
            cht_filecontent = et_filecontent
             ).

    APPEND '' TO et_filecontent.
    get_text_string_content(
        EXPORTING
            it_strings     = lt_strings
        CHANGING
            cht_filecontent = et_filecontent
             ).

    get_text_paragraph_content(
        EXPORTING
            it_paragraphs  = lt_paragraphs
            it_tabs   = lt_tabs
        CHANGING
            cht_filecontent = et_filecontent
             ).

    get_text_windows_content(
        EXPORTING
            it_windows  = lt_windows
        CHANGING
            cht_filecontent = et_filecontent
             ).

    get_text_page_content(
        EXPORTING
            it_pages  = lt_pages
            it_page_windows = lt_page_windows
        CHANGING
            cht_filecontent = et_filecontent
             ).

    get_text_txtelement_content(
        EXPORTING
            it_windows = lt_windows
                it_form_lines = lt_form_lines
        CHANGING
            cht_filecontent = et_filecontent
            ).

    APPEND '' TO et_filecontent.

    ev_filecontent = concat_lines_of( table = et_filecontent sep = cl_abap_char_utilities=>cr_lf ).

    " align with GUI_DOWNLOAD which adds a blank line
    ev_filecontent = ev_filecontent && cl_abap_char_utilities=>cr_lf.

    rv_success = abap_true.

  ENDMETHOD.

  METHOD get_text_header_content.
    DATA: lt_header_struct     TYPE REF TO cl_abap_structdescr,
          lt_header_components TYPE        abap_compdescr_tab,
          lt_fieldattr         TYPE STANDARD TABLE OF dfies INITIAL SIZE 0,
          ls_header            TYPE thead,
          wa_fieldattr         TYPE dfies.

    FIELD-SYMBOLS : <fs_components> TYPE abap_compdescr.
    FIELD-SYMBOLS: <f1> TYPE any, <f2> TYPE any, <f3> TYPE any.

    CALL FUNCTION 'DDIF_FIELDINFO_GET'
      EXPORTING
        tabname   = 'ITCTA'
      TABLES
        dfies_tab = lt_fieldattr
      EXCEPTIONS
        OTHERS    = 1.

    ls_header = is_header.
    lt_header_struct ?= cl_abap_typedescr=>describe_by_data( ls_header ).
    lt_header_components = lt_header_struct->components.

    ASSIGN ls_header TO <f1>.

    APPEND |{ space WIDTH = 80 PAD = '-' }| TO cht_filecontent.
    APPEND 'HEADER' TO cht_filecontent.
    APPEND |{ space WIDTH = 80 PAD = '-' }| TO cht_filecontent.

    LOOP AT lt_header_components INTO DATA(wa_header).
      CLEAR wa_fieldattr.
      ASSIGN wa_header-name TO <f2>.
      ASSIGN COMPONENT <f2> OF STRUCTURE <f1> TO <f3>.
      READ TABLE lt_fieldattr INTO wa_fieldattr WITH KEY tabname = 'ITCTA'
                                  fieldname = <f2>
                                  langu = sy-langu.
      APPEND  |{ <f2> WIDTH = 22 }{ cl_abap_char_utilities=>horizontal_tab }| &&
              |{ wa_fieldattr-scrtext_m WIDTH = 22 }{ cl_abap_char_utilities=>horizontal_tab }| &&
              |{ <f3> WIDTH = 22 }| TO cht_filecontent.
    ENDLOOP.
  ENDMETHOD.

  METHOD get_text_form_header_content.
    DATA: lt_formheader_struct     TYPE REF TO   cl_abap_structdescr.
    DATA: lt_formheader_components TYPE          abap_compdescr_tab.
    DATA: lt_header_struct         TYPE REF TO   cl_abap_structdescr.
    DATA: lt_header_components     TYPE          abap_compdescr_tab.
    DATA: ls_header TYPE thead.
    DATA: ls_form_header TYPE itcta.
    DATA: lt_fieldattr TYPE STANDARD TABLE OF dfies INITIAL SIZE 0.
    FIELD-SYMBOLS: <f1> TYPE any, <f2> TYPE any, <f3> TYPE any.

    ls_header = is_header.
    ls_form_header = is_form_header.
    lt_header_struct ?= cl_abap_typedescr=>describe_by_data( ls_header ).
    lt_header_components = lt_header_struct->components.
    lt_formheader_struct ?= cl_abap_typedescr=>describe_by_data( ls_form_header ).
    lt_formheader_components = lt_formheader_struct->components.

    ASSIGN ls_form_header TO <f1>.

    LOOP AT lt_header_components INTO DATA(wa_formheader).
      ASSIGN wa_formheader-name TO <f2>.
      ASSIGN COMPONENT <f2> OF STRUCTURE <f1> TO <f3>.
      IF line_exists( lt_formheader_components[ name = <f2> ] ).
        READ TABLE lt_fieldattr INTO DATA(wa_fieldattr) WITH KEY tabname = 'ITCTA'
                                    fieldname = <f2>
                                    langu = sy-langu.
        APPEND  |{ <f2> WIDTH = 22 }{ cl_abap_char_utilities=>horizontal_tab }| &&
                |{ wa_fieldattr-scrtext_m WIDTH = 22 }{ cl_abap_char_utilities=>horizontal_tab }| &&
                |{ <f3> WIDTH = 22 }| TO cht_filecontent.
      ENDIF.
      CLEAR wa_fieldattr.
    ENDLOOP.
  ENDMETHOD.

  METHOD get_text_string_content.

    DATA: lt_fieldattr TYPE STANDARD TABLE OF dfies INITIAL SIZE 0.
    DATA: var1                      LIKE sy-index.
    DATA: var2                      LIKE sy-index.
    DATA: var3                      LIKE sy-index.
    DATA: lt_string_table           TYPE REF TO   cl_abap_tabledescr.
    DATA: lt_string_struct          TYPE REF TO   cl_abap_structdescr.
    DATA: lt_string_components      TYPE          abap_compdescr_tab.
    DATA: lt_strings                TYPE tty_itcds.
    DATA: lt_results                TYPE tty_result.
    DATA: ls_result                 TYPE ts_result.
    DATA: entry.
    FIELD-SYMBOLS: <f1> TYPE any, <f2> TYPE any, <f3> TYPE any.

    lt_strings = it_strings.
    lt_string_table ?= cl_abap_typedescr=>describe_by_data( lt_strings ).
    lt_string_struct ?= lt_string_table->get_table_line_type( ).
    lt_string_components = lt_string_struct->components.

    DATA: ls_string_attr TYPE ty_string_array.
    DATA: lt_string_standardattr TYPE STANDARD TABLE OF ty_string_array.
    DATA: lt_string_fontattr TYPE STANDARD TABLE OF ty_string_array.

************************ Standard attribute *****************************
    ls_string_attr-name = 'TDMARK'.
    APPEND ls_string_attr TO lt_string_standardattr.
    ls_string_attr-name = 'TDPROTLINE'.
    APPEND ls_string_attr TO lt_string_standardattr.
    ls_string_attr-name = 'TDHIDDEN'.
    APPEND ls_string_attr TO lt_string_standardattr.
    ls_string_attr-name = 'TDSUPER'.
    APPEND ls_string_attr TO lt_string_standardattr.
    ls_string_attr-name = 'TDBARCODE'.
    APPEND ls_string_attr TO lt_string_standardattr.

************************ Font attribute *********************************
    ls_string_attr-name = 'TDFAMILY'.
    APPEND ls_string_attr TO lt_string_fontattr.
    ls_string_attr-name = 'TDHEIGHT'.
    APPEND ls_string_attr TO lt_string_fontattr.
    ls_string_attr-name = 'TDBOLD'.
    APPEND ls_string_attr TO lt_string_fontattr.
    ls_string_attr-name = 'TDITALIC'.
    APPEND ls_string_attr TO lt_string_fontattr.
    ls_string_attr-name = 'TDUNDERLIN'.
    APPEND ls_string_attr TO lt_string_fontattr.
    ls_string_attr-name = 'TDULPOS'.
    APPEND ls_string_attr TO lt_string_fontattr.
    ls_string_attr-name = 'TDULTHICK'.
    APPEND ls_string_attr TO lt_string_fontattr.
    ls_string_attr-name = 'TDULGREY'.
    APPEND ls_string_attr TO lt_string_fontattr.
    ls_string_attr-name = 'TDUNDERLIN'.
    APPEND ls_string_attr TO lt_string_fontattr.

    CALL FUNCTION 'DDIF_FIELDINFO_GET'
      EXPORTING
        tabname   = 'ITCDS'
      TABLES
        dfies_tab = lt_fieldattr
      EXCEPTIONS
        OTHERS    = 1.

    var2 = 1.

    LOOP AT lt_strings INTO DATA(lv_string).
      ASSIGN lv_string TO <f1>.

      var1 = 0.
      MOVE lv_string-tdstring TO ls_result-id.
      MOVE t_meaning TO ls_result-item.
      MOVE lv_string-tdtext TO ls_result-value.
      var3 = var2.
      APPEND ls_result TO lt_results.
      CLEAR: ls_result-item, ls_result-value.
      ADD 1 TO var2.

      MOVE 'Standard Attributes' TO ls_result-item.
      entry = abap_false.
      LOOP AT lt_string_standardattr INTO DATA(wa_stdattr_itcds).
        ASSIGN wa_stdattr_itcds-name TO <f2>.
        ASSIGN COMPONENT <f2> OF STRUCTURE <f1> TO <f3>.
        READ TABLE lt_fieldattr INTO DATA(wa_fieldattr) WITH KEY tabname = 'ITCDS'
                                    fieldname = <f2>
                                    langu = sy-langu.
        MOVE wa_fieldattr-scrtext_m TO ls_result-attr.
        MOVE <f3> TO ls_result-value.
        APPEND ls_result TO lt_results.
        CLEAR: ls_result-attr, ls_result-value.
        entry = abap_true.
        ADD 1 TO var2.
      ENDLOOP.

      MOVE 'Font Attributes' TO ls_result-item.
      entry = abap_false.
      LOOP AT lt_string_fontattr INTO DATA(wa_fontattr_itcds).
        ASSIGN wa_fontattr_itcds-name TO <f2>.
        ASSIGN COMPONENT <f2> OF STRUCTURE <f1> TO <f3>.
        READ TABLE lt_fieldattr INTO wa_fieldattr WITH KEY tabname = 'ITCDS'
                                    fieldname = <f2>
                                    langu = sy-langu.
        MOVE wa_fieldattr-scrtext_m TO ls_result-attr.

        MOVE <f3> TO ls_result-value.
        APPEND ls_result TO lt_results.
        CLEAR: ls_result-attr, ls_result-value.
        entry = abap_true.
        ADD 1 TO var2.
      ENDLOOP.

    ENDLOOP.

    CLEAR ls_result.

    LOOP AT lt_results INTO ls_result.
      IF sy-tabix = 1.
        APPEND |{ space WIDTH = 80 PAD = '-' }| TO cht_filecontent.
        APPEND |Characters    Attributes| TO cht_filecontent.
        APPEND |{ space WIDTH = 80 PAD = '-' }| TO cht_filecontent.
        APPEND '' TO cht_filecontent.
      ENDIF.
      AT NEW id.
        APPEND '' TO cht_filecontent.
      ENDAT.

      AT NEW item.
        IF ls_result-item <> t_meaning.

          APPEND |{ ls_result-item WIDTH = 15 }| TO cht_filecontent.
        ENDIF.
      ENDAT.
      IF ls_result-item = t_meaning.
        APPEND |{ ls_result-id WIDTH = 12 }{ ls_result-value WIDTH = 15 }| TO cht_filecontent.
      ELSE.
        APPEND |{ ls_result-attr WIDTH = 18 }{ cl_abap_char_utilities=>horizontal_tab }{ ls_result-value WIDTH = 36 }| TO cht_filecontent.
      ENDIF.

    ENDLOOP.

  ENDMETHOD.

  METHOD get_text_paragraph_content.
    DATA: var1                      LIKE sy-index.
    DATA: var2                      LIKE sy-index.
    DATA: var3                      LIKE sy-index.
    DATA: lt_paragraphs             TYPE tty_itcdp.
    DATA: lt_tabs                    TYPE tty_itcdq.
    DATA: lt_fieldattr              TYPE STANDARD TABLE OF dfies INITIAL SIZE 0.
    DATA: lt_para_table             TYPE REF TO   cl_abap_tabledescr.
    DATA: lt_para_struct            TYPE REF TO   cl_abap_structdescr.
    DATA: lt_para_components        TYPE          abap_compdescr_tab.
    DATA: lt_results                TYPE tty_result.
    DATA: ls_result                 TYPE ts_result.
    DATA: entry.
    FIELD-SYMBOLS: <f1> TYPE any, <f2> TYPE any, <f3> TYPE any.

    lt_paragraphs = it_paragraphs.

    CALL FUNCTION 'DDIF_FIELDINFO_GET'
      EXPORTING
        tabname   = 'ITCDP'
      TABLES
        dfies_tab = lt_fieldattr
      EXCEPTIONS
        OTHERS    = 1.

    var2 = 1.

    lt_para_table ?= cl_abap_typedescr=>describe_by_data( lt_paragraphs ).
    lt_para_struct ?= lt_para_table->get_table_line_type( ).
    lt_para_components = lt_para_struct->components.

    DATA: ls_para_attr TYPE ty_string_array.
    DATA: lt_para_standardattr TYPE STANDARD TABLE OF ty_string_array.
    DATA: lt_para_fontattr TYPE STANDARD TABLE OF ty_string_array.
    DATA: lt_para_outlineattr TYPE STANDARD TABLE OF ty_string_array.

************************ PARAGRAPHS Standard attribute *****************************
    ls_para_attr-name = 'TDPLDIST'.
    APPEND ls_para_attr TO lt_para_standardattr.
    ls_para_attr-name = 'TDPLDISTU'.
    APPEND ls_para_attr TO lt_para_standardattr.
    ls_para_attr-name = 'TDNOBLANKS'.
    APPEND ls_para_attr TO lt_para_standardattr.
    ls_para_attr-name = 'TDPTOP'.
    APPEND ls_para_attr TO lt_para_standardattr.
    ls_para_attr-name = 'TDPTOPU'.
    APPEND ls_para_attr TO lt_para_standardattr.
    ls_para_attr-name = 'TDPBOT'.
    APPEND ls_para_attr TO lt_para_standardattr.
    ls_para_attr-name = 'TDPBOTU'.
    APPEND ls_para_attr TO lt_para_standardattr.
    ls_para_attr-name = 'TDPLEFT'.
    APPEND ls_para_attr TO lt_para_standardattr.
    ls_para_attr-name = 'TDPLEFTU'.
    APPEND ls_para_attr TO lt_para_standardattr.
    ls_para_attr-name = 'TDPRIGHT'.
    APPEND ls_para_attr TO lt_para_standardattr.
    ls_para_attr-name = 'TDPRIGHTU'.
    APPEND ls_para_attr TO lt_para_standardattr.
    ls_para_attr-name = 'TDPENTRY'.
    APPEND ls_para_attr TO lt_para_standardattr.
    ls_para_attr-name = 'TDPENTRYU'.
    APPEND ls_para_attr TO lt_para_standardattr.
    ls_para_attr-name = 'TDPJUSTIFY'.
    APPEND ls_para_attr TO lt_para_standardattr.
    ls_para_attr-name = 'TDPPROTNEX'.
    APPEND ls_para_attr TO lt_para_standardattr.
    ls_para_attr-name = 'TDPPROTPAG'.
    APPEND ls_para_attr TO lt_para_standardattr.

************************ PARAGRAPHS Font attribute *********************************
    ls_para_attr-name = 'TDFAMILY'.
    APPEND ls_para_attr TO lt_para_fontattr.
    ls_para_attr-name = 'TDHEIGHT'.
    APPEND ls_para_attr TO lt_para_fontattr.
    ls_para_attr-name = 'TDBOLD'.
    APPEND ls_para_attr TO lt_para_fontattr.
    ls_para_attr-name = 'TDITALIC'.
    APPEND ls_para_attr TO lt_para_fontattr.
    ls_para_attr-name = 'TDUNDERLIN'.
    APPEND ls_para_attr TO lt_para_fontattr.
    ls_para_attr-name = 'TDULPOS'.
    APPEND ls_para_attr TO lt_para_fontattr.
    ls_para_attr-name = 'TDULPOSU'.
    APPEND ls_para_attr TO lt_para_fontattr.
    ls_para_attr-name = 'TDULTHICK'.
    APPEND ls_para_attr TO lt_para_fontattr.
    ls_para_attr-name = 'TDULTHICKU'.
    APPEND ls_para_attr TO lt_para_fontattr.
    ls_para_attr-name = 'TDULGREY'.
    APPEND ls_para_attr TO lt_para_fontattr.
    ls_para_attr-name = 'TDUNDERLIN'.
    APPEND ls_para_attr TO lt_para_fontattr.
************************ PARAGRAPHS outline attribute *********************************
    ls_para_attr-name = 'TDLFIRSTPA'.
    APPEND ls_para_attr TO lt_para_outlineattr.
    ls_para_attr-name = 'TDLDEPTH'.
    APPEND ls_para_attr TO lt_para_outlineattr.
    ls_para_attr-name = 'TDLCHAINED'.
    APPEND ls_para_attr TO lt_para_outlineattr.
    ls_para_attr-name = 'TDNUMBERIN'.
    APPEND ls_para_attr TO lt_para_outlineattr.
    ls_para_attr-name = 'TDNUMOUTL'.
    APPEND ls_para_attr TO lt_para_outlineattr.
    ls_para_attr-name = 'TDNUMFIXC'.
    APPEND ls_para_attr TO lt_para_outlineattr.
    ls_para_attr-name = 'TDLSTRING'.
    APPEND ls_para_attr TO lt_para_outlineattr.
    ls_para_attr-name = 'TDNUMLEFT'.
    APPEND ls_para_attr TO lt_para_outlineattr.
    ls_para_attr-name = 'TDNUMLEFTU'.
    APPEND ls_para_attr TO lt_para_outlineattr.
    ls_para_attr-name = 'TDLFIRSTC'.
    APPEND ls_para_attr TO lt_para_outlineattr.
    ls_para_attr-name = 'TDLLASTC'.
    APPEND ls_para_attr TO lt_para_outlineattr.

    LOOP AT lt_paragraphs INTO DATA(lv_paragraph).
      ASSIGN lv_paragraph TO <f1>.
      var1 = 0.
      MOVE lv_paragraph-tdpargraph TO ls_result-id.
      MOVE t_meaning TO ls_result-item.
      MOVE lv_paragraph-tdtext TO ls_result-value.
      var3 = var2.
      APPEND ls_result TO lt_results.
      CLEAR: ls_result-item, ls_result-value.
      ADD 1 TO var2.

************************ Standard attribute ****************************
      MOVE 'Standard attribute' TO ls_result-item.
      LOOP AT lt_para_standardattr INTO DATA(wa_stdattr_itcdp).
        ASSIGN wa_stdattr_itcdp-name TO <f2>.
        ASSIGN COMPONENT <f2> OF STRUCTURE <f1> TO <f3>.
        READ TABLE lt_fieldattr INTO DATA(wa_fieldattr) WITH KEY tabname = 'ITCDP'
                                    fieldname = <f2>
                                    langu = sy-langu.
        MOVE wa_fieldattr-scrtext_m TO ls_result-attr.
        MOVE <f3> TO ls_result-value.
        APPEND ls_result TO lt_results.
        CLEAR: ls_result-attr, ls_result-value.
        entry = abap_true.
        ADD 1 TO var2.
      ENDLOOP.

      CLEAR ls_result-item.
      MOVE 'Font Attributes' TO ls_result-item.
      entry = abap_false.
      LOOP AT lt_para_fontattr INTO DATA(wa_fontattr_itcdp).
        ASSIGN wa_fontattr_itcdp-name TO <f2>.
        ASSIGN COMPONENT <f2> OF STRUCTURE <f1> TO <f3>.
        READ TABLE lt_fieldattr INTO wa_fieldattr WITH KEY tabname = 'ITCDP'
                                    fieldname = <f2>
                                    langu = sy-langu.
        MOVE wa_fieldattr-scrtext_m TO ls_result-attr.

        MOVE <f3> TO ls_result-value.
        APPEND ls_result TO lt_results.
        CLEAR: ls_result-attr, ls_result-value.
        entry = abap_true.
        ADD 1 TO var2.
      ENDLOOP.
      IF entry = abap_true.
        ADD 1 TO var1.
      ENDIF.

      CLEAR ls_result-item.
      MOVE 'Outline Attributes' TO ls_result-item.
      entry = abap_false.
      LOOP AT lt_para_outlineattr INTO DATA(wa_outlineattr_itcdp).
        ASSIGN wa_outlineattr_itcdp-name TO <f2>.
        ASSIGN COMPONENT <f2> OF STRUCTURE <f1> TO <f3>.
        READ TABLE lt_fieldattr INTO wa_fieldattr WITH KEY tabname = 'ITCDP'
                                    fieldname = <f2>
                                    langu = sy-langu.
        MOVE wa_fieldattr-scrtext_m TO ls_result-attr.

        MOVE <f3> TO ls_result-value.
        APPEND ls_result TO lt_results.
        CLEAR: ls_result-attr, ls_result-value.
        entry = abap_true.
        ADD 1 TO var2.
      ENDLOOP.

      CLEAR ls_result-item.
      MOVE 'Tabs' TO ls_result-item.
      entry = abap_false.
      LOOP AT lt_tabs INTO DATA(lv_tabs) WHERE tdpargraph = lv_paragraph-tdpargraph.
        MOVE lv_tabs-tdtabpos TO ls_result-attr+0(8).
        MOVE lv_tabs-tdtabposu TO ls_result-attr+8(2).
        CONDENSE ls_result-attr.
        MOVE lv_tabs-tdtjustify TO ls_result-value.
        APPEND ls_result TO lt_results.
        CLEAR: ls_result-attr, ls_result-value.
        entry = abap_true.
        ADD 1 TO var2.
      ENDLOOP.

    ENDLOOP.
    IF entry = abap_true.
      ADD 1 TO var1.
    ENDIF.
    READ TABLE lt_results INTO ls_result INDEX var3.
    ls_result-vol = var2 - var3 + var1.
    MODIFY lt_results FROM ls_result INDEX var3.
    CLEAR ls_result-vol.

    APPEND '' TO cht_filecontent.
    LOOP AT lt_results INTO ls_result.
      IF sy-tabix = 1.
        APPEND |{ space WIDTH = 80 PAD = '-' }| TO cht_filecontent.
        APPEND |Paragraphs    Attributes| TO cht_filecontent.
        APPEND |{ space WIDTH = 80 PAD = '-' }| TO cht_filecontent.
        APPEND '' TO cht_filecontent.
      ENDIF.
      AT NEW id.
        APPEND '' TO cht_filecontent.
      ENDAT.

      AT NEW item.
        IF ls_result-item <> t_meaning.
          APPEND |{ ls_result-item WIDTH = 15 }| TO cht_filecontent.
        ENDIF.
      ENDAT.
      IF ls_result-item = t_meaning.
        APPEND |{ ls_result-id WIDTH = 12 }{ ls_result-value WIDTH = 15 }| TO cht_filecontent.
      ELSE.
        APPEND |{ ls_result-attr WIDTH = 18 }{ cl_abap_char_utilities=>horizontal_tab }{ ls_result-value WIDTH = 36 }| TO cht_filecontent.
      ENDIF.

    ENDLOOP.

  ENDMETHOD.

  METHOD get_text_windows_content.

    DATA: ls_window_attr TYPE ty_string_array.
    DATA: lt_window_attr TYPE STANDARD TABLE OF ty_string_array.
    DATA: lt_fieldattr              TYPE STANDARD TABLE OF dfies INITIAL SIZE 0.
    DATA: var1                      LIKE sy-index.
    DATA: var2                      LIKE sy-index.
    DATA: var3                      LIKE sy-index.
    DATA: lt_windows                TYPE tty_itctw.
    DATA: lt_results                TYPE tty_result.
    DATA: ls_result                 TYPE ts_result.
    DATA: entry.

    FIELD-SYMBOLS: <f1> TYPE any, <f2> TYPE any, <f3> TYPE any.

    lt_windows = it_windows.
************************ Window attribute *****************************
    ls_window_attr-name = 'TDWTYPE'.
    APPEND ls_window_attr TO lt_window_attr.
    ls_window_attr-name = 'TDFIRSTPAR'.
    APPEND ls_window_attr TO lt_window_attr.
    ls_window_attr-name = 'TDWTYPE'.
    APPEND ls_window_attr TO lt_window_attr.


    CALL FUNCTION 'DDIF_FIELDINFO_GET'
      EXPORTING
        tabname   = 'ITCTW'
      TABLES
        dfies_tab = lt_fieldattr
      EXCEPTIONS
        OTHERS    = 1.

    var2 = 1.

    LOOP AT lt_windows INTO DATA(lv_window).
      ASSIGN lv_window TO <f1>.
      MOVE lv_window-tdwindow TO ls_result-id.
      MOVE t_meaning TO ls_result-item.
      MOVE lv_window-tdtext TO ls_result-value.
      var3 = var2.
      APPEND ls_result TO lt_results.
      CLEAR: ls_result-item, ls_result-value.
      ADD 1 TO var2.

      LOOP AT lt_window_attr INTO DATA(wa_window_attr_itctw).
        ASSIGN wa_window_attr_itctw-name TO <f2>.
        ASSIGN COMPONENT <f2> OF STRUCTURE <f1> TO <f3>.
        READ TABLE lt_fieldattr INTO DATA(wa_fieldattr) WITH KEY tabname = 'ITCTW'
                                    fieldname = <f2>
                                    langu = sy-langu.
        MOVE wa_fieldattr-scrtext_m TO ls_result-attr.
        MOVE <f3> TO ls_result-value.
        APPEND ls_result TO lt_results.
        CLEAR: ls_result-attr, ls_result-value.
        entry = abap_true.
        ADD 1 TO var2.
      ENDLOOP.

      READ TABLE lt_results INTO ls_result INDEX var3.
      ls_result-vol = var2 - var3 + var1.
      MODIFY lt_results FROM ls_result INDEX var3.
      CLEAR ls_result-vol.

    ENDLOOP.

    APPEND '' TO cht_filecontent.

    LOOP AT lt_results INTO ls_result.
      IF sy-tabix = 1.
        APPEND |{ space WIDTH = 80 PAD = '-' }| TO cht_filecontent.
        APPEND |Windows       Attributes| TO cht_filecontent.
        APPEND |{ space WIDTH = 80 PAD = '-' }| TO cht_filecontent.
        APPEND '' TO cht_filecontent.
      ENDIF.
      AT NEW id.
        APPEND '' TO cht_filecontent.
      ENDAT.

      AT NEW item.
        IF ls_result-item <> t_meaning.
*          APPEND |{ ls_result-id WIDTH = 3 }{ ls_result-value WIDTH = 40 }| TO et_filecontent.
*        ELSE.
          APPEND |{ ls_result-item WIDTH = 15 }| TO cht_filecontent.
        ENDIF.
      ENDAT.
      IF ls_result-item = t_meaning.
        APPEND |{ ls_result-id WIDTH = 12 }{ ls_result-value WIDTH = 15 }| TO cht_filecontent.
      ELSE.
        APPEND |{ ls_result-attr WIDTH = 18 }{ cl_abap_char_utilities=>horizontal_tab }{ ls_result-value WIDTH = 36 }| TO cht_filecontent.
      ENDIF.

    ENDLOOP.

  ENDMETHOD.

  METHOD get_text_page_content.
    TYPES: BEGIN OF ty_page_array,
             name TYPE string,
           END OF ty_page_array.
    DATA:
      lt_fieldattr               TYPE STANDARD TABLE OF dfies INITIAL SIZE 0,
      lt_fieldattr2              TYPE STANDARD TABLE OF dfies INITIAL SIZE 0,
      lt_pages                   TYPE tty_itctg,
      lt_page_windows            TYPE tty_itcth,
      wa_fieldattr               TYPE dfies,
      var1                       LIKE sy-index,
      var2                       LIKE sy-index,
      var3                       LIKE sy-index,
      lt_page_table              TYPE REF TO   cl_abap_tabledescr,
      lt_page_struct             TYPE REF TO   cl_abap_structdescr,
      lt_page_components         TYPE          abap_compdescr_tab,
      lt_page_windows_table      TYPE REF TO   cl_abap_tabledescr,
      lt_page_windows_struct     TYPE REF TO   cl_abap_structdescr,
      lt_page_windows_components TYPE          abap_compdescr_tab.
    DATA: ls_page_attr          TYPE ty_page_array.
    DATA: lt_page_printattr     TYPE STANDARD TABLE OF ty_page_array.
    DATA: lt_page_counter       TYPE STANDARD TABLE OF ty_page_array.
    DATA: lt_page_sidewindow    TYPE STANDARD TABLE OF ty_page_array.
    DATA: lt_results TYPE tty_result.
    DATA: ls_result TYPE ts_result.
    DATA: entry.
    FIELD-SYMBOLS : <fs_components> TYPE abap_compdescr.
    FIELD-SYMBOLS: <f1> TYPE any, <f2> TYPE any, <f3> TYPE any.

    lt_pages = it_pages.
    lt_page_windows = it_page_windows.
    lt_page_table ?= cl_abap_typedescr=>describe_by_data( lt_pages ).
    lt_page_struct ?= lt_page_table->get_table_line_type( ).
    lt_page_components = lt_page_struct->components.
    lt_page_windows_table ?= cl_abap_typedescr=>describe_by_data( lt_page_windows ).
    lt_page_windows_struct ?= lt_page_table->get_table_line_type( ).
    lt_page_windows_components = lt_page_struct->components.

    CALL FUNCTION 'DDIF_FIELDINFO_GET'
      EXPORTING
        tabname   = 'ITCTG'
      TABLES
        dfies_tab = lt_fieldattr
      EXCEPTIONS
        OTHERS    = 0.

    CALL FUNCTION 'DDIF_FIELDINFO_GET'
      EXPORTING
        tabname   = 'ITCTH'
      TABLES
        dfies_tab = lt_fieldattr2
      EXCEPTIONS
        OTHERS    = 0.

    var2 = 1.

************************ Print attribute *****************************
    ls_page_attr-name = 'TDPAPERRES'.
    APPEND ls_page_attr TO lt_page_printattr.
    ls_page_attr-name = 'TDPRINTMOD'.
    APPEND ls_page_attr TO lt_page_printattr.
*************************** page counter *******************************
    ls_page_attr-name = 'TDCMODEPAG'.
    APPEND ls_page_attr TO lt_page_counter.
    ls_page_attr-name = 'TDNTYPEPAG'.
    APPEND ls_page_attr TO lt_page_counter.
    ls_page_attr-name = 'TDPAGOUTL'.
    APPEND ls_page_attr TO lt_page_counter.
*************************** side window ******************************
    ls_page_attr-name = 'TDWLEFT'.
    APPEND ls_page_attr TO lt_page_sidewindow.
    ls_page_attr-name = 'TDWTOP'.
    APPEND ls_page_attr TO lt_page_sidewindow.
    ls_page_attr-name = 'TDWWIDTH'.
    APPEND ls_page_attr TO lt_page_sidewindow.
    ls_page_attr-name = 'TDWHEIGHT'.
    APPEND ls_page_attr TO lt_page_sidewindow.


    LOOP AT lt_pages INTO DATA(lv_page).
      ASSIGN lv_page TO <f1>.

      var1 = 0.
      MOVE lv_page-tdpage TO ls_result-id.
      MOVE t_meaning TO ls_result-item.
      MOVE lv_page-tdtext TO ls_result-value.
      var3 = var2.
      APPEND ls_result TO lt_results.
      CLEAR: ls_result-item, ls_result-value.
      ADD 1 TO var2.
************************ Standard attribute *****************************
      IF NOT lv_page-tdnextpage IS INITIAL.
        MOVE t_standard TO ls_result-item.
        MOVE 'Next Page' TO ls_result-attr.
        MOVE lv_page-tdnextpage TO ls_result-value.
        APPEND ls_result TO lt_results.
        CLEAR: ls_result-item, ls_result-value.
        ADD 1 TO var2.
      ENDIF.
************************* Print attribute ******************************
      MOVE t_print TO ls_result-item.
      entry = abap_false.
      LOOP AT lt_page_printattr INTO DATA(wa_page_printattr_itcdp).
        ASSIGN wa_page_printattr_itcdp-name TO <f2>.
        ASSIGN COMPONENT <f2> OF STRUCTURE <f1> TO <f3>.

        READ TABLE lt_fieldattr INTO wa_fieldattr WITH KEY tabname = 'ITCTG'
                                    fieldname = <f2>
                                    langu = sy-langu.

        MOVE wa_fieldattr-scrtext_m TO ls_result-attr.

        MOVE <f3> TO ls_result-value.
        APPEND ls_result TO lt_results.
        CLEAR: ls_result-attr, ls_result-value.
        entry = abap_true.
        ADD 1 TO var2.
        CLEAR wa_fieldattr.
      ENDLOOP.

*************************** page counter *******************************
      MOVE t_pagecounter TO ls_result-item.
      entry = abap_false.

      LOOP AT lt_page_counter INTO DATA(wa_page_counter_itcdp).
        ASSIGN wa_page_counter_itcdp-name TO <f2>.
        ASSIGN COMPONENT <f2> OF STRUCTURE <f1> TO <f3>.

        READ TABLE lt_fieldattr INTO wa_fieldattr WITH KEY tabname = 'ITCTG'
                                    fieldname = <f2>
                                    langu = sy-langu.

        MOVE wa_fieldattr-scrtext_m TO ls_result-attr.
        MOVE <f3> TO ls_result-value.
        APPEND ls_result TO lt_results.
        CLEAR: ls_result-attr, ls_result-value.
        entry = abap_true.
        ADD 1 TO var2.
        CLEAR wa_fieldattr.
      ENDLOOP.
      IF entry = abap_true.
        ADD 1 TO var1.
      ENDIF.

*************************** side window ******************************
      CLEAR ls_result-item.
      MOVE t_pagewins TO ls_result-item.
      APPEND ls_result TO lt_results.
      CLEAR ls_result-item.
      ADD 1 TO var2.
      entry = abap_false.

      LOOP AT lt_page_windows INTO DATA(lv_page_window) WHERE tdpage = lv_page-tdpage.
        MOVE t_title TO ls_result-item.
        MOVE lv_page_window-tdwindow TO ls_result-attr.

        READ TABLE lt_fieldattr2 INTO DATA(lv_tdwleft_itcth) WITH KEY tabname = 'ITCTH'
                                      fieldname = 'TDWLEFT'
                                      langu = sy-langu.
        MOVE lv_tdwleft_itcth-scrtext_m TO ls_result-value+3(20).
        MOVE lv_page_window-tdwleft TO ls_result-value+23(6).
        MOVE lv_page_window-tdwleftu TO ls_result-value+30(2).
        APPEND ls_result TO lt_results.
        CLEAR ls_result-item.
        MOVE t_after_title TO ls_result-item.
        ADD 1 TO var2.

        READ TABLE lt_fieldattr2 INTO DATA(lv_tdwtop_itcth) WITH KEY tabname = 'ITCTH'
                                      fieldname = 'TDWTOP'
                                      langu = sy-langu.
        MOVE lv_tdwtop_itcth-scrtext_m TO ls_result-value+3(20).
        MOVE lv_page_window-tdwtop TO ls_result-value+23(6).
        MOVE lv_page_window-tdwtopu TO ls_result-value+30(2).
        APPEND ls_result TO lt_results.
        CLEAR ls_result-item.
        MOVE t_after_title TO ls_result-item.
        ADD 1 TO var2.

        READ TABLE lt_fieldattr2 INTO DATA(lv_tdwwidth_itcth) WITH KEY tabname = 'ITCTH'
                                      fieldname = 'TDWWIDTH'
                                      langu = sy-langu.
        MOVE lv_tdwwidth_itcth-scrtext_m TO ls_result-value+3(20).
        MOVE lv_page_window-tdwwidth TO ls_result-value+23(6).
        MOVE lv_page_window-tdwwidthu TO ls_result-value+30(2).
        APPEND ls_result TO lt_results.
        CLEAR ls_result-item.
        MOVE t_after_title TO ls_result-item.
        ADD 1 TO var2.

        READ TABLE lt_fieldattr2 INTO DATA(lv_tdwheight_itcth) WITH KEY tabname = 'ITCTH'
                                      fieldname = 'TDWHEIGHT'
                                      langu = sy-langu.
        MOVE lv_tdwheight_itcth-scrtext_m TO ls_result-value+3(20).
        MOVE lv_page_window-tdwheight TO ls_result-value+23(6).
        MOVE lv_page_window-tdwheightu TO ls_result-value+30(2).
        APPEND ls_result TO lt_results.
        CLEAR ls_result-item.
        ADD 1 TO var2.

        CLEAR: ls_result-item, ls_result-attr.
        entry = abap_true.
      ENDLOOP.
      IF entry = abap_true.
        ADD 1 TO var1.
      ELSE.
        SUBTRACT 1 FROM var2.
        DELETE lt_results INDEX var2.
      ENDIF.

      READ TABLE lt_results INTO ls_result INDEX var3.
      ls_result-vol = var2 - var3 + var1.
      MODIFY lt_results FROM ls_result INDEX var3.
      CLEAR ls_result-vol.
    ENDLOOP.

    LOOP AT lt_results INTO ls_result.
      IF sy-tabix = 1.
        APPEND |{ space WIDTH = 80 PAD = '-' }| TO cht_filecontent.
        APPEND |Pages         Attributes| TO cht_filecontent.
        APPEND |{ space WIDTH = 80 PAD = '-' }| TO cht_filecontent.
        APPEND '' TO cht_filecontent.
      ENDIF.
      AT NEW id.
        APPEND '' TO cht_filecontent.
      ENDAT.

      AT NEW item.
        CASE ls_result-item.
          WHEN t_meaning.
            APPEND |{ ls_result-id WIDTH = 3 }| TO cht_filecontent.
          WHEN t_pagecounter.
            APPEND |{ 'Page Counter' WIDTH = 15 }| TO cht_filecontent.
            APPEND |{ space WIDTH = 80 PAD = '-' }| TO cht_filecontent.
          WHEN t_print.
            APPEND |{ 'Print Attributes' WIDTH = 15 }| TO cht_filecontent.
            APPEND |{ space WIDTH = 80 PAD = '-' }| TO cht_filecontent.
          WHEN t_pagewins.
            APPEND |{ 'Page Window' WIDTH = 15 }| TO cht_filecontent.
            APPEND |{ space WIDTH = 80 PAD = '-' }| TO cht_filecontent.
          WHEN t_title.
            READ TABLE lt_results INTO DATA(lv_title) INDEX sy-tabix.
            APPEND |{ lv_title-attr WIDTH = 18 }| TO cht_filecontent.
        ENDCASE.
      ENDAT.

      CASE ls_result-item.
        WHEN t_meaning.
        WHEN t_title.
        WHEN t_after_title.
          APPEND |{ lv_title-value WIDTH = 36 }| TO cht_filecontent.
        WHEN t_standard.
          APPEND |{ 'Standard Attributes' WIDTH = 15 }| TO cht_filecontent.
          APPEND |{ space WIDTH = 80 PAD = '-' }| TO cht_filecontent.
          APPEND |{ ls_result-attr WIDTH = 18 }{ cl_abap_char_utilities=>horizontal_tab }{ ls_result-value WIDTH = 36 }| TO cht_filecontent.
        WHEN OTHERS.
          APPEND |{ ls_result-attr WIDTH = 18 }{ cl_abap_char_utilities=>horizontal_tab }{ ls_result-value WIDTH = 36 }| TO cht_filecontent.
      ENDCASE.

    ENDLOOP.

  ENDMETHOD.

  METHOD get_text_txtelement_content.
    TYPES: BEGIN OF ty_intertab,
             flag(1),
             content(135),
           END OF ty_intertab.
    TYPES: tty_intertab TYPE TABLE OF ty_intertab.

    DATA: ls_intertab       TYPE ty_intertab.
    DATA: lt_intertab       TYPE tty_intertab.
    DATA: prev_flag,
          start.
    DATA: tmpwindow         TYPE itctw-tdwindow.
    DATA: lt_windows        TYPE tty_itctw.
    DATA lt_form_lines     TYPE tty_tline.

    lt_windows = it_windows.
    lt_form_lines = it_form_lines.

    APPEND '' TO cht_filecontent.

    DATA(subtitle) = abap_false.
    LOOP AT lt_windows INTO DATA(wa_window).
      CLEAR ls_intertab-content.
      ls_intertab-flag = 'O'.
      REFRESH lt_intertab.
      start = abap_false.
      LOOP AT lt_form_lines ASSIGNING FIELD-SYMBOL(<fsline>).
        CASE <fsline>-tdformat.
          WHEN '/W'.
            tmpwindow = <fsline>-tdline.
            IF tmpwindow = wa_window-tdwindow.
              IF subtitle = abap_false.
                subtitle = abap_true.
                APPEND |{ space WIDTH = 80 PAD = '-' }| TO cht_filecontent.
                APPEND |Text Elements for Following Windows: | TO cht_filecontent.
                APPEND |{ space WIDTH = 80 PAD = '-' }| TO cht_filecontent.
                APPEND '' TO cht_filecontent.
              ENDIF.
              start = abap_true.
              ls_intertab-flag = 'T'.
              ls_intertab-content = tmpwindow.
              prev_flag = ls_intertab-flag.
              APPEND ls_intertab TO lt_intertab.
              CLEAR ls_intertab-content.
            ELSE.
              IF start = abap_true.
                EXIT.
              ENDIF.
            ENDIF.
          WHEN '/E'.
            IF start = abap_true.
              CLEAR ls_intertab-content.
              APPEND ls_intertab TO lt_intertab.
              ls_intertab-flag = 'E'.
              MOVE <fsline>-tdformat TO ls_intertab-content(10).
              MOVE <fsline>-tdline TO ls_intertab-content+10(59).
              CONDENSE ls_intertab-content.
              prev_flag = ls_intertab-flag.
              APPEND ls_intertab TO lt_intertab.
              CLEAR ls_intertab-content.
            ENDIF.
          WHEN OTHERS.
            IF start = abap_true.
              IF prev_flag = 'T'.
                CLEAR ls_intertab-content.
                APPEND ls_intertab TO lt_intertab.
              ENDIF.
              ls_intertab-flag = 'O'.
              MOVE <fsline>-tdformat TO ls_intertab-content(2).
              MOVE <fsline>-tdline TO ls_intertab-content+2(132).
              prev_flag = ls_intertab-flag.
              APPEND ls_intertab TO lt_intertab.
              CLEAR ls_intertab-content.
            ENDIF.
        ENDCASE.
      ENDLOOP.

      LOOP AT lt_intertab INTO DATA(wa_intertab).
        CASE wa_intertab-flag.
          WHEN 'T'.
            IF wa_intertab-content IS INITIAL.
              SKIP.
            ELSE.
              APPEND |{ wa_intertab-content WIDTH = 255 }| TO cht_filecontent.

            ENDIF.
          WHEN 'E'.
            APPEND |{ wa_intertab-content WIDTH = 255 }| TO cht_filecontent.
          WHEN 'O'.
            APPEND |{ wa_intertab-content(2)  } { wa_intertab-content+2 }| TO cht_filecontent.

          WHEN OTHERS.
            SKIP.
        ENDCASE.
      ENDLOOP.
    ENDLOOP.

  ENDMETHOD.

ENDCLASS.