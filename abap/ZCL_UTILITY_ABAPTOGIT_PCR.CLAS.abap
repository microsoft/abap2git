" Utility class for HR/payroll PCR/schema
class ZCL_UTILITY_ABAPTOGIT_PCR definition
  public
  final
  create public .

public section.

    " constructor
    " io_objtelemetry - class object for telemetry
    " iv_methtelemetry - method name for telemetry
    " for telemetry, the method will be invoked with parameters
    " iv_message as string (for message content) and iv_kind as string (for category)
    METHODS constructor
      IMPORTING
        io_objtelemetry  TYPE REF TO object OPTIONAL
        iv_methtelemetry TYPE string OPTIONAL.

    " fetch HR/payroll schema/PCR language code lines into files
    " iv_folder - local folder name to save the files
    " iv_folder_structure - 'flat' or 'eclipse'
    METHODS get_hr_schemapcrs
      IMPORTING
                iv_folder           TYPE string
                iv_folder_structure TYPE string DEFAULT 'eclipse'
      RETURNING VALUE(rv_success)   TYPE abap_bool.

    " build PCR/schema content
    " iv_objname - object name
    " iv_objtype - object type
    " it_trobjcontent - TR object file content
    " it_commit_objects - TR object list
    METHODS build_pcrschema_content
      IMPORTING
                iv_objname          TYPE string
                iv_objtype          TYPE string
      CHANGING
                it_trobjcontent     TYPE zcl_utility_abaptogit_tr=>tty_abaptext
                it_commit_objects   TYPE zcl_utility_abaptogit_tr=>tty_commit_object.

protected section.
private section.

    " telemetry callback
    DATA oref_telemetry TYPE REF TO object.
    DATA method_name_telemetry TYPE string.

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
        et_filecontent TYPE zcl_utility_abaptogit_tr=>tty_abaptext.

    " construct HR/payroll personnel calculation rule code content
    " iv_pcrname - PCR name
    " et_filecontent - PCR code content lines
    METHODS build_pcr_content_active
      IMPORTING
        iv_pcrname     TYPE string
      EXPORTING
        et_filecontent TYPE zcl_utility_abaptogit_tr=>tty_abaptext.

    " wrapper to write telemetry with the callback registered
    METHODS write_telemetry
      IMPORTING
        iv_message TYPE string
        iv_kind    TYPE string DEFAULT 'error'.

ENDCLASS.



CLASS ZCL_UTILITY_ABAPTOGIT_PCR IMPLEMENTATION.

  METHOD constructor.

    IF io_objtelemetry IS SUPPLIED.
      me->oref_telemetry = io_objtelemetry.
    ENDIF.

    IF iv_methtelemetry IS SUPPLIED.
      me->method_name_telemetry = iv_methtelemetry.
    ENDIF.

  ENDMETHOD.


  METHOD get_hr_schemapcrs.
    DATA wacode TYPE zcl_utility_abaptogit=>ts_code_object.
    DATA lt_schemas TYPE STANDARD TABLE OF zcl_utility_abaptogit=>ts_code_object.
    DATA lt_pcrs TYPE STANDARD TABLE OF zcl_utility_abaptogit=>ts_code_object.
    DATA lt_filecontent TYPE zcl_utility_abaptogit_tr=>tty_abaptext.
    DATA lv_basefolder TYPE string.
    DATA lv_commit_object TYPE zcl_utility_abaptogit_tr=>ts_commit_object.
    DATA lv_code_name TYPE string.
    DATA lv_path TYPE string.
    DATA lv_folder TYPE rlgrap-filename.
    DATA lv_codefolder TYPE string.
    DATA lv_filefolder TYPE string.
    DATA lt_filefolders TYPE TABLE OF string WITH DEFAULT KEY.
    DATA lv_success TYPE abap_bool.
    DATA lv_schpcrname TYPE string.
    DATA lv_progcls TYPE t52ba-pwert.

    IF iv_folder NP '*\'.
      lv_basefolder = iv_folder && '\'.
    ELSE.
      lv_basefolder = iv_folder.
    ENDIF.

    " root folder for a package like \src\.schemapcr\
    lv_folder = |{ lv_basefolder }{ zcl_utility_abaptogit_tr=>c_schemapcr }{ zcl_utility_abaptogit=>c_delim }|.
    lv_codefolder = lv_folder.
    CALL FUNCTION 'GUI_CREATE_DIRECTORY'
      EXPORTING
        dirname = lv_folder
      EXCEPTIONS
        failed  = 1
        OTHERS = 2.
    IF sy-subrc <> 0.
      me->write_telemetry( iv_message = |GET_HR_SCHEMAPCRS creates existing folder { lv_folder }| ).
    ENDIF.

    " download schemas
    SELECT sname, 'PSCC', 'PSCC', ' ' INTO TABLE @lt_schemas FROM t52cc WHERE sname LIKE 'Z%' OR sname LIKE 'Y%'.

    LOOP AT lt_schemas INTO wacode.

      CLEAR lt_filecontent.

      lv_schpcrname = wacode-obj_name.
      me->build_schema_content_active(
          EXPORTING
              iv_schemaname = lv_schpcrname
          IMPORTING
              et_filecontent = lt_filecontent
               ).
      SELECT SINGLE pwert FROM t52ba INTO @lv_progcls WHERE potyp = 'SCHE' AND ponam = @lv_schpcrname AND pattr = 'PCL'. "#EC WARNOK

      lv_commit_object-objname = wacode-obj_name.
      lv_commit_object-objtype = 'PSCC'.
      lv_commit_object-progcls = lv_progcls.
      lv_code_name = zcl_utility_abaptogit_ado=>build_code_name(
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
            failed = 1
            OTHERS  = 2. "#EC FB_RC
        IF sy-subrc <> 0.
          me->write_telemetry( iv_message = |GET_HR_SCHEMAPCRS creates existing folder { lv_folder }| ).
        ENDIF.
      ENDIF.
      lv_path = |{ lv_basefolder }{ zcl_utility_abaptogit_tr=>c_schemapcr }{ zcl_utility_abaptogit=>c_delim }{ lv_code_name }|.
      CALL FUNCTION 'GUI_DOWNLOAD'
        EXPORTING
          filename              = lv_path
          filetype              = 'ASC'
          write_field_separator = 'X'
        TABLES
          data_tab              = lt_filecontent
        EXCEPTIONS
          OTHERS                = 1.
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
      me->build_pcr_content_active(
          EXPORTING
              iv_pcrname = lv_schpcrname
          IMPORTING
              et_filecontent = lt_filecontent
               ).
      SELECT SINGLE pwert FROM t52ba INTO @lv_progcls WHERE potyp = 'CYCL' AND ponam = @lv_schpcrname AND pattr = 'PCL'. "#EC WARNOK

      lv_commit_object-objname = wacode-obj_name.
      lv_commit_object-objtype = 'PCYC'.
      lv_commit_object-progcls = lv_progcls.
      lv_code_name = zcl_utility_abaptogit_ado=>build_code_name(
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
            failed = 1
            OTHERS  = 2. "#EC FB_RC
        IF sy-subrc <> 0.
          me->write_telemetry( iv_message = |GET_HR_SCHEMAPCRS creates existing folder { lv_folder }| ).
        ENDIF.
      ENDIF.
      lv_path = |{ lv_basefolder }{ zcl_utility_abaptogit_tr=>c_schemapcr }{ zcl_utility_abaptogit=>c_delim }{ lv_code_name }|.

      CALL FUNCTION 'GUI_DOWNLOAD'
        EXPORTING
          filename              = lv_path
          filetype              = 'ASC'
          write_field_separator = 'X'
        TABLES
          data_tab              = lt_filecontent
        EXCEPTIONS
          OTHERS                = 1.
      IF sy-subrc <> 0.
        me->write_telemetry( iv_message = |GET_HR_SCHEMAPCRS fails to save local file { lv_path }| ).
        rv_success = abap_false.
      ENDIF.

    ENDLOOP.

  ENDMETHOD.


  METHOD build_pcrschema_content.

      DATA lt_filecontent TYPE zcl_utility_abaptogit_tr=>tty_abaptext.
      DATA lv_cdate TYPE cdate.
      DATA lv_udate TYPE aedat.
      DATA lv_progcls TYPE t52ba-pwert.
      DATA lv_version_no TYPE i.
      DATA lv_filecontent TYPE string.
      DATA lv_filelines TYPE i.
      DATA lv_objname TYPE string.
      lv_objname = iv_objname.

      IF iv_objtype = 'PSCC'.
          me->build_schema_content_active(
              EXPORTING
                  iv_schemaname = iv_objname
              IMPORTING
                  et_filecontent = lt_filecontent
                   ).
          SELECT SINGLE cdate FROM t52cc INTO @lv_cdate WHERE sname = @iv_objname.
          SELECT SINGLE udate FROM t52cc INTO @lv_udate WHERE sname = @iv_objname.
          SELECT SINGLE pwert FROM t52ba INTO @lv_progcls WHERE potyp = 'SCHE' AND ponam = @iv_objname AND pattr = 'PCL'. "#EC WARNOK
      ELSE.
          me->build_pcr_content_active(
              EXPORTING
                  iv_pcrname = iv_objname
              IMPORTING
                  et_filecontent = lt_filecontent
                   ).
          SELECT SINGLE cdate FROM t52ce INTO @lv_cdate WHERE cname = @iv_objname.
          SELECT SINGLE udate FROM t52ce INTO @lv_udate WHERE cname = @iv_objname.
          SELECT SINGLE pwert FROM t52ba INTO @lv_progcls WHERE potyp = 'CYCL' AND ponam = @iv_objname AND pattr = 'PCL'. "#EC WARNOK
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

      lv_filelines = lines( lt_filecontent ).
      it_trobjcontent[ lines( it_trobjcontent ) ] = it_trobjcontent[ lines( it_trobjcontent ) ]-line && |, lines { lv_filelines }|.

      TRANSLATE lv_objname TO UPPER CASE.

      APPEND VALUE zcl_utility_abaptogit_tr=>ts_commit_object(
          devclass = zcl_utility_abaptogit_tr=>c_schemapcr
          objname = lv_objname
          objtype = iv_objtype
          objtype2 = iv_objtype
          fugr = ''
          progcls = lv_progcls
          delflag = abap_false
          verno = lv_version_no
          filecontent = lv_filecontent
          ) TO it_commit_objects.

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
*    APPEND |* Last Changed By       : { ls_desc-uname }| TO et_filecontent.
    APPEND |* Last Changed Date     : { ls_desc-udate }| TO et_filecontent.
    APPEND |* Last Changed Time     : { ls_desc-utime }| TO et_filecontent.

    " fetch schema instructions' comments
    SELECT * FROM t52c3 INTO TABLE @lt_comments
        WHERE schem = @iv_schemaname AND spras = @zcl_utility_abaptogit_tr=>c_en.

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
    SELECT SINGLE ltext FROM t52ce_t INTO @lv_ltext WHERE cname = @iv_pcrname AND sprsl = @zcl_utility_abaptogit_tr=>c_en.

    " meta data for this PCR, don't want to bother to add an additional XML/JSON description file
    APPEND '* PCR header part is auto generated and not part of engineer changes.' TO et_filecontent.
    APPEND |* PCR                   : { iv_pcrname }| TO et_filecontent.
    APPEND |* Description           : { lv_ltext }| TO et_filecontent.
    APPEND |* Country Grouping      : { lv_cnt }| TO et_filecontent.
    APPEND |* Owner                 : { ls_desc-respu }| TO et_filecontent.
    APPEND |* Creation Date         : { ls_desc-cdate }| TO et_filecontent.
    APPEND |* Only Changed by Owner : { ls_desc-relea }| TO et_filecontent.
*    APPEND |* Last Changed By       : { ls_desc-uname }| TO et_filecontent.
    APPEND |* Last Changed Date     : { ls_desc-udate }| TO et_filecontent.
    APPEND |* Last Changed Time     : { ls_desc-utime }| TO et_filecontent.

    " fetch PCR instructions
    lv_index = 1.
    SELECT * FROM t52c5 INTO @DATA(wa_instr) WHERE ccycl = @iv_pcrname.
      IF lv_first = abap_true OR wa_instr-abart <> lv_esg OR wa_instr-lgart <> lv_wgt.
        APPEND '' TO et_filecontent.
        " show ES group and wage type
*        APPEND |* ES Group { wa_instr-abart }, Wage/Time Type { wa_instr-lgart }| TO et_filecontent.
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