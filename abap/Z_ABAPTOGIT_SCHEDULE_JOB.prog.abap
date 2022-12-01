*&---------------------------------------------------------------------*
*& Report z_abaptogit_schedule_job
*&---------------------------------------------------------------------*
*& This program will schedule job based on how many days in advance
*& and what the interval in hours you want to schedule the job
*& for example co_schedule_days is 1 days in advance
*& and co_interval_in_hour is 1. It means we will schedule a job to run every hour
*& in next 24 hours(1 day)
*&---------------------------------------------------------------------*
REPORT z_abaptogit_schedule_job.

" TODO: specify organization for ADO REST API
DATA lv_orgid TYPE string.

" TODO: specify Git repository ID for ADO REST API
DATA lv_repoid TYPE string.

" TODO: specify project name for ADO REST API
DATA lv_project TYPE string.

" TODO: specify the Git branch name prefix, usually users/system/,
" for the branch holding the ABAP objects
DATA lv_baseprefix TYPE string.

" TODO: specify user name for ADO REST API
DATA lv_username TYPE string.

" TODO: specify personal access token of the user name above for ADO REST API
DATA lv_pat TYPE string.

CONSTANTS co_interval_in_hour TYPE i VALUE 1.
CONSTANTS co_times_in_one_day TYPE i VALUE 24.
CONSTANTS co_schedule_days TYPE i VALUE 28. " 4 weeks 4 * 7 = 28

TYPES : BEGIN OF schedule,
          sdate TYPE btcsdate,
          stime TYPE t,
        END OF schedule.

DATA:
  gv_package_names TYPE string,
  gt_values_tab    TYPE zvales_tab,
  gv_orgid         TYPE string,
  gv_repoid        TYPE string,
  gv_project       TYPE string,
  gv_baseprefix    TYPE string,
  gv_username      TYPE string,
  gv_pat           TYPE string.

PERFORM f_initiliaztion.
PERFORM f_main.

FORM f_main.
  DATA lv_query                     TYPE string.
  DATA lv_current_timestamp         TYPE timestamp.
  DATA lv_last_job_timestamp        TYPE timestamp.
  DATA rv_secs                      TYPE tzntstmps. " intentional use TZNTSTMPS to ignore after decimal
  DATA lv_schedule_start            TYPE timestamp.
  DATA lv_current_time_string(6)    TYPE c.
  DATA lv_start_time_string(6)      TYPE c.
  DATA lv_two_day_ago               TYPE sy-datum.
  DATA lv_system_date               TYPE btcsdate.
  DATA lv_system_time               TYPE btcstime.
  DATA lv_schedule_hour_str(2)      TYPE c.
  DATA lv_start_time                TYPE i.
  DATA lv_start_time_offset         TYPE i.
  DATA lv_start_date_offset         TYPE i.

  DATA lv_scheduled_time            TYPE t.
  DATA lv_scheduled_date            TYPE btcsdldate.
  DATA:
    wa_schedule TYPE schedule,
    itab        TYPE TABLE OF schedule.

  lv_system_date = sy-datum.
  lv_system_time = sy-uzeit.

  DATA(lv_loop_times) = co_times_in_one_day / co_interval_in_hour * co_schedule_days + 1.

  DO lv_loop_times TIMES.
    " Calculate scheduled time
    lv_current_time_string = lv_system_time.
    CONCATENATE lv_current_time_string(2) '0000' INTO lv_start_time_string.
    lv_start_time = lv_current_time_string(2).

    lv_start_time_offset = ( lv_start_time + ( sy-index - 1 ) * co_interval_in_hour ) MOD 24.
    UNPACK lv_start_time_offset TO lv_schedule_hour_str. " 0 to 00
    CONCATENATE lv_schedule_hour_str '0000' INTO lv_scheduled_time.

    " Calculate scheduled date
    lv_start_date_offset = ( lv_start_time + ( sy-index - 1 ) * co_interval_in_hour ) DIV 24.
    lv_scheduled_date = lv_system_date + lv_start_date_offset.

    wa_schedule-sdate = lv_scheduled_date.
    wa_schedule-stime = lv_scheduled_time.

    APPEND wa_schedule TO itab.
  ENDDO.

  lv_two_day_ago = sy-datum - 2.

  SELECT sdlstrtdt, sdlstrttm
  FROM   tbtco
  WHERE  status = 'S'  " S is released
  AND jobname = @co_title
  AND sdlstrtdt > @lv_two_day_ago
  ORDER BY sdlstrtdt DESCENDING, sdlstrttm  DESCENDING
  INTO  @DATA(ws_job)
  UP TO 1 ROWS.
  ENDSELECT.

  " If there is job exists
  IF ws_job-sdlstrtdt IS NOT INITIAL AND ws_job-sdlstrttm IS NOT INITIAL.
    DELETE itab WHERE ( sdate < ws_job-sdlstrtdt OR ( sdate = ws_job-sdlstrtdt AND stime <= ws_job-sdlstrttm ) ).
  ENDIF.

  LOOP AT itab INTO DATA(schedule_item).
    PERFORM schedule_catchup_abaptogit
        USING   schedule_item-sdate
                schedule_item-stime
                gv_package_names
                gv_orgid
                gv_project
                gv_repoid
                gv_baseprefix
                gv_username
                gv_pat.
  ENDLOOP.
   WRITE:/ | Successful Scheduled { lines( itab ) } jobs |.
ENDFORM.

" TODO: Initiliaze required variables
FORM f_initiliaztion.
    " TODO: specify organization for ADO REST API
    lv_orgid = "".
    " TODO: specify Git repository ID for ADO REST API
    lv_repoid = "".

    " TODO: specify project name for ADO REST API
    lv_project = "".

    " TODO: specify the Git branch name prefix, usually users/system/,
    " for the branch holding the ABAP objects
    lv_baseprefix = "".

    " TODO: specify user name for ADO REST API
    lv_username = "".

    " TODO: specify personal access token of the user name above for ADO REST API
    lv_pat = "".
ENDFORM.

FORM schedule_catchup_abaptogit USING iv_schedule_date  TYPE btcsdldate
                                      iv_schedule_time  TYPE t
                                      iv_package_names  TYPE string
                                      iv_orgid          TYPE string
                                      iv_project        TYPE string
                                      iv_repoid         TYPE string
                                      iv_baseprefix     TYPE string
                                      iv_username       TYPE string
                                      iv_pat            TYPE string.
  DATA: rt_tstmp         TYPE timestamp,
        lv_system_date   TYPE btcsdate,
        lv_system_time   TYPE btcstime,
        lv_utc_timestamp TYPE timestamp,
        lv_jobname       TYPE btcjob,
        lv_jobcount      TYPE btcjobcnt.

  lv_jobname = co_title.

  CALL FUNCTION 'JOB_OPEN'
    EXPORTING
      jobname          = lv_jobname
      sdlstrtdt        = sy-datum
      sdlstrttm        = sy-uzeit
    IMPORTING
      jobcount         = lv_jobcount
    EXCEPTIONS
      cant_create_job  = 1
      invalid_job_data = 2
      jobname_missing  = 3
      OTHERS           = 4.

  IF sy-subrc <> 0.
    WRITE:/ 'Error Opening job : ', lv_jobname.
    EXIT.
  ENDIF.

  SUBMIT z_abaptogit_catchupsync VIA JOB lv_jobname
                                              NUMBER lv_jobcount
                                              WITH p_pname = iv_package_names
                                              WITH p_orgid = iv_orgid
                                              WITH p_proj = iv_project
                                              WITH p_repoid = iv_repoid
                                              WITH p_prefix = iv_baseprefix
                                              WITH p_usrnam = iv_username
                                              WITH p_pat = iv_pat
                                              AND RETURN.
  IF sy-subrc <> 0.
    WRITE:/ |Error on submitting job :  { lv_jobname }, number { lv_jobcount }, schedule date :  { iv_schedule_date }, schedule time { iv_schedule_time }|.
    EXIT.
  ENDIF.


  CALL FUNCTION 'JOB_CLOSE'
    EXPORTING
      jobcount             = lv_jobcount
      jobname              = lv_jobname
      sdlstrtdt            = iv_schedule_date
      sdlstrttm            = iv_schedule_time
    EXCEPTIONS
      cant_start_immediate = 1
      invalid_startdate    = 2
      jobname_missing      = 3
      job_close_failed     = 4
      job_nosteps          = 5
      job_notex            = 6
      lock_failed          = 7
      OTHERS               = 8.

  IF sy-subrc <> 0.
    WRITE:/ |Error on Closing job :  { lv_jobname }, number { lv_jobcount }, schedule date :  { iv_schedule_date }, schedule time { iv_schedule_time }|.
    EXIT.
  ENDIF.
ENDFORM.