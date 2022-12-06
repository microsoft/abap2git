# ABAP2GIT FAQ

## Table of Contents

1. what is SPOTSYNC_TR?
2. what is CATCHUP_TRS?
3. why using BADI CTS_REQUEST_CHECK~CHECK_BEFORE_RELEASE is not a good idea to sync code to Azure DevOps?

## FAQ

### What is SPOTSYNC_TR?

- It is syncing SAP code to Azure DevOps by transport request

### What is CATCHUP_TRS?

- It is syncing all transport request objects after transport request in .sync_status.json in /SRC folder

### Why using BADI CTS_REQUEST_CHECK~CHECK_BEFORE_RELEASE is not a good idea to sync code to Azure DevOps?

- Release workbench request contains unreleased tasks and check_before_release is possible to running with unsuccessful release

- It is running before or parallel as Virtual forge or ATC rules, those check could fail, and we cannot sync the release

### What if we don’t have schedule background job role in SAP

- Contact security team to add a customized role with authorized objects we need to schedule job

### What if we cannot get approval to add a customized role for schedule background job?

- We need to use semi auto solution to schedule 28 days \* 24 hour job/day by person who can schedule the job with ABAP2GIT and catchup sync
- In Catch UP sync we check if transport request is released or unreleased
- Schedule a periodic job in SM36 with abaptogit_catchupsync program

### What is rolling based release solution?

- When multiple people release multiple TRs in a time window(let’s assume 1 hour), we schedule only one job in that time window

### Why not use spotsync in scheduled job?

- Spot sync in ABAPTOGIT requires to running TR in sequence
  - For example, first releasing TR called TR1 and second releasing called TR2, assume TR1 releases before TR2
  - In Spot sync commit must be in sequence. If TR2 is releasing before TR1 then the whole request sync timeline is messed up

### Why not use catchup sync when developer releases a TR?

- Assume we schedule one sync job every hour when developer releases at least one TR at that hour. However, We have one job is scheduled to run to sync TR1 for developer1 at 2pm, and now developer2 is releasing a TR2 at 1:59:59 and this TR doesn’t catch the sync in 2pm and there are no TR releases in 3pm, 4pm or the end of day. The TR2 will never have the chance to be sync to ADO until some developers release a TR. It could be hours or days
