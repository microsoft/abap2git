# abap2git

**abap2git** is a simplified sync tool written in ABAP code to one-way sync ABAP objects from an SAP system to Git repository with Azure DevOps REST API with following benefits:
* Support multiple packages in a Git branch
* Support multiple SAP systems in the same Git repo, each in their respective branches
* Sync ABAP objects in a released workbench/customizing transport request (shortened as TR below) to specific Git branch
* Support configuration changes in customizing transport request for 1380 ones specified in OBJH table
* Support HR/Payroll schemas/personnel calculation rules (PCR) in customizing transport request
* The sync for the transport request only accesses the ABAP objects contained and not other unchanged ABAP objects

## Features
1. Download ABAP objects in specific package(s) of an SAP system to local folder, each package a folder.
2. Download ABAP objects in all Y*/Z* customization packages to local folder.
3. Support active version mode and latest version mode. In active version mode, for each ABAP object, active version, if any, otherwise latest version, will be downloaded; in latest version mode, for each ABAP object, only latest version will be downloaded or a released version no later than an optional released TR (to sync to that specific snapshot of the system), if not available the object will not be downloaded. Active version mode is best to download all current version of the ABAP objects for code inspection and analysis; latest version mode is best for continuous integration purpose.
4. Sync a specific released TR to specific Git repo branch, provided the ABAP objects in that branch are downloaded in latest version mode above.
5. Catchup: sync the Git repo branch to latest TR by generating commit for each TR between last sync-ed one and latest one, this helps you bring the git repo in sync.

## How To
* Add the classes and reports in abap folder to the target SAP system.
* Create a branch for target SAP system in the Git repository, suggested branch name is "users/system/system id", for example, users/system/sy1.
* Run the report Z_ABAPTOGIT_LOCALSYNC to download initial ABAP objects with active or latest version mode.
* Use git command/CLI/... to push the initial objects.
* Register the ABAP Test Cockpit (ATC) BAdI class ZCL_IM_BADI_ABAPTOGIT_SYNC in abap folder to sync released TR to the Git branch with downloaded objects in latest version mode.
* Run the report Z_ABAPTOGIT_CATCHUPSYNC to "catch up" the TRs as mentioned in feature #5.

## FAQ
### Why is BAdI CTS_REQUEST_CHECK~CHECK_BEFORE_RELEASE not a good place to sync code to Azure DevOps?
The BAdI class is called before releasing the TR, the release process may fail afterwards due to many reasons, containing unreleased tasks, failed ATC rules, failed virtual forge checks, failed ATC unit tests, etc., then the request will end up with unreleased state, in this case the request is not supposed to sync to Git.

### Why use background job to call abap2git?
Given the BAdI above with improper timing to sync to Git, scheduled background job with delay would be a solution to ensure the TR release process is completed when the job starts.

### What if ABAP developer doesn't have the privilege to schedule background job in SAP?
Contact security team to add a customized role with authorize objects required to schedule job and have ABAP developer apply for that role.

### What if security cannot approve customized role for scheduling background job?
Check out next question for options and pros/cons.

### What are the options and pros/cons for background job solution?
1. In BAdI class schedule background job to call abap2git spot sync method. The pros is the short turnaround time between TR release and Git sync, which ensures rapid continuous integration right after TR. The cons are privilege needed to schedule background job, and the race condition upon multiple TRs released in short time causing earlier TR got sync after later TR due to uncertain start time of their corresponding background jobs. A sequential queuing mechanism is required to eliminate this race condition and it's not provided by abap2git.
2. In BAdI class schedule background job but manage a time window (say 1 hour) that only one job is scheduled within the window. Also requires background job privilege. This reduces but doesn't eliminate the race condition given the events (TR is acutally released; abap2git checks if that TR is in released status) may race when the events happen at the time window boundary. Worst case a TR is not sync-ed when there's no further TR followed.
3. Use SM36 to schedule recurring background job in specific frequency which fetches configurations/secrets (ADO PAT) and calls Z_ABAPTOGIT_CATCHUPSYNC program catching up released TRs since last sync. This is best but relies on your security/BASIS policy.
4. Use Z_ABAPTOGIT_SCHEDULE_JOB program to schedule 28 days \* 24 jobs (1 per hour) at a time, requiring a developer with background job privilege to run it every 4 weeks. This doesn't require all ABAP developers with the privilege.

### Why configuration changes look the same across multiple Git commits?
Currently configuration changes in a specific data table are captured with current data rows in the table when the TR is sync-ed, thus if multiple TRs are sync-ed in catching up or changes are made before catching up, the configuration changes captured for the TRs will be the same and don't reflect the actual changes released by the TR. Future release of abap2git will attempt to fetch the actual changes from audit log or TR export file.

## Contributing

This project welcomes contributions and suggestions.  Most contributions require you to agree to a
Contributor License Agreement (CLA) declaring that you have the right to, and actually do, grant us
the rights to use your contribution. For details, visit https://cla.opensource.microsoft.com.

When you submit a pull request, a CLA bot will automatically determine whether you need to provide
a CLA and decorate the PR appropriately (e.g., status check, comment). Simply follow the instructions
provided by the bot. You will only need to do this once across all repos using our CLA.

This project has adopted the [Microsoft Open Source Code of Conduct](https://opensource.microsoft.com/codeofconduct/).
For more information see the [Code of Conduct FAQ](https://opensource.microsoft.com/codeofconduct/faq/) or
contact [opencode@microsoft.com](mailto:opencode@microsoft.com) with any additional questions or comments.

## Trademarks

This project may contain trademarks or logos for projects, products, or services. Authorized use of Microsoft 
trademarks or logos is subject to and must follow 
[Microsoft's Trademark & Brand Guidelines](https://www.microsoft.com/en-us/legal/intellectualproperty/trademarks/usage/general).
Use of Microsoft trademarks or logos in modified versions of this project must not cause confusion or imply Microsoft sponsorship.
Any use of third-party trademarks or logos are subject to those third-party's policies.
