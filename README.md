# abap2git

**abap2git** is a simplified sync tool written in ABAP code to one-way sync ABAP objects from an SAP system to Git repository with Azure DevOps REST API. Compared to existing solution abapGit (https://abapgit.org/), it brings following benefits:
* Support multiple packages in a Git branch
* Support multiple SAP systems in the same Git repo, each in their respective branches
* Support HR/Payroll schemas/personnel calculation rules (PCR)
* Sync ABAP objects in a released workbench/customizing (for schema/PCR) transport request (shortened as TR below) to specific Git branch
* The sync for the transport request only accesses the ABAP objects contained and not other ABAP objects, thus more efficient

## Features
1. Download ABAP objects in specific package(s) of an SAP system to local folder, each package a folder.
2. Download ABAP objects in all Y*/Z* customization packages to local folder.
3. Support active version mode and latest version mode. In active version mode, for each ABAP object, active version, if any, otherwise latest version, will be downloaded; in latest version mode, for each ABAP object, only latest version will be downloaded or a released version no later than an optional released TR (to sync to that specific snapshot of the system), if not available the object will not be downloaded. Active version mode is best to download all current version of the ABAP objects for code inspection and analysis; latest version mode is best for continuous integration purpose.
4. Sync a specific released TR to specific Git repo branch, provided the ABAP objects in that branch are downloaded in latest version mode above.
5. Catchup: sync the Git repo branch to latest TR by generating commit for each TR between last sync-ed one and latest one, this helps you bring the git repo in sync if #4 is not run to catch some TRs.

## How To
* Add the classes and reports in abap folder to the target SAP system.
* Create a branch for target SAP system in the Git repository, suggested branch name is "users/system/system id", for example, users/system/sy1.
* Run the report Z_ABAPTOGIT_LOCALSYNC to download initial ABAP objects with active or latest version mode.
* Use git command/CLI/... to push the initial objects.
* Register the ABAP Test Cockpit (ATC) BAdI class ZCL_IM_BADI_ABAPTOGIT_SYNC in abap folder to sync released TR to the Git branch with downloaded objects in latest version mode.
* Run the report Z_ABAPTOGIT_CATCHUPSYNC to "catch up" the TRs as mentioned in feature #5.

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
