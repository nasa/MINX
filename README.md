# MINX (MISR INteractive eXplorer)
---
MINX is an interactive visualization and analysis program written in IDL and designed to make MISR data more accessible to science users.
Its principal use is to retrieve heights and motion for aerosol plumes and clouds using stereoscopic methods.

MINX is platform independent and has been tested on Mac OS X, MS Windows, and Linux.

#### Binaries for Mac OS X, MS Windows, and Linux can be found under [MINX Releases](https://github.com/nasa/MINX/releases "MINX Releases")
##### Full Documentation can be found under the [webdoc](https://github.com/nasa/MINX/blob/master/webdoc "MINX documentation") directory

  

# How to run MINX from source
---

### In the IDL Development Environment / IDL Workbench Import the Project
* File Menu -> Import
* Existing Projects into Workspace
* Next Button
* Browse to Select Root Directory
* Navigate to the root MINX directory containing MINX.prj and click the Open button
* Ensure the MINX project is displayed in the projects listing
* Finish Button

### Build the Project
* Select the MINX project in Project Explorer
* Project Menu -> Build Project

### Run the Project
* Select the MINX project in Project Explorer
* Run Menu -> Run Project MINX

# How to build MINX binaries
---

### In the IDL Development Environment / IDL Workbench Import the Project
* File Menu -> Import
* Existing Projects into Workspace
* Next Button
* Browse to Select Root Directory
* Navigate to the root MINX directory containing MINX.prj and click the Open button
* Ensure the MINX project is displayed in the projects listing
* Finish Button

### Build the Project
* Select the MINX project in Project Explorer
* Project Menu -> Build Project

### Use MAKE_RT at the IDL Console to Create a MINX Stand-alone IDL Runtime Distribution
#### For Mac OS X
* `MAKE_RT, 'MINX4', '/Users/<username>/<MINX_package_destination>', SAVEFILE='/<MINX Source Path>/MINX/minx.sav', /VM, /MACINT64, /HIRES_MAPS`
#### For Windows
* `MAKE_RT, 'MINX4', 'C:\Users\<username>\<MINX_package_destination>', SAVEFILE='C:\<MINX Source Path>\MINX\minx.sav', /VM, /WIN32, /HIRES_MAPS`
#### For Linux
* `MAKE_RT, 'MINX4', '/home/<username>/<MINX_package_destination>', SAVEFILE='/<MINX Source Path>/MINX/minx.sav', /VM, /LIN32, /HIRES_MAPS`

*Note: Depending on your version of IDL and the current OS, you may also be able to specify the /WIN64 (for Windows 64-bit), /MACINT32 (for OS X 32-bit), or /LIN64 (for Linux 64-bit) keywords to build MINX packages*

