SwinGame SDK is divided into multiple sections:

CoreSDK contains the main code for the SwinGame API, and the C# and VB6 interface code.
Demos contains all the demo code
SDKs contains the "starter packs" for each language we support
Showcase is an old set of tests... its now in Tests/Full
Tests this contains testing code and the Test program that illustrates most functions in use

----
Building the libraries:

All the libraries can be built from the command line.

Core Pascal Library:  in the CoreSDK folder...
 * On Mac or Unx: run ./build_lib.sh
 * On PC: run winbuild_lib.cmd

.NET interface: in the CoreSDK folder
 * On Mac or Unx: run ./mono_lib.sh
 * On PC: run DotNet_lib.cmd

VB6 interface: in the CoreSDK folder
 * On PC: run ./mono_lib.sh then run VB6_lib.cmd

Delphi version of library:
 * On PC: Open the Delphi project, compile to create libraries this builds into SDK

----
Creating an SDK:

The SDKs share many resources, so these are stored in the Base subfolder. Step 2 of the instructions below executes a script to copy the common files from the Base into the required SDK.

1: Build the libraries as needed.
2: Run copy scripts: These are in the SDK folder, or one of its parents
   * For Pascal: Run either copycommon.sh or copy_FPC_common.cmd
   * For .NET: Run either unx_cp_common.sh or win_cp_common.cmd
   * For VB6: Copy common resources manually (or write a script and update this...)
   * For Delphi: Run copy_delphi_common.cmd
3: Move into the SDK folder
4: Check SDK can compile and run
5: Copy to temporary location
6: Strip out .svn details
7: Package and ship


----
Build a demo or Test

The demos share many files from the SDKs. The copy_lib_mono.sh and unxdemo.sh scripts copy the shared files from the SDK into the demo. These are run on Mac or Unix like: ./unxdemo.sh AlienFlight this will copy the library files into the AlienFlight demo.

Scripts need to be provided for the Windows versions.

(Tests have win_lib.cmd in the C# version only.)
