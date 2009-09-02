SwinGame SDK is divided into multiple sections:

CoreSDK contains the main code for the SwinGame API (Pascal).
Demos contains all the demo code (mostly for SwinGame 2)
Dist - contains the files for distribution that are created by the scripts in Tools
Templates - contains the static parts of the distributions. These files are copied by the scripts in Tools into the Dist directory when built
Tests - contains testing code and the Test program that illustrates most functions in use (needs to be updated)
Tools - contains the scripts that create the different distributions.

(old directories that need to be removed...)
SDKs contains old templates that haven't been converted to the new form

----
Building the libraries:

All the libraries can be built from the command line (using bash - install MSYS on windows).

The building scripts are in Tools/Scripts.

The scripts named "bundle_${lang}_templates.sh" can be called to create a given distribution in the Dist directory.
The create_library.sh is used to compile the SwinGame API as a library (dll, so, or dylib), use -i to install the library.
The scripts named "produce_${platform}_templates.sh" can be used to create all of the files for distribution on that platform and to bundle them for distribution.

----
Testing new features:

This can be done with the ./build script in CoreSDK (on Mac only at the moment).

----
Structure of a template:

./
./bin (or ./build) for output of compiling game
./lib includes SwinGame source and binaries
./obj (or ./tmp) for temporary output from compiling game
./Resources includes SwinGame resources (images, sounds, etc)
./src Source code for game

Of these Resources needs to retain the same structure to allow SwinGame to load data correctly.

----
Adding new templates to a language:

1: Create a project using the environment desired.
2: Check that it works :)
3: Remove shared folders including those in:
3.1: Templates/Common/Resources
3.2: Templates/${lang}/Common
4: Place template directory in /Templates/${lang}/${template kind}
5: Alter bundle_${lang}_templates.sh to include template for appropriate platforms

