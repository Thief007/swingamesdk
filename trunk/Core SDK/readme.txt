This project is a Swin Game Application. This can be compiled on Windows, Mac, and Linux using the Free Pascal Compiler, or Delphi.  The project contains all of the files and setup required to compile under any of these environments.

NOTE: Avoid editing the GameLauncher.dpr or GameLauncher.pas files, all game code should be in GameLogic.pas and other units.

----------
SwinGame on Windows
----------
This project can be compiled from within Turbo Delphi or from the command line use Free Pascal.

To compile with FPC:
- Open a command prompts (Start - Run, enter "cmd")
- Navigate to the Project directory
- Execute the build script using "winbuild.cmd productname" where productname is the name of the application you want to create.
- The compiled program can be executed from the .\bin directory

To compile with Delphi:
- Open GameLauncher.dpr
- Compile using the Delphi IDE
- The compiled program can be found in the .\bin directory

----------
SwinGame on the Mac
----------
The project can be compiled from within XCode on the Mac to create application bundles, or from the command line. 

To compile with FPC:
- Open a Terminal window
- Navigate to the Project directory
- Execute the build script using "./macbuild.sh [-i icon] [-e exename] programname" where icon is optional and the name of the icon file to use (exclude icns extension), exename is the optional name of the executable within the bundle, and programname is the name of the bundle.
- Program can be executed from './bin/programname.app/Contents/MacOS' or via the executable bundle

To compile with XCode:
- Open the XCode project file
- Add all your source files to the 'Put all sources also in this target' target by selecting the target from the list, and then ticking the target column.
- Select the game's main target
- Build the application

----------
SwinGame on Unix
----------
The project can be compiled on Unix using Lazarus, or using the FPC command line compiler. 

To compile with FPC:
- Open a Terminal window
- Navigate to the Project directory
- Compile using "./unxbuild.sh programname" where program name is the name of the executable to be produced
- The program can be executed from the ./bin directory

To compile with Lazarus:
- Open lazarus
- Navigate to the project directory
- Open GameLauncher.lpi
- Compile using GameLauncher
- Program can be executed within Lazarus or from the ./bin folder

Note: With unix you will need to install libsdl, libsdl_mixer, libsdl_image, libsdl_ttf and the associated development packages.