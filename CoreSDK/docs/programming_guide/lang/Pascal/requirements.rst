The Pascal version of SwinGame requires that you install and configure the `Free Pascal Compiler <http://www.freepascal.org>`_. The Free Pascal Compiler is professional Pascal compiler that contains all of the tools you need to compile your SwinGames. As the name suggests, the Free Pascal Compiler is free and available for download for a number of platforms. The following sections describe how to configure your development machine to make use of the Pascal version of SwinGame.

.. contents::
    :local:

Getting start with SwinGame on Windows
**************************************
The following steps guide you through the process of installing the Free Pascal Compiler for Windows, using it to compile the standard project template, and running the resulting game.

1. Download the 32bit version of the Free Pascal Compiler for your version of Windows, making sure that you remember the version number of the compiler. 
2. Install the compiler in its default location. If you must install it in another location **do not** install Free Pascal in a directory that contains spaces in its path. i.e. don't install in *Program Files*. See Common Problems.
3. Edit your **PATH** to include the ``c:\FPC\*version*\bin\i386-win32`` path [#]_ . For example, if you installed FPC version 2.2.4 the path you need to include would be ``c:\FPC\2.2.4\bin\i386-win32``

To test that your computer is correctly configured try the following:

4. Open a **Command Prompt**, this can be done by running **cmd.exe**. [#]_
5. Run the Free Pascal Compiler from the command line by typing the following:

  .. sourcecode:: bash
    
      fpc

6. Check the output generated, you should see listed the compilers name and version. You can press q to quit the output.
7. Download the Pascal SwinGame project template of your choice, making sure you download the version that matches the compiler you have installed. This will be a zip file that contains a basic Pascal project and the scripts you need to compile your game.
8. Extract the zip file to a location on you machine.
9. In the command prompt, navigate to the location where you extracted the zip file. This is done using ``cd c:\path\etc\``.
10. Navigate into the *SwinGame* directory, you can do this using ``cd SwinGame``
11. Run ``winbuild.cmd Test.exe``, this will execute the *winbuild* script that is used to compile you game and to copy the resources to the appropriate locations. The *Test.exe* part indicates the name of the executable that will be created.
12. Finally, to run the game enter ``bin\Test.exe``. The *winbuild* script places the compiled output in a *bin* directory [#]_. This command tells Windows to run the *Test.exe* program from the bin directory.

You should see a splash screen, and a *Hello World* program start... you now have your machine correctly configured for creating SwinGames using Pascal.

.. [#] Changing the environment variables will not alter existing console windows. Make sure that you open a new Console after changing the Path variable.

.. [#] Details on setting your path can be found in the `Setting Environment Variables <http://mercury.it.swin.edu.au/swinbrain/index.php/Setting_Environment_Variables_How_To>`_ article.

.. [#] The bin directory stands for binary. It is the standard name given by developers to the folder that contains their programs.

Common Problems
---------------
You may encounter the following problems when configuring your machine to create SwinGames. If you encounter other problems you can get additional assistance from the `SwinGame Forum <http://www.swingame.com/swinforum/index.php>`_.

* The following error messages indicate that Windows cannot find the compiler, or one of its related tools. Check your path variable and ensure that it includes the path to the Free Pascal Compiler. You can check the path from the command line using ``echo %PATH%``::

    'fpc' is not recognized as an internal or external command,
    'cp' is not recognized as an internal or external command,


* An error message like the one shown below indicates that Free Pascal is installed in a directory with a space in its path. To correct this, uninstall Free Pascal and reinstall it in an alternate location::

    cpp: Files\bin\i386-Win32\..\lib/gcc-lib/i386-mingw32\2.95\: No such file or directory
    windres: no resources
    Error...

* Errors similar to the following error message indicates you have a different version of the Free Pascal Compiler to the version used to create SwinGame. To correct this, download the version of SwinGame that matches your compiler [#]_::

    PPU Loading .\lib\SGSDK_Audio.ppu
    PPU Invalid Version 80
    Fatal: Can't find unit SGSDK_Audio used by MyGame
    Fatal: Compilation aborted

.. [#] You can check the compiler version by checking the directory it is installed in, or by running *fpc* from the command line and reading the compiler header message.

Getting started with SwinGame on Mac OS
***************************************
The following steps guide you through the process of installing the tools you need on Mac OS to create a SwinGame using Pascal, using these to compile the standard project template, and running the resulting game.

1. Mac OS comes with a set of development tools which SwinGame makes use of. The tools are associated with Apple's XCode development environment and you can either install it from the Mac OS installation DVD, or `download <http://developer.apple.com/technology/xcode.html>`_ and install the latest version from the Apple website [#]_.
2. Download the Free Pascal Compiler for Mac OS, making sure that you remember the version number of the compiler. 
3. Run the package installer and install the compiler.
4. Open a **Terminal** window
5. Run the Free Pascal Compiler from the command line by typing the following:

  .. sourcecode:: bash
    
      fpc

6. Check the output generated, you should see listed the compilers name and version. You can press q to quit the output.
7. Download the Pascal SwinGame project template of your choice, making sure you download the version that matches the compiler you have installed. This will be a disk image that contains a basic Pascal project and the scripts you need to compile your game.
8. Drag the files from the disk image to a location on your Mac.
9. In the Terminal, navigate to the location where you extracted the zip file. This is done using ``cd ~/Documents/MyGame/etc``.
10. Navigate into the *SwinGame* directory, you can do this using ``cd SwinGame``
11. Run ``./build.sh Test``, this will execute the *build* script that is used to compile you game and to copy the resources to the appropriate locations. The *Test* part indicates the name of the application that will be created.
12. Finally, to run the game type ``open ./bin/Test.app``. The *build* script places the compiled output in a *bin* directory [#]_. This command tells Mac OS to open the *Test* application from the bin directory. You can also run this using ``./bin/Test.app/Contents/MacOS/Test`` which is longer but will output any error messages into the terminal window. Alternatively you can view error messages by opening the *Console* application and viewing the *Console Messages*.

When the game rungs you should see a splash screen, and a *Hello World* program start... you now have your machine correctly configured for creating SwinGames using Pascal.

.. [#] Please note that the XCode download is around 1,000 megabytes, or 1,750 megabytes with the iPhone SDK. At this stage SwinGame does not run on the iPhone, though you can create Free Pascal programs that will run on the phone.
.. [#] The bin directory stands for binary. It is the standard name given by developers to the folder that contains their programs.

Common Problems
---------------
You may encounter the following problems when configuring your machine to create SwinGames. If you encounter other problems you can get additional assistance from the `SwinGame Forum <http://www.swingame.com/swinforum/index.php>`_.

* An error message like the one shown below indicate that XCode and the development tools are not present. Install the development tools from the Mac OS installation DVD or download and install from the Apple website::
    
    ./build.sh: line 67: /usr/bin/as: No such file or directory
    An error occurred while assembling ./bin/i386/GameLauncher.s

* Errors similar to the following error message indicates you have a different version of the Free Pascal Compiler to the version used to create SwinGame. To correct this, download the version of SwinGame that matches your compiler [#]_::

    PPU Loading .\lib\SGSDK_Audio.ppu
    PPU Invalid Version 80
    Fatal: Can't find unit SGSDK_Audio used by MyGame
    Fatal: Compilation aborted

.. [#] You can check the compiler version by checking the directory it is installed in, or by running *fpc* from the command line and reading the compiler header message.

Getting Started with SwinGame on Linux
**************************************
The following steps guide you through the process of installing the tools you need on Linux to create a SwinGame using Pascal, using these tools to compile the standard project template, and running the resulting game. The instructions are based on the Ubuntu linux distribution, if you have an alternate Linux distributions you may need to follow a different process from the instructions below. 

1. SwinGame uses Simple DirectMedia Layer (SDL) for its low level operations. You need to install the following SDL packages available from Synaptic Package Manager, or as source from the `SDL website <www.libsdl.org>`_. The ``<vvv>`` indicates the location of the version number, for example libsdl1.2-dev would install version 1.2 of the SDL development library. In all cases download and install the latest version available for your platform. The packages to install are::
    libsdl<vvv>-dev
    libsdl-ttf<vvv>-dev
    libsdl-mixer<vvv>-dev
    libsdl-image<vvv>-dev
    libsdl-gfx<vvv>-dev
    SDL-image-devel
    

2. Download the Free Pascal Compiler for Linux, making sure that you remember the version number of the compiler.
3. Install the Free Pascal Compiler based on the type of file you downloaded. Typically this will involve using your package manager to install the downloaded ``deb`` file.

