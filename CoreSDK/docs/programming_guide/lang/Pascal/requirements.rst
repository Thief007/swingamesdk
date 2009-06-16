The Pascal version of SwinGame requires that you install and configure the `Free Pascal Compiler <http://www.freepascal.org>`_. The Free Pascal Compiler is professional Pascal compiler that contains all of the tools you need to compile your SwinGames. As the name suggests, the Free Pascal Compiler is free and available for download for a number of platforms. The following sections describe how to configure your development machine to make use of the Pascal version of SwinGame.

.. contents::
    :local:

Getting start with SwinGame on Windows
**************************************
The following steps guide you through installing the Free Pascal Compiler for Windows.

* Download the 32bit version of the Free Pascal Compiler for your version of Windows, making sure that you remember the version number of the compiler. 
* Install the compiler in its default location. If you must install it in another location **do not** install Free Pascal in a directory that contains spaces in its path. i.e. don't install in *Program Files*. See Common Problems.
* Edit your **PATH** to include the ``c:\FPC\*version*\bin\i386-win32`` path [#]_ . For example, if you installed FPC version 2.2.4 the path you need to include would be ``c:\FPC\2.2.4\bin\i386-win32``

To test that your computer is correctly configured try the following:

* Open a **Command Prompt**, this can be done by running **cmd.exe**. [#]_
* Run the Free Pascal Compiler from the command line by typing the following:

  .. sourcecode:: bash
    
      fpc

* Check the output generated, you should see listed the compilers name and version. You can press q to quit the output.

.. [#] Changing the environment variables will not alter existing console windows. Make sure that you open a new Console after changing the Path variable.

.. [#] Details on setting your path can be found in the `Setting Environment Variables <http://mercury.it.swin.edu.au/swinbrain/index.php/Setting_Environment_Variables_How_To>`_ article.

* Download the Pascal SwinGame project template of your choice, making sure you download the version that matches the compiler you have installed. This will be a zip file that contains a basic Pascal project and the scripts you need to compile your game.
* Extract the zip file to a location on you machine.
* In the command prompt, navigate to the location where you extracted the zip file. This is done using ``cd c:\path\etc\``.
* Navigate into the *SwinGame* directory, you can do this using *cd SwinGame*
* Run ``winbuild.cmd Test.exe``, this will execute the *winbuild* script that is used to compile you game and to copy the resources to the appropriate locations. The *Test.exe* part indicates the name of the executable that will be created.
* Finally, to run the game enter ``bin\Test.exe``. The *winbuild* script places the compiled output in a *bin* directory [#]_. This command tells Windows to run the *Test.exe* program from the bin directory.
* You should see a splash screen, and a *Hello World* program start... you now have you machine correctly configured for creating SwinGames using Pascal.

.. [#] The bin directory stands for binary. It is the standard name given by developers to the folder that contains their programs.

Common Problems
---------------

* The following error messages indicate that Windows cannot find the compiler, or one of its related tools. Check your path variable and ensure that it includes the path to the Free Pascal Compiler. You can check the path from the command line using *echo %PATH%*::

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

Check List
**********
To get started with the Pascal version of SwinGame.

* Install same version of the `Free Pascal Compiler <http://www.freepascal.org>`_ as the SwinGame library.
* Make sure there are no spaces in the path to the Free Pascal Compiler, i.e. don't install in *Program Files*.
* Include the Free Pascal compiler on the PATH environment variable.
* If you are using **Mac OS**, make sure you have installed the developer tools.
* If you are using **Linux**, make sure you have installed all of the required libraries.
 
