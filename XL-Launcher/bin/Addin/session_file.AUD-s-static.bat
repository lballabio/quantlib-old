
@REM Where the Launcher.xla is to be found
SET LAUNCHER_DIR=.

@REM This environment variable tells the launcher which session file to use.
@REM If this variable is not set then Launcher.xla looks for a file called session_file.xml in the same directory.
@REM If that file is not found then the launcher defaults to hard coded parameters.
SET XL_LAUNCHER_PATH=%LAUNCHER_DIR%\session_file.AUD-s-static.xml

START "EXCEL" "EXCEL.EXE" /e /r "%LAUNCHER_DIR%\Launcher.xla"
