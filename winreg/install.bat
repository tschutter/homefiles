@echo off

rem Ensure ADMIN Privileges.
rem Adaptation of https://sites.google.com/site/eneerge/home/BatchGotAdmin
rem and http://stackoverflow.com/q/4054937
rem Check for ADMIN Privileges
>nul 2>&1 "%SYSTEMROOT%\system32\cacls.exe" "%SYSTEMROOT%\system32\config\system"
if '%errorlevel%' NEQ '0' (
    rem Get ADMIN Privileges
    echo Set UAC = CreateObject^("Shell.Application"^) > "%temp%\getadmin.vbs"
    echo UAC.ShellExecute "%~s0", "", "", "runas", 1 >> "%temp%\getadmin.vbs"
    "%temp%\getadmin.vbs"
    del "%temp%\getadmin.vbs"
    exit /B
) else (
    rem Got ADMIN Privileges
    pushd "%cd%"
    cd /d "%~dp0"
)

rem If regedit is run without elevated privileges, it will fail and
rem report a misleading invalid file error (if /s is not specified).

rem regedit /s emacs.reg
rem regedit /s no-hide-known-file-extensions.reg
rem regedit /s no-language-bar.reg
rem regedit /s no-recycle-bin.reg
regedit /s no-screen-saver.reg
rem regedit /s no-shortcut-text.reg
rem regedit /s no-shutdown-event-tracker.reg
rem regedit /s verbose-boot.reg
