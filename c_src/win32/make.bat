@echo off

rem
rem Run this script to compile the windows backend of gen_serial.
rem This will produce the executable serial_esock.exe
rem
rem Set the following environment variables to your correct paths!
rem As shown below, they are appropriate for a MS Windows SDK 7.1 default installation.
rem

set LIB=c:\Program Files (x86)\Microsoft Visual Studio 10.0\VC\lib;c:\Program Files\Microsoft SDKs\Windows\v7.1\Lib
set INCLUDE=c:\Program Files (x86)\Microsoft Visual Studio 10.0\VC\include;c:\Program Files\Microsoft SDKs\Windows\v7.1\Include
set PATH=c:\Program Files (x86)\Microsoft Visual Studio 10.0\VC\bin;c:\Program Files (x86)\Microsoft Visual Studio 10.0\Common7\IDE;c:\Program Files\Microsoft SDKs\Windows\v7.1\bin


cl /O2 /Feserial_esock.exe /DWIN32 /I.. win32_main.c win32pipe_threads.c win32serial_overlapped.c ..\erlang_serial.c /link ws2_32.lib

pause
