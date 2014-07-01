@echo off
set ERL=C:\Erlang\bin\erl.exe
if not exist ebin md ebin
%ERL% -make
pause
