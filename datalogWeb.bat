@echo off
rem Set the path to the Rscript executable
set RSCRIPT="C:\Program Files\R\R-4.5.1\bin\Rscript.exe"

rem Set the path to the R script to execute
set RSCRIPT_FILE="C:\GitHub\Datalogger2WebR\processall_stations.R"

cd "C:\GitHub\Datalogger2WebR"
rem Execute the R script
%RSCRIPT% --vanilla %RSCRIPT_FILE%

rem Pause so the user can see the output
rem pause
exit