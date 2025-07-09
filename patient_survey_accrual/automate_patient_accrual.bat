:: Set the environmental path variable to R 4.4.1
set PATH=C:\Program Files\R\R-4.4.1\bin\x64;%PATH%

:: directory to the root project folder in windows backslash format
set proj_root="D:\long_covid"

:: path to the Rscript.exe version used to develop the R script in windows backslash format
set r_version="C:\Program Files\R\R-4.4.1\bin\Rscript.exe"

:: two R commands separated by a semi colon encased in double quotes
:: load renv, then source script. All paths are forward slash and encased in single quotes
:: because these are R commands and not command prompt commands
set r_commands="renv::load('D:/long_covid'); source('D:/long_covid/patient_survey_accrual/render_patient_accrual_report.R')"

:: path to the log output file in windows back slash format
set log_out="D:\long_covid\patient_survey_accrual\patient_accrual.log"

:: Change directory to the project root
cd %proj_root%

:: Call the script
call %r_version% -e %r_commands% > %log_out% 2>&1

:: Remove the R 4.2.2 path so that other functions aren't affected
set PATH=%PATH:C:\Program Files\R\R-4.4.1\bin\x64;=%

:: Exit the shell
exit