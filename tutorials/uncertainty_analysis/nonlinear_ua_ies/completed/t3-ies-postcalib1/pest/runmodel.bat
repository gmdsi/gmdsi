@echo off
Rem ###################################
Rem Some intermediate files are deleted as a precaution.
Rem ###################################
cd %~dp0..\runmodel\model\obs
del /s *.csv

Rem ###################################
Rem PLPROC assigns parameters from ppoints
Rem ###################################
cd %~dp0..\runmodel\preproc\
plproc plproc.dat
plproc plproc_stream.dat
plproc plproc_ghb.dat
plproc plproc_rch.dat

Rem ###################################
Rem The MF6 model is run
Rem ###################################
cd %~dp0..\runmodel\model\
mf6 mfsim.nam

Rem ###################################
Rem The observations are postprocessed
Rem ###################################
cd  %~dp0..\runmodel\postproc\
olproc olproc.in 1
olproc olproc-pred.in 1

Rem ###################################
Rem Return the command line to the working folder
Rem ###################################
cd %~dp0.\