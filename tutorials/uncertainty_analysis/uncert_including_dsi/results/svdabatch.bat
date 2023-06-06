@echo off
REM This part of batch file added by SVDAPREP
REM
REM Delete model input files.
del hk.pts > nul
REM
REM Run PARCALC to compute base parameters from super parameters.
parcalc > nul
REM
REM The following is copied directly from file model.bat
REM
@echo off
Rem ###################################
Rem Some intermediate files are deleted.
Rem ###################################

del hk.ref > nul
del rect.hds > nul
del rect.bud > nul

Rem ###################################
Rem Now the actual model is run
Rem ###################################

plproc plproc.dat > nul
mf6 > nul
mpath7 rect.mpsim > nul
mod2obs_dbl < mod2obs.in > nul


