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


