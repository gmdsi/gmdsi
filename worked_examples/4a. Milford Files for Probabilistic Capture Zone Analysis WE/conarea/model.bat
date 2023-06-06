REM -- First delete files that are written by this script, but that are not read by PEST.


REM Written by PLPROC
del kx.dat
del kz.dat
del milford1.wel
del milford1.riv

REM Written by MODFLOW-USG
del milford1.hds
del milford1.cbb
del milford1.cbcln

REM -- Run components of the model
.\exec\plproc plproc.dat
.\exec\USGs_1 milford1.nam
.\exec\usgmod2obs < usgmod2obs.in
.\exec\usgbud2smp < usgbud2smp_riv.in
.\exec\usgbud2smp < usgbud2smp_pump.in
.\exec\usgbud2smp < usgbud2smp_drn.in
.\exec\obs2obs .\observ_files\obs2obs.in obs2obs.out

call run_mp3du.bat