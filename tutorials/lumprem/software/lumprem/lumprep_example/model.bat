REM LUMPREM output files are deleted.
 
del lr_lm1.out
del lr_lm2.out
 
REM LUMPREM models are run.
 
lumprem lr_lm1.in lr_lm1.out
lumprem lr_lm2.in lr_lm2.out
