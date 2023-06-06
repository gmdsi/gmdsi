del milford1_PATHLINE.bin
del milford1.ptl
del .\particle\captured.txt

.\exec\mp3du .\particles\milford1.json
.\exec\writep3doutput .\particles\milford1-output.json
.\exec\mp3dufbe < .\particles\mp3dufbe.in
.\exec\mp3duaccum < .\particles\mp3duaccum.in

del milford1_PATHLINE.bin
del milford1.ptl

