lumprem  lr_lu1.in lr_lu1.out 
lumprem  lr_lu2.in lr_lu2.out 
lumprem  lr_lu3.in lr_lu3.out 
lumprem  lr_ghb.in lr_ghb.out 

echo  lr2series.in | lr2series

echo  ts6evp.in | ts6proc
echo  ts6ghb.in | ts6proc
echo  ts6rch.in | ts6proc
echo  ts6wel.in | ts6proc
pause  