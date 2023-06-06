mkdir worker1
mkdir worker2
cd worker1
copy ..\*.*
start pestpp-ies case_dsi /h %COMPUTERNAME%:4004
cd ..
cd worker2
copy ..\*.*
start pestpp-ies case_dsi /h %COMPUTERNAME%:4004
cd ..
start pestpp-ies case_dsi /h %COMPUTERNAME%:4004
pestpp-ies case_dsi /h :4004