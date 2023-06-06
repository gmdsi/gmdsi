mkdir worker1
mkdir worker2
cd worker1
copy ..\*.*
start agent_hp case2_svda /h %COMPUTERNAME%:4004
cd ..
cd worker2
copy ..\*.*
start agent_hp case2_svda /h %COMPUTERNAME%:4004
cd ..
start agent_hp case2_svda /h %COMPUTERNAME%:4004
pest_hp case2_svda /h :4004