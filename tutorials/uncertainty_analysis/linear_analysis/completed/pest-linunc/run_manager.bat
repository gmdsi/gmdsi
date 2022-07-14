echo pest_hp_mkl linunc-red.pst /h :4004> manager.bat
call D:\Workspace\hugm0001\anaconda\Scripts\activate base
echo agent_hp linunc-red.pst /h %computername%:4004 > agent.bat
start agent.bat
start manager.bat