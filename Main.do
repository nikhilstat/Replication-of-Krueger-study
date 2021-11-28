
clear all        
capture log close
set more off 

cd "C:\Users\czd631.UTC\OneDrive - University of Tennessee at Chattanooga\Documents\Vahid@UTK\ECON582\Replication\ECON582_Replication"     
set logtype text                             

log using replication.txt, replace 

gen now ="DateTime: $S_DATE $S_TIME"
disp now


do Replication_Part1

do Replication_Part2


log close