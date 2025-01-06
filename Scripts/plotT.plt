reset session
set term wxt size 971, 600

set xlabel "Time"        font "Times New Roman, 15"
set ylabel "Temperature (℃)" font "Times New Roman, 15"

set tics font "Times New Roman, 15"
set key  font "Times New Roman, 15"
set grid

# Directory variable
dir = "./test/prj16/6pipe/L008"

# Plot commands using the directory variable
plot dir."/Output/obsf_T.dat" using 1:2 with lines title "Obs1", \
     dir."/Output/obsf_T.dat" using 1:3 with lines title "Obs2", \
     dir."/Output/obsf_T.dat" using 1:4 with lines title "Obs3", \
     dir."/Output/obsf_T.dat" using 1:5 with lines title "Obs4", \
     dir."/Output/obsf_T.dat" using 1:6 with lines title "Obs5"

pause -1