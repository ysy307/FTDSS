reset session
set term wxt size 971, 600
set tics  font "Times New Roman, 10"
set key   font "Times New Roman, 12" maxrows 5
set grid

dir = "./test/prj2"
qx = "u"
qy = "v"
obsa = "(4.0,2.4)"
obsb = "(5.0,2.4)"
obsc = "(6.0,2.4)"
obsd = "(7.0,2.4)"
obse = "(5.0,1.6)"
set xlabel "Time" font "Times New Roman, 15"
set ylabel "Flux (m/d)" font "Times New Roman, 15"
set logscale y
set format y "10^{%L}"

# Plot commands using the directory variable
plot dir."/Output/obsf_Flux.dat" using 1:($2 *86400) with lines linewidth 2 linetype 1 title qx.obsa, \
     dir."/Output/obsf_Flux.dat" using 1:($4 *86400) with lines linewidth 2 linetype 2 title qx.obsb, \
     dir."/Output/obsf_Flux.dat" using 1:($6 *86400) with lines linewidth 2 linetype 3 title qx.obsc, \
     dir."/Output/obsf_Flux.dat" using 1:($8 *86400) with lines linewidth 2 linetype 4 title qx.obsd, \
     dir."/Output/obsf_Flux.dat" using 1:($10*86400) with lines linewidth 2 linetype 5 title qx.obse, \
     dir."/Output/obsf_Flux.dat" using 1:($3 *86400) with lines linewidth 2 linetype 1 dt (10,5) title qy.obsa, \
     dir."/Output/obsf_Flux.dat" using 1:($5 *86400) with lines linewidth 2 linetype 2 dt (10,5) title qy.obsb, \
     dir."/Output/obsf_Flux.dat" using 1:($7 *86400) with lines linewidth 2 linetype 3 dt (10,5) title qy.obsc, \
     dir."/Output/obsf_Flux.dat" using 1:($9 *86400) with lines linewidth 2 linetype 4 dt (10,5) title qy.obsd, \
     dir."/Output/obsf_Flux.dat" using 1:($11*86400) with lines linewidth 2 linetype 5 dt (10,5) title qy.obse


pause -1 "Press any key to continue"