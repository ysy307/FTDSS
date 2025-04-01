reset session
set term wxt size 1294, 800  # Increased size to accommodate multiple plots

# Directory variable
dir = "./test/prj2"
obsa = "(4.0,2.4)"
obsb = "(5.0,2.4)"
obsc = "(6.0,2.4)"
obsd = "(7.0,2.4)"
obse = "(5.0,1.6)"

set tics  font "Times New Roman, 10"
set key   font "Times New Roman, 12"
set grid

# Start multiplot layout
set multiplot layout 2, 2

# First plot: Temperature
set xlabel "Time" font "Times New Roman, 15"
set ylabel "Temperature (℃)" font "Times New Roman, 15"
plot dir."/Output/obsf_T.dat" using 1:2 with lines linewidth 2 title obsa, \
     dir."/Output/obsf_T.dat" using 1:3 with lines linewidth 2 title obsb, \
     dir."/Output/obsf_T.dat" using 1:4 with lines linewidth 2 title obsc, \
     dir."/Output/obsf_T.dat" using 1:5 with lines linewidth 2 title obsd, \
     dir."/Output/obsf_T.dat" using 1:6 with lines linewidth 2 title obse

# Second plot: Pressure
set xlabel "Time" font "Times New Roman, 15"
set ylabel "Pressure (m)" font "Times New Roman, 15"
plot dir."/Output/obsf_P.dat" using 1:2 with lines linewidth 2 title obsa, \
     dir."/Output/obsf_P.dat" using 1:3 with lines linewidth 2 title obsb, \
     dir."/Output/obsf_P.dat" using 1:4 with lines linewidth 2 title obsc, \
     dir."/Output/obsf_P.dat" using 1:5 with lines linewidth 2 title obsd, \
     dir."/Output/obsf_P.dat" using 1:6 with lines linewidth 2 title obse

# Third plot: Si
set xlabel "Time" font "Times New Roman, 15"
set ylabel "Si (-)" font "Times New Roman, 15"
set yrange [0:1]
plot dir."/Output/obsf_Fr.dat" using 1:2 with lines linewidth 2 title obsa, \
     dir."/Output/obsf_Fr.dat" using 1:3 with lines linewidth 2 title obsb, \
     dir."/Output/obsf_Fr.dat" using 1:4 with lines linewidth 2 title obsc, \
     dir."/Output/obsf_Fr.dat" using 1:5 with lines linewidth 2 title obsd, \
     dir."/Output/obsf_Fr.dat" using 1:6 with lines linewidth 2 title obse
unset yrange

# Fourth plot: Flux
set xlabel "Time" font "Times New Roman, 15"
# set ylabel "Klh (m/d)" font "Times New Roman, 15"
set ylabel "Flux (m/d)" font "Times New Roman, 15"
set logscale y
set format y "10^{%L}"
# plot dir."/Output/obsf_K.dat" using 1:2 with lines linewidth 2 title obsa, \
#      dir."/Output/obsf_K.dat" using 1:3 with lines linewidth 2 title obsb, \
#      dir."/Output/obsf_K.dat" using 1:4 with lines linewidth 2 title obsc, \
#      dir."/Output/obsf_K.dat" using 1:5 with lines linewidth 2 title obsd, \
#      dir."/Output/obsf_K.dat" using 1:6 with lines linewidth 2 title obse
plot dir."/Output/obsf_Flux.dat" using 1:($2 *86400) with lines linewidth 2 title obsa, \
     dir."/Output/obsf_Flux.dat" using 1:($4 *86400) with lines linewidth 2 title obsb, \
     dir."/Output/obsf_Flux.dat" using 1:($6 *86400) with lines linewidth 2 title obsc, \
     dir."/Output/obsf_Flux.dat" using 1:($8 *86400) with lines linewidth 2 title obsd, \
     dir."/Output/obsf_Flux.dat" using 1:($10*86400) with lines linewidth 2 title obse

unset format y
unset logscale y
unset multiplot
pause -1 "Press any key to continue"
