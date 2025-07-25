reset session
set term wxt size 971, 600

set xlabel "Time"        font "Times New Roman, 15"
set ylabel "Temperature (℃)" font "Times New Roman, 15"

set tics font "Times New Roman, 15"
set key  font "Times New Roman, 15"
set grid

# パスを読み込む
dir = system("tr -d '\\n' < /workspaces/FTDSS/ProjectPath.dir")
datafile = dir."/Output/obsf_T.dat"

# 実データ行の列数を取得
col_count = int(system("awk '!/^#/ && NR > 6 {print; exit}' ".datafile." | tr -s '\t ' ' ' | wc -w"))
print "Detected column count: ", col_count

# プロットコマンド生成
plotcmd = ""
do for [i=2:col_count] {
    if (i > 2) {
        plotcmd = plotcmd . ", "
    }
    plotcmd = plotcmd . sprintf("'%s' using 1:%d with lines title 'Obs%d'", datafile, i, i-1)
}
eval("plot ".plotcmd)

pause mouse close