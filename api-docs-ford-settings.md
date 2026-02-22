---
project: FTDSS
summary: This solver can calculate freezing/thawing processes in the soil integrated heat and water transport.
author: Kikuchi Shun
email: shungiku1012@gmail.com
project_github: https://github.com/ysy307/FTDSS.git
src_dir: ./src
output_dir: ./docs/api-docs
page_dir: ./docs
fixed_length_limit: False
preprocess: true
extensions: F90
fpp_extensions: F90
graph: true
coloured_edges: true
print_creation_date: true
creation_date: %Y-%m-%d %H:%M %z
docmark: !
predocmark: >
display: public
         protected
         private
source: true
sort: alpha
extra_mods:
    iso_fortran_env:https://gcc.gnu.org/onlinedocs/gfortran/ISO_005fFORTRAN_005fENV.html
    iso_c_binding:https://gcc.gnu.org/onlinedocs/gfortran/ISO_005fC_005fBINDING.html#ISO_005fC_005fBINDING
    ieee_arithmetic:https://gcc.gnu.org/onlinedocs/gfortran/IEEE-modules.html
    json_module:http://jacobwilliams.github.io/json-fortran/
graph_maxnodes: 500
graph_maxdepth: 50
md_extensions: markdown.extensions.toc
---

@warning
Work in progress
@endwarning

## 概要
土壌凍結融解過程の水分・熱移動の数値計算

@note
このプロジェクトでは，Fortranで書かれている．
@endnote

@todo
- リファクタリング
- コマンドライン引数への対応
- 他
@endtodo

@bug
現在の所バグは見つかっていない．
@endbug
