src_dir: ./src
output_dir: ./docs/api-docs
include:/opt/intel/oneapi/mkl/latest/include/
        /opt/intel/oneapi/mpi/latest/include/
project: FTDSS
summary: This solver can calculate freezing/thawing processes in the soil integrated heat and water transport.
author: Kikuchi Shun
email: shungiku1012@gmail.com
project_github: https://github.com/ysy307/FTDSS.git
page_dir: ./docs
source: true
graph: true
coloured_edges: true
print_creation_date: true
creation_date: %Y-%m-%d %H:%M %z
predocmark_alt: >
predocmark: <
docmark_alt:
docmark: !
display: public
         protected
         private
extra_mods: json_module: http://jacobwilliams.github.io/json-fortran/
graph_maxnodes: 250
graph_maxdepth: 10
# ソース探索対象に .F90/.f90 を両方含める
extensions:
  F90
  f90

# プリプロセッサ対象も F90 と fpp（必要に応じて）
fpp_extensions:
  F90
  fpp

# .F90 はプリプロセスが必要なので true のまま
preprocess: true

{!./api-docs-index.md!}
