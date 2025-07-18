project: FTDSS
summary: This solver can calculate freezing/thawing processes in the soil integrated heat and water transport.
author: Kikuchi Shun
email: shungiku1012@gmail.com
project_github: https://github.com/ysy307/FTDSS.git
src_dir: ./src
output_dir: ./docs/api-docs
page_dir: ./docs
include:
    /opt/intel/oneapi/mpi/latest/include
    /opt/intel/oneapi/mkl/latest/include
extensions: F90
graph: true
coloured_edges: true
print_creation_date: true
creation_date: %Y-%m-%d %H:%M %z
predocmark_alt: >
predocmark: <
docmark_alt: *
docmark: !
display: public protected private
extra_mods:
json_module: http://jacobwilliams.github.io/json-fortran/
graph_maxnodes: 250
graph_maxdepth: 10
preprocess: true
fpp_extensions: fpp FPP

{!./api-docs-index.md!}