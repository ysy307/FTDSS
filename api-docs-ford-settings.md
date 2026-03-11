---
project: FTCMS
summary: This solver can calculate freezing/thawing processes in the soil integrated heat and water transport.
author: Kikuchi Shun
email: shungiku1012@gmail.com
project_github: https://github.com/ysy307/FTCMS.git
src_dir: ./src
include: ./src
         ./include
output_dir: ./docs/api-docs
page_dir: ./docs/pages
fixed_length_limit: False
extensions: f90
            F90
            fypp
fpp_extensions: F90
                fypp
preprocess: true
preprocessor: fypp -I include
display: public
         protected
         private
source: true
sort: alpha
extra_mods: iso_fortran_env:https://gcc.gnu.org/onlinedocs/gfortran/ISO_005fFORTRAN_005fENV.html
            iso_c_binding:https://gcc.gnu.org/onlinedocs/gfortran/ISO_005fC_005fBINDING.html#ISO_005fC_005fBINDING
            ieee_arithmetic:https://gcc.gnu.org/onlinedocs/gfortran/IEEE-modules.html
            json_module:http://jacobwilliams.github.io/json-fortran/
graph: true
graph_maxnodes: 500
graph_maxdepth: 50
coloured_edges: true
print_creation_date: true
creation_date: %Y-%m-%d %H:%M %z
docmark: !
predocmark: >
md_extensions: markdown.extensions.toc
---

{!./docs/pages/top.md!}