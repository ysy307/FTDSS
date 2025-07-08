add_library(Domain OBJECT
    Element/Element.F90
    Element/Element_SquareFirst.F90
    Element/Element_SquareSecond.F90
    Element/Element_TriangleFirst.F90
    Element/Element_TriangleSecond.F90
    Element/Element_Factory.F90
    Side/Side.F90
    Side/Side_First.F90
    Side/Side_Second.F90
    Side/Side_Factory.F90
    adjacency/node.F90
    adjacency/element.F90
    adjacency/adjacency.F90
    multicoloring/multicoloring.F90
    rcm/rcm.F90
    Domain_Module.F90
)
target_link_libraries(Domain PRIVATE
    Core
    Input
)
enable_build_flags(Domain)
enable_thirdparty (Domain)