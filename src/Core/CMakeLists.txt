add_library(Core OBJECT
    Error.F90
    Allocate.F90
    BaseTypes.F90
    Element/Element.F90
    Element/Element_Base.F90
    Element/Element_SquareFirst.F90
    Element/Element_SquareSecond.F90
    Element/Element_TriangleFirst.F90
    Element/Element_TriangleSecond.F90
    Side/Side.F90
    Side/Side_Base.F90
    Side/Side_First.F90
    Side/Side_Second.F90
    Unique.F90
    VTK.F90
)
enable_build_flags(Core)
enable_thirdparty (Core)