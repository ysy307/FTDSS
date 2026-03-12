#include "writer_vtk.h"

// Translation unit for VtkWriterBase ensures the virtual destructor
// vtable is generated in exactly one object file.
VtkWriterBase::~VtkWriterBase() = default;
