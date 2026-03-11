#include "writer_vtu.h"
#include <cfenv>
#include <cstring>
#include <iostream>
#include <vtkCellArray.h>
#include <vtkCellData.h>
#include <vtkDoubleArray.h>
#include <vtkIdTypeArray.h>
#include <vtkPointData.h>
#include <vtkPoints.h>
#include <vtkUnsignedCharArray.h>

// ===================================================================
//  VtuWriter — Implementation
//
//  Performance notes
//  -----------------
//  * vtkDoubleArray::SetArray(ptr, n, 1) — the third argument ("save")
//    tells VTK NOT to free the caller-owned buffer.  This gives
//    zero-copy access to Fortran-managed coordinate and field arrays.
//  * vtkCellArray is populated by copying connectivity indices once.
//    The index array is typically O(num_cells) while field data is
//    O(num_points), so the copy cost is negligible.
//  * Binary VTK XML with SetEncodeAppendedData(false) writes a raw
//    appended data block, bypassing base64 encoding entirely.
// ===================================================================

// ------------------------------------------------------------
// Constructor / Destructor
// ------------------------------------------------------------

VtuWriter::VtuWriter()
    : grid_(vtkSmartPointer<vtkUnstructuredGrid>::New()),
      writer_(vtkSmartPointer<vtkXMLUnstructuredGridWriter>::New())
{
}

VtuWriter::~VtuWriter() = default;

// ------------------------------------------------------------
// initialize
// ------------------------------------------------------------

void VtuWriter::initialize(const std::string &filename)
{
    filename_ = filename;
    writer_->SetFileName(filename_.c_str());
    // Binary VTK XML with raw appended data: maximum I/O throughput
    writer_->SetDataModeToBinary();
    writer_->SetEncodeAppendedData(false);
    writer_->SetCompressorTypeToNone();
    writer_->SetInputData(grid_);
}

// ------------------------------------------------------------
// setMesh
// ------------------------------------------------------------

void VtuWriter::setMesh(int num_points, const double *points,
                        int num_cells, int conn_size,
                        const int *connectivity, const int *offsets,
                        const int *cell_types)
{
    // --- Points: zero-copy via SetArray ----------------------------
    // Fortran column-major layout for points(3, num_points) gives
    // memory order (x1,y1,z1, x2,y2,z2, ...) which VTK expects.
    auto coords = vtkSmartPointer<vtkDoubleArray>::New();
    coords->SetNumberOfComponents(3);
    coords->SetArray(const_cast<double *>(points),
                     static_cast<vtkIdType>(3) * num_points,
                     1 /* save: do not free caller-owned buffer */);

    auto vtk_points = vtkSmartPointer<vtkPoints>::New();
    vtk_points->SetDataTypeToDouble();
    vtk_points->SetData(coords);
    grid_->SetPoints(vtk_points);

    // --- Connectivity ----------------------------------------------
    // Convert the flat connectivity array to vtkIdTypeArray.
    auto cell_conn = vtkSmartPointer<vtkIdTypeArray>::New();
    cell_conn->SetNumberOfTuples(conn_size);
    for (int i = 0; i < conn_size; ++i)
        cell_conn->SetValue(i, static_cast<vtkIdType>(connectivity[i]));

    // VTK CellArray (9.x API) expects (num_cells+1) offsets where
    // offsets_vtk[0] = 0 and offsets_vtk[i+1] = offsets[i].
    // The caller passes VTK XML-style offsets (offsets[i] = exclusive
    // end of cell i), so we prepend 0.
    auto cell_offsets = vtkSmartPointer<vtkIdTypeArray>::New();
    cell_offsets->SetNumberOfTuples(num_cells + 1);
    cell_offsets->SetValue(0, 0);
    for (int i = 0; i < num_cells; ++i)
        cell_offsets->SetValue(i + 1, static_cast<vtkIdType>(offsets[i]));

    auto cell_array = vtkSmartPointer<vtkCellArray>::New();
    cell_array->SetData(cell_offsets, cell_conn);

    // --- Cell types ------------------------------------------------
    auto types_arr = vtkSmartPointer<vtkUnsignedCharArray>::New();
    types_arr->SetNumberOfTuples(num_cells);
    for (int i = 0; i < num_cells; ++i)
        types_arr->SetValue(i, static_cast<unsigned char>(cell_types[i]));

    grid_->SetCells(types_arr, cell_array);
}

// ------------------------------------------------------------
// addScalarPointData
// ------------------------------------------------------------

void VtuWriter::addScalarPointData(const std::string &name,
                                   int num_points,
                                   const double *data)
{
    auto arr = vtkSmartPointer<vtkDoubleArray>::New();
    arr->SetName(name.c_str());
    arr->SetNumberOfComponents(1);
    arr->SetNumberOfTuples(num_points);
    std::memcpy(arr->GetVoidPointer(0), data,
                static_cast<size_t>(num_points) * sizeof(double));
    grid_->GetPointData()->AddArray(arr);
}

// ------------------------------------------------------------
// addVectorPointData
// ------------------------------------------------------------

void VtuWriter::addVectorPointData(const std::string &name,
                                   int num_points,
                                   const double *data)
{
    auto arr = vtkSmartPointer<vtkDoubleArray>::New();
    arr->SetName(name.c_str());
    arr->SetNumberOfComponents(3);
    arr->SetNumberOfTuples(num_points);
    std::memcpy(arr->GetVoidPointer(0), data,
                static_cast<size_t>(3) * static_cast<size_t>(num_points) * sizeof(double));
    grid_->GetPointData()->AddArray(arr);
}

// ------------------------------------------------------------
// addScalarCellData
// ------------------------------------------------------------

void VtuWriter::addScalarCellData(const std::string &name,
                                  int num_cells,
                                  const double *data)
{
    auto arr = vtkSmartPointer<vtkDoubleArray>::New();
    arr->SetName(name.c_str());
    arr->SetNumberOfComponents(1);
    arr->SetNumberOfTuples(num_cells);
    std::memcpy(arr->GetVoidPointer(0), data,
                static_cast<size_t>(num_cells) * sizeof(double));
    grid_->GetCellData()->AddArray(arr);
}

// ------------------------------------------------------------
// addVectorCellData
// ------------------------------------------------------------

void VtuWriter::addVectorCellData(const std::string &name,
                                  int num_cells,
                                  const double *data)
{
    auto arr = vtkSmartPointer<vtkDoubleArray>::New();
    arr->SetName(name.c_str());
    arr->SetNumberOfComponents(3);
    arr->SetNumberOfTuples(num_cells);
    std::memcpy(arr->GetVoidPointer(0), data,
                static_cast<size_t>(3) * static_cast<size_t>(num_cells) * sizeof(double));
    grid_->GetCellData()->AddArray(arr);
}

// ------------------------------------------------------------
// write
// ------------------------------------------------------------

void VtuWriter::write()
{
    // Keep strict Fortran FPE settings outside this library boundary.
    // VTK may raise FP exception flags internally even on successful writes.
    fenv_t env;
    feholdexcept(&env);
    const int ok = writer_->Write();
    fesetenv(&env);

    if (ok <= 0)
    {
        std::cerr << "Error: VTK failed to write file: " << filename_ << std::endl;
    }

    // Clear field arrays so the next call starts with a clean slate.
    // The mesh topology (points + cells) is preserved for reuse.
    grid_->GetPointData()->Initialize();
    grid_->GetCellData()->Initialize();
}

// ------------------------------------------------------------
// finalize
// ------------------------------------------------------------

void VtuWriter::finalize()
{
    grid_ = nullptr;
    writer_ = nullptr;
}
