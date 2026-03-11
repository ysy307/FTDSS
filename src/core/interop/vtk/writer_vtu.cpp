#include "writer_vtu.h"
#include <algorithm>
#include <cfenv>
#include <cstring>
#include <future>
#include <iostream>
#include <vtkCellArray.h>
#include <vtkCellData.h>
#include <vtkDoubleArray.h>
#include <vtkIdTypeArray.h>
#include <vtkPointData.h>
#include <vtkPoints.h>
#include <vtkUnsignedCharArray.h>

// ===================================================================
//  VtuWriter — Implementation  (async I/O edition)
//
//  Performance notes
//  -----------------
//  * Field arrays are memcpy'd into VTK-owned buffers at addScalar*/
//    addVector* time.  This decouples the Fortran buffer lifetime
//    from the background write operation.
//  * write() snapshots the entire vtkUnstructuredGrid into a
//    self-contained writer+grid pair and launches it on a background
//    thread via std::async.  The main thread returns immediately.
//  * The next write() or finalize() call joins the previous future
//    before proceeding, ensuring serial write completion order.
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

VtuWriter::~VtuWriter()
{
    // Ensure any in-flight background write completes before destruction.
    waitForWrite();
}

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
    // Ensure any in-flight background write completes before
    // modifying the staging grid.
    waitForWrite();

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

    // --- Connectivity: bulk copy int→vtkIdType ----------------------
    auto cell_conn = vtkSmartPointer<vtkIdTypeArray>::New();
    cell_conn->SetNumberOfTuples(conn_size);
    vtkIdType *conn_ptr = cell_conn->GetPointer(0);
    std::copy(connectivity, connectivity + conn_size, conn_ptr);

    // VTK CellArray (9.x API) expects (num_cells+1) offsets where
    // offsets_vtk[0] = 0 and offsets_vtk[i+1] = offsets[i].
    // The caller passes VTK XML-style offsets (offsets[i] = exclusive
    // end of cell i), so we prepend 0.
    auto cell_offsets = vtkSmartPointer<vtkIdTypeArray>::New();
    cell_offsets->SetNumberOfTuples(num_cells + 1);
    vtkIdType *off_ptr = cell_offsets->GetPointer(0);
    off_ptr[0] = 0;
    std::copy(offsets, offsets + num_cells, off_ptr + 1);

    auto cell_array = vtkSmartPointer<vtkCellArray>::New();
    cell_array->SetData(cell_offsets, cell_conn);

    // --- Cell types: bulk copy int→unsigned char -------------------
    auto types_arr = vtkSmartPointer<vtkUnsignedCharArray>::New();
    types_arr->SetNumberOfTuples(num_cells);
    unsigned char *types_ptr = types_arr->GetPointer(0);
    std::copy(cell_types, cell_types + num_cells, types_ptr);

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
// write  (async: snapshot grid → background thread)
// ------------------------------------------------------------

void VtuWriter::write()
{
    // Wait for any previous background write to finish first.
    waitForWrite();

    // Build a lightweight snapshot that shares the mesh topology
    // (points + cells) with the staging grid — no copy of coordinates
    // or connectivity.  Only the field data arrays (which are already
    // VTK-owned via memcpy in addScalar*/addVector*) are moved into
    // the snapshot so write() owns them exclusively.
    auto snapshot = vtkSmartPointer<vtkUnstructuredGrid>::New();

    // Share mesh topology (zero-copy — reference-counted).
    snapshot->SetPoints(grid_->GetPoints());
    snapshot->SetCells(grid_->GetCellTypesArray(),
                       grid_->GetCells());

    // Transfer field data arrays to the snapshot (reference-counted
    // move, not a deep copy).  Each vtkDataArray was freshly created
    // in addScalar*/addVector* and is now exclusively owned by the
    // snapshot after we clear the staging grid.
    vtkPointData *pd = grid_->GetPointData();
    for (int i = 0; i < pd->GetNumberOfArrays(); ++i)
        snapshot->GetPointData()->AddArray(pd->GetArray(i));

    vtkCellData *cd = grid_->GetCellData();
    for (int i = 0; i < cd->GetNumberOfArrays(); ++i)
        snapshot->GetCellData()->AddArray(cd->GetArray(i));

    // Clear field arrays on the staging grid immediately so the
    // Fortran side can start attaching new fields for the next step.
    pd->Initialize();
    cd->Initialize();

    // Capture filename by value for the lambda.
    std::string fname = filename_;

    // Launch background write.
    write_future_ = std::async(std::launch::async,
                               [snapshot, fname]()
                               {
                                   auto bg_writer = vtkSmartPointer<vtkXMLUnstructuredGridWriter>::New();
                                   bg_writer->SetFileName(fname.c_str());
                                   bg_writer->SetDataModeToBinary();
                                   bg_writer->SetEncodeAppendedData(false);
                                   bg_writer->SetCompressorTypeToNone();
                                   bg_writer->SetInputData(snapshot);

                                   // Keep strict Fortran FPE settings outside this library boundary.
                                   fenv_t env;
                                   feholdexcept(&env);
                                   const int ok = bg_writer->Write();
                                   fesetenv(&env);

                                   if (ok <= 0)
                                   {
                                       std::cerr << "Error: VTK failed to write file: " << fname << std::endl;
                                   }
                               });
}

// ------------------------------------------------------------
// waitForWrite
// ------------------------------------------------------------

void VtuWriter::waitForWrite()
{
    if (write_future_.valid())
    {
        write_future_.get();
    }
}

// ------------------------------------------------------------
// finalize
// ------------------------------------------------------------

void VtuWriter::finalize()
{
    waitForWrite();
    grid_ = nullptr;
    writer_ = nullptr;
}
