#pragma once
#include "writer_vtk.h"
#include <string>
#include <vtkSmartPointer.h>
#include <vtkUnstructuredGrid.h>
#include <vtkXMLUnstructuredGridWriter.h>

// ===================================================================
//  Concrete VTK XML Unstructured Grid writer.
//
//  Writes .vtu files using binary VTK XML encoding with raw-appended
//  data (SetEncodeAppendedData(false)).  This format provides the
//  highest throughput for large-scale meshes because it avoids
//  base64 encoding overhead and ASCII conversion.
//
//  Memory strategy
//  ---------------
//  * Point coordinates: zero-copy via vtkDoubleArray::SetArray()
//    (the caller-owned buffer is never freed by VTK).
//  * Field arrays (scalar / vector): zero-copy via SetArray().
//  * Cell connectivity: one copy of the index array is required to
//    construct the vtkCellArray offset representation.
// ===================================================================

/*!
 * @brief High-performance VTK XML unstructured grid writer (VtuWriter).
 *
 * Concrete implementation of VtkWriterBase that serialises a
 * @c vtkUnstructuredGrid to a .vtu file in binary VTK XML format
 * with raw appended data encoding.
 *
 * RAII semantics are enforced via @c vtkSmartPointer.
 * A single instance writes one file: call `initialize()` → populate
 * mesh/fields → `write()` → `finalize()`.
 */
class VtuWriter : public VtkWriterBase
{
public:
    VtuWriter();
    ~VtuWriter() override;

    /*!
     * @brief Bind the writer to an output .vtu file path.
     *
     * Configures binary VTK XML output with raw (non-base64) appended
     * data encoding and no compression for maximum I/O throughput.
     * Must be called before any other method.
     *
     * @param[in] filename  Path to the target .vtu file.
     */
    void initialize(const std::string &filename) override;

    /*!
     * @brief Define the unstructured mesh topology.
     *
     * Point coordinates are transferred zero-copy via
     * @c vtkDoubleArray::SetArray().  The connectivity and offsets
     * arrays are converted into the internal @c vtkCellArray format.
     *
     * @param[in] num_points     Number of mesh vertices.
     * @param[in] points         Interleaved (x,y,z) coordinates; length = 3*num_points.
     * @param[in] num_cells      Number of cells.
     * @param[in] conn_size      Total length of the connectivity array.
     * @param[in] connectivity   0-based flat connectivity; length = conn_size.
     * @param[in] offsets        VTK XML per-cell end offsets; length = num_cells.
     * @param[in] cell_types     VTK cell-type IDs; length = num_cells.
     */
    void setMesh(int num_points, const double *points,
                 int num_cells, int conn_size,
                 const int *connectivity, const int *offsets,
                 const int *cell_types) override;

    /*!
     * @brief Attach a named scalar point-data array (float64).
     *
     * The caller-owned buffer is referenced zero-copy.
     *
     * @param[in] name        Array name written to the VTU file.
     * @param[in] num_points  Number of mesh vertices.
     * @param[in] data        Scalar values; length = num_points.
     */
    void addScalarPointData(const std::string &name,
                            int num_points,
                            const double *data) override;

    /*!
     * @brief Attach a named 3-component vector point-data array (float64).
     *
     * The caller-owned buffer is referenced zero-copy.
     *
     * @param[in] name        Array name written to the VTU file.
     * @param[in] num_points  Number of mesh vertices.
     * @param[in] data        Interleaved (vx,vy,vz) components; length = 3*num_points.
     */
    void addVectorPointData(const std::string &name,
                            int num_points,
                            const double *data) override;

    /*!
     * @brief Attach a named scalar cell-data array (float64).
     *
     * The caller-owned buffer is referenced zero-copy.
     *
     * @param[in] name       Array name written to the VTU file.
     * @param[in] num_cells  Number of cells.
     * @param[in] data       Scalar values; length = num_cells.
     */
    void addScalarCellData(const std::string &name,
                           int num_cells,
                           const double *data) override;

    /*!
     * @brief Attach a named 3-component vector cell-data array (float64).
     *
     * The caller-owned buffer is referenced zero-copy.
     *
     * @param[in] name       Array name written to the VTU file.
     * @param[in] num_cells  Number of cells.
     * @param[in] data       Interleaved (vx,vy,vz) components; length = 3*num_cells.
     */
    void addVectorCellData(const std::string &name,
                           int num_cells,
                           const double *data) override;

    /*!
     * @brief Flush the grid to disk.
     *
     * Invokes the VTK pipeline update and writes the .vtu file.
     * After writing, all point- and cell-data arrays are cleared so
     * the grid is ready for the next time-step without reinitialising
     * the mesh topology.
     */
    void write() override;

    /*!
     * @brief Release all VTK pipeline resources.
     *
     * Sets internal @c vtkSmartPointer members to @c nullptr,
     * decrementing reference counts and freeing memory.
     */
    void finalize() override;

private:
    //> Internal unstructured grid accumulating mesh and field data.
    vtkSmartPointer<vtkUnstructuredGrid> grid_;
    //> VTK XML writer bound to grid_ via SetInputData().
    vtkSmartPointer<vtkXMLUnstructuredGridWriter> writer_;
    //> Target file path set by initialize().
    std::string filename_;
};
