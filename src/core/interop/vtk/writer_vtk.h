#pragma once
#include <string>

// ===================================================================
//  Abstract base for all VTK-based grid writers.
//
//  Defines the lifecycle interface that every concrete writer must
//  implement.  Concrete subclasses (e.g. VtuWriter) provide
//  format-specific implementations.
//
//  Calling chain:
//    Fortran → ISO_C_BINDING → C wrapper (c_wrapper.cpp)
//              → C++ (VtkWriterBase / VtuWriter)
// ===================================================================

/*!
 * @brief Abstract base for all VTK-based unstructured-grid writers.
 *
 * Subclasses bind to a specific VTK output format.  The interface
 * follows RAII semantics: after `initialize()` succeeds `write()` and
 * `finalize()` must be called exactly once.
 *
 * Points are passed as interleaved (x,y,z) triplets so that
 * a Fortran column-major array `points(3, num_points)` maps
 * directly to `double* points` without any re-ordering.
 *
 * Connectivity follows the VTK XML "offsets" convention:
 * `offsets[i]` is the exclusive end position of cell `i` in the
 * flat `connectivity` array.
 */
class VtkWriterBase
{
public:
    virtual ~VtkWriterBase();

    /*!
     * @brief Bind the writer to an output file path.
     * @param[in] filename  Path to the target VTK output file.
     */
    virtual void initialize(const std::string &filename) = 0;

    /*!
     * @brief Populate the unstructured mesh topology.
     *
     * @param[in] num_points     Number of mesh vertices.
     * @param[in] points         Interleaved (x,y,z) coords; length = 3*num_points.
     * @param[in] num_cells      Number of cells.
     * @param[in] conn_size      Total length of the connectivity array.
     * @param[in] connectivity   0-based flat connectivity; length = conn_size.
     * @param[in] offsets        Per-cell exclusive end offsets; length = num_cells.
     * @param[in] cell_types     VTK cell-type IDs; length = num_cells.
     */
    virtual void setMesh(int num_points, const double *points,
                         int num_cells, int conn_size,
                         const int *connectivity, const int *offsets,
                         const int *cell_types) = 0;

    /*!
     * @brief Attach a named scalar point-data array (float64).
     * @param[in] name        Array name written to the output file.
     * @param[in] num_points  Number of mesh vertices.
     * @param[in] data        Scalar values; length = num_points.
     */
    virtual void addScalarPointData(const std::string &name,
                                    int num_points,
                                    const double *data) = 0;

    /*!
     * @brief Attach a named 3-component vector point-data array (float64).
     * @param[in] name        Array name written to the output file.
     * @param[in] num_points  Number of mesh vertices.
     * @param[in] data        Interleaved (vx,vy,vz) components; length = 3*num_points.
     */
    virtual void addVectorPointData(const std::string &name,
                                    int num_points,
                                    const double *data) = 0;

    /*!
     * @brief Attach a named scalar cell-data array (float64).
     * @param[in] name       Array name written to the output file.
     * @param[in] num_cells  Number of cells.
     * @param[in] data       Scalar values; length = num_cells.
     */
    virtual void addScalarCellData(const std::string &name,
                                   int num_cells,
                                   const double *data) = 0;

    /*!
     * @brief Attach a named 3-component vector cell-data array (float64).
     * @param[in] name       Array name written to the output file.
     * @param[in] num_cells  Number of cells.
     * @param[in] data       Interleaved (vx,vy,vz) components; length = 3*num_cells.
     */
    virtual void addVectorCellData(const std::string &name,
                                   int num_cells,
                                   const double *data) = 0;

    /*!
     * @brief Flush all data to disk.
     *
     * Point- and cell-data arrays attached via `addScalar*` / `addVector*`
     * are cleared after the write so subsequent calls start with an
     * empty field set.
     */
    virtual void write() = 0;

    /*!
     * @brief Release all VTK pipeline resources.
     */
    virtual void finalize() = 0;
};
