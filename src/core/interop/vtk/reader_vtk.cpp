#include "reader_vtk.h"
#include <algorithm>
#include <cstring>
#include <iostream>
#include <vtkCellArray.h>
#include <vtkCellData.h>
#include <vtkDataArray.h>
#include <vtkDoubleArray.h>
#include <vtkFloatArray.h>
#include <vtkIdList.h>
#include <vtkIdTypeArray.h>
#include <vtkIntArray.h>
#include <vtkPointData.h>
#include <vtkPoints.h>
#include <vtkUnsignedCharArray.h>

// ===================================================================
//  Bulk-copy helpers
//
//  These templates dispatch to std::memcpy when source and destination
//  types match, and fall back to a tight scalar loop (which the
//  compiler will auto-vectorise) when a type conversion is needed.
//  This eliminates all GetComponent / GetTuple virtual-call overhead.
// ===================================================================

namespace
{

/// Copy `n` elements from `src` to `dst`.  Fast-path when types match.
template <typename DstT, typename SrcT>
inline void bulkCopy(DstT *dst, const SrcT *src, std::size_t n)
{
    for (std::size_t i = 0; i < n; ++i)
        dst[i] = static_cast<DstT>(src[i]);
}

template <>
inline void bulkCopy<double, double>(double *dst, const double *src, std::size_t n)
{
    std::memcpy(dst, src, n * sizeof(double));
}

template <>
inline void bulkCopy<int, int>(int *dst, const int *src, std::size_t n)
{
    std::memcpy(dst, src, n * sizeof(int));
}

template <>
inline void bulkCopy<long long, long long>(long long *dst, const long long *src, std::size_t n)
{
    std::memcpy(dst, src, n * sizeof(long long));
}

/// Dispatch a bulk copy from a vtkDataArray to a typed buffer.
template <typename DstT>
void copyFromVtkArray(DstT *dst, vtkDataArray *arr, std::size_t total)
{
    switch (arr->GetDataType())
    {
    case VTK_DOUBLE:
        bulkCopy(dst, static_cast<const double *>(arr->GetVoidPointer(0)), total);
        return;
    case VTK_FLOAT:
        bulkCopy(dst, static_cast<const float *>(arr->GetVoidPointer(0)), total);
        return;
    case VTK_INT:
        bulkCopy(dst, static_cast<const int *>(arr->GetVoidPointer(0)), total);
        return;
    case VTK_ID_TYPE:
        bulkCopy(dst, static_cast<const vtkIdType *>(arr->GetVoidPointer(0)), total);
        return;
    case VTK_LONG:
        bulkCopy(dst, static_cast<const long *>(arr->GetVoidPointer(0)), total);
        return;
    case VTK_LONG_LONG:
        bulkCopy(dst, static_cast<const long long *>(arr->GetVoidPointer(0)), total);
        return;
    case VTK_SHORT:
        bulkCopy(dst, static_cast<const short *>(arr->GetVoidPointer(0)), total);
        return;
    case VTK_UNSIGNED_CHAR:
        bulkCopy(dst, static_cast<const unsigned char *>(arr->GetVoidPointer(0)), total);
        return;
    default:
        // Fallback: element-wise through the virtual API (rare path)
        for (std::size_t i = 0; i < total; ++i)
            dst[i] = static_cast<DstT>(arr->GetComponent(static_cast<vtkIdType>(i / arr->GetNumberOfComponents()),
                                                          static_cast<int>(i % arr->GetNumberOfComponents())));
        return;
    }
}

} // anonymous namespace

// ===================================================================
// VtkReader implementation
// ===================================================================

VtkReader::VtkReader() : initialized(false)
{
    this->reader = vtkSmartPointer<vtkUnstructuredGridReader>::New();
    this->grid = vtkSmartPointer<vtkUnstructuredGrid>::New();
}

int VtkReader::initialize(const char *filename)
{
    if (!filename || strlen(filename) == 0)
    {
        this->initialized = false;
        return -1;
    }

    this->reader = vtkSmartPointer<vtkUnstructuredGridReader>::New();
    this->reader->SetFileName(filename);
    this->reader->Update();

    this->grid = vtkSmartPointer<vtkUnstructuredGrid>::New();
    this->grid->ShallowCopy(this->reader->GetOutput());

    if (!this->grid || this->grid->GetNumberOfPoints() == 0)
    {
        std::cerr << "Error: Failed to read valid UnstructuredGrid data from: " << filename << std::endl;
        this->initialized = false;
        this->grid = vtkSmartPointer<vtkUnstructuredGrid>::New();
        return -1; // エラーコード
    }

    this->initialized = true;
    return 0; // 成功
}

void VtkReader::getHeaderInfo(char *format, int format_len, char *dataset, int dataset_len)
{
    if (!initialized)
        return;

    // Legacy VTKはASCIIかBINARYかをファイルから直接判断するのが難しいため、ASCIIと仮定
    strncpy(format, "ASCII", format_len - 1);
    format[format_len - 1] = '\0';

    // データセットタイプ
    strncpy(dataset, "UNSTRUCTURED_GRID", dataset_len - 1);
    dataset[dataset_len - 1] = '\0';
}

int VtkReader::getNumPoints()
{
    if (!initialized)
        return 0;
    return this->grid->GetNumberOfPoints();
}

void VtkReader::getPoints(double *x_arr, double *y_arr, double *z_arr)
{
    if (!initialized)
        return;

    vtkIdType num_points = this->grid->GetNumberOfPoints();
    vtkDataArray *pts_data = this->grid->GetPoints()->GetData();

    if (pts_data->GetDataType() == VTK_DOUBLE)
    {
        // Fast path: points stored as double — direct pointer access
        const double *raw = static_cast<const double *>(pts_data->GetVoidPointer(0));
        for (vtkIdType i = 0; i < num_points; ++i)
        {
            x_arr[i] = raw[3 * i];
            y_arr[i] = raw[3 * i + 1];
            z_arr[i] = raw[3 * i + 2];
        }
    }
    else if (pts_data->GetDataType() == VTK_FLOAT)
    {
        // Float path: convert while de-interleaving
        const float *raw = static_cast<const float *>(pts_data->GetVoidPointer(0));
        for (vtkIdType i = 0; i < num_points; ++i)
        {
            x_arr[i] = static_cast<double>(raw[3 * i]);
            y_arr[i] = static_cast<double>(raw[3 * i + 1]);
            z_arr[i] = static_cast<double>(raw[3 * i + 2]);
        }
    }
    else
    {
        // Rare fallback for other numeric types
        for (vtkIdType i = 0; i < num_points; ++i)
        {
            double p[3];
            pts_data->GetTuple(i, p);
            x_arr[i] = p[0];
            y_arr[i] = p[1];
            z_arr[i] = p[2];
        }
    }
}

int VtkReader::getNumCells()
{
    if (!initialized)
        return 0;
    return this->grid->GetNumberOfCells();
}

long long VtkReader::getTotalConnectivitySize()
{
    if (!initialized)
        return 0;
    return this->grid->GetCells()->GetConnectivityArray()->GetNumberOfTuples();
}

void VtkReader::getCellInfo(long long *connectivity, long long *offsets, int *types)
{
    if (!initialized)
        return;

    vtkCellArray *cells = this->grid->GetCells();

    // --- Connectivity: bulk copy ---
    vtkDataArray *conn_array = cells->GetConnectivityArray();
    long long num_conn_values = conn_array->GetNumberOfTuples();
    copyFromVtkArray(connectivity, conn_array, static_cast<std::size_t>(num_conn_values));

    // --- Offsets: try bulk copy from VTK 9.x offset array ---
    vtkDataArray *offset_array = cells->GetOffsetsArray();
    long long num_cells = this->grid->GetNumberOfCells();

    if (offset_array && offset_array->GetNumberOfTuples() == num_cells + 1)
    {
        copyFromVtkArray(offsets, offset_array, static_cast<std::size_t>(num_cells + 1));
    }
    else
    {
        // Legacy VTKではOffsetsが直接取得できないため、手動で計算する
        long long current_offset = 0;
        offsets[0] = 0;

        for (long long i = 0; i < num_cells; ++i)
        {
            vtkIdType num_points_in_cell;
            const vtkIdType *point_ids;
            this->grid->GetCellPoints(i, num_points_in_cell, point_ids);

            current_offset += num_points_in_cell;
            offsets[i + 1] = current_offset;
        }
    }

    // --- Cell types: bulk copy from vtkUnsignedCharArray ---
    vtkUnsignedCharArray *type_array = this->grid->GetCellTypesArray();
    if (type_array)
    {
        const unsigned char *src = static_cast<const unsigned char *>(type_array->GetVoidPointer(0));
        for (long long i = 0; i < num_cells; ++i)
            types[i] = static_cast<int>(src[i]);
    }
    else
    {
        for (long long i = 0; i < num_cells; ++i)
            types[i] = this->grid->GetCellType(i);
    }
}

void VtkReader::getCellDataInt32(const char *dataName, int *data)
{
    if (!initialized)
        return;

    vtkDataArray *data_array = this->grid->GetCellData()->GetArray(dataName);
    if (!data_array)
    {
        std::cerr << "Warning: Cell data '" << dataName << "' not found." << std::endl;
        return;
    }

    std::size_t total = static_cast<std::size_t>(data_array->GetNumberOfTuples()) *
                        static_cast<std::size_t>(data_array->GetNumberOfComponents());
    copyFromVtkArray(data, data_array, total);
}

void VtkReader::getCellDataFloat64(const char *dataName, double *data)
{
    if (!initialized)
        return;

    vtkDataArray *data_array = this->grid->GetCellData()->GetArray(dataName);
    if (!data_array)
    {
        std::cerr << "Warning: Cell data '" << dataName << "' not found." << std::endl;
        return;
    }

    std::size_t total = static_cast<std::size_t>(data_array->GetNumberOfTuples()) *
                        static_cast<std::size_t>(data_array->GetNumberOfComponents());
    copyFromVtkArray(data, data_array, total);
}

void VtkReader::getPointDataInt32(const char *dataName, int *data)
{
    if (!initialized)
        return;

    vtkDataArray *data_array = this->grid->GetPointData()->GetArray(dataName);
    if (!data_array)
    {
        std::cerr << "Warning: Point data array '" << dataName << "' not found in the file." << std::endl;
        return;
    }

    std::size_t total = static_cast<std::size_t>(data_array->GetNumberOfTuples()) *
                        static_cast<std::size_t>(data_array->GetNumberOfComponents());
    copyFromVtkArray(data, data_array, total);
}

void VtkReader::getPointDataFloat64(const char *dataName, double *data)
{
    if (!initialized)
        return;

    vtkDataArray *data_array = this->grid->GetPointData()->GetArray(dataName);
    if (!data_array)
    {
        std::cerr << "Warning: Point data array '" << dataName << "' not found in the file." << std::endl;
        return;
    }

    std::size_t total = static_cast<std::size_t>(data_array->GetNumberOfTuples()) *
                        static_cast<std::size_t>(data_array->GetNumberOfComponents());
    copyFromVtkArray(data, data_array, total);
}

int VtkReader::getNumberOfPointDataComponents(const char *dataName)
{
    if (!initialized)
        return 0;
    vtkDataArray *data_array = this->grid->GetPointData()->GetArray(dataName);
    if (!data_array)
    {
        // 配列が見つからない場合、警告は出さずに0を返す
        return 0;
    }
    return data_array->GetNumberOfComponents();
}

int VtkReader::getNumberOfCellDataComponents(const char *dataName)
{
    if (!initialized)
        return 0;
    vtkDataArray *data_array = this->grid->GetCellData()->GetArray(dataName);
    if (!data_array)
    {
        // 配列が見つからない場合、警告は出さずに0を返す
        return 0;
    }
    return data_array->GetNumberOfComponents();
}
