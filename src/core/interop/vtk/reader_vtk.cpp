#include "reader_vtk.h"
#include <cstring>
#include <iostream>
#include <vtkCellArray.h>
#include <vtkCellData.h>
#include <vtkDataArray.h>
#include <vtkIdList.h>
#include <vtkPointData.h>

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
    int num_points = this->grid->GetNumberOfPoints();
    for (int i = 0; i < num_points; ++i)
    {
        double p[3];
        this->grid->GetPoint(i, p);
        x_arr[i] = p[0];
        y_arr[i] = p[1];
        z_arr[i] = p[2];
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
    vtkDataArray *conn_array = cells->GetConnectivityArray();
    long long num_conn_values = conn_array->GetNumberOfTuples();
    for (long long i = 0; i < num_conn_values; ++i)
    {
        connectivity[i] = conn_array->GetTuple1(i);
    }

    // Legacy VTKではOffsetsが直接取得できないため、手動で計算する
    long long num_cells = this->grid->GetNumberOfCells();
    long long current_offset = 0;
    offsets[0] = 0;

    for (long long i = 0; i < num_cells; ++i)
    {
        types[i] = this->grid->GetCellType(i);

        vtkIdType num_points_in_cell;
        const vtkIdType *point_ids;
        this->grid->GetCellPoints(i, num_points_in_cell, point_ids);

        current_offset += num_points_in_cell;
        if (i + 1 <= num_cells)
        {
            offsets[i + 1] = current_offset;
        }
    }
}

// --- ここから修正 ---

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

    long long num_tuples = data_array->GetNumberOfTuples();
    int num_components = data_array->GetNumberOfComponents();
    long long k = 0;
    for (long long i = 0; i < num_tuples; ++i)
    {
        for (int j = 0; j < num_components; ++j)
        {
            data[k++] = static_cast<int>(data_array->GetComponent(i, j));
        }
    }
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

    long long num_tuples = data_array->GetNumberOfTuples();
    int num_components = data_array->GetNumberOfComponents();
    long long k = 0;
    for (long long i = 0; i < num_tuples; ++i)
    {
        for (int j = 0; j < num_components; ++j)
        {
            data[k++] = data_array->GetComponent(i, j); // 元のコードのバグも修正
        }
    }
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

    long long num_tuples = data_array->GetNumberOfTuples();
    int num_components = data_array->GetNumberOfComponents();
    long long k = 0;
    for (long long i = 0; i < num_tuples; ++i)
    {
        for (int j = 0; j < num_components; ++j)
        {
            data[k++] = static_cast<int>(data_array->GetComponent(i, j));
        }
    }
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

    long long num_tuples = data_array->GetNumberOfTuples();
    int num_components = data_array->GetNumberOfComponents();
    long long k = 0;
    for (long long i = 0; i < num_tuples; ++i)
    {
        for (int j = 0; j < num_components; ++j)
        {
            data[k++] = data_array->GetComponent(i, j);
        }
    }
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