#include "reader_vtu.h"
#include <cstring>
#include <iostream>
#include <vtkCellArray.h>
#include <vtkCellData.h>
#include <vtkDataArray.h>
#include <vtkPointData.h>

VtuReader::VtuReader() : initialized(false)
{
    this->reader = vtkSmartPointer<vtkXMLUnstructuredGridReader>::New();
    this->grid = vtkSmartPointer<vtkUnstructuredGrid>::New();
}

int VtuReader::initialize(const char *filename)
{
    if (!filename || strlen(filename) == 0)
    {
        this->initialized = false;
        return -1;
    }

    // ★★★ 修正点①: 毎回新しいリーダーのインスタンスを生成する ★★★
    // これにより、古いファイル名やエラー状態が残るのを防ぐ
    this->reader = vtkSmartPointer<vtkXMLUnstructuredGridReader>::New();
    this->reader->SetFileName(filename);
    this->reader->Update();

    // ★★★ 修正点②: 新しいグリッドのインスタンスに結果をコピーする ★★★
    // 直接ポインタを代入するのではなく、ShallowCopyを使うのがより安全
    this->grid = vtkSmartPointer<vtkUnstructuredGrid>::New();
    this->grid->ShallowCopy(this->reader->GetOutput());

    if (!this->grid || this->grid->GetNumberOfPoints() == 0)
    {
        std::cerr << "Error: Failed to read valid UnstructuredGrid data from: " << filename << std::endl;
        this->initialized = false;
        // 失敗した場合も、gridをクリーンな状態に保つ
        this->grid = vtkSmartPointer<vtkUnstructuredGrid>::New();
        return -1;
    }

    this->initialized = true;
    return 0;
}

void VtuReader::getHeaderInfo(char *format, int format_len, char *dataset, int dataset_len)
{
    if (!initialized)
        return;
    strncpy(format, "XML", format_len - 1);
    format[format_len - 1] = '\0';
    strncpy(dataset, "UNSTRUCTURED_GRID", dataset_len - 1);
    dataset[dataset_len - 1] = '\0';
}

int VtuReader::getNumPoints()
{
    if (!initialized)
        return 0;
    return this->grid->GetNumberOfPoints();
}

void VtuReader::getPoints(double *x_arr, double *y_arr, double *z_arr)
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

int VtuReader::getNumCells()
{
    if (!initialized)
        return 0;
    return this->grid->GetNumberOfCells();
}

long long VtuReader::getTotalConnectivitySize()
{
    if (!initialized)
        return 0;
    return this->grid->GetCells()->GetConnectivityArray()->GetNumberOfTuples();
}

void VtuReader::getCellInfo(long long *connectivity, long long *offsets, int *types)
{
    if (!initialized)
        return;
    vtkCellArray *cells = this->grid->GetCells();
    long long num_cells = cells->GetNumberOfCells();
    vtkDataArray *conn_array = cells->GetConnectivityArray();
    long long num_conn_values = conn_array->GetNumberOfTuples();
    for (long long i = 0; i < num_conn_values; ++i)
    {
        connectivity[i] = conn_array->GetTuple1(i) + 1;
    }
    vtkDataArray *offset_array = cells->GetOffsetsArray();
    for (long long i = 0; i < num_cells + 1; ++i)
    {
        offsets[i] = offset_array->GetTuple1(i);
    }
    for (long long i = 0; i < num_cells; ++i)
    {
        types[i] = this->grid->GetCellType(i);
    }
}

void VtuReader::getCellDataInt32(const char *dataName, int *data)
{
    if (!initialized)
        return;
    vtkDataArray *data_array = this->grid->GetCellData()->GetArray(dataName);
    if (!data_array)
    {
        std::cerr << "Warning: Cell data array '" << dataName << "' not found in the file." << std::endl;
        return;
    }
    long long num_tuples = data_array->GetNumberOfTuples();
    for (long long i = 0; i < num_tuples; ++i)
    {
        data[i] = static_cast<int>(data_array->GetTuple1(i));
    }
}

void VtuReader::getPointDataFloat64(const char *dataName, double *data)
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
    for (long long i = 0; i < num_tuples; ++i)
    {
        data[i] = data_array->GetTuple1(i);
    }
}