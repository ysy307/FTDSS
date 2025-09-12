#include "reader_vtk.h"
#include <cstring>
#include <iostream>
#include <vector>
#include <vtkCellArray.h>
#include <vtkCellData.h>
#include <vtkDataArray.h>
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

    // フォーマット
    const char *fmt = this->reader->GetInputString(); // Legacy VTKはASCII/BINARYを判定する良い方法がない
    strncpy(format, "ASCII", format_len - 1);         // ここではASCIIと仮定
    format[format_len - 1] = '\0';

    // データセットタイプ
    const char *ds_type = this->grid->GetClassName();       // e.g., "vtkUnstructuredGrid"
    strncpy(dataset, "UNSTRUCTURED_GRID", dataset_len - 1); // 一般的な名称に変換
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

    // --- Connectivity and Types (変更なし) ---
    vtkCellArray *cells = this->grid->GetCells();
    vtkDataArray *conn_array = cells->GetConnectivityArray();
    long long num_conn_values = conn_array->GetNumberOfTuples();
    for (long long i = 0; i < num_conn_values; ++i)
    {
        connectivity[i] = conn_array->GetTuple1(i);
    }

    // --- Offsets and Typesの計算 (Legacy VTK対応) ---
    long long num_cells = this->grid->GetNumberOfCells();
    long long current_offset = 0;
    offsets[0] = 0;

    for (long long i = 0; i < num_cells; ++i)
    {
        // Cell Typeを取得
        types[i] = this->grid->GetCellType(i);

        // 現在のセルの頂点数を取得
        vtkIdList *cell_points = vtkIdList::New();
        this->grid->GetCellPoints(i, cell_points);
        long long num_points_in_cell = cell_points->GetNumberOfIds();
        cell_points->Delete();

        // 次のオフセットを、現在のオフセットに頂点数を足して計算
        current_offset += num_points_in_cell;
        // offsets配列の範囲外に書き込まないようにチェック
        if (i + 1 <= num_cells)
        {
            offsets[i + 1] = current_offset;
        }
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

    int num_tuples = data_array->GetNumberOfTuples();
    for (int i = 0; i < num_tuples; ++i)
    {
        data[i] = static_cast<int>(data_array->GetTuple1(i));
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

    int num_tuples = data_array->GetNumberOfTuples();
    for (int i = 0; i < num_tuples; ++i)
    {
        data[i] = static_cast<int>(data_array->GetTuple1(i));
    }
}

void VtkReader::getPointDataInt32(const char *dataName, int *data)
{
    if (!initialized)
        return;

    // 節点データを名前で取得
    vtkDataArray *data_array = this->grid->GetPointData()->GetArray(dataName);
    if (!data_array)
    {
        std::cerr << "Warning: Point data array '" << dataName << "' not found in the file." << std::endl;
        return;
    }

    long long num_tuples = data_array->GetNumberOfTuples();
    for (long long i = 0; i < num_tuples; ++i)
    {
        // doubleで取得して格納
        data[i] = data_array->GetTuple1(i);
    }
}

void VtkReader::getPointDataFloat64(const char *dataName, double *data)
{
    if (!initialized)
        return;

    // 節点データを名前で取得
    vtkDataArray *data_array = this->grid->GetPointData()->GetArray(dataName);
    if (!data_array)
    {
        std::cerr << "Warning: Point data array '" << dataName << "' not found in the file." << std::endl;
        return;
    }

    long long num_tuples = data_array->GetNumberOfTuples();
    for (long long i = 0; i < num_tuples; ++i)
    {
        // doubleで取得して格納
        data[i] = data_array->GetTuple1(i);
    }
}