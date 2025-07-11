#ifndef VTK_READER_H
#define VTK_READER_H

#include <string>
#include <vtkSmartPointer.h>
#include <vtkUnstructuredGrid.h>
#include <vtkUnstructuredGridReader.h>

class VtkReader
{
public:
    VtkReader();
    // ファイルを読み込み、リーダーを初期化
    int initialize(const char *filename);

    // ヘッダー情報（フォーマットとデータセットタイプ）を取得
    void getHeaderInfo(char *format, int format_len, char *dataset, int dataset_len);

    // ポイント数を取得
    int getNumPoints();
    // ポイント座標を取得
    void getPoints(double *x, double *y, double *z);

    // セル数を取得
    int getNumCells();
    // Connectivity配列の総サイズを取得 (Fortranでの確保用)
    long long getTotalConnectivitySize();

    // セルの接続性、オフセット、タイプを取得
    void getCellInfo(long long *connectivity, long long *offsets, int *types);

    // 指定された名前のセルデータを取得 (整数型を想定)
    void getCellDataInt32(const char *dataName, int *data);

private:
    vtkSmartPointer<vtkUnstructuredGridReader> reader;
    vtkSmartPointer<vtkUnstructuredGrid> grid;
    bool initialized;
};

#endif // VTK_READER_H