#include "reader.h"
#include <memory>

// グローバルなリーダーインスタンスを管理
static std::unique_ptr<VtkReader> g_reader = nullptr;

extern "C"
{

    // 関数1: リーダーを初期化する
    void c_vtk_initialize(const char *filename, int *error_code)
    {
        if (!g_reader)
        {
            g_reader = std::make_unique<VtkReader>();
        }
        *error_code = g_reader->initialize(filename);
    }

    // 関数2: ヘッダー情報を読み込む
    void c_vtk_read_header(char *format, int format_len, char *dataset, int dataset_len)
    {
        if (g_reader)
        {
            g_reader->getHeaderInfo(format, format_len, dataset, dataset_len);
        }
    }

    // 関数3: ポイントに関する情報を取得
    void c_vtk_get_num_points(int *num_points)
    {
        if (g_reader)
            *num_points = g_reader->getNumPoints();
        else
            *num_points = 0;
    }
    void c_vtk_get_points(double *x, double *y, double *z)
    {
        if (g_reader)
            g_reader->getPoints(x, y, z);
    }

    // 関数4: セルに関する情報を取得
    void c_vtk_get_num_cells(int *num_cells)
    {
        if (g_reader)
            *num_cells = g_reader->getNumCells();
        else
            *num_cells = 0;
    }
    void c_vtk_get_total_connectivity_size(long long *size)
    {
        if (g_reader)
            *size = g_reader->getTotalConnectivitySize();
        else
            *size = 0;
    }
    void c_vtk_get_cell_info(long long *connectivity, long long *offsets, int *types)
    {
        if (g_reader)
            g_reader->getCellInfo(connectivity, offsets, types);
    }

    // 関数5: "CellEntryIds"を取得する
    void c_vtk_get_cell_entity_ids(int *ids)
    {
        if (g_reader)
            g_reader->getCellDataInt32("CellEntityIds", ids);
    }

    // 後片付け用
    void c_vtk_finalize()
    {
        g_reader.reset();
    }
}