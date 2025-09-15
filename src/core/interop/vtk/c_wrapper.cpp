#include "reader_vtk.h"
#include "reader_vtu.h"
#include <iostream>

// ===================================================================
// このファイルでは、グローバル変数を一切使用しません。
// 全ての操作は、Fortranから渡されるハンドル（オブジェクトのポインタ）
// 経由で行われます。
// ===================================================================

extern "C"
{
    // ===================================================================
    // VTK (.vtk) 用のC言語インターフェース関数群
    // ===================================================================

    /**
     * @brief VTKリーダーを初期化し、そのハンドル(ポインタ)を返す
     */
    void *c_vtk_initialize(const char *filename, int *ierr)
    {
        VtkReader *reader = new VtkReader();
        *ierr = reader->initialize(filename);
        if (*ierr != 0)
        {
            delete reader; // 失敗した場合はここで解放
            return nullptr;
        }
        return static_cast<void *>(reader); // 成功したらハンドルを返す
    }

    /**
     * @brief VTKリーダーのハンドルを受け取り、対応するオブジェクトを解放する
     */
    void c_vtk_finalize(void *handle)
    {
        if (!handle)
            return;
        VtkReader *reader = static_cast<VtkReader *>(handle);
        delete reader;
    }

    /**
     * @brief ヘッダー情報を取得する
     */
    void c_vtk_read_header(void *handle, char *format, int format_len, char *dataset, int dataset_len)
    {
        if (!handle)
            return;
        static_cast<VtkReader *>(handle)->getHeaderInfo(format, format_len, dataset, dataset_len);
    }

    /**
     * @brief ポイント数を取得する
     */
    void c_vtk_get_num_points(void *handle, int *num_points)
    {
        if (!handle)
        {
            *num_points = 0;
            return;
        }
        *num_points = static_cast<VtkReader *>(handle)->getNumPoints();
    }

    /**
     * @brief ポイント座標を取得する
     */
    void c_vtk_get_points(void *handle, double *x, double *y, double *z)
    {
        if (!handle)
            return;
        static_cast<VtkReader *>(handle)->getPoints(x, y, z);
    }

    /**
     * @brief セル数を取得する
     */
    void c_vtk_get_num_cells(void *handle, int *num_cells)
    {
        if (!handle)
        {
            *num_cells = 0;
            return;
        }
        *num_cells = static_cast<VtkReader *>(handle)->getNumCells();
    }

    /**
     * @brief 全コネクティビティ配列のサイズを取得する
     */
    void c_vtk_get_total_connectivity_size(void *handle, long long *size)
    {
        if (!handle)
        {
            *size = 0;
            return;
        }
        *size = static_cast<VtkReader *>(handle)->getTotalConnectivitySize();
    }

    /**
     * @brief セル情報（コネクティビティ、オフセット、タイプ）を取得する
     */
    void c_vtk_get_cell_info(void *handle, long long *connectivity, long long *offsets, int *types)
    {
        if (!handle)
            return;
        static_cast<VtkReader *>(handle)->getCellInfo(connectivity, offsets, types);
    }

    // --- ここから追加 ---
    /**
     * @brief 名前で指定されたセルデータ配列の成分数を取得する
     */
    void c_vtk_get_num_cell_data_components(void *handle, const char *array_name, int *num_components)
    {
        if (!handle)
        {
            *num_components = 0;
            return;
        }
        *num_components = static_cast<VtkReader *>(handle)->getNumberOfCellDataComponents(array_name);
    }

    /**
     * @brief 名前で指定されたポイントデータ配列の成分数を取得する
     */
    void c_vtk_get_num_point_data_components(void *handle, const char *array_name, int *num_components)
    {
        if (!handle)
        {
            *num_components = 0;
            return;
        }
        *num_components = static_cast<VtkReader *>(handle)->getNumberOfPointDataComponents(array_name);
    }
    // --- ここまで追加 ---

    /**
     * @brief 名前で指定されたセルデータ配列を取得する
     */
    void c_vtk_get_cell_data_int32(void *handle, const char *array_name, int *data)
    {
        if (!handle)
            return;
        static_cast<VtkReader *>(handle)->getCellDataInt32(array_name, data);
    }
    void c_vtk_get_cell_data_float64(void *handle, const char *array_name, double *data)
    {
        if (!handle)
            return;
        static_cast<VtkReader *>(handle)->getCellDataFloat64(array_name, data);
    }

    /**
     * @brief 名前で指定されたポイントデータ配列を取得する
     */
    void c_vtk_get_point_data_int32(void *handle, const char *array_name, int *data)
    {
        if (!handle)
            return;
        static_cast<VtkReader *>(handle)->getPointDataInt32(array_name, data);
    }
    void c_vtk_get_point_data_float64(void *handle, const char *array_name, double *data)
    {
        if (!handle)
            return;
        static_cast<VtkReader *>(handle)->getPointDataFloat64(array_name, data);
    }

    // ===================================================================
    // VTU (.vtu) 用のC言語インターフェース関数群
    // ===================================================================

    /**
     * @brief VTUリーダーを初期化し、そのハンドル(ポインタ)を返す
     */
    void *c_vtu_initialize(const char *filename, int *ierr)
    {
        VtuReader *reader = new VtuReader();
        *ierr = reader->initialize(filename);
        if (*ierr != 0)
        {
            delete reader; // 失敗した場合はここで解放
            return nullptr;
        }
        return static_cast<void *>(reader); // 成功したらハンドルを返す
    }

    /**
     * @brief VTUリーダーのハンドルを受け取り、対応するオブジェクトを解放する
     */
    void c_vtu_finalize(void *handle)
    {
        if (!handle)
            return;
        VtuReader *reader = static_cast<VtuReader *>(handle);
        delete reader;
    }

    /**
     * @brief ヘッダー情報を取得する
     */
    void c_vtu_read_header(void *handle, char *format, int format_len, char *dataset, int dataset_len)
    {
        if (!handle)
            return;
        static_cast<VtuReader *>(handle)->getHeaderInfo(format, format_len, dataset, dataset_len);
    }

    /**
     * @brief ポイント数を取得する
     */
    void c_vtu_get_num_points(void *handle, int *num_points)
    {
        if (!handle)
        {
            *num_points = 0;
            return;
        }
        *num_points = static_cast<VtuReader *>(handle)->getNumPoints();
    }

    /**
     * @brief ポイント座標を取得する
     */
    void c_vtu_get_points(void *handle, double *x, double *y, double *z)
    {
        if (!handle)
            return;
        static_cast<VtuReader *>(handle)->getPoints(x, y, z);
    }

    /**
     * @brief セル数を取得する
     */
    void c_vtu_get_num_cells(void *handle, int *num_cells)
    {
        if (!handle)
        {
            *num_cells = 0;
            return;
        }
        *num_cells = static_cast<VtuReader *>(handle)->getNumCells();
    }

    /**
     * @brief 全コネクティビティ配列のサイズを取得する
     */
    void c_vtu_get_total_connectivity_size(void *handle, long long *size)
    {
        if (!handle)
        {
            *size = 0;
            return;
        }
        *size = static_cast<VtuReader *>(handle)->getTotalConnectivitySize();
    }

    /**
     * @brief セル情報（コネクティビティ、オフセット、タイプ）を取得する
     */
    void c_vtu_get_cell_info(void *handle, long long *connectivity, long long *offsets, int *types)
    {
        if (!handle)
            return;
        static_cast<VtuReader *>(handle)->getCellInfo(connectivity, offsets, types);
    }

    // --- ここから追加 ---
    /**
     * @brief 名前で指定されたセルデータ配列の成分数を取得する
     */
    void c_vtu_get_num_cell_data_components(void *handle, const char *array_name, int *num_components)
    {
        if (!handle)
        {
            *num_components = 0;
            return;
        }
        *num_components = static_cast<VtuReader *>(handle)->getNumberOfCellDataComponents(array_name);
    }

    /**
     * @brief 名前で指定されたポイントデータ配列の成分数を取得する
     */
    void c_vtu_get_num_point_data_components(void *handle, const char *array_name, int *num_components)
    {
        if (!handle)
        {
            *num_components = 0;
            return;
        }
        *num_components = static_cast<VtuReader *>(handle)->getNumberOfPointDataComponents(array_name);
    }
    // --- ここまで追加 ---

    /**
     * @brief 名前で指定されたセルデータ配列を取得する
     */
    void c_vtu_get_cell_data_int32(void *handle, const char *array_name, int *ids)
    {
        if (!handle)
            return;
        static_cast<VtuReader *>(handle)->getCellDataInt32(array_name, ids);
    }

    void c_vtu_get_cell_data_float64(void *handle, const char *array_name, double *data)
    {
        if (!handle)
            return;
        static_cast<VtuReader *>(handle)->getCellDataFloat64(array_name, data);
    }

    /**
     * @brief 名前で指定されたポイントデータ配列を取得する
     */
    void c_vtu_get_point_data_int32(void *handle, const char *array_name, int *data)
    {
        if (!handle)
            return;
        static_cast<VtuReader *>(handle)->getPointDataInt32(array_name, data);
    }

    void c_vtu_get_point_data_float64(void *handle, const char *array_name, double *data)
    {
        if (!handle)
            return;
        static_cast<VtuReader *>(handle)->getPointDataFloat64(array_name, data);
    }

} // extern "C"