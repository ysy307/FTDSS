#include "writer_vtu.h"
#include <iostream>

// ===================================================================
// このファイルでは、グローバル変数を一切使用しません。
// 全ての操作は、Fortranから渡されるハンドル（オブジェクトのポインタ）
// 経由で行われます。
// ===================================================================

extern "C"
{
    // ===================================================================
    // VTU writer (.vtu) 用のC言語インターフェース関数群
    //
    // Fortran → ISO_C_BINDING → これらの関数 → VtuWriter (C++)
    //
    // ハンドル (void*) は VtuWriter オブジェクトのポインタを保持します。
    // メモリ管理はすべてこの層で完結し、Fortran 側から直接 new/delete
    // を呼び出す必要はありません。
    // ===================================================================

    /**
     * @brief VTU ライターを生成しファイルパスに紐付ける。
     *
     * @param[in]  filename  出力 .vtu ファイルパス (null 終端 C 文字列)
     * @return     生成した VtuWriter オブジェクトへの不透明ハンドル
     */
    void *c_vtu_writer_create(const char *filename)
    {
        VtuWriter *writer = new VtuWriter();
        writer->initialize(filename);
        return static_cast<void *>(writer);
    }

    /**
     * @brief 非構造格子のトポロジーをセットする。
     *
     * Fortran 側で 1-based に変換された connectivity を受け取り、
     * C++ 側では 0-based に変換してから VTK に渡す。
     *
     * @param[in] handle        c_vtu_writer_create で取得したハンドル
     * @param[in] num_points    頂点数
     * @param[in] points        インターリーブ座標 (x,y,z)*n; 長さ = 3*num_points
     * @param[in] num_cells     セル数
     * @param[in] conn_size     connectivity 配列の総要素数
     * @param[in] connectivity  0-based フラット connectivity; 長さ = conn_size
     * @param[in] offsets       VTK XML 形式のオフセット配列; 長さ = num_cells
     * @param[in] cell_types    VTK セルタイプ ID; 長さ = num_cells
     */
    void c_vtu_writer_set_mesh(void *handle,
                               int num_points, const double *points,
                               int num_cells, int conn_size,
                               const int *connectivity,
                               const int *offsets,
                               const int *cell_types)
    {
        if (!handle)
            return;
        static_cast<VtuWriter *>(handle)->setMesh(
            num_points, points, num_cells, conn_size,
            connectivity, offsets, cell_types);
    }

    /**
     * @brief スカラー点データ配列 (float64) を追加する。
     *
     * @param[in] handle      ライターハンドル
     * @param[in] name        VTU ファイルに書き込む配列名
     * @param[in] num_points  頂点数
     * @param[in] data        スカラー値; 長さ = num_points
     */
    void c_vtu_writer_add_scalar_point_data(void *handle,
                                            const char *name,
                                            int num_points,
                                            const double *data)
    {
        if (!handle)
            return;
        static_cast<VtuWriter *>(handle)->addScalarPointData(name, num_points, data);
    }

    /**
     * @brief 3 成分ベクトル点データ配列 (float64) を追加する。
     *
     * @param[in] handle      ライターハンドル
     * @param[in] name        VTU ファイルに書き込む配列名
     * @param[in] num_points  頂点数
     * @param[in] data        インターリーブ成分 (vx,vy,vz); 長さ = 3*num_points
     */
    void c_vtu_writer_add_vector_point_data(void *handle,
                                            const char *name,
                                            int num_points,
                                            const double *data)
    {
        if (!handle)
            return;
        static_cast<VtuWriter *>(handle)->addVectorPointData(name, num_points, data);
    }

    /**
     * @brief スカラーセルデータ配列 (float64) を追加する。
     *
     * @param[in] handle     ライターハンドル
     * @param[in] name       VTU ファイルに書き込む配列名
     * @param[in] num_cells  セル数
     * @param[in] data       スカラー値; 長さ = num_cells
     */
    void c_vtu_writer_add_scalar_cell_data(void *handle,
                                           const char *name,
                                           int num_cells,
                                           const double *data)
    {
        if (!handle)
            return;
        static_cast<VtuWriter *>(handle)->addScalarCellData(name, num_cells, data);
    }

    /**
     * @brief 3 成分ベクトルセルデータ配列 (float64) を追加する。
     *
     * @param[in] handle     ライターハンドル
     * @param[in] name       VTU ファイルに書き込む配列名
     * @param[in] num_cells  セル数
     * @param[in] data       インターリーブ成分 (vx,vy,vz); 長さ = 3*num_cells
     */
    void c_vtu_writer_add_vector_cell_data(void *handle,
                                           const char *name,
                                           int num_cells,
                                           const double *data)
    {
        if (!handle)
            return;
        static_cast<VtuWriter *>(handle)->addVectorCellData(name, num_cells, data);
    }

    /**
     * @brief 蓄積されたデータをディスクに書き出す。
     *
     * write 後は点・セルデータ配列がクリアされるため、
     * 次のタイムステップでメッシュトポロジーを再設定せずに
     * フィールドだけ追加して再度 write を呼び出せます。
     *
     * @param[in] handle  ライターハンドル
     */
    void c_vtu_writer_write(void *handle)
    {
        if (!handle)
            return;
        static_cast<VtuWriter *>(handle)->write();
    }

    /**
     * @brief 非同期書き込みの完了を待機する。
     *
     * write() が非同期に起動したバックグラウンドスレッドの完了を
     * ブロックして待ちます。write() 後に直ちにファイルを読み戻す
     * 場合や、finalize() 前に明示的に同期したい場合に使用します。
     *
     * @param[in] handle  ライターハンドル
     */
    void c_vtu_writer_wait(void *handle)
    {
        if (!handle)
            return;
        static_cast<VtuWriter *>(handle)->waitForWrite();
    }

    /**
     * @brief VTK パイプラインリソースを解放してハンドルを破棄する。
     *
     * この関数呼び出し後は handle を使用してはなりません。
     *
     * @param[in] handle  ライターハンドル
     */
    void c_vtu_writer_destroy(void *handle)
    {
        if (!handle)
            return;
        VtuWriter *writer = static_cast<VtuWriter *>(handle);
        writer->finalize();
        delete writer;
    }

} // extern "C"