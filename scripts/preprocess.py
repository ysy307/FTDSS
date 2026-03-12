# -*- coding: utf-8 -*-
"""
並列有限要素法のためのメッシュ前処理ツール．

このスクリプトは，単一のメッシュファイルを読み込み，
METISライブラリを用いてドメインをN個のパーティションに分割します．
反復法ソルバーでの使用を想定し，通信パターンとキャッシュ効率を最適化する
ためのノード順序付けを行い，ParaViewで可視化可能なVTK形式（.vtu, .pvtu）
で出力します．

主な機能:
- METISによるグラフ分割（セルの隣接関係に基づく）
- 元のメッシュのPoint/Cell Dataの維持
- 反復法ソルバーに最適化されたノード順序付け (RCM, 内部/境界)
- マルチフィジックス対応の境界条件の事前グループ化
- コマンドラインによる柔軟な設定
"""

import argparse
import json
from pathlib import Path
from typing import Dict, List, Optional, Set, Tuple

import meshio
import networkx as nx
import numpy as np
import pymetis
from pymetis import CSRAdjacency
from scipy.sparse import csc_matrix
from scipy.sparse.csgraph import reverse_cuthill_mckee

# 型ヒントの定義
NodeMap = Dict[int, int]


class ParallelMeshProcessor:
    """メッシュを並列計算用に処理し，VTKファイルとして出力するクラス．"""

    def __init__(
        self,
        input_file: str,
        n_parts: int,
        control_file: str,
        coloring: bool = True,
        reordering_strategy: str = 'RCM',
    ) -> None:
        self.input_file_path = Path(input_file).resolve()
        if not self.input_file_path.is_file():
            raise FileNotFoundError(f"入力ファイルが見つかりません: '{self.input_file_path}'")

        self.control_file_path = Path(control_file).resolve()
        if not self.control_file_path.is_file():
            raise FileNotFoundError(f"制御ファイルが見つかりません: '{self.control_file_path}'")

        self.base_dir = self.input_file_path.parent
        self.base_name = self.input_file_path.stem
        self.output_dir = self.base_dir

        if n_parts <= 0:
            raise ValueError('パーティション数は正の整数である必要があります．')
        self.n_parts = n_parts
        self.perform_coloring = coloring

        if reordering_strategy.upper() not in ['RCM', 'ND']:
            raise ValueError("並べ替え戦略は 'RCM' または 'ND' である必要があります．")
        self.reordering_strategy = reordering_strategy.upper()
        print(f'✨ 並べ替え戦略: {self.reordering_strategy} を使用します．')

        # クラス変数の初期化
        self.mesh: Optional[meshio.Mesh] = None
        self.colors: Optional[np.ndarray] = None
        self.num_total_cells: int = 0
        self.num_points: int = 0
        self.part_assignments: Optional[np.ndarray] = None
        self.node_to_ranks: Optional[List[Set[int]]] = None
        self.final_global_id_map: Optional[np.ndarray] = None
        self.node_owner_rank: Optional[np.ndarray] = None
        self.bc_group_map: Dict[int, Dict[str, int]] = {}
        self.bc_manifest: Dict[str, Dict] = {}

    def run(self) -> None:
        """処理のメインフローを実行する．"""
        self._setup_directory()
        self._load_mesh()
        self._process_boundary_conditions()
        self._partition_graph()
        self._map_nodes_to_ranks()
        self._generate_global_node_numbering()
        self._write_output_files()
        self._update_basic_json()
        self._print_summary()

    def _setup_directory(self) -> None:
        self.output_dir.mkdir(exist_ok=True)

    def _load_mesh(self) -> None:
        print(f'1. メッシュファイルを読み込み中: {self.input_file_path}')
        self.mesh = meshio.read(self.input_file_path)
        self.num_total_cells = sum(len(b.data) for b in self.mesh.cells)
        self.num_points = self.mesh.points.shape[0]
        print(f'   -> {self.num_total_cells}個のセルと{self.num_points}個の点を発見しました．')

    def _process_boundary_conditions(self) -> None:
        """制御ファイルを読み込み，BCのシグネチャに基づいてグループ化する．"""
        print('2. 境界条件をグループ化中...')
        with open(self.control_file_path, 'r', encoding='utf-8') as f:
            # "conditions"キーの下にあることを想定
            controls = json.load(f).get("boundary_conditions", [])

        unique_signatures = {}
        next_group_id = 0

        for bc_def in controls:
            entity_id = bc_def['id']
            if entity_id not in self.bc_group_map:
                self.bc_group_map[entity_id] = {}

            # 物理ごとにループし、シグネチャを作成
            for phys_key in ['thermal', 'hydraulic', 'mechanical']:
                if phys_key in bc_def and bc_def.get(f"calculate_{phys_key}", False):
                    phys_data = bc_def[phys_key]
                    signature = (phys_key, phys_data['type'])
                    
                    if signature not in unique_signatures:
                        unique_signatures[signature] = next_group_id
                        self.bc_manifest[str(next_group_id)] = {
                            'physics_type': phys_key,
                            'bc_type_name': phys_data['type'],
                        }
                        next_group_id += 1
                    
                    group_id = unique_signatures[signature]
                    self.bc_group_map[entity_id][phys_key] = group_id
        
        print(f'   -> {len(self.bc_manifest)}個のユニークなBCグループを発見しました．')
        # マニフェスト情報をJSONファイルとして出力（デバッグ用に便利）
        manifest_path = self.output_dir / 'bc_manifest.json'
        with open(manifest_path, 'w', encoding='utf-8') as f:
            json.dump(self.bc_manifest, f, indent=4)


    def _partition_graph(self) -> None:
        print('3. ドメインを分割中...')
        # (この部分は変更なし)
        rows, cols, cell_idx = [], [], 0
        for block in self.mesh.cells:
            for conn in block.data:
                for node in conn[conn != -1]:
                    rows.append(node)
                    cols.append(cell_idx)
                cell_idx += 1
        node_cell = csc_matrix(
            (np.ones(len(rows), dtype=np.int8), (rows, cols)),
            shape=(self.num_points, self.num_total_cells),
        )
        cell_cell = (node_cell.transpose() @ node_cell).tocsr()
        cell_cell.setdiag(0)
        cell_cell.eliminate_zeros()
        xadj, adjncy = cell_cell.indptr, cell_cell.indices
        _, part_list = pymetis.part_graph(
            self.n_parts, adjacency=CSRAdjacency(xadj, adjncy)
        )
        self.part_assignments = np.array(part_list)
        if self.perform_coloring:
            print('   -> セルグラフをカラーリング中...')
            graph = nx.from_scipy_sparse_array(cell_cell)
            coloring_dict = nx.coloring.greedy_color(graph, strategy='smallest_last')
            self.colors = np.array(
                [coloring_dict.get(i, -1) + 1 for i in range(self.num_total_cells)],
                dtype=np.int32,
            )
        print('   -> ドメイン分割が完了しました．')

    def _map_nodes_to_ranks(self) -> None:
        print('4. ノードとランクのマッピング中...')
        # (この部分は変更なし)
        self.node_to_ranks = [set() for _ in range(self.num_points)]
        cell_idx = 0
        for block in self.mesh.cells:
            for conn in block.data:
                rank = self.part_assignments[cell_idx]
                for node_id in conn[conn != -1]:
                    self.node_to_ranks[node_id].add(rank)
                cell_idx += 1
        print('   -> マッピングが完了しました．')

    def _generate_global_node_numbering(self) -> None:
        print('5. 最終的なノード番号を生成中...')
        self._determine_node_ownership()
        # (この部分は変更なし)
        all_ordered_gids = []
        for rank in range(self.n_parts):
            rank_cell_indices = np.where(self.part_assignments == rank)[0]
            if len(rank_cell_indices) == 0:
                continue
            unique_gids, map_gid_to_lid = self._get_nodes_for_rank(rank_cell_indices)
            owned_gids = [
                gid for gid in unique_gids if self.node_owner_rank[gid] == rank
            ]
            ordered_gids_for_rank = self._reorder_nodes_locally(
                rank, owned_gids, unique_gids, map_gid_to_lid
            )
            all_ordered_gids.append(ordered_gids_for_rank)
        final_ordered_list = np.concatenate(all_ordered_gids)
        self.final_global_id_map = np.full(self.num_points, -1, dtype=np.int32)
        self.final_global_id_map[final_ordered_list] = np.arange(
            len(final_ordered_list)
        )
        print('   -> ノード番号の生成が完了しました．')

    def _determine_node_ownership(self) -> None:
        self.node_owner_rank = np.array(
            [min(s) if s else -1 for s in self.node_to_ranks], dtype=np.int32
        )

    def _reorder_nodes_locally(
        self, rank: int, owned_gids: List[int], unique_gids: np.ndarray, map_gid_to_lid: NodeMap
    ) -> np.ndarray:
        # (この部分は変更なし)
        if not owned_gids:
            return np.array([], dtype=np.int32)
        internal_gids = [gid for gid in owned_gids if len(self.node_to_ranks[gid]) == 1]
        boundary_gids = [gid for gid in owned_gids if len(self.node_to_ranks[gid]) > 1]
        rank_cell_indices = np.where(self.part_assignments == rank)[0]
        local_node_graph = self._build_local_node_graph(
            rank_cell_indices, unique_gids, map_gid_to_lid
        )
        inverse_perm = None
        if self.reordering_strategy == 'RCM':
            permutation = reverse_cuthill_mckee(local_node_graph, symmetric_mode=True)
            inverse_perm = np.argsort(permutation)
        elif self.reordering_strategy == 'ND':
            xadj, adjncy = local_node_graph.indptr, local_node_graph.indices
            _, perm = pymetis.node_nd(adjacency=CSRAdjacency(xadj, adjncy))
            inverse_perm = perm
        def sort_by_perm(gids: List[int]) -> np.ndarray:
            if not gids:
                return np.array([], dtype=np.int32)
            lids = [map_gid_to_lid[gid] for gid in gids]
            positions = inverse_perm[lids]
            return np.array(gids)[np.argsort(positions)]
        sorted_internal = sort_by_perm(internal_gids)
        sorted_boundary = sort_by_perm(boundary_gids)
        receives_data = any(
            self.node_owner_rank[gid] < rank
            for gid in unique_gids
            if self.node_owner_rank[gid] != rank
        )
        if receives_data:
            return np.concatenate([sorted_boundary, sorted_internal])
        else:
            return np.concatenate([sorted_internal, sorted_boundary])

    def _get_nodes_for_rank(self, rank_cell_indices: np.ndarray) -> Tuple[np.ndarray, NodeMap]:
        # (この部分は変更なし)
        nodes_in_rank = set()
        cursor = 0
        for block in self.mesh.cells:
            mask = (rank_cell_indices >= cursor) & (
                rank_cell_indices < cursor + len(block.data)
            )
            if np.any(mask):
                block_indices = rank_cell_indices[mask] - cursor
                nodes_in_rank.update(block.data[block_indices].flatten())
            cursor += len(block.data)
        unique_gids = np.array(sorted(list(nodes_in_rank - {-1})), dtype=np.int32)
        map_gid_to_lid = {gid: lid for lid, gid in enumerate(unique_gids)}
        return unique_gids, map_gid_to_lid

    def _build_local_node_graph(
        self, rank_cell_indices: np.ndarray, unique_gids: np.ndarray, map_gid_to_lid: NodeMap
    ) -> csc_matrix:
        # (この部分は変更なし)
        cells_for_graph, _ = self._extract_structured_cell_data(
            0, rank_cell_indices, map_gid_to_lid, is_for_graph_build=True
        )
        rows, cols, cell_idx = [], [], 0
        for block in cells_for_graph:
            for conn in block.data:
                for node_id in conn:
                    rows.append(node_id)
                    cols.append(cell_idx)
                cell_idx += 1
        local_node_cell = csc_matrix(
            (np.ones(len(rows), dtype=np.int8), (rows, cols)),
            shape=(len(unique_gids), len(rank_cell_indices)),
        )
        local_node_graph = (local_node_cell @ local_node_cell.transpose()).tocsr()
        local_node_graph.setdiag(0)
        local_node_graph.eliminate_zeros()
        return local_node_graph

    def _write_output_files(self) -> None:
        """全ランクのVTUファイルとマスターPVTUファイルを書き出す．"""
        print('6. 分割VTUファイルを書き出し中...')
        for rank in range(self.n_parts):
            rank_cell_indices = np.where(self.part_assignments == rank)[0]
            if len(rank_cell_indices) == 0:
                continue

            unique_gids, map_gid_to_lid = self._get_nodes_for_rank(rank_cell_indices)
            cells, cell_data = self._extract_structured_cell_data(
                rank, rank_cell_indices, map_gid_to_lid
            )
            point_data = self._create_point_data(rank, unique_gids)

            part_mesh = meshio.Mesh(
                points=self.mesh.points[unique_gids],
                cells=cells,
                point_data=point_data,
                cell_data=cell_data,
            )
            output_vtu_path = self.output_dir / f'{self.base_name}_{rank}.vtu'
            meshio.write(output_vtu_path, part_mesh, binary=True)
        
        self._write_pvtu_file() # PVTU生成を呼び出す

    def _extract_structured_cell_data(
        self, rank: int, rank_cell_indices: np.ndarray, map_gid_to_lid: NodeMap, is_for_graph_build: bool = False
    ) -> Tuple[List[meshio.CellBlock], Dict[str, List[np.ndarray]]]:
        """元のCell Dataを維持しつつ，ランクごとのセル情報を抽出する．"""
        cells, cell_data = [], {key: [] for key in self.mesh.cell_data.keys()}
        
        if not is_for_graph_build:
            cell_data['rank'] = []
            if self.perform_coloring and self.colors is not None:
                cell_data['color'] = []
            
            active_physics = sorted(list(set(v['physics_type'] for v in self.bc_manifest.values())))
            for phys in active_physics:
                cell_data[f'bc_group_id_{phys}'] = []

        cursor = 0
        for block_idx, block in enumerate(self.mesh.cells):
            mask = (rank_cell_indices >= cursor) & (
                rank_cell_indices < cursor + len(block.data)
            )
            global_indices = rank_cell_indices[mask]

            if len(global_indices) > 0:
                block_indices = global_indices - cursor
                local_conn = np.vectorize(map_gid_to_lid.get)(block.data[block_indices])
                cells.append(meshio.CellBlock(block.type, local_conn))

                # 元のCell Dataを維持する
                for key, data_list in self.mesh.cell_data.items():
                    if block_idx < len(data_list) and data_list[block_idx] is not None:
                        cell_data[key].append(data_list[block_idx][block_indices])

                if not is_for_graph_build:
                    cell_data['rank'].append(np.full(len(local_conn), rank, dtype=np.int32))
                    if self.perform_coloring and self.colors is not None:
                        cell_data['color'].append(self.colors[global_indices])
                    
                    # 物理ごとにBCグループIDを付与
                    entity_id_key = next((k for k in self.mesh.cell_data.keys() if 'physical' in k or 'medit' in k), None)
                    if entity_id_key and entity_id_key in cell_data:
                        entity_ids = cell_data[entity_id_key][-1]
                        for phys in active_physics:
                            group_ids = np.array(
                                [self.bc_group_map.get(eid, {}).get(phys, -1) for eid in entity_ids],
                                dtype=np.int32
                            )
                            cell_data[f'bc_group_id_{phys}'].append(group_ids)
                    else:
                        for phys in active_physics:
                            cell_data[f'bc_group_id_{phys}'].append(np.full(len(local_conn), -1, dtype=np.int32))
            cursor += len(block.data)
        return cells, cell_data

    def _create_point_data(
        self, rank: int, unique_gids: np.ndarray
    ) -> Dict[str, np.ndarray]:
        """元のPoint Dataを維持しつつ，並列計算用の点データを生成する．"""
        point_data = {
            key: arr[unique_gids] for key, arr in self.mesh.point_data.items()
        }
        point_data['global_node_id'] = self.final_global_id_map[unique_gids]
        num_local_points = len(unique_gids)
        point_data['node_type'] = np.ones(num_local_points, dtype=np.int32)
        
        for lid, gid in enumerate(unique_gids):
            sharing_ranks = self.node_to_ranks[gid]
            owner = self.node_owner_rank[gid]
            point_data['node_type'][lid] = (
                1 if len(sharing_ranks) == 1 else (2 if rank == owner else 3)
            )
        return point_data
        
    def _write_pvtu_file(self) -> None:
        """ParaViewで全体を可視化するためのマスターファイル(.pvtu)を書き出す．"""
        pvtu_filename = self.output_dir / f'{self.base_name}.pvtu'
        print(f'7. PVTUファイルを書き出し中: {pvtu_filename}')
        
        # 代表としてランク0のメッシュからデータ配列の情報を取得
        sample_mesh_path = self.output_dir / f'{self.base_name}_0.vtu'
        if not sample_mesh_path.exists():
            print("   -> ランク0のvtuファイルが見つからないため、PVTUを生成できません。")
            return
            
        sample_mesh = meshio.read(sample_mesh_path)

        with open(pvtu_filename, 'w', encoding='utf-8') as f:
            f.write('<?xml version="1.0"?>\n')
            f.write('<VTKFile type="PUnstructuredGrid" version="1.0" byte_order="LittleEndian" header_type="UInt64">\n')
            f.write('  <PUnstructuredGrid GhostLevel="0">\n')
            
            # PointData
            f.write('    <PPointData Scalars="global_node_id">\n')
            for key, arr in sample_mesh.point_data.items():
                dtype = str(arr.dtype)
                num_comp = arr.shape[1] if arr.ndim > 1 else 1
                f.write(f'      <PDataArray type="{dtype.capitalize()}" Name="{key}" NumberOfComponents="{num_comp}"/>\n')
            f.write('    </PPointData>\n')

            # CellData
            f.write('    <PCellData>\n')
            for key, data_list in sample_mesh.cell_data.items():
                arr = data_list[0] # 最初のブロックの配列を代表とする
                dtype = str(arr.dtype)
                num_comp = arr.shape[1] if arr.ndim > 1 else 1
                f.write(f'      <PDataArray type="{dtype.capitalize()}" Name="{key}" NumberOfComponents="{num_comp}"/>\n')
            f.write('    </PCellData>\n')

            # Points
            f.write('    <PPoints>\n')
            point_type = str(sample_mesh.points.dtype).capitalize()
            f.write(f'      <PDataArray type="{point_type}" NumberOfComponents="3" Name="Points"/>\n')
            f.write('    </PPoints>\n')

            for r in range(self.n_parts):
                if np.any(self.part_assignments == r):
                    f.write(f'    <Piece Source="{self.base_name}_{r}.vtu"/>\n')
            
            f.write('  </PUnstructuredGrid>\n')
            f.write('</VTKFile>\n')

    def _update_basic_json(self) -> None:
        """もし 'Basic.json' が存在すれば，メッシュファイル名を分割後の形式に更新する．"""
        # このメソッドは元のJSONを破壊的に変更するため、今回は使用しない。
        # 代わりに、Fortranソルバー実行時には分割後のメッシュを指す新しいJSONを使うことを推奨。
        pass

    def _print_summary(self) -> None:
        """処理完了のサマリーメッセージを表示する．"""
        pvtu_path = self.output_dir / f'{self.base_name}.pvtu'
        print(
            f"\n✅ 処理が完了しました．すべてのファイルは '{self.output_dir}/' に保存されました．"
        )
        print(
            f"   -> ParaViewで '{pvtu_path.name}' を開くと，分割されたメッシュ全体を可視化できます．"
        )
        print(
            f"   -> Fortranソルバーの実行には、元の '{self.control_file_path.name}' をそのまま使用してください．"
        )

def main() -> None:
    parser = argparse.ArgumentParser(
        description='並列計算のためのメッシュ前処理ツール．',
        formatter_class=argparse.RawTextHelpFormatter,
    )
    parser.add_argument(
        '-f', '--file_name', required=True, type=str,
        help='入力メッシュファイル (例: mesh.msh)',
    )
    parser.add_argument(
        '-n', '--n_parts', required=True, type=int,
        help='作成するパーティション数 (例: 4)',
    )
    parser.add_argument(
        '-c', '--control_file', required=True, type=str,
        help='制御ファイル（境界条件定義を含むJSON）(例: conditions.json)',
    )
    parser.add_argument(
        '--no-coloring', action='store_false', dest='coloring',
        help='セルのカラーリングを無効化する',
    )
    parser.add_argument(
        '-r', '--reordering', type=str, default='RCM', choices=['RCM', 'ND'],
        help='ノードの並べ替え戦略:\n'
        '  RCM: Reverse Cuthill-McKee (反復法ソルバー向け，デフォルト)\n'
        '  ND : Nested Dissection (直接法ソルバー向け)',
    )
    args = parser.parse_args()

    try:
        processor = ParallelMeshProcessor(
            input_file=args.file_name,
            n_parts=args.n_parts,
            control_file=args.control_file,
            coloring=args.coloring,
            reordering_strategy=args.reordering,
        )
        processor.run()
    except (FileNotFoundError, ValueError) as e:
        print(f'\n❌ エラー: {e}')
    except Exception as e:
        print(f'\n❌ 予期せぬエラーが発生しました: {e}')

if __name__ == '__main__':
    main()