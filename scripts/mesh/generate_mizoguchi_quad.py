#!/usr/bin/env python3
"""Generate a mirror-symmetric 2D Mizoguchi validation mesh."""

from __future__ import annotations

import argparse
from pathlib import Path


def node_id(ix: int, iz: int, nx: int) -> int:
    return iz * (nx + 1) + ix


def build_cells(nx: int, nz: int, element: str) -> tuple[list[list[int]], list[int], list[int]]:
    cells: list[list[int]] = []
    cell_types: list[int] = []
    entity_ids: list[int] = []

    for iz in range(nz):
        for ix in range(nx):
            bottom_left = node_id(ix, iz, nx)
            bottom_right = node_id(ix + 1, iz, nx)
            top_right = node_id(ix + 1, iz + 1, nx)
            top_left = node_id(ix, iz + 1, nx)
            if element == "quad":
                cells.append([bottom_left, bottom_right, top_right, top_left])
                cell_types.append(9)  # VTK_QUAD
                entity_ids.append(1)
            else:
                rising_diagonal = (ix < nx / 2) != (iz % 2 == 1)
                if rising_diagonal:
                    cells.extend(
                        ([bottom_left, bottom_right, top_right], [bottom_left, top_right, top_left])
                    )
                else:
                    cells.extend(
                        ([bottom_left, bottom_right, top_left], [bottom_right, top_right, top_left])
                    )
                cell_types.extend((5, 5))  # VTK_TRIANGLE
                entity_ids.extend((1, 1))

    for iz in range(nz):
        cells.append([node_id(0, iz, nx), node_id(0, iz + 1, nx)])
        cell_types.append(3)  # VTK_LINE
        entity_ids.append(2)
    for ix in range(nx):
        cells.append([node_id(ix, 0, nx), node_id(ix + 1, 0, nx)])
        cell_types.append(3)
        entity_ids.append(3)
    for iz in range(nz):
        cells.append([node_id(nx, iz, nx), node_id(nx, iz + 1, nx)])
        cell_types.append(3)
        entity_ids.append(4)
    for ix in range(nx):
        cells.append([node_id(ix, nz, nx), node_id(ix + 1, nz, nx)])
        cell_types.append(3)
        entity_ids.append(5)

    return cells, cell_types, entity_ids


def color_cells(cells: list[list[int]], num_nodes: int) -> list[int]:
    """Greedily color cells so cells of one color share no node."""
    colors_at_node: list[set[int]] = [set() for _ in range(num_nodes)]
    colors: list[int] = []
    for cell in cells:
        unavailable: set[int] = set()
        for node in cell:
            unavailable.update(colors_at_node[node])
        color = 1
        while color in unavailable:
            color += 1
        colors.append(color)
        for node in cell:
            colors_at_node[node].add(color)
    return colors


def write_scalar(output, name: str, values: list[int]) -> None:
    output.write(f"SCALARS {name} int 1\n")
    output.write("LOOKUP_TABLE default\n")
    for start in range(0, len(values), 16):
        output.write(" ".join(str(value) for value in values[start : start + 16]))
        output.write("\n")


def write_xml_array(
    output, data_type: str, name: str, values: list[int] | list[float], components: int = 1
) -> None:
    component_attribute = "" if components == 1 else f' NumberOfComponents="{components}"'
    output.write(
        f'        <DataArray type="{data_type}" Name="{name}"{component_attribute} format="ascii">\n'
    )
    for start in range(0, len(values), 16):
        output.write("          ")
        output.write(" ".join(f"{value:.17g}" if isinstance(value, float) else str(value) for value in values[start : start + 16]))
        output.write("\n")
    output.write("        </DataArray>\n")


def generate_vtu(
    path: Path, nx: int, nz: int, width: float, height: float, element: str
) -> None:
    num_nodes = (nx + 1) * (nz + 1)
    cells, cell_types, entity_ids = build_cells(nx, nz, element)
    colors = color_cells(cells, num_nodes)
    points: list[float] = []
    for iz in range(nz + 1):
        z = height * iz / nz
        for ix in range(nx + 1):
            points.extend((width * ix / nx, 0.0, z))
    connectivity = [node for cell in cells for node in cell]
    offsets: list[int] = []
    offset = 0
    for cell in cells:
        offset += len(cell)
        offsets.append(offset)

    path.parent.mkdir(parents=True, exist_ok=True)
    with path.open("w", encoding="ascii") as output:
        output.write('<?xml version="1.0"?>\n')
        output.write('<VTKFile type="UnstructuredGrid" version="0.1" byte_order="LittleEndian">\n')
        output.write("  <UnstructuredGrid>\n")
        output.write(f'    <Piece NumberOfPoints="{num_nodes}" NumberOfCells="{len(cells)}">\n')
        output.write("      <PointData>\n")
        write_xml_array(output, "Int32", "global_node_id", list(range(num_nodes)))
        write_xml_array(output, "Int32", "node_type", [1] * num_nodes)
        write_xml_array(output, "Int32", "num_sharing_ranks", [1] * num_nodes)
        write_xml_array(output, "Int32", "owner_ranks", [1] * num_nodes)
        write_xml_array(output, "Int32", "communication_partners", [-1] * num_nodes)
        output.write("      </PointData>\n")
        output.write("      <CellData>\n")
        write_xml_array(output, "Int32", "CellEntityIds", entity_ids)
        write_xml_array(output, "Int32", "rank", [0] * len(cells))
        write_xml_array(output, "Int32", "color", colors)
        output.write("      </CellData>\n")
        output.write("      <Points>\n")
        write_xml_array(output, "Float64", "Points", points, components=3)
        output.write("      </Points>\n")
        output.write("      <Cells>\n")
        write_xml_array(output, "Int32", "connectivity", connectivity)
        write_xml_array(output, "Int32", "offsets", offsets)
        write_xml_array(output, "UInt8", "types", cell_types)
        output.write("      </Cells>\n")
        output.write("    </Piece>\n")
        output.write("  </UnstructuredGrid>\n")
        output.write("</VTKFile>\n")


def generate(
    path: Path, nx: int, nz: int, width: float, height: float, element: str
) -> None:
    if path.suffix.lower() == ".vtu":
        generate_vtu(path, nx, nz, width, height, element)
        return
    num_nodes = (nx + 1) * (nz + 1)
    cells, cell_types, entity_ids = build_cells(nx, nz, element)
    colors = color_cells(cells, num_nodes)
    total_cell_entries = sum(len(cell) + 1 for cell in cells)

    path.parent.mkdir(parents=True, exist_ok=True)
    with path.open("w", encoding="ascii") as output:
        output.write("# vtk DataFile Version 3.0\n")
        output.write("Mirror-symmetric Mizoguchi x-z quadrilateral mesh\n")
        output.write("ASCII\n")
        output.write("DATASET UNSTRUCTURED_GRID\n")
        output.write(f"POINTS {num_nodes} double\n")
        for iz in range(nz + 1):
            z = height * iz / nz
            for ix in range(nx + 1):
                x = width * ix / nx
                output.write(f"{x:.17g} 0 {z:.17g}\n")

        output.write(f"CELLS {len(cells)} {total_cell_entries}\n")
        for cell in cells:
            output.write(f"{len(cell)} {' '.join(str(node) for node in cell)}\n")
        output.write(f"CELL_TYPES {len(cells)}\n")
        for start in range(0, len(cell_types), 16):
            output.write(" ".join(str(value) for value in cell_types[start : start + 16]))
            output.write("\n")

        output.write(f"POINT_DATA {num_nodes}\n")
        write_scalar(output, "global_node_id", list(range(num_nodes)))
        write_scalar(output, "node_type", [1] * num_nodes)
        write_scalar(output, "num_sharing_ranks", [1] * num_nodes)
        write_scalar(output, "owner_ranks", [1] * num_nodes)
        write_scalar(output, "communication_partners", [-1] * num_nodes)

        output.write(f"CELL_DATA {len(cells)}\n")
        write_scalar(output, "CellEntityIds", entity_ids)
        write_scalar(output, "rank", [0] * len(cells))
        write_scalar(output, "color", colors)


def main() -> None:
    parser = argparse.ArgumentParser()
    parser.add_argument("output", type=Path)
    parser.add_argument("--nx", type=int, default=4)
    parser.add_argument("--nz", type=int, default=800)
    parser.add_argument("--width", type=float, default=0.004)
    parser.add_argument("--height", type=float, default=0.2)
    parser.add_argument("--element", choices=("triangle", "quad"), default="quad")
    args = parser.parse_args()
    if args.nx < 1 or args.nz < 1:
        parser.error("nx and nz must be positive")
    generate(args.output, args.nx, args.nz, args.width, args.height, args.element)


if __name__ == "__main__":
    main()
