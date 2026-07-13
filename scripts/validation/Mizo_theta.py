import argparse
import numpy as np
import pyvista as pv

def calculate_theta_error_high_precision(vtu_base_dir, target_time_hours, dz=0.01, res=0.001):
    # ------------------------------------------------------------
    # Experimental data definition
    # ------------------------------------------------------------
    experimental_data = {
        0.01: [0.345, 0.420, 0.375, 0.410],
        0.02: [0.345, 0.420, 0.400, 0.410],
        0.03: [0.345, 0.400, 0.395, 0.400],
        0.04: [0.345, 0.390, 0.395, 0.395],
        0.05: [0.345, 0.345, 0.405, 0.395],
        0.06: [0.345, 0.270, 0.400, 0.390],
        0.07: [0.345, 0.250, 0.345, 0.390],
        0.08: [0.345, 0.300, 0.260, 0.390],
        0.09: [0.340, 0.310, 0.280, 0.385],
        0.10: [0.345, 0.320, 0.290, 0.375],
        0.11: [0.345, 0.325, 0.300, 0.350],
        0.12: [0.345, 0.330, 0.305, 0.310],
        0.13: [0.350, 0.330, 0.310, 0.290],
        0.14: [0.345, 0.335, 0.315, 0.270],
        0.15: [0.345, 0.340, 0.320, 0.260],
        0.16: [0.355, 0.340, 0.320, 0.270],
        0.17: [0.350, 0.340, 0.325, 0.275],
        0.18: [0.350, 0.340, 0.325, 0.275],
        0.19: [0.350, 0.340, 0.325, 0.280],
        0.20: [0.350, 0.340, 0.330, 0.280],
    }

    time_idx_map = {0: 0, 12: 1, 24: 2, 50: 3}
    if target_time_hours not in time_idx_map:
        raise ValueError(f"Invalid time: {target_time_hours}h.")
    
    idx = time_idx_map[target_time_hours]

    # ------------------------------------------------------------
    # Load VTU
    # ------------------------------------------------------------
    file_num = int((target_time_hours * 60) / 5)
    input_file = f"{vtu_base_dir}/Out_{file_num:03d}.vtu"
    
    grid = pv.read(input_file)
    ratio = 917.0 / 1000.0
    
    # Calculate theta_tot strictly on the original mesh
    if "WaterContent" in grid.point_data:
        water = grid.point_data["WaterContent"]
        ice = grid.point_data["IceContent"]
        grid.point_data["theta_tot"] = water + ice * ratio
    elif "WaterContent" in grid.cell_data:
        water = grid.cell_data["WaterContent"]
        ice = grid.cell_data["IceContent"]
        grid.cell_data["theta_tot"] = water + ice * ratio
    else:
        raise ValueError("WaterContent not found in the VTU file.")

    # ------------------------------------------------------------
    # High-resolution uniform sampling
    # ------------------------------------------------------------
    bounds = grid.bounds
    x_min, x_max, y_min, y_max, z_min, z_max = bounds
    
    nx = max(2, int(np.ceil((x_max - x_min) / res)) + 1)
    nz = max(2, int(np.ceil((z_max - z_min) / res)) + 1)
    dy = 1.0 if y_min == y_max else (y_max - y_min)
    
    fine_grid = pv.ImageData(
        dimensions=(nx, 1, nz),
        spacing=(res, dy, res),
        origin=(x_min, y_min, z_min)
    )
    
    sampled = fine_grid.sample(grid)
    
    pts = sampled.points
    theta_tot_vals = sampled.point_data["theta_tot"]
    
    # Filter valid points inside the mesh
    valid = ~np.isnan(theta_tot_vals)
    pts = pts[valid]
    theta_tot_vals = theta_tot_vals[valid]
    
    depths = z_max - pts[:, 2]

    # ------------------------------------------------------------
    # Evaluate errors
    # ------------------------------------------------------------
    print(f"Target Time: {target_time_hours}h (High Precision Integration, res={res}m)")
    print(f"{'Depth (m)':<20} | {'Simulated':<12} | {'Exp':<12} | {'Error':<12}")
    print("-" * 65)

    for depth_end, values in experimental_data.items():
        d_start = depth_end - dz
        d_end = depth_end
        
        mask = (depths >= d_start) & (depths < d_end)
        exp_val = values[idx]
        
        if np.any(mask):
            sim_val = np.mean(theta_tot_vals[mask])
            error = sim_val - exp_val
            print(f"{d_start:4.2f} - {d_end:4.2f}          | {sim_val:12.6f} | {exp_val:12.6f} | {error:+12.6f}")
        else:
            print(f"{d_start:4.2f} - {d_end:4.2f}          | {'No Data':<12} | {exp_val:12.6f} | {'-':<12}")

if __name__ == "__main__":
    parser = argparse.ArgumentParser()
    parser.add_argument("--vtu_dir", type=str, required=True)
    parser.add_argument("--time", type=int, choices=[0, 12, 24, 50], required=True)
    parser.add_argument("--res", type=float, default=0.001, help="Sampling resolution in meters (default: 0.001)")
    
    args = parser.parse_args()
    calculate_theta_error_high_precision(args.vtu_dir, args.time, res=args.res)