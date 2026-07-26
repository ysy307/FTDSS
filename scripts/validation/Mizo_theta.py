"""Validate FTCMS Mizoguchi water-content profiles and solver robustness."""

import argparse
import json
from pathlib import Path

import numpy as np
import pyvista as pv


EXPERIMENTAL_DATA = {
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
TIME_INDEX = {0: 0, 12: 1, 24: 2, 50: 3}
ICE_TO_WATER_VOLUME_RATIO = 917.0 / 1000.0


def _vtu_path(vtu_dir, time_hours, output_interval_minutes):
    file_number = round(time_hours * 60.0 / output_interval_minutes)
    vtu_dir = Path(vtu_dir)
    matches = []
    for path in vtu_dir.glob("Out_*.vtu"):
        try:
            suffix = int(path.stem.removeprefix("Out_"))
        except ValueError:
            continue
        if suffix == file_number:
            matches.append(path)
    if len(matches) != 1:
        raise FileNotFoundError(
            f"Expected one VTU output for {time_hours} h (index {file_number}) "
            f"in {vtu_dir}, found {len(matches)}"
        )
    return matches[0]


def _sample_total_water(path, resolution):
    grid = pv.read(path)
    if "WaterContent" in grid.point_data:
        water = np.asarray(grid.point_data["WaterContent"])
        ice = np.asarray(grid.point_data["IceContent"])
        grid.point_data["theta_tot"] = water + ICE_TO_WATER_VOLUME_RATIO * ice
    elif "WaterContent" in grid.cell_data:
        water = np.asarray(grid.cell_data["WaterContent"])
        ice = np.asarray(grid.cell_data["IceContent"])
        grid.cell_data["theta_tot"] = water + ICE_TO_WATER_VOLUME_RATIO * ice
    else:
        raise ValueError(f"WaterContent not found in {path}")

    x_min, x_max, y_min, y_max, z_min, z_max = grid.bounds
    nx = max(2, int(np.ceil((x_max - x_min) / resolution)) + 1)
    nz = max(2, int(np.ceil((z_max - z_min) / resolution)) + 1)
    dy = 1.0 if y_min == y_max else y_max - y_min
    image = pv.ImageData(
        dimensions=(nx, 1, nz),
        spacing=(resolution, dy, resolution),
        origin=(x_min, y_min, z_min),
    )
    sampled = image.sample(grid)
    theta = np.asarray(sampled.point_data["theta_tot"])
    valid = np.isfinite(theta)
    if "vtkValidPointMask" in sampled.point_data:
        valid &= np.asarray(sampled.point_data["vtkValidPointMask"]).astype(bool)
    points = sampled.points[valid]
    if points.size == 0:
        raise ValueError(f"No valid sample points found in {path}")
    return z_max - points[:, 2], theta[valid]


def _depth_profile(depths, theta, time_hours, dz):
    experiment_index = TIME_INDEX[time_hours]
    rows = []
    for depth_end, values in EXPERIMENTAL_DATA.items():
        depth_start = depth_end - dz
        mask = (depths >= depth_start) & (depths < depth_end)
        simulated = float(np.mean(theta[mask])) if np.any(mask) else None
        experimental = values[experiment_index]
        rows.append(
            {
                "depth_start_m": depth_start,
                "depth_end_m": depth_end,
                "simulated": simulated,
                "experimental": experimental,
                "error": None if simulated is None else simulated - experimental,
            }
        )
    return rows


def _print_profile(rows):
    print(f"{'Depth (m)':<20} | {'Simulated':<12} | {'Exp':<12} | {'Error':<12}")
    print("-" * 65)
    for row in rows:
        interval = f"{row['depth_start_m']:4.2f} - {row['depth_end_m']:4.2f}"
        if row["simulated"] is None:
            print(f"{interval:<20} | {'No Data':<12} | {row['experimental']:12.6f} | {'-':<12}")
        else:
            print(
                f"{interval:<20} | {row['simulated']:12.6f} | "
                f"{row['experimental']:12.6f} | {row['error']:+12.6f}"
            )


def _profile_metrics(rows):
    errors = np.asarray([row["error"] for row in rows if row["error"] is not None])
    if errors.size == 0:
        raise ValueError("No depth bins contain simulation data")
    return {
        "rmse": float(np.sqrt(np.mean(errors**2))),
        "mae": float(np.mean(np.abs(errors))),
        "bias": float(np.mean(errors)),
        "max_abs_error": float(np.max(np.abs(errors))),
    }


def _redistribution_metrics(initial_rows, target_rows, dz):
    simulated_changes = []
    experimental_changes = []
    for initial, target in zip(initial_rows, target_rows):
        if initial["simulated"] is None or target["simulated"] is None:
            simulated_changes.append(np.nan)
        else:
            simulated_changes.append(target["simulated"] - initial["simulated"])
        experimental_changes.append(target["experimental"] - initial["experimental"])
    simulated_changes = np.asarray(simulated_changes)
    experimental_changes = np.asarray(experimental_changes)
    depth_ends = np.asarray(list(EXPERIMENTAL_DATA))
    upper = depth_ends <= 0.05
    lower = (depth_ends > 0.05) & (depth_ends <= 0.12)
    valid = np.isfinite(simulated_changes)
    simulated_upper = float(np.sum(simulated_changes[upper & valid]) * dz)
    experimental_upper = float(np.sum(experimental_changes[upper]) * dz)
    return {
        "upper_0_005m_gain_m": simulated_upper,
        "lower_005_012m_change_m": float(np.sum(simulated_changes[lower & valid]) * dz),
        "column_0_020m_change_m": float(np.sum(simulated_changes[valid]) * dz),
        "experimental_upper_0_005m_gain_m": experimental_upper,
        "experimental_lower_005_012m_change_m": float(
            np.sum(experimental_changes[lower]) * dz
        ),
        "experimental_column_0_020m_change_m": float(np.sum(experimental_changes) * dz),
        "upper_gain_fraction_of_experiment": (
            simulated_upper / experimental_upper if experimental_upper != 0.0 else None
        ),
    }


def _read_solver_history(path):
    names = None
    records = []
    with Path(path).open(encoding="utf-8") as stream:
        for line in stream:
            stripped = line.strip()
            if stripped.startswith("# attempt "):
                names = stripped[2:].split()
            elif stripped and not stripped.startswith("#"):
                if names is None:
                    raise ValueError(f"Missing solver-history schema in {path}")
                values = stripped.split()
                if len(values) != len(names):
                    raise ValueError(f"Malformed solver-history row in {path}: {stripped}")
                records.append(dict(zip(names, values)))
    if not records:
        raise ValueError(f"No solver attempts found in {path}")
    return records


def _solver_metrics(path):
    records = _read_solver_history(path)
    accepted = [record for record in records if int(record["accepted"]) == 1]
    rejected = [record for record in records if int(record["accepted"]) == 0]
    nonlinear_rejected = [record for record in rejected if record["status"] != "lte_rejected"]
    lte_rejected = [record for record in rejected if record["status"] == "lte_rejected"]
    accepted_lte = [
        float(record["lte_rel"])
        for record in accepted
        if float(record["lte_rel"]) >= 0.0
    ]
    aa_uses = [int(record.get("aa_uses", 0)) for record in records]
    aa_gamma = [float(record.get("aa_gamma_max", 0.0)) for record in records]
    return {
        "attempts": len(records),
        "accepted_steps": len(accepted),
        "rejected_steps": len(rejected),
        "nonlinear_rejections": len(nonlinear_rejected),
        "lte_rejections": len(lte_rejected),
        "nonlinear_rejection_fraction": len(nonlinear_rejected) / len(records),
        "final_accepted_time_s": max(float(record["time_accepted_s"]) for record in accepted),
        "max_accepted_lte": max(accepted_lte, default=-1.0),
        "aa_uses": sum(aa_uses),
        "attempts_using_aa": sum(count > 0 for count in aa_uses),
        "max_abs_aa_gamma": max(aa_gamma, default=0.0),
    }


def calculate_theta_error_high_precision(
    vtu_base_dir,
    target_time_hours,
    dz=0.01,
    res=0.001,
    output_interval_minutes=5.0,
):
    if target_time_hours not in TIME_INDEX:
        raise ValueError(f"Invalid time: {target_time_hours} h")
    target_path = _vtu_path(vtu_base_dir, target_time_hours, output_interval_minutes)
    depths, theta = _sample_total_water(target_path, res)
    rows = _depth_profile(depths, theta, target_time_hours, dz)

    print(f"Target Time: {target_time_hours} h (sampling resolution={res} m)")
    _print_profile(rows)
    metrics = _profile_metrics(rows)
    print(
        "Profile metrics: "
        f"RMSE={metrics['rmse']:.6f}, MAE={metrics['mae']:.6f}, "
        f"bias={metrics['bias']:+.6f}, max|error|={metrics['max_abs_error']:.6f}"
    )

    result = {"time_hours": target_time_hours, "profile": rows, "profile_metrics": metrics}
    if target_time_hours > 0:
        initial_path = _vtu_path(vtu_base_dir, 0, output_interval_minutes)
        initial_depths, initial_theta = _sample_total_water(initial_path, res)
        initial_rows = _depth_profile(initial_depths, initial_theta, 0, dz)
        redistribution = _redistribution_metrics(initial_rows, rows, dz)
        result["redistribution"] = redistribution
        print(
            "Redistribution: "
            f"upper gain={redistribution['upper_0_005m_gain_m']:+.6e} m, "
            f"lower change={redistribution['lower_005_012m_change_m']:+.6e} m, "
            f"column change={redistribution['column_0_020m_change_m']:+.6e} m"
        )
        print(
            "Experimental redistribution: "
            f"upper gain={redistribution['experimental_upper_0_005m_gain_m']:+.6e} m, "
            f"lower change={redistribution['experimental_lower_005_012m_change_m']:+.6e} m, "
            "simulated/experimental upper gain="
            f"{redistribution['upper_gain_fraction_of_experiment']:.3f}"
        )
    return result


def main():
    parser = argparse.ArgumentParser()
    parser.add_argument("--vtu_dir", type=Path, required=True)
    parser.add_argument("--time", type=int, choices=sorted(TIME_INDEX), required=True)
    parser.add_argument("--res", type=float, default=0.001)
    parser.add_argument("--output_interval_minutes", type=float, default=5.0)
    parser.add_argument("--solver_history", type=Path)
    parser.add_argument("--check", action="store_true")
    parser.add_argument("--rmse_limit", type=float)
    parser.add_argument("--min_upper_gain_fraction", type=float)
    parser.add_argument("--max_abs_column_change_m", type=float)
    parser.add_argument("--max_nonlinear_rejection_fraction", type=float, default=0.2)
    parser.add_argument("--require_aa", action="store_true")
    parser.add_argument("--json_output", type=Path)
    args = parser.parse_args()

    result = calculate_theta_error_high_precision(
        args.vtu_dir,
        args.time,
        res=args.res,
        output_interval_minutes=args.output_interval_minutes,
    )

    history_path = args.solver_history
    if history_path is None:
        candidate = args.vtu_dir.parent / "solver_history.log"
        if candidate.is_file():
            history_path = candidate
    if history_path is not None:
        solver = _solver_metrics(history_path)
        result["solver"] = solver
        print(
            "Solver metrics: "
            f"attempts={solver['attempts']}, accepted={solver['accepted_steps']}, "
            f"nonlinear rejects={solver['nonlinear_rejections']}, "
            f"LTE rejects={solver['lte_rejections']}, "
            f"nonlinear reject fraction={solver['nonlinear_rejection_fraction']:.3f}, "
            f"final accepted time={solver['final_accepted_time_s'] / 3600.0:.3f} h, "
            f"AA uses={solver['aa_uses']}, max|AA gamma|={solver['max_abs_aa_gamma']:.3f}"
        )

    failures = []
    if args.check:
        if args.rmse_limit is not None and result["profile_metrics"]["rmse"] > args.rmse_limit:
            failures.append(
                f"RMSE {result['profile_metrics']['rmse']:.6f} exceeds {args.rmse_limit:.6f}"
            )
        if args.time > 0 and result["redistribution"]["upper_0_005m_gain_m"] <= 0.0:
            failures.append("no upward water gain was detected in the upper 0.05 m")
        if args.time > 0 and args.min_upper_gain_fraction is not None:
            gain_fraction = result["redistribution"]["upper_gain_fraction_of_experiment"]
            if gain_fraction < args.min_upper_gain_fraction:
                failures.append(
                    "upper water gain is only "
                    f"{gain_fraction:.3f} of the experimental gain, below "
                    f"{args.min_upper_gain_fraction:.3f}"
                )
        if args.time > 0 and args.max_abs_column_change_m is not None:
            column_change = abs(result["redistribution"]["column_0_020m_change_m"])
            if column_change > args.max_abs_column_change_m:
                failures.append(
                    f"absolute column-water change {column_change:.6e} m exceeds "
                    f"{args.max_abs_column_change_m:.6e} m"
                )
        if "solver" not in result:
            failures.append("solver history is required for --check")
        else:
            solver = result["solver"]
            if solver["final_accepted_time_s"] + 1.0e-8 < args.time * 3600.0:
                failures.append("simulation did not reach the requested validation time")
            if solver["nonlinear_rejection_fraction"] > args.max_nonlinear_rejection_fraction:
                failures.append(
                    "nonlinear rejection fraction "
                    f"{solver['nonlinear_rejection_fraction']:.3f} exceeds "
                    f"{args.max_nonlinear_rejection_fraction:.3f}"
                )
            if solver["max_accepted_lte"] > 1.0 + 1.0e-12:
                failures.append(f"accepted step has normalized LTE {solver['max_accepted_lte']:.6f} > 1")
            if args.require_aa and solver["aa_uses"] == 0:
                failures.append("AA(1) was required but was not used by any nonlinear attempt")

    result["check_failures"] = failures
    if args.json_output is not None:
        args.json_output.write_text(json.dumps(result, indent=2) + "\n", encoding="utf-8")
    if failures:
        for failure in failures:
            print(f"CHECK FAILED: {failure}")
        raise SystemExit(1)
    if args.check:
        print("All requested V&V checks passed.")


if __name__ == "__main__":
    main()
