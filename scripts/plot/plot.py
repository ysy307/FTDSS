#!/usr/bin/env python3
import os
import re
import logging
import pandas as pd
import matplotlib
matplotlib.use('Agg')  # For Docker/headless environments
import matplotlib.pyplot as plt

# Matplotlibのフォント検索警告をミュートする
logging.getLogger('matplotlib.font_manager').setLevel(logging.ERROR)

# Retrieve project path from environment variable
project_path = os.environ.get("FTDSS_PROJECT_PATH")

if not project_path:
    print("Error: Environment variable 'FTDSS_PROJECT_PATH' is not set.")
    exit(1)

output_dir = os.path.join(project_path, "Output")

if not os.path.exists(output_dir):
    print(f"Error: Output directory not found at {output_dir}")
    exit(1)

# Configure plot styling
plt.rcParams['font.family'] = 'serif'
plt.rcParams['font.serif'] = ['Times New Roman', 'Liberation Serif', 'DejaVu Serif']
plt.rcParams['mathtext.fontset'] = 'stix'
plt.rcParams['axes.labelsize'] = 15
plt.rcParams['xtick.labelsize'] = 10
plt.rcParams['ytick.labelsize'] = 10
plt.rcParams['legend.fontsize'] = 12

# Define target variables and their Y-axis labels
target_vars = {
    'T': 'Temperature (℃)',
    'P': 'Pressure (m)',
    'Fr': 'Si (-)',
    'Flux': 'Flux (m/d)',
    'K': 'Hydraulic Conductivity (m/d)',
    'Qw': 'Volumetric Water Content (-)',
    'Qi': 'Volumetric Ice Content (-)',
    'Qv': 'Volumetric Vapor Content (-)',
    'TC': 'Thermal Conductivity (W/m/K)',
    'C': 'Heat Capacity (J/m3/K)'
}

def extract_labels(filepath):
    """Extract coordinates from header comments to use as legend labels."""
    labels = []
    pattern = re.compile(r"Point\s+\d+:\s*\(\s*([0-9\.E\+\-]+),\s*([0-9\.E\+\-]+)")
    try:
        with open(filepath, 'r') as f:
            for line in f:
                if not line.startswith('#'):
                    break
                match = pattern.search(line)
                if match:
                    x, y = float(match.group(1)), float(match.group(2))
                    labels.append(f"({x:.1f},{y:.1f})")
    except Exception as e:
        print(f"Warning: Could not parse header for {filepath}: {e}")
    return labels

def plot_variable(var_name, ylabel):
    """Read data and generate a plot for a given variable."""
    # Check for .dat or .csv
    file_dat = os.path.join(output_dir, f"obsf_{var_name}.dat")
    file_csv = os.path.join(output_dir, f"obsf_{var_name}.csv")
    
    if os.path.exists(file_dat):
        filepath = file_dat
        sep = r'\s+'
    elif os.path.exists(file_csv):
        filepath = file_csv
        sep = ','
    else:
        return  # File does not exist, skip
    
    # Extract labels from header, fallback to default if empty
    labels = extract_labels(filepath)
    
    # Read data (skipping comment lines)
    df = pd.read_csv(filepath, sep=sep, comment='#')
    time_col = df.columns[0]
    num_data_cols = len(df.columns) - 1

    # Ensure we have enough labels
    if var_name == 'Flux':
        num_obs = num_data_cols // 2
    else:
        num_obs = num_data_cols
        
    while len(labels) < num_obs:
        labels.append(f"Obs{len(labels)+1}")

    fig, ax = plt.subplots(figsize=(9.71, 6.00))

    if var_name == 'Flux':
        # Flux requires u and v components and unit conversion (* 86400)
        ax.set_yscale('log')
        for i in range(num_obs):
            u_col = 1 + 2 * i
            v_col = 2 + 2 * i
            # Use absolute values for logarithmic scale to prevent missing data points
            u_data = df.iloc[:, u_col].abs() * 86400.0
            v_data = df.iloc[:, v_col].abs() * 86400.0
            
            line = ax.plot(df[time_col], u_data, linewidth=2, linestyle='-', label=f"u {labels[i]}")
            color = line[0].get_color()
            ax.plot(df[time_col], v_data, linewidth=2, linestyle='--', color=color, label=f"v {labels[i]}")
    else:
        # Standard variables
        for i in range(num_obs):
            ax.plot(df[time_col], df.iloc[:, i+1], linewidth=2, label=labels[i])

        if var_name == 'Fr':
            ax.set_ylim(bottom=0.0, top=1.0)

    ax.set_xlabel("Time", fontsize=15)
    ax.set_ylabel(ylabel, fontsize=15)
    ax.grid(True, linestyle=':', alpha=0.7)
    
    # Legend configuration
    if var_name == 'Flux':
        ax.legend(ncol=2, loc='upper right', framealpha=0.9)
    else:
        ax.legend(loc='best', framealpha=0.9)

    plt.tight_layout()
    
    # Save image
    out_img = os.path.join(output_dir, f"obsf_{var_name}.png")
    plt.savefig(out_img, dpi=300)
    plt.close()
    print(f"Saved: {out_img}")

# Process all target variables
for var, ylabel in target_vars.items():
    plot_variable(var, ylabel)

print("All available plots have been generated.")