# Installation Guide
This guide covers installing and uninstalling the MeDUSA package on Windows, macOS, and Linux.
> Important
> Run all below commands in the 2026-34-MEDUSA/installation folder!

## 1. Prerequisites

- R version 4.0 or newer
- Internet access for downloading dependencies

## 2. Installation

### Windows
```bat
install.bat
```
> Tip: You can double-click the **Windows batch file (`.bat`)** or run it from the Command Prompt.

### macOS / Linux
```bash
bash install.sh
```

### R-based Installation
```bash
Rscript install.R
```

### Install Flags
`--no-db` → Skip database download during installation

---

## 3. Uninstallation

### Windows
```
uninstall.bat
```
> Tip: You can double-click the **Windows batch file (`.bat`)** or run it from the Command Prompt.

### Mac / Linux
```bash
bash uninstall.sh
```

### R-based Installation
If administrator privileges are required, use the **R installation script (`uninstall.R`)** as the default installer.  
```bash
Rscript uninstall.R
```

### Uninstall Flags:
`--all` → Remove the MeDUSA databases in addition to the removal of the pacakge.

### Verification
Run in R:

```r
"MeDUSA" %in% rownames(installed.packages())
```

Expected result:
- `FALSE`

## 5. Troubleshooting

### `RScript` not found
- Install R from https://cran.r-project.org/
- Restart terminal and rerun

### Permission errors
- Ensure your terminal has write access to the project and R library directories