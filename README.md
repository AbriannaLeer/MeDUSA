# MeDUSA

MeDUSA is modular, customizable, and user friendly toolset in R to ease the data processing of Direct Infusion Untarged Single-cell Analysis.

![Summary of methods](docs/medusa-medusa.png)

## Installation & Running

### Local Installation (Recommended)
MeDUSA requires R (>= 4.0) installed on your system. You can download the latest version of R from [CRAN](https://cran.r-project.org/).

Please open your ***terminal*** and execute all installation commands from the **project root directory (`2026-34-MeDUSA/`)**. 

#### Method 1: Platform-Specific Shell Scripts (Recommended)
***Windows:***
```bash
installation\install.bat
```
or open your file manager, navigate to the `installation/` directory, and ***double-click*** the `install.bat` executable file.

***MacOS / Linux:***
```bash
chmod +x installation/install.sh
./installation/install.sh
```
#### Method 2: R-Script Execution (Any OS)
```bash
Rscript installation/install.R
```

This will:
1. Set up `renv` for reproducible package management
2. Install all R dependencies (including necessary CRAN and Bioconductor packages)
3. Build and install the `MeDUSA` package
4. Download and process compound databases (HMDB, LIPID MAPS)


To skip database download (e.g. if you don't need the identify functions yet):
```bash
Rscript installation/install.R --no-db
```

Databases can be set up later with: ```Rscript scripts/db_setup.R```


### Running the Pipeline
Once the setup completes successfully, launch the core workflow from the root directory inside `R`:
``` R
source("scripts/run_pipeline.R") 
```

### Running the GUI (Graphical User Interface):
***Via R Console / RStudio***

Make sure you are already inside an interactive R session.

If it is the first time to launch the GUI, paste the following one-liner to ensure `shiny` is installed:
``` R
if (!requireNamespace("shiny", quietly = TRUE)) install.packages("shiny"); shiny::runApp("modapp.R")
```

Otherwise:
``` R
shiny::runApp("modapp.R")
```




### Multithreading and memory issues
The Extract Workflow: The data extraction functions retain partial multi-threading support exposed via the `cl` parameter (a parallel cluster object).

Memory Pressure: During intensive benchmarking, high memory pressure was observed particularly within the final binning phases, making multi-threading impractical for certain low-memory environments. While `DuckDB` integration has significantly mitigated this overhead, multi-threading remains constrained until threaded database connections are completely refactored.

Troubleshooting: If you experience frozen or orphaned backend processes, utilize `showConnections()` and `closeAllConnections()` within your R environment to safely clear dead locks.


## Data Structures
MeDUSA's modularity is achived via common data objects that are interchangeable.

### MZ-OBJ: The standard "MZ Object"
DataFrame where:
  - Rownames = Float: MZ
  - Colnames = String: sample_names
  - Data     = Float: Intensities

### MzT-OBJ: An "MZ Object" rebranded to represent scan-time.
DataFrame where:
  - Rownames = Float: MZ
  - Colnames = Float: ScanTime
  - Data     = Float: Intensities

### mzLog-OBJ: A log2 of "MZ Object."
DataFrame where:
  - Rownames = Float: MZ
  - Colnames = String: sample_names
  - Data     = Float: log2(Intensities)

Programatically: the methods for [mz-obj, mzT-obj, mzLog-obj] are all interchangeable....proceed with caution.

### mzLong-OBJ: A pivot longer of an "MZ Object"
DataFrame where columns are:
  - Float : MZ
  - String: ScanTime
  - Float : intensity

### METADATA: The standard "MZ Metadata object"
Dataframe required columns are:
  - Integer: measurement
  - String : sample_name (Must match above)
  - String : type (i.e. cell, media_cell, solvent)
  - String : phenotype
Additional columns could be helpful. Such as:
  - Integer: time
  - Integer: plate
  - Integer: cell_count
  - String : sampler
  - String : polarity
  - String : sampling_day
  - String : filtered_out

## R Project Structure
MeDUSA's files and methods are named to quickly identify input and output expectations.

### Primary files:
**file_name** : [suggested order]_[input object type]_[description].R
i.e. "2_mz_quality_check.R"

**public function** : [input_object]_[category]_magic
suggested method to handle everything in the file category (i.e. "mz_post_magic")

**public function** : [input object]_[subcategory]_[description]
(i.e. "mzlog_analysis_fold")

**private funciton** : [type].[description]
(i.e. "extract.binning")

### Support files
file: z_[description].R
  - common methods written by ehetzel
file: zz_[description].R
  - common methods written by others

## Support Project Structure
Also in this repo are Rproj adjacent tools

**docs**: JOSS paper materials
**examples** : a small set of data and an example of ideal usage
**scripts** : mostly data massaging scripts to provide compound data in the docker container
**validation**: TBD

