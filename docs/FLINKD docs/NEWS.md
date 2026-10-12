## Table of contents
1. [Version 0.0 - Setup](#version-00---setup)
2. [Version 0.1 - Optimizations](#version-01---optimizations)
3. [Version 0.2 - Pipeline Refactor & Benchmarking Support](#version-02---pipeline-refactor--benchmarking-support)
4. [Version 0.3 - Installation Guide](#version-03---installation-guide)
5. [Version 0.4 - CI/CD](#version-04---cicd)
6. [Version 1.0 - GUI](#version-10---gui)

## Version 0.0 - Setup
Summary: Initial project notes and NEWS.md setup.

### NEWS.md setup

- added NEWS.md creation with [newsmd](https://github.com/Dschaykib/newsmd)

## Version 0.1 - Optimizations & Refactoring
Summary: Perfomance based optimizations and refactoring for stability and maintainability
- ## Performance
  - #### Vectorized Welch T-test using matrix algebra
    <details>

      ```R
      mzlog_analysis_welch <- function(phenoA_mz_obj, phenoB_mz_obj, ...) {
        ...
        p_vals <- pbapply::pbapply(df_l$mz_idx, 1, function(x) {
          t.test(df_l$df_a[x,], df_l$df_b[x,])$p.value
        })
      }
      ```
      To:
      ```R
        mzlog_analysis_welch <- function(phenoA_mz_obj, phenoB_mz_obj, ...) {
          ...
          var1 <- matrixStats::rowVars(matA, na.rm = TRUE)
          var2 <- matrixStats::rowVars(matB, na.rm = TRUE)
          
          t_stat <- (rowMeans(matA) - rowMeans(matB)) / sqrt(var1/n1 + var2/n2)
          p_vals <- 2 * pt(-abs(t_stat), df)
        }
      ```
    </details>

  - #### Optimized fold change with fast row-wise aggregations
    <details>

      ```R
      mzlog_analysis_fold <- function(phenoA_mz_obj, phenoB_mz_obj, fold_math = "mean") {
        ...
        valA <- apply(df_l$df_a, 1, fold_math)
        valB <- apply(df_l$df_b, 1, fold_math)
      }
      ```

      To:
      ```R
      mzlog_analysis_fold <- function(phenoA_mz_obj, phenoB_mz_obj, fold_math = "mean") {
        ...
        if (fold_math == "mean") {
          valA <- rowMeans(df_l$df_a, na.rm = TRUE)
        } else if (fold_math == "median") {
          valA <- matrixStats::rowMedians(as.matrix(df_l$df_a), na.rm = TRUE)
        }
      }
      ```
    </details>
  - #### Replaced slow T-test loop with vectorized matrix math
    <details>

      ```R
      out$p <- pbapply::pbapply(df_l$mz, 1 , cl = cl, function(i){
            t.test(df_l$df_a[df_l$mz==i,], df_l$df_b[df_l$mz==i,])$p.value
          })
      ```
      To:
      ```R
      var1 <- matrixStats::rowVars(matA, na.rm = TRUE)
        var2 <- matrixStats::rowVars(matB, na.rm = TRUE)
        t_stat <- (mu1 - mu2) / sqrt(v1_n1 + v2_n2)
        p_vals <- 2 * pt(-abs(t_stat), df)
      ```
    </details>

  - #### Sample exactly the amount needed, avoid creating entire sequence
    <details>

      ```R
        mz_post_imputation <- function(input_mz_obj, low_noise=10, high_noise=NULL){
        ...
        tmp[bool] <- sample(low_noise:high_noise,
                                        sum(bool),
                                        replace = TRUE)
        }
      ```

      To:
      ```R
          mz_post_imputation <- function(input_mz_obj, low_noise=10, high_noise=NULL){
          ...
          width <- (high_noise - low_noise + 1)
          tmp[bool] <- (low_noise - 1L) + sample.int(width, sum(bool), replace = TRUE)
          }
      ```
    </details>

  - #### Operate on matrices instead of dataframes
    <details>

      ```R
      mz_post_imputation <- function(input_mz_obj, low_noise=10, high_noise=NULL){
        tmp <- input_mz_obj[colnames(input_mz_obj) != 'mz']
        ...
        input_mz_obj[colnames(input_mz_obj) != 'mz'] <- tmp
      ```

      to:

      ```R
        tmp <- as.matrix(input_mz_obj[colnames(input_mz_obj) != 'mz'])
        ...
        input_mz_obj[colnames(input_mz_obj) != 'mz'] <- as.data.frame(tmp)
      ```
    </details>

  - #### Removed double transpose on entire quotNorm
    <details>

      ```R
      normal <- data.frame(quotNorm(t(dplyr::select(input_mz_obj,-mz))))
      normal <- as.data.frame(t(normal))
      ```

      to:

      ```R
      result <- quotNorm(t(dplyr::select(input_mz_obj,-mz)))
      normal  <- as.data.frame(t(result$X))
      ```
    </details>

- ## Stability
  - #### Removed unstable merge-by-rowname logic and replaced it with a cleaner matrix-to-dataframe pipeline.
    <details>

      ```R
        t_mz_obj <- merge(x = t_mz_obj, y = subset(tibble::column_to_rownames(metadata, "sample_name"), select = qual_col), by = 0, all.x = TRUE)
        rownames(t_mz_obj) <- paste(t_mz_obj$Row.names, t_mz_obj[[ qual_col ]], sep = "&")
      ```
      To:
      ```R
      pca_res <- prcomp(t_mz_obj, scale. = TRUE)
        plot_df <- as.data.frame(pca_res$x)
        meta_subset <- metadata[match(plot_df$sample_name, metadata$sample_name), ]
        plot_df[[qual_col]] <- meta_subset[[qual_col]]
      ```
    </details>

- ## Refactoring
  - #### Hardened heatmap sample matching
    <details>

      ```R
      if (sum( sort(metadata$sample_name) != sort(colnames(plot_obj)))) { 
            stop("ERROR:MeDUSA::mzlog_analysis_heatmap: annotation sample_name does not match") 
          }
      ```

      To:
      ```R
      match_idx <- match(current_samples, paste0(pol, "_", metadata$sample_name))
          if (any(is.na(match_idx))) {
            stop("ERROR:MeDUSA::mzlog_analysis_heatmap: metadata and samples do not match")
          }
      ```
    </details>

  - #### Removed redundant call to row.names
    <details>

      ```R
      mz_post_pivot_longer <- function(input_mz_obj, plot = TRUE) {
        row.names(input_mz_obj) <- input_mz_obj$mz
        input_mzlong <- tidyr::pivot_longer(input_mz_obj, !mz, names_to = "sample_name", values_to = "intensity")
          if(plot){
            mzlongpost.plot(input_mzlong, local.mz_polarity_guesser(input_mz_obj), "PivotLonger")
          }
        data.frame(input_mzlong)
      }
      ```

      to:

      ```R
      mz_post_pivot_longer <- function(input_mz_obj, plot = TRUE) {
        input_mzlong <- tidyr::pivot_longer(input_mz_obj, !mz, names_to = "sample_name", values_to = "intensity")
          if(plot){
            mzlongpost.plot(input_mzlong, local.mz_polarity_guesser(input_mz_obj), "PivotLonger")
          }
        data.frame(input_mzlong)
      }
      ```
    </details>

  - #### Removed redundant call to function by adding helper variable "polarity"
    <details>

      ```R
      mz_post_normalization <- function(input_mz_obj, metadata, plot = TRUE ){
        ...
        dilution <- dplyr::left_join(metadata, dilution, by = join_by)

        if(plot){
          ggpubr::ggboxplot(dilution, x="phenotype", y="normal.dilution",
                            ylab.    = "Dilution Factor",
                            xlab.    = "Phenotype",
                            title    = "Normalized Dilution Factor per Phenotype",
                            subtitle = local.mz_polarity_guesser(input_mz_obj),
                            legend = "none")

          local.save_plot(paste("Normalization",local.mz_polarity_guesser(input_mz_obj),sep="-"))
        }
        ...
      }
      ```

      to:

      ```R
      mz_post_normalization <- function(input_mz_obj, metadata, plot = TRUE ){
        ...
        polarity <- local.mz_polarity_guesser(input_mz_obj)

        if(plot){
          ggpubr::ggboxplot(dilution, x="phenotype", y="normal.dilution",
                            ylab.    = "Dilution Factor",
                            xlab.    = "Phenotype",
                            title    = "Normalized Dilution Factor per Phenotype",
                            subtitle = polarity,
                            legend = "none")

          local.save_plot(paste("Normalization",polarity,sep="-"))
        }
        ...
      }
      ```
    </details>

  - #### Changed grep() to which() to avoid RegEX
    <details>

      ```R
      mz_post_log <- function(input_mz_obj) {
        ind <- grep("mz", colnames(input_mz_obj))
        ...
      }
      ```

      to:

      ```R
      mz_post_log <- function(input_mz_obj) {
        ind <- which(colnames(input_mz_obj) == "mz")
        ...
      }
      ```
    </details>

  - #### Refactored tryCatch{()} calls by adding helper function
    <details>

      ```R
      mz_post_magic <- function(input_mz_obj, metadata, noise = c(10,5000), plot = TRUE){
        tryCatch({
          input_mz_obj <- mz_post_imputation(input_mz_obj, noise[1], noise[2])
          print("INFO:MeDUSA::mz_post_magic: imputation success")
        }, error = function(e) {
          print(e)
          stop("ERROR: in mz_post_imputation")
        })
        tryCatch({
          input_mz_obj <- mz_post_normalization(input_mz_obj, metadata, plot)
          print("INFO:MeDUSA::mz_post_magic: normalization success")
        }, error = function(e) {
          print(e)
          stop("ERROR:MeDUSA::mz_post_magic: in mz_post_normalization")
        })
        tryCatch({
          input_mzlong <- mz_post_pivot_longer(input_mz_obj, plot)
          print("INFO:MeDUSA::mz_post_magic: pivot_longer success")
        }, error = function(e) {
          print(e)
          stop("ERROR:MeDUSA::mz_post_magic: in mz_post_pivot_longer")
        })
        tryCatch({
          input_mzlong <- mzlong_post_log(input_mzlong, plot)
          print("INFO:MeDUSA::mz_post_magic: mzlong_log_transform success")
        }, error = function(e) {
          print(e)
          stop("ERROR:MeDUSA::mz_post_magic: in mzlong_post_log")
        })
        out <- mz_post_log(input_mz_obj)
        list("mzLong" = input_mzlong ,"mzLog" = out)
      }
      ```

      To:
      ```R

      mz_post_magic_stages <- function(stage_name, stage_func, ...){
        result <- tryCatch({
          stage_func(...)
        }, error = function(e) {
          print(e)
          stop("ERROR: in: ", stage_name)
        })
        result
      }

      mz_post_magic <- function(input_mz_obj, metadata, noise = c(10,5000), plot = TRUE){

        input_mz_obj <- mz_post_magic_stages("mz_post_imputation", 
                                              mz_post_imputation, input_mz_obj, noise[1], noise[2])
        print("INFO:MeDUSA::mz_post_magic: imputation success")

        input_mz_obj <- mz_post_magic_stages("mz_post_normalization", 
                                              mz_post_normalization,input_mz_obj, metadata, plot)
        print("INFO:MeDUSA::mz_post_magic: normalization success")

        out <- mz_post_magic_stages("mz_post_log", mz_post_log, input_mz_obj)
        print("INFO:MeDUSA::mz_post_magic: log_transform success")

        input_mzlong <- mz_post_magic_stages("mz_post_pivot_longer", mz_post_pivot_longer, out)
        print("INFO:MeDUSA::mz_post_magic: pivot_longer success")

        list("mzLong" = input_mzlong ,"mzLog" = out)
      }
      ```
    </details>

## Version 0.2 - Pipeline Refactor & Benchmarking Support
Summary: Resumable pipeline and debugging statements.

### Pipeline
- Added step-level execution switches so each pipeline stage can be run or skipped independently
- Added `.rds` checkpoints after each pipeline step
- Added support for resuming the pipeline from intermediate checkpoints

### Runtime reporting
- Added standardized baseline storage under `output/baseline/`
- Added dataset-specific baseline file naming
- Added `create_baseline()` helper for baseline creation
- Improved messaging when baseline files are missing

### Debugging
- Added `debug_utils.R`
- Added `inspect_checkpoint()` for checking saved pipeline state
- Added `inspect_step()` for loading and inspecting checkpoints during debugging
- Improved checkpoint lookup to support cross-date inspection
- Checkpoints are now automatically resolved from the latest available run if today's output is missing
- Added support for explicitly selecting checkpoint date via `output_date`
- Fixed issue where `inspect_checkpoint()` fails when no output exists for the current date

### Output organization
- Standardized runtime outputs under `output/<date>/`
- Added baseline runtime storage under `output/baseline/`
- Added checkpoint storage under `output/<date>/checkpoints/`

## Version 0.3 - Installation Guide
Summary: Simplified installation script and documented installation guide

- ### Extended the installation guide with code snippets and prerequisteries

- ### Added dynamic directory traversal to the installation script.
    Prevents having to be in a specific directory when calling the uninstall script.

- ### Added forced unlinking when de-installing MeDUSA package
  <details>

    ```R
    unlink(renv_dir, recursive = TRUE)
    ```
    to:
    ```R
    unlink(renv_dir, recursive = TRUE, force = TRUE)
    ```
  </details>

## Version 0.4 - CI/CD
Summary: Added automation for linting and styling.
- ### Added github workflow for linting on pull requests
  <details>

    ```yaml
    name: lint.yml
    run-name: R-linting

    on: 
        pull_request:
            branches: [main]

    jobs:
        check-R-linting-issues:
            runs-on: ubuntu-latest
            steps: 
                - uses: actions/checkout@v4

                - uses: r-lib/actions/setup-r@v2
                  with:
                    use-public-rspm: true

                - name: Install lintr
                  run: install.packages("lintr")
                  shell: Rscript {0}

                - name: Lint
                  run: lintr::lint_dir(".")
                  shell: Rscript {0}
                  env: 
                    LINTR_ERROR_ON_LINT: true
    ```
  </details>

- ### Added automatic styling script using styler package in scripts/styler.sh
  <details>

    ```sh
    set -e

    ## Set default to current working directory
    TARGET="${1:-.}"

    Rscript -e "
        if (!requireNamespace('styler', quietly = TRUE)) {
            install.packages('styler', quiet = TRUE)
        }
    "

    if [ -f "$TARGET" ]; then
        Rscript -e "styler::style_file('$TARGET')"

    elif [ -d "$TARGET" ]; then
        Rscript -e "styler::style_dir(path = '$TARGET', recursive = TRUE, filetype='R')"
    else
        echo "ERROR: '$TARGET' is not a valid file or directory"
        exit 1
    fi
    ```
  </details>
  



## Version 1.0 - GUI
Summary: Implemented an interactive, modular Shiny application for the MeDUSA analytical pipeline.

- ### Unified App Architecture (`modapp.R`)
  - Integrated a synchronized state-machine layout allowing seamless state transfer between independent processing modules.
  - Implemented a dual-mode reactive bottom navigation layer splitting workflows dynamically between "Pre-processing" and "Statistical Analysis".
  - Embedded automatic step-skipping mechanics (`Skip to Analysis →`) with multi-stage input preservation across disconnected tabs.
  - Created a dedicated, non-technical explainer tab (`how_it_works_gui.R`) walking users through the 8 pipeline stages using everyday analogies (e.g., starry-night haze subtraction for blanks, light-bending diamonds for mass defect analysis).

- ### Modular Pipeline Component Implementations
  - #### GUI Step 1&2: Upload & Quality Check (`2_quality_check_gui.R` / `dataloader.R`)
    - Implemented raw DIMS dataset parser and optional Metadata mapping engine.
    - Added parallel processing optimization switches utilizing customizable CPU core distribution.
    - Embedded dynamic report outputs displaying `Metrics Table`, `Quality Plots` (for `n_peaks`, `median_mz`), and automatic metadata validation.
  - #### GUI Step 3: Blank Subtraction (`3_subtraction_gui.R`)
    - Developed comparative sample isolation contrasting cells against blanks, growth media, and quality controls.
    - Added dual subtraction frameworks (`Blank aggregation` and `Blank and Media Max`) paired with strict fold-change signal intensity retention thresholds.
  - #### GUI Step 4: Mass Defect Filter (`4_mass_defect_gui.R`)
    - Built a linear filter (slope/intercept parameters) isolating true cellular biomolecules from salt clusters and electrospray artifacts.
    - Integrated a live scatter plot mapping absolute $m/z$ vs. fractional mass defect distribution.
  - #### GUI Step 5: Isotope Tagging (`5_tagging_gui.R`)
    - Refactored peak tracking to display continuous numbering for isotope clusters, collapsing redundant Carbon-13 versions of the same chemical entity.
    - Replaced redundant inline metadata with localized user definitions for high-precision tracking variables (`diff_to_prev` and `total_dist`).
  - #### GUI Step 6: Noise Filtering (`6_filter_gui.R`)
    - Transposed the core intensity preview engine (Samples as rows, $m/z$ features as columns) to facilitate vertical scroll layouts across immense clinical cohorts.
    - Isolated and protected the structural `mz` identification vector to eliminate down-stream matrix truncation errors.
  - #### GUI Step 7: Post-Processing & Normalization (`7_post_processing_gui.R`)
    - Linked high-throughput multi-stage imputation workflows with automated data-wide normalization.
    - Generated unified split-output models isolating `mzLog` (wide, log-scaled matrix) for mathematical analysis and `mzLong` (long format) for external integration.
  - #### GUI Step 8: Analysis Hub (`8_analysis_gui.R`)
    - Subdivided high-dimensional analysis into distinct tactical sub-modules:
      - `8a_visualization.R`: Implemented a multi-table consensus matrix parser and integrated Principal Component Analysis (PCA) 2D mapping and interactive intensity heatmaps.
      - `8b_statistics.R`: Accelerated matrix-derived Welch's t-test, Fold-Change math, Volcano plots, and ANOVA controls.
      - `8c_classification.R`: Random Forest machine-learning estimators training phenotype prediction models directly across post-processed `mzLog` tables.
      - `8d_shortcut.R`: OMITTED.
  - #### GUI Step 9: Metabolite Identification (`9_identify_gui.R`)
    - Developed an active library-query wrapper mapping $m/z$ features against known databases (HMDB, Lipids, or Custom CSV entries) while strictly computing chemical adduct formations (e.g., `M+H` protonation shifts) under user-specified ppm tolerances.
   
- ### Added Phenotype Filtering and Metadata Building Modules
  - #### Phenotype Filtering Framework (`phenotype_filter.R`)
    - Implemented `pheno_filter` to accept either plain dataframes or reactive pipelines with a 
    - Added a `keep_mz` positioning toggle to systematically ensure the identity column `mz` remains as the first column in the returned object.
  - #### Metadata Construction (`metadata_builder.R`)
    - Classify raw sample names into analytical categories (`blank`, `media`, `qc`, or `cell`).
    - Standardized the output into a structured data.frame for downstream pipelines.
  - #### Bidirectional Matrix Converter & File I/O Hub (`dataloader.R`)
    - Implemented an automated validation and parsing module (`dataloader_module`) that dynamically handles structural conversions between wide and long mass spectrometry layouts using regular expression header parsing and optimized `tidyr` reshaping pipelines.
  - #### Documentation Engine (`how_it_works_gui.R`)
    - Developed an active pure-UI explainer suite walking non-technical operators through the eight analytical stages using contextual analogies and clear parameter mapping.
