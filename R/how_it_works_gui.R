# ══════════════════════════════════════════════════════════════════════════
# "How it works" — explainer page for non-technical users
#
# Pure UI module: no server logic, content only.
# Sub-tabs walk through each of the 8 pipeline stages in plain language,
# each with: what it does, an analogy, why it matters, and what controls
# appear on the matching Pipeline page.
# ══════════════════════════════════════════════════════════════════════════

how_it_works_ui <- function(id) {
  ns <- NS(id)

  # Shared block helpers ----------------------------------------------------
  step_block <- function(does, analogy, matters, gui,clar) {
    tagList(
      h4("What this step does"),
      p(does),
      h4("Analogy"),
      tags$blockquote(tags$em(analogy)),
      h4("Why it matters"),
      p(matters),
      h4("What you'll see in the GUI"),
      gui,
      h4("Clarifications for usage"),
      clar
    )
  }

  tabsetPanel(
    id = ns("how_tabs"),

    # ----------------------------------------------------------------------
    # Overview
    # ----------------------------------------------------------------------
    tabPanel(
      "Overview",
      fluidRow(column(
        width = 10, offset = 1,
        br(),
        h2("How MeDUSA works"),
        p("MeDUSA helps you turn raw mass-spectrometry measurements of single
          cells into a list of biological molecules (metabolites) you can
          actually reason about. The tool walks your data through eight
          stages, each one cleaning, organising, or interpreting the signal
          a little further."),
        p(tags$em("Don't worry if some of the words below are unfamiliar — each
          stage has its own tab that explains everything in everyday language,
          with an analogy.")),
        hr(),

        h3("What you'll need before you start"),
        p("1. A CSV file containing all of your samples — cells, blanks,
          growth media, and quality-control samples — mixed together. The
          file must contain at least these three columns:"),
        tags$table(
          class = "table table-bordered table-condensed",
          style = "max-width: 600px;",
          tags$thead(tags$tr(
            tags$th("Column"),
            tags$th("What it means")
          )),
          tags$tbody(
            tags$tr(
              tags$td(tags$code("mz")),
              tags$td("A number identifying a chemical signal (its mass-to-charge ratio).")
            ),
            tags$tr(
              tags$td(tags$code("intensity")),
              tags$td("How strong that signal was in a given sample.")
            ),
            tags$tr(
              tags$td(tags$code("sample_name")),
              tags$td("A unique name for the sample, e.g. ", tags$code("sumr_blank_001"), ".")
            )
          )
        ),
        p("Extra columns (", tags$code("mzmin"), ", ", tags$code("mzmax"),
          ", ", tags$code("mz10"), ", ", tags$code("mz90"), ", ",
          tags$code("sample"), ") are accepted but ignored. If a ",
          tags$code("feature"), " column is present, measurements of the same
          feature are averaged across samples before processing."),
        hr(),
        p("2. A metadata file containing identifying (meta) information about the samples in the main data,
          this file must contain the following columns:"),
        tags$table(
          class = "table table-bordered table-condensed",
          style = "max-width: 600px;",
          tags$thead(tags$tr(
            tags$th("Column"),
            tags$th("What it means")
          )),
          tags$tbody(
            tags$tr(
              tags$td(tags$code("sample_name")),
              tags$td("The same unique name found in the data, matches data and metadata")
            ),
            tags$tr(
              tags$td(tags$code("type")),
              tags$td("The type of entity the sample is, e.g. cell, blank")
            ),
            tags$tr(
              tags$td(tags$code("grouping column (e.g. phenotype)")),
              tags$td("A property of the samples, used to group and analyze the data")
            )
          )
        ),
        p("For the current implementation, the columns ", tags$code("sample_name"), " and ", tags$code("type"),
          " are required within the metadata. For the grouping column, any name is allowed."),
        hr(),
        h3("The eight stages, at a glance"),
        tags$ol(
          tags$li(tags$b("Upload & Quality Check"), " — load the data and check its overall health."),
          tags$li(tags$b("Blank Subtraction"), " — remove signals that came from the background, not from cells."),
          tags$li(tags$b("Mass Defect Filter"), " — throw out signals that look like salts or instrument artefacts."),
          tags$li(tags$b("Isotope Tagging"), " — group different versions of the same molecule together."),
          tags$li(tags$b("Noise Filtering"), " — drop weak or unreliable signals."),
          tags$li(tags$b("Post-processing"), " — fill in gaps and put intensities on a comparable scale."),
          tags$li(tags$b("Analysis"), " — find patterns and differences between groups of cells."),
          tags$li(tags$b("Identification"), " — look up what the surviving signals actually are.")
        ),
        p(tags$em("You can move forward through the stages with the blue ", tags$b("Go to Next Step"),
          " button, or jump back at any point. Each stage also lets you upload your own intermediate
          file if you'd rather skip ahead or replay something.")),
        br()
      ))
    ),

    # ----------------------------------------------------------------------
    # Step 1
    # ----------------------------------------------------------------------
    tabPanel(
      "1. Upload & Quality Check",
      fluidRow(column(
        width = 10, offset = 1, br(),
        h2("Step 1 — Upload & Quality Check"),
        step_block(
          does = "You hand MeDUSA your raw CSV file. The tool reads it,
            recognises which rows belong to which sample, and computes a
            handful of quality numbers per sample — how many peaks were
            detected, how strong they were, the smallest and largest mass
            seen, and so on.",
          analogy = "Like a librarian taking a freshly delivered box of books:
            before sorting them onto the shelves, she counts them, checks
            none are damaged, and notes whether anything is missing.",
          matters = "If a sample was poorly measured — too few peaks, way
            fewer signals than its neighbours — you want to know that
            upfront, so you can either discard it or interpret later results
            with caution.",
          gui = tagList(
            tags$ul(
              tags$li("A ", tags$b("DIMS dataset"), " file picker (the CSV described in the Overview)."),
              tags$li("A ", tags$b("Metadata"), " file picker (the metadata described in the Overview)."),
              tags$li("An ", tags$b("Sample Polarity"), " option select — indicate whether the polarity of the uploaded data is positive or negative"),
              tags$li("A ", tags$b("CPU cores"), " field — how many processor cores to use; higher = faster, but leave at least one free."),
              tags$li("A ", tags$b("Run Quality Check"), " button."),
              tags$li("Four result tabs on the right: ", tags$b("Data Preview"), ", ", tags$b("Metrics Table"), ", ",
                tags$b("Quality Plots"), " (with arrows to flip through metrics like ", tags$code("n_peaks"), " or ", tags$code("median_mz"), "), and ", tags$b("Metadata Check"), ".")
            )
          ),
          clar = tagList(
            tags$ul(
              tags$li("This page is the main upload page of the pipeline. Both", tags$b("a data file and a metadata file are required")," with the column names specified in the Overview section."),
              tags$li("After uploading a metadata file, a preview of the detected column is shown, alongside an option to pick the", tags$b("primary grouping column.")," 
          Select which column should be used to group samples throughout the pipeline according to this column's values."),
              tags$li("The", tags$b("sample polarity"),"can be indicated here, this value is stored globally and can be updated in several other places in the pipeline."),
              tags$li("Besides the quality check results, this page offers a ", tags$b("metadata check,"),"this option verifies whether the uploaded metadata satisfies the constraints for further use."),
            )
          )
        ),
        br()
      ))
    ),

    # ----------------------------------------------------------------------
    # Step 2
    # ----------------------------------------------------------------------
    tabPanel(
      "2. Blank Subtraction",
      fluidRow(column(
        width = 10, offset = 1, br(),
        h2("Step 2 — Blank Subtraction"),
        step_block(
          does = "MeDUSA compares your cell samples against the blanks
            (empty wells), the growth media, and the quality-control
            samples. Any signal that's roughly as strong in the blanks as
            in the cells is most likely contamination or background — and
            is removed from the cell samples.",
          analogy = "Like taking a photo of stars on a hazy night, then
            taking a second photo of just the haze without the stars, and
            subtracting one from the other. What's left should be the
            stars.",
          matters = "Single cells contain very little material, so the
            true cellular signal is fragile. Without this step, the rest
            of the pipeline would be flooded with signals from solvents,
            plastic, dust, and the growth medium — and your conclusions
            would be about the lab, not the biology.",
          gui = tagList(
            tags$ul(
              tags$li("Two ", tags$b("checkbox groups"), " on the left: ", tags$b("Subtract"),
                " (samples to subtract — usually blanks/media/QC, auto-detected from the names) and ",
                tags$b("Keep"), " (cell samples)."),
              tags$li("A ", tags$b("Subtraction method"), ": ", tags$em("Blank aggregation"),
                " (treat all your subtract-samples as one group) or ", tags$em("Blank and Media Max"),
                " (use whichever is louder, blank or media, peak by peak)."),
              tags$li("An ", tags$b("Aggregation method"), " (mean / median) and a ", tags$b("Threshold"),
                " — how much louder a cell signal must be than the blank before it's kept."),
              tags$li("Three result tabs: ", tags$b("Preview"), ", ", tags$b("Split check"),
                " (which samples ended up in which group), and ", tags$b("Result"),
                " — including a survival plot showing which signals were kept (green) vs removed (red).")
            )
          ),
          clar = tagList(
            tags$ul(
              tags$li("The ", tags$b("options to select for subtraction"),"are auto-detected from the dataset and show-up if a valid dataset is passed on from the upload-page or manually uploaded. "),
              tags$li("Options not chosen for subtraction will be marked under ", tags$b("Keep,"),"this group of values is passed into the analysis page further on and used for grouping there"),
            )
          )
        ),
        br()
      ))
    ),

    # ----------------------------------------------------------------------
    # Step 3
    # ----------------------------------------------------------------------
    tabPanel(
      "3. Mass Defect Filter",
      fluidRow(column(
        width = 10, offset = 1, br(),
        h2("Step 3 — Mass Defect Filter"),
        step_block(
          does = tagList(
            "Every real biological molecule has a slightly predictable
            relationship between its full mass and the digits after the
            decimal point of that mass — its ", tags$em("mass defect"),
            ". MeDUSA uses a simple line equation (slope and intercept) to
            decide whether each signal sits in the region where real
            biomolecules live, and discards anything outside that region."
          ),
          analogy = "Like distinguishing real diamonds from glass beads by
            holding them up to a light: real diamonds bend the light at a
            very specific angle. Anything that bends it differently can
            safely be set aside.",
          matters = "Electrospray ionisation — the technique that gets your
            molecules into the mass spectrometer — also picks up salt
            clusters and instrument artefacts. Those don't follow the
            biology rule, so this filter removes them cleanly.",
          gui = tagList(
            tags$ul(
              tags$li("A ", tags$b("Polarity"), " choice (Positive / Negative) — match this to how your samples were measured."),
              tags$li("Two numeric fields: ", tags$b("Slope"), " (default ", tags$code("0.00112"),
                ") and ", tags$b("Intercept"), " (default ", tags$code("0.01953"),
                "). The defaults work for most experiments; only change them if you know why."),
              tags$li("A ", tags$b("Plot results"), " checkbox."),
              tags$li("On the right: a scatter plot of every surviving signal (m/z on the x-axis, mass defect on the y-axis) and a table of the kept peaks.")
            )
          ),
          clar = tagList(
            tags$ul(
              tags$li(tags$b("Sample Polarity"),"is used within this step, therefore the selection from the upload page is passed here, but can be adjusted if desired.")
            )
          )
        ),
        br()
      ))
    ),

    # ----------------------------------------------------------------------
    # Step 4
    # ----------------------------------------------------------------------
    tabPanel(
      "4. Isotope Tagging",
      fluidRow(column(
        width = 10, offset = 1, br(),
        h2("Step 4 — Isotope Tagging"),
        step_block(
          does = "Most molecules show up in the spectrum more than once,
            because some of their carbon atoms are the heavier isotope
            (carbon-13 instead of carbon-12). MeDUSA scans for groups of
            peaks that sit exactly the right mass distance apart and tags
            them as belonging to the same molecule.",
          analogy = "Recognising that two slightly different fingerprints
            belong to the same person — same identity, just left by a
            slightly different finger.",
          matters = "Without grouping isotopes together, you'd treat the
            same metabolite as several different ones, inflate your
            feature count, and dilute every statistical test that follows.",
          gui = tagList(
            tags$ul(
              tags$li(tags$b("Isotope Mass Target"), " (default ", tags$code("1.0034"),
                ") — the mass gap MeDUSA looks for. This is the mass difference between carbon-13 and carbon-12."),
              tags$li(tags$b("Iterations (Max Isotopes)"), " — how many isotope peaks to chain together per group (5 is plenty for most metabolites)."),
              tags$li(tags$b("Tolerance (ppm)"), " — how forgiving to be about the exact mass gap; 5 ppm is a sensible default."),
              tags$li("On the right: a summary (how many isotope groups were found) and a preview table with columns ",
                tags$code("Group"), ", ", tags$code("mz"), ", ", tags$code("diff_to_prev"),
                " (distance from the previous peak in the group), and ", tags$code("total_dist"),
                " (distance from the lightest peak in the group).")
            )
          ),
          clar = tagList(
            tags$ul(
              tags$li("This step can take a while and does not alter the dataset, therefore a", tags$b("skip button"), "is available to safely skip this part if desired."),
              tags$li("The", tags$b("download button"),"of this page downloads a report on the isotope hunting results, since no changes to the dataset are made in this step."),
            )
          )
        ),
        br()
      ))
    ),

    # ----------------------------------------------------------------------
    # Step 5
    # ----------------------------------------------------------------------
    tabPanel(
      "5. Noise Filtering",
      fluidRow(column(
        width = 10, offset = 1, br(),
        h2("Step 5 — Noise Filtering"),
        step_block(
          does = "MeDUSA drops three kinds of unreliable signals at once:
            anything weaker than your intensity threshold, anything missing
            from too many samples, and anything on the built-in blacklist
            of known contaminants.",
          analogy = "Like skimming a guest list before a dinner party:
            remove the people who only RSVPed faintly, the ones who never
            showed up to the last few dinners, and anyone on the do-not-invite
            list.",
          matters = "Weak or sporadic peaks make later statistics noisy
            and unreliable. Stripping them out here keeps only the
            features that are strong, consistent, and biologically
            interpretable.",
          gui = tagList(
            tags$ul(
              tags$li(tags$b("Min Intensity Threshold"), " (default ", tags$code("5000"),
                ") — minimum signal strength to keep a feature."),
              tags$li(tags$b("Missingness (fraction)"), " (default ", tags$code("0.1"),
                ") — drop features missing in more than this fraction of samples."),
              tags$li(tags$b("Blacklist"), " option select — decide whether to exclude m/z values known to be contaminants or not, using the default list or a custom upload."),
              tags$li("On the right: a summary of features before and after filtering, plus a transposed preview (samples as rows, m/z features as columns) so you can scroll through many samples easily.")
            )
          ),
          clar = tagList(
            tags$ul(
              tags$li("Since the Isotope Tagging step does not alter the dataset, to continue on from a manually updated dataset on this page a .csv file as retrieved from the Mass Defect Filter step should be uploaded here, if desired."),
              tags$li("A warning will be shown if no features remain after filtering, consider changing the parameters if this happens. A common fix to this issue is to lower the",tags$b("missingness fraction")," to a smaller value."),
              
            )
          )
        ),
        br()
      ))
    ),

    # ----------------------------------------------------------------------
    # Step 6
    # ----------------------------------------------------------------------
    tabPanel(
      "6. Post-processing",
      fluidRow(column(
        width = 10, offset = 1, br(),
        h2("Step 6 — Post-processing"),
        step_block(
          does = tagList(
            "MeDUSA fills in missing values, smooths out
            sample-to-sample variability by normalising intensities, and
            applies a log transform so the resulting numbers are well-suited
            to statistical tests. It then emits the data in two shapes:
            ", tags$em("mzLog"), " (wide, log-scaled) for plotting and stats,
            and ", tags$em("mzLong"), " (one row per measurement) for
            downstream tools."
          ),
          analogy = "Like the post-production stage of a film: colour
            grading, audio levelling, gap-filling — all the boring but
            essential work that makes the raw footage actually watchable.",
          matters = "Raw intensities range over many orders of magnitude
            and have gaps. Most statistical methods assume a more uniform
            scale and no missing data — this step gives them what they
            need.",
          gui = tagList(
            tags$ul(
              tags$li(tags$b("Polarity"), " (Positive / Negative) — match your acquisition."),
              tags$li(tags$b("Low noise bound"), " and ", tags$b("High noise bound"),
                " — the range used to estimate background noise during normalisation."),
              tags$li(tags$b("Generate plots"), " checkbox — produce diagnostic plots showing the effect of normalisation."),
              tags$li("Four result tabs: ", tags$b("Summary"), ", ", tags$b("mzLog preview"),",", tags$b("mzLong preview"),
                ", and ", tags$b("Plots"), ". You can download either shape.")
            )
          ),
          clar = tagList(
            tags$ul(
              tags$li("The metadata can be inserted/updated on the", tags$b("Upload page."),"A valid metadata is required for this step."),
              
            )
          )
        ),
        br()
      ))
    ),

    # ----------------------------------------------------------------------
    # Step 7
    # ----------------------------------------------------------------------
    tabPanel(
      "7. Analysis",
      fluidRow(column(
        width = 10, offset = 1, br(),
        h2("Step 7 — Analysis"),
        step_block(
          does = tagList(
            "MeDUSA opens an analysis hub with three doors:
            ", tags$b("Visualization"), " (see the structure of your data),
            ", tags$b("Statistics"), " (find features that differ between
            groups), and ", tags$b("Classification"), " (train a model to
            predict cell type from the metabolite profile). Open any door,
            do your work, come back to the hub, open another."
          ),
          analogy = "Like the foyer of a museum with three wings:
            \"What does it look like?\", \"What's different between these
            samples?\", and \"Can a computer tell them apart?\"",
          matters = "This is where the biology shows up. Everything before
            this step was cleaning; everything from here on is finding
            answers.",
          gui = tagList(
            tags$ul(
              tags$li(tags$b("Visualization"), " — Principal Component Analysis (PCA), which projects high-dimensional samples onto a 2D map so you can see whether groups cluster apart; and heatmaps, which colour each feature by intensity."),
              tags$li(tags$b("Statistics"), " — Welch's t-test (compare two groups), fold-change calculation, a volcano plot (effect size vs significance), and ANOVA (compare more than two groups)."),
              tags$li(tags$b("Classification"), " — Random Forest, a machine-learning model that learns to tell phenotypes apart from their metabolite profiles."),
              tags$li("A ", tags$b("Go to Identification"), " card to finish analysis and move on to step 8.")
            )
          ),
          clar = tagList(
            tags$ul(
              tags$li("The analysis page consists of several", tags$b("subpages."),"Each page from here on out can be used in any order depending on needs."),
              tags$li("On the visualization page, the option to select which", tags$b("m/z tables to use ")," exists. By default the full m/z table passed on from post-processing is selected, by completing the Welch T-test, Fold change, ANOVA or Random Forest on the other subpages, their m/z tables become available for visualization here as well."),
              tags$li("On the statistics page", tags$b("the comparison groups"),"can be selected, this requires a selected grouping column on the Hub page and a valid metadata."),
              tags$li("A", tags$b("volcano plot"),"can only be generated, after the execution of the Welch T-test and the Fold change, the status bar on the volcano page shows whether these have been done or not."),
              tags$li("For ANOVA there is an option to apply a", tags$b("log transform"),"usually this should be checked and a suitable column be selected for (generally intensity), as ANOVA requires this. If a log column already exists in the data uncheck the checkbox and pick this column as the column to log-transform. "),
              tags$li(tags$b("Random Forest classification"),"can take a while to complete."),
            )
          )
        ),
        br()
      ))
    ),

    # ----------------------------------------------------------------------
    # Step 8
    # ----------------------------------------------------------------------
    tabPanel(
      "8. Identification",
      fluidRow(column(
        width = 10, offset = 1, br(),
        h2("Step 8 — Identification"),
        step_block(
          does = tagList(
            "MeDUSA takes each surviving m/z value and looks it up in
            a reference database of known molecules, accounting for the
            different chemical ", tags$em("adducts"), " (the small ions a
            molecule picks up during ionisation, like an extra hydrogen).
            For each match, you get a candidate name, formula, and
            database ID."
          ),
          analogy = "Like running a fingerprint through a national database
            to find the person it belongs to — except some prints look
            similar, so you may get a short list of suspects rather than
            a single answer.",
          matters = "Up to this point, your features were just numbers.
            Identification is what turns them into biology: \"this peak is
            glucose-6-phosphate\", \"this one looks like a phospholipid\".
            Without it, you have a list of suspicious m/z values but no
            story to tell.",
          gui = tagList(
            tags$ul(
              tags$li(tags$b("Select Database"), " — choose ", tags$em("HMDB"),
                " (Human Metabolome Database), ", tags$em("Lipids"),
                " (a curated lipid database), or ", tags$em("Custom CSV"), " (your own list)."),
              tags$li(tags$b("Adducts"), " — comma-separated chemical adducts to consider, e.g. ",
                tags$code("M+H"), " for protonated ions in positive mode."),
              tags$li(tags$b("Tolerance"), " — how close a database mass needs to be to count as a match (default ", tags$code("5e-6"), ", i.e. 5 ppm)."),
              tags$li("If Custom CSV is selected: a database file picker and a field naming the column that holds the exact masses."),
              tags$li("A ", tags$b("Filter m/z by unique feature"), " checkbox — if checked the result table will only return the search result of one m/z value for each feature."),
              tags$li("On the right: an interactive, sortable, searchable table of identification results — downloadable as a CSV.")
            )
          ),
          clar = tagList(
            tags$ul(
              tags$li("This page has two", tags$b("overwrite dataset options.")," The first one, takes a processed dataset or a custom mz table to match its contents against the selected database. The second upload requires a dataset with a feature column, usually the original non-processed dataset, this allows for when the checkbox is checked to filter the mz table to only show matches for one unique m/z value per unique feature, for a clearer overview."),
              
            )
          )
        ),
        br()
      ))
    )
  )
}
