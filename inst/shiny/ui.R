library(shiny)
library(shinythemes)
library(waiter)

full_app <- isTRUE(getOption("DIMPLE.full_app", FALSE))

app_card <- function(title, subtitle = NULL, ..., class = NULL) {
  div(
    class = paste(c("dimple-card", class), collapse = " "),
    div(
      class = "dimple-card-header",
      div(
        h3(title),
        if (!is.null(subtitle)) p(subtitle, class = "card-subtitle")
      )
    ),
    div(class = "dimple-card-body", ...)
  )
}

plot_card <- function(title, subtitle, plot_id, download_id, height = "430px") {
  app_card(
    title,
    subtitle,
    plotOutput(plot_id, height = height),
    div(
      class = "plot-actions",
      downloadButton(download_id, "Download PDF", class = "btn btn-default btn-sm")
    )
  )
}

fluidPage(
  shinyjs::useShinyjs(),
  autoWaiter(),
  theme = shinytheme("flatly"),
  tags$head(
    tags$meta(name = "viewport", content = "width=device-width, initial-scale=1"),
    tags$style(HTML("
      body {
        background: #f7f8fa;
        color: #00274C;
      }
      a {
        color: #00274C;
      }
      a:hover,
      a:focus {
        color: #001F3D;
      }
      .container-fluid {
        max-width: 1500px;
        padding-left: 26px;
        padding-right: 26px;
      }
      .dimple-hero {
        margin: 20px 0 18px 0;
        padding: 30px 34px;
        border-radius: 16px;
        background: #00274C;
        color: white;
        box-shadow: 0 10px 30px rgba(0, 39, 76, 0.18);
      }
      .dimple-hero .eyebrow {
        display: inline-block;
        margin-bottom: 6px;
        font-size: 12px;
        font-weight: 700;
        letter-spacing: 0.12em;
        text-transform: none;
        color: #FFCB05;
        opacity: 1;
      }
      .dimple-hero h1 {
        margin: 0;
        font-size: 34px;
        font-weight: 700;
        letter-spacing: -0.02em;
      }
      .dimple-hero p {
        max-width: 780px;
        margin: 8px 0 0 0;
        font-size: 16px;
        line-height: 1.55;
        opacity: 0.92;
      }
      .nav-tabs {
        border-bottom: 0;
        margin-bottom: 18px;
        display: flex;
        gap: 7px;
        flex-wrap: wrap;
      }
      .nav-tabs > li {
        float: none;
        margin: 0;
      }
      .nav-tabs > li > a {
        margin: 0;
        padding: 10px 16px;
        border: 1px solid #c8d1da;
        border-radius: 999px;
        background: white;
        color: #00274C;
        font-weight: 600;
      }
      .nav-tabs > li > a:hover {
        border-color: #00274C;
        background: #f5f1dc;
      }
      .nav-tabs > li.active > a,
      .nav-tabs > li.active > a:hover,
      .nav-tabs > li.active > a:focus {
        border: 1px solid #FFCB05;
        background: #FFCB05;
        color: #00274C;
      }
      .tab-content {
        padding-top: 2px;
      }
      .dimple-card {
        margin-bottom: 20px;
        border: 1px solid #dce5eb;
        border-radius: 14px;
        background: white;
        box-shadow: 0 4px 18px rgba(37, 63, 80, 0.055);
        overflow: visible;
      }
      .dimple-card-header {
        padding: 18px 20px 12px 20px;
        border-bottom: 1px solid #eef2f5;
      }
      .dimple-card-header h3 {
        margin: 0;
        color: #00274C;
        font-size: 18px;
        font-weight: 700;
      }
      .card-subtitle {
        margin: 5px 0 0 0;
        color: #5a6a78;
        font-size: 13px;
        line-height: 1.45;
      }
      .dimple-card-body {
        padding: 18px 20px 20px 20px;
      }
      .control-label {
        color: #00274C;
        font-size: 13px;
        font-weight: 700;
      }
      .form-control {
        border-color: #cedae2;
        border-radius: 8px;
        box-shadow: none;
      }
      .form-control:focus {
        border-color: #00274C;
        box-shadow: 0 0 0 2px rgba(0, 39, 76, 0.12);
      }
      .btn-primary,
      .btn-default {
        border-radius: 8px;
        font-weight: 600;
      }
      .btn-primary {
        border-color: #00274C;
        background: #00274C;
      }
      .btn-primary:hover,
      .btn-primary:focus {
        border-color: #001F3D;
        background: #001F3D;
      }
      .btn-default {
        border-color: #cbd8e0;
        color: #00274C;
        background: #fff;
      }
      .btn-default:hover,
      .btn-default:focus {
        border-color: #FFCB05;
        color: #00274C;
        background: #fff8d6;
      }
      .upload-or {
        margin: 14px 0;
        display: flex;
        align-items: center;
        gap: 10px;
        color: #83929c;
        font-size: 12px;
        font-weight: 700;
        text-transform: uppercase;
        letter-spacing: .08em;
      }
      .upload-or:before,
      .upload-or:after {
        content: '';
        height: 1px;
        flex: 1;
        background: #e2e9ee;
      }
      .helper-text {
        margin-top: 6px;
        color: #748793;
        font-size: 12px;
        line-height: 1.45;
      }
      .status-box {
        margin-top: 16px;
        padding: 12px 14px;
        border-radius: 10px;
        background: #fff8d6;
        color: #00274C;
        font-size: 13px;
      }
      .metric-grid {
        display: grid;
        grid-template-columns: repeat(4, minmax(0, 1fr));
        gap: 12px;
        margin-bottom: 20px;
      }
      .metric-card {
        min-height: 96px;
        padding: 16px 18px;
        border: 1px solid #dce5eb;
        border-radius: 13px;
        background: white;
        box-shadow: 0 3px 14px rgba(37, 63, 80, 0.045);
      }
      .metric-label {
        display: block;
        color: #5a6a78;
        font-size: 12px;
        font-weight: 700;
        letter-spacing: .04em;
        text-transform: uppercase;
      }
      .metric-value {
        display: block;
        margin-top: 5px;
        color: #00274C;
        font-size: 27px;
        font-weight: 700;
      }
      .metric-detail {
        display: block;
        margin-top: 2px;
        color: #82919a;
        font-size: 11px;
      }
      .plot-actions {
        display: flex;
        justify-content: flex-end;
        margin-top: 8px;
      }
      .section-intro {
        margin-bottom: 18px;
        color: #4d5f6f;
        line-height: 1.55;
      }
      .notice {
        padding: 14px 16px;
        border: 1px solid #d8e5eb;
        border-radius: 10px;
        background: #f7fafb;
        color: #40566a;
      }
      .compute-disabled {
        opacity: 0.42;
        filter: grayscale(0.75);
      }
      .compute-disabled,
      .compute-disabled * {
        pointer-events: none !important;
        user-select: none;
      }
      .local-run-code {
        margin: 12px 0 0 0;
        padding: 12px 14px;
        border: 1px solid #d8e5eb;
        border-radius: 8px;
        background: #f5f7f9;
        color: #16324f;
        white-space: pre-wrap;
      }
      .table-scroll {
        overflow-x: auto;
      }
      .table > thead > tr > th {
        border-bottom: 1px solid #dce5eb;
        color: #00274C;
        font-size: 12px;
        text-transform: uppercase;
        letter-spacing: .03em;
      }
      .table > tbody > tr > td {
        border-top: 1px solid #edf1f4;
      }
      .about-logo {
        display: block;
        max-width: 360px;
        width: 100%;
        margin: 0 auto 16px auto;
      }
      .team-grid {
        display: grid;
        grid-template-columns: repeat(5, minmax(0, 1fr));
        gap: 14px;
      }
      .team-member {
        padding: 14px;
        text-align: center;
        border: 1px solid #e0e7ec;
        border-radius: 12px;
        background: #fafcfd;
      }
      .team-member img {
        width: 100%;
        max-width: 150px;
        border-radius: 10px;
      }
      .team-member strong {
        display: block;
        margin-top: 9px;
        color: #00274C;
        font-size: 13px;
      }
      @media (max-width: 991px) {
        .metric-grid { grid-template-columns: repeat(2, minmax(0, 1fr)); }
        .team-grid { grid-template-columns: repeat(2, minmax(0, 1fr)); }
      }
      @media (max-width: 600px) {
        .container-fluid { padding-left: 14px; padding-right: 14px; }
        .dimple-hero { padding: 24px 22px; border-radius: 12px; }
        .dimple-hero h1 { font-size: 28px; }
        .metric-grid { grid-template-columns: 1fr 1fr; }
        .team-grid { grid-template-columns: 1fr 1fr; }
      }
    "))
  ),

  div(
    class = "dimple-hero",
    span("DIMPLE: DIstance Matrices for MultiPLEx imaging", class = "eyebrow"),
    h1("DIMPLE"),
    p("Explore multiplex imaging experiments, visualize cell locations and intensity surfaces, compare cohort-level distance summaries, compute and export distance matrices, and inspect quantile-defined three-way spatial interactions—all in the browser.")
  ),

  tabsetPanel(
    id = "main_tabs",

    tabPanel(
      "Pairwise spatial cellular interactions",
      fluidRow(
        column(
          width = 4,
          app_card(
              "Load an experiment",
              "Upload a DIMPLE MltplxExperiment or start with the public lung cancer example.",
              h4("What is a MltplxExperiment?"),
              p(
                  "A MltplxExperiment is the core data object in the DIMPLE R package for organizing and analyzing multiplex imaging data across slides, including cell locations and types, spatial intensity and distance summaries, and associated patient or slide metadata. It uses the RDS binary format of R, and and can be loaded with readRDS('path/to/object.RDS').",
                  class = "helper-text"
              ),
              fileInput(
                  "file1",
                  "MltplxExperiment (.RDS)",
                  accept = c(".RDS", ".rds")
              ),
              div(class = "upload-or", "or"),
              actionButton(
                  "exampledata",
                  "Use lung cancer example",
                  class = "btn-primary btn-block"
              ),
              p(
                  "The example dataset is downloaded once from the DIMPLE GitHub release and cached locally.",
                  class = "helper-text"
              ),
              uiOutput("data_status")
          ),
          app_card(
            "Image controls",
            "Choose a slide and how you want to inspect its spatial structure.",
            selectInput("slide_ids_to_plot", "Slide", choices = ""),
            selectInput(
              "cell_types_to_plot",
              "Cell types for intensity surface",
              choices = "",
              multiple = TRUE
            ),
            selectInput(
              "dm_plot_mode",
              "Distance matrix view",
              choices = c("Heatmap" = "heatmap", "Network" = "network"),
              selected = "heatmap"
            )
          )
        ),
        column(
          width = 8,
          uiOutput("experiment_overview"),
          plot_card(
            "Cell locations",
            "Observed cell locations for the selected slide.",
            "ppplot",
            "save_pp"
          ),
          plot_card(
            "Cell intensity surface",
            "Smoothed spatial intensity for the selected cell types.",
            "intensity_plot",
            "save_int"
          ),
          plot_card(
            "Distance matrix",
            "Pairwise cell-type distances for the selected slide.",
            "dm_plot",
            "save_dm"
          )
        )
      )
    ),

    tabPanel(
      "Compute spatial distances",
      p(
        "Build or update the active DIMPLE experiment, compute spatial distance metrics, and export the updated results. This tab is for processing only; visual summaries are available in the other tabs.",
        class = "section-intro"
      ),
      if (!full_app) {
        app_card(
          "Full processing is disabled in the online app",
          "Raw-data quantification and spatial-distance computation can be computationally intensive. Install DIMPLE locally and run the full app in RStudio to enable these controls.",
          p(
            "DIMPLE and its required R package dependencies are installed automatically when the package is installed. An internet connection is required for installation and the first use of the lung cancer example; after that, the application can run locally without repeatedly downloading those resources.",
            class = "helper-text"
          ),
          tags$pre(
            class = "local-run-code",
            'install.packages("pak")\npak::pkg_install("bayesrx/DIMPLE")\n\nlibrary(DIMPLE)\nshiny_app(full_app = TRUE)'
          )
        )
      },
      div(
        class = if (!full_app) "compute-disabled" else NULL,
        fluidRow(
          column(
            width = 4,
            app_card(
              "1. Raw data",
              "Upload both files to build a new MltplxExperiment. Leave both blank to recompute the experiment already loaded in the app.",
              fileInput(
                "compute_cell_file",
                "Cell data (CSV, TSV, XLSX)",
                accept = c(".csv", ".tsv", ".xlsx")
              ),
              fileInput(
                "compute_meta_file",
                "Metadata (CSV, TSV, XLSX)",
                accept = c(".csv", ".tsv", ".xlsx")
              ),
              p(
                "Cell data must contain ROI/slide ID, X/Y coordinates, and a cell-type annotation. Metadata must contain one row per ROI/slide and a patient or sample ID.",
                class = "helper-text"
              )
            ),
            app_card(
              "2. Column mapping",
              "Match the required DIMPLE fields to columns in the uploaded raw files.",
              h4("Cell data"),
              uiOutput("compute_col_roi_id_ui"),
              uiOutput("compute_col_x_ui"),
              uiOutput("compute_col_y_ui"),
              uiOutput("compute_col_cell_type_ui"),
              tags$hr(),
              h4("Metadata"),
              uiOutput("compute_col_meta_roi_id_ui"),
              uiOutput("compute_col_patient_id_ui")
            )
          ),
          column(
            width = 4,
            app_card(
              "3. Spatial processing",
              "Set KDE/intensity parameters, the pairwise metric, and optional quantile-distance processing.",
              uiOutput("distance_intensity_parameters"),
              selectInput(
                "distance_metric",
                "Distance metric",
                choices = c(
                  "Jensen-Shannon distance" = "jsd",
                  "Correlation" = "cor"
                ),
                selected = "jsd"
              ),
              checkboxInput(
                "compute_quantile_distances",
                "Compute quantile-specific distances",
                value = TRUE
              ),
              conditionalPanel(
                condition = "input.compute_quantile_distances",
                numericInput(
                  "compute_n_bins",
                  "Number of quantile bins",
                  value = 3,
                  min = 2,
                  max = 10,
                  step = 1
                ),
                uiOutput("compute_mask_type_select")
              ),
              actionButton(
                "compute_distances",
                "Process and update experiment",
                class = "btn-primary btn-block"
              ),
              uiOutput("distance_compute_status")
            )
          ),
          column(
            width = 4,
            app_card(
              "4. Export updated data",
              "The processed object replaces the active experiment in this app. Export the complete object or its pairwise distance table for downstream analysis.",
              downloadButton(
                "download_updated_experiment",
                "Download updated experiment (.RDS)",
                class = "btn btn-default btn-block"
              ),
              br(),
              downloadButton(
                "download_distances",
                "Download distances (.CSV)",
                class = "btn btn-default btn-block"
              )
            )
          )
        )
      )
    ),
    
    tabPanel(
        "Cohort-level summary",
        p(
            "Compare pairwise cell-type distances across cohort subgroups defined by the metadata attached to the experiment.",
            class = "section-intro"
        ),
        uiOutput("cohort_notice"),
        uiOutput("cohort_overview"),
        fluidRow(
            column(
                width = 4,
                app_card(
                    "Choose a cell-type pair",
                    "Select the pairwise distance to summarize and a metadata variable for stratification.",
                    selectInput(
                        "cohort_pair",
                        "Cell-type pair",
                        choices = ""
                    ),
                    selectInput(
                        "cohort_group",
                        "Stratify by",
                        choices = c("None" = "")
                    ),
                    p(
                        "When patient_id is available, distances from multiple slides are averaged within patient before plotting to avoid counting the same patient multiple times.",
                        class = "helper-text"
                    )
                ),
                app_card(
                    "Distance summary",
                    "Summary statistics for the selected pair, overall or within the chosen cohort strata.",
                    div(class = "table-scroll", tableOutput("cohort_distance_summary"))
                ),
                app_card(
                    "Metadata preview",
                    "First rows of the metadata attached to the experiment.",
                    div(class = "table-scroll", tableOutput("metadata_preview"))
                )
            ),
            column(
                width = 8,
                plot_card(
                    "Pairwise distance by cohort",
                    "Box plots compare the selected cell-type distance across the chosen metadata stratification.",
                    "cohort_plot",
                    "save_cohort_plot",
                    height = "470px"
                ),
                app_card(
                    "Pairwise regression screen",
                    "Test every pairwise cell-type distance for association with a two-level patient metadata covariate. The descriptive cohort plot stays first; this inferential screen follows directly underneath it.",
                    uiOutput("cohort_regression_notice"),
                    fluidRow(
                        column(
                            width = 4,
                            selectInput(
                                "regression_group_factor",
                                "Covariate to test",
                                choices = c("Select a two-level covariate" = "")
                            ),
                            selectInput(
                                "regression_covariates",
                                "Covariates to adjust for",
                                choices = character(0),
                                multiple = TRUE
                            ),
                            selectInput(
                                "regression_agg",
                                "Patient-level aggregation",
                                choices = c(
                                    "Median" = "median",
                                    "Mean" = "mean",
                                    "Maximum" = "max",
                                    "Minimum" = "min"
                                ),
                                selected = "median"
                            ),
                            selectInput(
                                "regression_adjust_counts",
                                "Adjust for cell-type counts",
                                choices = c("Yes" = "yes", "No" = "no"),
                                selected = "yes"
                            ),
                            p(
                                "For patients represented by multiple slides, the selected aggregation function combines slide-level distances before each pairwise linear model is fit. P-values shown in the heatmap are FDR adjusted.",
                                class = "helper-text"
                            )
                        ),
                        column(
                            width = 8,
                            plotOutput("cohort_regression_heatmap", height = "520px"),
                            div(
                                class = "plot-actions",
                                downloadButton(
                                    "save_cohort_regression_heatmap",
                                    "Download PDF",
                                    class = "btn btn-default btn-sm"
                                )
                            )
                        )
                    )
                )
            )
        )
    ),

    tabPanel(
      "Three-way interactions",
      p(
        "Inspect spatial patterns and distance matrices within quantile-defined tissue regions. This view remains descriptive and does not fit regression models.",
        class = "section-intro"
      ),
      fluidRow(
        column(
          width = 4,
          app_card(
            "Quantile controls",
            "Select a slide and visualization mode for its quantile-specific spatial summaries.",
            selectInput("slide_ids_to_plot_mask", "Slide", choices = ""),
            selectInput(
              "dm_plot_mode_qdist",
              "Distance matrix view",
              choices = c("Heatmap" = "heatmap", "Network" = "network"),
              selected = "heatmap"
            ),
            uiOutput("quantile_notice")
          )
        ),
        column(
          width = 8,
          plot_card(
            "Quantile intensity regions",
            "Spatial regions defined by the experiment's quantile mask.",
            "quantile_mask_plot",
            "save_mask"
          ),
          plot_card(
            "Quantile-specific distance matrix",
            "Pairwise cell-type distances within the selected quantile regions.",
            "quantile_dm_plot",
            "save_qdm"
          )
        )
      )
    ),

    tabPanel(
        "About",
        app_card(
            "About DIMPLE",
            NULL,
            
            p(
                "DIMPLE was developed by the MI-SPACE team ",
                tags$a(
                    "learn more about MI-SPACE here",
                    href = "https://sites.google.com/umich.edu/veerab/mi-space?authuser=0",
                    target = "_blank"
                ),
                "."
            ),
            
            p(
                "To read more about the DIMPLE methodology, see ",
                tags$a(
                    "the published methodology paper",
                    href = "https://www.sciencedirect.com/science/article/pii/S2666389923002714",
                    target = "_blank"
                ),
                ". Additional documentation, including a book chapter, will be linked here when available."
            ),
            
            p(
                "The source code for DIMPLE is available on ",
                tags$a(
                    "GitHub",
                    href = "https://github.com/bayesrx/DIMPLE",
                    target = "_blank"
                ),
                "."
            ),
            
            p(
                "For questions or issues running the app, contact ",
                tags$a(
                    "Michael Kleinsasser",
                    href = "mailto:mkleinsa@umich.edu"
                ),
                "."
            ),
            
            h3("Development team"),
            
            div(
                class = "team-grid",
                
                div(
                    class = "team-member",
                    tags$img(src = "maria.png", alt = "Maria Masotti, PhD"),
                    strong("Maria Masotti, PhD")
                ),
                
                div(
                    class = "team-member",
                    tags$img(src = "joel.png", alt = "Joel Eliason, PhD"),
                    strong("Joel Eliason, PhD")
                ),
                
                div(
                    class = "team-member",
                    tags$img(src = "veera.png", alt = "Veera Baladandayuthapani, PhD"),
                    strong("Veera Baladandayuthapani, PhD")
                ),
                
                div(
                    class = "team-member",
                    tags$img(src = "nate.png", alt = "Nate Osher, PhD"),
                    strong("Nate Osher, PhD")
                ),
                
                div(
                    class = "team-member",
                    tags$img(src = "arvind.png", alt = "Arvind Rao, PhD"),
                    strong("Arvind Rao, PhD")
                ),
                
                div(
                    class = "team-member",
                    tags$img(src = "mike.jpg", alt = "Michael Kleinsasser"),
                    strong("Michael Kleinsasser")
                ),
                
                div(
                    class = "team-member",
                    tags$img(src = "nick.png", alt = "Nicholas Lesniak, PhD"),
                    strong("Nicholas Lesniak, PhD")
                ),
                
                div(
                    class = "team-member",
                    tags$img(src = "andrew.jpg", alt = "Andrew Whiteman, PhD"),
                    strong("Andrew Whiteman, PhD")
                )
            )
        )
    )
  )
)
