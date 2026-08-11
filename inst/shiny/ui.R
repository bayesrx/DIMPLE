library(shiny)
library(shinythemes)
library(waiter)

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
        background: #f4f7fa;
        color: #263746;
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
        background: linear-gradient(120deg, #12324a 0%, #245a75 62%, #2f7785 100%);
        color: white;
        box-shadow: 0 10px 30px rgba(22, 50, 79, 0.16);
      }
      .dimple-hero .eyebrow {
        display: inline-block;
        margin-bottom: 6px;
        font-size: 12px;
        font-weight: 700;
        letter-spacing: 0.12em;
        text-transform: uppercase;
        opacity: 0.78;
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
        border: 1px solid #d8e1e8;
        border-radius: 999px;
        background: white;
        color: #496274;
        font-weight: 600;
      }
      .nav-tabs > li > a:hover {
        border-color: #9eb6c5;
        background: #eef4f7;
      }
      .nav-tabs > li.active > a,
      .nav-tabs > li.active > a:hover,
      .nav-tabs > li.active > a:focus {
        border: 1px solid #245a75;
        background: #245a75;
        color: white;
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
        overflow: hidden;
      }
      .dimple-card-header {
        padding: 18px 20px 12px 20px;
        border-bottom: 1px solid #eef2f5;
      }
      .dimple-card-header h3 {
        margin: 0;
        color: #16324f;
        font-size: 18px;
        font-weight: 700;
      }
      .card-subtitle {
        margin: 5px 0 0 0;
        color: #6d7e8a;
        font-size: 13px;
        line-height: 1.45;
      }
      .dimple-card-body {
        padding: 18px 20px 20px 20px;
      }
      .control-label {
        color: #314b5c;
        font-size: 13px;
        font-weight: 700;
      }
      .form-control {
        border-color: #cedae2;
        border-radius: 8px;
        box-shadow: none;
      }
      .form-control:focus {
        border-color: #4f889d;
        box-shadow: 0 0 0 2px rgba(79, 136, 157, 0.12);
      }
      .btn-primary,
      .btn-default {
        border-radius: 8px;
        font-weight: 600;
      }
      .btn-primary {
        border-color: #246078;
        background: #246078;
      }
      .btn-primary:hover,
      .btn-primary:focus {
        border-color: #194b60;
        background: #194b60;
      }
      .btn-default {
        border-color: #cbd8e0;
        color: #355466;
        background: #fff;
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
        background: #eef6f4;
        color: #315f58;
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
        color: #718491;
        font-size: 12px;
        font-weight: 700;
        letter-spacing: .04em;
        text-transform: uppercase;
      }
      .metric-value {
        display: block;
        margin-top: 5px;
        color: #16324f;
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
        color: #617783;
        line-height: 1.55;
      }
      .notice {
        padding: 14px 16px;
        border: 1px solid #d8e5eb;
        border-radius: 10px;
        background: #f7fafb;
        color: #536d7c;
      }
      .table-scroll {
        overflow-x: auto;
      }
      .table > thead > tr > th {
        border-bottom: 1px solid #dce5eb;
        color: #48606f;
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
        color: #294656;
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
    span("Multiplex spatial imaging", class = "eyebrow"),
    h1("DIMPLE"),
    p("DIstance Matrices for MultiPLEx imaging — explore spatial cell patterns, summarize cohort characteristics, and inspect quantile-specific relationships without leaving the browser.")
  ),

  tabsetPanel(
    id = "main_tabs",

    tabPanel(
      "Explore Images",
      fluidRow(
        column(
          width = 4,
          app_card(
            "Load an experiment",
            "Upload a DIMPLE MltplxExperiment or start with the public lung cancer example.",
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
      "Cohort Summary",
      p(
        "Explore the attached patient metadata descriptively. This section intentionally focuses on cohort composition and distributions rather than regression modeling.",
        class = "section-intro"
      ),
      uiOutput("cohort_notice"),
      uiOutput("cohort_overview"),
      fluidRow(
        column(
          width = 4,
          app_card(
            "Choose a cohort variable",
            "Numeric variables are shown with a box plot; categorical variables are shown as counts.",
            selectInput("cohort_variable", "Variable", choices = ""),
            selectInput(
              "cohort_group",
              "Optional stratification",
              choices = c("None" = "")
            ),
            p(
              "When patient_id is available, plots and summaries use one record per patient to avoid counting patients multiple times across slides.",
              class = "helper-text"
            )
          ),
          app_card(
            "Variable summary",
            "A compact descriptive summary of the selected cohort variable.",
            div(class = "table-scroll", tableOutput("cohort_variable_summary"))
          )
        ),
        column(
          width = 8,
          plot_card(
            "Cohort distribution",
            "A descriptive view of the selected patient-level variable.",
            "cohort_plot",
            "save_cohort_plot",
            height = "470px"
          ),
          app_card(
            "Metadata preview",
            "First rows of the metadata attached to the experiment.",
            div(class = "table-scroll", tableOutput("metadata_preview"))
          )
        )
      )
    ),

    tabPanel(
      "Quantile Distances",
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
        "About MI-SPACE",
        NULL,
        tags$img(src = "logo.png", class = "about-logo", alt = "MI-SPACE logo"),
        p(
          "MI-SPACE is an interdisciplinary group of faculty and students at the University of Michigan developing Multiplex Imaging based Spatial Analytic tools for discovery of cellular interactions in the tumor microenvironment."
        ),
        uiOutput("tab"),
        div(
          class = "team-grid",
          div(class = "team-member", tags$img(src = "maria.png", alt = "Maria Masotti"), strong("Maria Masotti, PhD")),
          div(class = "team-member", tags$img(src = "joel.png", alt = "Joel Eliason"), strong("Joel Eliason")),
          div(class = "team-member", tags$img(src = "veera.png", alt = "Veera Baladandayuthapani"), strong("Veera Baladandayuthapani, PhD")),
          div(class = "team-member", tags$img(src = "nate.png", alt = "Nate Osher"), strong("Nate Osher")),
          div(class = "team-member", tags$img(src = "arvind.png", alt = "Arvind Rao"), strong("Arvind Rao, PhD"))
        )
      )
    )
  )
)
