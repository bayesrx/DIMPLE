library(shiny)
library(ggplot2)
library(dplyr)
library(tidyr)
library(purrr)
library(waiter)
library(DIMPLE)

options(shiny.maxRequestSize = 100000 * 1024^2)

function(input, output, session) {

  full_app <- isTRUE(getOption("DIMPLE.full_app", FALSE))

  theme_set(
    theme_minimal(base_size = 13) +
      theme(
        plot.title = element_text(face = "bold", colour = "#16324f", size = 15),
        plot.subtitle = element_text(colour = "#6d7e8a", size = 11),
        axis.title = element_text(face = "bold", colour = "#405a69"),
        axis.text = element_text(colour = "#536d7c"),
        legend.position = "right",
        legend.title = element_text(face = "bold"),
        panel.grid.minor = element_blank(),
        panel.grid.major = element_line(colour = "#e8eef2", linewidth = 0.35),
        plot.background = element_rect(fill = "white", colour = NA),
        panel.background = element_rect(fill = "white", colour = NA)
      )
  )

  get_cell_types <- function(exp) {
    sort(unique(unlist(lapply(exp$mltplx_objects, function(obj) {
      obj$mltplx_image$cell_types
    }))))
  }

  has_quantile_dist <- function(exp) {
    length(exp$mltplx_objects) > 0 &&
      !is.null(exp$mltplx_objects[[1]]$quantile_dist)
  }

  metric_card <- function(label, value, detail = NULL) {
    div(
      class = "metric-card",
      span(label, class = "metric-label"),
      span(value, class = "metric-value"),
      if (!is.null(detail)) span(detail, class = "metric-detail")
    )
  }

  has_intensities <- function(exp) {
    length(exp$mltplx_objects) > 0 &&
      !is.null(exp$ps) &&
      !is.null(exp$bw) &&
      all(vapply(exp$mltplx_objects, function(obj) {
        !is.null(obj$mltplx_intensity)
      }, logical(1)))
  }

  experiment <- reactiveVal(NULL)

  loaded_experiment <- reactive({
    if (!is.null(input$file1)) {
      inFile <- input$file1
      exp <- readRDS(inFile$datapath)
    } else {
      req(input$exampledata)

      lung_url <- "https://github.com/bayesrx/DIMPLE/releases/download/lung-data-v1/lung_experiment_10_30_jsd_qdist.RDS"
      lung_sha256 <- "75a4beaac18d0dc0789507b491061d4a270a674e76cf9c34b6eea8f7c72e583b"
      cache_dir <- file.path(tools::R_user_dir("DIMPLE", which = "cache"), "shiny")
      lung_file <- file.path(cache_dir, "lung_experiment_10_30_jsd_qdist.RDS")

      if (!file.exists(lung_file)) {
        dir.create(cache_dir, recursive = TRUE, showWarnings = FALSE)
        temp_file <- tempfile(pattern = "dimple-lung-", tmpdir = cache_dir, fileext = ".RDS")
        on.exit(unlink(temp_file), add = TRUE)

        download_error <- NULL
        shiny::withProgress(message = "Downloading lung cancer example data...", value = 0, {
          shiny::incProgress(0.1, detail = "Downloading 121 MB data file")
          status <- tryCatch(
            utils::download.file(lung_url, temp_file, mode = "wb", quiet = TRUE),
            error = function(e) {
              download_error <<- conditionMessage(e)
              NA_integer_
            }
          )

          if (is.na(status) || status != 0L || !file.exists(temp_file)) {
            if (is.null(download_error)) {
              download_error <- paste0("download.file returned status ", status)
            }
            stop("Unable to download the lung example data: ", download_error, call. = FALSE)
          }

          shiny::incProgress(0.8, detail = "Verifying downloaded data")
          downloaded_sha256 <- digest::digest(
            temp_file,
            algo = "sha256",
            serialize = FALSE,
            file = TRUE
          )

          if (!identical(tolower(downloaded_sha256), tolower(lung_sha256))) {
            stop(
              "The downloaded lung example data failed SHA-256 verification. Please try again.",
              call. = FALSE
            )
          }

          if (!file.rename(temp_file, lung_file)) {
            stop("Unable to move the downloaded lung example data into the cache.", call. = FALSE)
          }
          shiny::incProgress(0.1, detail = "Done")
        })
      }

      exp <- readRDS(lung_file)
    }

    cell_types <- get_cell_types(exp)
    first_slide <- if (length(exp$slide_ids) > 0) exp$slide_ids[[1]] else ""

    updateSelectInput(
      session,
      inputId = "slide_ids_to_plot",
      label = "Slide",
      choices = exp$slide_ids,
      selected = first_slide
    )
    updateSelectInput(
      session,
      inputId = "slide_ids_to_plot_mask",
      label = "Slide",
      choices = exp$slide_ids,
      selected = first_slide
    )

    exp
  })

  observeEvent(loaded_experiment(), {
    experiment(loaded_experiment())
  }, ignoreInit = TRUE)

  output$data_status <- renderUI({
    exp <- experiment()
    if (is.null(exp)) {
      return(div(class = "notice", "No experiment is currently loaded."))
    }

    div(
      class = "status-box",
      strong("Experiment ready"),
      tags$br(),
      paste(format(length(exp$slide_ids), big.mark = ","), "slides loaded")
    )
  })

  observeEvent(experiment(), {
    exp <- experiment()
    req(exp)

    first_slide <- if (length(exp$slide_ids) > 0) exp$slide_ids[[1]] else ""
    updateSelectInput(
      session,
      inputId = "slide_ids_to_plot",
      label = "Slide",
      choices = exp$slide_ids,
      selected = first_slide
    )
    updateSelectInput(
      session,
      inputId = "slide_ids_to_plot_mask",
      label = "Slide",
      choices = exp$slide_ids,
      selected = first_slide
    )
  }, ignoreInit = TRUE)

  output$experiment_overview <- renderUI({
    exp <- experiment()
    req(exp)
    cell_types <- get_cell_types(exp)
    total_cells <- sum(vapply(exp$mltplx_objects, function(obj) obj$mltplx_image$ppp$n, numeric(1)))
    n_patients <- if (!is.null(exp$metadata) && "patient_id" %in% names(exp$metadata)) {
      dplyr::n_distinct(exp$metadata$patient_id)
    } else {
      NA_integer_
    }

    div(
      class = "metric-grid",
      metric_card("Slides", format(length(exp$slide_ids), big.mark = ",")),
      metric_card("Cells", format(total_cells, big.mark = ",")),
      metric_card("Cell types", format(length(cell_types), big.mark = ",")),
      metric_card(
        if (is.na(n_patients)) "Metadata" else "Patients",
        if (is.na(n_patients)) {
          if (is.null(exp$metadata)) "None" else format(nrow(exp$metadata), big.mark = ",")
        } else {
          format(n_patients, big.mark = ",")
        },
        if (is.na(n_patients) && !is.null(exp$metadata)) "metadata rows" else NULL
      )
    )
  })

  ppplot <- function() {
    req(experiment())
    req(input$slide_ids_to_plot)
    plot_ppp(experiment(), input$slide_ids_to_plot)
  }

  output$ppplot <- renderPlot({
    ppplot()
  })

  selected_experiment <- reactive({
    req(experiment())
    req(input$slide_ids_to_plot)
    exp1 <- filter_MltplxExp(experiment(), input$slide_ids_to_plot)
    cell_types <- get_cell_types(exp1)
    default_types <- if (length(cell_types) > 0) cell_types[[1]] else character(0)

    updateSelectInput(
      session,
      inputId = "cell_types_to_plot",
      label = "Cell types for intensity surface",
      choices = cell_types,
      selected = default_types
    )

    exp1
  })

  intensity_plot <- function() {
    req(selected_experiment())
    req(input$slide_ids_to_plot)
    req(input$cell_types_to_plot)
    plot_intensity_surface(
      experiment(),
      types = input$cell_types_to_plot,
      slide_ids = input$slide_ids_to_plot
    )
  }

  output$intensity_plot <- renderPlot({
    intensity_plot()
  })

  dm_plot <- function() {
    req(experiment())
    req(input$slide_ids_to_plot)
    req(input$dm_plot_mode)

    exp_to_plot <- experiment()

    # igraph does not accept adjacency matrices containing NA/NaN values.
    # For the network view, treat non-finite distances as missing edges by
    # replacing them with zero in a temporary copy used only for plotting.
    # The experiment object itself is not modified, and the heatmap continues
    # to display the original distance matrix with missing values omitted.
    if (identical(input$dm_plot_mode, "network")) {
      exp_to_plot$mltplx_objects <- lapply(exp_to_plot$mltplx_objects, function(obj) {
        if (!is.null(obj$mltplx_dist) && !is.null(obj$mltplx_dist$dist)) {
          dist_mat <- obj$mltplx_dist$dist
          dist_mat[!is.finite(dist_mat)] <- 0
          obj$mltplx_dist$dist <- dist_mat
        }
        obj
      })
    }

    plot_dist_matrix(
      exp_to_plot,
      input$slide_ids_to_plot,
      mode = input$dm_plot_mode
    )
  }

  output$dm_plot <- renderPlot({
    dm_plot()
  })

  output$save_pp <- downloadHandler(
    filename = "cell_locations.pdf",
    content = function(file) {
      ggsave(file, plot = ppplot(), width = 8, height = 6)
    }
  )

  output$save_int <- downloadHandler(
    filename = "intensity_surface.pdf",
    content = function(file) {
      ggsave(file, plot = intensity_plot(), width = 8, height = 6)
    }
  )

  output$save_dm <- downloadHandler(
    filename = "distance_matrix.pdf",
    content = function(file) {
      ggsave(file, plot = dm_plot(), width = 8, height = 6)
    }
  )

  cohort_data <- reactive({
    exp <- experiment()
    req(exp)
    req(exp$metadata)

    metadata <- as.data.frame(exp$metadata)
    if ("patient_id" %in% names(metadata)) {
      metadata <- dplyr::distinct(metadata, patient_id, .keep_all = TRUE)
    }
    metadata
  })

  cohort_pair_table <- reactive({
    exp <- experiment()
    req(exp)
    req(!is.null(exp$dist_metric_name))

    distances <- DIMPLE::dist_to_df(exp, reduce_symmetric = TRUE) %>%
      dplyr::mutate(
        type1 = as.character(type1),
        type2 = as.character(type2)
      ) %>%
      dplyr::filter(type1 != type2) %>%
      dplyr::distinct(type1, type2) %>%
      dplyr::arrange(type1, type2) %>%
      dplyr::mutate(
        pair_id = as.character(dplyr::row_number()),
        pair_label = paste(type1, type2, sep = " - ")
      )

    distances
  })

  observe({
    exp <- experiment()
    req(exp)

    if (!is.null(exp$metadata)) {
      metadata_names <- names(exp$metadata)
      identifier_names <- intersect(c("slide_id", "patient_id"), metadata_names)
      variable_choices <- setdiff(metadata_names, identifier_names)

      group_choices <- variable_choices[vapply(
        exp$metadata[variable_choices],
        function(x) {
          values <- x[!is.na(x)]
          n_unique <- dplyr::n_distinct(values)
          is.factor(x) || is.character(x) || is.logical(x) || n_unique <= 10
        },
        logical(1)
      )]

      current_group <- isolate(input$cohort_group)
      selected_group <- if (!is.null(current_group) && current_group %in% group_choices) {
        current_group
      } else if (length(group_choices) > 0) {
        group_choices[[1]]
      } else {
        ""
      }

      updateSelectInput(
        session,
        inputId = "cohort_group",
        label = "Stratify by",
        choices = c("None" = "", group_choices),
        selected = selected_group
      )
    }

    if (!is.null(exp$dist_metric_name)) {
      pairs <- cohort_pair_table()
      if (nrow(pairs) > 0) {
        pair_choices <- stats::setNames(pairs$pair_id, pairs$pair_label)
        current_pair <- isolate(input$cohort_pair)
        selected_pair <- if (!is.null(current_pair) && current_pair %in% pairs$pair_id) {
          current_pair
        } else {
          pairs$pair_id[[1]]
        }

        updateSelectInput(
          session,
          inputId = "cohort_pair",
          label = "Cell-type pair",
          choices = pair_choices,
          selected = selected_pair
        )
      } else {
        updateSelectInput(
          session,
          inputId = "cohort_pair",
          label = "Cell-type pair",
          choices = character(0),
          selected = character(0)
        )
      }
    }
  })

  output$cohort_notice <- renderUI({
    exp <- experiment()
    req(exp)

    if (is.null(exp$metadata)) {
      return(div(
        class = "notice",
        strong("No cohort metadata is attached to this experiment."),
        tags$br(),
        "Cohort comparisons require slide- or patient-level metadata."
      ))
    }

    if (is.null(exp$dist_metric_name)) {
      return(div(
        class = "notice",
        strong("No distance matrices are available."),
        tags$br(),
        "Use the Compute Distances tab to generate pairwise distances before viewing cohort summaries."
      ))
    }

    pairs <- cohort_pair_table()
    if (nrow(pairs) == 0) {
      div(
        class = "notice",
        "No between-cell-type distance pairs are available in this experiment."
      )
    }
  })

  output$cohort_overview <- renderUI({
    exp <- experiment()
    req(exp$metadata)
    cohort <- cohort_data()
    n_patients <- if ("patient_id" %in% names(exp$metadata)) {
      dplyr::n_distinct(exp$metadata$patient_id)
    } else {
      nrow(cohort)
    }
    missing_cells <- sum(is.na(cohort))
    total_cells <- nrow(cohort) * ncol(cohort)
    completeness <- if (total_cells > 0) 100 * (1 - missing_cells / total_cells) else 100

    div(
      class = "metric-grid",
      metric_card("Patients", format(n_patients, big.mark = ","), if (!"patient_id" %in% names(exp$metadata)) "metadata records" else NULL),
      metric_card("Slides", format(length(exp$slide_ids), big.mark = ",")),
      metric_card("Metadata fields", format(ncol(exp$metadata), big.mark = ",")),
      metric_card("Completeness", paste0(format(round(completeness, 1), nsmall = 1), "%"), "non-missing patient-level values")
    )
  })

  output$metadata_preview <- renderTable({
    exp <- experiment()
    req(exp$metadata)
    utils::head(exp$metadata, 8)
  }, striped = FALSE, bordered = FALSE, spacing = "s", rownames = FALSE)

  cohort_distance_data <- reactive({
    exp <- experiment()
    req(exp)
    req(exp$metadata)
    req(!is.null(exp$dist_metric_name))
    req(input$cohort_pair)

    pairs <- cohort_pair_table()
    pair <- pairs[pairs$pair_id == input$cohort_pair, , drop = FALSE]
    req(nrow(pair) == 1)

    grouping_var <- input$cohort_group
    grouping_valid <- !is.null(grouping_var) && nzchar(grouping_var) && grouping_var %in% names(exp$metadata)

    data <- DIMPLE::dist_to_df(exp, reduce_symmetric = TRUE) %>%
      dplyr::mutate(
        type1 = as.character(type1),
        type2 = as.character(type2)
      ) %>%
      dplyr::filter(
        type1 == pair$type1[[1]],
        type2 == pair$type2[[1]],
        is.finite(dist)
      )

    if ("patient_id" %in% names(data)) {
      if (grouping_valid) {
        data <- data %>%
          dplyr::filter(!is.na(.data[[grouping_var]])) %>%
          dplyr::group_by(patient_id, .data[[grouping_var]]) %>%
          dplyr::summarise(dist = mean(dist, na.rm = TRUE), .groups = "drop")
      } else {
        data <- data %>%
          dplyr::group_by(patient_id) %>%
          dplyr::summarise(dist = mean(dist, na.rm = TRUE), .groups = "drop")
      }
      attr(data, "observation_unit") <- "patients"
    } else {
      attr(data, "observation_unit") <- "slides"
    }

    attr(data, "type1") <- pair$type1[[1]]
    attr(data, "type2") <- pair$type2[[1]]
    attr(data, "grouping_var") <- if (grouping_valid) grouping_var else NULL
    data
  })

  output$cohort_distance_summary <- renderTable({
    data <- cohort_distance_data()
    validate(need(nrow(data) > 0, "No distance observations are available for this pair."))
    grouping_var <- attr(data, "grouping_var")
    observation_unit <- attr(data, "observation_unit")

    summarize_distances <- function(df) {
      q <- stats::quantile(df$dist, probs = c(0.25, 0.75), na.rm = TRUE, names = FALSE)
      dplyr::tibble(
        N = nrow(df),
        Mean = mean(df$dist, na.rm = TRUE),
        SD = stats::sd(df$dist, na.rm = TRUE),
        Median = stats::median(df$dist, na.rm = TRUE),
        Q1 = q[[1]],
        Q3 = q[[2]]
      )
    }

    if (!is.null(grouping_var)) {
      result <- data %>%
        dplyr::group_by(.data[[grouping_var]]) %>%
        dplyr::group_modify(~ summarize_distances(.x)) %>%
        dplyr::ungroup()
      names(result)[[1]] <- grouping_var
    } else {
      result <- summarize_distances(data)
    }

    numeric_cols <- vapply(result, is.numeric, logical(1))
    numeric_names <- names(result)[numeric_cols]
    numeric_names <- setdiff(numeric_names, "N")
    result[numeric_names] <- lapply(result[numeric_names], function(x) round(x, 3))
    names(result)[names(result) == "N"] <- paste0("N ", observation_unit)
    result
  }, striped = FALSE, bordered = FALSE, spacing = "s", rownames = FALSE)

  cohort_plot <- function() {
    exp <- experiment()
    data <- cohort_distance_data()
    type1 <- attr(data, "type1")
    type2 <- attr(data, "type2")
    grouping_var <- attr(data, "grouping_var")
    observation_unit <- attr(data, "observation_unit")

    validate(need(nrow(data) > 0, "No distance observations are available for this pair."))

    if (!is.null(grouping_var)) {
      plot_df <- data %>%
        dplyr::filter(!is.na(.data[[grouping_var]])) %>%
        dplyr::mutate(.cohort_group = as.factor(.data[[grouping_var]]))

      validate(need(nrow(plot_df) > 0, "No observations are available for this stratification."))

      ggplot(plot_df, aes(x = .cohort_group, y = dist, fill = .cohort_group)) +
        geom_boxplot(width = 0.58, alpha = 0.82, outlier.shape = NA) +
        geom_jitter(width = 0.11, alpha = 0.35, size = 1.8, colour = "#294656") +
        scale_fill_viridis_d(option = "C", end = 0.82) +
        labs(
          title = paste(type1, "and", type2, "distance by", grouping_var),
          subtitle = paste(format(nrow(plot_df), big.mark = ","), observation_unit, "shown"),
          x = grouping_var,
          y = paste0("Distance", if (!is.null(exp$dist_metric_name)) paste0(" (", exp$dist_metric_name, ")") else "")
        ) +
        guides(fill = "none")
    } else {
      plot_df <- data %>%
        dplyr::mutate(.cohort_group = factor("All observations"))

      ggplot(plot_df, aes(x = .cohort_group, y = dist)) +
        geom_boxplot(width = 0.34, fill = "#4f889d", colour = "#244b60", alpha = 0.86, outlier.shape = NA) +
        geom_jitter(width = 0.08, alpha = 0.38, size = 1.9, colour = "#294656") +
        labs(
          title = paste("Distance between", type1, "and", type2),
          subtitle = paste(format(nrow(plot_df), big.mark = ","), observation_unit, "shown"),
          x = NULL,
          y = paste0("Distance", if (!is.null(exp$dist_metric_name)) paste0(" (", exp$dist_metric_name, ")") else "")
        )
    }
  }

  output$cohort_plot <- renderPlot({
    cohort_plot()
  })

  output$save_cohort_plot <- downloadHandler(
    filename = "cohort_pairwise_distance.pdf",
    content = function(file) {
      ggsave(file, plot = cohort_plot(), width = 8, height = 6)
    }
  )

  read_compute_file <- function(file_input) {
    req(file_input)
    ext <- tolower(tools::file_ext(file_input$name))

    if (identical(ext, "csv")) {
      return(utils::read.csv(file_input$datapath, check.names = FALSE))
    }
    if (identical(ext, "tsv")) {
      return(utils::read.delim(file_input$datapath, check.names = FALSE))
    }
    if (identical(ext, "xlsx")) {
      return(as.data.frame(readxl::read_excel(file_input$datapath)))
    }

    stop("Unsupported file type. Please upload CSV, TSV, or XLSX files.", call. = FALSE)
  }

  guess_compute_col <- function(col_names, pattern) {
    matches <- col_names[grepl(pattern, col_names, ignore.case = TRUE)]
    if (length(matches) > 0) matches[[1]] else ""
  }

  compute_cell_data <- reactive({
    req(input$compute_cell_file)
    read_compute_file(input$compute_cell_file)
  })

  compute_meta_data <- reactive({
    req(input$compute_meta_file)
    read_compute_file(input$compute_meta_file)
  })

  output$compute_col_roi_id_ui <- renderUI({
    req(compute_cell_data())
    cols <- names(compute_cell_data())
    selectInput(
      "compute_col_roi_id",
      "ROI / slide ID",
      choices = c("", cols),
      selected = guess_compute_col(cols, "image|roi|phenotype|core|slide")
    )
  })

  output$compute_col_x_ui <- renderUI({
    req(compute_cell_data())
    cols <- names(compute_cell_data())
    selectInput(
      "compute_col_x",
      "X coordinate",
      choices = c("", cols),
      selected = guess_compute_col(cols, "(^x$)|x.*coord|coord.*x")
    )
  })

  output$compute_col_y_ui <- renderUI({
    req(compute_cell_data())
    cols <- names(compute_cell_data())
    selectInput(
      "compute_col_y",
      "Y coordinate",
      choices = c("", cols),
      selected = guess_compute_col(cols, "(^y$)|y.*coord|coord.*y")
    )
  })

  output$compute_col_cell_type_ui <- renderUI({
    req(compute_cell_data())
    cols <- names(compute_cell_data())
    selectInput(
      "compute_col_cell_type",
      "Cell type",
      choices = c("", cols),
      selected = guess_compute_col(cols, "cell.*type|type|phenotype|marker|class")
    )
  })

  output$compute_col_meta_roi_id_ui <- renderUI({
    req(compute_meta_data())
    cols <- names(compute_meta_data())
    selectInput(
      "compute_col_meta_roi_id",
      "ROI / slide ID",
      choices = c("", cols),
      selected = guess_compute_col(cols, "image|roi|phenotype|core|slide")
    )
  })

  output$compute_col_patient_id_ui <- renderUI({
    req(compute_meta_data())
    cols <- names(compute_meta_data())
    selectInput(
      "compute_col_patient_id",
      "Patient / sample ID",
      choices = c("", cols),
      selected = guess_compute_col(cols, "patient|participant|subject|sample")
    )
  })

  output$distance_intensity_parameters <- renderUI({
    exp <- experiment()
    ps_default <- if (!is.null(exp) && !is.null(exp$ps)) exp$ps else 30
    bw_default <- if (!is.null(exp) && !is.null(exp$bw)) exp$bw else 40

    tagList(
      numericInput(
        "distance_ps",
        "Pixel size (ps)",
        value = ps_default,
        min = 1,
        step = 1
      ),
      numericInput(
        "distance_bw",
        "Smoothing bandwidth (bw)",
        value = bw_default,
        min = 1,
        step = 1
      )
    )
  })

  output$compute_mask_type_select <- renderUI({
    cell_types <- character(0)

    if (!is.null(input$compute_cell_file) &&
        !is.null(input$compute_col_cell_type) &&
        nzchar(input$compute_col_cell_type)) {
      cd <- compute_cell_data()
      cell_types <- sort(unique(as.character(cd[[input$compute_col_cell_type]])))
      cell_types <- cell_types[!is.na(cell_types) & nzchar(cell_types)]
    } else {
      exp <- experiment()
      if (!is.null(exp)) cell_types <- get_cell_types(exp)
    }

    req(length(cell_types) > 0)
    selectInput(
      "compute_mask_type",
      "Quantile mask cell type",
      choices = cell_types,
      selected = cell_types[[1]]
    )
  })

  compute_q_probs <- reactive({
    n <- as.integer(input$compute_n_bins)
    breaks <- round(seq(0, 100, length.out = n + 1))
    tibble::tibble(
      from = breaks[-(n + 1)],
      to = breaks[-1]
    )
  })

  output$distance_compute_status <- renderUI({
    exp <- experiment()

    if (is.null(exp)) {
      return(div(
        class = "notice",
        "No active experiment yet. Upload raw cell and metadata files here, or load an RDS experiment from the first tab."
      ))
    }

    details <- c(
      paste(format(length(exp$slide_ids), big.mark = ","), "slides"),
      if (!is.null(exp$ps) && !is.null(exp$bw)) paste0("ps ", exp$ps, ", bw ", exp$bw) else NULL,
      if (!is.null(exp$dist_metric_name)) paste("metric", exp$dist_metric_name) else "no pairwise distances"
    )

    div(
      class = "status-box",
      strong("Active experiment"),
      tags$br(),
      paste(details, collapse = " · ")
    )
  })

  clear_quantile_distances <- function(exp) {
    exp$mltplx_objects <- lapply(exp$mltplx_objects, function(obj) {
      obj$quantile_dist <- NULL
      obj
    })
    exp$qdist_mask <- NULL
    exp$qdist_n_quantiles <- NULL
    exp
  }

  observeEvent(input$compute_distances, {
    if (!full_app) {
      showNotification(
        "Spatial processing is disabled in this deployment. Run shiny_app(full_app = TRUE) locally to enable it.",
        type = "warning",
        duration = NULL
      )
      return()
    }

    req(input$distance_ps, input$distance_bw, input$distance_metric)

    result <- tryCatch({
      withProgress(
        message = "Processing DIMPLE experiment...",
        value = 0,
        {
          has_cell_upload <- !is.null(input$compute_cell_file)
          has_meta_upload <- !is.null(input$compute_meta_file)

          if (xor(has_cell_upload, has_meta_upload)) {
            stop("Upload both the cell data file and metadata file, or leave both blank to update the currently loaded experiment.", call. = FALSE)
          }

          if (has_cell_upload && has_meta_upload) {
            cd <- compute_cell_data()
            md <- compute_meta_data()

            required_inputs <- list(
              input$compute_col_roi_id,
              input$compute_col_x,
              input$compute_col_y,
              input$compute_col_cell_type,
              input$compute_col_meta_roi_id,
              input$compute_col_patient_id
            )
            missing_required <- vapply(
              required_inputs,
              function(x) is.null(x) || length(x) != 1 || !nzchar(x),
              logical(1)
            )
            if (any(missing_required)) {
              stop("Select all required cell-data and metadata columns before processing.", call. = FALSE)
            }
            if (identical(input$compute_col_meta_roi_id, input$compute_col_patient_id)) {
              stop("The metadata ROI/slide ID and patient/sample ID must be different columns.", call. = FALSE)
            }

            x <- suppressWarnings(as.numeric(cd[[input$compute_col_x]]))
            y <- suppressWarnings(as.numeric(cd[[input$compute_col_y]]))
            if (anyNA(x) || anyNA(y)) {
              stop("The selected X and Y coordinate columns must contain numeric, non-missing values.", call. = FALSE)
            }

            marks <- factor(cd[[input$compute_col_cell_type]])
            slide_id <- cd[[input$compute_col_roi_id]]
            if (anyNA(marks) || anyNA(slide_id)) {
              stop("Cell type and ROI/slide ID columns cannot contain missing values.", call. = FALSE)
            }

            metadata <- as.data.frame(md, check.names = FALSE)
            if (!identical(input$compute_col_meta_roi_id, "slide_id") && "slide_id" %in% names(metadata)) {
              stop("Metadata already contains a column named 'slide_id'. Select that column as the ROI/slide ID or rename it before upload.", call. = FALSE)
            }
            if (!identical(input$compute_col_patient_id, "patient_id") && "patient_id" %in% names(metadata)) {
              stop("Metadata already contains a column named 'patient_id'. Select that column as the patient/sample ID or rename it before upload.", call. = FALSE)
            }
            names(metadata)[match(input$compute_col_meta_roi_id, names(metadata))] <- "slide_id"
            names(metadata)[match(input$compute_col_patient_id, names(metadata))] <- "patient_id"
            metadata <- tibble::as_tibble(metadata)

            incProgress(0.35, detail = "Building experiment and estimating spatial intensities")
            exp <- DIMPLE::new_MltplxExperiment(
              x = x,
              y = y,
              marks = marks,
              slide_id = slide_id,
              ps = input$distance_ps,
              bw = input$distance_bw,
              metadata = metadata
            )
          } else {
            exp <- experiment()
            if (is.null(exp)) {
              stop("Load an experiment or upload both raw input files before processing.", call. = FALSE)
            }

            same_intensity_settings <- has_intensities(exp) &&
              isTRUE(all.equal(as.numeric(exp$ps), as.numeric(input$distance_ps))) &&
              isTRUE(all.equal(as.numeric(exp$bw), as.numeric(input$distance_bw)))

            if (!same_intensity_settings) {
              incProgress(0.35, detail = "Estimating spatial intensities")
              exp <- DIMPLE::update_intensity(
                exp,
                ps = input$distance_ps,
                bw = input$distance_bw
              )
            } else {
              incProgress(0.35, detail = "Using existing spatial intensities")
            }
          }

          incProgress(0.35, detail = "Computing pairwise distances")
          if (identical(input$distance_metric, "jsd")) {
            exp <- DIMPLE::update_dist(exp, jsd)
          } else if (identical(input$distance_metric, "cor")) {
            exp <- DIMPLE::update_dist(exp, cor)
          } else {
            stop("Unsupported distance metric.", call. = FALSE)
          }

          if (isTRUE(input$compute_quantile_distances)) {
            req(input$compute_n_bins, input$compute_mask_type)
            incProgress(0.25, detail = "Computing quantile-specific distances")
            if (identical(input$distance_metric, "jsd")) {
              exp <- DIMPLE::update_qdist(
                exp,
                jsd,
                input$compute_mask_type,
                compute_q_probs(),
                .dist_metric_name = "jsd"
              )
            } else {
              exp <- DIMPLE::update_qdist(
                exp,
                cor,
                input$compute_mask_type,
                compute_q_probs(),
                .dist_metric_name = "cor"
              )
            }
          } else {
            exp <- clear_quantile_distances(exp)
            incProgress(0.25, detail = "Skipping quantile-specific distances")
          }

          incProgress(0.05, detail = "Updating active experiment")
          exp
        }
      )
    }, error = function(e) {
      showNotification(
        paste("Unable to process experiment:", conditionMessage(e)),
        type = "error",
        duration = NULL
      )
      NULL
    })

    if (!is.null(result)) {
      experiment(result)
      showNotification(
        "Processing complete. The active experiment has been updated.",
        type = "message"
      )
    }
  })

  output$download_updated_experiment <- downloadHandler(
    filename = function() {
      paste0("DIMPLE_experiment_", format(Sys.Date(), "%Y%m%d"), ".RDS")
    },
    content = function(file) {
      validate(need(full_app, "Full processing/export is disabled in this deployment."))
      exp <- experiment()
      req(exp)
      saveRDS(exp, file)
    }
  )

  output$download_distances <- downloadHandler(
    filename = function() {
      paste0("DIMPLE_distances_", Sys.Date(), ".csv")
    },
    content = function(file) {
      validate(need(full_app, "Full processing/export is disabled in this deployment."))
      exp <- experiment()
      req(exp)
      validate(need(
        !is.null(exp$dist_metric_name),
        "No distance matrices are available."
      ))

      distances <- DIMPLE::dist_to_df(
        exp,
        reduce_symmetric = TRUE
      )

      utils::write.csv(distances, file, row.names = FALSE)
    }
  )

  output$quantile_notice <- renderUI({
    exp <- experiment()
    if (!has_quantile_dist(exp)) {
      div(
        class = "notice",
        "This experiment does not contain quantile-specific distance information."
      )
    }
  })

  quantile_mask_plot <- function() {
    exp <- experiment()
    req(input$slide_ids_to_plot_mask)
    validate(need(has_quantile_dist(exp), "Quantile-specific distances are not available for this experiment."))

    qdist1 <- filter_MltplxExp(exp, input$slide_ids_to_plot_mask)$mltplx_objects[[1]]$quantile_dist
    plot_quantile_intensity_surface(
      exp,
      qdist1$mask_type,
      cbind.data.frame(from = c(qdist1$quantiles[, 3]), to = c(qdist1$quantiles[, 4])),
      input$slide_ids_to_plot_mask
    )
  }

  output$quantile_mask_plot <- renderPlot({
    quantile_mask_plot()
  })

  output$save_mask <- downloadHandler(
    filename = "quantile_mask.pdf",
    content = function(file) {
      ggsave(file, plot = quantile_mask_plot(), width = 8, height = 6)
    }
  )

  quantile_dm_plot <- function() {
    exp <- experiment()
    req(input$slide_ids_to_plot_mask)
    req(input$dm_plot_mode_qdist)
    validate(need(has_quantile_dist(exp), "Quantile-specific distances are not available for this experiment."))

    exp_to_plot <- exp

    # The quantile network view builds an igraph object from each slice of the
    # quantile distance array. igraph rejects adjacency matrices containing
    # NA/NaN/Inf values, so treat those entries as missing edges in a temporary
    # copy used only for network plotting. Heatmaps continue to use the original
    # quantile distances.
    if (identical(input$dm_plot_mode_qdist, "network")) {
      exp_to_plot$mltplx_objects <- lapply(exp_to_plot$mltplx_objects, function(obj) {
        # Some slides store quantile_dist as atomic NA when the mask cell type
        # is not present. Check that it is a list-like QuantileDist object
        # before using `$`; otherwise leave that slide untouched.
        if (is.list(obj$quantile_dist) &&
            !is.null(obj$quantile_dist$quantile_dist_array)) {
          qdist_arr <- obj$quantile_dist$quantile_dist_array
          qdist_arr[!is.finite(qdist_arr)] <- 0
          obj$quantile_dist$quantile_dist_array <- qdist_arr
        }
        obj
      })
    }

    plot_qdist_matrix(
      exp_to_plot,
      input$slide_ids_to_plot_mask,
      mode = input$dm_plot_mode_qdist
    )
  }

  output$quantile_dm_plot <- renderPlot({
    quantile_dm_plot()
  })

  output$save_qdm <- downloadHandler(
    filename = "quantile_distance_matrix.pdf",
    content = function(file) {
      ggsave(file, plot = quantile_dm_plot(), width = 8, height = 6)
    }
  )

  url <- a(
    "DIMPLE on GitHub",
    href = "https://github.com/bayesrx/DIMPLE",
    target = "_blank",
    rel = "noopener noreferrer"
  )

  output$tab <- renderUI({
    tagList(url)
  })
}
