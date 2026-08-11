library(shiny)
library(ggplot2)
library(dplyr)
library(tidyr)
library(purrr)
library(waiter)
library(DIMPLE)

options(shiny.maxRequestSize = 100000 * 1024^2)

function(input, output, session) {

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

  experiment <- reactive({
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

    if (!is.null(exp$metadata)) {
      metadata_names <- names(exp$metadata)
      identifier_names <- intersect(c("slide_id", "patient_id"), metadata_names)
      variable_choices <- setdiff(metadata_names, identifier_names)
      if (length(variable_choices) == 0) variable_choices <- metadata_names

      numeric_choices <- variable_choices[vapply(exp$metadata[variable_choices], is.numeric, logical(1))]
      default_variable <- if (length(numeric_choices) > 0) numeric_choices[[1]] else variable_choices[[1]]

      group_choices <- variable_choices[vapply(
        exp$metadata[variable_choices],
        function(x) {
          n_unique <- dplyr::n_distinct(x[!is.na(x)])
          is.factor(x) || is.character(x) || is.logical(x) || n_unique <= 10
        },
        logical(1)
      )]

      updateSelectInput(
        session,
        inputId = "cohort_variable",
        label = "Variable",
        choices = variable_choices,
        selected = default_variable
      )
      updateSelectInput(
        session,
        inputId = "cohort_group",
        label = "Optional stratification",
        choices = c("None" = "", group_choices),
        selected = ""
      )
    }

    exp
  })

  output$data_status <- renderUI({
    exp <- experiment()
    div(
      class = "status-box",
      strong("Experiment ready"),
      tags$br(),
      paste(format(length(exp$slide_ids), big.mark = ","), "slides loaded")
    )
  })

  output$experiment_overview <- renderUI({
    exp <- experiment()
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
    plot_dist_matrix(
      experiment(),
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
    req(exp$metadata)

    metadata <- as.data.frame(exp$metadata)
    if ("patient_id" %in% names(metadata)) {
      metadata <- dplyr::distinct(metadata, patient_id, .keep_all = TRUE)
    }
    metadata
  })

  output$cohort_notice <- renderUI({
    exp <- experiment()
    if (is.null(exp$metadata)) {
      div(
        class = "notice",
        strong("No cohort metadata is attached to this experiment."),
        tags$br(),
        "The image exploration tab is still available, but cohort summaries require metadata."
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

  output$cohort_variable_summary <- renderTable({
    data <- cohort_data()
    req(input$cohort_variable)
    req(input$cohort_variable %in% names(data))

    x <- data[[input$cohort_variable]]
    n_nonmissing <- sum(!is.na(x))
    n_missing <- sum(is.na(x))

    if (is.numeric(x)) {
      if (n_nonmissing == 0) {
        return(data.frame(Statistic = c("Non-missing", "Missing"), Value = c(0, n_missing)))
      }

      q <- stats::quantile(x, probs = c(0.25, 0.75), na.rm = TRUE, names = FALSE)
      values <- c(
        n_nonmissing,
        n_missing,
        mean(x, na.rm = TRUE),
        stats::sd(x, na.rm = TRUE),
        stats::median(x, na.rm = TRUE),
        q[[1]],
        q[[2]],
        min(x, na.rm = TRUE),
        max(x, na.rm = TRUE)
      )

      data.frame(
        Statistic = c("Non-missing", "Missing", "Mean", "SD", "Median", "Q1", "Q3", "Min", "Max"),
        Value = c(
          as.character(values[1:2]),
          format(round(values[3:9], 2), trim = TRUE, scientific = FALSE)
        ),
        check.names = FALSE
      )
    } else {
      x_chr <- as.character(x)
      x_chr[is.na(x_chr) | x_chr == ""] <- "Missing"
      counts <- sort(table(x_chr), decreasing = TRUE)
      data.frame(
        Level = names(counts),
        N = as.integer(counts),
        Percent = paste0(round(100 * as.integer(counts) / sum(counts), 1), "%"),
        check.names = FALSE
      )
    }
  }, striped = FALSE, bordered = FALSE, spacing = "s", rownames = FALSE)

  cohort_plot <- function() {
    data <- cohort_data()
    req(input$cohort_variable)
    req(input$cohort_variable %in% names(data))

    variable <- input$cohort_variable
    group <- input$cohort_group
    x <- data[[variable]]
    group_is_valid <- !is.null(group) && nzchar(group) && group %in% names(data) && group != variable

    if (is.numeric(x)) {
      plot_df <- data.frame(value = x)

      if (group_is_valid) {
        plot_df$group <- as.factor(data[[group]])
        plot_df <- plot_df[!is.na(plot_df$value) & !is.na(plot_df$group), , drop = FALSE]

        ggplot(plot_df, aes(x = group, y = value, fill = group)) +
          geom_boxplot(width = 0.58, alpha = 0.82, outlier.shape = NA) +
          geom_jitter(width = 0.11, alpha = 0.35, size = 1.8, colour = "#294656") +
          scale_fill_viridis_d(option = "C", end = 0.82) +
          labs(
            title = paste(variable, "by", group),
            subtitle = paste(format(nrow(plot_df), big.mark = ","), "patient-level observations"),
            x = group,
            y = variable
          ) +
          guides(fill = "none")
      } else {
        plot_df$cohort <- factor("All patients")
        plot_df <- plot_df[!is.na(plot_df$value), , drop = FALSE]

        ggplot(plot_df, aes(x = cohort, y = value)) +
          geom_boxplot(width = 0.34, fill = "#4f889d", colour = "#244b60", alpha = 0.86, outlier.shape = NA) +
          geom_jitter(width = 0.08, alpha = 0.38, size = 1.9, colour = "#294656") +
          labs(
            title = variable,
            subtitle = paste(format(nrow(plot_df), big.mark = ","), "patient-level observations"),
            x = NULL,
            y = variable
          )
      }
    } else {
      plot_df <- data.frame(category = as.character(x), stringsAsFactors = FALSE)
      plot_df$category[is.na(plot_df$category) | plot_df$category == ""] <- "Missing"

      if (group_is_valid) {
        plot_df$group <- as.factor(data[[group]])
        plot_df <- plot_df[!is.na(plot_df$group), , drop = FALSE]

        ggplot(plot_df, aes(x = category, fill = group)) +
          geom_bar(position = "dodge", width = 0.72) +
          scale_fill_viridis_d(option = "C", end = 0.82) +
          coord_flip() +
          labs(
            title = paste(variable, "by", group),
            subtitle = "Patient-level counts",
            x = NULL,
            y = "Patients",
            fill = group
          )
      } else {
        counts <- as.data.frame(table(plot_df$category), stringsAsFactors = FALSE)
        names(counts) <- c("category", "n")
        counts$category <- reorder(counts$category, counts$n)

        ggplot(counts, aes(x = category, y = n)) +
          geom_col(width = 0.7, fill = "#4f889d") +
          geom_text(aes(label = n), hjust = -0.18, size = 3.7, colour = "#405a69") +
          coord_flip(clip = "off") +
          scale_y_continuous(expand = expansion(mult = c(0, 0.12))) +
          labs(
            title = variable,
            subtitle = "Patient-level counts",
            x = NULL,
            y = "Patients"
          )
      }
    }
  }

  output$cohort_plot <- renderPlot({
    cohort_plot()
  })

  output$save_cohort_plot <- downloadHandler(
    filename = "cohort_summary.pdf",
    content = function(file) {
      ggsave(file, plot = cohort_plot(), width = 8, height = 6)
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

    plot_qdist_matrix(
      exp,
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
