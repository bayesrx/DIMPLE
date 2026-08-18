#' Run the DIMPLE Shiny application
#'
#' Launch the Shiny application bundled with DIMPLE.
#'
#' @param full_app Logical; if `TRUE`, enable the computationally intensive
#'   raw-data processing and spatial-distance workflow. Set to `FALSE` for
#'   lightweight/hosted deployments.
#' @param ... Additional arguments passed to [shiny::runApp()].
#'
#' @return The value returned by [shiny::runApp()]. The function normally
#'   blocks while the application is running.
#' @export
#'
#' @examples
#' \dontrun{
#' shiny_app(full_app = TRUE)
#' }
shiny_app <- function(full_app = TRUE, ...) {
  app_dir <- system.file("shiny", package = "DIMPLE")

  if (!nzchar(app_dir)) {
    stop("The bundled DIMPLE Shiny application could not be found.", call. = FALSE)
  }

  app_dependencies <- c(
    shiny = requireNamespace("shiny", quietly = TRUE),
    shinythemes = requireNamespace("shinythemes", quietly = TRUE),
    waiter = requireNamespace("waiter", quietly = TRUE),
    shinyjs = requireNamespace("shinyjs", quietly = TRUE),
    digest = requireNamespace("digest", quietly = TRUE),
    readxl = !isTRUE(full_app) || requireNamespace("readxl", quietly = TRUE)
  )

  if (!all(app_dependencies)) {
    missing <- names(app_dependencies)[!app_dependencies]
    stop(
      "Missing package dependencies for the DIMPLE Shiny app: ",
      paste(missing, collapse = ", "),
      call. = FALSE
    )
  }

  old_options <- options(DIMPLE.full_app = isTRUE(full_app))
  on.exit(options(old_options), add = TRUE)

  shiny::runApp(appDir = app_dir, ...)
}
