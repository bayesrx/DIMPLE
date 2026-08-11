#' Run the DIMPLE Shiny application
#'
#' Launch the Shiny application bundled with DIMPLE.
#'
#' @param ... Additional arguments passed to [shiny::runApp()].
#'
#' @return The value returned by [shiny::runApp()]. The function normally
#'   blocks while the application is running.
#' @export
#'
#' @examples
#' \dontrun{
#' shiny_app()
#' }
shiny_app <- function(...) {
  app_dir <- system.file("shiny", package = "DIMPLE")

  if (!nzchar(app_dir)) {
    stop("The bundled DIMPLE Shiny application could not be found.", call. = FALSE)
  }

  app_dependencies <- c(
    shiny = requireNamespace("shiny", quietly = TRUE),
    shinythemes = requireNamespace("shinythemes", quietly = TRUE),
    waiter = requireNamespace("waiter", quietly = TRUE),
    shinyjs = requireNamespace("shinyjs", quietly = TRUE),
    here = requireNamespace("here", quietly = TRUE),
    spatstat = requireNamespace("spatstat", quietly = TRUE),
    devtools = requireNamespace("devtools", quietly = TRUE),
    ggpubr = requireNamespace("ggpubr", quietly = TRUE)
  )

  if (!all(app_dependencies)) {
    missing <- names(app_dependencies)[!app_dependencies]
    stop(
      "Missing package dependencies for the DIMPLE Shiny app: ",
      paste(missing, collapse = ", "),
      call. = FALSE
    )
  }

  shiny::runApp(appDir = app_dir, ...)
}
