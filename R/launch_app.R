#' Launch the Palmer Penguins Shiny Application
#'
#' This function launches the Shiny application for exploring and
#' visualizing the Palmer Penguins dataset.
#'
#' @import shiny
#'
#' @return A Shiny application object
#'
#' @export
#'
#' @examples
#' if (interactive()) {
#'   launch_app()
#' }
launch_app <- function() {
  # Set up logger
  logger::log_layout(logger::layout_glue_colors)
  logger::log_threshold(logger::INFO)

  logger::log_info("Launching modreport application")

  # Create directory for the report template
  template_dir <- file.path(tempdir(), "inst", "rmd")
  dir.create(template_dir, recursive = TRUE, showWarnings = FALSE)

  # Create the template file
  template_path <- file.path(template_dir, "report_template.Rmd")
  if (!file.exists(template_path)) {
    logger::log_info("Creating report template at {template_path}")
    content <- create_report_template_content()
    writeLines(content, template_path)
  }

  # For development/testing, add the tempdir as a system file search path
  .libPaths(c(tempdir(), .libPaths()))

  logger::log_info("Starting Shiny app")
  shinyApp(ui = app_ui(), server = app_server)
}
