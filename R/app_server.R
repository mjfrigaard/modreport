#' Server for the Palmer Penguins Shiny App
#'
#' @param input Shiny input object
#' @param output Shiny output object
#' @param session Shiny session object
#'
#' @return A server function
#'
#' @export
app_server <- function(input, output, session) {
  # Set up app-level logger
  logger::log_layout(logger::layout_glue_colors)
  logger::log_threshold(logger::INFO)

  logger::log_info("Starting modreport application server")

  # Load the data
  tryCatch(
    {
      data <- palmerpenguins::penguins
      logger::log_info("Loaded penguins dataset with {nrow(data)} rows")
    },
    error = function(e) {
      logger::log_error("Error loading penguins dataset: {e$message}")
      # Provide a fallback empty dataframe with the right structure
      data <- data.frame(
        species = character(),
        island = character(),
        bill_length_mm = numeric(),
        bill_depth_mm = numeric(),
        flipper_length_mm = numeric(),
        body_mass_g = numeric(),
        sex = character(),
        year = numeric()
      )
    }
  )

  # Call input module to get user inputs
  logger::log_info("Initializing input module")
  selected_inputs <- mod_input_server("inputs", data)

  # Call output modules to render visualizations based on inputs
  logger::log_info("Initializing graph module")
  mod_graph_output_server("graph", data, selected_inputs)

  logger::log_info("Initializing table module")
  mod_table_output_server("table", data, selected_inputs)

  logger::log_info("Initializing report module")
  mod_download_report_server("report", data, selected_inputs)

  # Log when app is closed
  session$onSessionEnded(function() {
    logger::log_info("Application session ended")
  })
}
