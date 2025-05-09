#' Table Output Module UI
#'
#' @param id The module identifier
#'
#' @return A UI definition
#'
#' @export
mod_table_output_ui <- function(id) {
  ns <- NS(id)
  tagList(
    bslib::card(
      bslib::card_header("Penguin Data"),
      bslib::card_body(
        tableOutput(ns("penguin_table"))
      )
    )
  )
}

#' Table Output Module Server
#'
#' @param id The module identifier
#' @param data The Palmer Penguins dataset
#' @param inputs A reactive list of user inputs
#'
#' @return NULL (outputs are rendered to the UI)
#'
#' @export
#'
mod_table_output_server <- function(id, data, inputs) {
  moduleServer(id, function(input, output, session) {
    # Set up module-specific logger
    logger::log_layout(logger::layout_glue_colors)
    logger_id <- paste0("table_module_", id)
    logger::log_threshold(logger::INFO)

    logger::log_info("Initializing table output module", logger = logger_id)

    # Create a filtered dataset based on user inputs
    filtered_data <- reactive({
      req(inputs())

      logger::log_debug("Filtering data for table with x_var={inputs()$x_var}, y_var={inputs()$y_var}",
        logger = logger_id
      )

      filtered <- data |>
        dplyr::filter(
          species %in% inputs()$species,
          island %in% inputs()$islands
        ) |>
        tidyr::drop_na(all_of(c(inputs()$x_var, inputs()$y_var)))

      logger::log_info("Filtered data for table has {nrow(filtered)} rows",
        logger = logger_id
      )
      return(filtered)
    })

    # Render the table
    output$penguin_table <- renderTable({
      req(filtered_data(), inputs())

      logger::log_info("Rendering table with selected variables",
        logger = logger_id
      )

      tryCatch(
        {
          table_data <- filtered_data() |>
            dplyr::select(
              species,
              island,
              inputs()$x_var, inputs()$y_var
            ) |>
            dplyr::arrange(species, island) |>
            head(10) # Show only first 10 rows for better display

          logger::log_info("Table rendering complete with {nrow(table_data)} rows",
            logger = logger_id
          )
          return(table_data)
        },
        error = function(e) {
          logger::log_error("Error rendering table: {e$message}",
            logger = logger_id
          )
          # Return a simple error message dataframe
          data.frame(Error = paste("Error rendering table:", e$message))
        }
      )
    })
  })
}
