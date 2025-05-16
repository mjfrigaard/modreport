#' Graph Output Module UI
#'
#' @param id The module identifier
#'
#' @return A UI definition
#'
#' @export
mod_graph_output_ui <- function(id) {
  ns <- NS(id)
  tagList(
    bslib::card(
      bslib::card_header("Penguin Plot"),
      bslib::card_body(
        plotOutput(ns("penguin_plot"), height = "400px")
      )
    )
  )
}

#' Graph Output Module Server
#'
#' @param id The module identifier
#' @param data The Palmer Penguins dataset
#' @param inputs A reactive list of user inputs
#'
#' @return NULL (outputs are rendered to the UI)
#'
#' @export
#'
mod_graph_output_server <- function(id, data, inputs) {
  moduleServer(id, function(input, output, session) {
    # Set up module-specific logger
    logger::log_layout(logger::layout_glue_colors)
    logger_id <- paste0("graph_module_", id)
    logger::log_threshold(logger::INFO)

    logger::log_info("Initializing graph output module", logger = logger_id)

    # Create a filtered dataset based on user inputs
    filtered_data <- reactive({
      req(inputs())

      logger::log_debug("Filtering data with x_var={inputs()$x_var}, y_var={inputs()$y_var}",
        logger = logger_id
      )
      logger::log_debug("Filtering species: {paste(inputs()$species, collapse=', ')}",
        logger = logger_id
      )
      logger::log_debug("Filtering islands: {paste(inputs()$islands, collapse=', ')}",
        logger = logger_id
      )

      filtered <- data |>
        dplyr::filter(
          species %in% inputs()$species,
          island %in% inputs()$islands
        ) |>
        tidyr::drop_na(all_of(c(inputs()$x_var, inputs()$y_var)))

      logger::log_info("Filtered data has {nrow(filtered)} rows", logger = logger_id)
      return(filtered)
    })

    # Render the plot
    output$penguin_plot <- renderPlot({
      req(filtered_data(), inputs())

      logger::log_info("Rendering plot with plot_type={inputs()$plot_type}",
        logger = logger_id
      )

      tryCatch({
          p <- ggplot2::ggplot(
            filtered_data(),
            ggplot2::aes_string(
              x = inputs()$x_var,
              y = inputs()$y_var,
              color = "species"
            )
          )

          if (inputs()$plot_type == "scatter") {
            p <- p + ggplot2::geom_point(size = 3, alpha = 0.8)
          } else {
            p <- p + ggplot2::geom_boxplot()
          }

          p <- p +
            ggplot2::labs(
              title = "Palmer Penguins Visualization",
              x = gsub("_", " ", stringr::str_to_title(inputs()$x_var)),
              y = gsub("_", " ", stringr::str_to_title(inputs()$y_var))
            ) +
            ggplot2::theme_minimal(base_size = 14)

          logger::log_info("Plot rendering complete", logger = logger_id)
          return(p)
        },
        error = function(e) {
          logger::log_error("Error rendering plot: {e$message}", logger = logger_id)
          # Return a simple error plot
          ggplot2::ggplot() +
            ggplot2::annotate("text",
              x = 0.5, y = 0.5,
              label = paste("Error rendering plot:", e$message)
            ) +
            ggplot2::theme_void()
        })
    })
  })
}
