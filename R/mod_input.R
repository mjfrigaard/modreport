#' Input Module UI
#'
#' @param id The module identifier
#'
#' @return A UI definition
#'
#' @export
mod_input_ui <- function(id) {
  ns <- NS(id)

  tagList(
    shiny::selectInput(
      ns("x_var"),
      "X Variable:",
      choices = c("bill_length_mm", "bill_depth_mm", "flipper_length_mm", "body_mass_g"),
      selected = "bill_length_mm"
    ),
    shiny::selectInput(
      ns("y_var"),
      "Y Variable:",
      choices = c("bill_length_mm", "bill_depth_mm", "flipper_length_mm", "body_mass_g"),
      selected = "body_mass_g"
    ),
    shiny::radioButtons(
      ns("plot_type"),
      "Plot Type:",
      choices = c("Scatter" = "scatter", "Box" = "box"),
      selected = "scatter"
    ),
    shiny::checkboxGroupInput(
      ns("species"),
      "Species:",
      choices = c("Adelie", "Chinstrap", "Gentoo"),
      selected = c("Adelie", "Chinstrap", "Gentoo")
    ),
    shiny::checkboxGroupInput(
      ns("islands"),
      "Islands:",
      choices = c("Biscoe", "Dream", "Torgersen"),
      selected = c("Biscoe", "Dream", "Torgersen")
    )
  )
}

#' Input Module Server
#'
#' @param id The module identifier
#' @param data The Palmer Penguins dataset
#'
#' @importFrom shiny moduleServer reactive
#'
#' @return A reactive list of user inputs
#' @export
mod_input_server <- function(id, data) {
  shiny::moduleServer(id, function(input, output, session) {
    # Set up module-specific logger
    logger::log_layout(logger::layout_glue_colors)
    logger_id <- paste0("input_module_", id)
    logger::log_threshold(logger::INFO)

    logger::log_info("Initializing input module", logger = logger_id)

    # Create a reactive list to store all user inputs
    input_values <- shiny::reactive({
      inputs <- list(
        x_var = input$x_var,
        y_var = input$y_var,
        plot_type = input$plot_type,
        species = input$species,
        islands = input$islands
      )

      logger::log_debug("Input values updated: x_var={inputs$x_var}, y_var={inputs$y_var}, plot_type={inputs$plot_type}",
        logger = logger_id
      )
      logger::log_debug("Selected species: {paste(inputs$species, collapse=', ')}",
        logger = logger_id
      )
      logger::log_debug("Selected islands: {paste(inputs$islands, collapse=', ')}",
        logger = logger_id
      )

      return(inputs)
    })

    # Log when inputs change
    observe({
      vals <- input_values()
      logger::log_info("Inputs changed: x_var={vals$x_var}, y_var={vals$y_var}, plot_type={vals$plot_type}",
        logger = logger_id
      )
    })

    return(input_values)
  })
}
