#' Download Report Module UI
#'
#' @param id The module identifier
#'
#' @return A UI definition
#'
#' @export
mod_download_report_ui <- function(id) {
  ns <- NS(id)
  tagList(
    bslib::card(
      bslib::card_header("Generate Report"),
      bslib::card_body(
        p("Create an HTML report with your current penguin visualization settings."),
        downloadButton(
          ns("download_report"),
          "Download HTML Report",
          class = "btn-primary"
        )
      )
    )
  )
}

#' Download Report Module Server
#'
#' @param id The module identifier
#' @param data The Palmer Penguins dataset
#' @param inputs A reactive list of user inputs
#'
#' @return NULL (outputs are rendered to the UI)
#'
#' @export
mod_download_report_server <- function(id, data, inputs) {
  moduleServer(id, function(input, output, session) {
    # Set up module-specific logger
    logger::log_layout(logger::layout_glue_colors)
    logger_id <- paste0("report_module_", id)
    logger::log_threshold(logger::INFO)

    output$download_report <- downloadHandler(
      filename = function() {
        filename <- paste0("penguin-report-", format(Sys.time(), "%Y-%m-%d-%H%M%S"), ".html")
        logger::log_info("Generating report with filename: {filename}",
          logger = logger_id
        )
        return(filename)
      },
      content = function(file) {
        # Instead of using the package system.file approach directly, we'll
        # create the template file programmatically to ensure it exists
        temp_dir <- tempdir()
        logger::log_info("Using temporary directory: {temp_dir}",
          logger = logger_id
        )

        temp_report <- file.path(temp_dir, "report.Rmd")
        logger::log_info("Temp report path: {temp_report}",
          logger = logger_id
        )

        # Check if the package is installed and attempt to find the template
        if (file.exists(system.file(package = "modreport"))) {
          template_path <- system.file("rmd", "report_template.Rmd", package = "modreport")
          logger::log_info("Template path from system.file: {template_path}",
            logger = logger_id
          )

          # If template path is empty, we need to create the template directly
          if (template_path == "") {
            logger::log_warn("Template not found in package, creating directly in temp dir",
              logger = logger_id
            )
            # Create template directly - we'll define the template content below
            create_report_template(temp_report)
          } else {
            # Copy from package
            logger::log_info("Copying template from package", logger = logger_id)
            file_copy_success <- file.copy(template_path, temp_report, overwrite = TRUE)
            logger::log_info("File copy success: {file_copy_success}", logger = logger_id)
          }
        } else {
          logger::log_warn("Package not found, creating template directly",
            logger = logger_id
          )
          create_report_template(temp_report)
        }

        # Check if the template file now exists
        if (!file.exists(temp_report)) {
          logger::log_error("Template file does not exist at: {temp_report}", logger = logger_id)
          stop("Failed to create or copy report template.")
        } else {
          logger::log_info("Template file exists and is ready for rendering", logger = logger_id)
        }

        # Set up parameters to pass to the Rmd
        params <- list(
          data = data,
          x_var = inputs()$x_var,
          y_var = inputs()$y_var,
          plot_type = inputs()$plot_type,
          species = inputs()$species,
          islands = inputs()$islands,
          report_date = Sys.Date()
        )

        logger::log_info("Rendering report with parameters: x_var={params$x_var}, y_var={params$y_var}, plot_type={params$plot_type}",
          logger = logger_id
        )

        # Render the report
        tryCatch(
          {
            rmarkdown::render(
              input = temp_report,
              output_file = file,
              params = params,
              envir = new.env(parent = globalenv())
            )
            logger::log_info("Report successfully rendered to: {file}", logger = logger_id)
          },
          error = function(e) {
            logger::log_error("Error rendering report: {e$message}", logger = logger_id)
            # Create a simple error report instead
            create_error_report(file, e$message, params)
          }
        )
      }
    )
  })
}
