#' UI for the Palmer Penguins Shiny App
#'
#' @return A UI definition
#'
#' @export
#'
app_ui <- function() {
  bslib::page(

    theme = bslib::bs_theme(
      version = 5,
      bootswatch = "flatly"),
    bslib::page_sidebar(
      title = "Palmer Penguins Explorer",
      sidebar = bslib::sidebar(
        title = h4("Inputs"),
        width = "300px",
        mod_input_ui("inputs"),
        hr(),
        mod_download_report_ui("report")
      ),
      bslib::layout_column_wrap(
        width = "100%",
        heights_equal = "row",
        h4("Outputs"),
        mod_graph_output_ui("graph"),
        mod_table_output_ui("table")
      )
    )
  )
}
