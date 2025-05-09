#' Create Error Report
#'
#' @param filepath The path to create the error report
#' @param error_message The error message
#' @param params The parameters that were to be used
#'
#' @return NULL (creates file as a side effect)
#'
#' @keywords internal
#'
create_error_report <- function(filepath, error_message, params) {
  error_html <- paste0(
    '<!DOCTYPE html>
    <html>
    <head>
      <title>Report Generation Error</title>
      <style>
        body { font-family: Arial, sans-serif; margin: 40px; line-height: 1.6; }
        h1 { color: #d9534f; }
        .error-box { background-color: #f9f2f2; border-left: 4px solid #d9534f; padding: 10px 20px; margin: 20px 0; }
        .params-box { background-color: #f5f5f5; border-left: 4px solid #5bc0de; padding: 10px 20px; margin: 20px 0; }
        pre { background-color: #f8f8f8; padding: 10px; overflow: auto; }
      </style>
    </head>
    <body>
      <h1>Error Generating Report</h1>
      <div class="error-box">
        <h3>Error Message:</h3>
        <pre>', error_message, '</pre>
      </div>
      <div class="params-box">
        <h3>Report Parameters:</h3>
        <ul>
          <li><strong>X Variable:</strong> ', params$x_var, "</li>
          <li><strong>Y Variable:</strong> ", params$y_var, "</li>
          <li><strong>Plot Type:</strong> ", params$plot_type, "</li>
          <li><strong>Selected Species:</strong> ", paste(params$species, collapse = ", "), "</li>
          <li><strong>Selected Islands:</strong> ", paste(params$islands, collapse = ", "), "</li>
          <li><strong>Report Date:</strong> ", params$report_date, "</li>
        </ul>
      </div>
      <p>Please contact support with this error information.</p>
    </body>
    </html>"
  )

  writeLines(error_html, filepath)
}
