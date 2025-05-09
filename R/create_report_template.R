#' Create Report Template
#'
#' @param filepath The path to create the template file
#'
#' @return NULL (creates file as a side effect)
#'
#' @keywords internal
#'
create_report_template <- function(filepath) {
  template_content <- '---
title: "Palmer Penguins Report"
params:
  data: NULL
  x_var: "bill_length_mm"
  y_var: "body_mass_g"
  plot_type: "scatter"
  species: NULL
  islands: NULL
  report_date: !r Sys.Date()
output:
  html_document:
    theme: flatly
    toc: true
    toc_float: true
---

```{r setup, include=FALSE}
knitr::opts_chunk$set(
  echo = FALSE,
  message = FALSE,
  warning = FALSE,
  fig.width = 10,
  fig.height = 6
)

library(dplyr)
library(ggplot2)
library(knitr)
library(tidyr)

```

# Palmer Penguins Analysis Report

**Generated on:** `r params$report_date`

## Analysis Parameters

This report was generated with the following parameters:

- **X Variable:** `r gsub("_", " ", stringr::str_to_title(params$x_var))`
- **Y Variable:** `r gsub("_", " ", stringr::str_to_title(params$y_var))`
- **Plot Type:** `r stringr::str_to_title(params$plot_type)`
- **Selected Species:** `r paste(params$species, collapse = ", ")`
- **Selected Islands:** `r paste(params$islands, collapse = ", ")`

## Filtered Dataset

```{r filter-data}
filtered_data <- params$data %>%
  dplyr::filter(
    species %in% params$species,
    island %in% params$islands
  ) %>%
  tidyr::drop_na(all_of(c(params$x_var, params$y_var)))

# Display summary statistics
filtered_data %>%
  group_by(species) %>%
  summarize(
    n = n(),
    mean_x = mean(!!sym(params$x_var)),
    sd_x = sd(!!sym(params$x_var)),
    mean_y = mean(!!sym(params$y_var)),
    sd_y = sd(!!sym(params$y_var))
  ) %>%
  kable(
    col.names = c(
      "Species",
      "n",
      paste0("Mean ", gsub("_", " ", params$x_var)),
      paste0("SD ", gsub("_", " ", params$x_var)),
      paste0("Mean ", gsub("_", " ", params$y_var)),
      paste0("SD ", gsub("_", " ", params$y_var))
    ),
    digits = 2
)

```

## Data Visualization

```{r plot-data}
p <- ggplot2::ggplot(
  filtered_data,
  ggplot2::aes_string(
    x = params$x_var,
    y = params$y_var,
    color = "species"
  )
)

if (params$plot_type == "scatter") {
  p <- p + ggplot2::geom_point(size = 3, alpha = 0.8)
} else {
  p <- p + ggplot2::geom_boxplot()
}

```

```{r plot-render}
p +
  ggplot2::labs(
    title = "Palmer Penguins Visualization",
    x = gsub("_", " ", stringr::str_to_title(params$x_var)),
    y = gsub("_", " ", stringr::str_to_title(params$y_var))
  ) +
  ggplot2::theme_minimal(base_size = 14)

```

## Data Table

The table below shows a sample of the data used for this visualization:

```{r display-table}

filtered_data %>%
  dplyr::select(species, island, params$x_var, params$y_var) %>%
  dplyr::arrange(species, island) %>%
  head(10) %>%
  kable()

```

## Correlation Analysis

```{r correlation}
if (params$plot_type == "scatter") {
  filtered_data %>%
    group_by(species) %>%
    summarize(
      correlation = cor(!!sym(params$x_var), !!sym(params$y_var)),
      .groups = "drop"
    ) %>%
    kable(
      col.names = c(
        "Species",
        paste0("Correlation between ",
               gsub("_", " ", params$x_var), " and ",
               gsub("_", " ", params$y_var))
      ),
      digits = 3
    )
}

```

## Summary

This report provides a visualization of the relationship between `r gsub("_", " ", params$x_var)` and `r gsub("_", " ", params$y_var)` for the selected penguin species. The data comes from the Palmer Penguins dataset, which contains measurements of penguin species observed on islands in the Palmer Archipelago, Antarctica.

For more information about the dataset, visit: https://allisonhorst.github.io/palmerpenguins/
'

  # Write the template content to the file
  writeLines(template_content, filepath)
}
