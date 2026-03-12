if (!requireNamespace("shiny", quietly = TRUE)) {
  stop("Package 'shiny' is required. Run install.packages('shiny') first.")
}

shiny::runApp("inst/shiny-app")
