#' Launch the ARMA Simulation Shiny App
#'
#' @description
#' Opens the interactive Shiny application for simulating and fitting
#' ARMA models. The app allows users to specify model parameters or
#' input their own data, and visualise theoretical and sample ACF/ACVF.
#'
#' @examples
#' if (interactive()) {
#'   launch_app()
#' }
#'
#' @export
launch_app <- function() {
  app_dir <- system.file("shiny-app", package = "TsimSeriesACF")
  if (app_dir == "") {
    stop("Could not find the app directory. Try re-installing `TsimSeriesACF`.")
  }
  shiny::runApp(app_dir, display.mode = "normal")
}
