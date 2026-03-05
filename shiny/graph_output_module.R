library(shiny)

# UI: just a container; actual plotOutputs are built dynamically in server
graphOutputUI <- function(id) {
  ns <- NS(id)
  uiOutput(ns("plot_area"))
}

# Server: renders whichever of Data/ACVF/ACF are checked, with equal height
graphOutputServer <- function(id, model, max_lag, selected_graphs,
                              line_to_modify, pch_val, col_val) {
  moduleServer(id, function(input, output, session) {

    # build plot area based on checkbox selection
    output$plot_area <- renderUI({
      selected <- selected_graphs()
      if (length(selected) == 0) return(p("Select at least one graph to display."))

      ns <- session$ns
      h  <- paste0(floor(600 / length(selected)), "px")

      id_map <- c(Data = "plot_data", ACVF = "plot_acvf", ACF = "plot_acf")
      do.call(tagList, lapply(selected, function(g) plotOutput(ns(id_map[g]), height = h)))
    })

    # helper: get sigma from the wn vector
    get_sigma <- function(m) sd(m$wn)

    # --- data plot ---
    output$plot_data <- renderPlot({
      req(model(), "Data" %in% selected_graphs())
      x <- model()$data
      plot(x, type = "l", main = "Simulated Time Series",
           xlab = "Time", ylab = expression(X[t]))
    })

    # --- acvf plot ---
    output$plot_acvf <- renderPlot({
      req(model(), "ACVF" %in% selected_graphs())
      m   <- model()
      lag <- max_lag()
      lags <- 0:lag
      sig <- get_sigma(m)

      samp <- get_sample_acvf(m, max_lag = lag)
      theo <- get_theoretical_acvf(m, sigma = sig, max_lag = lag)

      plot(lags, theo, type = "l", col = "steelblue",
           ylim = range(c(samp, theo)),
           main = "ACVF", xlab = "Lag", ylab = expression(gamma(h)))
      lines(lags, samp, col = "tomato", lty = 2)

      # apply point styling to whichever line is selected
      if (!is.null(line_to_modify()) && !is.null(pch_val()) && !is.null(col_val())) {
        vals <- if (line_to_modify() == "Theoretical") theo else samp
        points(lags, vals, pch = as.integer(pch_val()), col = col_val())
      }

      legend("topright", c("Theoretical", "Sample"),
             col = c("steelblue", "tomato"), lty = c(1, 2), bty = "n")
    })

    # --- acf plot ---
    output$plot_acf <- renderPlot({
      req(model(), "ACF" %in% selected_graphs())
      m   <- model()
      lag <- max_lag()
      lags <- 0:lag
      sig <- get_sigma(m)

      samp <- get_sample_acf(m, max_lag = lag)
      theo <- get_theoretical_acf(m, sigma = sig, max_lag = lag)

      plot(lags, theo, type = "l", col = "steelblue",
           ylim = range(c(samp, theo, -1, 1)),
           main = "ACF", xlab = "Lag", ylab = expression(rho(h)))
      lines(lags, samp, col = "tomato", lty = 2)
      abline(h = 0, lty = 3)
      # 95% confidence bands (iid approximation)
      abline(h = c(1, -1) * 1.96 / sqrt(m$n), lty = 2, col = "gray50")

      if (!is.null(line_to_modify()) && !is.null(pch_val()) && !is.null(col_val())) {
        vals <- if (line_to_modify() == "Theoretical") theo else samp
        points(lags, vals, pch = as.integer(pch_val()), col = col_val())
      }

      legend("topright", c("Theoretical", "Sample"),
             col = c("steelblue", "tomato"), lty = c(1, 2), bty = "n")
    })

  })
}
