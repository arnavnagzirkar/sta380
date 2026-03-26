library(shiny)
library(colourpicker)
source("../../R/ts_functions.R")

# Compute AIC or BIC for a grid of ARMA(p, q) models.
# Fits models via ML for all (p, q) in 0:max_p x 0:max_q.
# Returns a (max_p+1) x (max_q+1) matrix; (0,0) and non-converged cells are NA.
compute_aic_bic <- function(data, max_p, max_q, criterion = "AIC") {
  mat <- matrix(NA_real_,
                nrow = max_p + 1,
                ncol = max_q + 1,
                dimnames = list(
                  paste0("AR", 0:max_p),
                  paste0("MA", 0:max_q)
                ))

  for (p in 0:max_p) {
    for (q in 0:max_q) {
      if (p == 0 && q == 0) next  # white noise: skip
      tryCatch({
        fit <- suppressWarnings(arima(data, order = c(p, 0, q), method = "ML"))
        mat[p + 1, q + 1] <- suppressWarnings(if (criterion == "BIC") BIC(fit) else AIC(fit))
      }, error = function(e) {})
    }
  }

  return(mat)
}

# ── UI ────────────────────────────────────────────────────────────────────────

ui <- fluidPage(

  tags$head(
    tags$style(HTML("
      /* ── Full-viewport layout ── */
      html, body {
        height: 100%;
        margin: 0;
        overflow: hidden;
        font-family: -apple-system, BlinkMacSystemFont, 'Segoe UI', Roboto, Helvetica, Arial, sans-serif;
      }
      .container-fluid {
        height: 100%;
        display: flex;
        flex-direction: column;
        padding: 0;
      }

      /* ── Title bar ── */
      .title-bar {
        background-color: #1e293b;
        color: #e2e8f0;
        padding: 16px 24px;
        font-size: 20px;
        font-weight: 600;
        letter-spacing: 0.5px;
        flex-shrink: 0;
        border-bottom: 1px solid #0f172a;
      }

      /* ── Main row fills remaining height ── */
      .main-row {
        flex: 1 1 0;
        min-height: 0;
        display: flex;
        flex-direction: row;
        margin: 0 !important;
      }
      .main-row > [class*='col-'] {
        float: none !important;
        display: flex;
        flex-direction: column;
        height: 100%;
        padding: 0;
      }

      /* ── Left sidebar ── */
      .sidebar-panel {
        background-color: #f8fafc;
        padding: 24px 20px;
        border-right: 1px solid #e2e8f0;
        height: 100%;
        overflow-y: auto;
        box-sizing: border-box;
        box-shadow: 2px 0 10px rgba(0,0,0,0.02);
      }

      /* ── Inputs & Typography ── */
      .sidebar-panel label, .control-label {
        font-weight: 500;
        color: #475569;
        margin-bottom: 6px;
        margin-top: 8px;
      }
      .form-control {
        border-radius: 6px;
        border: 1px solid #cbd5e1;
        padding: 8px 12px;
        box-shadow: none;
        transition: border-color 0.2s, box-shadow 0.2s;
      }
      .form-control:focus {
        border-color: #3b82f6;
        box-shadow: 0 0 0 3px rgba(59, 130, 246, 0.15);
      }

      /* ── Buttons ── */
      .btn {
        border-radius: 6px;
        font-weight: 500;
        transition: all 0.2s;
      }
      #simulate_btn {
        background-color: #3b82f6;
        color: white;
        border: none;
        width: 100%;
        padding: 10px;
        margin-top: 8px;
        margin-bottom: 16px;
      }
      #simulate_btn:hover {
        background-color: #2563eb;
      }

      /* ── Field validation styling (Anti-Jank) ── */
      .input-error-text {
        color: #ef4444;
        font-size: 12px;
        margin-top: 4px;
        min-height: 18px; /* Prevents vertical layout shifts */
        display: block;
      }
      .invalid-field .form-control {
        border: 1px solid #ef4444 !important;
        background-color: #fef2f2 !important;
        box-shadow: 0 0 0 3px rgba(239, 68, 68, 0.1) !important;
      }
      .invalid-field label {
        color: #dc2626;
      }

      /* ── Formula preview box ── */
      #formula_preview {
        background-color: #ffffff;
        border: 1px solid #e2e8f0;
        border-radius: 6px;
        padding: 12px;
        font-size: 13px;
        margin-bottom: 16px;
        margin-top: 4px;
        white-space: pre-wrap;
        word-wrap: break-word;
        overflow-wrap: break-word;
        max-height: 160px;
        overflow-y: auto;
      }

      /* ── Graph area ── */
      .graph-area {
        height: 100%;
        display: flex;
        flex-direction: column;
        align-items: stretch;
        width: 100%;
        overflow-y: auto; /* Allows scrolling if plots don't fit */
        background-color: #ffffff;
      }
      .graph-area > .shiny-html-output {
        flex: 1 1 auto;
        display: flex;
        flex-direction: column;
        padding: 16px;
        gap: 16px;
      }
      .graph-area .plot-wrapper {
        flex: 1 1 300px;
        min-height: 300px; /* The absolute minimum height R needs */
        display: flex;
        flex-direction: column;
      }
      .graph-area .shiny-plot-output {
        width: 100% !important;
        height: 100% !important;
        flex: 1 1 0;
        min-height: 0;
      }

      /* ── Right stacked panels (if used) ── */
      .right-panel {
        border-left: 1px solid #e2e8f0;
        height: 100%;
        display: flex;
        flex-direction: column;
      }

      /* ── Model selection modal grid ── */
      .model-tile {
        padding: 10px 14px;
        border: 1px solid #e2e8f0;
        text-align: center;
        font-size: 13px;
        cursor: pointer;
        border-radius: 6px;
        transition: all 0.15s;
        user-select: none;
      }
      .model-tile:hover {
        background-color: #eff6ff;
        border-color: #3b82f6;
      }
      .model-tile.tile-auto-best {
        background-color: #f0fdf4;
        border-color: #22c55e;
        color: #166534;
        font-weight: 600;
      }
      .model-tile.tile-selected {
        background-color: #3b82f6;
        border-color: #2563eb;
        color: #fff;
        font-weight: 600;
      }
      .model-tile.tile-na {
        color: #94a3b8;
        cursor: default;
        background-color: #f8fafc;
      }
      .model-tile.tile-na:hover {
        background-color: #f8fafc;
        border-color: #e2e8f0;
      }
      .model-grid-header {
        padding: 8px 10px;
        background-color: #f1f5f9;
        color: #475569;
        text-align: center;
        font-size: 13px;
        font-weight: 600;
        border-radius: 4px;
      }
    ")),
    tags$script(HTML("
      Shiny.addCustomMessageHandler('toggleInputError', function(message) {
        var wrapper = document.getElementById(message.id);
        if (!wrapper) return;
        var input = wrapper.querySelector('.form-control');
        if (!input) return;

        if (message.show) {
          wrapper.classList.add('invalid-field');
        } else {
          wrapper.classList.remove('invalid-field');
        }
      });
    "))
  ),

  # ── Title Bar ───────────────────────────────────────────────────────────────
  div(class = "title-bar", "Simulating and Fitting ARMA Models"),

  # ── Main 2-Column Layout ────────────────────────────────────────────────────
  fluidRow(class = "main-row",

           # ── Left Sidebar (controls) ──────────────────────────────────────────────
           column(4,
                  div(class = "sidebar-panel",

                      # Top-level toggle: Simulation Inputs vs Graph Output
                      selectInput("modify_target", "What would you like to modify?",
                                  choices = c("Simulation Inputs", "Graph Output")
                      ),

                      tags$hr(style = "border-color: #e2e8f0; margin: 20px 0;"),

                      # ── Branch: Simulation Inputs ─────────────────────────────────────
                      conditionalPanel(
                        condition = "input.modify_target == 'Simulation Inputs'",

                        selectInput("sim_mode",
                                    "Would you like to simulate or input your own data?",
                                    choices = c("Simulate", "Input own data")
                        ),

                        # ── Sub-branch: Simulate ──
                        conditionalPanel(
                          condition = "input.sim_mode == 'Simulate'",

                          tags$label("Currently Simulating:"),
                          withMathJax(uiOutput("formula_preview")),
                          div(style = "margin-bottom: 20px; font-size: 13px; color: #64748b;", textOutput("sim_status")),

                          div(
                            id = "ar_coefs_wrapper",
                            textInput("ar_coefs", "AR Coefficients:", value = "0.5"),
                            div(class = "input-error-text", textOutput("ar_coefs_error"))
                          ),

                          div(
                            id = "ma_coefs_wrapper",
                            textInput("ma_coefs", "MA Coefficients:", value = "0.1"),
                            div(class = "input-error-text", textOutput("ma_coefs_error"))
                          ),

                          div(
                            id = "noise_params_wrapper",
                            textInput(
                              "noise_params",
                              "Noise Parameters: W_t ~ N(mu, sigma)",
                              value = "0, 1"
                            ),
                            div(class = "input-error-text", textOutput("noise_params_error"))
                          ),
                          numericInput("seed", "Insert seed for simulation",
                                       value = 1, min = 1
                          ),
                          div(
                            id = "n_obs_wrapper",
                            numericInput("n_obs", "Number of Observations",
                                         value = 100, min = 1, step = 1
                            ),
                            div(class = "input-error-text", textOutput("n_obs_error"))
                          ),
                          actionButton("simulate_btn", "Simulate")
                        ),

                        # ── Sub-branch: Input own data ──
                        conditionalPanel(
                          condition = "input.sim_mode == 'Input own data'",

                          textAreaInput("user_data",
                                        "Paste data separated by commas here:",
                                        placeholder = "1, 2, 0, 1.2, ...",
                                        rows = 6
                          )
                        )
                      ),

                      # ── Branch: Graph Output ──────────────────────────────────────────
                      conditionalPanel(
                        condition = "input.modify_target == 'Graph Output'",

                        div(style = "display: flex; align-items: flex-end; gap: 8px; margin-bottom: 12px;",
                            div(style = "flex: 1;",
                                selectInput("model_criterion", "Model Selection Criteria:",
                                            choices = c("AIC", "BIC"))
                            ),
                            div(style = "padding-bottom: 15px;",
                                actionButton("open_model_select", "Select model",
                                             style = "background-color: #f1f5f9; color: #475569;
                                                  border: 1px solid #cbd5e1; font-size: 13px; padding: 7px 12px;
                                                  white-space: nowrap; font-weight: 500;")
                            )
                        ),

                        numericInput("max_p", "Max AR order (p) to search",
                                     value = 5, min = 0, max = 10, step = 1
                        ),
                        numericInput("max_q", "Max MA order (q) to search",
                                     value = 5, min = 0, max = 10, step = 1
                        ),

                        tags$hr(style = "border-color: #e2e8f0; margin: 20px 0;"),

                        checkboxGroupInput("display_graphs",
                                           "Which graphs would you like to display?",
                                           choices = c("Data", "ACVF", "ACF", "Forecast overlay"),
                                           selected = c("Data", "ACVF", "ACF", "Forecast overlay"),
                                           inline = TRUE
                        ),

                        selectInput("graph_modify",
                                    "Which graph would you like to modify?",
                                    choices = c("Data plot", "ACF / ACVF")
                        ),

                        numericInput("max_lag", "Displayed maximum lag",
                                     value = 20, min = 1
                        ),

                        # ── Sub-branch: Data plot ──
                        conditionalPanel(
                          condition = "input.graph_modify == 'Data plot'",

                          numericInput("data_pch",
                                       "Select the point symbol for the plot",
                                       value = 1, min = 0, max = 25
                          ),
                          colourInput("data_col",
                                      "Point color",
                                      value = "#000000"
                          ),
                          conditionalPanel(
                            condition = "input.display_graphs.indexOf('Forecast overlay') !== -1",
                            tags$hr(style = "border-color: #e2e8f0; margin: 20px 0;"),
                            sliderInput("forecast_start", "Forecast start (t)",
                                        min = 1, max = 99, value = 80, step = 1),
                            uiOutput("selected_model_text")
                          )
                        ),

                        # ── Sub-branch: ACF / ACVF ──
                        conditionalPanel(
                          condition = "input.graph_modify == 'ACF / ACVF'",

                          numericInput("line_pch",
                                       "Select the point symbol for the line",
                                       value = 1, min = 0, max = 25
                          ),
                          colourInput("theo_col",
                                      "Theoretical line color",
                                      value = "#3b82f6"
                          ),
                          colourInput("sample_col",
                                      "Sample line color",
                                      value = "#ef4444"
                          )
                        )
                      )
                  )
           ),

           # ── Center Graph Area ────────────────────────────────────────────────────
           column(8,
                  div(class = "graph-area",
                      uiOutput("plots_ui")
                  )
           )
  )
)

# ── Server ────────────────────────────────────────────────────────────────────

server <- function(input, output, session) {
  # ── Manual model selection (NULL = use AIC/BIC minimum) ────────────────
  manual_model <- reactiveVal(NULL)

  # Reset manual selection whenever the grid inputs change
  observeEvent(list(input$model_criterion, input$max_p, input$max_q,
                    input$simulate_btn, input$user_data), {
                      manual_model(NULL)
                    }, ignoreInit = TRUE)

  # Parse comma-separated numeric input
  parse_num_vector <- function(x) {
    x <- trimws(x)

    # blank input = no coefficients
    if (!nzchar(x)) {
      return(numeric(0))
    }

    parts <- trimws(strsplit(x, ",")[[1]])

    # reject empty entries like "0.5,,0.2" or "0.5,"
    if (any(parts == "")) {
      return(NULL)
    }

    vals <- suppressWarnings(as.numeric(parts))

    # reject non-numeric values
    if (any(is.na(vals))) {
      return(NULL)
    }

    vals
  }

  # Parse noise parameters entered as: mu, sigma
  parse_noise_params <- function(x) {
    x <- trimws(x)

    if (!nzchar(x)) {
      return(NULL)
    }

    parts <- trimws(strsplit(x, ",")[[1]])

    # must have exactly two values
    if (length(parts) != 2 || any(parts == "")) {
      return(NULL)
    }

    vals <- suppressWarnings(as.numeric(parts))

    if (any(is.na(vals))) {
      return(NULL)
    }

    list(mu = vals[1], sigma = vals[2])
  }

  # ── Parse user input data ──
  parse_user_data <- reactive({
    req(input$sim_mode == "Input own data")

    x <- trimws(input$user_data)

    if (!nzchar(x)) return(NULL)

    parts <- trimws(strsplit(x, ",")[[1]])

    # reject bad formats like trailing commas
    if (any(parts == "")) return(NULL)

    vals <- suppressWarnings(as.numeric(parts))

    if (any(is.na(vals))) return(NULL)

    vals
  })

  # ── Unified data source: either simulated or user-supplied ──────────────
  active_data <- reactive({
    if (input$sim_mode == "Input own data") {
      vals <- parse_user_data()
      req(!is.null(vals), length(vals) >= 2)
      vals
    } else {
      model <- tryCatch(sim_model(), error = function(e) NULL)
      req(!is.null(model))
      model$data
    }
  })

  ar_vals <- reactive({
    parse_num_vector(input$ar_coefs)
  })

  ma_vals <- reactive({
    parse_num_vector(input$ma_coefs)
  })

  noise_vals <- reactive({
    parse_noise_params(input$noise_params)
  })

  output$ar_coefs_error <- renderText({
    ar <- ar_vals()

    if (is.null(ar)) {
      "Enter numbers separated by commas only."
    } else {
      ""
    }
  })

  output$ma_coefs_error <- renderText({
    ma <- ma_vals()

    if (is.null(ma)) {
      "Enter numbers separated by commas only."
    } else {
      ""
    }
  })

  output$noise_params_error <- renderText({
    nv <- noise_vals()

    if (is.null(nv)) {
      "Enter exactly two numbers: mu, sigma"
    } else if (nv$sigma <= 0) {
      "Sigma must be greater than 0."
    } else {
      ""
    }
  })

  output$n_obs_error <- renderText({
    if (is.na(input$n_obs)) {
      "Enter a whole number greater than or equal to 1."
    } else if (input$n_obs < 1 || input$n_obs != floor(input$n_obs)) {
      "Number of observations must be a whole number ≥ 1."
    } else {
      ""
    }
  })

  observe({
    session$sendCustomMessage("toggleInputError", list(
      id = "ar_coefs_wrapper",
      show = is.null(ar_vals())
    ))

    nv <- noise_vals()

    session$sendCustomMessage("toggleInputError", list(
      id = "ma_coefs_wrapper",
      show = is.null(ma_vals())
    ))

    session$sendCustomMessage("toggleInputError", list(
      id = "noise_params_wrapper",
      show = is.null(nv) || (!is.null(nv) && nv$sigma <= 0)
    ))

    session$sendCustomMessage("toggleInputError", list(
      id = "n_obs_wrapper",
      show = is.na(input$n_obs) || input$n_obs < 1 || input$n_obs != floor(input$n_obs)
    ))
  })

  # ── Formula preview ──
  output$formula_preview <- renderUI({
    ar <- ar_vals()
    ma <- ma_vals()
    nv <- noise_vals()

    if (is.null(ar) || is.null(ma) || is.null(nv)) {
      return(p("Please enter valid numeric inputs to preview the model."))
    }

    ar_part <- ""
    if (length(ar) > 0) {
      ar_terms <- vapply(seq_along(ar), function(i) {
        paste0(ar[i], "X_{t-", i, "}")
      }, character(1))
      ar_part <- paste(ar_terms, collapse = " + ")
    }

    ma_part <- ""
    if (length(ma) > 0) {
      ma_terms <- vapply(seq_along(ma), function(i) {
        if (ma[i] >= 0) {
          paste0("+ ", ma[i], "W_{t-", i, "}")
        } else {
          paste0("- ", abs(ma[i]), "W_{t-", i, "}")
        }
      }, character(1))
      ma_part <- paste(ma_terms, collapse = " ")
    }

    lhs <- "X_t ="
    rhs <- paste0(
      if (nzchar(ar_part)) paste0(ar_part, " + ") else "",
      "W_t",
      if (nzchar(ma_part)) paste0(" ", ma_part) else ""
    )
    noise_line <- paste0("W_t \\sim \\mathcal{N}(\\mu = ", nv$mu, ",\\, \\sigma = ", nv$sigma, ")")

    # Wrap in MathJax display-mode delimiters
    latex_str <- paste0(
      "$$", lhs, " ", rhs, "$$",
      "$$", noise_line, "$$"
    )

    withMathJax(HTML(latex_str))
  })

  output$sim_status <- renderText({
    ar <- ar_vals()
    ma <- ma_vals()

    if (is.null(ar) || is.null(ma)) {
      return("Invalid AR/MA coefficient input.")
    }

    p <- length(ar)
    q <- length(ma)

    model_type <- if (p > 0 && q > 0) {
      paste0("ARMA(", p, ",", q, ")")
    } else if (p > 0) {
      paste0("AR(", p, ")")
    } else if (q > 0) {
      paste0("MA(", q, ")")
    } else {
      "White Noise"
    }

    paste(model_type, "| n =", input$n_obs, "| seed =", input$seed)
  })

  sim_model <- eventReactive(input$simulate_btn, {
    ar <- ar_vals()
    ma <- ma_vals()
    nv <- noise_vals()

    validate(
      need(!is.null(ar),
           "AR coefficients must be numeric values separated by commas, e.g. 0.5, -0.2"),
      need(!is.null(ma),
           "MA coefficients must be numeric values separated by commas, e.g. 0.3, 0.1"),
      need(!is.null(nv),
           "Noise parameters must be entered as: mu, sigma   (example: 0, 1)"),
      need(input$n_obs >= 1 && input$n_obs == floor(input$n_obs),
           "Number of observations must be a whole number ≥ 1."),
      need(input$seed >= 1,
           "Seed must be at least 1."),
      need(nv$sigma > 0,
           "Sigma must be greater than 0.")
    )

    set.seed(input$seed)

    wn <- rnorm(
      n = input$n_obs,
      mean = nv$mu,
      sd = nv$sigma
    )

    gen_arma(
      n = input$n_obs,
      wn = wn,
      ar_coefs = ar,
      ma_coefs = ma
    )
  })

  # ── Update forecast_start slider range when simulation runs ─────────────
  observeEvent(input$simulate_btn, {
    n <- input$n_obs
    if (!is.na(n) && n >= 2) {
      updateSliderInput(session, "forecast_start",
                        min   = 2,
                        max   = n - 1,
                        value = max(2, round(n * 0.8)))
    }
  })

  # Update slider when user data changes
  observeEvent(parse_user_data(), {
    vals <- parse_user_data()
    if (!is.null(vals) && length(vals) >= 2) {
      n <- length(vals)
      updateSliderInput(session, "forecast_start",
                        min   = 2,
                        max   = n - 1,
                        value = max(2, round(n * 0.8)))
    }
  }, ignoreNULL = TRUE)

  # ── Dynamic plot UI: show only selected graphs ──────────────────────────
  output$plots_ui <- renderUI({
    selected <- input$display_graphs
    if (is.null(selected) || length(selected) == 0) {
      return(div(style = "color:#888; font-style:italic; text-align:center; padding-top:40px;",
                 "No graphs selected. Choose at least one in Graph Output."))
    }

    plot_list <- list()
    if ("Data" %in% selected)
      plot_list <- c(plot_list, list(
        div(class = "plot-wrapper", plotOutput("plot_data", height = "100%", width = "100%"))
      ))
    if ("ACVF" %in% selected)
      plot_list <- c(plot_list, list(
        div(class = "plot-wrapper", plotOutput("plot_acvf", height = "100%", width = "100%"))
      ))
    if ("ACF" %in% selected)
      plot_list <- c(plot_list, list(
        uiOutput("acf_causality_warning"),
        div(class = "plot-wrapper", plotOutput("plot_acf", height = "100%", width = "100%"))
      ))
    do.call(tagList, plot_list)
  })

  # ── Data plot (with optional forecast overlay) ────────────────────────
  output$plot_data <- renderPlot({
    req("Data" %in% input$display_graphs)
    data <- tryCatch(active_data(), error = function(e) NULL)
    validate(need(!is.null(data),
                  if (input$sim_mode == "Simulate")
                    "Click 'Simulate' after entering valid inputs."
                  else
                    "Enter valid comma-separated numeric values."))

    n     <- length(data)
    t_obs <- seq_len(n)

    show_fc <- "Forecast overlay" %in% input$display_graphs
    fc      <- if (show_fc) tryCatch(forecast_out(), error = function(e) NULL) else NULL

    if (!is.null(fc)) {
      pred     <- as.numeric(fc$pred)
      se       <- as.numeric(fc$se)
      upper    <- pred + 1.96 * se
      lower    <- pred - 1.96 * se
      t_start  <- input$forecast_start
      last_obs <- data[t_start - 1]
      t_fc     <- (t_start - 1):n
      pred     <- c(last_obs, pred)
      upper    <- c(last_obs, upper)
      lower    <- c(last_obs, lower)
      y_range  <- range(c(data, upper, lower), na.rm = TRUE)
    } else {
      y_range <- range(data, na.rm = TRUE)
    }

    main_title <- if (input$sim_mode == "Simulate") "Simulated Time Series" else "Input Time Series"

    par(mar = c(4, 4, 2, 1))
    plot(t_obs, data, type = "l",
         ylim = y_range,
         main = main_title,
         ylab = expression(X[t]),
         xlab = "t"
    )
    points(t_obs, data, pch = input$data_pch, col = input$data_col)

    if (!is.null(fc)) {
      polygon(c(t_fc, rev(t_fc)), c(upper, rev(lower)),
              col = adjustcolor("#3b82f6", alpha.f = 0.15), border = NA)
      lines(t_fc, pred,  col = "#3b82f6", lwd = 2)
      lines(t_fc, upper, col = "#3b82f6", lwd = 1, lty = 2)
      lines(t_fc, lower, col = "#3b82f6", lwd = 1, lty = 2)
      abline(v = t_start, col = "grey50", lty = 3)
      legend("topleft",
             legend = c("Observed", "Forecast", "95% PI"),
             col    = c("black", "#3b82f6", "#3b82f6"),
             lty    = c(1, 1, 2),
             lwd    = c(1, 2, 1),
             bty    = "n"
      )
    }
  })


  # ── ACVF plot (theoretical + sample, or sample-only for user data) ────
  output$plot_acvf <- renderPlot({
    req("ACVF" %in% input$display_graphs)
    max_lag <- input$max_lag
    lags    <- 0:max_lag

    if (input$sim_mode == "Simulate") {
      model <- tryCatch(sim_model(), error = function(e) NULL)
      validate(need(!is.null(model), "Click 'Simulate' after entering valid inputs."))

      nv    <- noise_vals()
      sigma <- if (!is.null(nv)) nv$sigma else 1

      theo_acvf   <- get_theoretical_acvf(model, sigma = sigma, max_lag = max_lag)
      sample_acvf <- get_sample_acvf(model, max_lag = max_lag)
      y_range     <- range(c(theo_acvf, sample_acvf), na.rm = TRUE)

      par(mar = c(4, 4, 2, 1))
      plot(lags, theo_acvf,
           type = "b", pch = input$line_pch, col = input$theo_col, lwd = 2,
           ylim = y_range,
           main = "ACVF: Theoretical vs Sample",
           xlab = "Lag", ylab = expression(gamma(h))
      )
      lines(lags, sample_acvf,
            type = "b", pch = input$line_pch, col = input$sample_col, lwd = 2, lty = 2)
      abline(h = 0, col = "grey60", lty = 3)
      legend("topright",
             legend = c("Theoretical", "Sample"),
             col    = c(input$theo_col, input$sample_col),
             lty    = c(1, 2), pch = input$line_pch, lwd = 2, bty = "n"
      )
    } else {
      data <- tryCatch(active_data(), error = function(e) NULL)
      validate(need(!is.null(data), "Enter valid comma-separated numeric values."))

      eff_lag     <- min(max_lag, length(data) - 1)
      sample_acvf <- acf(data, lag.max = eff_lag, type = "covariance",
                         plot = FALSE)$acf[, , 1]
      lags_out    <- 0:eff_lag
      y_range     <- range(sample_acvf, na.rm = TRUE)

      plot(lags_out, sample_acvf,
           type = "b", pch = input$line_pch, col = input$sample_col, lwd = 2, lty = 2,
           ylim = y_range,
           main = "ACVF: Sample",
           xlab = "Lag", ylab = expression(gamma(h))
      )
      abline(h = 0, col = "grey60", lty = 3)
      legend("topright",
             legend = "Sample",
             col    = input$sample_col,
             lty    = 2, pch = input$line_pch, lwd = 2, bty = "n"
      )
    }
  })

  # ── ACF plot (theoretical + sample, or sample-only for user data) ──────
  output$plot_acf <- renderPlot({
    req("ACF" %in% input$display_graphs)
    max_lag <- input$max_lag
    lags    <- 0:max_lag

    if (input$sim_mode == "Simulate") {
      model <- tryCatch(sim_model(), error = function(e) NULL)
      validate(need(!is.null(model), "Click 'Simulate' after entering valid inputs."))

      nv    <- noise_vals()
      sigma <- if (!is.null(nv)) nv$sigma else 1

      invisible(capture.output(
        theo_acf <- get_theoretical_acf(model, sigma = sigma, max_lag = max_lag)
      ))
      sample_acf <- get_sample_acf(model, max_lag = max_lag)

      n        <- model$n
      ci_bound <- 1.96 / sqrt(n)
      y_range  <- range(c(theo_acf, sample_acf, ci_bound, -ci_bound), na.rm = TRUE)

      par(mar = c(4, 4, 2, 1))
      plot(lags, theo_acf,
           type = "b", pch = input$line_pch, col = input$theo_col, lwd = 2,
           ylim = y_range,
           main = "ACF: Theoretical vs Sample",
           xlab = "Lag", ylab = expression(rho(h))
      )
      lines(lags, sample_acf,
            type = "b", pch = input$line_pch, col = input$sample_col, lwd = 2, lty = 2)
      abline(h =  0,        col = "grey60", lty = 3)
      abline(h =  ci_bound, col = "grey40", lty = 2)
      abline(h = -ci_bound, col = "grey40", lty = 2)
      legend("topright",
             legend = c("Theoretical", "Sample", "95% CI"),
             col    = c(input$theo_col, input$sample_col, "grey40"),
             lty    = c(1, 2, 2),
             pch    = c(input$line_pch, input$line_pch, NA),
             lwd    = 2, bty = "n"
      )
    } else {
      data <- tryCatch(active_data(), error = function(e) NULL)
      validate(need(!is.null(data), "Enter valid comma-separated numeric values."))

      n        <- length(data)
      eff_lag  <- min(max_lag, n - 1)
      sample_acf <- acf(data, lag.max = eff_lag, type = "correlation",
                        plot = FALSE)$acf[, , 1]
      lags_out <- 0:eff_lag
      ci_bound <- 1.96 / sqrt(n)
      y_range  <- range(c(sample_acf, ci_bound, -ci_bound), na.rm = TRUE)

      plot(lags_out, sample_acf,
           type = "b", pch = input$line_pch, col = input$sample_col, lwd = 2, lty = 2,
           ylim = y_range,
           main = "ACF: Sample",
           xlab = "Lag", ylab = expression(rho(h))
      )
      abline(h =  0,        col = "grey60", lty = 3)
      abline(h =  ci_bound, col = "grey40", lty = 2)
      abline(h = -ci_bound, col = "grey40", lty = 2)
      legend("topright",
             legend = c("Sample", "95% CI"),
             col    = c(input$sample_col, "grey40"),
             lty    = c(2, 2),
             pch    = c(input$line_pch, NA),
             lwd    = 2, bty = "n"
      )
    }
  })


  # ── Causality reactive ────────────────────────────────────────────────
  acf_is_causal <- reactive({
    req(input$sim_mode == "Simulate")
    model <- tryCatch(sim_model(), error = function(e) NULL)
    if (is.null(model)) return(TRUE)
    is_causal(model)
  })

  output$acf_causality_warning <- renderUI({
    if (!isTRUE(acf_is_causal())) {
      div(
        style = paste(
          "color: #dc2626;",
          "font-size: 13px;",
          "font-weight: 500;",
          "background-color: #fef2f2;",
          "border-left: 4px solid #ef4444;",
          "padding: 8px 12px;",
          "flex-shrink: 0;"
        ),
        "⚠ AR part is not causal, so the theoretical ACF may not be accurate."
      )
    }
  })

  # ── AIC / BIC reactive ───────────────────────────────────────────────
  aic_bic_mat <- reactive({
    data <- tryCatch(active_data(), error = function(e) NULL)
    req(!is.null(data))

    compute_aic_bic(data,
                    max_p     = input$max_p,
                    max_q     = input$max_q,
                    criterion = input$model_criterion)
  })

  # ── Open model selection modal ──────────────────────────────────────
  observeEvent(input$open_model_select, {
    showModal(modalDialog(
      title = "Select forecast model",
      uiOutput("model_select_grid"),
      footer = tagList(
        actionButton("modal_use_best", "Use best",
                     style = "background-color: #22c55e; color: #fff; border: none; font-weight: 500;"),
        modalButton("Close")
      ),
      size = "m",
      easyClose = TRUE
    ))
  })

  # "Use best" resets to auto
  observeEvent(input$modal_use_best, {
    manual_model(NULL)
    removeModal()
  })

  # Tile click: set manual_model and close modal
  observeEvent(input$model_tile_click, {
    manual_model(input$model_tile_click)
    removeModal()
  }, ignoreNULL = TRUE, ignoreInit = TRUE)

  # ── Modal grid renderUI ───────────────────────────────────────────────
  output$model_select_grid <- renderUI({
    mat <- aic_bic_mat()
    if (is.null(mat)) {
      return(p(if (input$sim_mode == "Simulate")
        "Simulate first to populate the grid."
        else
          "Enter valid data to populate the grid.",
        style = "color: #888; font-style: italic;"))
    }

    auto_idx <- which(mat == min(mat, na.rm = TRUE), arr.ind = TRUE)
    auto_p   <- auto_idx[1, 1] - 1
    auto_q   <- auto_idx[1, 2] - 1

    sel <- manual_model()
    if (!is.null(sel)) {
      parts  <- as.integer(strsplit(sel, ",")[[1]])
      sel_p  <- parts[1]; sel_q <- parts[2]
    } else {
      sel_p  <- auto_p;   sel_q <- auto_q
    }

    # Column headers row
    n_col <- ncol(mat)
    header_row <- div(
      style = paste0("display: grid; grid-template-columns: 80px repeat(", n_col, ", 1fr);
                     gap: 6px; margin-bottom: 6px;"),
      div(),
      lapply(colnames(mat), function(nm) div(class = "model-grid-header", nm))
    )

    body_rows <- lapply(seq_len(nrow(mat)), function(i) {
      p_val <- i - 1
      div(
        style = paste0("display: grid; grid-template-columns: 80px repeat(", n_col, ", 1fr);
                        gap: 6px; margin-bottom: 6px;"),
        div(class = "model-grid-header", rownames(mat)[i]),
        lapply(seq_len(n_col), function(j) {
          q_val   <- j - 1
          val     <- mat[i, j]
          txt     <- if (is.na(val)) "—" else formatC(val, digits = 1, format = "f")
          is_na   <- is.na(val)
          is_auto <- (p_val == auto_p && q_val == auto_q)
          is_sel  <- (p_val == sel_p  && q_val == sel_q)

          tile_class <- paste("model-tile",
                              if (is_na)        "tile-na"
                              else if (is_sel)  "tile-selected"
                              else if (is_auto) "tile-auto-best"
                              else              ""
          )

          onclick_js <- if (!is_na) {
            sprintf("Shiny.setInputValue('model_tile_click', '%d,%d', {priority: 'event'})",
                    p_val, q_val)
          } else ""

          div(class = tile_class,
              onclick = onclick_js,
              txt)
        })
      )
    })

    div(
      div(style = "font-size: 14px; margin-bottom: 12px; color: #475569;",
          paste(input$model_criterion, "values — click a cell to select that model")),
      header_row,
      do.call(tagList, body_rows),
      div(style = "font-size: 13px; color: #475569; margin-top: 16px; padding: 12px; background: #f8fafc; border-radius: 6px;",
          if (is.null(manual_model())) {
            tags$span(style = "color: #166534; font-weight: 600;",
                      paste0("Auto: ARMA(", auto_p, ", ", auto_q,
                             ") — lowest ", input$model_criterion))
          } else {
            tags$span(style = "color: #2563eb; font-weight: 600;",
                      paste0("Selected: ARMA(", sel_p, ", ", sel_q, ")"))
          })
    )
  })

  # ── AIC / BIC panel ──────────────────────────────────────────────────

  # ── Best model fit (auto or manually selected) ──────────────────────────
  best_fit <- reactive({
    req("Forecast overlay" %in% input$display_graphs)
    mat  <- aic_bic_mat()
    data <- tryCatch(active_data(), error = function(e) NULL)
    req(mat, data, input$forecast_start)

    sel <- manual_model()
    if (!is.null(sel)) {
      parts  <- as.integer(strsplit(sel, ",")[[1]])
      best_p <- parts[1]; best_q <- parts[2]
    } else {
      idx    <- which(mat == min(mat, na.rm = TRUE), arr.ind = TRUE)
      best_p <- idx[1, 1] - 1
      best_q <- idx[1, 2] - 1
    }

    t_start <- input$forecast_start
    train   <- data[seq_len(t_start - 1)]

    tryCatch(
      arima(train, order = c(best_p, 0, best_q), method = "ML"),
      error = function(e) NULL
    )
  })

  # ── Forecast from forecast_start to end of series ────────────────────
  forecast_out <- reactive({
    fit  <- best_fit()
    data <- tryCatch(active_data(), error = function(e) NULL)
    req(fit, data, input$forecast_start)
    h <- length(data) - input$forecast_start + 1
    req(h >= 1)
    predict(fit, n.ahead = h)
  })

  # ── Selected model label + equation ─────────────────────────────────
  output$selected_model_text <- renderUI({
    mat <- tryCatch(aic_bic_mat(), error = function(e) NULL)
    req(mat)

    fit <- tryCatch(best_fit(), error = function(e) NULL)
    req(fit)

    sel <- manual_model()
    if (!is.null(sel)) {
      parts  <- as.integer(strsplit(sel, ",")[[1]])
      best_p <- parts[1]; best_q <- parts[2]
      source_label <- "manually selected"
    } else {
      idx    <- which(mat == min(mat, na.rm = TRUE), arr.ind = TRUE)
      best_p <- idx[1, 1] - 1
      best_q <- idx[1, 2] - 1
      source_label <- paste0("selected by ", input$model_criterion)
    }

    # Extract fitted coefficients from arima object
    cf <- coef(fit)
    ar_coefs <- cf[grepl("^ar", names(cf))]
    ma_coefs <- cf[grepl("^ma", names(cf))]

    fmt <- function(x) {
      s <- formatC(round(x, 3), format = "f", digits = 3)
      s
    }

    ar_part <- ""
    if (length(ar_coefs) > 0) {
      ar_terms <- vapply(seq_along(ar_coefs), function(i) {
        paste0(fmt(ar_coefs[i]), "X_{t-", i, "}")
      }, character(1))
      ar_part <- paste(ar_terms, collapse = " + ")
    }

    ma_part <- ""
    if (length(ma_coefs) > 0) {
      ma_terms <- vapply(seq_along(ma_coefs), function(i) {
        v <- ma_coefs[i]
        if (v >= 0) paste0("+ ", fmt(v),  "W_{t-", i, "}")
        else        paste0("- ", fmt(abs(v)), "W_{t-", i, "}")
      }, character(1))
      ma_part <- paste(ma_terms, collapse = " ")
    }

    rhs <- paste0(
      if (nzchar(ar_part)) paste0(ar_part, " + ") else "",
      "W_t",
      if (nzchar(ma_part)) paste0(" ", ma_part) else ""
    )

    latex_str <- paste0("$$X_t = ", rhs, "$$")

    tagList(
      div(style = "font-size: 13px; font-weight: 500; color: #475569; margin-top: 8px;",
          paste0("Forecast: ARMA(", best_p, ", ", best_q, ") — ", source_label)),
      withMathJax(HTML(latex_str))
    )
  })
}

shinyApp(ui, server)




