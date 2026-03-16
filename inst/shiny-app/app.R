library(shiny)
source("../../R/ts_functions.R")

# ── UI ────────────────────────────────────────────────────────────────────────

ui <- fluidPage(
  
  tags$head(
    tags$style(HTML("
      /* ── Full-viewport layout ── */
      html, body {
        height: 100%;
        margin: 0;
        overflow: hidden;
      }
      .container-fluid {
        height: 100%;
        display: flex;
        flex-direction: column;
        padding: 0;
      }
 
      /* ── Title bar ── */
      .title-bar {
        background-color: #2c3e50;
        color: #d4a017;
        padding: 14px 24px;
        font-size: 22px;
        font-weight: bold;
        font-style: italic;
        flex-shrink: 0;
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
        background-color: #f0f0f0;
        padding: 15px;
        border-right: 1px solid #ccc;
        height: 100%;
        overflow-y: auto;
        box-sizing: border-box;
      }
 
      /* ── Graph area ── */
      .graph-area {
        height: 100%;
        display: flex;
        flex-direction: column;
        align-items: stretch;
        width: 100%;
        overflow: hidden;
      }
      /* uiOutput wrapper must also be a flex column */
      .graph-area > .shiny-html-output {
        flex: 1 1 0;
        min-height: 0;
        display: flex;
        flex-direction: column;
      }
      /* Each plot wrapper stretches equally */
      .graph-area .plot-wrapper {
        flex: 1 1 0;
        min-height: 0;
        display: flex;
        flex-direction: column;
      }
      .graph-area .shiny-plot-output {
        width: 100% !important;
        height: 100% !important;
        flex: 1 1 0;
        min-height: 0;
      }
 
      /* ── Right stacked panels ── */
      .right-panel {
        border-left: 2px solid #333;
        height: 100%;
        display: flex;
        flex-direction: column;
      }
      .right-panel-top, .right-panel-bottom {
        flex: 1;
        display: flex;
        align-items: center;
        justify-content: center;
        padding: 10px;
        min-height: 0;
      }
      .right-panel-top {
        border-bottom: 2px solid #333;
      }
      .right-panel-top h4, .right-panel-bottom h4 {
        color: #555;
        font-size: 20px;
        margin: 0;
      }
 
      /* ── Formula preview box ── */
      #formula_preview {
        background-color: #f9f9f9;
        border: 1px solid #ddd;
        padding: 8px 10px;
        font-size: 13px;
        white-space: pre-wrap;
        word-wrap: break-word;
        overflow-wrap: break-word;
        max-height: 160px;
        overflow-y: auto;
      }
 
      /* ── Misc ── */
      .sidebar-panel label {
        font-weight: normal;
        color: #555;
      }
      .sidebar-panel .control-label {
        color: #555;
        margin-top: 6px;
      }
 
      /* ── Field validation styling ── */
      .input-error-text {
        color: #c0392b;
        font-size: 12px;
        margin-top: -8px;
        margin-bottom: 10px;
      }
      .invalid-field .form-control {
        border: 2px solid #c0392b !important;
        background-color: #fff5f5 !important;
      }
      .invalid-field label {
        color: #c0392b;
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
          input.style.border = '2px solid #c0392b';
          input.style.background = '#fff5f5';
        } else {
          wrapper.classList.remove('invalid-field');
          input.style.border = '';
          input.style.background = '';
        }
      });
    "))
  ),
  
  # ── Title Bar ───────────────────────────────────────────────────────────────
  div(class = "title-bar", "Simulating and Fitting ARMA Models"),
  
  # ── Main 3-Column Layout ────────────────────────────────────────────────────
  fluidRow(class = "main-row",
           
           # ── Left Sidebar (controls) ──────────────────────────────────────────────
           column(3,
                  div(class = "sidebar-panel",
                      
                      # Top-level toggle: Simulation Inputs vs Graph Output
                      selectInput("modify_target", "What would you like to modify?",
                                  choices = c("Simulation Inputs", "Graph Output")
                      ),
                      
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
                          textOutput("sim_status"),
                          
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
                        
                        selectInput("model_criterion", "Model Selection Criteria:",
                                    choices = c("AIC", "BIC")
                        ),
                        
                        checkboxGroupInput("display_graphs",
                                           "Which graphs would you like to display?",
                                           choices = c("Data", "ACVF", "ACF"),
                                           selected = c("Data", "ACVF", "ACF"),
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
                          textInput("data_col",
                                    "Input Color for the point symbol",
                                    value = "#FFFFFF"
                          )
                        ),
                        
                        # ── Sub-branch: ACF / ACVF ──
                        conditionalPanel(
                          condition = "input.graph_modify == 'ACF / ACVF'",
                          
                          selectInput("line_modify",
                                      "Which line would you like to modify?",
                                      choices = c("Theoretical", "Sample")
                          ),
                          numericInput("line_pch",
                                       "Select the point symbol for the line",
                                       value = 1, min = 0, max = 25
                          ),
                          textInput("line_col",
                                    "Input Color for the line's point symbol",
                                    value = "#FFFFFF"
                          )
                        )
                      )
                  )
           ),
           
           # ── Center Graph Area ────────────────────────────────────────────────────
           column(6,
                  div(class = "graph-area",
                      uiOutput("plots_ui")
                  )
           ),
           
           # ── Right Info Panels ────────────────────────────────────────────────────
           column(3,
                  div(class = "right-panel",
                      div(class = "right-panel-top",
                          uiOutput("coef_est_panel")
                      ),
                      div(class = "right-panel-bottom",
                          uiOutput("aic_bic_panel")
                      )
                  )
           )
  )
)

# ── Server ────────────────────────────────────────────────────────────────────

server <- function(input, output, session) {
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
  
  # ── Data plot ──────────────────────────────────────────────────────────
  output$plot_data <- renderPlot({
    req("Data" %in% input$display_graphs)
    req(input$sim_mode == "Simulate")
    model <- sim_model()
    validate(need(!is.null(model), "Click 'Simulate' after entering valid inputs."))
    
    plot(model$data, type = "l",
         main = "Simulated Time Series",
         ylab = expression(X[t]),
         xlab = "t"
    )
    points(model$data, pch = input$data_pch, col = input$data_col)
  })
  
  # ── ACVF plot (theoretical + sample) ──────────────────────────────────
  output$plot_acvf <- renderPlot({
    req("ACVF" %in% input$display_graphs)
    req(input$sim_mode == "Simulate")
    model <- sim_model()
    validate(need(!is.null(model), "Click 'Simulate' after entering valid inputs."))
    
    nv      <- noise_vals()
    sigma   <- if (!is.null(nv)) nv$sigma else 1
    max_lag <- input$max_lag
    lags    <- 0:max_lag
    
    theo_acvf   <- get_theoretical_acvf(model, sigma = sigma, max_lag = max_lag)
    sample_acvf <- get_sample_acvf(model, max_lag = max_lag)
    
    y_range <- range(c(theo_acvf, sample_acvf), na.rm = TRUE)

    theo_col   <- if (input$line_modify == "Theoretical") input$line_col else "#2c7bb6"
    sample_col <- if (input$line_modify == "Sample") input$line_col else "#d7191c"

    plot(lags, theo_acvf,
         type = "b",
         pch  = input$line_pch,
         col  = theo_col,
         lwd  = 2,
         ylim = y_range,
         main = "ACVF: Theoretical vs Sample",
         xlab = "Lag",
         ylab = expression(gamma(h))
    )
    lines(lags, sample_acvf,
          type = "b",
          pch  = input$line_pch,
          col  = sample_col,
          lwd  = 2,
          lty  = 2
    )
    abline(h = 0, col = "grey60", lty = 3)
    legend("topright",
           legend = c("Theoretical", "Sample"),
           col    = c(theo_col, sample_col),
           lty    = c(1, 2),
           pch    = input$line_pch,
           lwd    = 2,
           bty    = "n"
    )
  })
  
  # ── ACF plot (theoretical + sample) ───────────────────────────────────
  output$plot_acf <- renderPlot({
    req("ACF" %in% input$display_graphs)
    req(input$sim_mode == "Simulate")
    model <- sim_model()
    validate(need(!is.null(model), "Click 'Simulate' after entering valid inputs."))
    
    nv      <- noise_vals()
    sigma   <- if (!is.null(nv)) nv$sigma else 1
    max_lag <- input$max_lag
    lags    <- 0:max_lag
    
    invisible(capture.output(
      theo_acf <- get_theoretical_acf(model, sigma = sigma, max_lag = max_lag)
    ))
    sample_acf <- get_sample_acf(model, max_lag = max_lag)
    
    # 95% confidence bounds (Bartlett's approximation)
    n       <- model$n
    ci_bound <- 1.96 / sqrt(n)
    y_range <- range(c(theo_acf, sample_acf, ci_bound, -ci_bound), na.rm = TRUE)
    
    theo_col   <- if (input$line_modify == "Theoretical") input$line_col else "#2c7bb6"
    sample_col <- if (input$line_modify == "Sample") input$line_col else "#d7191c"

    plot(lags, theo_acf,
         type = "b",
         pch  = input$line_pch,
         col  = theo_col,
         lwd  = 2,
         ylim = y_range,
         main = "ACF: Theoretical vs Sample",
         xlab = "Lag",
         ylab = expression(rho(h))
    )
    lines(lags, sample_acf,
          type = "b",
          pch  = input$line_pch,
          col  = sample_col,
          lwd  = 2,
          lty  = 2
    )
    abline(h =  0,        col = "grey60",  lty = 3)
    abline(h =  ci_bound, col = "grey40",  lty = 2)
    abline(h = -ci_bound, col = "grey40",  lty = 2)
    legend("topright",
           legend = c("Theoretical", "Sample", "95% CI"),
           col    = c(theo_col, sample_col, "grey40"),
           lty    = c(1, 2, 2),
           pch    = c(input$line_pch, input$line_pch, NA),
           lwd    = 2,
           bty    = "n"
    )
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
          "color: #c0392b;",
          "font-size: 12px;",
          "font-style: italic;",
          "padding: 4px 8px;",
          "flex-shrink: 0;"
        ),
        "⚠ AR part is not causal, so the theoretical ACF may not be accurate."
      )
    }
  })
  
  # ── Placeholder: coefficient estimates panel ──
  output$coef_est_panel <- renderUI({
    h4("Coef. Est.")
  })
  
  # ── Placeholder: AIC / BIC panel ──
  output$aic_bic_panel <- renderUI({
    h4("AIC / BIC")
  })
}

shinyApp(ui, server)