library(shiny)

# ── UI ────────────────────────────────────────────────────────────────────────

ui <- fluidPage(

  tags$head(
    tags$style(HTML("
      /* ── Title bar ── */
      .title-bar {
        background-color: #2c3e50;
        color: #d4a017;
        padding: 14px 24px;
        font-size: 22px;
        font-weight: bold;
        font-style: italic;
        margin: -15px -15px 15px -15px;
      }

      /* ── Left sidebar ── */
      .sidebar-panel {
        background-color: #f0f0f0;
        padding: 15px;
        border-right: 1px solid #ccc;
        min-height: 650px;
      }

      /* ── Graph area ── */
      .graph-area {
        min-height: 650px;
        display: flex;
        align-items: center;
        justify-content: center;
      }

      /* ── Right stacked panels ── */
      .right-panel {
        border-left: 2px solid #333;
        min-height: 650px;
        display: flex;
        flex-direction: column;
      }
      .right-panel-top, .right-panel-bottom {
        flex: 1;
        display: flex;
        align-items: center;
        justify-content: center;
        padding: 10px;
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
  fluidRow(

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
            verbatimTextOutput("formula_preview"),
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
        plotOutput("main_plot", height = "600px")
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
  output$formula_preview <- renderText({
    ar <- ar_vals()
    ma <- ma_vals()
    nv <- noise_vals()
    
    if (is.null(ar) || is.null(ma) || is.null(nv)) {
      return("Please enter valid numeric inputs to preview the model.")
    }
    
    ar_part <- ""
    if (length(ar) > 0) {
      ar_terms <- vapply(seq_along(ar), function(i) {
        paste0(ar[i], "X_{t - ", i, "}")
      }, character(1))
      ar_part <- paste(ar_terms, collapse = " + ")
    }
    
    ma_part <- ""
    if (length(ma) > 0) {
      ma_terms <- vapply(seq_along(ma), function(i) {
        if (ma[i] >= 0) {
          paste0("+ ", ma[i], "W_{t - ", i, "}")
        } else {
          paste0("- ", abs(ma[i]), "W_{t - ", i, "}")
        }
      }, character(1))
      ma_part <- paste(ma_terms, collapse = " ")
    }
    
    paste0(
      "X_t = ",
      if (nzchar(ar_part)) paste0(ar_part, " + ") else "",
      "W_t",
      if (nzchar(ma_part)) paste0(" ", ma_part) else "",
      ", where\nW_t ~ N(mu = ", nv$mu, ", sigma = ", nv$sigma, ")"
    )
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

  # ── Placeholder for the main plot area. Added a graph to test simulation 
  # button
  output$main_plot <- renderPlot({
    req(input$sim_mode == "Simulate")
    
    model <- sim_model()
    
    validate(
      need(!is.null(model), "Click 'Simulate' after entering valid inputs.")
    )
    
    plot.ts(
      model$data,
      main = "Simulated Time Series",
      ylab = "X_t",
      xlab = "t"
    )
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
