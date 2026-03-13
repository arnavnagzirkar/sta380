library(shiny)

# ── UI ────────────────────────────────────────────────────────────────────────

ui <- fluidPage(

  tags$head(tags$style(HTML("
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
  "))),

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

            textInput("ar_coefs", "AR Coefficients:", value = "0.5"),
            textInput("ma_coefs", "MA Coefficients:", value = "0.1"),
            textInput("noise_params",
              "Noise Parameters: W_t ~ N(mu, sigma)",
              value = "0, 1"
            ),
            numericInput("seed", "Insert seed for simulation",
              value = 1, min = 1
            ),
            numericInput("n_obs", "Number of Observations",
              value = 100, min = 1
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

  # ── Reactive: parse comma-separated AR coefficients ──
  ar_vals <- reactive({
    txt <- trimws(input$ar_coefs)
    if (!nzchar(txt)) return(NULL)
    suppressWarnings(as.numeric(strsplit(txt, ",")[[1]]))
  })

  # ── Reactive: parse comma-separated MA coefficients ──
  ma_vals <- reactive({
    txt <- trimws(input$ma_coefs)
    if (!nzchar(txt)) return(NULL)
    suppressWarnings(as.numeric(strsplit(txt, ",")[[1]]))
  })

  # ── Reactive: parse noise parameters (mu, sigma) ──
  noise_vals <- reactive({
    txt <- trimws(input$noise_params)
    parts <- strsplit(txt, ",")[[1]]
    mu <- if (length(parts) >= 1) as.numeric(trimws(parts[1])) else 0
    sigma <- if (length(parts) >= 2) as.numeric(trimws(parts[2])) else 1
    list(mu = mu, sigma = sigma)
  })

  # ── Formula preview ──
  output$formula_preview <- renderText({
    ar <- ar_vals()
    ma <- ma_vals()
    nv <- noise_vals()

    # Build AR terms
    ar_part <- ""
    if (!is.null(ar) && length(ar) > 0) {
      ar_terms <- vapply(seq_along(ar), function(i) {
        paste0(ar[i], "X_{t - ", i, "}")
      }, character(1))
      ar_part <- paste(ar_terms, collapse = " + ")
    }

    # Build MA terms with sign
    ma_part <- ""
    if (!is.null(ma) && length(ma) > 0) {
      ma_terms <- vapply(seq_along(ma), function(i) {
        if (ma[i] >= 0) {
          paste0("+ ", ma[i], "W_{t - ", i, "}")
        } else {
          paste0("- ", abs(ma[i]), "W_{t - ", i, "}")
        }
      }, character(1))
      ma_part <- paste(ma_terms, collapse = " ")
    }

    formula <- paste0(
      "X_t = ",
      if (nzchar(ar_part)) paste0(ar_part, " + ") else "",
      "W_t",
      if (nzchar(ma_part)) paste0(" ", ma_part) else "",
      ", where\nW_t ~ N(mu = ", nv$mu, ", sigma = ", nv$sigma, ")"
    )
    formula
  })
  
  output$sim_status <- renderText({
    ar_txt <- trimws(input$ar_coefs)
    ma_txt <- trimws(input$ma_coefs)
    
    ar <- if (nzchar(ar_txt)) strsplit(ar_txt, ",")[[1]] else character(0)
    ma <- if (nzchar(ma_txt)) strsplit(ma_txt, ",")[[1]] else character(0)
    
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

  # ── Placeholder: main plot area ──
  output$main_plot <- renderPlot({
    plot.new()
    text(0.5, 0.5, "Equal split between chosen graphs to display",
         cex = 1.4, col = "#999999")
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
