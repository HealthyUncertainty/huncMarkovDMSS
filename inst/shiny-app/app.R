# DMSS Multi-Comparator Cost-Effectiveness Model — Shiny App
# Launch with: huncMarkovDMSS::launch_app()

library(shiny)
library(huncMarkovDMSS)
library(dplyr)

has_ggplot <- requireNamespace("ggplot2", quietly = TRUE)
has_DT     <- requireNamespace("DT",      quietly = TRUE)
has_scales <- requireNamespace("scales",  quietly = TRUE)

if (has_ggplot) library(ggplot2)

default_params <- get_default_parameters()
default_reg    <- get_parameter_registry()
treatment_names <- c("Cardivex", "SoC", "Zonapride", "Septoplasty", "Ablatherm")

get_default_se <- function(nm) {
  idx <- which(default_reg$name == nm)
  if (length(idx) == 1 && !is.na(default_reg$se[idx])) default_reg$se[idx] else NA_real_
}

fmt_cost <- function(x) {
  ifelse(is.na(x), NA_character_,
         formatC(round(x, 0), format = "f", digits = 0, big.mark = ","))
}
fmt_qaly <- function(x) {
  ifelse(is.na(x), NA_character_,
         formatC(round(x, 2), format = "f", digits = 2))
}

# ---- Helper: slider or numeric input by type ----
param_input <- function(reg_row) {
  nm  <- reg_row$name
  lbl <- reg_row$display_name
  val <- reg_row$default_value
  typ <- reg_row$type
  if (typ %in% c("probability", "utility")) {
    sliderInput(nm, lbl, min = 0, max = 1, value = val, step = 0.001)
  } else {
    numericInput(nm, lbl, value = val, min = 0)
  }
}
se_input <- function(reg_row) {
  nm  <- reg_row$name
  val <- reg_row$se
  numericInput(paste0("se_", nm), paste0("SE: ", reg_row$display_name),
               value = round(val, 5), min = 0, step = 0.001)
}

# ---- Parameter sub-tab builder ----
build_param_tab <- function(category) {
  rows <- default_reg[default_reg$category == category, ]
  tabPanel(category,
    br(),
    do.call(tagList, lapply(seq_len(nrow(rows)), function(i) {
      fluidRow(
        column(6, param_input(rows[i, ])),
        column(6, se_input(rows[i, ]))
      )
    }))
  )
}

categories <- unique(default_reg$category)

# ============================================================
# UI
# ============================================================
ui <- fluidPage(
  titlePanel("DMSS Multi-Comparator Cost-Effectiveness Model"),

  sidebarLayout(
    sidebarPanel(
      width = 3,

      h4("Model Configuration"),
      numericInput("time_horizon", "Time Horizon (years)",
                   value = 80, min = 1, max = 100, step = 1),
      sliderInput("discount_rate", "Discount Rate (%)",
                  min = 0, max = 10, value = 3, step = 0.5),
      numericInput("wtp", "WTP Threshold (CAD $)",
                   value = 150000, min = 0, step = 10000),

      hr(),
      h4("Treatment Selection"),
      checkboxGroupInput("treatments", "Include Treatments:",
                         choices  = treatment_names,
                         selected = treatment_names),

      hr(),
      h4("Analysis Type"),
      radioButtons("analysis_type", NULL,
                   choices  = c("Deterministic" = "det",
                                "Probabilistic (PSA)" = "psa"),
                   selected = "det"),
      conditionalPanel(
        condition = "input.analysis_type == 'psa'",
        numericInput("n_sims", "PSA Iterations:",
                     value = 1000, min = 100, max = 10000, step = 100),
        checkboxInput("use_seed", "Use Random Seed", value = FALSE),
        conditionalPanel(
          condition = "input.use_seed == true",
          numericInput("seed_val", "Seed Value:", value = 42, min = 1)
        )
      ),

      hr(),
      actionButton("run",          "Run Model",         class = "btn-primary btn-lg"),
      actionButton("reset_params", "Reset to Defaults", class = "btn-secondary"),
      br(), br(),
      helpText("Fictional illustrative model. Not for clinical use.")
    ),

    mainPanel(
      width = 9,
      tabsetPanel(
        # ---- About ----
        tabPanel("About",
          br(),
          h3("DMSS Multi-Comparator Cost-Effectiveness Model"),
          p("This interactive tool implements a population-proportion Markov cohort model for ",
            "cost-effectiveness analysis of five treatment strategies in ",
            "Diffuse Mucosal Sclerosis Syndrome (DMSS). ",
            "It is a fictional illustrative example developed for the HEPackageR skill validation exercise."),

          h4("Model Structure"),
          tags$ul(
            tags$li("4 health states: Grade I, Grade II, Grade III, Dead"),
            tags$li("5 treatment arms: Cardivex, SoC, Zonapride, Septoplasty, Ablatherm"),
            tags$li("4-week cycle length (4/52 years)"),
            tags$li("80-year time horizon (1,040 cycles)"),
            tags$li("3% annual discount rate (costs and QALYs)"),
            tags$li("Age/sex-dependent background mortality (fictional sex-blended life table, 48% female)"),
            tags$li("Frozen grade distributions: cycle 8 for Cardivex/SoC, cycle 1 for others"),
            tags$li("Perioperative mortality: Septoplasty 1.1%, Ablatherm 0.9%"),
            tags$li("23 PSA-sampled parameters (Beta / Gamma / 1-Gamma distributions)")
          ),

          h4("Source"),
          p("Fictional simplified analogue of ICER HCM model (Nov 2021). ",
            "Parameters are entirely fictional and not derived from any ICER source document. ",
            "Developed as part of the R-to-Excel HTA method validation experiment."),

          h4("Validation"),
          p("Base-case deterministic results validated against the original R scripts:"),
          tableOutput("validation_table"),

          hr(),
          h4("Development"),
          p("Ian Cromwell (", tags$a(href = "mailto:healthyuncertainty@gmail.com",
                                     "healthyuncertainty@gmail.com"), ")"),
          p("Developed using the ",
            tags$a(href = "https://github.com/HealthyUncertainty/hepackager", "HEPackageR"),
            " skill for Claude AI.")
        ),

        # ---- Parameters ----
        tabPanel("Parameters",
          br(),
          h4("Adjust Model Parameters"),
          p("Modify parameter means and standard errors. SE values are used in PSA sampling."),
          do.call(tabsetPanel, lapply(categories, build_param_tab))
        ),

        # ---- Results ----
        tabPanel("Results",
          br(),
          h4("Cost-Effectiveness Results"),
          conditionalPanel(
            condition = "output.has_results == false",
            div(class = "alert alert-info", style = "font-size: 16px;",
                "\u2139 Click 'Run Model' to generate results.")
          ),
          conditionalPanel(
            condition = "output.has_results == true",
            h5("Base-Case Summary"),
            if (has_DT) DT::dataTableOutput("results_table")
            else        tableOutput("results_table"),
            br(),
            h5("ICER Table"),
            if (has_DT) DT::dataTableOutput("icer_table")
            else        tableOutput("icer_table"),
            br(),
            downloadButton("download_results", "Download Results (CSV)")
          )
        ),

        # ---- Frontier ----
        tabPanel("Frontier",
          br(),
          h4("Cost-Effectiveness Frontier"),
          p("Non-dominated treatment strategies (extended dominance checked)."),
          conditionalPanel(
            condition = "output.has_results == false",
            div(class = "alert alert-info", "\u2139 Run the model first.")
          ),
          plotOutput("frontier_plot", height = "550px"),
          br(),
          downloadButton("download_frontier", "Download Plot (PNG)")
        ),

        # ---- Probabilistic ----
        tabPanel("Probabilistic",
          conditionalPanel(
            condition = "input.analysis_type == 'psa' && output.has_results == true",
            br(),
            h4("Cost-Effectiveness Plane"),
            plotOutput("ce_plane", height = "500px"),
            br(),
            h4("Cost-Effectiveness Acceptability Curve"),
            plotOutput("ceac_plot", height = "450px"),
            br(),
            downloadButton("download_psa", "Download Plots (PNG)")
          ),
          conditionalPanel(
            condition = "input.analysis_type == 'det'",
            br(),
            div(class = "alert alert-info", style = "font-size: 16px;",
                "\u2139 Switch to 'Probabilistic (PSA)' mode to view CE plane and CEAC.")
          ),
          conditionalPanel(
            condition = "input.analysis_type == 'psa' && output.has_results == false",
            br(),
            div(class = "alert alert-info", "\u2139 Run the model first.")
          )
        ),

        # ---- Trace ----
        tabPanel("Trace",
          br(),
          h4("Markov Trace — State Occupancy Over Time"),
          conditionalPanel(
            condition = "output.has_results == false",
            div(class = "alert alert-info", "\u2139 Run the model first.")
          ),
          plotOutput("trace_plot", height = "600px"),
          br(),
          downloadButton("download_trace", "Download Plot (PNG)")
        )
      )
    )
  )
)

# ============================================================
# Server
# ============================================================
server <- function(input, output, session) {

  model_results <- reactiveVal(NULL)

  output$has_results <- reactive({ !is.null(model_results()) })
  outputOptions(output, "has_results", suspendWhenHidden = FALSE)

  # Validation table (static)
  output$validation_table <- renderTable({
    data.frame(
      Arm        = c("Cardivex", "SoC", "Zonapride", "Septoplasty", "Ablatherm"),
      `Cost (CAD)` = c("$1,464,400", "$417,042", "$491,781", "$367,217", "$307,733"),
      QALYs      = c("15.39", "14.40", "14.65", "15.44", "15.45"),
      `Life Years` = c("17.80", "17.80", "17.80", "17.61", "17.64"),
      check.names = FALSE, stringsAsFactors = FALSE
    )
  })

  # Collect SE overrides from UI inputs
  collect_se_overrides <- function() {
    overrides <- list()
    for (nm in default_reg$name) {
      val <- input[[paste0("se_", nm)]]
      if (!is.null(val) && !is.na(val) && val > 0) overrides[[nm]] <- val
    }
    overrides
  }

  # Build params from inputs
  build_params <- function() {
    params <- get_default_parameters()
    params$time_horizon_years  <- input$time_horizon
    params$discount_rate_annual <- input$discount_rate / 100
    params$wtp_threshold        <- input$wtp
    for (nm in default_reg$name) {
      val <- input[[nm]]
      if (!is.null(val) && !is.na(val)) params[[nm]] <- val
    }
    params
  }

  # Reset parameters
  observeEvent(input$reset_params, {
    for (i in seq_len(nrow(default_reg))) {
      nm <- default_reg$name[i]
      updateNumericInput(session, nm,    value = default_reg$default_value[i])
      updateNumericInput(session, paste0("se_", nm), value = round(default_reg$se[i], 5))
    }
    updateNumericInput(session, "time_horizon",  value = 80)
    updateSliderInput(session,  "discount_rate", value = 3)
    updateNumericInput(session, "wtp",           value = 150000)
    showNotification("Parameters reset to defaults.", type = "message")
  })

  # Run model
  observeEvent(input$run, {
    req(length(input$treatments) >= 2)
    withProgress(message = "Running DMSS model...", value = 0, {
      incProgress(0.2, detail = "Building parameters...")
      params      <- build_params()
      n_sim       <- if (input$analysis_type == "psa") input$n_sims else 1L
      seed_val    <- if (input$analysis_type == "psa" && isTRUE(input$use_seed)) input$seed_val else NULL
      se_ov       <- collect_se_overrides()

      incProgress(0.5, detail = "Running model...")
      res <- tryCatch(
        run_model(parameters  = params,
                  n_sim       = n_sim,
                  return_trace = TRUE,
                  seed        = seed_val,
                  se_overrides = if (length(se_ov) > 0) se_ov else NULL),
        error = function(e) { showNotification(paste("Error:", e$message), type = "error"); NULL }
      )
      if (!is.null(res)) {
        # Filter to selected treatments
        sel <- input$treatments
        res$results_summary <- res$results_summary[res$results_summary$Treatment %in% sel, ]
        res$icer_table      <- res$icer_table[res$icer_table$Treatment %in% sel, ]
        if (!is.null(res$trace_data)) res$trace_data <- res$trace_data[sel]
        if (!is.null(res$psa_results)) {
          keep_cols <- c(paste0(sel, "_cost"), paste0(sel, "_qaly"))
          keep_cols <- keep_cols[keep_cols %in% names(res$psa_results)]
          res$psa_results <- res$psa_results[, keep_cols, drop = FALSE]
        }
        model_results(res)
      }
      incProgress(1.0, detail = "Complete")
    })
    if (!is.null(model_results()))
      showNotification("Analysis complete!", type = "message", duration = 3)
  })

  # ---- Results table ----
  make_results_df <- function() {
    req(model_results())
    df <- model_results()$results_summary
    data.frame(
      Treatment = df$Treatment,
      `Total Cost (CAD)` = fmt_cost(df$Cost),
      `QALYs`  = fmt_qaly(df$QALY),
      `Life Years` = fmt_qaly(df$LY),
      check.names = FALSE, stringsAsFactors = FALSE
    )
  }

  output$results_table <- if (has_DT) {
    DT::renderDataTable({
      DT::datatable(make_results_df(),
                    options = list(pageLength = -1, dom = "t", paging = FALSE,
                                   ordering = TRUE, autoWidth = FALSE),
                    rownames = FALSE)
    })
  } else {
    renderTable({ make_results_df() })
  }

  # ---- ICER table ----
  make_icer_df <- function() {
    req(model_results())
    df <- model_results()$icer_table
    data.frame(
      Treatment   = df$Treatment,
      `Cost (CAD)` = fmt_cost(df$Cost),
      QALYs        = fmt_qaly(df$QALY),
      `Inc. Cost`  = fmt_cost(df$Inc_Cost),
      `Inc. QALYs` = fmt_qaly(df$Inc_QALY),
      ICER         = ifelse(is.na(df$ICER), "—", fmt_cost(df$ICER)),
      Status       = df$Status,
      check.names = FALSE, stringsAsFactors = FALSE
    )
  }

  output$icer_table <- if (has_DT) {
    DT::renderDataTable({
      DT::datatable(make_icer_df(),
                    options = list(pageLength = -1, dom = "t", paging = FALSE,
                                   ordering = FALSE, autoWidth = FALSE),
                    rownames = FALSE)
    })
  } else {
    renderTable({ make_icer_df() })
  }

  # ---- Frontier ----
  frontier_plot_obj <- reactive({
    req(model_results())
    if (!has_ggplot) return(NULL)
    plot_frontier(model_results()$results_summary)
  })
  output$frontier_plot <- renderPlot({ req(frontier_plot_obj()); frontier_plot_obj() })

  # ---- CE Plane ----
  ce_plane_obj <- reactive({
    req(model_results(), input$analysis_type == "psa", !is.null(model_results()$psa_results))
    if (!has_ggplot) return(NULL)
    plot_ce_plane(model_results()$psa_results, reference = "SoC", wtp = input$wtp)
  })
  output$ce_plane <- renderPlot({ req(ce_plane_obj()); ce_plane_obj() })

  # ---- CEAC ----
  ceac_plot_obj <- reactive({
    req(model_results(), input$analysis_type == "psa", !is.null(model_results()$psa_results))
    if (!has_ggplot) return(NULL)
    plot_ceac(model_results()$psa_results)
  })
  output$ceac_plot <- renderPlot({ req(ceac_plot_obj()); ceac_plot_obj() })

  # ---- Trace ----
  trace_plot_obj <- reactive({
    req(model_results(), !is.null(model_results()$trace_data))
    if (!has_ggplot) return(NULL)
    plot_trace(model_results()$trace_data,
               cycle_length_years = model_results()$parameters$cycle_length_years)
  })
  output$trace_plot <- renderPlot({ req(trace_plot_obj()); trace_plot_obj() })

  # ---- Downloads ----
  output$download_results <- downloadHandler(
    filename = function() paste0("dmss_results_", Sys.Date(), ".csv"),
    content  = function(file) {
      req(model_results())
      write.csv(model_results()$results_summary, file, row.names = FALSE)
    }
  )
  output$download_frontier <- downloadHandler(
    filename = function() paste0("dmss_frontier_", Sys.Date(), ".png"),
    content  = function(file) {
      req(frontier_plot_obj())
      ggplot2::ggsave(file, plot = frontier_plot_obj(), width = 10, height = 7, dpi = 300)
    }
  )
  output$download_psa <- downloadHandler(
    filename = function() paste0("dmss_psa_", Sys.Date(), ".png"),
    content  = function(file) {
      req(ce_plane_obj(), ceac_plot_obj())
      png(file, width = 12, height = 10, units = "in", res = 300)
      gridExtra::grid.arrange(ce_plane_obj(), ceac_plot_obj(), nrow = 2)
      dev.off()
    }
  )
  output$download_trace <- downloadHandler(
    filename = function() paste0("dmss_trace_", Sys.Date(), ".png"),
    content  = function(file) {
      req(trace_plot_obj())
      ggplot2::ggsave(file, plot = trace_plot_obj(), width = 12, height = 7, dpi = 300)
    }
  )
}

shinyApp(ui = ui, server = server)
