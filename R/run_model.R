#' Run DMSS Multi-Comparator Cost-Effectiveness Model
#'
#' Runs the DMSS Markov model for all five treatment arms and returns costs,
#' QALYs, life years, and ICER table. Supports deterministic (n_sim = 1) and
#' probabilistic sensitivity analysis (n_sim > 1).
#'
#' @param parameters Named list of model parameters. NULL uses defaults.
#' @param n_sim Integer. PSA iterations (1 = deterministic).
#' @param return_trace Logical. Include Markov traces in output?
#' @param seed Optional integer random seed.
#' @param se_overrides Optional named list of SE overrides for PSA.
#' @return List with results_summary, psa_results, icer_table, parameters,
#'   and optionally trace_data.
#' @export
#' @importFrom stats rbeta rgamma
#' @examples
#' res <- run_model()
#' res$results_summary
run_model <- function(parameters = NULL,
                      n_sim      = 1,
                      return_trace = FALSE,
                      seed       = NULL,
                      se_overrides = NULL) {

  if (is.null(parameters)) parameters <- get_default_parameters()
  validate_parameters(parameters)
  if (!is.null(seed)) set.seed(seed)

  treatment_names <- c("Cardivex", "SoC", "Zonapride", "Septoplasty", "Ablatherm")

  # ---- Deterministic run (always) ----
  det_results <- lapply(treatment_names, function(trt) {
    .run_single_arm(trt, parameters)
  })
  names(det_results) <- treatment_names

  results_summary <- data.frame(
    Treatment  = treatment_names,
    Cost       = sapply(treatment_names, function(t) det_results[[t]]$cost),
    QALY       = sapply(treatment_names, function(t) det_results[[t]]$qaly),
    LY         = sapply(treatment_names, function(t) det_results[[t]]$ly),
    stringsAsFactors = FALSE
  )

  # ---- PSA ----
  psa_results <- NULL
  if (n_sim > 1) {
    psa_mat <- .generate_psa_draws(parameters, n_sim, se_overrides)

    psa_costs <- matrix(0, nrow = n_sim, ncol = length(treatment_names),
                        dimnames = list(NULL, treatment_names))
    psa_qalys <- matrix(0, nrow = n_sim, ncol = length(treatment_names),
                        dimnames = list(NULL, treatment_names))

    for (j in seq_len(n_sim)) {
      psa_p <- .apply_psa_draw(parameters, psa_mat[j, ])
      for (trt in treatment_names) {
        res <- .run_single_arm(trt, psa_p)
        psa_costs[j, trt] <- res$cost
        psa_qalys[j, trt] <- res$qaly
      }
    }

    # Wide-format data frame expected by visualisation functions
    psa_df <- as.data.frame(psa_costs)
    names(psa_df) <- paste0(treatment_names, "_cost")
    psa_q_df <- as.data.frame(psa_qalys)
    names(psa_q_df) <- paste0(treatment_names, "_qaly")
    psa_results <- cbind(psa_df, psa_q_df)
  }

  # ---- Traces (deterministic) ----
  trace_data <- NULL
  if (return_trace) {
    trace_data <- lapply(treatment_names, function(trt) generate_trace(parameters, trt))
    names(trace_data) <- treatment_names
  }

  # ---- ICER table ----
  icer_table <- .build_icer_table(results_summary)

  list(
    results_summary = results_summary,
    psa_results     = psa_results,
    icer_table      = icer_table,
    parameters      = parameters,
    trace_data      = trace_data
  )
}

# ---- Internal helpers ----

# Run one arm deterministically; returns list(cost, qaly, ly)
.run_single_arm <- function(treatment, params) {
  n_cycles       <- ceiling(params$time_horizon_years / params$cycle_length_years)
  cycle_times    <- (0:(n_cycles - 1)) * params$cycle_length_years
  disc           <- (1 / (1 + params$discount_rate_annual))^cycle_times

  trace <- generate_trace(params, treatment)

  lys <- costs <- qalys <- numeric(n_cycles)
  for (i in seq_len(n_cycles)) {
    alive_i  <- sum(trace[i, c("Grade_I", "Grade_II", "Grade_III")])
    lys[i]   <- alive_i * params$cycle_length_years
    u_i      <- .get_utilities_cycle(params, treatment, i)
    qalys[i] <- sum(trace[i, ] * u_i) * params$cycle_length_years
    costs[i] <- get_costs(params, treatment, i, trace[i, ])
  }

  list(
    cost = sum(costs * disc),
    qaly = sum(qalys * disc),
    ly   = sum(lys * disc)
  )
}

# Sample PSA draws matrix [n_sim x n_params]
.generate_psa_draws <- function(params, n_sim, se_overrides = NULL) {
  reg <- get_parameter_registry()
  mat <- matrix(NA_real_, nrow = n_sim, ncol = nrow(reg),
                dimnames = list(NULL, reg$name))

  for (i in seq_len(nrow(reg))) {
    nm   <- reg$name[i]
    val  <- params[[nm]]
    se   <- if (!is.null(se_overrides) && !is.null(se_overrides[[nm]])) {
              se_overrides[[nm]]
            } else {
              reg$se[i]
            }
    dist <- reg$distribution[i]
    if (is.null(val) || is.na(se) || se <= 0 || is.na(dist)) {
      mat[, nm] <- val
      next
    }
    mat[, nm] <- switch(dist,
      beta = {
        var_v <- se^2
        if (val > 0 && val < 1 && var_v < val * (1 - val)) {
          alpha_p <- val * (val * (1 - val) / var_v - 1)
          beta_p  <- (1 - val) * (val * (1 - val) / var_v - 1)
          rbeta(n_sim, alpha_p, beta_p)
        } else rep(val, n_sim)
      },
      gamma = {
        if (val > 0) {
          shape_p <- (val / se)^2
          scale_p <- se^2 / val
          rgamma(n_sim, shape = shape_p, scale = scale_p)
        } else rep(0, n_sim)
      },
      utility = {
        # 1 - Gamma on (1 - mean)
        inv <- 1 - val
        if (inv > 0) {
          shape_p <- (inv / se)^2
          scale_p <- se^2 / inv
          pmax(0, 1 - rgamma(n_sim, shape = shape_p, scale = scale_p))
        } else rep(val, n_sim)
      },
      rep(val, n_sim)
    )
  }
  mat
}

# Apply one row of PSA draws to params list
.apply_psa_draw <- function(params, draw_row) {
  psa_p <- params
  reg   <- get_parameter_registry()
  for (nm in reg$name) {
    if (!is.na(draw_row[nm])) psa_p[[nm]] <- draw_row[nm]
  }
  # Derived: surgical CCB proportion
  psa_p$pct_calcblock_surgical <- psa_p$pct_calcblock_soc * 0.50
  psa_p
}

# Simple ICER table (no dampack dependency)
.build_icer_table <- function(summary_df) {
  df <- summary_df[order(summary_df$QALY), ]
  df$Inc_Cost   <- c(NA, diff(df$Cost))
  df$Inc_QALY   <- c(NA, diff(df$QALY))
  df$ICER       <- df$Inc_Cost / df$Inc_QALY
  df$Status     <- ""

  # Simple dominance (strong)
  for (i in seq_len(nrow(df))) {
    if (i == 1) next
    if (!is.na(df$Inc_Cost[i]) && df$Inc_Cost[i] < 0) {
      df$Status[i - 1] <- "Dominated"
      df$Status[i]     <- "Dominates"
    }
  }

  df[, c("Treatment", "Cost", "QALY", "LY", "Inc_Cost", "Inc_QALY", "ICER", "Status")]
}
