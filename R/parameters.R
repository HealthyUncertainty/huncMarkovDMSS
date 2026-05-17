#' Get Default Model Parameters
#'
#' Returns the full set of default parameters for the DMSS cost-effectiveness
#' model. All monetary values are in Canadian dollars (CAD).
#'
#' @return Named list of model parameters.
#' @export
#' @examples
#' params <- get_default_parameters()
#' params$time_horizon_years
get_default_parameters <- function() {
  list(
    # ---- Structural ----
    cycle_length_years      = 4 / 52,
    time_horizon_years      = 80,
    age_start               = 55,
    pct_female              = 0.48,
    discount_rate_annual    = 0.03,
    wtp_threshold           = 150000,
    apply_hcc               = FALSE,

    # ---- Perioperative mortality ----
    p_mort_septoplasty_proc = 0.011,
    p_mort_ablatherm_proc   = 0.009,

    # ---- Grade response probabilities (cycle 1) ----
    p_grade1_cardivex    = 0.26,
    p_grade1_soc         = 0.09,
    p_grade1_zonapride   = 0.30,
    p_grade1_septoplasty = 0.72,
    p_grade1_ablatherm   = 0.72,

    # ---- Utilities ----
    age_decrement_per_year       = 0.0008,
    u_grade1_cardivex            = 0.93,
    u_grade2_cardivex            = 0.85,
    u_grade3_cardivex            = 0.69,
    u_grade1_soc                 = 0.93,
    u_grade2_soc                 = 0.83,
    u_grade3_soc                 = 0.68,
    u_grade1_other               = 0.93,
    u_grade2_other               = 0.84,
    u_grade3_other               = 0.69,
    disutility_septoplasty_proc  = 0.08,
    disutility_ablatherm_proc    = 0.04,
    disutility_pacemaker         = 0.04,
    pct_pacemaker_septoplasty    = 0.05,
    pct_pacemaker_ablatherm      = 0.12,

    # ---- Drug costs (per cycle) ----
    cost_cardivex_cycle  = 4923,
    cost_betablock_c1    = 42,
    cost_betablock_cycle = 58,
    cost_calcblock_c1    = 45,
    cost_calcblock_cycle = 52,
    cost_zonapride_c1    = 285,
    cost_zonapride_cycle = 380,

    # ---- Drug utilisation proportions ----
    pct_betablock_cardivex = 0.72,
    pct_betablock_soc      = 0.70,
    pct_calcblock_cardivex = 0.18,
    pct_calcblock_soc      = 0.14,

    # ---- State costs (per cycle) ----
    cost_grade1_cycle = 680,
    cost_grade2_cycle = 1850,
    cost_grade3_cycle = 2560,

    # ---- Procedure costs (one-time, cycle 1) ----
    cost_septoplasty_proc = 110000,
    cost_ablatherm_proc   = 50000,
    cost_zonapride_hosp   = 7800,
    cost_scan             = 95
  )
}

#' Get Parameter Registry
#'
#' Returns metadata for all PSA-sampled parameters.
#'
#' @return A data.frame with columns: name, display_name, default_value,
#'   category, type, min, max, step, se, distribution, dist_note.
#' @export
#' @examples
#' reg <- get_parameter_registry()
#' head(reg)
get_parameter_registry <- function() {

  se_from_range <- function(lower, upper) (upper - lower) / 3.92

  p <- function(name, display_name, default_value, category, type,
                min = NA, max = NA, step = NA,
                se = NA, distribution = NA, dist_note = "") {
    data.frame(
      name          = name,
      display_name  = display_name,
      default_value = default_value,
      category      = category,
      type          = type,
      min           = min,
      max           = max,
      step          = step,
      se            = se,
      distribution  = distribution,
      dist_note     = dist_note,
      stringsAsFactors = FALSE
    )
  }

  registry <- rbind(
    # ---- Response probabilities ----
    p("p_grade1_cardivex",    "Prob: Grade I Response (Cardivex)",           0.26, "Response Probabilities", "probability", 0, 1, 0.01, se_from_range(0.19,0.33), "beta",  "95% CI 0.19-0.33"),
    p("p_grade1_soc",         "Prob: Grade I Response (SoC)",                0.09, "Response Probabilities", "probability", 0, 1, 0.01, se_from_range(0.07,0.11), "beta",  "95% CI 0.07-0.11"),
    p("p_grade1_zonapride",   "Prob: Grade I Response (Zonapride)",          0.30, "Response Probabilities", "probability", 0, 1, 0.01, se_from_range(0.22,0.38), "beta",  "95% CI 0.22-0.38"),
    p("p_grade1_septoplasty", "Prob: Grade I Response (Septoplasty)",        0.72, "Response Probabilities", "probability", 0, 1, 0.01, se_from_range(0.54,0.90), "beta",  "95% CI 0.54-0.90"),
    p("p_grade1_ablatherm",   "Prob: Grade I Response (Ablatherm)",          0.72, "Response Probabilities", "probability", 0, 1, 0.01, se_from_range(0.54,0.90), "beta",  "95% CI 0.54-0.90"),

    # ---- Utilities: Cardivex ----
    p("u_grade1_cardivex", "Utility: Grade I (Cardivex)",  0.93, "Utilities - Cardivex", "utility", 0, 1, 0.01, se_from_range(0.64,0.99), "utility", "1-Gamma; 95% CI 0.64-0.99"),
    p("u_grade2_cardivex", "Utility: Grade II (Cardivex)", 0.85, "Utilities - Cardivex", "utility", 0, 1, 0.01, se_from_range(0.64,0.96), "utility", "1-Gamma; 95% CI 0.64-0.96"),
    p("u_grade3_cardivex", "Utility: Grade III (Cardivex)",0.69, "Utilities - Cardivex", "utility", 0, 1, 0.01, se_from_range(0.54,0.82), "utility", "1-Gamma; 95% CI 0.54-0.82"),

    # ---- Utilities: SoC ----
    p("u_grade1_soc", "Utility: Grade I (SoC)",  0.93, "Utilities - SoC", "utility", 0, 1, 0.01, se_from_range(0.64,0.99), "utility", "1-Gamma; 95% CI 0.64-0.99"),
    p("u_grade2_soc", "Utility: Grade II (SoC)", 0.83, "Utilities - SoC", "utility", 0, 1, 0.01, se_from_range(0.63,0.95), "utility", "1-Gamma; 95% CI 0.63-0.95"),
    p("u_grade3_soc", "Utility: Grade III (SoC)",0.68, "Utilities - SoC", "utility", 0, 1, 0.01, se_from_range(0.54,0.81), "utility", "1-Gamma; 95% CI 0.54-0.81"),

    # ---- Utilities: Other comparators ----
    p("u_grade1_other", "Utility: Grade I (Zonapride/Surgical)",  0.93, "Utilities - Other", "utility", 0, 1, 0.01, se_from_range(0.64,0.99), "utility", "1-Gamma; 95% CI 0.64-0.99"),
    p("u_grade2_other", "Utility: Grade II (Zonapride/Surgical)", 0.84, "Utilities - Other", "utility", 0, 1, 0.01, se_from_range(0.63,0.96), "utility", "1-Gamma; 95% CI 0.63-0.96"),
    p("u_grade3_other", "Utility: Grade III (Zonapride/Surgical)",0.69, "Utilities - Other", "utility", 0, 1, 0.01, se_from_range(0.54,0.82), "utility", "1-Gamma; 95% CI 0.54-0.82"),

    # ---- State costs ----
    p("cost_grade1_cycle", "State Cost: Grade I (per cycle, CAD)",  680,  "Costs - State", "cost", 0, NA, 10, se_from_range(553,820),   "gamma", "95% CI 553-820"),
    p("cost_grade2_cycle", "State Cost: Grade II (per cycle, CAD)", 1850, "Costs - State", "cost", 0, NA, 10, se_from_range(1505,2230), "gamma", "95% CI 1505-2230"),
    p("cost_grade3_cycle", "State Cost: Grade III (per cycle, CAD)",2560, "Costs - State", "cost", 0, NA, 10, se_from_range(2083,3086), "gamma", "95% CI 2083-3086"),

    # ---- Drug costs ----
    p("cost_cardivex_cycle",  "Drug Cost: Cardivex (per cycle, CAD)", 4923, "Costs - Drug", "cost", 0, NA, 100, se_from_range(4006,5932), "gamma", "95% CI 4006-5932"),
    p("cost_zonapride_cycle", "Drug Cost: Zonapride (per cycle, CAD)", 380, "Costs - Drug", "cost", 0, NA, 10,  se_from_range(309,458),   "gamma", "95% CI 309-458"),

    # ---- Background medications ----
    p("pct_betablock_cardivex", "Pct: Beta-Blocker Use (Cardivex arm)", 0.72, "Background Meds", "probability", 0, 1, 0.01, se_from_range(0.54,0.90), "beta", "95% CI 0.54-0.90"),
    p("pct_betablock_soc",      "Pct: Beta-Blocker Use (SoC/other)",    0.70, "Background Meds", "probability", 0, 1, 0.01, se_from_range(0.53,0.87), "beta", "95% CI 0.53-0.87"),
    p("pct_calcblock_cardivex", "Pct: CCB Use (Cardivex arm)",          0.18, "Background Meds", "probability", 0, 1, 0.01, se_from_range(0.14,0.23), "beta", "95% CI 0.14-0.23"),
    p("pct_calcblock_soc",      "Pct: CCB Use (SoC/other arms)",        0.14, "Background Meds", "probability", 0, 1, 0.01, se_from_range(0.10,0.17), "beta", "95% CI 0.10-0.17")
  )

  registry
}

#' Validate Model Parameters
#'
#' @param params Named list of model parameters.
#' @return NULL (invisible). Throws error if validation fails.
#' @export
#' @examples
#' params <- get_default_parameters()
#' validate_parameters(params)
validate_parameters <- function(params) {
  required <- c("cycle_length_years", "time_horizon_years", "discount_rate_annual")
  missing  <- setdiff(required, names(params))
  if (length(missing) > 0)
    stop("Missing required parameters: ", paste(missing, collapse = ", "), call. = FALSE)
  if (params$time_horizon_years <= 0)
    stop("time_horizon_years must be positive", call. = FALSE)
  if (params$discount_rate_annual < 0 || params$discount_rate_annual > 1)
    stop("discount_rate_annual must be between 0 and 1", call. = FALSE)
  reg <- get_parameter_registry()
  for (i in seq_len(nrow(reg))) {
    nm  <- reg$name[i]
    val <- params[[nm]]
    if (is.null(val)) next
    if (!is.na(reg$min[i]) && val < reg$min[i])
      stop(nm, " must be >= ", reg$min[i], call. = FALSE)
    if (!is.na(reg$max[i]) && val > reg$max[i])
      stop(nm, " must be <= ", reg$max[i], call. = FALSE)
  }
  invisible(NULL)
}
