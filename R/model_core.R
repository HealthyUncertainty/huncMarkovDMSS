# Internal helpers — not exported

# Life table (fictional sex-blended rates, structural analogue of CDC tables)
.dmss_mortality_table <- data.frame(
  age = c(0, 1, 10, 20, 30, 40, 45, 50, 55, 60, 65, 70, 75, 80, 85, 90, 95, 100),
  qx  = c(
    0.005200, 0.000420, 0.000095, 0.000790, 0.001480, 0.002350,
    0.48 * 0.001950 + 0.52 * 0.002780,   # 45
    0.48 * 0.002920 + 0.52 * 0.005010,   # 50
    0.48 * 0.004500 + 0.52 * 0.007700,   # 55
    0.48 * 0.006620 + 0.52 * 0.011080,   # 60
    0.48 * 0.009320 + 0.52 * 0.015670,   # 65
    0.48 * 0.013970 + 0.52 * 0.021900,   # 70
    0.48 * 0.022470 + 0.52 * 0.032430,   # 75
    0.48 * 0.037910 + 0.52 * 0.052400,   # 80
    0.48 * 0.067120 + 0.52 * 0.089140,   # 85
    0.48 * 0.116290 + 0.52 * 0.154640,   # 90
    0.48 * 0.195200 + 0.52 * 0.247800,   # 95
    1.0
  )
)

# Background mortality cycle probability (interpolated from life table)
.bg_mort_cycle <- function(cycle, params) {
  current_age <- params$age_start + (cycle - 1) * params$cycle_length_years
  if (current_age >= 100) return(1.0)
  age_vec <- .dmss_mortality_table$age
  qx_vec  <- .dmss_mortality_table$qx
  idx_lo  <- max(which(age_vec <= current_age))
  idx_hi  <- min(which(age_vec > current_age))
  if (idx_hi > length(age_vec)) return(qx_vec[length(qx_vec)])
  w      <- (current_age - age_vec[idx_lo]) / (age_vec[idx_hi] - age_vec[idx_lo])
  qx_ann <- qx_vec[idx_lo] * (1 - w) + qx_vec[idx_hi] * w
  1 - (1 - qx_ann)^params$cycle_length_years
}

# Frozen alive-state distribution for a given treatment arm
.frozen_dist <- function(treatment, params) {
  grade_II_frac <- 0.680 / (0.680 + 0.320)
  grade_III_frac <- 1 - grade_II_frac
  if (treatment == "Cardivex") {
    raw <- c(Grade_I = 48.0/100, Grade_II = 43.5/100, Grade_III = 8.5/100)
    # Scale Grade_I by PSA-sampled probability relative to deterministic
    scale_f <- params$p_grade1_cardivex / 0.26
    raw["Grade_I"] <- raw["Grade_I"] * scale_f
    return(raw / sum(raw))
  } else if (treatment == "SoC") {
    raw <- c(Grade_I = 22.0/100, Grade_II = 56.0/100, Grade_III = 22.0/100)
    scale_f <- params$p_grade1_soc / 0.09
    raw["Grade_I"] <- raw["Grade_I"] * scale_f
    return(raw / sum(raw))
  } else {
    p1 <- switch(treatment,
      Zonapride   = params$p_grade1_zonapride,
      Septoplasty = params$p_grade1_septoplasty,
      Ablatherm   = params$p_grade1_ablatherm
    )
    return(c(Grade_I = p1,
             Grade_II  = (1 - p1) * grade_II_frac,
             Grade_III = (1 - p1) * grade_III_frac))
  }
}

#' Generate Markov Trace
#'
#' Generates the n_cycles x 4 population proportion trace for one treatment
#' arm. Grade distributions are frozen after cycle 1 (non-Cardivex/SoC) or
#' cycle 8 (Cardivex/SoC). Perioperative mortality is applied at cycle 2 for
#' surgical arms.
#'
#' @param params Named list of model parameters.
#' @param treatment Character. Treatment name.
#' @return Numeric matrix with columns Grade_I, Grade_II, Grade_III, Dead.
#' @export
#' @examples
#' params <- get_default_parameters()
#' tr <- generate_trace(params, "Cardivex")
#' head(tr)
generate_trace <- function(params, treatment) {
  n_cycles    <- ceiling(params$time_horizon_years / params$cycle_length_years)
  state_names <- c("Grade_I", "Grade_II", "Grade_III", "Dead")
  trace <- matrix(0, nrow = n_cycles, ncol = 4, dimnames = list(NULL, state_names))

  # Cycle 1: baseline (all alive Grade II/III, 68/32 split)
  trace[1, "Grade_II"]  <- 0.680
  trace[1, "Grade_III"] <- 0.320

  frozen <- .frozen_dist(treatment, params)
  p_proc <- if (treatment == "Septoplasty") params$p_mort_septoplasty_proc
            else if (treatment == "Ablatherm") params$p_mort_ablatherm_proc
            else 0

  for (i in 2:n_cycles) {
    prev_alive <- 1 - trace[i - 1, "Dead"]
    p_death_bg <- .bg_mort_cycle(i, params)
    p_death_proc <- if (i == 2) p_proc else 0
    p_survive <- prev_alive * (1 - p_death_bg) * (1 - p_death_proc)
    trace[i, "Grade_I"]   <- p_survive * frozen["Grade_I"]
    trace[i, "Grade_II"]  <- p_survive * frozen["Grade_II"]
    trace[i, "Grade_III"] <- p_survive * frozen["Grade_III"]
    trace[i, "Dead"]      <- 1 - p_survive
  }
  trace
}

#' Get Costs for One Cycle
#'
#' Returns total per-cycle costs for a given treatment, cycle, and trace row.
#' Includes state costs, background medications, treatment drug, procedure
#' costs (cycle 1 only), and cardiac scan costs.
#'
#' @param params Named list of model parameters.
#' @param treatment Character. Treatment name.
#' @param cycle Integer. Current cycle (1-based).
#' @param trace_row Named numeric vector. One row of the Markov trace.
#' @return Numeric. Total cost for this cycle.
#' @export
#' @examples
#' params <- get_default_parameters()
#' tr <- generate_trace(params, "Cardivex")
#' get_costs(params, "Cardivex", cycle = 1, tr[1, ])
get_costs <- function(params, treatment, cycle, trace_row) {
  n_cycles       <- ceiling(params$time_horizon_years / params$cycle_length_years)
  cycles_per_yr  <- round(1 / params$cycle_length_years)
  scan_cycles_cardivex <- c(1, 3, 5)
  scan_cycles_other    <- c(1, seq(1 + cycles_per_yr * 2, n_cycles, by = cycles_per_yr * 2))

  state_costs <- c(Grade_I   = params$cost_grade1_cycle,
                   Grade_II  = params$cost_grade2_cycle,
                   Grade_III = params$cost_grade3_cycle,
                   Dead      = 0)
  base_cost <- sum(trace_row * state_costs)

  alive <- sum(trace_row[c("Grade_I", "Grade_II", "Grade_III")])

  # Background meds
  if (treatment == "Cardivex") {
    pct_bb <- params$pct_betablock_cardivex
    pct_cb <- params$pct_calcblock_cardivex
  } else if (treatment %in% c("SoC", "Zonapride")) {
    pct_bb <- params$pct_betablock_soc
    pct_cb <- params$pct_calcblock_soc
  } else {
    pct_bb <- params$pct_betablock_soc
    pct_cb <- params$pct_calcblock_soc * 0.50
  }
  drug_bg <- if (cycle == 1)
    alive * (pct_bb * params$cost_betablock_c1  + pct_cb * params$cost_calcblock_c1)
  else
    alive * (pct_bb * params$cost_betablock_cycle + pct_cb * params$cost_calcblock_cycle)

  # Treatment-specific drug cost
  drug_tx <- 0
  if (treatment == "Cardivex") {
    drug_tx <- alive * params$cost_cardivex_cycle
  } else if (treatment == "Zonapride") {
    drug_tx <- alive * if (cycle == 1) params$cost_zonapride_c1 else params$cost_zonapride_cycle
  }

  # Procedure costs (cycle 1 only)
  proc_cost <- 0
  if (cycle == 1) {
    if (treatment == "Septoplasty") proc_cost <- alive * params$cost_septoplasty_proc
    if (treatment == "Ablatherm")   proc_cost <- alive * params$cost_ablatherm_proc
    if (treatment == "Zonapride")   proc_cost <- proc_cost + alive * params$cost_zonapride_hosp
  }

  # Cardiac scan costs
  scan_cost <- 0
  if (treatment == "Cardivex" && cycle %in% scan_cycles_cardivex) {
    scan_cost <- alive * params$cost_scan
  } else if (treatment != "Cardivex" && cycle %in% scan_cycles_other) {
    scan_cost <- alive * params$cost_scan
  }

  base_cost + drug_bg + drug_tx + proc_cost + scan_cost
}

# Internal: per-cycle utility vector for one treatment arm and cycle
.get_utilities_cycle <- function(params, treatment, cycle) {
  current_age <- params$age_start + (cycle - 1) * params$cycle_length_years
  age_dec <- params$age_decrement_per_year * (current_age - params$age_start)

  if (treatment == "Cardivex") {
    u <- c(Grade_I = params$u_grade1_cardivex, Grade_II = params$u_grade2_cardivex,
           Grade_III = params$u_grade3_cardivex, Dead = 0)
  } else if (treatment == "SoC") {
    u <- c(Grade_I = params$u_grade1_soc, Grade_II = params$u_grade2_soc,
           Grade_III = params$u_grade3_soc, Dead = 0)
  } else {
    u <- c(Grade_I = params$u_grade1_other, Grade_II = params$u_grade2_other,
           Grade_III = params$u_grade3_other, Dead = 0)
  }
  u[1:3] <- pmax(0, u[1:3] - age_dec)

  if (treatment == "Septoplasty") {
    if (cycle <= 6) u[1:3] <- pmax(0, u[1:3] - params$disutility_septoplasty_proc)
    u[1:3] <- u[1:3] - params$disutility_pacemaker * params$pct_pacemaker_septoplasty
  }
  if (treatment == "Ablatherm") {
    if (cycle == 2) u[1:3] <- pmax(0, u[1:3] - params$disutility_ablatherm_proc)
    u[1:3] <- u[1:3] - params$disutility_pacemaker * params$pct_pacemaker_ablatherm
  }
  u
}
