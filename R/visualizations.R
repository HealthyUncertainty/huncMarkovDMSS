# Visualization functions for the DMSS cost-effectiveness model
# Requires: ggplot2, dplyr (in Suggests)

globalVariables(c(
  "treatment", "inc_cost", "inc_qaly", "mean_inc_cost", "mean_inc_qaly",
  "arm", "sim", "cost", "qaly", "nmb", "n", "prob", "wtp",
  "on_frontier", "Treatment", "Cost", "QALY"
))

#' Plot CE Plane (multi-comparator PSA)
#'
#' @param psa_results Wide-format PSA data frame from run_model().
#' @param reference Reference arm name (default: "SoC").
#' @param wtp WTP threshold line (default: 150000).
#' @return ggplot object.
#' @export
#' @importFrom ggplot2 ggplot aes geom_point stat_ellipse geom_hline geom_vline
#'   geom_abline scale_y_continuous guides guide_legend labs theme_minimal theme
#'   element_text element_blank margin
#' @importFrom scales comma
plot_ce_plane <- function(psa_results, reference = "SoC", wtp = 150000) {
  if (!requireNamespace("ggplot2", quietly = TRUE))
    stop("ggplot2 is required for plot_ce_plane()")
  if (!requireNamespace("dplyr", quietly = TRUE))
    stop("dplyr is required for plot_ce_plane()")

  cost_cols <- grep("_cost$", names(psa_results), value = TRUE)
  treatments <- sub("_cost$", "", cost_cols)
  comparators <- treatments[treatments != reference]

  ref_cost <- psa_results[[paste0(reference, "_cost")]]
  ref_qaly <- psa_results[[paste0(reference, "_qaly")]]

  inc_data <- do.call(rbind, lapply(comparators, function(tx) {
    data.frame(
      treatment = tx,
      inc_cost  = psa_results[[paste0(tx, "_cost")]] - ref_cost,
      inc_qaly  = psa_results[[paste0(tx, "_qaly")]] - ref_qaly,
      stringsAsFactors = FALSE
    )
  }))

  mean_data <- inc_data %>%
    dplyr::group_by(treatment) %>%
    dplyr::summarise(mean_inc_cost = mean(inc_cost),
                     mean_inc_qaly = mean(inc_qaly), .groups = "drop")

  ggplot2::ggplot() +
    ggplot2::geom_point(data = inc_data,
      ggplot2::aes(x = inc_qaly, y = inc_cost, color = treatment), alpha = 0.25, size = 0.8) +
    ggplot2::stat_ellipse(data = inc_data,
      ggplot2::aes(x = inc_qaly, y = inc_cost, color = treatment),
      level = 0.95, type = "norm", linewidth = 0.8, linetype = "dotted") +
    ggplot2::geom_point(data = mean_data,
      ggplot2::aes(x = mean_inc_qaly, y = mean_inc_cost, color = treatment),
      size = 4, shape = 17) +
    ggplot2::geom_hline(yintercept = 0, linetype = "dashed", color = "gray50") +
    ggplot2::geom_vline(xintercept = 0, linetype = "dashed", color = "gray50") +
    ggplot2::geom_abline(slope = wtp, intercept = 0, color = "red",
                         linetype = "solid", linewidth = 0.5) +
    ggplot2::scale_y_continuous(labels = scales::comma) +
    ggplot2::guides(color = ggplot2::guide_legend(
      override.aes = list(shape = 17, size = 4, alpha = 1, linetype = 0))) +
    ggplot2::labs(
      title    = "Cost-Effectiveness Plane",
      subtitle = paste0("Reference: ", reference, " | WTP: CAD $", format(wtp, big.mark = ",")),
      x = "Incremental QALYs", y = "Incremental Costs (CAD)", color = "Treatment") +
    ggplot2::theme_minimal() +
    ggplot2::theme(
      plot.title   = ggplot2::element_text(face = "bold", size = 14),
      legend.position = "right",
      panel.grid.minor = ggplot2::element_blank())
}

#' Plot Cost-Effectiveness Acceptability Curve
#'
#' @param psa_results Wide-format PSA data frame from run_model().
#' @param wtp_range Numeric vector of WTP thresholds.
#' @return ggplot object.
#' @export
#' @importFrom ggplot2 ggplot aes geom_line scale_y_continuous scale_x_continuous
#'   labs theme_minimal theme element_text element_blank
#' @importFrom scales percent dollar
plot_ceac <- function(psa_results,
                      wtp_range = seq(0, 300000, by = 5000)) {
  if (!requireNamespace("ggplot2", quietly = TRUE))
    stop("ggplot2 is required for plot_ceac()")
  if (!requireNamespace("dplyr", quietly = TRUE))
    stop("dplyr is required for plot_ceac()")

  cost_cols  <- grep("_cost$", names(psa_results), value = TRUE)
  treatments <- sub("_cost$", "", cost_cols)
  n_sim      <- nrow(psa_results)

  long_df <- do.call(rbind, lapply(treatments, function(tx) {
    data.frame(sim = seq_len(n_sim), arm = tx,
               cost = psa_results[[paste0(tx, "_cost")]],
               qaly = psa_results[[paste0(tx, "_qaly")]],
               stringsAsFactors = FALSE)
  }))

  ceac_data <- do.call(rbind, lapply(wtp_range, function(w) {
    nmb_df <- long_df
    nmb_df$nmb <- nmb_df$qaly * w - nmb_df$cost
    best <- tapply(nmb_df$nmb, nmb_df$sim, function(x) {
      which.max(x)
    })
    winner_arms <- treatments[unlist(best)]
    counts <- table(factor(winner_arms, levels = treatments))
    data.frame(arm = names(counts), wtp = w,
               prob = as.numeric(counts) / n_sim,
               stringsAsFactors = FALSE)
  }))

  ggplot2::ggplot(ceac_data, ggplot2::aes(x = wtp, y = prob, color = arm)) +
    ggplot2::geom_line(linewidth = 1.2) +
    ggplot2::scale_y_continuous(labels = scales::percent, limits = c(0, 1)) +
    ggplot2::scale_x_continuous(labels = scales::dollar) +
    ggplot2::labs(
      title    = "Cost-Effectiveness Acceptability Curve",
      subtitle = "Probability each treatment is cost-effective across WTP thresholds",
      x = "Willingness-to-Pay Threshold (CAD)", y = "Probability Cost-Effective",
      color = "Treatment") +
    ggplot2::theme_minimal() +
    ggplot2::theme(
      plot.title = ggplot2::element_text(face = "bold", size = 14),
      panel.grid.minor = ggplot2::element_blank())
}

#' Plot Cost-Effectiveness Frontier
#'
#' @param results_summary Data frame from run_model()$results_summary.
#' @return ggplot object.
#' @export
#' @importFrom ggplot2 ggplot aes geom_line geom_point geom_text
#'   scale_color_manual scale_shape_manual scale_y_continuous scale_x_continuous
#'   labs theme_minimal theme element_text element_blank
plot_frontier <- function(results_summary) {
  if (!requireNamespace("ggplot2", quietly = TRUE))
    stop("ggplot2 is required for plot_frontier()")

  df <- results_summary[order(results_summary$QALY), ]

  # Extended dominance check
  frontier_idx <- 1L
  for (i in seq(2L, nrow(df))) {
    if (df$Cost[i] > df$Cost[frontier_idx[length(frontier_idx)]]) {
      if (length(frontier_idx) > 1) {
        prev <- frontier_idx[length(frontier_idx) - 1L]
        last <- frontier_idx[length(frontier_idx)]
        icer_pl <- (df$Cost[last]  - df$Cost[prev]) / (df$QALY[last]  - df$QALY[prev])
        icer_pc <- (df$Cost[i]     - df$Cost[prev]) / (df$QALY[i]     - df$QALY[prev])
        if (icer_pl < icer_pc) frontier_idx <- frontier_idx[-length(frontier_idx)]
      }
      frontier_idx <- c(frontier_idx, i)
    }
  }
  df$on_frontier <- seq_len(nrow(df)) %in% frontier_idx

  ggplot2::ggplot(df, ggplot2::aes(x = QALY, y = Cost)) +
    ggplot2::geom_line(data = df[df$on_frontier, ],
      ggplot2::aes(group = 1), color = "steelblue", linewidth = 1.2) +
    ggplot2::geom_point(ggplot2::aes(color = on_frontier, shape = on_frontier), size = 4) +
    ggplot2::geom_text(ggplot2::aes(label = Treatment), vjust = -1.2, size = 3.5, fontface = "bold") +
    ggplot2::scale_color_manual(values = c("TRUE" = "darkgreen", "FALSE" = "gray50"),
                                labels = c("TRUE" = "On Frontier", "FALSE" = "Dominated")) +
    ggplot2::scale_shape_manual(values = c("TRUE" = 16L, "FALSE" = 4L),
                                labels = c("TRUE" = "On Frontier", "FALSE" = "Dominated")) +
    ggplot2::scale_y_continuous(labels = scales::comma) +
    ggplot2::labs(title = "Cost-Effectiveness Frontier",
                  subtitle = "Non-dominated strategies (extended dominance checked)",
                  x = "Total QALYs", y = "Total Costs (CAD)",
                  color = "Status", shape = "Status") +
    ggplot2::theme_minimal() +
    ggplot2::theme(plot.title = ggplot2::element_text(face = "bold", size = 14),
                   legend.position = "bottom",
                   panel.grid.minor = ggplot2::element_blank())
}

#' Plot Markov Trace
#'
#' @param trace_data Named list of trace matrices (from run_model()$trace_data).
#' @param cycle_length_years Cycle length in years (for x-axis scaling).
#' @return ggplot object.
#' @export
#' @importFrom ggplot2 ggplot aes geom_line facet_wrap scale_y_continuous
#'   labs theme_minimal theme element_text element_blank
plot_trace <- function(trace_data, cycle_length_years = 4/52) {
  if (!requireNamespace("ggplot2", quietly = TRUE))
    stop("ggplot2 is required for plot_trace()")

  state <- NULL  # avoid R CMD check note

  long_list <- lapply(names(trace_data), function(trt) {
    tr <- trace_data[[trt]]
    n  <- nrow(tr)
    data.frame(
      Treatment = trt,
      Year      = (seq_len(n) - 1) * cycle_length_years,
      Grade_I   = tr[, "Grade_I"],
      Grade_II  = tr[, "Grade_II"],
      Grade_III = tr[, "Grade_III"],
      Dead      = tr[, "Dead"],
      stringsAsFactors = FALSE
    )
  })
  df <- do.call(rbind, long_list)

  # Reshape to long format manually (avoid reshape2/tidyr dependency)
  state_cols <- c("Grade_I", "Grade_II", "Grade_III", "Dead")
  long_df <- do.call(rbind, lapply(state_cols, function(s) {
    data.frame(Treatment = df$Treatment, Year = df$Year,
               state = s, value = df[[s]], stringsAsFactors = FALSE)
  }))

  ggplot2::ggplot(long_df, ggplot2::aes(x = Year, y = value, color = state)) +
    ggplot2::geom_line(linewidth = 0.8) +
    ggplot2::facet_wrap(~Treatment, ncol = 3L) +
    ggplot2::scale_y_continuous(labels = scales::percent) +
    ggplot2::labs(title = "Markov Trace — State Occupancy Over Time",
                  x = "Year", y = "Proportion of Cohort", color = "State") +
    ggplot2::theme_minimal() +
    ggplot2::theme(plot.title = ggplot2::element_text(face = "bold", size = 14),
                   panel.grid.minor = ggplot2::element_blank(),
                   strip.text = ggplot2::element_text(face = "bold"))
}
