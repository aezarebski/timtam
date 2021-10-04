suppressPackageStartupMessages(library(argparse))
suppressPackageStartupMessages(library(coda))
suppressPackageStartupMessages(library(ggplot2))
suppressPackageStartupMessages(library(reshape2))
suppressPackageStartupMessages(library(purrr))
suppressPackageStartupMessages(library(dplyr))

green_hex_colour <- "#7fc97f"
purple_hex_colour <- "#beaed4"

parser <- ArgumentParser()

parser$add_argument(
  "--input",
  type = "character",
  help = "Filepath containing summary."
)
parser$add_argument(
  "--output",
  type = "character",
  help = "Filepath to write plot to."
)


make_prevalence_comparison <- function(args) {
  input_list <- jsonlite::read_json(args$input, simplifyVector = TRUE)
  true_prevalence <- input_list$prevalence

  plot_df <- input_list$estimates |>
    filter(variable == "prevalence") |>
    select(-variable) |>
    dcast(replicate + from_aggregated ~ quantile)
  names(plot_df) <- c(c("replicate", "from_aggregated"), sprintf("q%d", 1:5))
  plot_df <- left_join(x = plot_df, y = true_prevalence, by = "replicate")

  plot_df <- plot_df[order(plot_df$prevalence), ]
  ## add another variable which can be used on the x-axis while respecting the
  ## prevalence ordering.
  plot_df$sorted_order <- sort(plot_df$replicate)

  facet_labels <- c("TRUE" = "Aggregated data", "FALSE" = "Exact sampling dates")

  prev_fig_1 <- ggplot(data = plot_df) +
    geom_linerange(mapping = aes(x = sorted_order, ymin = q1, ymax = q5, colour = from_aggregated)) +
    geom_point(mapping = aes(x = sorted_order, y = prevalence)) +
    facet_grid(from_aggregated ~ ., scale = "free_y", labeller = labeller(from_aggregated = facet_labels)) +
    labs(x = "Replicate", y = "Prevalence", colour = NULL) +
    scale_colour_manual(values = c(green_hex_colour, purple_hex_colour)) +
    theme_classic() +
    theme(
      axis.text.x = element_blank(),
      strip.background = element_blank(),
      legend.position = "null"
    )
  ## print(prev_fig_1)
  ggsave(filename = "out/prevalence-calibration-extra-1.png", plot = prev_fig_1, height = 14.8, width = 10.5, units = "cm")

  prev_fig_2 <- ggplot(data = plot_df) +
    geom_linerange(
      mapping = aes(
        x = sorted_order,
        ymin = (q1 - prevalence) / prevalence,
        ymax = (q5 - prevalence) / prevalence,
        colour = from_aggregated
      )
    ) +
    facet_grid(from_aggregated ~ ., scale = "free_y", labeller = labeller(from_aggregated = facet_labels)) +
    geom_hline(yintercept = 0, linetype = "dashed") +
    ## geom_hline(mapping = aes(yintercept = median((q3 - prevalence) / prevalence),
    ##                          colour = from_aggregated)) +
    scale_colour_manual(values = c(green_hex_colour, purple_hex_colour)) +
    labs(x = "Replicate", y = "Proportional error", colour = NULL) +
    theme_classic() +
    theme(
      axis.text.x = element_blank(),
      strip.background = element_blank(),
      legend.position = "null"
    )
  ## print(prev_fig_2)
  ggsave(filename = args$output, plot = prev_fig_2, height = 14.8, width = 10.5, units = "cm")
}

make_prev_r_naught_fig <- function(args) {
  true_params <- jsonlite::read_json("../example-parameters.json")
  true_r_naught <- true_params$birthRate / (true_params$deathRate + true_params$samplingRate + true_params$occurrenceRate)
  input_list <- jsonlite::read_json(args$input, simplifyVector = TRUE)
  true_prevalence <- input_list$prevalence
  plot_df <- input_list$estimates |>
    filter(variable == "prevalence" | variable == "r_naught", !from_aggregated) |>
    select(-from_aggregated) |>
    dcast(variable + replicate ~ quantile)
  names(plot_df) <- c(c("variable", "replicate"), sprintf("q%d", 1:5))
  plot_df <- left_join(x = plot_df, y = true_prevalence, by = "replicate")

  ## sort first by replicate so this is used as a tie-breaker.
  plot_df <- plot_df[order(plot_df$replicate), ]
  plot_df <- plot_df[order(plot_df$prevalence), ]
  ## add another variable which can be used on the x-axis while respecting the
  ## prevalence ordering.
  plot_df$sorted_order <- sort(plot_df$replicate)

  ## convert the prevalence estimates to relative errors
  prev_mask <- plot_df$variable == "prevalence"
  plot_df[prev_mask, ]$q1 <- (plot_df[prev_mask, ]$q1 - plot_df[prev_mask, ]$prevalence) / plot_df[prev_mask, ]$prevalence
  plot_df[prev_mask, ]$q2 <- (plot_df[prev_mask, ]$q2 - plot_df[prev_mask, ]$prevalence) / plot_df[prev_mask, ]$prevalence
  plot_df[prev_mask, ]$q3 <- (plot_df[prev_mask, ]$q3 - plot_df[prev_mask, ]$prevalence) / plot_df[prev_mask, ]$prevalence
  plot_df[prev_mask, ]$q4 <- (plot_df[prev_mask, ]$q4 - plot_df[prev_mask, ]$prevalence) / plot_df[prev_mask, ]$prevalence
  plot_df[prev_mask, ]$q5 <- (plot_df[prev_mask, ]$q5 - plot_df[prev_mask, ]$prevalence) / plot_df[prev_mask, ]$prevalence

  facet_labels <- c("prevalence" = "Prevalence", "r_naught" = "Reproduction number")

  prev_fig_1 <- ggplot(data = plot_df) +
    geom_linerange(
      mapping = aes(x = sorted_order, ymin = q1, ymax = q5),
      colour = green_hex_colour
    ) +
    geom_point(
      mapping = aes(x = sorted_order, y = q3),
      colour = green_hex_colour
    ) +
    geom_hline(
      data = data.frame(yi = c(0, true_r_naught), variable = c("prevalence", "r_naught")),
      mapping = aes(yintercept = yi),
      linetype = "dashed"
    ) +
    facet_grid(variable ~ .,
      scale = "free_y",
      labeller = labeller(variable = facet_labels)
    ) +
    labs(x = "Replicate (ordered by prevalence)", y = NULL, colour = NULL) +
    theme_classic() +
    theme(
      axis.text.x = element_blank(),
      strip.background = element_blank(),
      legend.position = "null"
    )
  ## print(prev_fig_1)
  ggsave(filename = "out/prevalence-calibration-extra-2.png", plot = prev_fig_1, height = 14.8, width = 10.5, units = "cm")
}


calibration_plot_fig <- function(plot_df_1, plot_df_2, facet_labels, hex_colour) {
  ggplot() +
    geom_linerange(
      data = plot_df_1,
      mapping = aes(x = sorted_ordering, ymin = q1, ymax = q5),
      colour = hex_colour
    ) +
    geom_point(
      data = plot_df_1,
      mapping = aes(x = sorted_ordering, y = q3),
      colour = hex_colour
    ) +
    geom_hline(
      data = plot_df_2,
      mapping = aes(yintercept = values),
      linetype = "dashed"
    ) +
    facet_grid(variable ~ .,
      scales = "free_y",
      labeller = labeller(variable = facet_labels)
    ) +
    labs(x = "Replicate", y = NULL) +
    theme_classic() +
    theme(
      strip.background = element_blank(),
      axis.text.x = element_blank()
    )
}

make_estimate_calibration_plot <- function(args, from_aggregated) {
  true_params <- jsonlite::read_json("../example-parameters.json")
  input_list <- jsonlite::read_json(args$input, simplifyVector = TRUE)

  if (from_aggregated) {
    var_names <- c("birth_rate", "rho_prob", "nu_prob")
    plot_df_1 <- input_list$estimates |>
      filter(from_aggregated == TRUE) |>
      select(-from_aggregated) |>
      filter(is.element(variable, var_names)) |>
      dcast(variable + replicate ~ quantile)
    names(plot_df_1) <- c(c("variable", "replicate"), sprintf("q%d", 1:5))
    plot_df_1 <- plot_df_1[order(plot_df_1$q3), ]
    plot_df_1 <- plot_df_1[order(plot_df_1$variable), ]
    plot_df_1$sorted_ordering <- rep(seq.int(nrow(plot_df_1) / 3), 3)

    plot_df_2 <- data.frame(
      variable = var_names[1],
      values = true_params$birthRate
    )

    est_coverage_df <- plot_df_1 |>
      filter(variable == "birth_rate") |>
      mutate(contains = q1 <= true_params$birthRate & true_params$birthRate <= q5) |>
      count(contains) |>
      as.data.frame()
    write.table(
      x = est_coverage_df,
      file = "out/aggregated-estimates-coverage.csv",
      sep = ",",
      row.names = FALSE
    )

    facet_labels <- c(
      birth_rate = "Birth rate",
      rho_prob = "Sequenced probability",
      nu_prob = "Unsequenced probability"
    )

    hex_colour <- purple_hex_colour
    plot_png <- "out/aggregated-estimate-calibration.png"
  } else {
    var_names <- c("birth_rate", "sampling_rate", "omega_rate")
    plot_df_1 <- input_list$estimates |>
      filter(from_aggregated == FALSE) |>
      select(-from_aggregated) |>
      filter(is.element(variable, var_names)) |>
      dcast(variable + replicate ~ quantile)
    names(plot_df_1) <- c(c("variable", "replicate"), sprintf("q%d", 1:5))
    plot_df_1 <- plot_df_1[order(plot_df_1$q3), ]
    plot_df_1 <- plot_df_1[order(plot_df_1$variable), ]
    plot_df_1$sorted_ordering <- rep(seq.int(nrow(plot_df_1) / 3), 3)

    plot_df_2 <- data.frame(
      variable = var_names,
      values = c(true_params$birthRate, true_params$samplingRate, true_params$occurrenceRate)
    )

    est_coverage_df <- plot_df_1 |>
      select(variable, q1, q5) |>
      left_join(plot_df_2, by = "variable") |>
      mutate(contains = q1 <= values & values <= q5) |>
      group_by(variable) |>
      count(contains)
    write.table(
      x = est_coverage_df,
      file = "out/estimates-coverage.csv",
      sep = ",",
      row.names = FALSE
    )

    facet_labels <- c(
      birth_rate = "Birth rate",
      sampling_rate = "Sampling rate",
      omega_rate = "Occurrence rate"
    )

    hex_colour <- green_hex_colour
    plot_png <- "out/estimate-calibration.png"
  }

  g <- calibration_plot_fig(plot_df_1, plot_df_2, facet_labels, hex_colour)

  ggsave(filename = plot_png, plot = g, height = 22.2, width = 10.5, units = "cm")
}

make_effective_size_plot <- function(args) {
  input_list <- jsonlite::read_json(args$input, simplifyVector = TRUE)

  tmp <- input_list$diagnostics$effectiveSize
  tmp_agg <- tmp$aggregated |>
    select(birth_rate, rho_prob, nu_prob, replicate, from_aggregated) |>
    set_names(c(sprintf("v%d", 1:3), "replicate", "from_aggregated"))
  tmp_not_agg <- tmp$not_aggregated |>
    select(birth_rate, sampling_rate, omega_rate, replicate, from_aggregated) |>
    set_names(c(sprintf("v%d", 1:3), "replicate", "from_aggregated"))
  plot_df <- rbind(tmp_agg, tmp_not_agg) |>
    melt(id.vars = c("replicate", "from_aggregated"))

  g <- ggplot() +
    geom_point(
      data = plot_df,
      mapping = aes(x = replicate, y = value, colour = variable)
    ) +
    geom_hline(yintercept = 200, linetype = "dashed") +
    scale_y_log10() +
    labs(x = "Replicate", y = "Effective sample size") +
    facet_wrap(~from_aggregated) +
    theme_classic() +
    theme(legend.position = "none")

  ggsave(
    filename = "out/effective-sample-sizes.png",
    plot = g,
    height = 21.0,
    width = 29.7,
    units = "cm"
  )
}

record_prev_coverage_est <- function(args) {
  input_list <- jsonlite::read_json(args$input, simplifyVector = TRUE)

  ## The summarise function throws a warning but from looking on stack exchange
  ## it looks like this is harmless.
  prevalence_coverage_df <- input_list$estimates |>
    filter(
      variable == "prevalence",
      is.element(quantile, c("2.5%", "97.5%"))
    ) |>
    left_join(input_list$prevalence, by = "replicate") |>
    select(-variable) |>
    group_by(replicate, from_aggregated) |>
    ## 0.5 here because each quantile gets its own row.
    summarise(contains = 0.5 * sum(min(value) <= prevalence & prevalence <= max(value))) |>
    group_by(from_aggregated) |>
    summarise(
      num_contains = sum(contains),
      num_not_contains = sum(!contains)
    ) |>
    as.data.frame()

  write.table(
    x = prevalence_coverage_df,
    file = "out/prevalence-coverage-table.csv",
    sep = ",",
    row.names = FALSE
  )
}

record_r_0_coverage_est <- function(args) {
  true_params <- jsonlite::read_json("../example-parameters.json")
  true_r_naught <- true_params$birthRate / (true_params$deathRate + true_params$samplingRate + true_params$occurrenceRate)
  input_list <- jsonlite::read_json(args$input, simplifyVector = TRUE)

  #' The summarise function throws a warning but from looking on stack exchange
  #' it looks like this is harmless.
  r_naught_coverage_df <- input_list$estimates |>
    filter(
      variable == "r_naught",
      from_aggregated == FALSE,
      is.element(quantile, c("2.5%", "97.5%"))
    ) |>
    select(-variable) |>
    mutate(truth = true_r_naught) |>
    group_by(replicate) |>
    ## 0.5 here because each quantile gets its own row.
    summarise(contains = 0.5 * sum(min(value) <= truth & truth <= max(value))) |>
    count(contains) |>
    as.data.frame()

  write.table(
    x = r_naught_coverage_df,
    file = "out/r-naught-coverage-table.csv",
    sep = ",",
    row.names = FALSE
  )
}

main <- function(args) {
  make_prevalence_comparison(args)
  make_prev_r_naught_fig(args)
  make_estimate_calibration_plot(args, TRUE)
  make_estimate_calibration_plot(args, FALSE)
  make_effective_size_plot(args)
  record_prev_coverage_est(args)
  record_r_0_coverage_est(args)
}

if (!interactive()) {
  args <- parser$parse_args()
} else {
  args <- list(
    input = "out/foo-summary.json",
    output = "out/foo-prevalence-figure.png"
  )
}
main(args)
