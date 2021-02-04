library(dplyr)
library(purrr)
library(magrittr)
library(ggplot2)
library(stringr)
library(reshape2)
library(jsonlite)

green_hex_colour <- "#7fc97f"
purple_hex_colour <- "#beaed4"

## Extract data on R0 and the prevalence and return it as a single row of a
## dataframe.
r_naught_and_prevalence_record <- function(sim_result) {
  sim_seed <- sim_result$simulationSeed
  r_naught <- sim_result$regularParameterEstimates %>%
    keep(~ .x$name == "rNaught") %>%
    purrr::flatten()
  prev <- sim_result$regularPrevalenceEstimate
  data.frame(
    seed = sim_seed,
    r_naught_est = r_naught$estimate,
    r_naught_lower = r_naught$credibleInterval[[1]],
    r_naught_upper = r_naught$credibleInterval[[2]],
    prev_est = prev$estimate,
    prev_lower = prev$credibleInterval[[1]],
    prev_upper = prev$credibleInterval[[2]],
    prev_truth = prev$truth
  )
}

## Extract data on birth rate and the prevalence and return it as a single row
## of a dataframe.
birth_rate_and_prevalence_record <- function(data_type, sim_result) {
  sim_seed <- sim_result$simulationSeed
  param_est <- switch(data_type,
    regular_data = sim_result$regularParameterEstimates,
    aggregated_data = sim_result$aggregatedParameterEstimates
  )
  birth_rate <- param_est %>%
    keep(~ .x$name == "birthRate") %>%
    purrr::flatten()
  prev <- switch(data_type,
    regular_data = sim_result$regularPrevalenceEstimate,
    aggregated_data = sim_result$aggregatedPrevalenceEstimate
  )
  data.frame(
    seed = sim_seed,
    birth_rate_est = birth_rate$estimate,
    birth_rate_lower = birth_rate$credibleInterval[[1]],
    birth_rate_upper = birth_rate$credibleInterval[[2]],
    prev_est = prev$estimate,
    prev_lower = prev$credibleInterval[[1]],
    prev_upper = prev$credibleInterval[[2]],
    prev_truth = prev$truth
  )
}

birth_rate_and_prev_gg_list <- function(data_type, true_birth_rate, vis_data) {
  plot_colour <- switch(data_type,
    regular_data = green_hex_colour,
    aggregated_data = purple_hex_colour
  )

  birth_rate_and_prev_df <- map(
    vis_data$simulationResults,
    ~ birth_rate_and_prevalence_record(data_type, .x)
  ) %>%
    bind_rows() %>%
    mutate(
      prev_err_est = (prev_est - prev_truth) / prev_truth,
      prev_err_lower = (prev_lower - prev_truth) / prev_truth,
      prev_err_upper = (prev_upper - prev_truth) / prev_truth
    )

  birth_rate_plot_df <- birth_rate_and_prev_df %>% select(seed, starts_with("birth_rate"))
  birth_rate_plot_df <- birth_rate_plot_df[order(birth_rate_plot_df$birth_rate_est), ]
  birth_rate_plot_df$replicate <- 1:nrow(birth_rate_plot_df)

  birth_rate_gg <- ggplot() +
    geom_point(
      data = birth_rate_plot_df,
      mapping = aes(x = replicate, y = birth_rate_est),
      colour = plot_colour,
      size = 0.5
    ) +
    geom_errorbar(
      data = birth_rate_plot_df,
      mapping = aes(x = replicate, ymin = birth_rate_lower, ymax = birth_rate_upper),
      colour = plot_colour
    ) +
    geom_hline(
      yintercept = true_birth_rate,
      linetype = "dashed"
    ) +
    geom_hline(
      yintercept = mean(birth_rate_plot_df$birth_rate_est),
      colour = plot_colour
    ) +
    labs(y = "Birth rate", x = "Replicate") +
    theme_classic() +
    theme(
      axis.text.x = element_blank(),
      axis.ticks.x = element_blank()
    )

  prev_err_plot_df <- birth_rate_and_prev_df %>% select(seed, starts_with("prev_err"))
  prev_err_plot_df <- prev_err_plot_df[order(prev_err_plot_df$prev_err_est), ]
  prev_err_plot_df$replicate <- 1:nrow(prev_err_plot_df)


  prev_err_gg <- ggplot() +
    geom_point(
      data = prev_err_plot_df,
      mapping = aes(x = replicate, y = prev_err_est),
      colour = plot_colour,
      size = 0.5
    ) +
    geom_errorbar(
      data = prev_err_plot_df,
      mapping = aes(x = replicate, ymin = prev_err_lower, ymax = prev_err_upper),
      colour = plot_colour
    ) +
    geom_hline(
      yintercept = 0,
      linetype = "dashed"
    ) +
    geom_hline(
      yintercept = mean(prev_err_plot_df$prev_err_est),
      colour = plot_colour
    ) +
    labs(y = "Relative bias\nin prevalence", x = "Replicate") +
    theme_classic() +
    theme(
      axis.text.x = element_blank(),
      axis.ticks.x = element_blank()
    )

  list(
    birth_rate = birth_rate_gg,
    prevalence = prev_err_gg
  )
}

## The main text figure for the R0 and the prevalence across replicates with the
## regular data.
r_naught_and_prevalence_ci_plot <- function(vis_data) {
  true_r_naught <- vis_data$simulationParameters$birthRate / (vis_data$simulationParameters$deathRate + vis_data$simulationParameters$samplingRate + vis_data$simulationParameters$occurrenceRate)

  #' This is the data frame that produces the figure in the main text. The
  #' definition of relative bias is the one taken from here:
  #' https://sisu.ut.ee/lcms_method_validation/51-Bias-and-its-constituents

  r_and_prev_df <- map(
    vis_data$simulationResults,
    r_naught_and_prevalence_record
  ) %>%
    bind_rows() %>%
    mutate(
      prev_err_est = (prev_est - prev_truth) / prev_truth,
      prev_err_lower = (prev_lower - prev_truth) / prev_truth,
      prev_err_upper = (prev_upper - prev_truth) / prev_truth
    ) %>%
    select(starts_with("r_naught"), starts_with("prev_err"))
  r_and_prev_df <- r_and_prev_df[order(r_and_prev_df$r_naught_est), ]
  r_and_prev_df$replicate <- 1:nrow(r_and_prev_df)

  r_df <- r_and_prev_df %>%
    select(replicate, starts_with("r_naught")) %>%
    mutate(var = "r_naught") %>%
    rename(
      est = r_naught_est,
      lower = r_naught_lower,
      upper = r_naught_upper
    )

  prev_df <- r_and_prev_df %>%
    select(replicate, starts_with("prev_err")) %>%
    mutate(var = "prev_err") %>%
    rename(
      est = prev_err_est,
      lower = prev_err_lower,
      upper = prev_err_upper
    )


  plot_df <- rbind(r_df, prev_df)
  plot_df$var <- factor(plot_df$var,
    levels = c("r_naught", "prev_err"), ordered = TRUE
  )

  truth_df <- data.frame(truth = c(true_r_naught, 0), var = c("r_naught", "prev_err"))
  truth_df$var <- factor(truth_df$var, levels = levels(plot_df$var), ordered = TRUE)

  facet_labels <- c(
    r_naught = "Basic reproduction number",
    prev_err = "Relative bias in prevalence"
  )
  ggplot() +
    geom_point(
      data = plot_df,
      mapping = aes(x = replicate, y = est),
      colour = green_hex_colour,
      size = 0.5
    ) +
    geom_errorbar(
      data = plot_df,
      mapping = aes(x = replicate, ymin = lower, ymax = upper),
      colour = green_hex_colour
    ) +
    geom_hline(
      data = truth_df,
      mapping = aes(yintercept = truth),
      linetype = "dashed"
    ) +
    geom_hline(
      data = plot_df %>% group_by(var) %>% summarise(mean_est = mean(est)),
      mapping = aes(yintercept = mean_est),
      colour = green_hex_colour
    ) +
    facet_grid(var ~ ., scales = "free_y", labeller = labeller(var = facet_labels)) +
    labs(y = NULL, x = "Replicate") +
    theme_classic() +
    theme(
      axis.text.x = element_blank(),
      axis.ticks.x = element_blank(),
      strip.background = element_blank()
    )
}


## =============================================================================

main <- function(args) {
  vis_data_json <- as.character(args[1])

  if (file.exists(vis_data_json)) {
    vis_data <- read_json(vis_data_json)

    fig_1 <- r_naught_and_prevalence_ci_plot(vis_data)
    ggsave(
      filename = "timtam-figure-1.pdf",
      plot = fig_1,
      height = 10,
      width = 10,
      units = "cm"
    )

    true_birth_rate <- vis_data$simulationParameters$birthRate

    reg_ggs <- birth_rate_and_prev_gg_list("regular_data", true_birth_rate, vis_data)
    ggsave(
      filename = "timtam-figure-s7a.pdf",
      plot = reg_ggs$prevalence,
      height = 5,
      width = 10,
      units = "cm"
    )
    ggsave(
      filename = "timtam-figure-s8a.pdf",
      plot = reg_ggs$birth_rate,
      height = 5,
      width = 10,
      units = "cm"
    )
    agg_ggs <- birth_rate_and_prev_gg_list("aggregated_data", true_birth_rate, vis_data)
    ggsave(
      filename = "timtam-figure-s7b.pdf",
      plot = agg_ggs$prevalence,
      height = 5,
      width = 10,
      units = "cm"
    )
    ggsave(
      filename = "timtam-figure-s8b.pdf",
      plot = agg_ggs$birth_rate,
      height = 5,
      width = 10,
      units = "cm"
    )
  } else {
    stop("Could not find visualisation data JSON.")
  }
}

if (!interactive()) {
  args <- commandArgs(trailingOnly = TRUE)
  main(args)
}
