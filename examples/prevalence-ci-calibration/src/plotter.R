library(dplyr)
library(purrr)
library(magrittr)
library(ggplot2)
library(stringr)
library(reshape2)
library(jsonlite)

green_hex_colour <- "#7fc97f"
purple_hex_colour <- "#beaed4"

vis_data <- read_json("demo.json")

r_naught_and_prevalence_ci_plot <- function(vis_data) {
  true_r_naught <- vis_data$simulationParameters$birthRate / (vis_data$simulationParameters$deathRate + vis_data$simulationParameters$samplingRate + vis_data$simulationParameters$occurrenceRate)

  r_naught_and_prevalence_record <- function(sim_result) {
    sim_seed <- sim_result$simulationSeed
    r_naught <- sim_result$regularParameterEstimates %>%
      keep(~ .x$name == "rNaught") %>%
      flatten()
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

make_figures <- function(vis_data) {
  fig_1 <- r_naught_and_prevalence_ci_plot(vis_data)
  ggsave(
    filename = "timtam-figure-1.pdf",
    plot = fig_1,
    height = 10,
    width = 10,
    units = "cm"
  )
}

main <- function(args) {
  vis_data_json <- as.character(args[1])

  if (file.exists(vis_data_json)) {
    vis_data <- read_json(vis_data_json)
    make_figures(vis_data)
  } else {
    stop("Could not find visualisation data JSON.")
  }
}

if (!interactive()) {
  args <- commandArgs(trailingOnly = TRUE)
  main(args)
}
