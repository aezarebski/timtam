suppressPackageStartupMessages(library(mcmc))
suppressPackageStartupMessages(library(coda))
suppressPackageStartupMessages(library(dplyr))
suppressPackageStartupMessages(library(ggplot2))
suppressPackageStartupMessages(library(cowplot))
suppressPackageStartupMessages(library(magrittr))
suppressPackageStartupMessages(library(reshape2))
suppressPackageStartupMessages(library(purrr))
suppressPackageStartupMessages(library(latex2exp))

library(argparse)

                                        # create parser object
parser <- ArgumentParser()


parser$add_argument(
         "-v",
         "--verbose",
         action = "store_true",
         default = FALSE,
         help = "Verbose output"
       )
parser$add_argument(
         "-p",
         "--prevalence",
         type = "character",
         help = "Filepath to final prevalence"
       )
parser$add_argument(
         "--posterior-samples",
         type = "character",
         help = "Filepath to posterior samples CSV"
       )
parser$add_argument(
       "--output-directory",
       type = "character",
       help = "Directory to write figures to"
)

main <- function(args) {
  green_hex_colour <- "#7fc97f"
  purple_hex_colour <- "#beaed4"

  true_params <- jsonlite::read_json("../example-parameters.json")

  final_prevalence <- unlist(jsonlite::read_json(args$prevalence))

  mcmc_csv <- args$posterior_samples
  if (args$verbose) {
    cat("I think the MCMC posterior samples are in ", mcmc_csv, "\n")
  }

  ## TODO This needs to be fixed up....
  post_samples_df <- read.csv(mcmc_csv, header = F)
  stopifnot(ncol(post_samples_df) == 6)
  names(post_samples_df) <- c("llhd", "birth_rate", "sampling_rate", "omega_rate", "nb_r", "nb_p")
  warning("Assuming basic parameters in CSV......")
  post_samples <- as.mcmc(post_samples_df)

  trace_fp <- function(n) {
    paste0(c(args$output_directory, sprintf("traceplot-%d.png", n)), collapse="/")
  }
  jx <- 1
  fig_n <- 1
  while (jx < ncol(post_samples)) {
    png(filename = trace_fp(fig_n), width = 700, height = 1500)
    plot(post_samples[, c(jx, jx + 1)])
    dev.off()
    jx <- jx + 2
    fig_n <- fig_n + 1
  }
  if (jx == ncol(post_samples)) {
    png(filename = trace_fp(fig_n), width = 700, height = 1500)
    plot(post_samples[, c(jx)])
    dev.off()
  }

  ## summary_stats <- as.data.frame(summary(post_samples)$statistics)

  ## diagnostics <- list(
  ##   varnames = varnames(post_samples),
  ##   mean = summary_stats$Mean,
  ##   sd = summary_stats$SD,
  ##   effective_size = effectiveSize(post_samples)
  ## )

  ## jsonlite::write_json(
  ##             x = diagnostics,
  ##             path = mcmc_diagnostics_json,
  ##             auto_unbox = T,
  ##             digits = 16
  ##           )

}

if (!interactive()) {
  args <- parser$parse_args()
  main(args)
}


stop()







## ======================================================
## Make the plots of the marginal posterior distribution.
## ======================================================


marginal_plot_summary <- function(samples, varname) {
  density_df <-
    samples |>
    density() |>
    extract(c("x", "y")) |>
    data.frame() |>
    mutate(variable = varname)

  qs <- quantile(x = samples, probs = c(0.025, 0.5, 0.975))
  list(df = density_df, point_est = qs[2], ci = qs[c(1, 3)])
}

birth_rate_marg <- marginal_plot_summary(post_samples_df$birth_rate, "birth_rate")

birth_rate_fig <- ggplot(mapping = aes(x = x, y = y)) +
  geom_line(
    data = birth_rate_marg$df,
    colour = green_hex_colour
  ) +
  geom_area(
    data = filter(birth_rate_marg$df, birth_rate_marg$ci[1] < x, x < birth_rate_marg$ci[2]),
    fill = green_hex_colour,
    alpha = 0.3
  ) +
  geom_vline(
    xintercept = true_params$birthRate,
    linetype = "dashed"
  ) +
  labs(y = "Posterior density", x = TeX("Birth rate ($\\lambda$)")) +
  theme_classic() +
  theme()

sampling_rate_marg <- marginal_plot_summary(post_samples_df$sampling_rate, "sampling_rate")

sampling_rate_fig <- ggplot(mapping = aes(x = x, y = y)) +
  geom_line(
    data = sampling_rate_marg$df,
    colour = green_hex_colour
  ) +
  geom_area(
    data = filter(sampling_rate_marg$df, sampling_rate_marg$ci[1] < x, x < sampling_rate_marg$ci[2]),
    fill = green_hex_colour,
    alpha = 0.3
  ) +
  geom_vline(
    xintercept = true_params$samplingRate,
    linetype = "dashed"
  ) +
  labs(y = NULL, x = TeX("Sampling rate ($\\psi$)")) +
  theme_classic() +
  theme()

omega_rate_marg <- marginal_plot_summary(post_samples_df$omega_rate, "omega_rate")

omega_rate_fig <- ggplot(mapping = aes(x = x, y = y)) +
  geom_line(
    data = omega_rate_marg$df,
    colour = green_hex_colour
  ) +
  geom_area(
    data = filter(omega_rate_marg$df, omega_rate_marg$ci[1] < x, x < omega_rate_marg$ci[2]),
    fill = green_hex_colour,
    alpha = 0.3
  ) +
  geom_vline(
    xintercept = true_params$occurrenceRate,
    linetype = "dashed"
  ) +
  labs(y = NULL, x = TeX("Occurrence rate ($\\omega$)")) +
  theme_classic() +
  theme()

marginals_fig <- plot_grid(birth_rate_fig, sampling_rate_fig, omega_rate_fig, ncol = 3)

ggsave(
  filename = marginal_plot_filepath("png"),
  plot = marginals_fig,
  height = 10,
  width = 2.8 * 10,
  units = "cm"
)
## ggsave(filename = marginal_plot_filepath("pdf"),
##        plot = marginals_fig,
##        height = 10,
##        width = 2.8 * 10,
##        units = "cm")

## =======================================================
## Make the plot of the posterior distribution of r-naught
## =======================================================

r_naught_samples <-
  post_samples_df |>
  mutate(r_naught = birth_rate / (sampling_rate + true_params$deathRate + omega_rate)) |>
  extract2("r_naught")

true_r_naught <- true_params$birthRate / (true_params$deathRate + true_params$samplingRate + true_params$occurrenceRate)

r_naught_df <- marginal_plot_summary(r_naught_samples, "r_naught")

r_naught_fig <- ggplot(mapping = aes(x = x, y = y)) +
  geom_line(
    data = r_naught_df$df,
    colour = green_hex_colour
  ) +
  geom_area(
    data = filter(r_naught_df$df, r_naught_df$ci[1] < x, x < r_naught_df$ci[2]),
    fill = green_hex_colour,
    alpha = 0.3
  ) +
  geom_vline(
    xintercept = true_r_naught,
    linetype = "dashed"
  ) +
  labs(y = "Posterior density", x = TeX("Basic reproduction number ($R_{0}$)")) +
  theme_classic() +
  theme()


ggsave(
  filename = r_naught_plot_filepath("png"),
  plot = r_naught_fig,
  height = 10,
  width = 1.3 * 10,
  units = "cm"
)
## ggsave(filename = r_naught_plot_filepath("pdf"),
##        plot = r_naught_fig,
##        height = 10,
##        width = 2.8 * 10,
##        units = "cm")

## ============================================================================
## Make the plot of the posterior distribution of the prevalence at the end of
## the observations
## ============================================================================

nb_samples <-
  post_samples_df |>
  mutate(nb_q = 1 - nb_p) |>
  select(nb_r, nb_q)

nb_samples_vec <- rnbinom(
  n = nrow(nb_samples),
  size = nb_samples$nb_r,
  prob = nb_samples$nb_q
)


nb_df <- marginal_plot_summary(nb_samples_vec, "nb")

nb_fig <- ggplot(mapping = aes(x = x, y = y)) +
  geom_line(
    data = nb_df$df,
    colour = green_hex_colour
  ) +
  geom_area(
    data = filter(nb_df$df, nb_df$ci[1] < x, x < nb_df$ci[2]),
    fill = green_hex_colour,
    alpha = 0.3
  ) +
  geom_vline(
    xintercept = final_prevalence,
    linetype = "dashed"
  ) +
  labs(y = "Posterior density", x = "Prevalence at present (square-root scale)") +
  scale_x_sqrt() +
  theme_classic() +
  theme()


ggsave(
  filename = nb_plot_filepath("png"),
  plot = nb_fig,
  height = 10,
  width = 1.3 * 10,
  units = "cm"
)
## ggsave(filename = r_naught_plot_filepath("pdf"),
##        plot = r_naught_fig,
##        height = 10,
##        width = 2.8 * 10,
##        units = "cm")
