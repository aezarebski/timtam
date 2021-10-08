suppressPackageStartupMessages(library(mcmc))
suppressPackageStartupMessages(library(coda))
suppressPackageStartupMessages(library(dplyr))
suppressPackageStartupMessages(library(ggplot2))
suppressPackageStartupMessages(library(magrittr))
suppressPackageStartupMessages(library(reshape2))
suppressPackageStartupMessages(library(purrr))
suppressPackageStartupMessages(library(latex2exp))
suppressPackageStartupMessages(library(argparse))

#' =============================================================================
#' Define the CLI
#' =============================================================================

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
  "--params",
  type = "character",
  default = "",
  help = "A comma-separated list of the variables in the posterior samples CSV."
)
parser$add_argument(
  "--output-directory",
  type = "character",
  help = "Directory to write figures to"
)

#' =============================================================================
#' Actual functionality
#' =============================================================================



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

main <- function(args) {
  green_hex_colour <- "#7fc97f"
  purple_hex_colour <- "#beaed4"

  true_params <- jsonlite::read_json("../example-parameters.json")

  final_prevalence <- unlist(jsonlite::read_json(args$prevalence))

  mcmc_csv <- args$posterior_samples
  if (args$verbose) {
    cat("I think the MCMC posterior samples are in ", mcmc_csv, "\n")
  }

  ## Because we do not want to commit to particular parameter names we read this
  ## information in from a command line argument.
  post_samples_df <- read.csv(mcmc_csv, header = F)
  if (args$params != "") {
    post_samples_var_names <- unlist(strsplit(args$params, split = ","))
  } else {
    stop("parameter names were not specificed correctly, please provide a string to use as a header for the posterior samples CSV.")
  }
  stopifnot(length(post_samples_var_names) == ncol(post_samples_df))
  names(post_samples_df) <- post_samples_var_names

  post_samples <- as.mcmc(post_samples_df)
  ## Make the trace plots for use as diagnostics of the MCMC.
  trace_fp <- function(n) {
    paste0(c(args$output_directory, sprintf("traceplot-%d.png", n)), collapse = "/")
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

  ## Define a list to hold all of the marginal posterior plots in.
  marginal_figs <- list()

  ## Make the posterior marginal plots
  if (is.element("sampling_rate", varnames(post_samples))) {
    birth_rate_col <- green_hex_colour
  } else {
    birth_rate_col <- purple_hex_colour
  }
  birth_rate_marg <- marginal_plot_summary(post_samples[, "birth_rate"], "birth_rate")
  birth_rate_fig <- ggplot(mapping = aes(x = x, y = y)) +
    geom_line(
      data = birth_rate_marg$df,
      colour = birth_rate_col
    ) +
    geom_area(
      data = filter(birth_rate_marg$df, birth_rate_marg$ci[1] < x, x < birth_rate_marg$ci[2]),
      fill = birth_rate_col,
      alpha = 0.3
    ) +
    geom_vline(
      xintercept = true_params$birthRate,
      linetype = "dashed"
    ) +
    labs(y = NULL, x = TeX("Birth rate ($\\lambda$)")) +
    theme_classic() +
    theme()
  marginal_figs <- c(marginal_figs, list(birth_rate_fig))

  if (is.element("sampling_rate", varnames(post_samples))) {
    sampling_rate_marg <- marginal_plot_summary(post_samples[, "sampling_rate"], "sampling_rate")
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
    marginal_figs <- c(marginal_figs, list(sampling_rate_fig))
  }

  if (is.element("omega_rate", varnames(post_samples))) {
    omega_rate_marg <- marginal_plot_summary(post_samples[, "omega_rate"], "omega_rate")
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
    marginal_figs <- c(marginal_figs, list(omega_rate_fig))
  }

  if (is.element("rho_prob", varnames(post_samples))) {
    rho_prob_marg <- marginal_plot_summary(post_samples[, "rho_prob"], "rho_prob")
    rho_prob_fig <- ggplot(mapping = aes(x = x, y = y)) +
      geom_line(
        data = rho_prob_marg$df,
        colour = purple_hex_colour
      ) +
      geom_area(
        data = filter(rho_prob_marg$df, rho_prob_marg$ci[1] < x, x < rho_prob_marg$ci[2]),
        fill = purple_hex_colour,
        alpha = 0.3
      ) +
      labs(y = NULL, x = TeX("Sequencing probability ($\\rho$)")) +
      theme_classic() +
      theme(axis.title.x = element_text(vjust = 0.6))
    marginal_figs <- c(marginal_figs, list(rho_prob_fig))
  }
  if (is.element("nu_prob", varnames(post_samples))) {
    nu_prob_marg <- marginal_plot_summary(post_samples[, "nu_prob"], "nu_prob")
    nu_prob_fig <- ggplot(mapping = aes(x = x, y = y)) +
      geom_line(
        data = nu_prob_marg$df,
        colour = purple_hex_colour
      ) +
      geom_area(
        data = filter(nu_prob_marg$df, nu_prob_marg$ci[1] < x, x < nu_prob_marg$ci[2]),
        fill = purple_hex_colour,
        alpha = 0.3
      ) +
      labs(y = NULL, x = TeX("Occurrence probability ($\\nu$)")) +
      theme_classic() +
      theme(axis.title.x = element_text(vjust = 0.6))
    marginal_figs <- c(marginal_figs, list(nu_prob_fig))
  }

  marginals_fig <- purrr::lift_dl(cowplot::plot_grid)(marginal_figs, ncol = 3)

  ggsave(
    filename = paste0(c(args$output_directory, "marginal-distributions.png"), collapse = "/"),
    plot = marginals_fig,
    height = 10,
    width = 2.8 * 10,
    units = "cm"
  )

  ## Make the matrix of scatter plots thing....
  sub_plot_scatter <- function(x_var, y_var, col) {
    ggplot(post_samples_df, aes_string(x = x_var, y = y_var)) +
      geom_point(shape = 1, size = 0.1, colour = col) +
      theme_minimal()
  }
  sub_plot_hist <- function(x_var, col) {
    ggplot(post_samples_df, aes_string(x = x_var)) +
      geom_histogram(fill = col, bins = 20) +
      theme_minimal()
  }
  if (all(is.element(c("sampling_rate", "omega_rate"), varnames(post_samples)))) {
    splom <- cowplot::plot_grid(
      sub_plot_hist("birth_rate", green_hex_colour),
      sub_plot_scatter("sampling_rate", "birth_rate", green_hex_colour),
      sub_plot_scatter("omega_rate", "birth_rate", green_hex_colour),
      sub_plot_scatter("birth_rate", "sampling_rate", green_hex_colour),
      sub_plot_hist("sampling_rate", green_hex_colour),
      sub_plot_scatter("omega_rate", "sampling_rate", green_hex_colour),
      sub_plot_scatter("birth_rate", "omega_rate", green_hex_colour),
      sub_plot_scatter("sampling_rate", "omega_rate", green_hex_colour),
      sub_plot_hist("omega_rate", green_hex_colour),
      align = "hv",
      ncol = 3
    )
  } else if (all(is.element(c("rho_prob", "nu_prob"), varnames(post_samples)))) {
    splom <- cowplot::plot_grid(
      sub_plot_hist("birth_rate", purple_hex_colour),
      sub_plot_scatter("rho_prob", "birth_rate", purple_hex_colour),
      sub_plot_scatter("nu_prob", "birth_rate", purple_hex_colour),
      sub_plot_scatter("birth_rate", "rho_prob", purple_hex_colour),
      sub_plot_hist("rho_prob", purple_hex_colour),
      sub_plot_scatter("nu_prob", "rho_prob", purple_hex_colour),
      sub_plot_scatter("birth_rate", "nu_prob", purple_hex_colour),
      sub_plot_scatter("rho_prob", "nu_prob", purple_hex_colour),
      sub_plot_hist("nu_prob", purple_hex_colour),
      align = "hv",
      ncol = 3
    )
  } else {
    stop("not sure how to construct the SPLOM for this data, you may need to double check that this program understands the given combinations of parameter provided by the params argument.")
  }
  ggsave(
    filename = paste0(c(args$output_directory, "splom.png"), collapse = "/"),
    plot = splom,
    height = 20,
    width = 20,
    units = "cm"
  )

  if (all(is.element(c("sampling_rate", "omega_rate"), varnames(post_samples)))) {
    ## Make a plot of the r-naught estimates
    r_naught_samples <-
      post_samples_df |>
      mutate(r_naught = birth_rate / (sampling_rate + true_params$deathRate + omega_rate)) |>
      extract2("r_naught")

    true_r_naught <- true_params$birthRate / (true_params$deathRate + true_params$samplingRate + true_params$occurrenceRate)

    r_naught_marg <- marginal_plot_summary(r_naught_samples, "r_naught")
    r_naught_fig <- ggplot(mapping = aes(x = x, y = y)) +
      geom_line(
        data = r_naught_marg$df,
        colour = green_hex_colour
      ) +
      geom_area(
        data = filter(r_naught_marg$df, r_naught_marg$ci[1] < x, x < r_naught_marg$ci[2]),
        fill = green_hex_colour,
        alpha = 0.3
      ) +
      geom_vline(
        xintercept = true_r_naught,
        linetype = "dashed"
      ) +
      labs(y = NULL, x = TeX("Reproduction number ($R_{0}$)")) +
      theme_classic() +
      theme()
    ggsave(
      filename = paste0(c(args$output_directory, "r-naught.png"), collapse = "/"),
      plot = r_naught_fig,
      height = 10,
      width = 2.8 * 10,
      units = "cm"
    )
  }

  ## Plot the negative binomial distribution of the prevalence
  if (is.element("sampling_rate", varnames(post_samples))) {
    prevalence_col <- green_hex_colour
  } else {
    prevalence_col <- purple_hex_colour
  }
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
      colour = prevalence_col
    ) +
    geom_area(
      data = filter(nb_df$df, nb_df$ci[1] < x, x < nb_df$ci[2]),
      fill = prevalence_col,
      alpha = 0.3
    ) +
    geom_vline(
      xintercept = final_prevalence,
      linetype = "dashed"
    ) +
    labs(y = NULL, x = "Prevalence at present\n(square-root scale)") +
    scale_x_sqrt() +
    theme_classic() +
    theme(axis.title.x = element_text(vjust = 0.4))
  ggsave(
    filename = paste0(c(args$output_directory, "prevalence.png"), collapse = "/"),
    plot = nb_fig,
    height = 10,
    width = 2.8 * 10,
    units = "cm"
  )


  marginal_figs <- c(marginal_figs, list(nb_fig + labs(y = NULL)))
  marginals_fig_2 <- purrr::lift_dl(cowplot::plot_grid)(marginal_figs, ncol = 1)
  saveRDS(object = marginals_fig_2, file = paste0(c(args$output_directory, "marginal-distributions-2.rds"), collapse = "/"))

  summary_stats <- as.data.frame(summary(post_samples)$statistics)

  diagnostics <- list(
    varnames = varnames(post_samples),
    mean = summary_stats$Mean,
    sd = summary_stats$SD,
    effective_size = effectiveSize(post_samples)
  )

  jsonlite::write_json(
    x = diagnostics,
    path = paste0(c(args$output_directory, "summary-statistics-and-diagnostics.json"), collapse = "/"),
    auto_unbox = TRUE,
    digits = 16,
    pretty = TRUE
  )
}

#' =============================================================================
#' Run the main function appropriately
#' =============================================================================

if (!interactive()) {
  args <- parser$parse_args()
} else {
  ## for unscheduled data
  args <- list(
    verbose = TRUE,
    output_directory = "out/unscheduled-data",
    prevalence = "out/ape-sim-final-prevalence.json",
    posterior_samples = "out/unscheduled-data/posterior-samples.csv",
    params = "llhd,birth_rate,sampling_rate,omega_rate,nb_r,nb_p"
  )
  ## for aggregated data
  ## args <- list(
  ##   verbose = TRUE,
  ##   output_directory = "out/aggregated-data",
  ##   prevalence = "out/ape-sim-final-prevalence.json",
  ##   posterior_samples = "out/aggregated-data/posterior-samples.csv",
  ##   params = "llhd,birth_rate,rho_prob,nu_prob,nb_r,nb_p"
  ## )
}
main(args)
