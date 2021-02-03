library(dplyr)
library(reshape2)
library(jsonlite)
library(ggplot2)
library(cowplot)
library(purrr)
library(magrittr)
library(coda)


summary_func <- function(x) quantile(x, probs = c(0.025, 0.5, 0.975))

green_hex_colour <- "#7fc97f"
purple_hex_colour <- "#beaed4"

#' Predicate for whether an MCMC sample file was generated using regular data
used_regular_data <- function(mcmc_csv) {
  grepl(pattern = "regular-data", x = mcmc_csv)
}

#' Predicate for whether an MCMC sample file was generated using aggregated data
used_aggregated_data <- function(mcmc_csv) {
  grepl(pattern = "aggregated-data", x = mcmc_csv)
}

run_mcmc_diagnostics <- function(output_dir, sim_seed, mcmc_csv) {
  if (used_regular_data(mcmc_csv)) {
    run_mcmc_diagnostics_for_regular_data(output_dir, sim_seed, mcmc_csv)
  } else if (used_aggregated_data(mcmc_csv)) {
    run_mcmc_diagnostics_for_aggregated_data(output_dir, sim_seed, mcmc_csv)
  } else {
    stop(sprintf("\n\tnot use how to run diagnostics on MCMC file: %s", mcmc_csv))
  }
}

#' Generate the plot that looks at the ESS for all of the MCMC runs in a single
#' figure.
run_total_mcmc_diagnostics <- function(sim_seeds, data_type) {

  ## We want to know that the MCMC has a sufficient sample size so we check
  ## the effective sample size for each parameter in each iteration.
  .ess <- function(sim_seed) {
    read.csv(sprintf("out/seed-%d/mcmc-effective-size-%d-%s.csv", sim_seed, sim_seed, data_type))
  }
  tmp <- lapply(sim_seeds, .ess) %>%
    bind_rows() %>%
    melt(id.vars = "sim_seed")
  g_ess <- ggplot(tmp, aes(x = sim_seed, y = value, colour = variable)) +
    geom_point() +
    geom_hline(yintercept = 200, linetype = "dashed") +
    labs(x = "Replicate number", y = "Effective sample size", colour = "Parameter") +
    theme_classic()
  ggsave(sprintf("out/mcmc-ess-%s.png", data_type), g_ess)
  ggsave(sprintf("out/mcmc-ess-%s.pdf", data_type), g_ess)
}

run_mcmc_diagnostics_for_aggregated_data <- function(output_dir, sim_seed, mcmc_csv) {
  if (file.exists(mcmc_csv)) {
    ## We want to know that the MCMC has behaved sensibly in the computations that
    ## we have run so we generate some diagnostic output to check this.
    mcmc_obj <- read.csv(mcmc_csv) %>%
      select(lambda, rho, nu) %>%
      as.mcmc()
    png(sprintf("%s/mcmc-trace-%d-aggregated-data.png", output_dir, sim_seed))
    plot(mcmc_obj)
    dev.off()

    tmp <- mcmc_obj %>%
      effectiveSize() %>%
      as.list() %>%
      as.data.frame() %>%
      mutate(sim_seed = sim_seed)
    write.table(
      x = tmp,
      file = sprintf("%s/mcmc-effective-size-%d-aggregated_data.csv", output_dir, sim_seed),
      sep = ",",
      row.names = FALSE
    )
  } else {
    stop(sprintf("\n\tcannot open file %s: No such file or directory", mcmc_csv))
  }

  return(NULL)
}

run_mcmc_diagnostics_for_regular_data <- function(output_dir, sim_seed, mcmc_csv) {
  if (file.exists(mcmc_csv)) {
    ## We want to know that the MCMC has behaved sensibly in the computations that
    ## we have run so we generate some diagnostic output to check this.
    mcmc_obj <- read.csv(mcmc_csv) %>%
      select(lambda, psi, omega) %>%
      as.mcmc()
    png(sprintf("%s/mcmc-trace-%d-regular-data.png", output_dir, sim_seed))
    plot(mcmc_obj)
    dev.off()

    tmp <- mcmc_obj %>%
      effectiveSize() %>%
      as.list() %>%
      as.data.frame() %>%
      mutate(sim_seed = sim_seed)
    write.table(
      x = tmp,
      file = sprintf("%s/mcmc-effective-size-%d-regular_data.csv", output_dir, sim_seed),
      sep = ",",
      row.names = FALSE
    )
  } else {
    stop(sprintf("\n\tcannot open file %s: No such file or directory", mcmc_csv))
  }

  return(NULL)
}





run_post_processing <- function(sim_seed) {
  cat("Running post-processing for ", sim_seed, "\n")

  output_dir <- sprintf("out/seed-%d", sim_seed)
  if (not(dir.exists(output_dir))) {
    stop(sprintf("\n\tcannot find directory %s: No such directory", output_dir))
  }

  all_events_csv <- sprintf("%s/all-simulated-events.csv", output_dir)
  if (not(file.exists(all_events_csv))) {
    stop(sprintf("\n\tcannot open file %s: No such file or directory", all_events_csv))
  }

  all_events <- read.csv(all_events_csv,
    header = FALSE,
    stringsAsFactors = FALSE
  ) %>%
    select(V1, V2) %>%
    set_names(c("event", "abs_time"))

  update_prev <- function(n, e) {
    switch(EXPR = as.character(e),
      infection = n + 1,
      occurrence = n - 1,
      removal = n - 1,
      sampling = n - 1
    )
  }

  prev_df <- data.frame(
    absolute_time = c(0, all_events$abs_time),
    prevalence = accumulate(
      .x = all_events$event,
      .f = update_prev, .init = 1
    )
  )

  app_config <- read_json(sprintf("%s/config-%d.json", output_dir, sim_seed))
  sim_duration <- app_config$simulationDuration

  mcmc_csv_list <- list()
  mcmc_csv_list$regular_data <- app_config$inferenceConfigurations %>%
    extract2(2) %>%
    extract("icMaybeMCMCConfig") %>%
    extract2(1) %>%
    extract2("mcmcOutputCSV")
  mcmc_csv_list$aggregated_data <- app_config$inferenceConfigurations %>%
    extract2(3) %>%
    extract("icMaybeMCMCConfig") %>%
    extract2(1) %>%
    extract2("mcmcOutputCSV")

  #' Read in MCMC samples and compute the CI of prevalence at present.
  read_mcmc_df_with_prevalence <- function(mcmc_csv) {
    if (file.exists(mcmc_csv)) {
      read.csv(mcmc_csv, stringsAsFactors = FALSE) %>%
        mutate(
          nb_min = qnbinom(p = 0.025, size = nbSize, prob = 1 - nbProb),
          nb_med = qnbinom(p = 0.5, size = nbSize, prob = 1 - nbProb),
          nb_max = qnbinom(p = 0.975, size = nbSize, prob = 1 - nbProb)
        )
    } else {
      stop(sprintf("Missing file: %s", mcmc_csv))
    }
  }

  mcmc_df_list <- list()
  mcmc_df_list$regular_data <- read_mcmc_df_with_prevalence(mcmc_csv_list$regular_data)
  mcmc_df_list$aggregated_data <- read_mcmc_df_with_prevalence(mcmc_csv_list$aggregated_data)

  sim_params <- app_config$simulationParameters
  names(sim_params) <- c("lambda", "mu", "psi", "rhoProbs", "omega", "nuProbs")



  #' Summarise the parameter estimates given the regular data
  tmp <- select(mcmc_df_list$regular_data, lambda, psi, omega)
  tmp$r_naught <- tmp$lambda / (sim_params$mu + tmp$psi + tmp$omega)
  tmp$birth_on_death <- tmp$lambda / sim_params$mu
  tmp <- data.frame(
    value = c(summary_func(tmp$lambda), summary_func(tmp$psi), summary_func(tmp$omega), summary_func(tmp$r_naught), summary_func(tmp$birth_on_death)),
    param = rep(c("lambda", "psi", "omega", "r_naught", "birth_on_death"), each = 3),
    statistic = rep(c("min", "mid", "max"), 5),
    sim_seed = rep(sim_seed, 15)
  )
  write.table(
    x = tmp,
    file = sprintf("%s/param-summary-%d-regular_data.csv", output_dir, sim_seed),
    sep = ",",
    row.names = FALSE
  )
  rm(tmp)

  #' Summarise the parameter estimates given the aggregated data
  tmp <- select(mcmc_df_list$aggregated_data, lambda, rho, nu)
  tmp$birth_on_death <- tmp$lambda / sim_params$mu
  tmp <- data.frame(
    value = c(summary_func(tmp$lambda), summary_func(tmp$rho), summary_func(tmp$nu), summary_func(tmp$birth_on_death)),
    param = rep(c("lambda", "psi", "omega", "birth_on_death"), each = 3),
    statistic = rep(c("min", "mid", "max"), 4),
    sim_seed = rep(sim_seed, 12)
  )
  write.table(
    x = tmp,
    file = sprintf("%s/param-summary-%d-aggregated_data.csv", output_dir, sim_seed),
    sep = ",",
    row.names = FALSE
  )
  rm(tmp, summary_func)

  nb_summary <- function(mcmc_df, sim_duration) {
    nb_summary_df <- mcmc_df %>%
      select(starts_with("nb_")) %>%
      colMeans() %>%
      as.list() %>%
      as.data.frame()
    nb_summary_df$absolute_time <- sim_duration
    return(nb_summary_df)
  }

  nb_summary_list <- list(
    regular_data = nb_summary(mcmc_df_list$regular_data, sim_duration),
    aggregated_data = nb_summary(mcmc_df_list$aggregated_data, sim_duration)
  )

  g <- ggplot() +
    geom_step(
      data = prev_df,
      mapping = aes(x = absolute_time, y = prevalence)
    ) +
    geom_errorbar(
      data = nb_summary_list$regular_data,
      mapping = aes(x = absolute_time, ymin = nb_min, y = nb_med, ymax = nb_max),
      colour = green_hex_colour
    ) +
    geom_point(
      data = nb_summary_list$regular_data,
      mapping = aes(x = absolute_time, ymin = nb_min, y = nb_med, ymax = nb_max),
      colour = green_hex_colour
    )

  ggsave(sprintf("%s/summary-figure-%d-regular-data.png", output_dir, sim_seed), g)
  ggsave(sprintf("%s/summary-figure-%d-log-scale-regular-data.png", output_dir, sim_seed), g + scale_y_log10())

  for (data_type in c("regular_data", "aggregated_data")) {
    result <- nb_summary_list[[data_type]]
    result$true_final_prevalence <- prev_df$prevalence %>% tail(1)

    write.table(
      x = result,
      file = sprintf("%s/summary-seed-%d-%s.csv", output_dir, sim_seed, data_type),
      sep = ",",
      row.names = FALSE
    )

    run_mcmc_diagnostics(output_dir, sim_seed, mcmc_csv_list$regular_data)
    run_mcmc_diagnostics(output_dir, sim_seed, mcmc_csv_list$aggregated_data)
  }

  return(NULL)
}


#' Do all the work regarding looking at the prevalence estimates for the
#' simulation seeds in the supplied vector and data type so that we can see how
#' well the MCMC estimates these this.
run_prevalence_plotting <- function(sim_seeds, data_type) {
  if (length(sim_seeds) == 0) {
    stop("Empty list of simulation seeds given to run_prevalence_plotting.")
  }

  geom_colour <- if (data_type == "regular_data") {
    green_hex_colour
  } else if (data_type == "aggregated_data") {
    purple_hex_colour
  } else {
    stop(sprintf("Did not recognise data type: %s", data_type))
  }

  .read_csv_from_seed <- function(sim_seed) {
    read.csv(sprintf(
      "out/seed-%d/summary-seed-%d-%s.csv",
      sim_seed, sim_seed, data_type
    )) %>% mutate(sim_seed = sim_seed)
  }
  plot_df <- lapply(sim_seeds, .read_csv_from_seed) %>%
    bind_rows() %>%
    mutate(
      contains_truth = nb_min <= true_final_prevalence & true_final_prevalence <= nb_max,
      point_prop_error = (nb_med - true_final_prevalence) / true_final_prevalence
    )
  plot_df <- plot_df[order(plot_df$point_prop_error), ]
  plot_df$ix <- 1:nrow(plot_df)

  g_prev_bias <- ggplot() +
    geom_point(
      data = plot_df,
      mapping = aes(
        x = ix,
        y = point_prop_error
      ),
      colour = geom_colour
    ) +
    geom_errorbar(
      data = plot_df,
      mapping = aes(
        x = ix,
        ymin = (nb_min - true_final_prevalence) / true_final_prevalence,
        ymax = (nb_max - true_final_prevalence) / true_final_prevalence
      ),
      colour = geom_colour
    ) +
    geom_hline(yintercept = mean(plot_df$point_prop_error), colour = geom_colour) +
    geom_hline(yintercept = 0, linetype = "dashed") +
    labs(x = "Replicate", y = "Proportional error in prevalence") +
    theme_classic() +
    theme(
      axis.text.x = element_blank(),
      axis.ticks.x = element_blank()
    )

  ## Save a copy of the actual object so that we can revive it later if tweaks
  ## need to be made.
  saveRDS(
    object = g_prev_bias,
    file = sprintf("out/replication-results-prevalence-bias-%s-figure.rds", data_type)
  )
  ggsave(
    sprintf("out/replication-results-prevalence-bias-%s.png", data_type),
    g_prev_bias
  )
  ggsave(
    sprintf("out/replication-results-prevalence-bias-%s.pdf", data_type),
    g_prev_bias
  )

  ## We save a copy of this data frame because it is useful as a way to map
  ## between the prevalence estimates and the particular simulation seed that
  ## was used. This helps in debugging.
  write.table(
    x = plot_df,
    file = sprintf("out/proportion-prevalence-in-ci-%s.csv", data_type),
    sep = ",",
    row.names = FALSE
  )

  ## We also make a simple text file which summarises these values for quick
  ## reference.
  sink(sprintf("out/proportion-prevalence-in-ci-%s-table.txt", data_type))
  print("Table of the number of CIs that contain the true prevalence.")
  print(table(plot_df$contains_truth))
  print(c("See ",
        sprintf("out/proportion-prevalence-in-ci-%s.csv", data_type),
        " for more details."))
  sink()

}

birth_on_death_ggplot <- function(true_birth_on_death, sim_seeds, data_type) {
  geom_colour <- if (data_type == "regular_data") {
    green_hex_colour
  } else if (data_type == "aggregated_data") {
    purple_hex_colour
  } else {
    stop(sprintf("Did not recognise data type: %s", data_type))
  }

  .read_birth_on_death <- function(sim_seed) {
    csv_name <- sprintf("out/seed-%d/param-summary-%d-%s.csv", sim_seed, sim_seed, data_type)
    if (file.exists(csv_name)) {
      read.csv(csv_name)
    }
  }
  params_df <- lapply(sim_seeds, .read_birth_on_death) %>%
    bind_rows() %>%
    filter(param == "birth_on_death") %>%
    select(value, statistic, sim_seed) %>%
    dcast(sim_seed ~ statistic)

  #' Sort the rows so that when plotted they come out in a nice order.
  params_df <- params_df[order(params_df$mid - true_birth_on_death), ]
  params_df$ix <- 1:nrow(params_df)

  ggplot(data = params_df) +
    geom_point(mapping = aes(x = ix, y = mid), colour = geom_colour) +
    geom_errorbar(mapping = aes(x = ix, ymin = min, ymax = max), colour = geom_colour) +
    geom_hline(yintercept = true_birth_on_death, linetype = "dashed") +
    geom_hline(yintercept = mean(params_df$mid), colour = geom_colour) +
    labs(x = "Replicate", y = "Ratio of the birth and death rates") +
    theme_classic() +
    theme(
      axis.text.x = element_blank(),
      axis.ticks.x = element_blank()
    )
}

## Generate a combined figure which works for a single column of text.
run_combined_figure <- function() {
  r_naught_fig_file <- "out/replication-results-r-naught-regular_data-figure.rds"
  prevalence_bias_fig_file <- "out/replication-results-prevalence-bias-regular_data-figure.rds"

  if (all(file.exists(c(r_naught_fig_file, prevalence_bias_fig_file)))) {
    r_naught_fig <- readRDS(r_naught_fig_file) +
      scale_y_continuous() +
      labs(x = NULL, y = "Basic reproduction\nnumber")
    prevalence_bias_fig <- readRDS(prevalence_bias_fig_file) +
      scale_y_continuous() +
      labs(x = "Replicate", y = "Proportional bias\nin prevalence")
    combined_plot <- plot_grid(r_naught_fig,
                               prevalence_bias_fig,
                               ncol = 1
                               )
    ggsave(
      filename = "out/replication-results-combined-plot.pdf",
      plot = combined_plot,
      height = 10,
      width = 10,
      units = "cm"
    )
  } else {
    stop("Missing figure RDS file in run_combined_figure!!!")
  }
}




vj.readDate <- function() {
  "not implemented yet"
}

vj.readGitCommit <- function() {
  "not implemented yet"
}

vj.readMCMCSamples <- function(mcmc_csv) {
  if (file.exists(mcmc_csv)) {
    read.csv(mcmc_csv)
  } else {
    stop("Could not find MCMC file: ", mcmc_csv)
  }
}

vj.singleParameterEstimate <- function(x_name, x_vals) {
  quants <- quantile(x, probs = c(0.025, 0.5, 0.975))
  list(name = x_name,
       estimate = quants[2],
       credibleInterval = quants[c(1,3)])
}

vj.parameterEstimates <- function(mcmc_samples) {
  "not implemented yet"
}

vj.prevalenceEstimate <- function() {
  "not implemented yet"
}

vj.readMcmcCsvFilepaths <- function(sim_seed) {
  config <- sprintf("out/seed-%d/config-%d.json", sim_seed, sim_seed) %>%
    read_json() %>%
    use_series("inferenceConfigurations")

  .f <- function(x) {
    x %>%
      use_series("icMaybeMCMCConfig") %>%
      use_series("mcmcOutputCSV")
  }

  list(
    aggregated_data_csv = config %>% extract2(3) %>% .f,
    regular_data_csv = config %>% extract2(2) %>% .f
  )
}

vj.readSingleSimulationResult <- function(sim_params, sim_seed) {
  mcmc_csv_list <- vj.readMcmcCsvFilepaths(sim_seed)

  mcmc_samples_regular_data <- vj.readMCMCSamples(mcmc_csv_list$regular_data_csv)
  mcmc_samples_aggregated_data <- vj.readMCMCSamples(mcmc_csv_list$aggregated_data_csv)

  list(simulationSeed = sim_seed,
       regularParameterEstimates = vj.parameterEstimates(mcmc_samples_regular_data),
       regularPrevalenceEstimate = vj.prevalenceEstimate(),
       aggregatedParameterEstimates = vj.parameterEstimates(mcmc_samples_aggregated_data),
       aggregatedPrevalenceEstimate = vj.prevalenceEstimate())
}

vj.readSimulationResults <- function(sim_params, sim_seeds) {
  lapply(X = sim_seeds, FUN = function(ss) vj.readSingleSimulationResult(sim_params, ss))
}

vj.readParameters <- function() {
  read_json("../example-parameters.json")
}

vj <- function(sim_seeds) {
  sim_params <- vj.readParameters()
  list(
    creationDate = vj.readDate(),
    gitCommit = vj.readGitCommit(),
    simulationParameters = sim_params,
    simulationResults = vj.readSimulationResults(sim_params, sim_seeds)
  )
}



main <- function(args) {
  if (not(dir.exists("out"))) {
    stop("Cannot find output directory: out.")
  }

  num_seeds <- as.integer(args[1])
  vis_data_json <- as.character(args[2])

  ## include validation that a sensible number of seeds was provided from the
  ## command line.
  if (and(is.integer(num_seeds), num_seeds > 0)) {
    successful_sim_seeds <- keep(
      1:num_seeds,
      function(n) {
        fp1 <- sprintf("out/seed-%d/all-simulated-events.csv", n)
        fp2 <- sprintf("out/seed-%d/regular-data-mcmc-samples.csv", n)
        fp3 <- sprintf("out/seed-%d/aggregated-data-mcmc-samples.csv", n)
        all(c(
          file.exists(fp1),
          file.exists(fp2),
          file.exists(fp3)
        ))
      }
    )

    ## Create the visualisation data JSON file.
    write_json(vj(successful_sim_seeds), vis_data_json, auto_unbox = TRUE, pretty = TRUE)
    stop()

    for (sim_seed in successful_sim_seeds) {
      run_post_processing(sim_seed)
    }
    run_prevalence_plotting(successful_sim_seeds, "regular_data")
    run_prevalence_plotting(successful_sim_seeds, "aggregated_data")


    config <- read_json("out/seed-1/config-1.json")
    sim_params <- config$simulationParameters
    names(sim_params) <- c("lambda", "mu", "psi", "rhoProbs", "omega", "nuProbs")

    .read_csv_param_summary <- function(sim_seed) {
      param_summary_csv <- sprintf("out/seed-%d/param-summary-%d-regular_data.csv", sim_seed, sim_seed)
      if (file.exists(param_summary_csv)) {
        read.csv(param_summary_csv)
      } else {
        stop(sprintf("Cannot find file: %s", param_summary_csv))
      }
    }
    params_df <- lapply(successful_sim_seeds, .read_csv_param_summary) %>% bind_rows()

    simulation_r_naught <- sim_params$lambda / (sim_params$mu + sim_params$psi + sim_params$omega)
    r_naught_df <- params_df %>%
      filter(param == "r_naught") %>%
      select(value, statistic, sim_seed) %>%
      dcast(sim_seed ~ statistic)
    r_naught_df <- r_naught_df[order(r_naught_df$mid), ]
    r_naught_df$ix <- 1:nrow(r_naught_df)
    g_r_naught <- ggplot(r_naught_df) +
      geom_errorbar(mapping = aes(x = ix, ymin = min, ymax = max), colour = green_hex_colour) +
      geom_point(mapping = aes(x = ix, y = mid), colour = green_hex_colour) +
      geom_hline(yintercept = simulation_r_naught, linetype = "dashed") +
      geom_hline(yintercept = mean(r_naught_df$mid), colour = green_hex_colour) +
      labs(x = "Replicate", y = "Basic reproduction number") +
      theme_classic() +
      theme(
        axis.text.x = element_blank(),
        axis.ticks.x = element_blank()
      )

    ## Save a copy of the actual object so that we can revive it later if tweaks
    ## need to be made.
    saveRDS(
      object = g_r_naught,
      file = "out/replication-results-r-naught-regular_data-figure.rds"
    )
    ggsave("out/replication-results-r-naught-regular_data.png", g_r_naught)
    ggsave("out/replication-results-r-naught-regular_data.pdf", g_r_naught)

    ci_contains_r_naught <- r_naught_df$min <= simulation_r_naught & simulation_r_naught <= r_naught_df$max
    sink("out/r-naught-in-ci-regular_data.txt")
    print((table(ci_contains_r_naught)))
    sink()

    run_total_mcmc_diagnostics(successful_sim_seeds, "regular_data")
    run_total_mcmc_diagnostics(successful_sim_seeds, "aggregated_data")

    true_birth_on_death <- sim_params$lambda / sim_params$mu
    ggsave(
      "out/birth-on-death-comparison-regular_data.pdf",
      birth_on_death_ggplot(
        true_birth_on_death,
        successful_sim_seeds,
        "regular_data"
      )
    )
    ggsave(
      "out/birth-on-death-comparison-aggregated_data.pdf",
      birth_on_death_ggplot(
        true_birth_on_death,
        successful_sim_seeds,
        "aggregated_data"
      )
    )

    run_combined_figure()
  } else {
    stop("Could not get num_seeds from command line argument.")
  }
}

if (!interactive()) {
  args <- commandArgs(trailingOnly = TRUE)
  main(args)
}


