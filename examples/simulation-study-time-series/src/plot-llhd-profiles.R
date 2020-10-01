library(ggplot2)
library(dplyr)
library(jsonlite)
library(purrr)
library(ape)
library(stringr)
library(reshape2)




#' Returns a plot showing the LLHD profiles fro a given inference configuration.
#'
#' @param infConfig is a list which points to the CSV with the profile values
#' @param true_parameters is a list with the true parameters to draw as a
#'   comparison
#' @param param_mesh a list encoding the parameter values used in the LLHD
#'   profile.
#'
llhd_profile_figure <- function(infConfig, true_parameters, param_mesh) {
    parse_doubles <- function(string, sep) {
        map(str_split(string = string, pattern = sep), as.double)
    }

    all_llhd_vals <- readLines(infConfig$llhdOutputCsv) %>%
        str_replace(pattern = "SimulationParameters,", replacement = "") %>%
        str_split(pattern = "EstimatedParameters,") %>%
        map(parse_doubles, sep = ",") %>%
        pluck(1)

    make_plot_df <- function(llhds,param_kind) {
        if (!is.element(el = param_kind, set = c("simulation", "estimated"))) {
            stop("Bad parameter kind: ", param_kind)
        }

        data.frame(parameter_name = rep(c("lambda", "mu", "psi", "rho", "omega", "nu"),
                                        each = length(param_mesh$lambda_mesh)),
                   parameter_value = c(param_mesh$lambda_mesh,
                                       param_mesh$mu_mesh,
                                       param_mesh$psi_mesh,
                                       param_mesh$rho_mesh,
                                       param_mesh$omega_mesh,
                                       param_mesh$nu_mesh),
                   parameter_kind = param_kind,
                   llhd = llhds)
    }

    plot_df <- rbind(make_plot_df(all_llhd_vals[[1]], "simulation"),
                     make_plot_df(all_llhd_vals[[2]], "estimated"))

    ggplot(plot_df,
           aes(x = parameter_value, y = llhd, linetype = parameter_kind)) +
        geom_line() +
        geom_vline(data = true_parameters, mapping = aes(xintercept = parameter_value)) +
        facet_wrap(~parameter_name, scales = "free_x") +
        labs(x = "Parameter value",
             y = "Log-likelihood",
             linetype = "Parameter kind") +
        theme_classic()
}


## We want to know the actual prevalence in the simulation through time which we
## obtain by processing a log of all the events in the simulation.
primary_count <- function(ps) {
  unlist(map(strsplit(x = ps, split = ":"), length))
}

## Return a dataframe describing the lineages through time of the reconstructed
## tree in absolute time.
tree_ltt_df <- function(inf_config, all_events) {
  raw_newick_string <- readLines(pluck(inf_config, "reconstructedTreeOutputFiles", 1))
  smaller_newick_string <- gsub(pattern = "([0-9]+&)*[0-9]+:",
                                replacement = "x:",
                                x = raw_newick_string)
  tree <- ape::read.tree(text=sprintf("(%s);", smaller_newick_string))
  tree_ltt <- as.data.frame(ltt.plot.coords(tree))
  ## When the tree is read in with \code{ape::read.tree} the last leaf is put at
  ## the present so we need to know the time of the last sequenced sample so we
  ## can adjust this correctly onto absolute time.
  time_shift <- all_events %>%
    filter(event == "sampling" | event == "catastrophe") %>%
    select(time) %>%
    max
  tree_ltt$time <- tree_ltt$time + time_shift
  return(tree_ltt)
}

## Read the parameters of the neative binomial distribution as used by BDSCOD
## NOTE: The parameterisation is different between BDSCOD and R.
read_nb_params <- function(nb_csv) {
  if (file.exists(nb_csv)) {
    x <- read.table(nb_csv,
                    header = FALSE,
                    sep = ",") %>%
      set_names(c("parameter_kind", "negative_binomial"))
    tmp <- x$negative_binomial %>%
      str_split(" ") %>%
      lapply(function(v) set_names(as.list(as.double(tail(v,2))), c("size", "inv_prob")))
    map2(x$parameter_kind, tmp, function(a, b) {b$parameter_kind <- a; return(b)})
  } else {
    stop("Could not find CSV: ", nb_csv)
  }
}

#' Return a dataframe with the estimate of the prevalence for a particular data
#' set.
#'
instantaneous_prevalence <- function(inf_config) {

  quantile_data <- function(nb_params) {
    x <- set_names(as.list(qnbinom(p = c(0.025,0.5,0.975),
                                   size = nb_params$size,
                                   prob = 1-nb_params$inv_prob)),
                   c("lower","mid","upper"))
    x$parameter_kind <- nb_params$parameter_kind
    as.data.frame(x)
  }

  nb_params_list <- read_nb_params(pluck(inf_config, "pointEstimatesCsv"))
  result <- bind_rows(map(.x = nb_params_list, .f = quantile_data))
  result$time <- pluck(inf_config, "inferenceTime")
  return(result)
}


main <- function() {

  ## To avoid hardcoding the files to read data from we use the configuration JSON
  ## used by the application. The way the simulation parameters are encoded is
  ## suited to the haskell application but is messy for R so we also construct the
  ## \code{true_parameters} object to hold these values in a dataframe.
  config <- read_json("ts-config.json")
  true_parameters <- config$simulationParametersClean %>%
    as.data.frame %>%
    (function(.x) melt(data = .x,
                       variable.name = "parameter_name",
                       value.name = "parameter_value",
                       id.vars = c()))
  param_mesh <- list(
    lambda_mesh = seq(from = config$acLlhdProfileMesh$lpmLambdaBounds[[1]],
                      to = config$acLlhdProfileMesh$lpmLambdaBounds[[2]],
                      length = config$acLlhdProfileMesh$lpmMeshSize),
    mu_mesh = seq(from = config$acLlhdProfileMesh$lpmMuBounds[[1]],
                  to = config$acLlhdProfileMesh$lpmMuBounds[[2]],
                  length = config$acLlhdProfileMesh$lpmMeshSize),
    psi_mesh = seq(from = config$acLlhdProfileMesh$lpmPsiBounds[[1]],
                   to = config$acLlhdProfileMesh$lpmPsiBounds[[2]],
                   length = config$acLlhdProfileMesh$lpmMeshSize),
    rho_mesh = seq(from = config$acLlhdProfileMesh$lpmRhoBounds[[1]],
                   to = config$acLlhdProfileMesh$lpmRhoBounds[[2]],
                   length = config$acLlhdProfileMesh$lpmMeshSize),
    omega_mesh = seq(from = config$acLlhdProfileMesh$lpmOmegaBounds[[1]],
                     to = config$acLlhdProfileMesh$lpmOmegaBounds[[2]],
                     length = config$acLlhdProfileMesh$lpmMeshSize),
    nu_mesh = seq(from = config$acLlhdProfileMesh$lpmNuBounds[[1]],
                  to = config$acLlhdProfileMesh$lpmNuBounds[[2]],
                  length = config$acLlhdProfileMesh$lpmMeshSize)
  )


  ## Loop over all the inference configurations and generate the LLHD profiles
  ## so that we can see how they change through time as more data becomes
  ## available.
  for (infConfig in config$inferenceConfigurations) {
    ggsave(sprintf("out/llhd-profiles-%.2f.png", infConfig$inferenceTime),
           llhd_profile_figure(infConfig, true_parameters, param_mesh))
  }

  ## The events are parsed into a data frame so that we can draw the LTT plot to
  ## understand how the estimated prevalence compares to the real thing.
  all_events <- read.csv("out/all-simulated-events.csv",
                         header = FALSE,
                         stringsAsFactors = FALSE) %>%
    set_names(c("event", "time", "primary", "secondary")) %>%
    mutate(delta = primary_count(primary),
           delta_sign = ifelse(event == "infection", +1, -1),
           population_size = 1 + cumsum(delta * delta_sign)) %>%
    select(event,time,population_size)

  ## Extract the estimates of the prevalence under each parameterisation so that
  ## we can draw these at the observation times in the LTT plot.
  prev_estimates <- config$inferenceConfigurations %>%
    map(instantaneous_prevalence) %>%
    bind_rows

  ## Create the visualisation of the prevalence estimates using both the true
  ## simulation parameters and the estimated parameters.
  prev_estimates$parameter_kind <- gsub(pattern = "Parameters",
                                        replacement = "",
                                        x = prev_estimates$parameter_kind)
  dodge_obj <- position_dodge(width = 0.5)

  prev_fig <- ggplot(data = prev_estimates, mapping = aes(x = time, colour = parameter_kind)) +
    geom_line(data = all_events, mapping = aes(y = population_size), colour = "black") +
    geom_errorbar(mapping = aes(ymin = lower, ymax = upper), width = 0.7, size = 0.7, position = dodge_obj) +
    geom_line(mapping = aes(y = mid), size = 0.7, position = dodge_obj) +
    geom_point(mapping = aes(y = mid), size = 1.5, position = dodge_obj) +
    labs(x = "Time", y = "Infection prevalence", colour = "Parameter Kind") +
    scale_color_manual(values = c("#7fc97f", "#beaed4")) +
    theme_classic() +
    theme(legend.position = "bottom")

  ## We save the figure twice so that there is both a PNG and a PDF available
  ## incase we need both.
  fig_height <- 10
  ggsave("out/prevalence-profiles.png",
         prev_fig,
         height = fig_height,
         width = 1.618 * fig_height,
         units = "cm")
  ggsave("out/prevalence-profiles.pdf",
         prev_fig,
         height = fig_height,
         width = 1.618 * fig_height,
         units = "cm")
}

if (!interactive()) {
  main()
}

