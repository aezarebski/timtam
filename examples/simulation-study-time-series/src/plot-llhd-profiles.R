library(ggplot2)
library(dplyr)
library(jsonlite)
library(purrr)
library(ape)
library(stringr)

## To avoid hardcoding the files to read data from we use the configuration JSON
## used by the application.
config <- read_json("ts-config.json")



## Returns a plot showing the LLHD profiles fro a given inference configuration.
llhd_profile_figure <- function(infConfig) {
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

        mesh_size <- 100

        lambda_mesh <- seq(from = 1, to = 2.5, length = mesh_size)
        mu_mesh <- seq(from = 0.05, to = 1.5, length = mesh_size)
        psi_mesh <- seq(from = 0.05, to = 1.5, length = mesh_size)
        omega_mesh <- seq(from = 0.05, to = 1.5, length = mesh_size)

        data.frame(parameter_name = rep(c("lambda", "mu", "psi", "omega"), each = mesh_size),
                   parameter_value = c(lambda_mesh, mu_mesh, psi_mesh, omega_mesh),
                   parameter_kind = param_kind,
                   llhd = llhds)
    }

    plot_df <- rbind(make_plot_df(all_llhd_vals[[1]], "simulation"),
                     make_plot_df(all_llhd_vals[[2]], "estimated"))


    true_parameters <- read.table("out/true-parameters.csv",
                                  header = TRUE) %>%
        rename(parameter_name = parameter,
               parameter_value = value)

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

## Returns a suitable name to save the LLHD profile figure to
llhd_profile_output_filepath <- function(infConfig) {
    sprintf("out/llhd-profiles-%.2f.png", infConfig$inferenceTime)
}

## Loop over all the inference configurations and generate the LLHD profile.
for (infConfig in config$inferenceConfigurations) {
    ggsave(llhd_profile_output_filepath(infConfig),
           llhd_profile_figure(infConfig))
}





## We want to know the actual prevalence in the simulation through time which we
## obtain by processing a log of all the events in the simulation.
primary_count <- function(ps) {
    unlist(map(strsplit(x = ps, split = ":"), length))
}

all_events <- read.csv("out/all-simulated-events.csv",
                       header = FALSE,
                       stringsAsFactors = FALSE) %>%
    set_names(c("event", "time", "primary", "secondary")) %>%
    mutate(delta = primary_count(primary),
           delta_sign = ifelse(event == "infection", +1, -1),
           population_size = 1 + cumsum(delta * delta_sign)) %>%
    select(event,time,population_size)
final_prevalence <- tail(all_events$population_size, 1)





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
        x <- read.table(nb_csv, header = FALSE, sep = ",") %>% set_names(c("parameter_kind", "negative_binomial"))
        tmp <- x$negative_binomial %>% str_split(" ") %>% lapply(function(v) set_names(as.list(as.double(tail(v,2))), c("size", "inv_prob")))
        map2(x$parameter_kind, tmp, function(a, b) {b$parameter_kind <- a; return(b)})
    } else {
        stop("Could not find CSV: ", nb_csv)
    }
}

instantaneous_prevalence <- function(inf_config) {
    nb_params_list <- read_nb_params(pluck(inf_config, "pointEstimatesCsv"))

    quantile_data <- function(nb_params) {
        x <- set_names(as.list(qnbinom(p = c(0.025,0.5,0.975), size = nb_params$size, prob = 1-nb_params$inv_prob)), c("lower","mid","upper"))
        x$parameter_kind <- nb_params$parameter_kind
        as.data.frame(x)
    }
    ## result <- set_names(as.list(qnbinom(p = c(0.025,0.5,0.975), size = nb_params$size, prob = 1-nb_params$inv_prob)), c("lower","mid","upper"))

    result <- map(.x = nb_params_list, .f = quantile_data) %>% bind_rows
    result$time <- pluck(inf_config, "inferenceTime")
    result
}

prev_estimates <- config$inferenceConfigurations %>% map(instantaneous_prevalence) %>% bind_rows

print(prev_estimates)

prev_fig <- ggplot(data = prev_estimates, mapping = aes(x = time)) +
    geom_ribbon(mapping = aes(ymin = lower, ymax = upper), alpha = 0.1) +
    geom_line(mapping = aes(y = mid), colour = "grey") +
    geom_line(data = all_events, mapping = aes(y = population_size), colour = "black") +
    labs(x = "Time", y = "Infection prevalence") +
    facet_wrap(~ parameter_kind) +
    theme_classic()

## print(prev_fig)
fig_height <- 10
ggsave("out/prevalence-profiles.pdf", prev_fig, height = fig_height, width = 1.618 * fig_height, units = "cm")

