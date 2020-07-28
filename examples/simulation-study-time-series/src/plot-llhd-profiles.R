library(ggplot2)
library(dplyr)
library(jsonlite)
library(purrr)
library(ape)

## To avoid hardcoding the files to read data from we use the configuration JSON
## used by the application.
config <- read_json("ts-config.json")



## Returns a plot showing the LLHD profiles fro a given inference configuration.
llhd_profile_figure <- function(infConfig) {
    eval_df <- read.table("out/evaluation-parameters.csv",
                          header = TRUE)
    eval_df$llhd <- as.double(as_vector(strsplit(x = readLines(infConfig$llhdOutputCsv), split = ",")))

    true_parameters <- read.table("out/true-parameters.csv",
                                  header = TRUE)

    ggplot(eval_df, mapping = aes(x = value, y = llhd)) +
        geom_line() +
        geom_vline(data = true_parameters, mapping = aes(xintercept = value), linetype = "dashed") +
        facet_wrap(~parameter, scales = "free") +
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
        set_names(flatten(map(strsplit(readLines(nb_csv), split = ","), as.numeric)), c("size", "inv_prob"))
    } else {
        stop("Could not find CSV: ", nb_csv)
    }
}

instantaneous_prevalence <- function(inf_config) {
    nb_params <- read_nb_params(pluck(inf_config, "negBinomCsv"))
    result <- set_names(as.list(qnbinom(p = c(0.025,0.5,0.975), size = nb_params$size, prob = 1-nb_params$inv_prob)), c("lower","mid","upper"))
    result$time <- pluck(inf_config, "inferenceTime")
    as.data.frame(result)
}

prev_estimates <- config$inferenceConfigurations %>% map(instantaneous_prevalence) %>% bind_rows


prev_fig <- ggplot(mapping = aes(x = time)) +
    geom_ribbon(data = prev_estimates, mapping = aes(ymin = lower, ymax = upper), alpha = 0.1) +
    geom_line(data = prev_estimates, mapping = aes(y = mid), colour = "grey") +
    geom_line(data = all_events, mapping = aes(y = population_size), colour = "black") +
    labs(x = "Time", y = "Infection prevalence") +
    theme_classic()

## print(prev_fig)
ggsave("out/prevalence-profiles.png", prev_fig, height = 5, width = 1.618 * 5, units = "cm")

