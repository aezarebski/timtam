library(ggplot2)
library(dplyr)
library(jsonlite)
library(purrr)
library(ape)

config <- read_json("ts-config.json")



## Make profile plots of the LLHD function.
eval_df <- read.table("out/evaluation-parameters.csv",
                      header = TRUE)
eval_df$llhd <- as.double(as_vector(strsplit(x = readLines(config$llhdOutputCsv), split = ",")))

true_parameters <- read.table("out/true-parameters.csv",
                              header = TRUE)

profiles_fig <- ggplot(eval_df, mapping = aes(x = value, y = llhd)) +
    geom_line() +
    geom_vline(data = true_parameters, mapping = aes(xintercept = value), linetype = "dashed") +
    facet_wrap(~parameter, scales = "free") +
    theme_classic()

ggsave("out/llhd-profiles.png", profiles_fig)






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





raw_newick_string <- readLines("out/reconstructed-newick-tree.txt")
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






partial_results <- read.csv("out/partial-evaluations.csv",
                            header = FALSE,
                            stringsAsFactors = FALSE)

nb_params <- partial_results$V2 %>%
    strsplit(split = " ") %>%
    map(~ set_names(as.list(as.double(tail(.x, 2))), c("size", "prob")))

prev_bounds <- function(x) {
    ## We use 1 - x$prob here because R uses a different parameterisation.
    set_names(as.list(qnbinom(p = c(0.005,0.5,0.995), size = x$size, prob = 1-x$prob)), c("lower","mid","upper"))
}

sim_obs_times <- read.csv("out/simulated-observations.csv", header = FALSE)$V1 %>% cumsum

plot_df <- map(nb_params, prev_bounds) %>% rev %>% tail(-1) %>% bind_rows
plot_df$time <- sim_obs_times
plot_df <- inner_join(plot_df, tree_ltt, by = "time")


prev_fig <- ggplot(plot_df, aes(x = time)) +
    geom_ribbon(mapping = aes(ymin = lower + N, ymax = upper + N), alpha = 0.2) +
    geom_line(mapping = aes(y = mid + N)) +
    geom_line(data = all_events, mapping = aes(y = population_size), colour = "red") +
    theme_classic()
print(prev_fig)

ggsave("out/prevalence-profiles.png", prev_fig)