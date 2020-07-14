library(dplyr)
library(ggplot2)
library(reshape2)
library(magrittr)
library(purrr)
library(jsonlite)

bdscod_llhds <- read.csv("out/simulation-sizes-and-llhds.csv", header = FALSE) %>%
    set_names(c("size", "bdscodLlhd", "name"))


try_read_json <- function(json_filepath) {
    tryCatch(expr = as.data.frame(read_json(json_filepath)),
             error = function(e) {
                 warning("Could not read file: ", json_filepath);
                 NULL
             })

}

pop_sim_llhds <- list.files(path = "out/", pattern = "^popsize", full.names = TRUE) %>%
    map(try_read_json) %>%
    keep(compose(`!`, is.null)) %>%
    bind_rows %>%
    mutate(name = gsub(pattern = ".*observations", replacement = "out/simulated-observations", x = inputJson)) %>%
    rename(popSimLlhd = convergedLlhd) %>%
    select(name, popSimLlhd, truncationParameter)

plot_df <- left_join(bdscod_llhds, pop_sim_llhds, by = "name")


llhd_comparison <- ggplot(data = plot_df,
                          mapping = aes(x = bdscodLlhd,
                                        y = popSimLlhd)) +
    geom_smooth(method = "lm") +
    geom_abline(intercept = 0, slope = 1) +
    geom_point()


ggsave("out/llhd-comparison.png", llhd_comparison)
ggsave("out/llhd-comparison.pdf", llhd_comparison)

truncation_parameter_trend <-
    ggplot(data = plot_df,
           mapping = aes(x = size,
                         y = truncationParameter)) +
    geom_point()

ggsave("out/truncation-comparison.png", truncation_parameter_trend)
ggsave("out/truncation-comparison.pdf", truncation_parameter_trend)
