library(dplyr)
library(ggplot2)
library(reshape2)
library(magrittr)
library(purrr)
library(jsonlite)


x1 <- read.csv("fobber.csv")
x2 <- read.csv("out/simulation-sizes-and-llhds.csv", header = FALSE) %>% set_names(c("Size", "Llhd", "Name"))
bdscod_records <- left_join(x1, x2, by = "Name")

try_read_json <- function(json_filepath) {
    tryCatch(expr = as.data.frame(read_json(json_filepath)),
             error = function(e) {
                 warning("Could not read file: ", json_filepath);
                 NULL
             })

}

pop_sim_records <- list.files(path = "out/", pattern = "^popsize", full.names = TRUE) %>%
    map(try_read_json) %>%
    keep(compose(`!`, is.null)) %>%
    bind_rows %>%
    mutate(Name = gsub(pattern = ".*observations", replacement = "out/simulated-observations", x = inputJson))

tmp <- pop_sim_records %>% rename(Mean2 = evaluationTime / numReplicates) %>% select(Name, Mean2)
plot_df <- left_join(bdscod_records, tmp, by = "Name") %>% select(Size, Mean, Mean2) %>% melt(id.vars = "Size")
rm(tmp)


g <- ggplot(data = plot_df, mapping = aes(x = Size, y = value)) +
    geom_point() +
    geom_smooth(method = "lm", linetype = "dashed") +
    geom_smooth(method = "loess", se = FALSE) +
    facet_wrap(~variable, scales = "free_y")

ggsave("out/profiles.png", g)
ggsave("out/profiles.pdf", g)

## The following commands can be used to fit a model to understand the scaling
## of the timing of the computation.
foo <- plot_df %>%
    filter(variable == "Mean") %>%
    mutate(ln_size = log(Size),
           ln_time = log(value))
bar <- plot_df %>%
    filter(variable == "Mean2") %>%
    mutate(ln_size = log(Size),
           ln_time = log(value))

lm(ln_time ~ ln_size, data = foo) %>% summary
lm(ln_time ~ ln_size, data = bar) %>% summary
