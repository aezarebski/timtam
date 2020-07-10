library(dplyr)
library(ggplot2)
library(reshape2)
library(magrittr)
library(jsonlite)


x1 <- read.csv("fobber.csv")
x2 <- read.csv("out/simulation-sizes-and-llhds.csv", header = FALSE) %>% set_names(c("Size", "Llhd", "Name"))
bdscod_records <- left_join(x1, x2, by = "Name")


pop_sim_records <- list.files(path = "out/", pattern = "^popsize", full.names = TRUE) %>%
    map(compose(as.data.frame, read_json)) %>%
    bind_rows %>%
    mutate(Name = gsub(pattern = ".*observations", replacement = "out/simulated-observations", x = inputJson))

tmp <- pop_sim_records %>% rename(Mean2 = evaluationTime / numReplicates) %>% select(Name, Mean2)
plot_df <- left_join(bdscod_records, tmp, by = "Name") %>% select(Size, Mean, Mean2) %>% melt(id.vars = "Size")
rm(tmp)

## g <- ggplot(data = bdscod_records,
##             mapping = aes(x = Size, y = Mean, ymin = Mean - 2 * Stddev, ymax = Mean + 2 * Stddev)) +
##     geom_point() +
##     geom_errorbar() +
##     geom_smooth(method = "lm")

## ggsave("out/profiles.png", g)
## ggsave("out/profiles.pdf", g)


g <- ggplot(data = plot_df, mapping = aes(x = Size, y = value)) +
    geom_point() + geom_smooth(method = "lm") + facet_wrap(~variable, scales = "free_y")

ggsave("out/profiles.png", g)
ggsave("out/profiles.pdf", g)
