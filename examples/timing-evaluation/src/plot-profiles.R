library(dplyr)
library(ggplot2)
library(magrittr)

x1 <- read.csv("fobber.csv")
x2 <- read.csv("out/simulation-sizes-and-llhds.csv", header = FALSE) %>% set_names(c("Size", "Llhd", "Name"))
x <- left_join(x1, x2, by = "Name")


g <- ggplot(data = x,
            mapping = aes(x = Size, y = Mean, ymin = Mean - 2 * Stddev, ymax = Mean + 2 * Stddev)) +
    geom_point() +
    geom_errorbar() +
    geom_smooth(method = "lm")

ggsave("out/profiles.png", g)
ggsave("out/profiles.pdf", g)
