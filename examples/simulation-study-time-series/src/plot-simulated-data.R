library(purrr)
library(magrittr)
library(dplyr)
library(ggplot2)
library(ape)
options(expressions = 20000)


primary_count <- function(ps) {
    unlist(map(strsplit(x = ps, split = ":"), length))
}

all_events <- read.csv("out/all-simulated-events.csv", header = FALSE) %>%
    set_names(c("event", "time", "primary", "secondary")) %>%
    mutate(delta = primary_count(primary)) %>%
    select(event,time,delta)
all_events$population_size <- cumsum(ifelse(all_events$event == "infection", +1, -1) * all_events$delta)



g <- ggplot(mapping = aes(x = time, y = population_size)) +
    geom_line(data = all_events)

ggsave("out/total-prevalence.png", g)

reconstructed_events <- read.csv("out/simulated-observations.csv", header = FALSE)

tree <- ape::read.tree(text=sprintf("(%s);", readLines("out/reconstructed-newick-tree.txt")))

print(tree)

png("out/reconstructed-tree.png")
plot(tree, show.tip.label = FALSE)
dev.off()
