library(purrr)
library(magrittr)
library(dplyr)
library(ggplot2)
library(ape)
options(expressions = 20000)


primary_count <- function(ps) {
    unlist(map(strsplit(x = ps, split = ":"), length))
}

all_events <- read.csv("out/all-simulated-events.csv",
                       header = FALSE,
                       stringsAsFactors = FALSE) %>%
    set_names(c("event", "time", "primary", "secondary")) %>%
    mutate(delta = primary_count(primary)) %>%
    select(event,time,delta)
all_events$population_size <- cumsum(ifelse(all_events$event == "infection", +1, -1) * all_events$delta)



g <- ggplot(mapping = aes(x = time, y = population_size)) +
    geom_line(data = all_events)

ggsave("out/total-prevalence.png", g)

reconstructed_events <- read.csv("out/simulated-observations.csv",
                                 header = FALSE,
                                 stringsAsFactors = FALSE)

## If the node labels are too long this will throw an error from deep within
## \code{ape} so we replace all the node labels with 'x'.
raw_newick_string <- readLines("out/reconstructed-newick-tree.txt")
smaller_newick_string <- gsub(pattern = "([0-9]+&)*[0-9]+:",
                              replacement = "x:",
                              x = raw_newick_string)
tree <- ape::read.tree(text=sprintf("(%s);", smaller_newick_string))
## tree <- ape::read.tree(text=sprintf("(%s);", raw_newick_string))

print(tree)

png("out/reconstructed-tree.png")
plot(tree, show.tip.label = FALSE)
dev.off()
