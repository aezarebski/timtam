library(purrr)
library(magrittr)
library(dplyr)
library(ggplot2)
library(ape)
options(expressions = 20000)

## To avoid hardcoding the files to read data from we use the configuration JSON
## used by the application.
config <- jsonlite::read_json("ts-config.json")

## We only want to look at the observations resulting from the final inference
## time so we need to find the corresponding inference configuration.
max_inf_time <- lift_dl(max)(map(config$inferenceConfigurations,
                                 ~ .x$inferenceTime))
last_inf_config <- detect(config$inferenceConfigurations,
                          ~ .x$inferenceTime == max_inf_time)

reconstructed_events <- read.csv(last_inf_config$observationsOutputCsv,
                                 header = FALSE,
                                 stringsAsFactors = FALSE)

## If the node labels are too long this will throw an error from deep within
## \code{ape} so we replace all the node labels with 'x'.
raw_newick_string <- readLines(last_inf_config$reconstructedTreeOutputFiles[[1]])
smaller_newick_string <- gsub(pattern = "([0-9]+&)*[0-9]+:",
                              replacement = "x:",
                              x = raw_newick_string)
tree <- ape::read.tree(text=sprintf("(%s);", smaller_newick_string))

print(tree)

png("out/reconstructed-tree.png")
plot(tree, show.tip.label = FALSE)
dev.off()
