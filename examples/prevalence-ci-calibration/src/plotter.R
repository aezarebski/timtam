library(dplyr)
library(purrr)
library(magrittr)
library(ggplot2)
library(stringr)
library(reshape2)
library(jsonlite)


make_figures <- function(vis_data) {
 
}

main <- function(args) {
  vis_data_json <- as.character(args[1])

  if (file.exists(vis_data_json)) {
    vis_data <- read_json(vis_data_json)
    make_figures(vis_data)
  } else {
    stop("Could not find visualisation data JSON.")
  }
}

if (!interactive()) {
  args <- commandArgs(trailingOnly = TRUE)
  main(args)
}
