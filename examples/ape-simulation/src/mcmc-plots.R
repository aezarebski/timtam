library(mcmc)
library(coda)
library(dplyr)
library(ggplot2)
library(magrittr)
library(purrr)

mcmc_csv <- "out/mcmc-samples.csv"
trace_png <- "out/mcmc-traceplot.png"

x <- read.csv(mcmc_csv, header = F)
names(x) <- c("birth_rate", "sampling_rate", "omega_rate")
x <- as.mcmc(x)

png(filename=trace_png, width=700, height=1500)
plot(x)
dev.off()

