library(mcmc)
library(coda)
library(dplyr)
library(ggplot2)
library(magrittr)
library(purrr)

mcmc_csv <- "out/mcmc-samples.csv"
trace_png <- function(n) sprintf("out/mcmc-traceplot-%d.png", n)
mcmc_diagnostics_json <- "out/mcmc-diagnostics.json"

x <- read.csv(mcmc_csv, header = F)
names(x) <- c("llhd", "birth_rate", "sampling_rate", "rho_prob", "omega_rate")
x <- as.mcmc(x)

png(filename=trace_png(1), width=700, height=1500)
plot(x[,1:3])
dev.off()

png(filename=trace_png(2), width=700, height=1500)
plot(x[,4:5])
dev.off()

summary_stats <- as.data.frame(summary(x)$statistics)

diagnostics <- list(
  varnames = varnames(x),
  mean = summary_stats$Mean,
  sd = summary_stats$SD,
  effective_size = effectiveSize(x)
)

jsonlite::write_json(
            x = diagnostics,
            path = mcmc_diagnostics_json,
            auto_unbox = T
          )
