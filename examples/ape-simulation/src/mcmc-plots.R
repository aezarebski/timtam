library(mcmc)
library(coda)
library(dplyr)
library(ggplot2)
library(magrittr)
library(purrr)

mcmc_input <- jsonlite::read_json("out/simulation-data.json")
mcmc_csv <- "out/mcmc-samples.csv"
trace_png <- function(n) sprintf("out/mcmc-traceplot-%d.png", n)
mcmc_diagnostics_json <- "out/mcmc-diagnostics.json"

x <- read.csv(mcmc_csv, header = F)
if (length(mcmc_input$mcmcInit) == 3) {
  names(x) <- c("llhd", "birth_rate", "sampling_rate", "omega_rate")
} else if (length(mcmc_input$mcmcInit) == 4) {
  names(x) <- c("llhd", "birth_rate", "sampling_rate", "rho_prob", "omega_rate")
} else {
  stop("")
}

x <- as.mcmc(x)

jx <- 1
fig_n <- 1
while (jx < ncol(x)) {
  png(filename=trace_png(fig_n), width=700, height=1500)
  plot(x[,c(jx, jx+1)])
  dev.off()
  jx <- jx + 2
  fig_n <- fig_n + 1
}
if (jx == ncol(x)) {
  png(filename=trace_png(fig_n), width=700, height=1500)
  plot(x[,c(jx)])
  dev.off()
}

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
