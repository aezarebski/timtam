library(ggplot2)
library(dplyr)
library(stringr)

INPUT_FILE <- "out/config.json"

config <- jsonlite::read_json(INPUT_FILE)

PARAMS <- list(lambda = config$simLambda,
               mu = config$simMu,
               psi = config$simPsi,
               rho = config$simRho,
               omega = config$simOmega,
               nu = config$simNu
               )

save_figures <- TRUE

INPUT_FILE <- config$outputLlhdFile

if (!file.exists(INPUT_FILE)) {
    stop(sprintf("Cannot find the input file: %s", INPUT_FILE))
}

x <- read.csv(INPUT_FILE, header = FALSE, stringsAsFactors = FALSE)
names(x) <- c("lambda", "mu", "psi", "rho", "omega", "nu", "llhd", "negative_binomial")
nb_params <- t(sapply(str_extract_all(x$negative_binomial, "[\\d.]+(?:e-?\\d+)?"), as.numeric))
x$neg_binom_r <- nb_params[,1]
x$neg_binom_p <- 1 - nb_params[,2] # R and Wikipedia have different parameterisations.


font_scale_factor <- 0.5
fig_theme <- theme(
    axis.title.x = element_text(size = font_scale_factor * 22),
    axis.title.y = element_blank(),
    ## axis.text.y = element_blank(),
    axis.ticks = ggplot2::element_blank(),
    axis.line.x.bottom = element_line(colour = "#000000"),
    legend.title = element_text(size = font_scale_factor * 22),
    plot.title = element_text(size = font_scale_factor * 32),
    plot.subtitle = element_text(size = font_scale_factor * 22),
    panel.grid.minor = ggplot2::element_blank(),
    panel.grid.major.y = ggplot2::element_line(color="#cbcbcb"),
    panel.grid.major.x = ggplot2::element_blank(),
    panel.background = ggplot2::element_blank(),
    strip.background = ggplot2::element_rect(fill="white"))
truth_linetype <- "dashed"

lambda_figure <- ggplot(filter(x, mu == PARAMS$mu, psi == PARAMS$psi, rho == PARAMS$rho, omega == PARAMS$omega, nu == PARAMS$nu), aes(x = lambda, y = llhd)) +
    geom_line() +
    geom_vline(xintercept = PARAMS$lambda, linetype = truth_linetype) +
    labs(x = "Birth rate",
         y = "Log-Likelihood",
         title = "Log-likelihood profile",
         subtitle = "Known observation model parameters") +
    fig_theme

if (save_figures) {
    ggsave("out/llhd-profile-lambda.pdf", plot = lambda_figure, height = 10.5, width = 14.8, units = "cm")
}

mu_figure <- ggplot(filter(x, lambda == PARAMS$lambda, psi == PARAMS$psi, rho == PARAMS$rho, omega == PARAMS$omega, nu == PARAMS$nu), aes(x = mu, y = llhd)) +
    geom_line() +
    geom_vline(xintercept = PARAMS$mu, linetype = truth_linetype) +
    labs(x = "Death rate",
         y = "Log-Likelihood",
         title = "Log-likelihood profile",
         subtitle = "Known observation model parameters") +
    fig_theme

if (save_figures) {
    ggsave("out/llhd-profile-mu.pdf", plot = mu_figure, height = 10.5, width = 14.8, units = "cm")
}

psi_figure <- ggplot(filter(x, lambda == PARAMS$lambda, mu == PARAMS$mu, rho == PARAMS$rho, omega == PARAMS$omega, nu == PARAMS$nu), aes(x = psi, y = llhd)) +
    geom_line() +
    geom_vline(xintercept = PARAMS$psi, linetype = truth_linetype) +
    labs(x = "Sampling rate",
         y = "Log-Likelihood",
         title = "Log-likelihood profile",
         subtitle = "Known observation model parameters") +
    fig_theme

if (save_figures) {
    ggsave("out/llhd-profile-psi.pdf", plot = psi_figure, height = 10.5, width = 14.8, units = "cm")
}

rho_figure <- ggplot(filter(x, lambda == PARAMS$lambda, mu == PARAMS$mu, psi == PARAMS$psi, omega == PARAMS$omega, nu == PARAMS$nu), aes(x = rho, y = llhd)) +
    geom_line() +
    geom_vline(xintercept = PARAMS$rho, linetype = truth_linetype) +
    labs(x = "Catastrophe extinction probability",
         y = "Log-Likelihood",
         title = "Log-likelihood profile",
         subtitle = "Known observation model parameters") +
    fig_theme

if (save_figures) {
    ggsave("out/llhd-profile-rho.pdf", plot = rho_figure, height = 10.5, width = 14.8, units = "cm")
}

omega_figure <- ggplot(filter(x, lambda == PARAMS$lambda, mu == PARAMS$mu, psi == PARAMS$psi, rho == PARAMS$rho, nu == PARAMS$nu), aes(x = omega, y = llhd)) +
    geom_line() +
    geom_vline(xintercept = PARAMS$omega, linetype = truth_linetype) +
    labs(x = "Occurrence rate",
         y = "Log-Likelihood",
         title = "Log-likelihood profile",
         subtitle = "Known observation model parameters") +
    fig_theme

if (save_figures) {
    ggsave("out/llhd-profile-omega.pdf", plot = omega_figure, height = 10.5, width = 14.8, units = "cm")
}

nu_figure <- ggplot(filter(x, lambda == PARAMS$lambda, mu == PARAMS$mu, psi == PARAMS$psi, rho == PARAMS$rho, omega == PARAMS$omega), aes(x = nu, y = llhd)) +
    geom_line() +
    geom_vline(xintercept = PARAMS$nu, linetype = truth_linetype) +
    labs(x = "Disaster extinction probability",
         y = "Log-Likelihood",
         title = "Log-likelihood profile",
         subtitle = "Known observation model parameters") +
    fig_theme

if (save_figures) {
    ggsave("out/llhd-profile-nu.pdf", plot = nu_figure, height = 10.5, width = 14.8, units = "cm")
}


## Suppress warnings because otherwise there is a warning about a missing new
## line character that does not affect things.
simulationEvents <- suppressWarnings(readLines(config$outputEventsFile))
et <- as.list(table(str_extract(simulationEvents, "^[a-zA-Z]+")))

mask <- grepl(pattern = "CatastropheEvent", x = simulationEvents)
num_catastropheed <- sapply(str_match_all(simulationEvents[mask], pattern = "Person\\s[0-9]+"), length)
rm(mask)

mask <- grepl(pattern = "DisasterEvent", x = simulationEvents)
num_disastered <- sapply(str_match_all(simulationEvents[mask], pattern = "Person\\s[0-9]+"), length)
rm(mask)


num_unobserved_lineages <- 1 + et$InfectionEvent - et$OccurrenceEvent - et$RemovalEvent - et$SamplingEvent - sum(num_catastropheed) - sum(num_disastered)

## Because we do not include the true parameters in the LLHD evaluations, we
## need to get the closest match to them among those that we have.
unique_thresh <- 0.0006
curr_nb <- as.list(filter(x, abs(lambda - PARAMS$lambda) < unique_thresh, mu == PARAMS$mu, psi == PARAMS$psi, abs(rho - PARAMS$rho) < unique_thresh, omega == PARAMS$omega, abs(nu - PARAMS$nu) < unique_thresh) %>% select(starts_with("neg_binom")))
curr_nb <- list(neg_binom_r = mean(curr_nb$neg_binom_r), neg_binom_p = mean(curr_nb$neg_binom_p))

prev_mesh <- 130:350
plot_df <- data.frame(prevalence = prev_mesh, log_prob = dnbinom(prev_mesh, size = curr_nb$neg_binom_r, prob = curr_nb$neg_binom_p, log = TRUE))

prev_figure <- ggplot(plot_df, aes(x = prevalence, y = log_prob)) +
    geom_line() +
    geom_vline(xintercept = num_unobserved_lineages, linetype = truth_linetype) +
    labs(x = "Unobserved Lineages",
         y = "Log-Likelihood",
         title = "Log-likelihood profile",
         subtitle = "Distribution of remaining lineages at present assuming known parameters") +
    fig_theme

if (save_figures) {
    ggsave("out/llhd-profile-prevalence.pdf", plot = prev_figure, height = 10.5, width = 14.8, units = "cm")
}
