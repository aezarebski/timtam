library(ggplot2)
library(dplyr)
library(stringr)
library(gridExtra)

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
} else {
    cat("Found the input file:", INPUT_FILE, "\n")
}

x <- read.csv(INPUT_FILE, header = FALSE, stringsAsFactors = FALSE)
names(x) <- c("variedParameter", "lambda", "mu", "psi", "rho", "omega", "nu", "llhd", "negative_binomial")
nb_params <- t(sapply(str_extract_all(x$negative_binomial, "[\\d.]+(?:e-?\\d+)?"), as.numeric))
x$neg_binom_r <- nb_params[,1]
x$neg_binom_p <- 1 - nb_params[,2] # R and Wikipedia have different parameterisations.


font_scale_factor <- 0.5
fig_theme <- theme(
    axis.title.x = element_text(size = font_scale_factor * 22),
    axis.title.y = element_blank(),
    ## axis.title.y = element_text(size = font_scale_factor * 22),
    axis.ticks = ggplot2::element_blank(),
    axis.line.x.bottom = element_line(colour = "#000000"),
    legend.title = element_text(size = font_scale_factor * 22),
    plot.title = element_text(size = font_scale_factor * 32),
    plot.subtitle = element_text(size = font_scale_factor * 22),
    panel.grid.minor.x = ggplot2::element_blank(),
    panel.grid.minor.y = ggplot2::element_blank(),
    ## panel.grid.minor.y = ggplot2::element_line(color="#cbcbcb"),
    panel.grid.major.y = ggplot2::element_line(color="#cbcbcb"),
    panel.grid.major.x = ggplot2::element_blank(),
    panel.background = ggplot2::element_blank(),
    strip.background = ggplot2::element_rect(fill="white"))

y_axis_breaks <- seq(from = -2e3, to = 1e3, by = 5)

truth_linetype <- "dashed"

lambda_figure <- ggplot(filter(x, variedParameter == "ParamLambda"), aes(x = lambda, y = llhd)) +
    geom_line() +
    geom_vline(xintercept = PARAMS$lambda, linetype = truth_linetype) +
    labs(x = "Birth rate",
         y = "Log-Likelihood") +
    scale_y_continuous(breaks = y_axis_breaks) +
    scale_x_continuous(breaks = scales::pretty_breaks(n = 4)) +
    fig_theme

if (save_figures) {
    ggsave("out/llhd-profile-lambda.pdf",
           plot = lambda_figure + labs(title = "Log-likelihood profile", subtitle = "Known observation model parameters"),
           height = 10.5, width = 14.8, units = "cm")
}

mu_figure <- ggplot(filter(x, variedParameter == "ParamMu"), aes(x = mu, y = llhd)) +
    geom_line() +
    geom_vline(xintercept = PARAMS$mu, linetype = truth_linetype) +
    labs(x = "Death rate",
         y = "Log-Likelihood") +
    scale_y_continuous(breaks = y_axis_breaks) +
    fig_theme

if (save_figures) {
    ggsave("out/llhd-profile-mu.pdf",
           plot = mu_figure + labs(title = "Log-likelihood profile", subtitle = "Known observation model parameters"),
           height = 10.5, width = 14.8, units = "cm")
}

psi_figure <- ggplot(filter(x, variedParameter == "ParamPsi"), aes(x = psi, y = llhd)) +
    geom_line() +
    geom_vline(xintercept = PARAMS$psi, linetype = truth_linetype) +
    labs(x = "Sampling rate",
         y = "Log-Likelihood") +
    scale_y_continuous(breaks = y_axis_breaks) +
    fig_theme

if (save_figures) {
    ggsave("out/llhd-profile-psi.pdf",
           plot = psi_figure + labs(title = "Log-likelihood profile", subtitle = "Known observation model parameters"),
           height = 10.5, width = 14.8, units = "cm")
}

rho_figure <- ggplot(filter(x, variedParameter == "ParamRho"), aes(x = rho, y = llhd)) +
    geom_line() +
    geom_vline(xintercept = PARAMS$rho, linetype = truth_linetype) +
    labs(x = "Catastrophe extinction\nprobability",
         y = "Log-Likelihood") +
    scale_y_continuous(breaks = y_axis_breaks) +
    scale_x_continuous(breaks = scales::pretty_breaks(n = 3)) +
    fig_theme

if (save_figures) {
    ggsave("out/llhd-profile-rho.pdf",
           plot = rho_figure + labs(title = "Log-likelihood profile", subtitle = "Known observation model parameters"),
           height = 10.5, width = 14.8, units = "cm")
}

omega_figure <- ggplot(filter(x, variedParameter == "ParamOmega"), aes(x = omega, y = llhd)) +
    geom_line() +
    geom_vline(xintercept = PARAMS$omega, linetype = truth_linetype) +
    labs(x = "Occurrence rate",
         y = "Log-Likelihood") +
    scale_y_continuous(breaks = y_axis_breaks) +
    fig_theme

if (save_figures) {
    ggsave("out/llhd-profile-omega.pdf",
           plot = omega_figure + labs(title = "Log-likelihood profile", subtitle = "Known observation model parameters"),
           height = 10.5, width = 14.8, units = "cm")
}

nu_figure <- ggplot(filter(x, variedParameter == "ParamNu"), aes(x = nu, y = llhd)) +
    geom_line() +
    geom_vline(xintercept = PARAMS$nu, linetype = truth_linetype) +
    labs(x = "Disaster extinction\nprobability",
         y = "Log-Likelihood") +
    scale_y_continuous(breaks = y_axis_breaks) +
    scale_x_continuous(breaks = scales::pretty_breaks(n = 3)) +
    fig_theme

if (save_figures) {
    ggsave("out/llhd-profile-nu.pdf",
           plot = nu_figure + labs(title = "Log-likelihood profile", subtitle = "Known observation model parameters"),
           height = 10.5, width = 14.8, units = "cm")
}



## We want a plot combining the profiles for all of the parameters so we need to
## combine them now.

combined_llhd_profiles <- grid.arrange(lambda_figure,
                                       mu_figure,
                                       psi_figure,
                                       rho_figure,
                                       omega_figure,
                                       nu_figure,
                                       ncol=3,
                                       left = "Log-Likelihood")

if (save_figures) {
    ggsave("out/parameter-llhd-profiles.pdf",
           plot = combined_llhd_profiles,
           height = 10.5, width = 14.8, units = "cm")
}


## Suppress warnings because otherwise there is a warning about a missing new
## line character that does not affect things.
simulationEvents <- suppressWarnings(readLines(config$outputEventsFile))
et <- as.list(table(str_extract(simulationEvents, "^[a-zA-Z]+")))

mask <- grepl(pattern = "Catastrophe", x = simulationEvents)
num_catastropheed <- sapply(str_match_all(simulationEvents[mask], pattern = "Person\\s[0-9]+"), length)
rm(mask)

mask <- grepl(pattern = "Disaster", x = simulationEvents)
num_disastered <- sapply(str_match_all(simulationEvents[mask], pattern = "Person\\s[0-9]+"), length)
rm(mask)

num_unobserved_lineages <- 1 + et$Infection - et$Occurrence - et$Removal - et$Sampling - sum(num_catastropheed) - sum(num_disastered)
cat(sprintf("\nThe number of unobserved lineages, a.k.a. the final prevalence, is %d\n\n", num_unobserved_lineages))

## Because we do not include the true parameters in the LLHD evaluations, we
## need to get the closest match to them among those that we have.
unique_thresh <- 0.006
curr_nb <- as.list(filter(x, abs(lambda - PARAMS$lambda) < unique_thresh, mu == PARAMS$mu, psi == PARAMS$psi, abs(rho - PARAMS$rho) < unique_thresh, omega == PARAMS$omega, abs(nu - PARAMS$nu) < unique_thresh) %>% select(starts_with("neg_binom")))
curr_nb <- list(neg_binom_r = mean(curr_nb$neg_binom_r), neg_binom_p = mean(curr_nb$neg_binom_p))

prev_mesh <- seq(from = max(num_unobserved_lineages - 100,0), to = min(num_unobserved_lineages + 100,1e3), by = 1)
plot_df <- data.frame(prevalence = prev_mesh, log_prob = dnbinom(prev_mesh, size = curr_nb$neg_binom_r, prob = curr_nb$neg_binom_p, log = TRUE))

prev_figure <- ggplot(plot_df, aes(x = prevalence, y = log_prob)) +
    geom_line() +
    geom_vline(xintercept = num_unobserved_lineages, linetype = truth_linetype) +
    labs(x = "Unobserved Lineages",
         y = "Log-Likelihood") +
    fig_theme +
    theme(axis.title.y = element_text(size = font_scale_factor * 22))

if (save_figures) {
    ggsave("out/llhd-profile-prevalence.pdf", plot = prev_figure, height = 10.5, width = 14.8, units = "cm")
}
