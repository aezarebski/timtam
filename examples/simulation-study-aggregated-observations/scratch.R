library(dplyr)
library(purrr)
library(magrittr)
library(ggplot2)
library(cowplot)
library(stringr)
library(reshape2)
library(jsonlite)
SAVE_FIGURES <- TRUE

green_hex_colour <- "#7fc97f"

app_config <- read_json("agg-app-config.json")



sim_param_df <- data.frame(
  lambda = app_config$simulationParameters[[1]],
  psi = app_config$simulationParameters[[3]],
  omega = app_config$simulationParameters[[5]]
)

reg_data_posterior_df <- read.csv("regular-data-mcmc-samples.csv", stringsAsFactors = FALSE) %>%
  select(lambda, psi, omega)
## Remove the burn in from the start of the samples
reg_data_posterior_df <- tail(reg_data_posterior_df, -1e3)

param_labels <- c(lambda = "Birth rate", psi = "Sequenced sampling rate", omega = "Unsequenced sampling rate")

g1_df <- reg_data_posterior_df %>%
  melt(id.vars = c(), variable.name = "parameter")

g1 <- ggplot() +
  geom_density(data = g1_df, mapping = aes(x = value, y = ..density..)) +
  facet_wrap(~parameter, scales = "free", labeller = labeller(parameter = param_labels)) +
  labs(y = "Posterior density", x = NULL) +
  theme_classic() +
  theme(axis.title = element_text(face = "bold", size = 17),
        axis.text = element_text(size = 15),,
        strip.background = element_blank(),
        strip.text = element_text(face = "bold", size = 17))

fig_height <- 10

if (SAVE_FIGURES) {
  ggsave("out/regular-data-posterior-marginals.png",
    g1,
    height = fig_height,
    width = 2.8 * fig_height,
    units = "cm"
  )
  ggsave("out/regular-data-posterior-marginals.pdf",
    g1,
    height = fig_height,
    width = 2.8 * fig_height,
    units = "cm"
  )
}


## Subsample because otherwise there is massive overplotting which makes it hard
## to grok
g2_df <- reg_data_posterior_df %>% sample_n(size = 2e3)

g2_alpha <- 0.1
g2_size <- 3

g2_a <- ggplot(mapping = aes(x = lambda, y = psi)) +
  geom_vline(xintercept = sim_param_df$lambda, linetype = "dashed") +
  geom_hline(yintercept = sim_param_df$psi, linetype = "dashed") +
  geom_point(data = g2_df, size = g2_size, alpha = g2_alpha) +
  geom_point(data = as.data.frame(as.list(colMeans(g2_df))), colour = green_hex_colour, size = g2_size + 4, shape = 18) +
  theme_classic() +
  theme(axis.title = element_text(face = "bold")) +
  labs(x = param_labels["lambda"], y = param_labels["psi"])
g2_b <- ggplot(mapping = aes(x = omega, y = psi)) +
  geom_vline(xintercept = sim_param_df$omega, linetype = "dashed") +
  geom_hline(yintercept = sim_param_df$psi, linetype = "dashed") +
  geom_point(data = g2_df, size = g2_size, alpha = g2_alpha) +
  geom_point(data = as.data.frame(as.list(colMeans(g2_df))), colour = green_hex_colour, size = g2_size + 4, shape = 18) +
  theme_classic() +
  theme(axis.title = element_text(face = "bold")) +
  labs(x = param_labels["omega"], y = param_labels["psi"])
g2_c <- ggplot(mapping = aes(x = lambda, y = omega)) +
  geom_vline(xintercept = sim_param_df$lambda, linetype = "dashed") +
  geom_hline(yintercept = sim_param_df$omega, linetype = "dashed") +
  geom_point(data = g2_df, size = g2_size, alpha = g2_alpha) +
  geom_point(data = as.data.frame(as.list(colMeans(g2_df))), colour = green_hex_colour, size = g2_size + 4, shape = 18) +
  theme_classic() +
  theme(axis.title = element_text(face = "bold")) +
  labs(x = param_labels["lambda"], y = param_labels["omega"])

g2 <- plot_grid(g2_a, g2_b, g2_c, ncol = 2)


fig_height <- 20

if (SAVE_FIGURES) {
  ggsave("out/regular-data-posterior-joints.png",
    g2,
    height = fig_height,
    width = 1.0 * fig_height,
    units = "cm"
  )
  ggsave("out/regular-data-posterior-joints.pdf",
    g2,
    height = fig_height,
    width = 1.0 * fig_height,
    units = "cm"
  )
}





















agg_data_posterior_df <- read.csv("aggregated-data-mcmc-samples.csv", stringsAsFactors = FALSE) %>%
  select(lambda, rho, nu)
## Remove the burn in from the start of the samples
agg_data_posterior_df <- tail(agg_data_posterior_df, -1e3)

param_labels <- c(lambda = "Birth rate", rho = "Sequenced sampling\nprobability", nu = "Unsequenced sampling\nprobability")

g3_df <- agg_data_posterior_df %>%
  melt(id.vars = c(), variable.name = "parameter")

g3 <- ggplot() +
  geom_density(data = g3_df, mapping = aes(x = value, y = ..density..)) +
  facet_wrap(~parameter, scales = "free", labeller = labeller(parameter = param_labels)) +
  labs(y = "Posterior density", x = NULL) +
  theme_classic() +
  theme(axis.title = element_text(face = "bold", size = 17),
        axis.text = element_text(size = 15),
        strip.background = element_blank(),
        strip.text = element_text(face = "bold", size = 17))

fig_height <- 10

if (SAVE_FIGURES) {
  ggsave("out/aggregated-data-posterior-marginals.png",
    g3,
    height = fig_height,
    width = 2.8 * fig_height,
    units = "cm"
  )
  ggsave("out/aggregated-data-posterior-marginals.pdf",
    g3,
    height = fig_height,
    width = 2.8 * fig_height,
    units = "cm"
  )
}


## Subsample because otherwise there is massive overplotting which makes it hard
## to grok
g4_df <- agg_data_posterior_df %>% sample_n(size = 2e3)

g4_alpha <- 0.1
g4_size <- 3

g4_a <- ggplot(mapping = aes(x = lambda, y = rho)) +
  geom_vline(xintercept = sim_param_df$lambda, linetype = "dashed") +
  geom_point(data = g4_df, size = g4_size, alpha = g4_alpha) +
  geom_point(data = as.data.frame(as.list(colMeans(g4_df))), colour = purple_hex_colour, size = g4_size + 4, shape = 18) +
  theme_classic() +
  theme(axis.title = element_text(face = "bold")) +
  labs(x = param_labels["lambda"], y = param_labels["rho"])
g4_b <- ggplot(mapping = aes(x = nu, y = rho)) +
  geom_point(data = g4_df, size = g4_size, alpha = g4_alpha) +
  geom_point(data = as.data.frame(as.list(colMeans(g4_df))), colour = purple_hex_colour, size = g4_size + 4, shape = 18) +
  theme_classic() +
  theme(axis.title = element_text(face = "bold")) +
  labs(x = param_labels["nu"], y = param_labels["rho"])
g4_c <- ggplot(mapping = aes(x = lambda, y = nu)) +
  geom_vline(xintercept = sim_param_df$lambda, linetype = "dashed") +
  geom_point(data = g4_df, size = g4_size, alpha = g4_alpha) +
  geom_point(data = as.data.frame(as.list(colMeans(g4_df))), colour = purple_hex_colour, size = g4_size + 4, shape = 18) +
  theme_classic() +
  theme(axis.title = element_text(face = "bold")) +
  labs(x = param_labels["lambda"], y = param_labels["nu"])

g4 <- plot_grid(g4_a, g4_b, g4_c, ncol = 2)


fig_height <- 20

if (SAVE_FIGURES) {
  ggsave("out/aggregated-data-posterior-joints.png",
    g4,
    height = fig_height,
    width = 1.0 * fig_height,
    units = "cm"
  )
  ggsave("out/aggregated-data-posterior-joints.pdf",
    g4,
    height = fig_height,
    width = 1.0 * fig_height,
    units = "cm"
  )
}























