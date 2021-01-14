library(dplyr)
library(ggplot2)
library(cowplot)
library(latex2exp)
library(reshape2)
library(magrittr)
library(purrr)
library(jsonlite)
library(latex2exp)


bdscod_llhds <- read.csv("out/simulation-sizes-and-llhds.csv", header = FALSE) %>%
  set_names(c("size", "bdscodLlhd", "name"))


try_read_json <- function(json_filepath) {
  tryCatch(
    expr = as.data.frame(read_json(json_filepath)),
    error = function(e) {
      warning("Could not read file: ", json_filepath)
      NULL
    }
  )
}

pop_sim_llhds <- list.files(path = "out/", pattern = "^popsize", full.names = TRUE) %>%
  map(try_read_json) %>%
  keep(compose(`!`, is.null)) %>%
  bind_rows() %>%
  mutate(name = gsub(pattern = ".*observations", replacement = "out/simulated-observations", x = inputJson)) %>%
  rename(popSimLlhd = convergedLlhd) %>%
  select(name, popSimLlhd, truncationParameter)

## We need to filter the resulting data frame because there there are missing
## entries from the \code{pop_sim_llhds} data frame for the simulations where a
## truncation parameter was not found.
plot_df <- left_join(bdscod_llhds, pop_sim_llhds, by = "name") %>%
  filter(not(is.na(truncationParameter)))


## We save a copy of a summary of the linear model between the LLHDs of the two
## methods so that we can quote the R^2 value, the amount of the variation
## explained.
sink(file = "out/llhd-fit-summary.txt")
print("================================================================================\n")
cat("Linear model comparing LLHD values\n")
cat("================================================================================\n")
summary(lm(
  formula = popSimLlhd ~ bdscodLlhd,
  data = plot_df
))
sink()

## We make a scatter plot comparing the LLHDs from the two evaluation strategies
## to make sure that the new approximation is accurate. We compute the limits
## manually so we can set them to the same values to improve the clarity of the
## comparison.
plot_axis_breaks <- seq(from = -100, to = 50, by = 50)

llhd_comparison <- ggplot(
  data = plot_df,
  mapping = aes(
    x = bdscodLlhd,
    y = popSimLlhd
  )
) +
  geom_smooth(
    method = "lm",
    linetype = "solid",
    colour = "black",
    se = FALSE,
    size = 0.3
  ) +
  geom_abline(
    intercept = 0,
    slope = 1,
    linetype = "dashed",
    colour = "grey",
    size = 0.3
  ) +
  geom_point(
    shape = 1,
    size = 1
  ) +
  scale_x_continuous(
    name = "TimTam log-likelihood",
    breaks = plot_axis_breaks
  ) +
  scale_y_continuous(
    name = "Numeric ODE log-likelihood", # Marc has asked that his name not be
    # used to describe the method so this
    # has been used instead
    breaks = plot_axis_breaks
  ) +
  coord_fixed() +
  theme_classic() +
  theme(axis.title = element_text(face = "bold"))

## Using cowplot to combine the figures did not work very well so we save the
## figures to every useful format and then we use inkscape to combine them
## manually.
ggsave("out/llhd-comparison.png", llhd_comparison, height = 7.5, width = 7.4, units = "cm")
ggsave("out/llhd-comparison.pdf", llhd_comparison, height = 7.5, width = 7.4, units = "cm")
ggsave("out/llhd-comparison.svg", llhd_comparison, height = 7.5, width = 7.4, units = "cm")

truncation_parameter_trend <-
  ggplot(
    data = plot_df,
    mapping = aes(
      x = size,
      y = truncationParameter
    )
  ) +
  geom_smooth(
    method = "lm",
    colour = "black",
    se = FALSE,
    size = 0.3
  ) +
  geom_point(
    shape = 1,
    size = 1
  ) +
  labs(
    x = "Number of observed events",
    y = "Truncation parameter"
  ) +
  theme_classic() +
  theme(axis.title = element_text(face = "bold"))

## Using cowplot to combine the figures did not work very well so we save the
## figures to every useful format and then we use inkscape to combine them
## manually.
ggsave("out/truncation-comparison.png", truncation_parameter_trend, height = 7.5, width = 7.4, units = "cm")
ggsave("out/truncation-comparison.pdf", truncation_parameter_trend, height = 7.5, width = 7.4, units = "cm")
ggsave("out/truncation-comparison.svg", truncation_parameter_trend, height = 7.5, width = 7.4, units = "cm")
