library(dplyr)
library(ggplot2)
library(cowplot)
library(latex2exp)
library(reshape2)
library(magrittr)
library(purrr)
library(jsonlite)
library(latex2exp)

axis_title_font_size <- 9

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
    ## breaks = plot_axis_breaks
  ) +
  scale_y_continuous(
    name = "Numeric ODE log-likelihood", # Marc has asked that his name not be
    # used to describe the method so this
    # has been used instead
    ## breaks = plot_axis_breaks
  ) +
  coord_fixed() +
  theme_classic() +
  theme(axis.title = element_text(face = "bold", size = axis_title_font_size))

## Using cowplot to combine the figures did not work very well so we save the
## figures to every useful format and then we use inkscape to combine them
## manually.
ggsave("out/llhd-comparison.png", llhd_comparison, height = 7.5, width = 7.4, units = "cm")
ggsave("out/llhd-comparison.pdf", llhd_comparison, height = 7.5, width = 7.4, units = "cm")
## ggsave("out/llhd-comparison.svg", llhd_comparison, height = 7.5, width = 7.4, units = "cm")

## It would be interesting to see if there is a relationship between the size of
## the dataset being analysed and the difference in the likelihood values
## calculated by the two methods both in absolute terms and as a proportion.
plot_df_2 <- plot_df %>%
  mutate(difference = bdscodLlhd - popSimLlhd,
         mean_val = 0.5 * (bdscodLlhd + popSimLlhd)) %>%
  select(size, difference, mean_val)

sink(file = "out/llhd-fit-summary.txt", append = TRUE)
print("================================================================================\n")
cat("Comparing LLHD values\n")
cat("================================================================================\n")
summary(lm(
  formula = difference ~ size,
  data = plot_df_2
))

summary(lm(
  formula = difference ~ mean_val,
  data = plot_df_2
))

summary(lm(
  formula = mean_val ~ size,
  data = plot_df_2
))
sink()


bland_altman <- ggplot(
  data = plot_df_2,
  mapping = aes(x = mean_val, y = difference)) +
  geom_point(
    shape = 1,
    size = 1
  ) +
  geom_hline(
    mapping = aes(yintercept = mean(difference))) +
  geom_hline(
    mapping = aes(yintercept = mean(difference) + 1.96 * sqrt(var(difference))),
    linetype = "dashed",
    colour = "grey"
  ) +
  geom_hline(
    mapping = aes(yintercept = mean(difference) - 1.96 * sqrt(var(difference))),
    linetype = "dashed",
    colour = "grey"
  ) +
  geom_smooth(
    method = "lm",
    linetype = "solid",
    colour = "black",
    se = TRUE,
    size = 0.3
  ) +
  labs(x = "Average of TimTam and\n(numeric) ODE method",
       y = "Difference between TimTam\nand (numeric) ODE method") +
  theme_classic() +
  theme(axis.title = element_text(face = "bold", size = axis_title_font_size))

diff_by_size <- ggplot(
  data = plot_df_2,
  mapping = aes(x = size, y = difference)
) +
  geom_point(
    shape = 1,
    size = 1
  ) +
  geom_smooth(
    method = "lm",
    linetype = "solid",
    colour = "black",
    se = TRUE,
    size = 0.3
  ) +
  labs(x = "Number of observed events",
       y = "Difference between TimTam\nand (numeric) ODE method") +
  theme_classic() +
  theme(axis.title = element_text(face = "bold", size = axis_title_font_size))

llhd_comparison_2 <- plot_grid(bland_altman, diff_by_size, ncol = 1, labels = c("A", "B"))

ggsave("out/llhd-comparison-2.png", llhd_comparison_2, height = 16.0, width = 8.4, units = "cm")
ggsave("out/llhd-comparison-2.pdf", llhd_comparison_2, height = 16.0, width = 8.4, units = "cm")

## Final comparison to explain difference in correlations.
llhd_comparison_3 <- ggplot(data = plot_df_2, mapping = aes(x = size, y = mean_val)) +
  geom_point(
    shape = 1,
    size = 1
  ) +
  geom_smooth(
    method = "lm",
    linetype = "solid",
    colour = "black",
    se = TRUE,
    size = 0.3
  ) +
  labs(x = "Number of observed events",
       y = "Average of TimTam and\n(numeric) ODE method") +
  theme_classic() +
  theme(axis.title = element_text(face = "bold", size = axis_title_font_size))

ggsave("out/llhd-comparison-3.png", llhd_comparison_3, height = 7.5, width = 7.4, units = "cm")
ggsave("out/llhd-comparison-3.pdf", llhd_comparison_3, height = 7.5, width = 7.4, units = "cm")

llhd_comparison_4 <- plot_grid(
  llhd_comparison,
  bland_altman,
  diff_by_size,
  ncol = 1,
  hjust = 0.0,
  labels = LETTERS[1:3]
)

ggsave("out/llhd-comparison-4.png", llhd_comparison_4, height = 22.5, width = 8.4, units = "cm")
ggsave("out/llhd-comparison-4.pdf", llhd_comparison_4, height = 22.5, width = 8.4, units = "cm")

## We also want to look at the values of the truncation parameter selected as a
## function of the size of the simulation.

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
## ggsave("out/truncation-comparison.svg", truncation_parameter_trend, height = 7.5, width = 7.4, units = "cm")
