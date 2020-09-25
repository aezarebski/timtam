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
    tryCatch(expr = as.data.frame(read_json(json_filepath)),
             error = function(e) {
                 warning("Could not read file: ", json_filepath);
                 NULL
             })

}

pop_sim_llhds <- list.files(path = "out/", pattern = "^popsize", full.names = TRUE) %>%
    map(try_read_json) %>%
    keep(compose(`!`, is.null)) %>%
    bind_rows %>%
    mutate(name = gsub(pattern = ".*observations", replacement = "out/simulated-observations", x = inputJson)) %>%
    rename(popSimLlhd = convergedLlhd) %>%
    select(name, popSimLlhd, truncationParameter)

plot_df <- left_join(bdscod_llhds, pop_sim_llhds, by = "name")



## We save a copy of a summary of the linear model between the LLHDs of the two
## methods so that we can quote the R^2 value, the amount of the variation
## explained.
sink(file = "out/llhd-fit-summary.txt")
print("================================================================================\n")
cat("Linear model comparing LLHD values\n")
cat("================================================================================\n")
summary(lm(formula = popSimLlhd ~ bdscodLlhd,
           data = plot_df))
sink()

lm_rsquared <- summary(lm(formula = popSimLlhd ~ bdscodLlhd,
           data = plot_df))$r.squared
lm_annotation <- TeX(sprintf("Linear model $R^2 = %.3f$", lm_rsquared))

## We make a scatter plot comparing the LLHDs from the two evaluation strategies
## to make sure that the new approximation is accurate. We compute the limits
## manually so we can set them to the same values to improve the clarity of the
## comparison.
llhd_range <- range(c(plot_df$bdscodLlhd, plot_df$popSimLlhd))
plot_lower_lim <- llhd_range[1] %>% divide_by(10) %>% floor() %>% multiply_by(10)
plot_upper_lim <- llhd_range[2] %>% divide_by(10) %>% ceiling() %>% multiply_by(10)

llhd_comparison <- ggplot(data = plot_df,
                          mapping = aes(x = bdscodLlhd,
                                        y = popSimLlhd)) +
  geom_smooth(method = "lm", linetype = "dashed", colour = "grey", size = 0.3, alpha = 0.2) +
  geom_abline(intercept = 0, slope = 1, linetype = "solid", size = 0.3) +
  geom_point(size = 1) +
  annotate(geom = "text",
           x = -50, y = -100,
           label = lm_annotation) +
  labs(x = "BDSCOD log-likelihood",
       y = "Manceau et al (2020)\nlog-likelihood") +
  coord_fixed() +
  xlim(plot_lower_lim, plot_upper_lim) +
  ylim(plot_lower_lim, plot_upper_lim) +
  theme_classic() +
  theme(axis.title = element_text(size = 5),
        axis.text = element_text(size = 5),
        axis.line = element_line(size = 0.2))

ggsave("out/llhd-comparison.png", llhd_comparison, height = 5.25, width = 7.4, units = "cm")
ggsave("out/llhd-comparison.pdf", llhd_comparison, height = 5.25, width = 7.4, units = "cm")

truncation_parameter_trend <-
    ggplot(data = plot_df,
           mapping = aes(x = size,
                         y = truncationParameter)) +
    geom_smooth(method = "lm", colour = "grey", size = 0.3, alpha = 0.2) +
    geom_point(size = 1) +
    labs(x = "Size of dataset",
         y = TeX("Truncation parameter, $N$")) +
    theme_classic() +
    theme(axis.title = element_text(size = 5),
          axis.text = element_text(size = 5),
          axis.line = element_line(size = 0.2))

ggsave("out/truncation-comparison.png", truncation_parameter_trend, height = 5.25, width = 7.4, units = "cm")
ggsave("out/truncation-comparison.pdf", truncation_parameter_trend, height = 5.25, width = 7.4, units = "cm")

comb_plot <- plot_grid(llhd_comparison, truncation_parameter_trend, ncol = 2, labels = c("A", "B"))


ggsave("out/bdscod-popsize-comparison.png", comb_plot, height = 10.5, width = 14.8, units = "cm")
ggsave("out/bdscod-popsize-comparison.pdf", comb_plot, height = 10.5, width = 14.8, units = "cm")
