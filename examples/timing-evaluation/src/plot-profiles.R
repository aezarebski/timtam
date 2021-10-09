library(dplyr)
library(ggplot2)
library(reshape2)
library(magrittr)
library(purrr)
library(jsonlite)
library(scales)


config <- read_json("app-config.json")

x1 <- read.csv("criterion-times.csv") %>% rename(bdscodMeanSeconds = Mean)
x2 <- read.csv(config$acOutputCsv, header = FALSE) %>%
  set_names(c("Size", "Llhd", "Name"))
bdscod_records <- left_join(x1, x2, by = "Name")

try_read_json <- function(json_filepath) {
    tryCatch(expr = as.data.frame(read_json(json_filepath)),
             error = function(e) {
                 warning("Could not read file: ", json_filepath);
                 NULL
             })

}

pop_sim_records <- list.files(path = "out/", pattern = "^popsize", full.names = TRUE) %>%
    map(try_read_json) %>%
    keep(compose(`!`, is.null)) %>%
    bind_rows %>%
    mutate(Name = gsub(pattern = ".*observations", replacement = "out/simulated-observations", x = inputJson))

## Since the evaluation time is given as the total amount of time it took to
## evaluate the log-likelihood multiple times, we need to divide it by the
## number of replicates to get the sample average of the evaluation time.
tmp <- pop_sim_records %>%
  rename(popsizeMeanSeconds = evaluationTime / numReplicates) %>%
  select(Name, popsizeMeanSeconds)
plot_df <- left_join(bdscod_records, tmp, by = "Name") %>%
    select(Size, bdscodMeanSeconds, popsizeMeanSeconds) %>%
  filter(not(is.na(popsizeMeanSeconds))) %>%
    melt(id.vars = "Size")
rm(tmp)


## The following commands can be used to fit a model to understand the scaling
## of the timing of the computation.
bdscod_model <- plot_df %>%
    filter(variable == "bdscodMeanSeconds") %>%
    mutate(ln_size = log(Size),
           ln_time = log(value)) %>%
    {lm(ln_time ~ ln_size, data = .)}

popsize_model <- plot_df %>%
    filter(variable == "popsizeMeanSeconds") %>%
    mutate(ln_size = log(Size),
           ln_time = log(value)) %>%
    {lm(ln_time ~ ln_size, data = .)}

## We need to run the predictive model on a mesh so that we can display it later
## as the model fit. We use the range of the measured sizes as a way to choose a
## suitable range of values to evaluate this over.
x_vals <- seq(
  from = 1,
  to = max(plot_df$Size) + min(plot_df$Size),
  ## to = config$acBinWidth * config$acNumBins,
  length = 200
)
y_vals1 <- exp(predict(bdscod_model, data.frame(ln_size = log(x_vals))))
y_vals2 <- exp(predict(popsize_model, data.frame(ln_size = log(x_vals))))
y_vals <- c(y_vals1, y_vals2)

plot_df_2 <- data.frame(Size = rep(x_vals, 2),
                        value = y_vals,
                        variable = rep(c("bdscodMeanSeconds",
                                         "popsizeMeanSeconds"),
                                       each = length(x_vals)))

## Since in both of the models the smallest data set is an outlier for the
## models we are using we repeat the estimation using robust linear regression
## as an alternative to arguing that the estimates are not unduly influenced by
## the initial data points. Note that \code{MASS} is a recommended package for R
## so is available in all reputable implementations.
bdscod_robust_model <- plot_df %>%
  filter(variable == "bdscodMeanSeconds") %>%
  mutate(ln_size = log(Size),
         ln_time = log(value)) %>%
  {MASS::rlm(ln_time ~ ln_size, data = .)}

popsize_robust_model <- plot_df %>%
  filter(variable == "popsizeMeanSeconds") %>%
  mutate(ln_size = log(Size),
         ln_time = log(value)) %>%
  {MASS::rlm(ln_time ~ ln_size, data = .)}

## Save a copy of the linear model summaries so we can check these later to
## describe the model fits in the manuscript
sink(file = "out/model-fit-summary.txt")
cat("================================================================================\n")
cat("BDSCOD model fit\n")
cat("================================================================================\n")
summary(bdscod_model)
cat("================================================================================\n")
cat("BDSCOD *ROBUST* model fit\n")
cat("================================================================================\n")
summary(bdscod_robust_model)
cat("\n================================================================================\n")
cat("Manceau et al (2020) model fit\n")
cat("================================================================================\n")
summary(popsize_model)
cat("\n================================================================================\n")
cat("Manceau et al (2020) *ROBUST* model fit\n")
cat("================================================================================\n")
summary(popsize_robust_model)
sink()

## Now we actually put together the plot so we can see what the times look like
## side by side.

facet_label_map <- c(bdscodMeanSeconds = "TimTam log-likelihood",
                     popsizeMeanSeconds = "Numeric ODE log-likelihood")

g <- ggplot(data = plot_df,
            mapping = aes(x = Size, y = value)) +
  geom_point(shape = 1,
             size = 1) +
  geom_line(data = plot_df_2) +
  facet_wrap(~variable,
             scales = "free_y",
             labeller = labeller(variable = facet_label_map)) +
  labs(x = "Number of observed events",
       y = "Mean evaluation time (seconds)") +
  scale_y_continuous(labels = number) +
  theme_classic() +
  theme(
    strip.background = element_blank(),
    strip.text = element_text(face = "bold"),
    axis.title = element_text(face = "bold")
  )

## print(g)
ggsave("out/profiles.png", g, height = 7.5, width = 14.8, units = "cm")
ggsave("out/profiles.pdf", g, height = 7.5, width = 14.8, units = "cm")
