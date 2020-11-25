library(dplyr)
library(purrr)
library(magrittr)
library(ggplot2)
library(stringr)
library(jsonlite)


## =============================================================================
## Generate cross sections of the LLHD function in the birth rate.
## =============================================================================
x <- list("out/llhd-evaluations-est-params-agg-data.csv",
          "out/llhd-evaluations-est-params-regular-data.csv",
          "out/llhd-evaluations-true-params-regular-data.csv")

read_llhds <- function(filename) {
  if (file.exists(filename)) {
    filename %>%
      read.csv(header = FALSE) %>%
      set_names(c("type", "value")) %>%
      mutate(lambda = seq(from = 1, to = 3.5, length = 100))
  } else {
    NULL
  }
  }

y <- map(x,
         read_llhds) %>%
  keep(compose(not, is_null)) %>% 
  bind_rows

g <- ggplot(y) +
  geom_line(mapping = aes(x = lambda, y = value)) +
  facet_wrap(~type)

ggsave("scratch-output-1.png", g)


## =============================================================================
## Generate a figure looking at the posterior distribution of the prevalence
## =============================================================================
x <- list(
  "out/final-negative-binomial-est-params-agg-data.csv",
  "out/final-negative-binomial-est-params-regular-data.csv",
  "out/final-negative-binomial-true-params-regular-data.csv"
  )

read_nb <- function(filename) {

  if (file.exists(filename)) {
    foo <- filename %>% readLines %>% str_split(pattern = "(,| )") %>% unlist

    percentile_probs <- c(0.025, 0.25, 0.5, 0.75, 0.975)
    percentile_vals <- qnbinom(p = c(0.025, 0.25, 0.5, 0.75, 0.975),
                               size = as.numeric(foo[3]),
                               prob = 1 - as.numeric(foo[4]))
    estimate_type <- foo[1]

    data.frame(percentile_prob = percentile_probs,
               percentile_value = percentile_vals,
               estimate_name = estimate_type)
  } else {
    NULL
  }
}

y <- map(x, read_nb) %>%
  keep(compose(not, is_null)) %>%
  bind_rows

g <- ggplot(y) +
  geom_point(mapping = aes(x = estimate_name,
                           y = percentile_value,
                           colour = percentile_prob)) +
  ylim(c(0, 1.1 * max(y$percentile_value)))


ggsave("scratch-output-2.png", g)
