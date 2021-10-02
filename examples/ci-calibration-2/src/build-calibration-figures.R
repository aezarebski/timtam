suppressPackageStartupMessages(library(argparse))
suppressPackageStartupMessages(library(coda))
suppressPackageStartupMessages(library(ggplot2))
suppressPackageStartupMessages(library(reshape2))
suppressPackageStartupMessages(library(dplyr))

green_hex_colour <- "#7fc97f"
purple_hex_colour <- "#beaed4"

parser <- ArgumentParser()

parser$add_argument(
  "--input",
  type = "character",
  help = "Filepath containing summary."
)
parser$add_argument(
  "--output",
  type = "character",
  help = "Filepath to write plot to."
)


make_prevalence_comparison <- function(args) {
  input_list <- jsonlite::read_json(args$input, simplifyVector = TRUE)
  true_prevalence <- input_list$prevalence

  plot_df <- input_list$estimates |>
    filter(variable == "prevalence") |>
    select(-variable) |>
    dcast(replicate + from_aggregated ~ quantile)
  names(plot_df) <- c(c("replicate", "from_aggregated"), sprintf("q%d", 1:5))
  plot_df <- left_join(x = plot_df, y = true_prevalence, by = "replicate")

  plot_df <- plot_df[order(plot_df$prevalence), ]
  ## add another variable which can be used on the x-axis while respecting the
  ## prevalence ordering.
  plot_df$sorted_order <- sort(plot_df$replicate)

  facet_labels <- c("TRUE" = "Aggregated data", "FALSE" = "Exact sampling dates")

  prev_fig_1 <- ggplot(data = plot_df) +
    geom_linerange(mapping = aes(x = sorted_order, ymin = q1, ymax = q5, colour = from_aggregated)) +
    geom_point(mapping = aes(x = sorted_order, y = prevalence)) +
    facet_grid(from_aggregated ~ ., scale = "free_y", labeller = labeller(from_aggregated = facet_labels)) +
    labs(x = "Replicate", y = "Prevalence", colour = NULL) +
    scale_colour_manual(values = c(green_hex_colour, purple_hex_colour)) +
    theme_classic() +
    theme(
      axis.text.x = element_blank(),
      strip.background = element_blank(),
      legend.position = "null"
    )
  ## print(prev_fig_1)
  ggsave(filename = "out/prevalence-calibration-extra-1.png", plot = prev_fig_1, height = 14.8, width = 10.5, units = "cm")

  prev_fig_2 <- ggplot(data = plot_df) +
    geom_linerange(
      mapping = aes(
        x = sorted_order,
        ymin = (q1 - prevalence) / prevalence,
        ymax = (q5 - prevalence) / prevalence,
        colour = from_aggregated
      )
    ) +
    facet_grid(from_aggregated ~ ., scale = "free_y", labeller = labeller(from_aggregated = facet_labels)) +
    geom_hline(yintercept = 0, linetype = "dashed") +
    ## geom_hline(mapping = aes(yintercept = median((q3 - prevalence) / prevalence),
    ##                          colour = from_aggregated)) +
    scale_colour_manual(values = c(green_hex_colour, purple_hex_colour)) +
    labs(x = "Replicate", y = "Proportional error", colour = NULL) +
    theme_classic() +
    theme(
      axis.text.x = element_blank(),
      strip.background = element_blank(),
      legend.position = "null"
    )
  ## print(prev_fig_2)
  ggsave(filename = args$output, plot = prev_fig_2, height = 14.8, width = 10.5, units = "cm")
}

make_prev_r_naught_fig <- function(args) {
  true_params <- jsonlite::read_json("../example-parameters.json")
  true_r_naught <- true_params$birthRate / (true_params$deathRate + true_params$samplingRate + true_params$occurrenceRate)
  input_list <- jsonlite::read_json(args$input, simplifyVector = TRUE)
  true_prevalence <- input_list$prevalence
  plot_df <- input_list$estimates |>
    filter(variable == "prevalence" | variable == "r_naught", !from_aggregated) |>
    select(-from_aggregated) |>
    dcast(variable + replicate ~ quantile)
  names(plot_df) <- c(c("variable", "replicate"), sprintf("q%d", 1:5))
  plot_df <- left_join(x = plot_df, y = true_prevalence, by = "replicate")

  ## sort first by replicate so this is used as a tie-breaker.
  plot_df <- plot_df[order(plot_df$replicate), ]
  plot_df <- plot_df[order(plot_df$prevalence), ]
  ## add another variable which can be used on the x-axis while respecting the
  ## prevalence ordering.
  plot_df$sorted_order <- sort(plot_df$replicate)

  ## convert the prevalence estimates to relative errors
  prev_mask <- plot_df$variable == "prevalence"
  plot_df[prev_mask, ]$q1 <- (plot_df[prev_mask, ]$q1 - plot_df[prev_mask, ]$prevalence) / plot_df[prev_mask, ]$prevalence
  plot_df[prev_mask, ]$q2 <- (plot_df[prev_mask, ]$q2 - plot_df[prev_mask, ]$prevalence) / plot_df[prev_mask, ]$prevalence
  plot_df[prev_mask, ]$q3 <- (plot_df[prev_mask, ]$q3 - plot_df[prev_mask, ]$prevalence) / plot_df[prev_mask, ]$prevalence
  plot_df[prev_mask, ]$q4 <- (plot_df[prev_mask, ]$q4 - plot_df[prev_mask, ]$prevalence) / plot_df[prev_mask, ]$prevalence
  plot_df[prev_mask, ]$q5 <- (plot_df[prev_mask, ]$q5 - plot_df[prev_mask, ]$prevalence) / plot_df[prev_mask, ]$prevalence

  facet_labels <- c("prevalence" = "Prevalence", "r_naught" = "Reproduction number")

  prev_fig_1 <- ggplot(data = plot_df) +
    geom_linerange(
      mapping = aes(x = sorted_order, ymin = q1, ymax = q5),
      colour = green_hex_colour
    ) +
    geom_point(
      mapping = aes(x = sorted_order, y = q3),
      colour = green_hex_colour
    ) +
    geom_hline(
      data = data.frame(yi = c(0, true_r_naught), variable = c("prevalence", "r_naught")),
      mapping = aes(yintercept = yi),
      linetype = "dashed"
    ) +
    facet_grid(variable ~ .,
      scale = "free_y",
      labeller = labeller(variable = facet_labels)
    ) +
    labs(x = "Replicate (ordered by prevalence)", y = NULL, colour = NULL) +
    theme_classic() +
    theme(
      axis.text.x = element_blank(),
      strip.background = element_blank(),
      legend.position = "null"
    )
  ## print(prev_fig_1)
  ggsave(filename = "out/prevalence-calibration-extra-2.png", plot = prev_fig_1, height = 14.8, width = 10.5, units = "cm")
}

main <- function(args) {
  make_prevalence_comparison(args)
  make_prev_r_naught_fig(args)
}

if (!interactive()) {
  args <- parser$parse_args()
} else {
  args <- list(
    input = "out/foo-summary.json",
    output = "out/foo-prevalence-figure.png"
  )
}
main(args)
