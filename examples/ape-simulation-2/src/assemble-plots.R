library(ggplot2)
library(cowplot)

assemble_data_plot <- function() {
  raw_data_figs <- readRDS("out/ape-sim-figures-1.rds")
  agg_data_fig <- readRDS("out/ape-sim-figures-2.rds")
  comb_fig <- plot_grid(
    raw_data_figs[[2]] + theme(legend.position = c(0.1, 0.9)),
    plot_grid(
      raw_data_figs[[1]] + scale_y_continuous(position = "right") + theme(axis.text.x = element_text(angle = -30)),
      agg_data_fig[[1]] + scale_y_sqrt(position = "right") + theme(legend.position = c(0.3, 0.7)),
      ncol = 1,
      labels = c("B", "C")
    ),
    ncol = 2,
    rel_widths = c(0.6, 0.4),
    labels = c("A", NULL)
  )
  print(comb_fig)
  ggsave(file = "out/ape-sim-figures-combined.png",
         plot = comb_fig,
         height = 14.8,
         width = 21.0,
         dpi = 300,
         units = "cm")
}

assemble_marginals_plot <- function() {
  marginals_unscheduled <- readRDS("out/unscheduled-data/marginal-distributions-2.rds")
  marginals_aggregated <- readRDS("out/aggregated-data/marginal-distributions-2.rds")
  comb_fig <- plot_grid(marginals_unscheduled + theme(plot.margin = unit(c(0.5, 0, 0, 0.5), "cm")),
                        marginals_aggregated + theme(plot.margin = unit(c(0.5, 0, 0, 0.5), "cm")),
                        ncol = 2,
                        labels = c("A", "B"),
                        hjust = -0.2)
  ggsave(file = "out/marginal-distributions-combined.pdf",
         plot = comb_fig,
         height = 21.0,
         width = 14.8,
         dpi = 300,
         units = "cm")
}

main <- function(args) {
  assemble_data_plot()
  assemble_marginals_plot()
}

if (!interactive()) {
  main()
}

