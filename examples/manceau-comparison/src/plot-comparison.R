library(ggplot2)
library(cowplot)

llAnkit <- c(
  -40.4552507476100,
  -40.8703238897302,
  -41.3817906577664,
  -41.9806102480506,
  -42.6589107277071,
  -43.4097480561603,
  -44.2269395654317,
  -45.1049441532352,
  -46.0387725346941,
  -47.0239173599859
)

llZar <- c(
    -40.4550918517829
    ,-40.87013575276441
    ,-41.38157282061684
    ,-41.9803636161457
    ,-42.658637698856054
    ,-43.40945251151045
    ,-44.226626725508694
    ,-45.104620318860626
    ,-46.03844474479797
    ,-47.02359300426023
)

lambdaVals <- 1.0 + 1:10 / 10

plot_df <- data.frame(lambda = lambdaVals, ankit = llAnkit, zarebski = llZar)

font_scale_factor <- 0.5
fig_theme <- theme(
    axis.title.x = element_text(size = font_scale_factor * 22),
    axis.ticks = ggplot2::element_blank(),
    axis.line.x.bottom = element_line(colour = "#000000"),
    legend.title = element_text(size = font_scale_factor * 22),
    legend.position = c(0.8,0.87),
    plot.title = element_text(size = font_scale_factor * 32),
    plot.subtitle = element_text(size = font_scale_factor * 22),
    panel.grid.minor = ggplot2::element_blank(),
    panel.grid.major.y = ggplot2::element_line(color="#cbcbcb"),
    panel.grid.major.x = ggplot2::element_blank(),
    panel.background = ggplot2::element_blank(),
    strip.background = ggplot2::element_rect(fill="white"))

llhd_comp_fig <- ggplot(plot_df, aes(x = lambda)) +
    geom_line(aes(y = zarebski)) +
    geom_point(aes(y = ankit), shape = 5, size = 3) +
    labs(x = "Birth Rate",
         y = "Log-likelihood") +
    scale_colour_manual(values = c("#1380A1", "#FAAB18")) +
    scale_y_continuous(position = "right") +
    fig_theme





segment <- function(p0, p1, ...) {
    seg_df <- data.frame(x = p0[1],
                         y = p0[2],
                         xend = p1[1],
                         yend = p1[2])
    geom_segment(data = seg_df,
                 mapping = aes(x = x,
                               y = y,
                               xend = xend,
                               yend = yend),
                 ...)
}

occurrence_x_pos <- 5.5
points_df <- data.frame(x = c(occurrence_x_pos,occurrence_x_pos,3,1,4,5),
                        y = c(1,5,2,0,0,0),
                        colour = c( "Occurrence"
                                 , "Occurrence"
                                 , "Sample"
                                 , "Catastrophe"
                                 , "Catastrophe"
                                 , "Catastrophe"
                                 ))


point_delta_circle <- 0.25
point_delta_triangle <- 0.37

toy_data_fig <- ggplot() +
    segment(c(occurrence_x_pos,7.5),
            c(occurrence_x_pos,-0.5),
            linetype = "dashed") +
    segment(c(1,0+point_delta_circle), c(1, 3)) +
    segment(c(2,3), c(2, 4)) +
    segment(c(3,2+point_delta_triangle), c(3, 3)) +
    segment(c(3,4), c(3, 6)) +
    segment(c(4,0+point_delta_circle), c(4, 4)) +
    segment(c(4,6), c(4, 7)) +
    segment(c(5,0+point_delta_circle), c(5, 6)) +
    segment(c(1,3), c(3,3)) +
    segment(c(2,4), c(4,4)) +
    segment(c(3,6), c(5,6)) +
    geom_point(data = points_df,
               mapping = aes(x = x, y = y, shape = colour),
               size = 6) +
    labs(shape = "Observation type") +
    scale_y_continuous(name = "Time", breaks = 0:7, labels = 7:0) +
    scale_shape_manual(values = c(1,4,2)) +
    theme(axis.line.y = element_line(),
          axis.line.x = element_blank(),
          axis.title.x = element_blank(),
          axis.text.x = element_blank(),
          axis.ticks.x = element_blank(),
          panel.background = element_blank(),
          legend.position = "bottom",
          legend.key = element_blank()
          )

manceau_comp <- plot_grid(toy_data_fig,
                          llhd_comp_fig,
                          rel_widths = c(0.6,1.0),
                          labels = c("A", "B"),
                          scale = 0.9)

## print(manceau_comp)
ggsave("out/manceau-comparison.pdf",
       plot = manceau_comp,
       height = 10.5,
       width = 28.8,
       units = "cm")
