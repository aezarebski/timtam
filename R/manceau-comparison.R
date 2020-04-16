library(ggplot2)

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
  -40.4550918517829,
  -40.87013575276441,
  -41.38157282061684,
  -41.9803636161457,
  -42.658637698856054,
  -43.40945251151045,
  -44.226626725508694,
  -45.104620318860626,
  -46.03844474479797,
  -47.02359300426023
)

lambdaVals <- 1.0 + 1:10 / 10

plot_df <- data.frame(lambda = lambdaVals, ankit = llAnkit, zarebski = llZar)

font_scale_factor <- 0.5
fig_theme <- theme(
    axis.title.x = element_text(size = font_scale_factor * 22),
    axis.ticks = ggplot2::element_blank(),
    axis.line.x.bottom = element_line(colour = "#000000"),
    legend.title = element_text(size = font_scale_factor * 22),
    plot.title = element_text(size = font_scale_factor * 32),
    plot.subtitle = element_text(size = font_scale_factor * 22),
    panel.grid.minor = ggplot2::element_blank(),
    panel.grid.major.y = ggplot2::element_line(color="#cbcbcb"),
    panel.grid.major.x = ggplot2::element_blank(),
    panel.background = ggplot2::element_blank(),
    strip.background = ggplot2::element_rect(fill="white"))

ggplot(plot_df, aes(x = lambda)) +
    geom_line(aes(y = zarebski, colour = "Approximation")) +
    geom_point(aes(y = ankit, colour = "Exact")) +
    labs(x = "Birth Rate",
         y = "Log-likelihood",
         colour = "Likelihood Function",
         title = "LLHD Comparison",
         subtitle = "Exact values and those from the approximation") +
    scale_colour_manual(values = c("#1380A1", "#FAAB18")) +
    fig_theme


ggsave("out/manceau-comparison.pdf", height = 10.5, width = 14.8, units = "cm")
