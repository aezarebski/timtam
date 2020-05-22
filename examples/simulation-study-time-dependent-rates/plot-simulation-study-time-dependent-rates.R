library(ggplot2)
library(jsonlite)


INPUT_FILE <- "time-dependent-rates-config.json"
OUTPUT_FILE <- "llhd-profiles.png"

sim_params <- read_json(INPUT_FILE)
sim_b1 <- sim_params$simulationParameters[[1]][[1]][[2]]
sim_b2 <- sim_params$simulationParameters[[1]][[2]][[2]]



x <- read.csv(sim_params$inferenceLlhdFile, header = FALSE)
names(x) <- c("llhd", "first birth rate", "second birth rate")


num_mesh_points <- 50

plot_df <- data.frame(llhd = x$llhd,
                      birth_rate = c(head(x[,2], num_mesh_points), tail(x[,3], num_mesh_points)),
                      param = rep(names(x)[2:3], each = num_mesh_points))

g <- ggplot(mapping = aes(x = birth_rate, y = llhd, colour = param)) +
    geom_line(data = plot_df) +
    geom_vline(data = data.frame(true_birth_rate = c(sim_b1, sim_b2), param = names(x)[2:3]),
               mapping = aes(xintercept = true_birth_rate, colour = param),
               linetype = "dashed") +
    labs(x = "Birth rate",
         y = "LLHD",
         colour = "Parameter")


ggsave(OUTPUT_FILE, plot = g, height = 10.5, width = 14.8, units = "cm")
