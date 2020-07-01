library(ape)
library(ggplot2)
library(ggtree)

reconstructed_tree <- read.newick("demo-newick-string-recontree.txt")

g <- ggtree(reconstructed_tree)

ggsave("out/reconstructed-tree.pdf", g)
