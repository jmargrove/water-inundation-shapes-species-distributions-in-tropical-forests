# som

library(reshape2)
library(ggplot2)

m <- matrix(rep(0, 8 * 21), nrow = 8)
for(i in 1:7){
 m[i + 1, seq(1, i * 3, by = 1)] <- 1
}

m_cylce <- m
for(i in 1:3){
  m_cylce <- cbind(m_cylce, m)
}

library(timevis)
x <- as.data.frame(m_cylce)
timevis(x)


ggsave(plot = p1, filename = './src/graphs/nursery-experiment-treatments.graph.png', device = "png")
