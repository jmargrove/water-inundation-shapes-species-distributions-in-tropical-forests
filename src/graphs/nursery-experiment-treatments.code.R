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



longData <- melt(m_cylce)
p1 <- ggplot(longData, aes(x = Var2, y = Var1)) + 
  geom_raster(aes(fill=value)) + 
  scale_fill_gradient(low="white", high="black") +
  labs(x="Day", y="Inundation Treatment") 

ggsave(plot = p1, filename = './src/graphs/nursery-experiment-treatments.graph.png', device = "png")
