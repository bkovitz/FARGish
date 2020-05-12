library(ggplot2)
library(readr)
library(reshape2)

nodes <- c(21:36)  # ZTag nodes
#nodes <- c(1:4)

support <- read_csv("support.csv", col_names = c('t', 'node', 's'))
df <- melt(subset(support, node %in% nodes), id=c('t','node'))
ggplot(data=df, aes(x=t,y=value,group=node,colour=node))+geom_line()
