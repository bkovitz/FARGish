library(ggplot2)
library(readr)

th = theme(legend.key.width = unit(1, "cm"))
# th = theme(axis.line.x = unit(30, "cm"))

plots <- function() {
  ss <<- read_csv("support.csv")
  ww <<- read_csv("weight.csv")
  tt <<- read_csv("totals.csv")
  p1 <<- ggplot(data=ss, mapping=aes(t, support, colour=id)) + geom_line() + th
  p2 <<- ggplot(data=ww, mapping=aes(t, weight, colour=suppid)) + geom_line() + th
  p3 <<- ggplot(data=tt, mapping=aes(t, y, colour=total)) + geom_line() + th
  egg::ggarrange(p1, p2, p3)
}
