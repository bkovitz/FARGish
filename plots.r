library(ggplot2)
library(readr)

th = theme(legend.key.width = unit(1, "cm"),
           legend.margin=margin(2,2,2,2),
           legend.key.height=unit(3, "mm"),
           legend.text=element_text(size=6),
           legend.spacing.y = unit(1, "mm"))
# th = theme(axis.line.x = unit(30, "cm"))

plots <- function() {
  ss <<- read.csv("support.csv", encoding="UTF-8")
  ww <<- read.csv("weight.csv", encoding="UTF-8")
  tt <<- read.csv("totals.csv", encoding="UTF-8")
  p1 <<- ggplot(data=ss, mapping=aes(t, support, colour=id)) + geom_line() + th
  p2 <<- ggplot(data=ww, mapping=aes(t, weight, colour=suppid)) + geom_line() + th
  p3 <<- ggplot(data=tt, mapping=aes(t, y, colour=total)) + geom_line() + th
  egg::ggarrange(p1, p2, p3)
}
