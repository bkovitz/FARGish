library(ggplot2)
library(readr)

plots <- function() {
  ss <<- read_csv("support.csv")
  ww <<- read_csv("weight.csv")
  tt <<- read_csv("totals.csv")
  x11()
  ggplot(data=ss, mapping=aes(t, support, colour=id)) + geom_line()
  x11()
  ggplot(data=ww, mapping=aes(t, weight, colour=suppid)) + geom_line()
  x11()
  ggplot(data=tt, mapping=aes(t, y, colour=total)) + geom_line()
}
