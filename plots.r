library(ggplot2)
library(readr)

plots <- function() {
  s <<- read_csv("support.csv")
  w <<- read_csv("weight.csv")
  t <<- read_csv("totals.csv")
  ggplot(data=s, mapping=aes(t, support, colour=id)) + geom_line()
  X11()
  ggplot(data=w, mapping=aes(t, weight, colour=suppid)) + geom_line()
  X11()
  ggplot(data=t, mapping=aes(t, y, colour=total)) + geom_line()
}
