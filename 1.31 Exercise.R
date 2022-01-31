install.packages("tidyverse")

?diamonds

library(ggplot2)
library(magrittr)
library(dplyr)

ggplot(diamonds) + geom_histogram(mapping = aes(x = y),
                 binwidth = 0.5) + coord_cartesian(ylim = c(0,50))

ggplot(diamonds) +geom_histogram(mapping = aes(x = y),binwidth = 0.5)

summary(select(diamonds, x, y, z))

diamonds %>% count(cut_width(carat, 0.5))

smaller <- diamonds %>% filter(carat < 3) 
ggplot(data =smaller, mapping = aes(x = carat)) +geom_histogram(binwidth = 0.1)

ggplot(data = smaller, mapping = aes(x =carat, color = cut)) + geom_freqpoly(binwidth= 0.1)

#Missing Values
diamonds2 <- diamonds %>% filter(between(y, 3, 20))
diamonds2 <- diamonds %>% mutate(y = ifelse(y < 3 | y > 20, NA, y))

ggplot(data = diamonds2, mapping = aes(x = x, y = y)) + geom_point()
ggplot(data = diamonds2, mapping = aes(x = x, y = y)) + geom_point(na.rm = TRUE)
