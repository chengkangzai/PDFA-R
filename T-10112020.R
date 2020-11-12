pacman::p_load("ggplot2","magrittr","dplyr","crayon")
data("economics", package = "ggplot2")



ggplot(data = diamonds, aes(cut)) + geom_bar()
ggplot(data = diamonds, aes(carat)) + geom_histogram(bins = 30)

ggplot(data = diamonds , aes(carat, col = "white")) + geom_freqpoly(bins =
                                                                      20)
ggplot(data = diamonds , aes(carat, col = "Distributeion Line")) + geom_freqpoly(bins =
                                                                                   20) + facet_wrap(-cut)

diamonds %>% ggplot(aes(carat, col="white")) + geom_freqpoly(bins=20) + facet_wrap(~cut)

diamonds %>% ggplot(aes(carat, col="white")) + geom_freqpoly(bins=20) + facet_wrap(~cut)
