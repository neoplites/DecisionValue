library(dplyr)
library(ggplot2)
library(magrittr)

data = tibble(t=1:100) %>% mutate(signal = 10/sqrt(t)
                                , noise = rnorm(n())
                                , value = signal + noise)
ggplot(data, aes(x=t, y=value)) + geom_line(color="black")  +
geom_line(data=data, aes(x=t, y=signal, color="darkred"))
