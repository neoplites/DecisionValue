# this is a model for decisions that one has to make and it is known that the utility drops with time
# examples: seeing a doctor due to a symptom, doing maintenance on a car, paying bills on time

library(tidyverse)
library(magrittr)
library(gridExtra)

N = 100
MAX = 5
sigma = 1
ticks = 100
# decisions range from min to max in importance
# total number of decisions is N
importance = runif(N, min=1, max=MAX)

# ticks = number of time periods
data = tibble(t=1:ticks)

# signal is the utility of the decision as a function of time
# signal = a * 1/(t^b)
# noise is normally distributed with params mu=0, sigma
for (i in 1:N) {
    signal = paste0("signal", i) 
    noise  = paste0("noise",  i)
    value  = paste0("value",  i)
    
    data %<>% 
    mutate(!!signal := importance[i]/sqrt(t) # exp is assumed to be 1/2
         , !!noise  := rnorm(n())
         , !!value  := importance[i]/sqrt(t) + rnorm(n())) # TODO: refactor this uglinness
}

sums = data %>% 
       select(starts_with("value")) %>%
       rowSums() %>%
       tibble()

names(sums) = c("sums")
sums %<>% mutate(t=1:n()) 

ggplot(sums, aes(x=t, y=sums)) + 
  geom_line() +
  ggtitle("Aggregated decision values")

g1 = ggplot(data, aes(x=t, y=value1)) + geom_line(color="black")  +
  geom_line(data=data, aes(x=t, y=signal1, color="darkred"))
g2 = ggplot(data, aes(x=t, y=value2)) + geom_line(color="black")  +
  geom_line(data=data, aes(x=t, y=signal2, color="darkred"))

grid.arrange(g1, g2)
