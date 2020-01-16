# this is a model for decisions that one has to make urgently. The assumption is the utility of the decision
# drops with time
# examples: seeing a doctor due to a symptom, doing maintenance on a car, starting a business, paying bills on time

# the decision function is: signal + noise where `signal = a * 1/(t^b)` and `t` is time and `a` and `b` are constants
# we are constantly making decisions, so each individual makes N decisions and the total utility as a func of time 
# is an aggregate of all signal + noise values for each time tick

library(tidyverse)
library(magrittr)
library(gridExtra)

# TODO: parametrize everything, 
# do not assume mu=0, same sigma for noise in all decisions, MIN importance, exponent of the signal etc.

N = 100 # total number of decisions
MAX = 5 # max importance
sigma = 1 # noise around each decision is normally distributed with params mu=0, sigma
ticks = 100 # number of time periods we are considering (the length of the decision time series)

# decisions range from min to max in importance
importance = runif(N, min=1, max=MAX)

data = tibble(t=1:ticks)

# signal is the utility of the decision as a function of time
# signal = a * 1/(t^b)
# noise ~ N(mu, sigma)

# the current approach is to add a set of signal, noise and value columns per decision
# this leads to a column explosion
# TODO: refactor to use 3D structures
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


# debug: plot value1 and value2
g1 = ggplot(data, aes(x=t, y=value1)) + geom_line(color="black")  +
  geom_line(data=data, aes(x=t, y=signal1, color="darkred"))
g2 = ggplot(data, aes(x=t, y=value2)) + geom_line(color="black")  +
  geom_line(data=data, aes(x=t, y=signal2, color="darkred"))

grid.arrange(g1, g2)
