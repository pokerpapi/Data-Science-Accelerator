#5.5.2 exercise 1
flights <- mutate(flights,
                  dep_time_mins = dep_time %/% 100 * 60 + dep_time %% 100,
                  sched_dep_time_mins = sched_dep_time %/% 100 * 60 +
                    sched_dep_time %% 100)

select(flights, starts_with('dep_time'), starts_with('sched'))
#exercise 2

flights %>% mutate(flight_time = arr_time - dep_time) %>%
  select(air_time, flight_time)

sum(flights$air_time == flights$flight_time, na.rm = TRUE)

#exercise 3

select(flights, dep_time, sched_dep_time, dep_delay)

#exercise 4

flights %>% 
  filter(!is.na(dep_delay), !is.na(arr_delay)) %>%
head(arrange(flights, min_rank(desc(dep_delay))), 10)

#exercise 5

1:3 + 1:10

#exercise 6
?sin

#5.6.7 exercise 2

flights %>% 
  filter(!is.na(dep_delay), !is.na(arr_delay)) %>%
  group_by(dest) %>% summarize(count = n())
  group_by(dest) %>% summarize(count = n())

#exercise 3
flights %>% select(starts_with("dep"), starts_with("arr")) %>%
  sapply(function(x){sum(is.na(x))})

nrow(flights %>% filter(!is.na(dep_time), is.na(arr_time)))

#exercise 4
library(ggplot2)

flights %>% group_by(month, day) %>%
  summarize(avg_dep_delay = mean(dep_delay, na.rm = TRUE),
            prop_cancelled = sum(is.na(dep_time)/n())) %>%
  ggplot(mapping = aes(x = avg_dep_delay, y = prop_cancelled)) +
  geom_point() +
  geom_smooth(method = 'lm', se = FALSE)
#exercise 5

worst <- flights %>% group_by(carrier) %>% 
  summarize(avg_arr_delay = mean(arr_delay, na.rm = TRUE),
            avg_dep_delay = mean(dep_delay, na.rm = TRUE))

arrange(worst, desc(avg_arr_delay))

arrange(worst, desc(avg_dep_delay))

flights %>% group_by(origin) %>%
  summarize(avg_dep_delay = mean(dep_delay, na.rm = TRUE)) 

flights %>% group_by(origin) %>%
  summarize(avg_arr_delay = mean(arr_delay, na.rm = TRUE)) 

flights %>% group_by(carrier, origin) %>%
  summarize(avg_dep_delay = mean(dep_delay, na.rm = TRUE)) %>%
  filter(carrier == '9E')

# carrier 9E on average flights were delayed most at JFK
#exercise 6

flights %>% 
  filter(!is.na(dep_delay), !is.na(arr_delay)) %>%
  count(dest)

flights %>% 
  filter(!is.na(dep_delay), !is.na(arr_delay)) %>%
  count(dest, sort = TRUE)

#5.7.1
# exercise 1 
# Before grouping, functions like mean(), median(), min(), or max() will operation over the whole dataset. For example, applying mean() on a variable before grouping will get the average of the variable over the entire dataset.

#exercise 2

flights %>% group_by(tailnum) %>%
  filter(dep_delay > 0) %>%
  summarize(avg_delay = mean(dep_delay)) %>%
  arrange(desc(avg_delay))

#exercise 3
flights %>% filter (dep_delay > 0) %>% 
  group_by(hour) %>%
  summarize(avg_delay = mean(dep_delay)) %>%
  arrange(avg_delay)

#exercise 4

flights %>% select(dest, arr_delay) %>% group_by(dest) %>%
  filter(arr_delay > 0) %>%
  mutate(total_delay = sum(arr_delay, na.rm = TRUE),
         prop_delay = arr_delay / total_delay)

#exercise 5

flights %>% filter(origin == 'JFK') %>% filter(!is.na(dep_delay)) %>%
  mutate(pre_dep_delay = lag(dep_delay, default = 0)) %>%
  ggplot(mapping = aes(x = dep_delay, y= pre_dep_delay)) +
  geom_point(alpha = .5)

flights <- flights %>% filter(origin == 'JFK') %>% filter(!is.na(dep_delay)) %>%
  mutate(pre_dep_delay = lag(dep_delay, default = 0))

cor(flights$dep_delay, flights$pre_dep_delay)

#exercies 6

flights %>% filter(!is.na(air_time)) %>% group_by(dest) %>%
  mutate(air_time_mean = mean(air_time),
         air_time_sd = sd(air_time),
         z = (air_time - air_time_mean) / air_time_sd) %>%
  select(z, air_time_mean, dest, everything()) %>%
  arrange(z)

flights %>% filter(!is.na(air_time)) %>% group_by(dest) %>%
  mutate(air_time_mean = mean(air_time),
         air_time_sd = sd(air_time),
         z = (air_time - air_time_mean) / air_time_sd) %>%
  select(z, air_time_mean, dest, everything()) %>%
  arrange(desc(z))

#exercise 7

flights %>% group_by(dest) %>%
  summarise(num_carrier = length(unique(carrier))) %>%
  filter(num_carrier >= 2) %>%
  arrange(desc(num_carrier))

flights %>% group_by(carrier) %>%
  summarise(num_dest = length(unique(dest))) %>%
  filter(num_dest >= 2) %>%
  arrange(desc(num_dest))

#exercise 8

flights %>% filter(!is.na(dep_delay)) %>% group_by(tailnum) %>%
  mutate(max_delay = cummax(dep_delay),
         less_one_hour = max_delay < 60) %>%
  summarize(count = sum(less_one_hour)) %>%
  arrange(desc(count))

# 19.3.1
#Read the source code for each of the following three functions, puzzle out what they do, and then brainstorm better names.

f1 <- function(string, prefix) {
  substr(string, 1, nchar(prefix)) == prefix
}

f2 <- function(x) {
  if (length(x) <= 1) return(NULL)
  x[-length(x)]
}

f3 <- function(x, y) {
  rep(y, length.out = length(x))
}
#1 function f1 test if each element of the character vector nchar starts with the string prefix better name would be contains_prefix()
#function f2 drops last element of vector x better name could be last_drop()
#function f3 repeats y once for each element of x better name could be remove_last()
#delay_finder()
#rnorm() samples from univariate (one variable at a time) vs MASS::mvrnorm from multivariate which is more than two variables analyzed together. rnorm() uses n, mean, sd and MASS::mvrnorm uses n, mum Sigma.
# to be consistent they should have the same names
#4 named norm_r() or norm_d(), naming convention groups functions by distribution
# named rnorm() or dnorm() naming convention groups functions by the actions it performs 
# r* functions always sample from distributions: rnorm, rbinom, runif, rexp. d* functions calculate the probability density or mass of a distribution: dnorm, dbinom, dunif, dexp.

#19.4.4 
#1 both used to test conditional statements, if test a single condition while ifelse() test each element.

age <- 19
ifelse(age >=21, 'Alcohol', 'Water')

age  <- 19
legal <- function(x){
  if (age >= 21) {
    return('Alcohol')
  } else {
    return('Water')
  }
}
legal(age)

#2 

greeting <- function(){
  h <- lubridate::hour(lubridate::now())
  if (dplyr::between(h, 12, 18)){
    print("Good Afternoon.")
  } else if(dplyr::between(h, 18, 24)){
    print("Good Evening")
  } else {
    print("Good Morning")
  }
}

greeting()

#3

fizzbuzz <- function(x){
  if (x %% 3 == 0 && x %% 5 == 0){
    print('fizzbuzz')
  } else if (x %% 3 == 0) {
    print('fizz')
  } else if (x %% 5 == 0) {
    print('buzz')
  } else {
    print(x)
  }
}

fizzbuzz(12)

#4


temp <- seq(-10, 50, by = 5)
cut(temp, c(-Inf, 0, 10, 20, 30, Inf), right = TRUE,
    labels = c("freezing", "cold", "cool", "warm", "hot"))

temp <- seq(-10, 50, by = 5)
cut(temp, c(-Inf, 0, 10, 20, 30, Inf), right = FALSE,
    labels = c("freezing", "cold", "cool", "warm", "hot"))

# Cut() works with vectors which is its biggest advantage

#5 in Switch(n,...) if n is numeric switch will return the n'th argument from ... so if n = 1 it will return the 1st argument and so on.
#6 What does this switch() call do? What happens if x is "e"?

x <- 'a'

switch(x, 
       a = ,
       b = "ab",
       c = ,
       d = "cd"
)
# this switch() returns ab if x == to a or b, and returns cd if x == to c or d. Because e isnt a named argument in switch Nothing happens.