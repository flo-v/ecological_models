# sparrow_hawk_great_tit

library(fields)
library(ggplot2)
library(dplyr)

rm(list=ls())

# param definition ----
# duration in days
days <- 180
# time steps per day
# nights are not listed as steps but they take place and have duration of half 
# the time steps the not night hours have (see step_num)
step_num <- 8
# Hawk attacks per time step
a <- NA
# additional false hawk attack rate based on a
a_false <- NA
# prob mortal injury/attack for hawk
i <- 0.0001
# attention tit pays to hawks (strength of anti-predator behavior)
T_attention <- NA
# predators/prey ratio
r <- 0.01

# time steps of hunting/day
#H_hunt <- 12
# max fasting time steps
H_hunger_max <- 24
T_hunger_max <- 18

# hunger states assumed at simulation start
H_hunger_start <- 1
T_hunger_start <- 1

# model hawk ----
# higher T_attention, higher H_fail_eat (hawk fails to eat per time step)
# lower a (hawk attacks per time step), higher H_fail_eat
# base_success is the success a hawk has when a tit pays zero attention
H_fail_eat <- function(T_attention, a){
  base_success <- 0.6
  attention_weight <- 0.75
  res <- base_success - (attention_weight * T_attention)
  if (res < 0){
    res <- 0
  }
  return((1 - res)^a)
}


# # check function output
# atk <- 0:10
# atten <- 0:10/10
# df <- matrix(ncol = length(atk), nrow = length(atten))
# for (co in 1:length(atk)){
#   for (ro in 1:length(atten)){
#     df[ro, co] <- H_fail_eat(atten[ro], atk[co])
#   }
# }

# chance to not die due to starvation for a hawk
H_enough_food <- function(T_attention, a, step_num, days, H_hunger_max, 
                          H_hunger_start){
  steps <- days * step_num
  night <- step_num / 2
  memo <- matrix(nrow = steps, ncol = H_hunger_max)
  memo[steps, ] <- rep(1, H_hunger_max)
  
  eat <- (1 - H_fail_eat(T_attention, a))
  # print(T_attention)
  # print(eat)
  for (s in (steps - 1):1){
    # after these time steps the night begins
    if (s %% night == 0){
      # for those that cannot die this night
      for (hunger in 1:(H_hunger_max - night)){
        memo[s, hunger] <-
          (1 - eat) * memo[s + 1, hunger + night] + eat * memo[s + 1, 1]
      }
      # for those which die this night unless they eat now
      for (hunger in (H_hunger_max - (night - 1)):H_hunger_max){
        memo[s, hunger] <-  eat * memo[s + 1, 1]
      }
    } else {# time step not before night
      # not able to die
      for (hunger in 1:(H_hunger_max - 1)){
        memo[s, hunger] <-
          (1 - eat) * memo[s + 1, hunger + 1] + eat * memo[s + 1, 1]
      }
      # die this night unless they eat now
      memo[s, H_hunger_max] <- eat * memo[s + 1, 1]
    }
  }
  # return(memo)
  return(memo[1, H_hunger_start])
}

# memo <- H_enough_food(0, 4, step_num, days, H_hunger_max, H_hunger_start)
# memo2 <- H_enough_food(0.5, 1, step_num, days, H_hunger_max, H_hunger_start)
# memo3 <- H_enough_food(0.7, 4, step_num, days, H_hunger_max, H_hunger_start)
# memo4 <- H_enough_food(0.75, 4, step_num, days, H_hunger_max, H_hunger_start)


# fitness of hawks over the whole simulation
H_fit <- function(T_attention, a, step_num, days, H_hunger_max, H_hunger_start,
                  i){
  temp <- H_enough_food(T_attention, a, step_num, days, H_hunger_max, H_hunger_start)
  # print(temp)
  temp * exp(-a * step_num * days * i)
}

# H_fit(0, 4, step_num, days, H_hunger_max, H_hunger_start,i)
# H_fit(0.5, 1, step_num, days, H_hunger_max, H_hunger_start,i)
# H_fit(0.7, 5, step_num, days, H_hunger_max, H_hunger_start,i)
# H_fit(0.75, 10, step_num, days, H_hunger_max, H_hunger_start,i)
# H_fit(0.8, 10, step_num, days, H_hunger_max, H_hunger_start,i)

# model tit ----

# probability to not eat per time step
# higher T_attention, higher H_fail_eat (tit fails to eat per time step)
# higher a (hawk attacks per time step), higher H_fail_eat
# base_success is the success a tit has when a tit pays zero attention and no 
# hawk attacks present
T_fail_eat <- function(T_attention, a){
  base_success <- 0.9
  attention_weight <- 0.75
  attack_weight <- 0.1
  res <- base_success - (attention_weight * T_attention) - attack_weight * a
  if (res < 0){
    res <- 0
  }
  return(1 - res)
}


# chance to not die due to starvation for tit
T_enough_food <- function(T_attention, a, step_num, days, T_hunger_max, 
                             T_hunger_start){
  steps <- days * step_num
  night <- step_num / 2
  memo <- matrix(nrow = steps, ncol = T_hunger_max)
  memo[steps, ] <- rep(1, T_hunger_max)
  
  eat <- (1 - T_fail_eat(T_attention, a))
  # print(T_attention)
  # print(eat)
  for (s in (steps - 1):1){
    # after these time steps the night begins
    if (s %% night == 0){
      # for those that cannot die this night
      for (hunger in 1:(T_hunger_max - night)){
        memo[s, hunger] <-
          (1 - eat) * memo[s + 1, hunger + night] + eat * memo[s + 1, 1]
      }
      # for those which die this night unless they eat now
      for (hunger in (T_hunger_max - (night - 1)):T_hunger_max){
        memo[s, hunger] <-  eat * memo[s + 1, 1]
      }
    } else {# time step not before night
      # not able to die
      for (hunger in 1:(T_hunger_max - 1)){
        memo[s, hunger] <-
          (1 - eat) * memo[s + 1, hunger + 1] + eat * memo[s + 1, 1]
      }
      # die this night unless they eat now
      memo[s, T_hunger_max] <- eat * memo[s + 1, 1]
    }
  }
  # return(memo)
  return(memo[1, T_hunger_start])
}


# also not die due to hawk attack, fitness over the whole simulation
T_fit <- function(T_attention, a, a_false, step_num, days, T_hunger_max, T_hunger_start,
                  r){
  temp <- T_enough_food(T_attention, a + a * a_false, step_num, days, T_hunger_max, 
                        T_hunger_start)
  # print(temp)
  temp * exp(-(1 - H_fail_eat(T_attention, a)) * step_num * days * r)
}


# T_fit(0.5, 1, 0, step_num, days, 18, T_hunger_start, r)





# model application ----

# setting the range of a & d params and how fine the grid should be (detail)
div_T_att <- 60
max_T_att <- 1 * div_T_att
div_a <- 4
max_a <- 16 * div_a

# preparing df
res <- data.frame(a = c(sapply(0:max_a/div_a, function(y){
  rep(y, max_T_att + 1)})) , T_attention = rep(0:max_T_att/div_T_att, max_a + 1))
# calculating fitness for different param values
res$hawk <- c(sapply(0:max_a/div_a, function(a){
  sapply(0:max_T_att/div_T_att, function(t_att){
    H_fit(t_att, a, step_num, days, H_hunger_max, H_hunger_start, i)})}))

res$tit_0 <- c(sapply(0:max_a/div_a, function(a){
  sapply(0:max_T_att/div_T_att, function(t_att){
    T_fit(t_att, a, 0, step_num, days, T_hunger_max, T_hunger_start, r)})}))
res$tit_0.5 <- c(sapply(0:max_a/div_a, function(a){
  sapply(0:max_T_att/div_T_att, function(t_att){
    T_fit(t_att, a, 0.5, step_num, days, T_hunger_max, T_hunger_start, r)})}))
res$tit_1 <- c(sapply(0:max_a/div_a, function(a){
  sapply(0:max_T_att/div_T_att, function(t_att){
    T_fit(t_att, a, 1, step_num, days, T_hunger_max, T_hunger_start, r)})}))
res$tit_2 <- c(sapply(0:max_a/div_a, function(a){
  sapply(0:max_T_att/div_T_att, function(t_att){
    T_fit(t_att, a, 2, step_num, days, T_hunger_max, T_hunger_start, r)})}))


# plotting

# hawk
temp <- res %>% group_by(T_attention) %>% mutate_at(vars(-a), funs(max)) 
df_aux <- temp[res$hawk == temp$hawk & res$hawk != 0,]

ggplot(res, aes(x=T_attention, y=a, fill=hawk)) + geom_tile() +
  scale_fill_gradient(low = "blue", high = "yellow") +
  geom_line(data = df_aux, mapping = aes(x=T_attention, y=a), colour = "black")

# tit
# a_false = 0
temp <- res %>% group_by(a) %>% mutate_at(vars(-T_attention), funs(max)) 
df_aux <- temp[res$tit_0 == temp$tit_0 & res$tit_0 != 0,]

ggplot(res, aes(x=a, y=T_attention, fill=tit_0)) + geom_tile() +
  scale_fill_gradient(low = "blue", high = "yellow") +
  geom_line(data = df_aux, mapping = aes(x=a, y=T_attention), colour = "black")

# a_false = 0.5
temp <- res %>% group_by(a) %>% mutate_at(vars(-T_attention), funs(max)) 
df_aux <- temp[res$tit_0.5 == temp$tit_0.5 & res$tit_0.5 != 0,]

ggplot(res, aes(x=a, y=T_attention, fill=tit_0.5)) + geom_tile() +
  scale_fill_gradient(low = "blue", high = "yellow") +
  geom_line(data = df_aux, mapping = aes(x=a, y=T_attention), colour = "black")

# a_false = 1
temp <- res %>% group_by(a) %>% mutate_at(vars(-T_attention), funs(max)) 
df_aux <- temp[res$tit_1 == temp$tit_1 & res$tit_1 != 0,]

ggplot(res, aes(x=a, y=T_attention, fill=tit_1)) + geom_tile() +
  scale_fill_gradient(low = "blue", high = "yellow") +
  geom_line(data = df_aux, mapping = aes(x=a, y=T_attention), colour = "black")

# a_false = 2
temp <- res %>% group_by(a) %>% mutate_at(vars(-T_attention), funs(max)) 
df_aux <- temp[res$tit_2 == temp$tit_2 & res$tit_2 != 0,]

ggplot(res, aes(x=a, y=T_attention, fill=tit_2)) + geom_tile() +
  scale_fill_gradient(low = "blue", high = "yellow") +
  geom_line(data = df_aux, mapping = aes(x=a, y=T_attention), colour = "black")
















