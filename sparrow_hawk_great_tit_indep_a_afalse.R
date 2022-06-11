# sparrow_hawk_great_tit

library(ggplot2)
library(dplyr)
library(reshape)

rm(list=ls())

# param definition ----

# duration in days
days <- 180
# time steps per day
# nights are not listed as steps but they take place and have duration of half 
# the time steps the not night hours have (see step_num)
step_num <- 8
# Hawk attacks per time step
a <- NA # a handful of attacks per day is reasonable, max 16 for the calculations
max_a <- 16 / step_num
# additional false hawk attack per time step
a_false <- NA
# block of time steps in which false_attacks are carried out, start
block_start <- NA
# block length
block_len <- NA
# prob mortal injury/attack for hawk
i <- 0.0001 #0.00015 / (step_num / 8)
i_false <- i/4 #i / 3
# attention tit pays to hawks (strength of anti-predator behavior)
T_attention <- NA
max_T_attention <- 0.5
# predators/prey ratio
r <- 0.002
# max fasting time steps
H_hunger_max <- step_num * 3
T_hunger_max <- step_num * 2.25
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

# chance to not die due to starvation for a hawk
H_enough_food <- function(T_attention, a, step_num, days, H_hunger_max, 
                          H_hunger_start){
  steps <- days * step_num
  night <- step_num / 2
  memo <- matrix(nrow = steps, ncol = H_hunger_max)
  memo[steps, ] <- rep(1, H_hunger_max)
  
  eat <- (1 - H_fail_eat(T_attention, a))
  
  for (s in (steps - 1):1){
    # after these time steps the night begins
    if (s %% step_num == 0){
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

# fitness of hawks over the whole simulation
H_fit <- function(T_attention, a, a_false, step_num, block_len, days, 
                  H_hunger_max, H_hunger_start, i, i_false){
  temp <- H_enough_food(T_attention, a, step_num, days, H_hunger_max, H_hunger_start)
  # print(temp)
  temp * exp(-days * (a * step_num * i + a_false * block_len * i_false ))
}

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
T_enough_food <- function(T_attention, a, a_false, step_num, block_start, 
                          block_len, days, T_hunger_max, T_hunger_start){
  steps <- days * step_num
  night <- step_num / 2
  block_end <- block_start + block_len - 1
  memo <- matrix(nrow = steps, ncol = T_hunger_max)
  memo[steps, ] <- rep(1, T_hunger_max)
  
  eat_block <- (1 - T_fail_eat(T_attention, a + a_false))
  eat_no_block <- (1 - T_fail_eat(T_attention, a))
  
  for (s in (steps - 1):1){
    # check if time step part of false attack block(period)
    if ((s %% step_num) >= block_start & 
        (s %% step_num) <= block_end){
      eat <- eat_block
    }else{
      eat <- eat_no_block
    }
    # after these time steps the night begins
    if (s %% step_num == 0){
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
T_fit <- function(T_attention, a, a_false, step_num, block_start, 
                  block_len, days, T_hunger_max, T_hunger_start, r){
  temp <- T_enough_food(T_attention, a, a_false, step_num, block_start, 
                        block_len, days, T_hunger_max, T_hunger_start)
  # print(temp)
  temp * exp(-(1 - H_fail_eat(T_attention, a)) * step_num * days * r)
}


# model application ----

# calculates the Nash equilibrium (where hawk and tit strategy line cross)
# for different parameter values
equilibr <- function(step_num, block_start, block_len, days,
                     max_T_attention, max_a, a_false,
                     H_hunger_max, H_hunger_start, i, i_false,
                     T_hunger_max, T_hunger_start, r){
  
  div_T_att <- round(40 / max_T_attention)
  temp_T_att <- max_T_attention * div_T_att
  div_a <- round(40 / max_a)
  temp_a <- max_a * div_a
  # preparing df
  df <- data.frame(a = c(sapply(0:temp_a/div_a, function(y){
    rep(y, temp_T_att + 1)})) , T_attention = rep(0:temp_T_att/div_T_att, temp_a + 1))
  
  df$hawk <- c(sapply(0:temp_a/div_a, function(a){
    sapply(0:temp_T_att/div_T_att, function(t_att){
      H_fit(t_att, a, a_false, step_num, block_len, days, H_hunger_max, 
            H_hunger_start, i, i_false)})}))
  df$tit <- c(sapply(0:temp_a/div_a, function(a){
    sapply(0:temp_T_att/div_T_att, function(t_att){
      T_fit(t_att, a, a_false, step_num, block_start, block_len, days, 
            T_hunger_max, T_hunger_start, r)})}))
  
  temp <- df %>% group_by(T_attention) %>% mutate_at(vars(-a), funs(max))
  df_hawk <- temp[df$hawk == temp$hawk & df$hawk != 0, ]
  temp <- df %>% group_by(a) %>% mutate_at(vars(-T_attention), funs(max)) 
  df_tit <- temp[df$tit == temp$tit & df$tit != 0, ]
  
  point_num <- 0
  resul <- c(0, 0, 0, 0)
  for (index1 in 1:dim(df_hawk)[1]){
    temp_vec <- df_tit$T_attention[df_tit$a == df_hawk$a[index1]]
    for (index2 in 1:length(temp_vec)){
      if (temp_vec[index2]== df_hawk$T_attention[index1]){
        point_num <- point_num + 1
        resul <- c(df_hawk$a[index1],
                   df_hawk$T_attention[index1], 
                   df_hawk$hawk[index1],
                   df_tit$tit[df_tit$a == df_hawk$a[index1]][index2])
      }
    }
  }
  if (point_num != 1){
    print("Warning: Equilibrium number != 1. It is:")
    print(point_num)
  }
  return(resul)
}


equilibr2 <- function(step_num, block_start, block_len, days,
                      max_T_att, max_a, a_false,
                      H_hunger_max, H_hunger_start, i, i_false,
                      T_hunger_max, T_hunger_start, r){
  
  div_T_att <- 10 * 8
  temp_T_att <- max_T_att * div_T_att
  div_a <- 14
  temp_a <- max_a * div_a
  # preparing df
  df <- data.frame(a = c(sapply(0:temp_a/div_a, function(y){
    rep(y, temp_T_att + 1)})) , T_attention = rep(0:temp_T_att/div_T_att, temp_a + 1))
  
  df$hawk <- c(sapply(0:temp_a/div_a, function(a){
    sapply(0:temp_T_att/div_T_att, function(t_att){
      H_fit(t_att, a, a_false, step_num, block_len, days, H_hunger_max, 
            H_hunger_start, i, i_false)})}))
  print(df$hawk)
  df$tit <- c(sapply(0:temp_a/div_a, function(a){
    sapply(0:temp_T_att/div_T_att, function(t_att){
      T_fit(t_att, a, a_false, step_num, block_start, block_len, days, 
            T_hunger_max, T_hunger_start, r)})}))
  print(df$tit)
  temp <- df %>% group_by(T_attention) %>% mutate_at(vars(-a), funs(max))
  df_hawk <- temp[df$hawk == temp$hawk & df$hawk != 0, ]
  temp <- df %>% group_by(a) %>% mutate_at(vars(-T_attention), funs(max)) 
  df_tit <- temp[df$tit == temp$tit & df$tit != 0, ]
  
  if (dim(df_hawk)[1] != dim(df_tit)[1]){
    print(dim(df_hawk)[1])
    print(dim(df_tit)[1])
    return(print("Dimensions are not same"))
  }
  point_num <- 0
  for (i in 1:dim(df_hawk)[1]){
    if (df_tit$T_attention[df_tit$a == df_hawk$a[i]] == df_hawk$T_attention[i]){
      point_num <- point_num + 1
      resul <- c(df_hawk$a[i], df_hawk$T_attention[i], df_hawk$hawk[i],
                 df_tit$tit[df_tit$a == df_hawk$a[i]], point_num)
    }
  }
  return(resul)
}

# 
# 
# # Nash eq for different param values
# arr <- array(numeric(), dim = c(8,6,4))
# block_start <- 1:8
# a_false <- c(0,1,2,4,6,8)
# for (ro in 1:length(block_start)){
#   print(ro)
#   for (co in 1:length(a_false)){
#     arr[ro,co,] <- equilibr(step_num, block_start[ro], 1, days, max_T_attention, 
#                             max_a, a_false[co], H_hunger_max, H_hunger_start, i, i_false, 
#                             T_hunger_max, T_hunger_start, r)
#   }
# }
# saveRDS(arr, "arr_i00015")
# df_H_fitness <- data.frame(arr[, , 3])
# colnames(df_H_fitness) <- a_false
# df_H_fitness <- melt(df_H_fitness, variable.name = "a_false", value.name = "H_fitness")
# df_H_fitness <- cbind(rep(block_len, length(a_false)), df_H_fitness)
# colnames(df_H_fitness) <- c("block_len", "a_false", "H_fitness")
# write.csv(df_H_fitness, "df_H_fitness.csv")
# 
# 
# 
# # Nash eq for different param values
# arr <- array(numeric(), dim = c(8,10,4))
# # block_len <- 1:8
# a_false <- 0:9
# for (ro in 1:length(block_len)){
#   for (co in 1:length(a_false)){
#     arr[ro,co,] <- equilibr(step_num, 1, block_len[ro], days, max_T_attention, max_a, 
#                           a_false[co], H_hunger_max, H_hunger_start, i, i_false, 
#                           T_hunger_max, T_hunger_start, r)
#   }
# }
# saveRDS(arr, "arr_i0001_5__blolen1_8__afal_0_9")
# arr2 <- readRDS("arr_i0001_5__blolen1_8__afal_0_9")
# 
# df_a <- data.frame(arr[, , 1])
# write.csv(df_a, "df_a.csv")
# 
# df_T_attention <- data.frame(arr[, , 2])
# write.csv(df_T_attention, "df_T_attention.csv")
# 
# df_H_fitness <- data.frame(arr[, , 3])
# colnames(df_H_fitness) <- a_false
# df_H_fitness <- melt(df_H_fitness, variable.name = "a_false", value.name = "H_fitness")
# df_H_fitness <- cbind(rep(block_len, length(a_false)), df_H_fitness)
# colnames(df_H_fitness) <- c("block_len", "a_false", "H_fitness")
# write.csv(df_H_fitness, "df_H_fitness.csv")
# df_H_fitness <- read.csv("~/R/ecological_models/df_H_fitness.csv")
# 
# df_T_fitness <- data.frame(arr[, , 4])
# colnames(df_T_fitness) <- a_false
# df_T_fitness <- melt(df_T_fitness, variable.name = "a_false", value.name = "T_fitness")
# df_T_fitness <- cbind(rep(block_len, length(a_false)), df_T_fitness)
# colnames(df_T_fitness) <- c("block_len", "a_false", "T_fitness")
# write.csv(df_T_fitness, "df_T_fitness.csv")
# df_T_fitness <- read.csv("~/R/ecological_models/df_T_fitness.csv")
# df_T <- df_T_fitness[df_T_fitness$a_false != 0, ]
# 
# 
# 
# ggplot(df_H_fitness, aes(x=a_false, y=block_len)) + geom_point(size=0.1) +
#   geom_point(data = df_H_fitness[order(-df_H_fitness$H_fitness), ][1:15, ], 
#              aes(x=a_false, y=block_len), color="yellow", size =5) +
#   geom_point(data = df_T[order(-df_T$T), ][1:15, ],
#              aes(x=a_false, y=block_len), color="blue", size =3)
#   
# # df_H_fitness$mul <- as.numeric(df_H_fitness$block_len) * 
# #   (as.integer(df_H_fitness$a_false) - 1)
# # 
# # df_T_fitness$su <- df_T_fitness$T_fitness + df_H_fitness$H_fitness
# 
# ggplot(df_H_fitness[order(-df_H_fitness$H_fitness), ][1:40, ], aes(x=a_false, y=block_len, fill=H_fitness)) + geom_tile() +
#   scale_fill_gradient(low = "blue", high = "yellow")
# 
# df_T <- df_T_fitness[df_T_fitness$a_false != 0, ]
# ggplot(df_T[order(-df_T$T_fitness), ][1:40, ], aes(x=a_false, y=block_len, fill=T_fitness)) + geom_tile() +
#   scale_fill_gradient(low = "blue", high = "yellow")
# 
# 
# 
# # ggplot(df_H_fitness, aes(x=a_false, y=block_len)) + geom_point(size=0.1) +
# #   geom_point(data = df_H_fitness, 
# #              aes(x=a_false, y=block_len, size = H_fitness), color="yellow") +
# #   geom_point(data = df_T_fitness,
# #              aes(x=a_false, y=block_len, size = 2 * T_fitness), color="blue")
# 
# 
# 
# # Nash eq for different param values
# c(0,1,6,10,12,16)
# max_block_start <- 8
# eq <- c(numeric(4))
# for (block_start in 1:max_block_start){
#   print(block_start)
#   resul <- equilibr(step_num, block_start, 1, days, max_T_attention, max_a, 5, 
#                     H_hunger_max, H_hunger_start, i, i_false, T_hunger_max, 
#                     T_hunger_start, r)
#   eq <- rbind(eq, resul)
# }
# eq <- data.frame(eq)[-1, ]
# colnames(eq) <- c("a", "T_attention", "H_fitness", "T_fitness")
# rownames(eq) <- (eq$block_start)
# 



