rm(list=ls())
library(ggplot2)
library(dplyr)

# param definition ----
# duration
T <- 180
# hours of hunting/day
q <- 12
# max fasting periods
fk <- 12
fs <- 6
# max prey capture prob
cmax <- 0.9
# distance where capture prob = cmax/2
d_chalf <- 10

# will be varied later on
a <- 0.1
# will be varied later on
d <- 15
# hunger starting state
x <- 0


# prob mortal injury/attack
i <- 0.001
# predators/prey
r <- 0.05

# max food encounter/hour
fmax <- 0.3
# distance where food encounter/hour = fmax/2
d_fhalf <- 10
# hours foraging lost for sea lions /attack
h <- 2


# model orca ----

# prob/attack
capture <- function(d, cmax, d_chalf){
  cmax * (1 - 0.5**(d / d_chalf))
}

# prob/day to not catch prey
fail_orca <- function(d, cmax, d_chalf, a, q){
  exp(-capture(d, cmax, d_chalf) * a * q) 
}

## different calculation methods ----

### forward recursion ----
# very inefficient
enough_food_fw_rec <- function(d, cmax, d_chalf, a, q, x, t, fk, T){
  if(t < T){
    if (x + 1 <= fk){
      fail_orca(d, cmax, d_chalf, a, q) * 
        enough_food_fw_rec(d, cmax, d_chalf, a, q, x + 1, t + 1, fk, T) +
        ((1 - fail_orca(d, cmax, d_chalf, a, q)) * 
           enough_food_fw_rec(d, cmax, d_chalf, a, q, 0, t + 1, fk, T))
    } else {(1 - fail_orca(d, cmax, d_chalf, a, q)) * 
        enough_food_fw_rec(d, cmax, d_chalf, a, q, 0, t + 1, fk, T)}
  } else{1}
}

### dynamic programming ----

# with dp and recursion, global structure needed that can be accessed/changed 
# from recursive calls to evade redundant calculation
enough_food_dp_rec <- function(d, cmax, d_chalf, a, q, x, t, fk, T){
  memo <- matrix(nrow=T, ncol=fk+1)
  memo[T,] <- rep(1, fk+1)
  rec <- function(d, cmax, d_chalf, a, q, x, t, fk, T){
    if (is.na(memo[t,x+1]) == F){
      return(memo[t,x+1])
    }
    else if (x + 1 > fk){
      memo[t,x+1] <<-
        (1 - fail_orca(d, cmax, d_chalf, a, q)) * 
        rec(d, cmax, d_chalf, a, q, 0, t + 1, fk, T)
      return(memo[t,x+1])
    }
    else{
      memo[t,x+1] <<-
        fail_orca(d, cmax, d_chalf, a, q) * 
        rec(d, cmax, d_chalf, a, q, x + 1, t + 1, fk, T) +
        (1 - fail_orca(d, cmax, d_chalf, a, q)) * 
        rec(d, cmax, d_chalf, a, q, 0, t + 1, fk, T)
      return(memo[t,x+1])
    }
  }
  rec(d, cmax, d_chalf, a, q, x, t, fk, T)
}

# with dp and normal loop
# (if needed) trim triangle of 1s and not needed ones at top to make more efficient
enough_food_dp_iter <- function(d, cmax, d_chalf, a, q, x, t, fk, T){
  memo <- matrix(nrow = T, ncol = fk + 1)
  memo[T, ] <- rep(1, fk + 1)
  
  for (tt in (T - 1):1){
    for (xx in 1:fk){
        memo[tt, xx] <- fail_orca(d, cmax, d_chalf, a, q) * memo[tt + 1, xx + 1] +
          (1 - fail_orca(d, cmax, d_chalf, a, q)) * memo[tt + 1, 1]
    }
    memo[tt, fk + 1] <- (1 - fail_orca(d, cmax, d_chalf, a, q)) * memo[tt + 1, 1]
  }
  return(memo[1, x + 1])
}


### test all 3 functions ----
enough_food_dp_iter(d, cmax, d_chalf, a, q, x, 1, 2, 10)
enough_food_dp_rec(d, cmax, d_chalf, a, q, x, 1, 2, 10)
enough_food_fw_rec(d, cmax, d_chalf, a, q, x, 1, 2, 10)

# check how time efficient they are
start_rec <- Sys.time()
enough_food_dp_rec(d, cmax, d_chalf, a, q, x, 1, fk, T)
end_rec <- Sys.time()

start_iter <- Sys.time()
enough_food_dp_iter(d, cmax, d_chalf, a, q, x, 1, fk, T)
end_iter <- Sys.time()

end_rec - start_rec
end_iter - start_iter

### final orca model ----
# decision for iterative approach

fit_orca <- function(d, cmax, d_chalf, a, q, x, t, fk, T, i){
  enough_food_dp_iter(d, cmax, d_chalf, a, q, x, t, fk, T) * exp(-a * i * q * T)
}

fit_orca(d, cmax, d_chalf, a, q, x, t, fk, T, i)



# model sea lion ----

# food encounter/hour
food <- function(d, fmax, d_fhalf){
  fmax * (d^2 / (d^2 + d_fhalf^2))
}

# prob/day to not catch food
fail_sea <- function(d, fmax, d_fhalf, a, q){
  exp(-food(d, fmax, d_fhalf) * (q  - a * q * h))
}

# chance to not die due to starvation for sea lion
enough_food_dp_iter_sea <- function(d, fmax, d_fhalf, a, q, x, t, fs, T){
  memo <- matrix(nrow = T, ncol = fs + 1)
  memo[T, ] <- rep(1, fs + 1)
  
  for (tt in (T - 1):1){
    for (xx in 1:fs){
      memo[tt, xx] <- fail_sea(d, fmax, d_fhalf, a, q) * memo[tt + 1, xx + 1] +
        (1 - fail_sea(d, fmax, d_fhalf, a, q)) * memo[tt + 1, 1]
    }
    memo[tt, fs + 1] <- (1 - fail_sea(d, fmax, d_fhalf, a, q)) * memo[tt + 1, 1]
  }
  return(memo[1, x + 1])
}

fit_sea <- function(d, fmax, d_fhalf, a, q, x, t, fs, T, cmax, d_chalf, r){
  enough_food_dp_iter_sea(d, fmax, d_fhalf, a, q, x, t, fs, T) * 
    exp(-capture(d, cmax, d_chalf) *a * r * q * T)
}

fit_sea(d, fmax, d_fhalf, a, q, x, t, fs, T, cmax, d_chalf, r)


# model appliaction ----
detail <- 2
maxd <- 35 * detail
maxa <- 25 * detail
divd <- detail
diva <- 100 * detail

res <- data.frame(d = c(sapply(0:maxd/divd, function(y){
  rep(y, 25 * detail + 1)})) , a = rep(0:maxa/diva, 35 * detail + 1))
res$orca <- c(sapply(0:maxd/divd, function(d){
  sapply(0:maxa/diva, function(a){
    fit_orca(d, cmax, d_chalf, a, q, x, t, fk, T, i)})}))
res$sealion <- c(sapply(0:maxd/divd, function(d){
  sapply(0:maxa/diva, function(a){
    fit_sea(d, fmax, d_fhalf, a, q, x, t, fs, T, cmax, d_chalf, r)})}))

# lapply(df1[,-c(1:2)], function(a) ave(a, df1$SiteID, FUN = max))
# temp <- res
# temp$orca <- ave(res$orca, as.factor(res$d), max)
# temp <- res %>% group_by(d) %>% mutate_at(.vars = vars(-"a"), .funs = max)
# mutate_at(.vars = vars("a"),.funs = list(~. * b))

temp <- res %>% group_by(d) %>% mutate_at(vars(-a), funs(max)) 
df_aux <- temp[res$orca == temp$orca & res$orca != 0, 1:4]

ggplot(res, aes(x=d, y=a, fill=orca)) + geom_tile() +
  scale_fill_gradient(low = "blue", high = "yellow") +
  geom_line(data = df_aux, mapping = aes(x=d, y=a), colour = "black")


temp <- res %>% group_by(a) %>% mutate_at(vars(-d), funs(max)) 
df_aux <- temp[res$sealion == temp$sealion & res$sealion != 0, 1:4]

ggplot(res, aes(x=a, y=d, fill=sealion)) + geom_tile()+
  scale_fill_gradient(low = "blue", high = "yellow")  +
  geom_line(data = df_aux, mapping = aes(x=a, y=d), colour = "black")






