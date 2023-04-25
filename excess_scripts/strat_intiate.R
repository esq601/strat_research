rel <- data.frame(
a_1 = sample(c('linear','defense_in_depth'),1),

g_1 = sample(c('3_n','3_s','4','5'),1),

str_f = runif(1,.6,.9),

str_e = runif(1,.6,.9),

res_f = runif(1,.1,.2),

res_e =runif(1,.15,.3)
)


for(i in 1:1000){
  rel <- bind_rows(rel,data.frame(
    a_1 = sample(c('linear','defense_in_depth'),1),
    
    g_1 = sample(c('3_n','3_s','4','5'),1),
    
    str_f = runif(1,.6,.9),
    
    str_e = runif(1,.6,.9),
    
    res1 = runif(1,.1,.2),
    
    res2 =runif(1,.15,.3)
  ))
}


start_loc <- function(strat_init){
  if(strat_init == 'linear'){
    start <- c('131508','141507','151506','161606','171706','181705')
  } else {
    start <- c('141507','151506','161505','121206','131205','111207')
  }
  start
}

eny_pos <- c('131811','141810','151809','161808','171807','141911','151910','152011','162010')


start_loc(rel[3,1])
