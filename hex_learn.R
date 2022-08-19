source('hex_setup.R')

library(foreach)
library(doParallel)
library(tidyverse)
library(data.table)



actions_samp <- function(players,adj) {
  dt <- data.table(adj %>%
    filter(s %in% players) %>%
    group_by(s) %>%
    sample_n(1))
  
  dt[s %in% target, a := 'adj0']
  dt[s %in% target, sp := s]
  dt[s %in% target, r := 0]
}

for(i in 1:length(players)){
  print(players[i])
  if (players[i] %in% target) {
    
  }
}

players <- c('010712','020711','030912')
#unit <- c('unit1','unit2')

target  <- '010712'

aout <- actions_samp(players,q_df_pos)
print(aout)
apply(aout$a,1,FUN = reward,names = aout$s, q = q_df_pos,target = target)




reward_new <- function(sa,target){
  
  group_bonus <- sa[sp %in% target, .(.N), by = .(sp)]
  
  r <- sum(sa$r) + 25*sum(group_bonus$N)
  
  r
}
target <- '020610'
aout <- data.frame(a = c('adj0','adj1','adj1'), sp = c(target,target,target), r = c(10,10,10))
out <- reward_new(data.table(aout),target)
out

data.table(q_df_pos)[s %in% players & a %in% c("adj2","adj1","adj4")]

# fwd_search <- function(s_curr,q_dataframe,depth = 5,disc = 0.95) {
#   if(depth <= 0) {
#     
#     return("0")
#     
#     break
#   }
#   t_df <- q_dataframe %>%
#     filter(s %in% s_curr) 
#   
#   #print(t_df)
#   u_set <- -Inf
#   a_out <- NA
#   fwd_search(s_curr,q_dataframe,depth-1,disc = 0.95)
#   
#   for(a in 1:nrow(t_df)){
#     print(t_df[[a,3]])
#     
#     fwd_search(s_curr,q_dataframe,disc = 0.95)
#     u <- (disc^depth)*t_df[[a,4]]
#     #print(u)
#     
#     if(u > u_set){
#       a_out <- t_df[[a,2]]
#       s_out <- t_df[[a,3]]
#       u_set <- u
#       
#     }
#     
#   }
#  return(list(a_out,s_out))
#}

cores=detectCores()
cl <- makeCluster(cores[1]-1) #not to overload your computer
registerDoParallel(cl)
cores


search_par <- function(state_init,depth=6,sims = 100,disc = 0.99,q_dft,target) {

  a_init <- actions(state_init,q_dft,5000)
  foreach(k=1:nrow(a_init), .combine = rbind,.packages = c('data.table','tidyverse'),
          .export = c('actions_samp','reward_new')) %dopar% {
    rew <- -Inf
    aout_init <- a_init[k,]
    
    state_init <- q_dft %>%
      right_join(aout_init %>%
                   pivot_longer(cols = everything(),
                                names_to = 's',
                                values_to = 'a'),
                 by = c('s','a'))
    rew_start <- reward_new(data.table(state_init),target)
    state_init <- state_init$sp
    
    for(i in 1:sims){
 
      df <- data.frame()
      
      state <- state_init
      
      rew_t <- rew_start
      
      for(j in 1:depth){
        
        aout <- actions_samp(state,q_dft)
        rew_t <- rew_t + (disc^j)*reward_new(aout,target)
        state <- aout$sp
      }
      
      if(rew_t > rew) {
        rew <- rew_t
        pol <- aout_init
        print(rew)
        print(pol)
      }
    }
    data.frame(r= rew,pol)
  }
}


search_no_par <- function(state_init,depth=6,sims = 100,disc = 0.99,q_dft,target) {
  
  
  a_init <- actions(state_init,q_dft,5000)
  
  rew <- -Inf
  
  for (k in 1:nrow(a_init)) {

    aout_init <- a_init[k,]
    
    print(aout_init)
    state_init <- q_dft %>%
      right_join(aout_init %>%
                   pivot_longer(cols = everything(),
                                names_to = 's',
                                values_to = 'a'),
                 by = c('s','a'))
    print(state_init)
    rew_start <- reward_new(data.table(state_init),target)
    
    state_init <- state_init$sp
    
    for(i in 1:sims){
      
      df <- data.frame()
      
      state <- state_init
      
      rew_t <- rew_start
      
      for(j in 1:depth){
        
        aout <- actions_samp(state,q_dft)
        
        rew_t <- rew_t + (disc^j)*reward_new(aout,target)
        
        state <- aout$sp
      }
      
      if(rew_t > rew) {
        rew <- rew_t
        pol <- aout_init
        print(rew)
        print(pol)
      }
    }
  }
  data.frame(r= rew,pol)
  
}
ls1 <- search_no_par(players,1,1,0.99,q_df_pos,target)





# Reward gradient

hexdf3 <- hexdf2 %>% 
  filter(x<=6 & y >= 8) %>%
  mutate(type = case_when(
    pos == target ~ 'target',
    pos %in% players ~ 'player',
    T ~ 'none'
  ))

q_df <- adj_df %>%
  filter(s %in% hexdf3$pos | sp %in% hexdf3$pos) %>%
  mutate(r = case_when(
    sp == target ~ 25,
    T ~ 0
  ))

pos_df <- df2 %>%   
  mutate(pos = paste0(
  ifelse(nchar(x)<2,paste0(0,x),x),
  ifelse(nchar(z)<2,paste0(0,z),z),
  ifelse(nchar(y)<2,paste0(0,y),y)
)) %>%
  ungroup() %>%
  select(pos,x_pos,y_pos)


q_df_pos <- q_df %>%
  left_join(pos_df, by = c("s" = "pos"))

tgts <- hexdf3 %>%
  filter(type == 'target') %>%
  select(pos,x_pos,y_pos) %>%
  distinct()

for(i in 1:nrow(q_df_pos)){
  for(j in 1:nrow(tgts)){
    dist <- sqrt((q_df_pos[[i,5]]-tgts[[j,2]])^2 + (q_df_pos[[i,6]]-tgts[[j,3]])^2)
    
    if(dist > 0){
      q_df_pos[[i,4]] <- q_df_pos[[i,4]] - (dist)/10
    }
    
  }
}


#ls1 <- search_par(players,5,2,0.99,q_df_pos,target)


players <- c("020610","050809","040911")
target <- "010712"

for(i in 1:5){
  print(i)
  if(i == 1) {
    df_run <- players
    df_hist <- data.frame(t(players))
  } else {
    df_run <- new$sp
  }
  ls1 <- search_par(df_run,5,50,.95,q_df_pos,target)
  print(ls1)
  ls2 <- ls1 %>%
    filter(r == max(r)) %>%
    select(-r)
  
  colnames(ls2) <- str_remove(colnames(ls2),"X")
  move <- ls2 %>%
    pivot_longer(cols = everything(),
                 names_to = 's',
                 values_to = 'a')
  #print(move)
  new <- q_df %>%
    right_join(move)
  
  hexdf3 <- hexdf2 %>% filter(x<=6 & y >= 8) %>%
    mutate(type = case_when(
      pos == target ~ 'target',
      pos %in% new$sp ~ 'player',
      T ~ 'none'
    ))
  print(data.frame(t(new$sp)))
  df_hist <- rbind(df_hist,data.frame(t(new$sp))) 
}

write_csv(df_hist, "hist_test.csv")



#<-4
saveGIF({
  for(i in 1:nrow(df_hist)){
    
    #print(tile)
    trans <- df_hist[i,]
    
    trans <- t(trans)
  
    plot_df <- hexdf3 %>%
      mutate(colorfill = case_when(
        pos %in% target ~ 'enemy',
        pos %in% trans ~ 'friendly',
        TRUE ~ 'non'
      )) %>%
      mutate(colorfill = case_when(
        pos %in% target & pos %in% trans ~ 'conflict',
        T ~ colorfill
      ))
    
    
    p1 <- ggplot(plot_df, aes(x = x_pos, y = y_pos)) +
      geom_polygon(color = 'black',aes(group = pos,x=x_h, y = y_h, fill = colorfill)) +
      coord_equal() +
      scale_fill_manual(breaks = c('enemy','friendly','non','conflict'), 
                        values = c('darkred','lightgreen','transparent','orange')) +
      theme_void() +
      labs(title = paste("Turn",i)) +
      geom_text(aes(label = pos),size = 3)
    #tile <- sample(trans$adj,1)
    print(p1)
    
  }
  
}, interval = 1, movie.name="test_new.gif",ani.res = '120',ani.height = 1300, ani.width = 1600)


