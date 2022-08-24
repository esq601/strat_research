source('hex_setup.R')

library(foreach)
library(doParallel)
library(tidyverse)
library(data.table)


cbind(data.table(),"s" = sample(q_df_pos[s == '060202']$a,100,replace = TRUE))


actions <- function(players,adj,search = 1000) {
  dt <- data.table()
 for(p in players){
   dt_temp <- data.table(sample(q_df_pos[s == p]$a,search,replace = TRUE))
   colnames(dt_temp) <- p
   dt<- cbind(dt,dt_temp)
 } 
  unique(dt)
}

actions(players$s,q_df_pos,10000)


# actions <- function(players,adj,search = 1000) {
#   
#   a1 <- adj %>%
#     filter(s %in% players) %>%
#     select(s,a) %>%
#     nest(data = a)
#   
#   df_out <- data.frame()
#   
#   for(i in 1:search){
#     df_iter = data.frame(iter = i)
#     for(j in 1:nrow(a1)){
#       a <- sample(a1$data[[j]]$a,size = 1)
#       s <- a1$s[j]
#       df <- data.frame(a)
#       colnames(df) <- s
#       df_iter <- cbind(df_iter,df)
#     }
#     df_out <- rbind(df_out,df_iter)
#   }
#   
#   df_out %>%
#     select(-iter) %>%
#     distinct()
#}



actions_samp <- function(players,adj,target) {

  
  dt_adj <- adj[s %in% players$s,.SD[sample(.N,1)], by = s]
  
  dt_adj<- dt_adj[s %in% target, c('a','sp','r') := .('adj0',s,0)]
  dt <- players[dt_adj, on = 's']
  dt
  # dt[s %in% target, ]
  # dt[s %in% target, r := 0]
}



reward_new <- function(sa,target){
  
  rew_conf <- 0
  
  if(any(sa$sp %in% target$s)){
    #print(sa[sp %in% target$s])
    #print(target[s %in% sa$sp])
    conf_out <- conflict(sa[sp %in% target$s],target[s %in% sa$sp])
    rew_conf <- reward_conf(conf_out)
    #print(conf_out)
    #print(rew_conf)
    #print(c('heyyyy'),rew_conf)
  }
  
  #group_bonus <- sa[sp %in% target & !(s %in% target), .(.N), by = .(sp)]
  #group_bonus <- group_bonus[,N := 5^N]
  r <- sum(sa$r) + rew_conf#sum(group_bonus$N)
  
  r
}



cores=detectCores()
cl <- makeCluster(cores[1]-1) #not to overload your computer
registerDoParallel(cl)


search_par <- function(state_init,depth=6,sims = 100,disc = 0.99,q_dft,target) {

  a_init <- actions(state_init,q_dft,2000)
  state_base <- colnames(a_init)[!(colnames(a_init) %in% target)]
  
  foreach(k=1:nrow(a_init), .combine = rbind,.packages = c('data.table','tidyverse'),
          .export = c('actions_samp','reward_new')) %dopar% {
            
    rew <- -Inf
    
    aout_init <- as.character(a_init[k,])
    
    sa_dt <- data.table(s = state_base,a = aout_init)
    
    state_init <- q_dft[sa_dt, on = .(s==s,a==a)]
    
    rew_start <- reward_new(state_init,target)
    
    state_init <- state_init$sp
    
    for(i in 1:sims){
 
      df <- data.frame()
      
      state <- state_init
      
      rew_t <- rew_start
      
      for(j in 1:depth){
        
        aout <- actions_samp(state,q_dft,target)
        
        
        rew_t <- rew_t + (disc^j)*reward_new(aout,target)
        state <- aout$sp
      }
      
      if(rew_t > rew) {
        rew <- rew_t
        pol <- sa_dt
      }
    }
    data.table(r= rew,pol,k)
  }
}




search_no_par <- function(state_init,depth=6,sims = 100,disc = 0.99,q_dft,target) {
  
  state_real <- state_init
  a_init <- actions(state_init$s,q_dft,5000)
  state_base <- colnames(a_init)
  rew <- -Inf
  
  for (k in 1:nrow(a_init)) {

    aout_init <- as.character(a_init[k,])
    
    #print(aout_init)
    
    sa_dt <- data.table(s = state_base,a = aout_init)[state_real, on = 's']
    #print(sa_dt)
    state_init <- q_dft[sa_dt, on = .(s==s,a==a)]
    
    
    #print(state_init)
    
    
    rew_start <- reward_new(state_init,target)
    #print(rew_start)
    #state_init <- state_init[,list(id,sp,str)]
    
    for(i in 1:sims){
      
      df <- data.frame()
      
      state <- state_init[,list(s = sp, id, str)]
      
      rew_t <- rew_start
      
      for(j in 1:depth){
        #print("state vvvv")
        #print(state)
        aout <- actions_samp(state,q_dft,target)
        #print(aout)
        rew_t <- rew_t + (disc^j)*reward_new(aout,target)
        #print(rew_t)
        state <- aout$sp
      }
      
      if(rew_t > rew) {
        rew <- rew_t
        pol <- sa_dt
        print(rew)
        print(pol)
      }
    }
  }
  data.table(r= rew,sa_dt)
  
}


# Reward gradient

gradient_function <- function(players,target){
  
  hexdf3 <- hexdf2 %>% 
    #filter(x<=6 & y >= 8) %>%
    mutate(type = case_when(
      pos == target ~ 'target',
      pos %in% players ~ 'player',
      T ~ 'none'
    ))
  print(hexdf3)
  q_df <- adj_df %>%
    filter(s %in% hexdf3$pos | sp %in% hexdf3$pos) %>%
    mutate(r = 0)
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
  data.table(q_df_pos)
}





players <- data.table(
  id = c('inf_1','inf_2','inf_3'),
  s = c('051112','020711','040608'),
  str = c(1,1,1)
)


target <- data.table(
  id = c('inf_a'),
  s = c('050910'),
  str = c(1),
  sp = c('050910')
)

q_df_pos <- gradient_function(players$s,target$s)


ls1 <- search_no_par(players,1,1,0.99,q_df_pos,target)
#ls1 <- search_par(players,5,2,0.99,q_df_pos,target)

#players <- new$sp

for(i in 1:3){
  print(i)
  if(i == 1) {
    df_run <- players
    df_hist <- data.frame(t(players$s))
  } else {
    df_run <- new$sp
  }
  ls1 <- search_no_par(df_run,1,5,.95,q_df_pos,target)
  
  
  
  print(ls1)
  
  move <- ls1[r == max(r)]
  #print(move)
  new <- q_df_pos[move, on = .(s==s,a==a)]
  
  hexdf3 <- hexdf2 %>% #filter(x<=6 & y >= 8) %>%
    mutate(type = case_when(
      pos == target ~ 'target',
      pos %in% new$sp ~ 'player',
      T ~ 'none'
    ))
  #print(data.frame(t(new$sp)))
  df_hist <- rbind(df_hist,data.frame(t(new$sp))) 
}


write_csv(df_hist, "hist_test.csv")


ls1[r == max(r)]
#<-4
saveGIF({
  for(i in 1:nrow(df_hist)){
    
    #print(tile)
    trans <- df_hist[i,]
    
    trans <- t(trans)
  
    plot_df <- hexdf2 %>%
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
  
}, interval = 1, movie.name="test_new2.gif",ani.res = '120',ani.height = 1300, ani.width = 1600)


