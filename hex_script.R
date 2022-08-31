list.of.packages <- c("tidyverse", "foreach","doParallel","data.table","gganimate","ggimage")
new.packages <- list.of.packages[!(list.of.packages %in% installed.packages()[,"Package"])]
if(length(new.packages)) install.packages(new.packages)

source('hex_setup.R')
source('hex_conflict.R')

library(foreach)
library(doParallel)
#library(tidyverse)
library(dplyr)
library(data.table)
#library(gganimate)
#library(ggimage)

cores=detectCores()
cl <- makeCluster(cores[1]) #not to overload your computer
registerDoParallel(cl)

actions <- function(players,adj,search = 2000) {
  dt <- data.table()
  for(p in players){
    dt_temp <- data.table(sample(q_df_pos[s == p]$a,search,replace = TRUE))
    colnames(dt_temp) <- p
    dt<- cbind(dt,dt_temp)
  } 
  unique(dt)
}
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
search_par <- function(state_init,depth=6,sims = 100,disc = 0.99,q_dft,target) {
  
  state_real <- state_init
  a_init <- actions(state_init$s,q_dft,10000)
  state_base <- colnames(a_init)
  
  foreach(k=1:nrow(a_init), .combine = rbind,.packages = c('data.table','tidyverse'),
          .export = c('actions_samp','reward_new')) %dopar% {
            
            rew <- -Inf
            aout_init <- as.character(a_init[k,])
            
            sa_dt <- data.table(s = state_base,a = aout_init)[state_real, on = 's']
            
            state_init <- q_dft[sa_dt, on = .(s==s,a==a)]
            
            if(any(!(state_init[,.N, by = sp][N>1,]$sp %in% target$s)==TRUE)){
              print('skip')
              next
            }
            
            if(any(state_init[s %in% sp,]$sp %in% target$s)==TRUE){
              print("skip follower")
              next
            }
            
            rew_start <- reward_new(state_init,target)
            
            for(i in 1:sims){
              
              df <- data.frame()
              q_dft_sim <- q_dft
              state <- state_init[,list(s = sp, id, str)]
              
              rew_t <- rew_start
              target_sim <- target
              
              for(j in 1:depth){
                
                
                aout <- actions_samp(state,q_dft_sim,target_sim)
                
                rew_t <- rew_t + (disc^j)*reward_new(aout,target_sim)
                #print(reward_new(aout,target))
                if(any(aout$sp %in% target_sim$s)){
                  
                  conf_out <- conflict(aout[sp %in% target_sim$s],target_sim[s %in% aout$sp])
                  
                  aout[sp %in% target_sim$s]$str <- conf_out[[1]]$str
                  target_sim[s %in% aout$sp]$str <- conf_out[[2]]$str
                  #print(conf_out)
                  
                  if(nrow(target_sim[str == 0])>0) {
                    
                    if(nrow(target_sim[str>0]) == 0 ){
                      q_dft_sim$r <- 0
                    } 
                  }
                  
                  aout <- aout[str>0]
                  aout[sp %in% target$s]$sp <- aout[sp %in% target$s]$s
                  
                  target_sim <- target_sim[str>0]
                }
                #print(rew_t)
                state$s <- aout$sp
              }
              
              if(rew_t > rew) {
                rew <- rew_t
                pol <- unique(sa_dt)
                print(rew)
                print(pol)
              }
            }
            
            data.table(r= rew,pol,k)
          }
}
gradient_function <- function(players,target){
  
  hexdf3 <- hexdf2 %>% 
    #filter(x<=6 & y >= 8) %>%
    mutate(type = case_when(
      pos %in% target ~ 'target',
      pos %in% players ~ 'player',
      T ~ 'none'
    ))
  #print(hexdf3)
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
transition_function <- function(players,target,q_df){
  
  players_move <- q_df[players, on = .(s==s,a==a), list(id,s,sp,str)]
  
  
  if(any(players_move$sp %in% target$s)){
    
    conf_out <- conflict(players_move[sp %in% target$s],
                         target[s %in% players_move$sp])
    
    players_move[sp %in% target$s]$str <- conf_out[[1]]$str
    target[s %in% players_move$sp]$str <- conf_out[[2]]$str
    #print(conf_out)
    
    if(nrow(target[str == 0])>0) {
      if(nrow(target[str>0]) == 0 ){
        q_df$r <- 0
      } else {
        print('transition grad')
        print(players_move[str>0])
        print(target[str>0])
        q_df <- gradient_function(players_move[str>0]$s,target[str>0]$s)
      }
      
    }
    
    players_move[sp %in% target$s]$sp <- players_move[sp %in% target$s]$s
    players_move <- players_move[str>0]
    target <- target[str>0]
    
    
  }
  list(players_move,target,q_df)
}


players <- data.table(
  id = c('inf_1','inf_2','inf_3','inf_4'),
  s = c('030710','030609','040507','060404'),
  str = 1,
  type = 'f'
)


target <- data.table(
  id = c('inf_a','inf_b','inf_c','inf_d'),
  s = c('071009','081008','100905','101309'),
  str = 1,
  sp = c('071009','081008','100905','101309'),
  type = 'e'
)

q_df_pos <- gradient_function(players$s,target$s)

i <- 0
trans<- list(i = 1,j = data.frame(2))
while(i == 0 | nrow(trans[[2]])>0){
  print(i)
  
  if(i ==0) {
    tgt_out <- target[,list(id,s,str,type)]
    table_out <- rbind(players,tgt_out)
    table_out[,turn:=i]
  }
  
  
  ls1 <- search_par(players,3,200,.95,q_df_pos,target)
  ls1 <- ls1[r == max(r)]
  print(ls1)
  trans <- transition_function(ls1,target,q_df_pos)
  
  players <- trans[[1]][,list(id,s=sp,str,type = "f")]
  target <- trans[[2]]
  q_df_pos <- trans[[3]]
  i <- i + 1
  
  tgt_out <- target[,list(id,s,str,type)]
  table_out_temp <- rbind(players,tgt_out)
  table_out_temp[,turn := i]
  table_out <- rbind(table_out,table_out_temp)
  
  
}


write_csv(x = table_out,file = paste0('out_table',Sys.Date(),'.csv'))
# 
# hexdt <- data.table(hexdf2)[,s := pos]
# 
# pieces <- unique(hexdt[table_out, on = 's',list(s,id,str,type,turn,x_pos,y_pos)])
# pieces[type == 'f', image := "DS/f_inf.svg"]
# pieces[type == 'e', image := 'DS/e_inf.svg']
# 
# 
# p1 <- ggplot(pieces, aes(x = x_pos, y = y_pos)) +
#   geom_polygon(data= hexdt,color = 'grey50',aes(group = pos,x=x_h, y = y_h),fill = 'transparent') +
#   geom_tile(data = pieces,aes(y = y_pos + .5,height = .2, width = 2*str,fill = str),color='black') +
#   #geom_text(data = pieces, aes(label = id,color = type),vjust = .25) +
#   geom_image(data=pieces, aes(image = image)) +
#   scale_fill_distiller(type = "div",direction = 1,limits = c(0,1), palette = "RdYlGn")  +
#   scale_color_manual(breaks = c('e','f'), values = c('darkred','darkgreen')) +
#   coord_equal() +
#   # scale_fill_manual(breaks = c('enemy','friendly','non','conflict'), 
#   #                   values = c('darkred','lightgreen','transparent','orange')) +
#   theme_void() +
#   labs(title = paste("Turn {closest_state}")) +
#   gganimate::transition_states(turn, transition_length = .2, state_length = 2,wrap = FALSE)
# 
# 
# #animate(p1)
# 
# animate(p1, height = 8, width = 10,fps = 10, units = "in", res = 300)
# anim_save('DS/test_fight_better1.gif')
