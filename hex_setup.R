library(tidyverse)
#library(animation)


#This will create a roughly rectangular hex board about 12x15
#Function needs to be generalized to create dimensions required
#or a board that's not purely rectangular

#However, the adjancency functions will work with any configuration
#if something else is coded here.

df1 <- data.frame()
xbase <- 6
zbase <- 1

for(j in 0:14){
  x <- xbase + j
  z <- zbase + j
  for(i in 1:12) {
    
    
    #print(i %% 2)
    if(i > 1){
      if(i %% 2 == 0) {
        z <- z + 1
      } else {
        x <- x - 1
      }
    }
    
    df1 <- bind_rows(df1, data.frame(x,z,y=i))
    
  }
}



# Create center-points of hexes

df2 <- df1 %>%
  mutate(yz_sum = y+z, xz_sum = x + z,xy_sum = x + y,sumval= x+y+z) %>%
  mutate(x_pos = x+z) %>%
  mutate(y_pos = case_when(
    y %% 2 == 1 ~ 3.4641*(y-1)/2,
    T ~ 3.4641*(y)/2 - (3.4641/2))) %>%
  rowwise() %>%
  mutate(pos = paste0(
    ifelse(nchar(x)<2,paste0(0,x),x),
    ifelse(nchar(z)<2,paste0(0,z),z),
    ifelse(nchar(y)<2,paste0(0,y),y)
  )) %>%
  mutate(yfct = factor(y_pos),xfct = factor(x_pos)) %>%
  group_by(yfct,xfct) %>%
  filter(xz_sum >6 )



# hex_removal <- c('120701','130801','140901','130902',
#                  '111712','111611','111510','111409','121509',
#                  '080705','201501','080806','051112','080301','61212','080402',
#                  '061111')
# 
# 
# df2 <- df2 %>% filter(!(pos %in% hex_removal))
#Create points for the verticies of hexs

hexdf <- data.frame()

for(i in 1:nrow(df2)) {
  
  
  x <- df2[[i,8]]
  y <- df2[[i,9]]
  pos <- df2[[i,10]]
  
  x_1 <- x-1
  x_2 <- x-1
  x_3 <- x
  x_4 <- x+1
  x_5 <- x+1
  x_6 <- x
  y_1 <- y - .57735
  y_2 <- y + .57735
  y_3 <- y + 1.1547
  y_4 <- y + .57735
  y_5 <- y - .57735
  y_6 <- y - 1.1547
  
  temp <- data.frame(pos = pos, x_h = c(x_1,x_2,x_3,x_4,x_5,x_6),
                     y_h = c(y_1,y_2,y_3,y_4,y_5,y_6))
  
  hexdf <- rbind(hexdf,temp)
}

hexdf1 <- hexdf %>%
  group_by(pos)




tile <- '030710'



#Create some new fields for demo purposes

hexdf2 <- hexdf1 %>%
  distinct() %>%
  left_join(df2, by = 'pos') %>%
  mutate(selected = case_when(pos == tile ~T,T ~ F)) %>%
  mutate(disp = case_when(
    z > 17 | x > 17 & y > 16 ~'enemy',
    T ~ 'friendly'
  )) %>%
  mutate(goals = case_when(
    x_pos > 24 & y_pos < 10 ~ 'area1',
    x_pos > 21 & y_pos >= 10 ~ 'area2',
    T ~ 'area3'
  ))

#Create random 'cities' or tiles for different values

cities <- df2 %>% filter(pos %in% c('151405','110702','101208','061010','040507'))




#ggsave('hexmap1.jpeg', dpi = 320)


#ggsave('hexmap2.jpeg', dpi = 320)




terr1 <- c('201501','191401','191502','201602','191603')




#ggsave('hexcoord.jpeg',dpi = 320)



# Create all adjacency tiles for given board
# Transition data frame

adj_df <- hexdf2 %>%
  ungroup() %>%
  select(pos,x,y,z) %>%
  distinct() %>%
  mutate(adj1 = paste0(ifelse(x<9,paste0(0,x+1),x+1),ifelse(z<10,paste0(0,z),z),ifelse(y<11,paste0(0,y-1),y-1)),
         adj2 = paste0(ifelse(x<9,paste0(0,x+1),x+1),ifelse(z<9,paste0(0,z+1),z+1),ifelse(y<10,paste0(0,y),y)),
         adj3 = paste0(ifelse(x<10,paste0(0,x),x),ifelse(z<9,paste0(0,z+1),z+1),ifelse(y<9,paste0(0,y+1),y+1)),
         adj4 = paste0(ifelse(x<11,paste0(0,x-1),x-1),ifelse(z<10,paste0(0,z),z),ifelse(y<9,paste0(0,y+1),y+1)),
         adj5 = paste0(ifelse(x<11,paste0(0,x-1),x-1),ifelse(z<11,paste0(0,z-1),z-1),ifelse(y<10,paste0(0,y),y)),
         adj6 = paste0(ifelse(x<10,paste0(0,x),x),ifelse(z<11,paste0(0,z-1),z-1),ifelse(y<11,paste0(0,y-1),y-1))) %>%
  select(pos,contains("adj")) %>%
  pivot_longer(cols = contains('adj'), values_to = 'adj',names_to = 'a') %>%
  rename(s = pos, sp = adj) %>%
  filter(sp %in% hexdf2$pos)

adj_df <- bind_rows(adj_df,data.frame(s=unique(adj_df$s),a = 'adj0',sp = unique(adj_df$s)))



# Random walk across the board

#saveRDS(adj_df, file = "hextest.rda")
