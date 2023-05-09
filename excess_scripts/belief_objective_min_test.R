library(tidyverse)


df1 <- read_csv('data/hex_ukraine_2.csv')


df2 <- df1 %>%
  mutate(z = as.numeric(str_sub(id,1,2)),x = as.numeric(str_sub(id,3,4)), y = as.numeric(str_sub(id,5,6))) %>%
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
  group_by(yfct,xfct)


hexdf <- data.frame()

for(i in 1:nrow(df2)) {
  
  
  x <- df2[[i,9]]
  y <- df2[[i,10]]
  pos <- df2[[i,11]]
  
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
  left_join(df2, by = 'pos') 


ggplot(hexdf2  , aes(x = x_pos, y = y_pos)) +
  geom_polygon(color = 'black',aes(group = pos,x=x_h, y = y_h), fill = 'transparent') +
  coord_equal() +
  scale_fill_manual(breaks = c('friendly','enemy','neutral'), 
                    values = c('darkgreen','red','transparent')) +
  theme_void() +
  geom_text(aes(label = pos),size = 3) +
  geom_text(aes(label = pos),size = 3) +
  labs(
    title = "Sample Hex-Grid with Location IDs"
  ) +
  theme(
    plot.title = element_text(hjust=.5)
  )






find_move <- function(current_id, target_id) {
  add_or_subtract <- function(value, change) {
    return(sprintf("%02d", as.integer(value) + change))
  }
  
  apply_move <- function(id, move) {
    c1 <- substr(id, 1, 2)
    c2 <- substr(id, 3, 4)
    c3 <- substr(id, 5, 6)
    
    if (move == 'adj2') {
      return(paste0(add_or_subtract(c1, 1), add_or_subtract(c2, 1), c3))
    } else if (move == 'adj3') {
      return(paste0(c1, add_or_subtract(c2, 1), add_or_subtract(c3, 1)))
    } else if (move == 'adj4') {
      return(paste0(add_or_subtract(c1, -1), c2, add_or_subtract(c3, 1)))
    } else if (move == 'adj5') {
      return(paste0(add_or_subtract(c1, -1), add_or_subtract(c2, -1), c3))
    } else if (move == 'adj6') {
      return(paste0(c1, add_or_subtract(c2, -1), add_or_subtract(c3, -1)))
    } else if (move == 'adj1') {
      return(paste0(add_or_subtract(c1, 1), c2, add_or_subtract(c3, -1)))
    } else {
      return(NULL)
    }
  }
  
  find_shortest_distance <- function(start_id, end_id) {
    moves <- c('adj2', 'adj3', 'adj4', 'adj5', 'adj6', 'adj1')
    min_distance <- Inf
    
    best_move <- ""
    
    for(end in end_id){
      
      for (move in moves) {
        next_id <- apply_move(start_id, move)
        distance <- abs(as.integer(substr(next_id, 1, 2)) - as.integer(substr(end, 1, 2))) +
          abs(as.integer(substr(next_id, 3, 4)) - as.integer(substr(end, 3, 4))) +
          abs(as.integer(substr(next_id, 5, 6)) - as.integer(substr(end, 5, 6)))
        
        if (distance < min_distance) {
          min_distance <- distance
          best_move <- move
        }
      }
    }

    
    if(start_id %in% end_id){
      best_move <- 'adj0'
    }
    return(best_move)
  }
  
  return(find_shortest_distance(current_id, target_id))
}

current_id <- "081109"
target_id1 <- c('040911',"070504")
move <- find_move(current_id, target_id)
print(move)


locs <- c('040608','081210')


vec <- as.vector(sapply(legal_acts$s, FUN = find_move, target_id = key_tern$s))
legal2 <- unique(data.table(s = legal_acts$s,a = vec))

units[type == 'e']
left_join(units[type == 'e'],legal2, by = "s")$a
legalt2 <- split(legal2, by = 's')
