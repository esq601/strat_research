source("hex_setup.R")


# Plot of the field

ggplot(hexdf1, aes (x=x_h, y = y_h,group = pos)) +
  geom_polygon(color = 'black', fill = 'transparent') +
  coord_equal() +
  theme_void()

#Some plots

ggplot(hexdf2, aes (x=x_h, y = y_h)) +
  geom_polygon(color = 'black',aes(group = pos,fill = disp)) +
  geom_point(data = cities, aes(x = x_pos, y = y_pos), size = 4) +
  coord_equal() +
  scale_fill_manual(breaks = c('friendly','enemy','neutral'), 
                    values = c('darkgreen','red','transparent')) +
  theme_void()
#Plot with different 'area types'

ggplot(hexdf2, aes (x=x_h, y = y_h)) +
  geom_polygon(color = 'black',aes(group = pos,fill = goals)) +
  geom_point(data = cities, aes(x = x_pos, y = y_pos), size = 4) +
  coord_equal() +
  scale_fill_manual(breaks = c('area1','area2','area3'), 
                    values = c('#4a5a78','#7d97c7','transparent')) +
  theme_void()

#Plot with position codes

ggplot(hexdf2 %>% filter(x<=6 & y >= 8), aes(x = x_pos, y = y_pos)) +
  geom_polygon(color = 'black',aes(group = pos,x=x_h, y = y_h), fill = 'transparent') +
  coord_equal() +
  scale_fill_manual(breaks = c('friendly','enemy','neutral'), 
                    values = c('darkgreen','red','transparent')) +
  theme_void() +
  geom_text(aes(label = pos),size = 3)

tile <- '101107'

# Save a neat gif

saveGIF({
  for(i in 1:20){
    print(tile)
    trans <- adj_df %>%
      filter(pos == tile)
    
    
    
    plot_df <- hexdf2 %>%
      mutate(colorfill = case_when(
        pos == tile ~ 'selected',
        pos %in% trans$adj ~ 'adj',
        TRUE ~ 'non'
      ))
    
    
    p1 <- ggplot(plot_df, aes(x = x_pos, y = y_pos)) +
      geom_polygon(color = 'black',aes(group = pos,x=x_h, y = y_h, fill = colorfill)) +
      coord_equal() +
      scale_fill_manual(breaks = c('selected','adj','non'), 
                        values = c('darkgreen','lightgreen','transparent')) +
      theme_void() +
      labs(title = paste("Turn",i)) +
      geom_text(aes(label = pos),size = 3)
    tile <- sample(trans$adj,1)
    print(p1)
    
  }
  
}, interval = .4, movie.name="test.gif",ani.res = '120',ani.height = 1300, ani.width = 1600)

