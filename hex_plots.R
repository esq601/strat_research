source("hex_setup.R")


# Plot of the field

ggplot(hexdf1, aes (x=x_h, y = y_h,group = pos)) +
  geom_polygon(color = 'black', fill = 'grey90') +
  coord_equal() +
  theme_void() 

ggsave('blankplot.jpeg',height = 8, width = 6,dpi = 320)

#Some plots

hexloc <- hexdf2a %>%
  select(pos,x_pos,y_pos) %>%
  distinct()

ggplot(hexdf2, aes (x=x_h, y = y_h)) +
  geom_polygon(color = 'black',aes(group = pos,fill = disp)) +
  geom_point(data = cities, aes(x = x_pos, y = y_pos), size = 4) +
  geom_text(data = hexloc, aes(x = x_pos, y = y_pos, label = pos)) +
  coord_equal() +
  scale_fill_manual(breaks = c('friendly','enemy','neutral'), 
                    values = c('#0492c2','#e34339','transparent'),
                    labels = c("Blue","Red","None")) +
  theme_void() +
  labs(
    title = "Initial Territorial Disposition",
    fill = "Affiliation",
    subtitle = "Cities Represented as Dots"
  ) +
  theme(
    plot.title = element_text(hjust=.5),
    plot.subtitle = element_text(hjust = 0.5)
  )

ggsave('hex_territory.png',width = 5,height = 4,dpi = 320, units = 'in')

#Plot with different 'area types'



ggplot(hexdf2, aes (x=x_h, y = y_h)) +
  geom_polygon(color = 'black',aes(group = pos, fill = goals)) +
  geom_point(data = cities, aes(x = x_pos, y = y_pos), size = 4) +
  coord_equal() +
  scale_fill_identity() +
  theme_void() +
  labs(
    title = "Geography of Conflict Area",
    subtitle = "With Key Cities"
  )

ggsave('images/final_area.jpeg', width = 9, height = 6, dpi = 320)

#Plot with position codes
head(hexdf2)

hexdf2a <- hexdf2 %>%
  mutate(new = x+y+z)

ggplot(hexdf2a  , aes(x = x_pos, y = y_pos)) +
  geom_polygon(color = 'black',aes(group = pos,x=x_h, y = y_h), fill = 'transparent') +
  coord_equal() +
  scale_fill_manual(breaks = c('friendly','enemy','neutral'), 
                    values = c('darkgreen','red','transparent')) +
  theme_void() +
  # geom_text(aes(label = pos),size = 3) +
  geom_text(aes(label = new),size = 3) +
  labs(
    title = "Sample Hex-Grid with Location IDs"
  ) +
  theme(
    plot.title = element_text(hjust=.5)
  )



ggsave('hex_sample.png',width = 4,height = 4, units = 'in')


library(data.table)
library(ggimage)

units <- data.table(s = c('131609','141608','111510','121408'),str= c(1,.95,1,.4), type = 'f')
eunit <- data.table(s = c('131710','131811','151809'), str = c(.8,.7,.93),type = 'e')

allu <- rbind(units,eunit)
pieces <- unique(data.table(hexdf2)[allu, on = 'pos==s',list(s,str,type,x_pos,y_pos)])
pieces[type == 'f', image := "f_inf.svg"]
pieces[type == 'e', image := 'e_inf.svg']

ggplot(hexdf2, aes (x=x_h, y = y_h)) +
  geom_polygon(color = 'black',aes(group = pos,fill = disp),alpha = 0.2) +
  geom_point(data = cities, aes(x = x_pos, y = y_pos), size = 4) +
  geom_image(data=pieces, aes(image = image,x = x_pos, y = y_pos)) +
  geom_tile(data = pieces,aes(y = y_pos + .5,height = .1, width = 2*str,
                              x = x_pos,color = str),fill = 'black',size = 1) +
  coord_equal(xlim = c(20,max(hexdf2$x_h)),ylim = c(6, max(hexdf2$y_h))) +
  scale_fill_manual(breaks = c('friendly','enemy','neutral'), 
                    values = c('#449c59','#e34339','transparent'),
                    labels = c("Arbathian","Donovian","None")) +
  scale_color_distiller(type = 'div', palette = "RdYlGn",direction = 1, limits = c(0,1)) +
  theme_void() +

  theme(
    plot.title = element_text(hjust=.5),
    plot.subtitle = element_text(hjust = 0.5),
    legend.position = 'none'
  )


ggsave('hex_units.png',width = 5,height = 4, units = 'in')


units <- data.table(s = c('030710','020711'),str= c(1,1), type = 'f')
eunit <- data.table(s = c('030811','040810'), str = 1,type = 'e')

allu <- rbind(units,eunit)
pieces <- unique(data.table(hexdf2)[allu, on = 'pos==s',list(s,str,type,x_pos,y_pos)])
pieces[type == 'f', image := "f_inf.svg"]
pieces[type == 'e', image := 'e_inf.svg']
pieces[,sc := 4]


dtall <- rbind(dtall,pieces)

pieces <- dtall
#dtall[,sc:=1]

ggplot(hexdf2 %>% filter(x_pos < 13 & y_pos > 14 & x_pos > 8 & y_pos < 18), aes (x=x_h, y = y_h)) +
  geom_polygon(color = 'black',aes(group = pos,fill = disp),alpha = 0.2) +
  # geom_point(data = cities, aes(x = x_pos, y = y_pos), size = 4) +
  geom_image(data=pieces, aes(image = image,x = x_pos, y = y_pos),size = .15) +
  geom_tile(data = pieces,aes(y = y_pos + .5,height = .2, width = 2*str,x = x_pos),color='black',fill = 'green') +
  #coord_equal(xlim = c(20,max(hexdf2$x_h)),ylim = c(6, max(hexdf2$y_h))) +
  coord_equal() +
  scale_fill_manual(breaks = c('friendly','enemy','neutral'), 
                    values = c('#449c59','#e34339','transparent'),
                    labels = c("Arbathian","Donovian","None")) +
  theme_void() +
  labs(
    title = "Example Conflict Execution",
    fill = "Affiliation"
  ) +
  facet_wrap(~sc) +
  theme(
    plot.title = element_text(hjust=.5),
    plot.subtitle = element_text(hjust = 0.5),
    legend.position = 'none',
    strip.text = element_blank()
  )

ggsave('hex_conflict.png',width = 5,height = 5, units = 'in')
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

