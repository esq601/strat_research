library(gganimate)
library(ggimage)
library(data.table)

source('hex_setup.R')
table_out <- data.table(read_csv("out_table2022-09-12.csv"))
hexdt <- data.table(hexdf2)[,s := pos]
str(table_out)
pieces <- unique(hexdt[table_out, on = 's',list(s,id,str,type,turn,x_pos,y_pos)])
pieces[type == 'f', image := "f_inf.svg"]
pieces[type == 'e', image := 'e_inf.svg']


p1 <- ggplot(pieces, aes(x = x_pos, y = y_pos,group = id)) +
  geom_polygon(data= hexdt,color = 'grey50',aes(group = pos,x=x_h, y = y_h),fill = 'transparent') +
  geom_tile(data = pieces,aes(y = y_pos + .5,height = .2, width = 2*str,fill = str),color='black') +
  #geom_text(data = pieces, aes(label = id,color = type),vjust = .25) +
  geom_image(data=pieces, aes(image = image)) +
  scale_fill_distiller(type = "div",direction = 1,limits = c(0,1), palette = "RdYlGn")  +
  scale_color_manual(breaks = c('e','f'), values = c('darkred','darkgreen')) +
  coord_equal(xlim = c(5,25),ylim = c(4,20)) +
  # scale_fill_manual(breaks = c('enemy','friendly','non','conflict'), 
  #                   values = c('darkred','lightgreen','transparent','orange')) +
  theme_void() +
  labs(title = paste("Turn {closest_state}")) +
  gganimate::transition_states(turn, transition_length = 1, state_length = 3,wrap = FALSE)


animate(p1)



animate(p1, height = 8, width = 10,fps = 10,duration = 20, units = "in", res = 120)
anim_save('test_fight_4fight1.gif')


pieces_sub <- pieces[turn %in% c(0,2,4,8,14,16,17,18,24)]

ggplot(pieces_sub, aes(x = x_pos, y = y_pos,group = id)) +
  geom_polygon(data= hexdt[x_h < max(pieces_sub$x_pos)+2 &
                             x_h > min(pieces_sub$x_pos)-2 &
                             y_h < max(pieces_sub$y_pos)+2 &
                             y_h > min(pieces_sub$y_pos)-2],color = 'grey50',aes(group = pos,x=x_h, y = y_h),fill = 'transparent') +
  
  #geom_text(data = pieces, aes(label = id,color = type),vjust = .25) +
  geom_image(data=pieces_sub, aes(image = image),size = .15) +
  geom_tile(data = pieces_sub,aes(y = y_pos + .5,height = .2, width = 2*str,fill = str),color='black') +
  geom_label(data = pieces_sub, aes(label = id),size = 3,label.padding = unit(0.01, "lines"),label.size = 0) +
  scale_fill_distiller(type = "div",direction = 1,limits = c(0,1), palette = "RdYlGn")  +
  scale_color_manual(breaks = c('e','f'), values = c('darkred','darkgreen')) +
  coord_equal() +
  theme_void() +
  theme(legend.position = 'none') +
  #coord_equal(xlim = c(10,17),ylim = c(4,20)) +
  # scale_fill_manual(breaks = c('enemy','friendly','non','conflict'), 
  #                   values = c('darkred','lightgreen','transparent','orange')) +

  facet_wrap(~turn,ncol = 3)

ggsave('hex_battle.png',width = 8,height = 8, units = 'in',dpi = 320)
