library(gganimate)
library(ggimage)
library(data.table)

source('hex_setup.R')
table_out <- data.table(read_csv("mcts_test_27mar_a.csv"))

70-28
42/3

#write_csv(units_log, 'mcts_test_27feb.csv')

# new_log <- data.table(read_csv("mcts_test_two_player_05aprh.csv"))
# units_log$turn <- units_log$turn + 10
# 
# units_log <- bind_rows(new_log,units_log)
table_out <- units_log
table_init <- copy(table_out)

table_init[turn == 1, c('sp','turn','a') := .(s,0,'adj0')]
table_out <- rbind(table_init[turn==0],table_out)
hexdt <- data.table(hexdf2)[,s := pos]
str(table_out)

hexdt1 <- unique(hexdt[,list(s,x_pos,y_pos)])
pieces <- hexdt1[table_out, on = c(s = 'sp'),list(s,a,id,str,type,turn,x_pos,y_pos)]
pieces[type == 'f', image := "f_inf.svg"]
pieces[type == 'e', image := 'e_inf.svg']

adj_dt <- data.table(a = c('adj0','adj1','adj2','adj3','adj4','adj5','adj6'),
           angle = c(0,(5/6)*2*pi,0,(1/6)*2*pi,(2/6)*2*pi,(3/6)*2*pi,(4/6)*2*pi),
           rad = c(0,1,1,1,1,1,1))

pieces <- adj_dt[pieces, on = .(a)]

### testing spoke
pnew <- pieces[turn == 2]
pnew
ggplot(pnew, aes(x = x_pos, y = y_pos,group = id)) +
  geom_polygon(data= hexdt,color = 'grey50',aes(group = pos,x=x_h, y = y_h),fill = 'transparent') +
  geom_tile(data = pnew,aes(y = y_pos + .5,height = .2, width = 2*str/100,fill = str),color='black') +
  geom_spoke(data =pnew, aes(x = x_pos, y = y_pos, group = id, angle = angle, radius = rad),
             arrow = arrow(length = unit(0.25, "cm")),size = 1) +
  #geom_text(data = pieces, aes(label = id,color = type),vjust = .25) +
  geom_image(data=pnew, aes(image = image)) +
  scale_fill_distiller(type = "div",direction = 1,limits = c(0,1), palette = "RdYlGn")  +
  scale_color_manual(breaks = c('e','f'), values = c('darkred','darkgreen')) +
  #coord_equal(xlim = c(5,25),ylim = c(0,25)) +
  # scale_fill_manual(breaks = c('enemy','friendly','non','conflict'), 
  #                   values = c('darkred','lightgreen','transparent','orange')) +
  theme_void()




### normal animation
p1 <- ggplot(pieces, aes(x = x_pos, y = y_pos,group = id)) +
  geom_polygon(data= hexdt,color = 'grey50',aes(group = pos,x=x_h, y = y_h),fill = 'transparent') +
  geom_tile(data = pieces,aes(y = y_pos + .5,height = .2, width = 2*str/100,fill = str),color='black') +
  geom_spoke(data =pieces, aes(x = x_pos, y = y_pos, group = id, angle = angle, radius = rad),
             arrow = arrow(length = unit(0.25, "cm")),size = 1) +
  #geom_text(data = pieces, aes(label = id,color = type),vjust = .25) +
  geom_image(data=pieces, aes(image = image)) +
  scale_fill_distiller(type = "div",direction = 1,limits = c(0,100), palette = "RdYlGn")  +
  scale_color_manual(breaks = c('e','f'), values = c('darkred','darkgreen')) +
  #coord_equal(xlim = c(5,25),ylim = c(0,20)) +
  coord_equal() +
  # scale_fill_manual(breaks = c('enemy','friendly','non','conflict'), 
  #                   values = c('darkred','lightgreen','transparent','orange')) +
  theme_void() +
  labs(title = paste("Turn {closest_state}")) +
  gganimate::transition_states(turn, transition_length = 1, state_length = 3,wrap = FALSE)


animate(p1)



animate(p1, height = 8, width = 10,fps = 10,duration = 30, units = "in", res = 120)
anim_save('images/test_fight_mcts_twoplayer_c.gif')


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
