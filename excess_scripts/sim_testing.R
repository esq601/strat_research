
out <- simulate_mcts(units,selected_a,legal_a = legal_acts,terr_loc=territory, 
                     q=q_work1,c =0.4,
                     n_iter = 200, depth =12,  actions=actions,
                     k_terr = key_tern)

(out[[2]][order(-q)])

out_lst <- out[[3]]

out_lst[['070807']]

act_vec <- vector()

for(i in 1:nrow(units[type == 'f']) ) {
  #print(units[type == 'f'][[i,2]])
  unitsel <- units[i]
  print(unitsel)
  print(out_lst[[unitsel$s]][[unitsel$id]])
  print(which.max(out_lst[[unitsel$s]][[unitsel$id]]$q))
  
  act <- out_lst[[unitsel$s]][[unitsel$id]][which.max(out_lst[[unitsel$s]][[unitsel$id]]$q)]$a
  act_vec <- c(act_vec,act)
}

act_vec
df <- out[[3]]


ggplot(df) +
  geom_boxplot(aes(x = event, y = t))


#lubridate::as.duration(out_f[[5]])


df1 <-df %>%
  group_by(event) %>%
  mutate(t = lubridate::as.duration(t)) %>%
  summarise(num = n(), total = sum(t), mean = mean(t), sd = sd(t))

df1#sum(df1$pct)


lubridate::as.duration(sum(df1$total))

df2 <- df[event == 'move_select',rownum := seq_len(.N)]
df2 <- df2[event == 'move_select']
ggplot(df2) +
  geom_point(aes(x = rownum, y = t))

(out[[2]][order(-q)])

dttest <- data.table()

list1 <- list('adj1' = testpr[a == 'adj1'])

for(i in 1:1500){
  t1 <- Sys.time()
  
  samp <- sample(list1[[last[[1]]]]$nexta,1,prob = list1[[last[[1]]]]$p)
  dttest <- rbind(dttest,data.table(time = Sys.time()-t1))
}

mean(dttest$time)


testpr <- prob_setup()

testls <- split(testpr, by = 'a')



belfun <- function(df,plist){
  sample(plist[[df]]$nexta,1,prob = plist[[df]]$p)
}

belfun(last[[1]],testls)


s_sel <- c('060101','090704')
last <- c('adj2','adj2')



l2 <- split(legal_acts, by = 's')
l2[['060505']][a == 'adj1']

samefun <- function(df,olist){
  
  return(olist[[df[[1]]]][a == df[[2]]]$sp)
}


samefun(s_act[1],l2)


dttest <- data.table()

for(i in 1:1500){
  t1 <- Sys.time()
  
  act_sel <- unlist(lapply(last,belfun, plist = testls))
  s_act <- data.table(s = s_sel, a = act_sel)
  
  
  
  #dtout <- l2[s_act, on = .(s,a)]
  
  dttest <- rbind(dttest,data.table(time = Sys.time()-t1))
}

class(dtout) == 'character'

s_sel <- c('050607','060707','070706')
act_sel <- c('adj2','adj1','adj4')

sssel <- c('inf_1050607','inf_2060707','inf_3070706')



samefun(s_act[4,],olist = lsto)

act_sel <- unlist(lapply(last,belfun, plist = testls))
s_act <- data.table(s = s_sel, a = act_sel)
lsto <- split(legal_acts, by = 's')

dtout <- apply(s_act,FUN = samefun,MARGIN = 1,olist = lsto)
dtout

test_ls <- split(out_single, by = 's.s')

belfun(sssel[[1]],test_ls)
test_ls[sssel[1]]


actvec_f <- apply(lasta,FUN = selfun,MARGIN = 1,
                  prob = sing_lst, legal_a = leg_lst, rand = FALSE)

s_act <- data.table(s = s_sel, a = act_sel)
sp_sel <- apply(s_act,FUN = samefun,MARGIN = 1,olist = lsto)

while(class(sp_sel) == 'list' | length(sp_sel) == 0 |any(duplicated(sp_sel)) == TRUE){
   print('ahhhh')
  actvec_f <- apply(s_act,FUN = selfun,MARGIN = 1,
                    prob = test_ls, legal_a = lsto, rand = rand_switch)
  
  s_act <- data.table(s = lasta$s, a = actvec_f)
  sp_sel <- apply(s_act,FUN = samefun,MARGIN = 1,olist = leg_lst)
  
  j_switch <- j_switch+1
  
  if(j_switch>10){
    rand_switch <- TRUE
  }
}



mean(dttest$time)

sample(testpr[a == 'adj0']$nexta,1,prob = testpr[a == 'adj0']$p)




# Create example data.table object
dt <- data.table(x = 1:10, y = 11:20, z = 21:30)

# Define example function that takes each row of dt as input
my_function <- function(row) {
  result <- row$x + row$y + row$z
  return(result)
}

# Use lapply to apply my_function to each row of dt
result_list <- lapply(as.data.frame(dt), my_function)

# Print result
print(result_list)




test <- split(out_single, by = 's.s')

stateidd <- c('inf_1050607','inf_2060808')
statee <- c('050607','060808')
leg_lss <- split(legal_acts, by = 's')

selfun <- function(state,stateid,prob,legal_a){
  #print(prob[[stateid]])
  if(is.null(prob[[stateid]])==TRUE){
    
    sample(legal_a[[state]]$a,1)
    #legal_a[[state]][sample(nrow(legal_a),1), ]
  } else{
    #print('here')
    sample(prob[[stateid]]$a,1,replace = TRUE,prob =prob[[stateid]]$q )
    #prob[[stateid]][sample(nrow(prob),1,replace = TRUE,prob =prob[[stateid]]$q ), ]
  }
}

selfun(statee,stateidd,test,leg_lss)


testdt <- data.table()

unlist(lapply(statee,selfun,
              stateid = stateidd,
              prob = test, legal_a = leg_lss))

sample(adj_df,1)

which(out_single$q == 0)

leg_lss


testid <- c('inf1','inf2','inf3')
tests <- c('123','566','788')
testa <- c('a','b','c')

paste0(testid,tests,testa,collapse = '')



#### testing conflict


loc1 <- c(8,8,6)
loc2 <- c(8,9,7)

if(sum(abs(loc1-loc2)) <= 2 & max(abs(loc1-loc2) ) <= 1){
  print('conf!')
} else {
  print('nope')
}


ptest <- data.table(sp = c('050708','060707','080806'))
ttest <- data.table(sp = c('060808','070807','080907'))

conf_check3 <- function(players,target){
  
  out <- data.table()
  
  if(nrow(players) > 0 & nrow(target) > 0){
    
    f_vec <- lapply(players$sp,
                              function(x) c(as.integer(substr(x, 1, 2)), 
                                              as.integer(substr(x, 3, 4)),
                                              as.integer(substr(x, 5, 6))))
    
    e_vec <- lapply(target$sp,
                    function(x) c(as.integer(substr(x, 1, 2)), 
                                  as.integer(substr(x, 3, 4)),
                                  as.integer(substr(x, 5, 6))))
    
  }
  list(f_vec,e_vec)
}

testlst <- conf_check3(ptest,ttest)
testlst[[1]]


compare_lists <- function(list1, list2) {
  lapply(list1, function(x) {
    # create a boolean vector to store the comparison results for each element in list2
    which(sapply(list2, function(y) {
      # calculate the maximum difference between each element of x and y
      max_diff1 <- max(abs(x-y))
      #max_diff2 <- max(abs(diff(y)))
      
      # calculate the difference between the sums of x and y
      sum_diff <- abs(sum(x) - sum(y))
      
      # return a boolean indicating if the criteria are met
      sum_diff <= 2 & max_diff1 <=1
    }))
  })
}

testlst[[1]]
testlst[[2]]

compare_lists(testlst[[1]],testlst[[2]])





#### checking out the tree growth


df_tree <- out[[1]]

df_tree1 <- data.table()
for(turnno in c(1,2,3)){
  for(unit in c('inf_1','inf_2','inf_3')){
    for(i in seq(from =0, to = 900, by = 100)){
      dt_tree_temp <- df_tree[turn == turnno & sim > i & sim < i+100 & id == unit] %>%
        group_by(s,id,turn) %>%
        summarise(num = n())
      
      dt_tree_temp$window <- i
      
      df_tree1 <- rbind(df_tree1, dt_tree_temp)
    }
  }
}




df_tree1 %>%
  group_by(id,window,turn) %>%
  summarise(sumt = sum(num))


hexdt_tree <- df_tree1 %>%
  left_join(hexdf2, by = c('s' = 'pos'))

hexdf3 <- as.data.table(hexdf2)

p1 <- ggplot(hexdf3[x_h <= max(hexdt_tree$x_h)], aes(x = x_pos, y = y_pos)) +
  geom_polygon(data = hexdf3[x_h <= max(hexdt_tree$x_h)],color = 'grey50',aes(group = pos,x=x_h, y = y_h),fill = 'grey80') +
  geom_polygon(data= hexdt_tree,color = 'grey50',aes(group = s,x=x_h, y = y_h,fill = num)) +
  #geom_point(data = hexdt_kt, aes(y = y_pos+0.5, x = x_pos),inherit.aes = FALSE, shape = '\u2605', color = 'gold', size = 15) +
  # geom_tile(data = pnew,aes(y = y_pos + .5,height = .2, width = 2*str/100,fill = str),color='black') +
  # geom_spoke(data =pnew, aes(x = x_pos, y = y_pos, group = id, angle = angle, radius = rad),
  #            arrow = arrow(length = unit(0.25, "cm")),size = 1) +
  # #geom_text(data = pieces, aes(label = id,color = type),vjust = .25) +
  # geom_image(data=pnew, aes(image = image)) +
  # scale_fill_distiller(type = "div",direction = 1,limits = c(0,1), palette = "RdYlGn")  +
  # scale_color_manual(breaks = c('e','f'), values = c('darkred','darkgreen')) +
  #coord_cartesian(xlim = c(0,20)) +
  #coord_equal(xlim = c(5,25),ylim = c(0,25)) +
  # scale_fill_manual(breaks = c('enemy','friendly','non','conflict'), 
  #                   values = c('darkred','lightgreen','transparent','orange')) +
  coord_fixed() +
  facet_wrap(turn~id) +
  theme_void() +
  transition_states(window) +
  view_follow()


animate(p1, end_pause = c(0,30))

library(extrafont)


ggplot(hexdf3[x_h <= max(hexdt_tree$x_h)], aes(x = x_pos, y = y_pos)) +
  geom_polygon(data = hexdf3[x_h <= max(hexdt_tree$x_h)],color = 'grey50',aes(group = pos,x=x_h, y = y_h),fill = '#9cc797') +
  geom_polygon(data= hexdt_tree[window == 900],color = 'grey50',aes(group = s,x=x_h, y = y_h,fill = num)) +
  #geom_point(data = hexdt_kt, aes(y = y_pos+0.5, x = x_pos),inherit.aes = FALSE, shape = '\u2605', color = 'gold', size = 15) +
  # geom_tile(data = pnew,aes(y = y_pos + .5,height = .2, width = 2*str/100,fill = str),color='black') +
  # geom_spoke(data =pnew, aes(x = x_pos, y = y_pos, group = id, angle = angle, radius = rad),
  #            arrow = arrow(length = unit(0.25, "cm")),size = 1) +
  # #geom_text(data = pieces, aes(label = id,color = type),vjust = .25) +
  # geom_image(data=pnew, aes(image = image)) +
  # scale_fill_distiller(type = "div",direction = 1,limits = c(0,1), palette = "RdYlGn")  +
  # scale_color_manual(breaks = c('e','f'), values = c('darkred','darkgreen')) +
  #coord_cartesian(xlim = c(0,20)) +
  #coord_equal(xlim = c(5,25),ylim = c(0,25)) +
  # scale_fill_manual(breaks = c('enemy','friendly','non','conflict'), 
#                   values = c('darkred','lightgreen','transparent','orange')) +
coord_fixed() +
  scale_fill_distiller(type='seq', palette =12, direction = 1) +
  facet_wrap(id~turn) +
  theme_void() +
  labs(title = "State Exploration by Unit and Depth",
       subtitle = "Simulations 900 - 1000",
       fill = "# Visits") +
  theme(
    text = element_text(family = 'Bahnschrift'),
    plot.background =   element_rect(fill = 'white', color = 'transparent')
  )

ggsave('images/depth_900.png',height = 8, width =5 , dpi = 320)

df
tree2 <- df_tree %>%
  select(-val) %>%
  pivot_wider(names_from = id, values_from = c(s,a)) %>%
  mutate(id_val = paste(turn,s_inf_1,s_inf_2,s_inf_3, sep = '_')) %>%
  select(state = id_val, level = turn, sim )

install.packages("igraph")
install.packages("ggraph")
install.packages("tidygraph")

library(igraph)
library(ggraph)
library(tidygraph)
library(stringr)
library(extrafont)
#write_csv(df_tree,'data/df_tree.csv',)

# arrange the data by simulation and level
df <- tree2[order(tree2$sim, tree2$level), ]

# create an edge list
edges <- data.frame(
  from = df$state[-nrow(df)],  # all states except the last one
  to = df$state[-1],           # all states except the first one
  sim = df$sim[-nrow(df)]      # corresponding simulation number
) %>%
  filter(stringr::str_detect(stringr::str_sub(to,1,1),"0") == FALSE)

#str_sub(edges$to,1,1)
# create a graph
graph <- graph_from_data_frame(edges, directed=TRUE)


# create a tidygraph object from the igraph object
tg <- as_tbl_graph(graph)

# visualize the graph
ggraph(tg, layout = 'tree') +
  geom_edge_link(edge_width = 0.25) +
  geom_node_point() +
  theme_graph() +
  labs(title = "Search Tree",
       subtitle = "Three Units, 1000 Simulations") +
  theme(
    text = element_text(family = 'Bahnshrift'),
    plot.background = element_rect(fill = 'white', color = 'transparent')
  )


ggsave('search_tree.png', width = 12, height = 6, dpi = 640)

tree3 <- tree2 %>%
  group_by(level) %>%
  summarise(distnum = n_distinct(state))
