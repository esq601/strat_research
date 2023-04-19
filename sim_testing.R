
out <- simulate_mcts(units,selected_a,legal_a = legal_acts,terr_loc=territory, 
                     q=q_work,c =2,
                     n_iter = 1*7^nrow(units[type == 'f']), depth =5, single_out = out_single, actions=actions)
df <- out[[4]]


ggplot(df) +
  geom_boxplot(aes(x = event, y = t))


lubridate::as.duration(out[[5]])


df1 <-df %>%
  group_by(event) %>%
  summarise(num = n(), total = sum(t), mean = mean(t), sd = sd(t),pct = lubridate::as.duration(total)/lubridate::as.duration(out[[5]]))
sum(df1$pct)

lubridate::as.duration(sum(df1$total))

df2 <- df[event == 'ex_update',rownum := seq_len(.N)]
df2 <- df2[event == 'ex_update']
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

act_sel <- unlist(lapply(last,belfun, plist = testls))
s_act <- data.table(s = s_sel, a = act_sel)
dtout <- apply(s_act,FUN = samefun,MARGIN = 1,olist = l2)
dtout
while(class(dtout) == 'list' | any(duplicated(dtout)) == TRUE){
  print('working!')
  act_sel <- unlist(lapply(last,belfun, plist = testls))
  s_act <- data.table(s = s_sel, a = act_sel)
  dtout <- apply(s_act,FUN = samefun,MARGIN = 1,olist = l2)
}
dtout
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
