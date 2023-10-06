library(tidyverse)

# The extraction function
extract_between_last_underscore_and_dot <- function(str) {
  str_extract(str, "(?<=_)[^_]+(?=\\.)")
}

meta_files <- paste0('sherlock_results/meta/',list.files('sherlock_results/meta/'))
outcome_files <- paste0('sherlock_results/outcome/',list.files('sherlock_results/outcome/'))
# Read in Lanchester Files

files_lanc <- meta_files[grep("lanc", meta_files)]

data_lanc <- data.frame()

for(file in 1:length(files_lanc)){
  
  dftemp <- suppressMessages(suppressWarnings(read_csv(files_lanc[file])))
  dftemp$sim_id <- files_lanc[file]
  data_lanc <- rbind(data_lanc,dftemp)
}


data_lanc <- data_lanc %>%
  mutate(sim_id = as.character(extract_between_last_underscore_and_dot(sim_id)))

write_csv(data_lanc,'sherlock_results/processed/data_lanc.csv')
# Read in main files

files_main <- meta_files[grep("main", meta_files)]

data_main <- data.frame()

for(file in 1:length(files_main)){
  
  dftemp <- suppressMessages(suppressWarnings(read_csv(files_main[file])))
  dftemp$sim_id <- files_main[file]
  data_main <- rbind(data_main,dftemp)
}

data_main <- data_main %>%
  mutate(sim_id = as.character(extract_between_last_underscore_and_dot(sim_id)))

write_csv(data_main,'sherlock_results/processed/data_main.csv')

# Read in reinforcmeent

files_rein <- meta_files[grep("rein", meta_files)]

data_rein <- data.frame()

for(file in 1:length(files_rein)){
  
  dftemp <- suppressMessages(suppressWarnings(read_csv(files_rein[file])))
  dftemp$sim_id <- files_rein[file]
  data_rein <- rbind(data_rein,dftemp)
}

data_rein <- data_rein %>%
  mutate(sim_id = as.character(extract_between_last_underscore_and_dot(sim_id)))

write_csv(data_rein,'sherlock_results/processed/data_rein.csv')

# Read in territory

files_terr <- outcome_files[grep("terr", outcome_files)]

data_terr <- data.frame()

for(file in 1:length(files_terr)){
  
  dftemp <- suppressMessages(suppressWarnings(read_csv(files_terr[file], 
                                                       col_types = cols(type = col_character()))))
  dftemp$sim_id <- files_terr[file]
  data_terr <- rbind(data_terr,dftemp)
}

data_terr <- data_terr %>%
  mutate(sim_id = as.character(extract_between_last_underscore_and_dot(sim_id))) %>%
  distinct()

write_csv(data_terr,'sherlock_results/processed/data_terr.csv')

# Read in units


files_unit <- outcome_files[grep("unit", outcome_files)]

data_unit <- data.frame()

for(file in 1:length(files_unit)){
  
  dftemp <- suppressMessages(suppressWarnings(read_csv(files_unit[file])))
  dftemp$sim_id <- files_unit[file]
  data_unit <- rbind(data_unit,dftemp)
}

data_unit <- data_unit %>%
  mutate(sim_id = as.character(extract_between_last_underscore_and_dot(sim_id)))
write_csv(data_unit,'sherlock_results/processed/data_unit.csv')




### Stop here
data_terr <- read_csv('sherlock_results/processed/data_terr.csv', 
                      col_types = cols(sim_id = col_character()))
data_unit <- read_csv('sherlock_results/processed/data_unit.csv', 
                      col_types = cols(sim_id = col_character()))
data_rein <- read_csv('sherlock_results/processed/data_rein.csv', 
                      col_types = cols(sim_id = col_character()))
data_main <- read_csv('sherlock_results/processed/data_main.csv', 
                      col_types = cols(sim_id = col_character()))
data_lanc <- read_csv('sherlock_results/processed/data_lanc.csv', 
                      col_types = cols(sim_id = col_character()))


data_unit_turns <- data_unit %>%
  group_by(sim_id) %>%
  summarise(num_turns = max(turn))


mod_cost <- -35000000
mob_full <- -10000000
mob_part <- -5000000
pos_forw <- -5000000

loss_cost <- -100000000
draw_cost <- -1000000
win_cost <- 0

data_main_wide <- data_main %>%
  pivot_wider(names_from = param, values_from = value) %>%
  mutate(dec_cost = case_when(
    modernize == 'Modernize' ~ mod_cost,
    T ~ 0
  )) %>%
  mutate(dec_cost = case_when(
    posture == 'Forward' ~ dec_cost + pos_forw,
    T ~ dec_cost
  )) %>%
  mutate(dec_cost = case_when(
    mobilize == 'Full' ~ dec_cost + mob_full,
    mobilize == 'Partial' ~ dec_cost + mob_part,
    T ~ dec_cost
  )) %>%
  mutate(p_inv = 0.6) %>%
  mutate(p_inv = ifelse(modernize == "Modernize", p_inv + .05,p_inv)) %>%
  mutate(p_inv = ifelse(posture == "Forward" , p_inv + .05,p_inv)) %>%
  mutate(p_inv = ifelse(mobilize == "Full" , p_inv + 0.1,p_inv)) %>%
  mutate(p_inv = ifelse(mobilize == "Partial" , p_inv + 0.025,p_inv))


# Compare relative Lanc advantages

advantage_lanc <- data_lanc %>%
  select(-target_side) %>%
  pivot_wider(names_from = shooter_side, values_from = mod) %>%
  mutate(low_rng = f*.8, high_rng = f*1.1) %>%
  mutate(adv = case_when(
    e < low_rng ~ 'f',
    e >= low_rng  ~ 'n'
  )) %>%
  mutate(diff = f - e)



adv_lanc_sum <- advantage_lanc %>%
  group_by(sim_id) %>%
  summarise(sit = paste(adv,collapse = '')) %>%
  mutate(adv = case_when(
    str_count(sit,'f') >= 3 ~ 'f',
    str_count(sit, 'e') >= 3 ~ 'e',
    str_count(sit, 'e') == 2 & str_count(sit, 'n') == 2 ~ 'e',
    str_count(sit, 'f') == 2 & str_count(sit, 'n') == 2 ~ 'f',
    T ~ 'n'
  ))

summary(as.factor(adv_lanc_sum$adv))

data_main_test <- data_main_wide %>%
  left_join(adv_lanc_sum) %>%
  filter(mobilize == 'Partial' & modernize == 'Modernize' & posture == 'Forward')

# Calculate Utility for Territory

strat_value <- data.frame(s = c('050607','081210','151102','131407'),
                          strat_val = c(1,1,1,1))

terr_gamma <- 0

data_terr1 <- data_terr %>%
  left_join(strat_value) %>%
  mutate(x_turn = ifelse(type == 'f',0,-1*strat_val))

data_terr1a <- data_terr1 %>%
  group_by(sim_id) %>%
  summarise(x = 1+(sum(x_turn)/ (4*24)),
            u = ifelse(terr_gamma == 0,x,(1- exp(-terr_gamma * x))/(1-exp(-terr_gamma))))

# 
# ggplot(data_terr1a) + 
#   geom_point(aes(x = x, y = u))
# 
# ggplot(data_terr1a) +
#   geom_histogram(aes(x = u))

# Calculate utility for units


worst_lives <- -40*1000-25*400
worst_cost <- -250000*1000-600000*400 + mod_cost+ mob_full + pos_forw + loss_cost
pos_forw/worst_cost


data_unit1 <- data_unit %>%
  group_by(sim_id,class,type,id) %>%
  summarise(str_lost = max(str)-min(str)) %>%
  group_by(sim_id,type,class) %>%
  summarise(total_lost = sum(str_lost)) %>%
  left_join(adv_lanc_sum) %>%
  mutate(cost = case_when(
    type == 'f' & class == 'inf' ~ -250000*total_lost,
    type == 'f' & class == 'arm' ~ -600000*total_lost,
    type == 'e' & class == 'inf' ~ -250000*total_lost,
    type == 'e' & class == 'arm' ~ -600000*total_lost
  ))


util_cost <- -1

data_util_test <- data_unit %>%
  group_by(sim_id) %>%
  filter(turn == max(turn)) %>%
  #filter(turn >12) %>%
  group_by(sim_id,type) %>%
  summarise(num = n(), total_str = sum(str)) %>%
  pivot_wider(names_from = type, values_from = c(num,total_str)) %>%
  rowwise() %>%
  mutate(outcome = case_when(
    num_e <= 3 & num_f > 2* num_e ~ 'w',
    num_f <= 3 & num_e > 2* num_f ~ 'l',
    T ~ 'd'
  ))

data_unit1_f <- data_unit1 %>%
  filter(type == 'f') %>%
  group_by(sim_id) %>%
  summarise(cost = sum(cost)) %>%
  left_join(data_main_wide %>% select(sim_id,dec_cost,p_inv)) %>%
  left_join(data_util_test %>% select(sim_id, outcome)) %>%
  mutate(out_cost = case_when(
    outcome == 'l' ~ loss_cost,
    outcome == 'd' ~ draw_cost,
    outcome == 'w' ~  win_cost,
    T ~ 0
  )) %>%
  mutate(c_x = 1-abs((dec_cost+p_inv*(cost+out_cost))/worst_cost), x =  c_x) %>%
  rowwise() %>%
  mutate(c_u = ifelse(util_cost == 0,c_x,(1- exp(-util_cost * c_x))/(1-exp(-util_cost))),
         u =  c_u)


# Combine Utilities

# 
# 
# 
# ggplot(data_util_test) +
#   geom_bar(aes(x = outcome,fill = outcome)) +
#   scale_x_discrete(labels = c("Draw","Loss","Win")) +
#   ggsci::scale_fill_aaas() +
#   labs(
#     title = "Outcome of Strategic and Tactical Simulation",
#     x = 'Outcome for Friendly',
#     y = 'Number of Observations'
#   ) +
#   theme(
#     legend.position = 'none'
#   )
# 
# ggsave('images/outcome_bar.jpeg',width = 6, height = 6, dpi = 320)

terr_wt <- .25
unit_wt <- 1-terr_wt

summary(data_joined$cost)



data_joined <- data_unit1_f %>%
  left_join(data_terr1a, by = 'sim_id', suffix = c('_unit','_terr')) %>%
  mutate(u = terr_wt * u_terr + unit_wt * u_unit, x = terr_wt*x_terr + unit_wt*x_unit) %>%
  left_join(data_main_wide) %>%
  left_join(adv_lanc_sum) %>%
  left_join(data_unit_turns) %>%
  left_join(data_util_test) %>%
  filter(num_turns > 10) %>%
  mutate(grp = paste(mobilize,modernize,posture,key_terrain))

ggplot(data_joined) +
  geom_density(aes(x = u, color = modernize)) +
  facet_wrap(~posture)

ggplot(data_joined) +
  geom_point(aes(x = x, y = u), color = 'black') +
  scale_x_continuous(limit= c(0,1)) +
  scale_y_continuous(limit = c(0,1))

ggplot(data_joined) +
  geom_density(aes(x = u)) +
  facet_wrap(vars(modernize, mobilize,posture,key_terrain)) +
  theme(
    strip.text = element_blank()
  )

ggplot(data_joined) +
  geom_density(aes(x = x, group = grp, color = grp)) +
  theme(
    legend.position = 'none'
  )

util <- data_unit1_f %>%
  select(sim_id,u)


data_joined_leaf <- data_joined %>%
  filter(key_terrain == 'All', mobilize == 'Partial',
         posture == 'Forward', eny_obj == 'Single')

ggplot(data_joined_leaf) +
  geom_histogram(aes(x = c_x, fill = modernize),position = 'dodge') +
  scale_x_continuous(limits = c(0,1))




adv_num <- data_joined %>%
  group_by(modernize,adv) %>%
  summarise(num = n()) %>%
  group_by(modernize) %>%
  mutate(prob = num/sum(num))


ggplot(data_joined) +
  geom_density(aes(x = u, group = grp,color = adv)) +
  facet_wrap(c('mobilize','modernize','posture'))

data_joined_sum <- data_joined %>%
  group_by(key_terrain, mobilize, modernize, posture, eny_obj,adv) %>%
  summarise(num = n())



data_main_test <- data_main_wide %>%
  left_join(adv_lanc_sum) %>%
  left_join(util)

data_main_test %>%
  filter(modernize == "Current") %>%
  group_by(adv) %>%
  summarise(n())



ggplot(data_unit1) +
  geom_density(aes(x = total_lost, fill = sit, color = modernize))


data_main <- data_main %>%
  mutate(sim_id = as.character(extract_between_last_underscore_and_dot(sim_id)))


data_lanc1 <- data_lanc %>%
  mutate(sim_id = as.character(extract_between_last_underscore_and_dot(sim_id)))

data_lanc2 <- data_lanc1 %>%
  filter(shooter_side == 'e', shooter_class == 'inf', target_class == 'arm') 

ggplot(data_lanc2) +
  geom_histogram(aes(x = mod))


data_main %>%
  group_by(param,value) %>%
  summarise(num = n())



n_distinct(data_terr$sim_id)



lancmean <- data_lanc %>% filter(shooter_side == 'e',target_side == 'f', 
                          target_class == 'inf', shooter_class == 'inf')

hist(lancmean$mod)

mean(lancmean$mod)
(.05+.2+.15)/3

vecout <- vector()
for(i in 1:10000){
  vecout <- c(vecout,rtri(.05,.2,.15))  
}

hist(vecout)

data.frame(shooter_side = c(rep('f',4),rep('e',4)),
           shooter_class = c(rep(c('inf','inf','arm','arm'),2)),
           target_side = c(rep('e',4),rep('f',4)),
           target_class = c(rep(c('inf','arm'),4)),
           mod = c(
             rtri(.05,.2,.15),
             rtri(.02,.1,.075),
             rtri(.075,.15,.125),
             rtri(.05,.2,.15),
             rtri(.05,.2,.15),
             rtri(.02,.1,.075),
             rtri(.075,.15,.125),
             rtri(.05,.15,.1)
           ))



library(rpart)
library(rpart.plot)


regtree <- data_joined %>%
  select(key_terrain, mobilize, modernize, posture, outcome) %>%
  ungroup() %>%
  mutate(outcome = as.factor(outcome))


ggplot(regtree) +
  geom_density(aes(x = u, color = mobilize)) +
  scale_x_continuous(limits = c(0,1))
str(regtree)
tree <- rpart(outcome ~ ., data = regtree)

rpart.plot(tree)
tree$variable.importance


## Calculate the probability of failure for each decision
test <- data_joined %>%
  filter(key_terrain == 'All',mobilize == 'None',posture == 'Dispersed',modernize == "Current")

summary(factor(test$outcome))

ev_data <- data_joined %>%
  #filter(eny_obj != "Full") %>%
  group_by(key_terrain, mobilize, posture, modernize) %>%
  mutate(f = ifelse(outcome == 'l',1,0)) %>%
  summarise(mean = mean(u, na.rm = T),sd = sd(u, na.rm= TRUE),sdp = mean + sd, sdm = mean-sd,
            num = n(),f_total = sum(f), p_f = f_total/num) %>%
  arrange(p_f) %>%
  ungroup() %>%
  mutate(p_inv = 0.6) %>%
  mutate(p_inv = ifelse(modernize == "Modernize", p_inv + .15,p_inv)) %>%
  mutate(p_inv = ifelse(posture == "Forward" , p_inv + .05,p_inv)) %>%
  mutate(p_inv = ifelse(mobilize == "Full" , p_inv + 0.1,p_inv)) %>%
  mutate(p_inv = ifelse(mobilize == "Partial" , p_inv + 0.05,p_inv)) %>%
  mutate(p_f_new = p_f * p_inv) %>%
  #mutate(mean_new = mean * p_inv) %>%
  mutate(rowval = row_number())

ev_data_pf <- ev_data %>%
  select(key_terrain, mobilize, posture, modernize, p_f_new)

n_samples <- 500

ev_data_bs <- data.frame()

for(moddec in c('Modernize','Current')){
  for(posdec in c('Forward','Dispersed')) {
    for(mobdec in c('None','Partial','Full')){
      for(ktdec in c('All','Most','Rear')){
        
        t1 <- Sys.time()
        
        dftemp <- data_joined %>%
          filter(modernize == moddec, posture == posdec, mobilize == mobdec,key_terrain==ktdec) %>%
          ungroup() %>%
          arrange(eny_obj)
        
        #for(i in 1:nrow(propdf)){
        
        
        dftemp_prop <- data.frame()
        dftemp1 <- data.frame()
        
        for(j in 1:n_samples){
          
          stratavar <- strata(dftemp, 
                              stratanames = strata_var,
                              size = prop_obj,method = 'srswr')
          #print(stratavar)
          dftemp2 <- dftemp[stratavar$ID_unit,]
          #print(dftemp2)
          dftemp2$group <- j
          dftemp1 <- rbind(dftemp1,dftemp2) 
          
          
        }
        #print(dftemp1)
        dftemp1 <- dftemp1 %>%
          
          #group_by(key_terrain, mobilize, posture, modernize) %>%
          #ungroup() %>%
          #group_by(outcome) %>%
          mutate(f = ifelse(outcome == 'l',1,0)) %>%
          summarise(mean = mean(x, na.rm = T),sd = sd(x, na.rm= TRUE),sdp = mean + sd, sdm = mean-sd,
                    num = n(),f_total = sum(f), p_f = f_total/num) %>%
          mutate(p_inv = 0.6) %>%
          mutate(p_inv = ifelse(moddec == "Modernize", p_inv + .15,p_inv)) %>%
          mutate(p_inv = ifelse(posdec == "Forward" , p_inv + .05,p_inv)) %>%
          mutate(p_inv = ifelse(mobdec == "Full" , p_inv + 0.1,p_inv)) %>%
          mutate(p_inv = ifelse(mobdec == "Partial" , p_inv + 0.05,p_inv)) %>%
          mutate(p_f_new = p_f * p_inv) %>%
          mutate(rowval = row_number())
        
        #print(dftemp1)
        #dftemp_prop <- rbind(dftemp_prop,dftemp1)
        
        # dftemp1 <- dftemp1 %>%
        #   ungroup() %>%
        #   summarise(sd = sd(mean),mean = mean(mean),f_total_mean = mean(f_total),sd_p_f = sd(p_f),p_f = mean(p_f))
        #print(dftemp1)
        dftemp1 <- cbind(data.frame(modernize = moddec, posture = posdec,
                                    mobilize = mobdec,key_terrain=ktdec),
                         dftemp1)
        
        
        #print(dftemp1)
        ev_data_bs <- rbind(ev_data_bs,dftemp1)
        #}
        
        
        
        print(nrow(dftemp))
        print(t1 - Sys.time())
        
        
      }
    }
  }
}


ev_data_bs1 <- ev_data_bs %>%
  group_by(modernize,posture,mobilize,key_terrain) %>%
  mutate(p_e = num/sum(num), p_e_new = p_e * p_inv) %>%
  mutate(grp = paste0(modernize,posture,mobilize,key_terrain)) %>%
  mutate(e_u = p_inv * mean)

ggplot(ev_data_bs1) +
  geom_bar(aes(x = grp, y = p_e,fill = outcome),stat= 'identity') +
  coord_flip()

ggplot(ev_data_bs1) +
  geom_bar(aes(x = grp, y = e_u,fill = outcome),stat= 'identity') +
  coord_flip()

ggplot(ev_data_bs)+
  geom_point(aes(x = mean, y = p_f_new, color = modernize,shape = mobilize),size = 2) #+

ggplot(ev_data) +
  geom_point(aes(x = mean, y = p_f_new, color = modernize,shape = mobilize),size = 2) #+
  #geom_segment(aes(x = sdm, xend = sdp, y = p_f_new, yend = p_f_new, color = modernize))
# Get examples of same strategic decisions but different outcomes

data_ex <- data_joined %>%
  filter(key_terrain == 'Rear', mobilize == 'None',
         posture == 'Forward', modernize == 'Current') %>%
  group_by(outcome) %>%
  sample_n(1)

data_sample_play <- data_unit %>%
  filter(sim_id %in% data_ex$sim_id) %>%
  group_by(sim_id,turn,type) %>%
  summarise(total_str = sum(str_old)) %>%
  left_join(data_util_test) %>%
  mutate(outcome = case_when(
    outcome == 'w' ~ 'Win',
    outcome == 'd' ~ 'Draw',
    T ~ 'Loss'
  ))

ggplot(data_sample_play) +
  geom_path(aes(x = turn, y = total_str, color = type),size = 1) +
  facet_wrap(~c(outcome)) +
  labs(
    y = "Total Strength of Army",
    x = "Turn",
    color = "Side",
    title = "Outcome of Campaign Over Time",
    subtitle = "Sample from Same Values of Strategic Decisions"
  )

ggsave('images/outcome_sample.jpeg',width = 8, height = 5, dpi = 320)

ggplot(ev_data,aes(x = rowval)) +
  geom_bar(aes(y = p_f), stat = 'identity') +
  #coord_flip() +
  ggsci::scale_fill_igv() +
  theme(
    axis.text.x = element_blank(),
    axis.ticks.x = element_blank()
  ) +
  labs(
    x = latex2exp::TeX("Decision Set $D^i$"),
    y = latex2exp::TeX("$P(F | D^i)$"),
    title = "Probabilities of Failure Given Strategic Decision Sets"
  )

ggsave('images/outcome_pf_bars_unlabeled.jpeg',width = 8, height = 5, dpi = 320)


ev_data_known <- data_joined %>%
  filter(eny_obj == "Single") %>%
  group_by(key_terrain, mobilize, posture, modernize) %>%
  mutate(f = ifelse(outcome == 'l',1,0)) %>%
  summarise(mean = mean(u, na.rm = T),sd = sd(u, na.rm= TRUE),sdp = mean + sd, sdm = mean-sd,
            num_k = n(),f_total = sum(f), p_f_k = f_total/num_k) %>%
  arrange(p_f_k) %>%
  ungroup() %>%
  mutate(rowval = row_number()) %>%
  select(key_terrain, mobilize, posture, modernize, p_f_k,num_k)
  
ev_new <- ev_data %>%
  left_join(ev_data_known) %>%
  mutate(color = case_when(
    p_f_k <= p_f ~ 'green',
    T ~ 'red'
  ))

ggplot(ev_new) +
  geom_bar(aes(x = rowval, y = p_f), stat = 'identity') +
  geom_point(aes(x = rowval, y = p_f_k, color = color)) +
  scale_color_identity() +
  coord_flip()

ev_advantage <- ev_data %>%
  filter(adv == 'f')


# Figuring out lives
data_joined$grp <- with(data_joined, reorder(grp, lives, FUN=median))

data_joined %>%
  ggplot(aes(y = grp, x = abs(lives))) +
  geom_boxplot()

#Individual Decision Probability Densities of Utility

data_ex1 <- data_joined %>%
  filter(key_terrain == 'Rear', mobilize == 'None',
         posture == 'Forward', modernize == 'Modernize')

data_ex2 <- data_joined %>%
  filter(key_terrain == 'All', mobilize == 'Full',
         posture == 'Forward', modernize == 'Current')

ggplot() +
  geom_density(data=data_ex1,aes(x = u), color = 'darkblue') +
  geom_density(data=data_ex2,aes(x = u), color = 'darkred') +
  scale_x_continuous(limits = c(0,1))


tree2 <- rpart(p_f ~ key_terrain + mobilize + posture + modernize, data = ev_data, method = 'anova',
               cp= 0.025, minsplit=2, minbucket=2)
rpart.plot(tree2)
tree2$variable.importance

levels(data_joined$outcome) <- c('l','d','w')
tree3 <- rpart(outcome ~ key_terrain + mobilize + posture + modernize, data = data_joined,
               cp= 0.0005, minsplit=5, minbucket=5)
rpart.plot(tree3)
hist(ev_data$p_f)

ggplot(ev_data) +
  geom_point(aes(x = mean, y = p_f, color = posture))



ggplot(data_joined) +
  geom_histogram(aes(x = u,fill = outcome), position = 'dodge')

advantage_sim <- advantage_lanc %>%
  group_by(sim_id) %>%
  summarise(disc = sum(diff))



ev_data1 <- data_joined %>%
  left_join(advantage_sim) 


data_lanc %>%
  filter(sim_id == '2023083000005017832')

data_joined %>%
  filter(sim_id == '2023083000005017832')

datatest <- data_unit %>%
  filter(sim_id == '2023083000005017832')

View(ev_data1 %>%filter(disc > 0.1 & u < 0.6))

ggplot(ev_data1 %>% filter) +
  geom_point(aes(x = u, y = disc))



data_util_test <- data_unit %>%
  group_by(sim_id) %>%
  filter(turn == max(turn)) %>%
  filter(turn >12) %>%
  group_by(sim_id,type) %>%
  summarise(num = n(), total_str = sum(str)) %>%
  pivot_wider(names_from = type, values_from = c(num,total_str)) %>%
  mutate(outcome = case_when(
    num_e <= 1 & num_f > 1 ~ 'w',
    num_f <= 1 & num_e > 1 ~ 'l',
    T ~ 'd'
  ))


### P_f vs utility

ggplot(ev_data) +
  geom_point(aes(x = mean, y = p_f)) +
  labs(
    title = 'Relationship Between Strategic Utility and Failure Probability',
    subtitle = 'Points represent a decision set',
    x = "E(u)",
    y = "P(F)"
  )

ggsave('images/outcome_util_pf.jpeg',width = 8, height = 5, dpi = 320)  
  
  

### Overall Utility

ev_data1  %>%
  ggplot() +
  geom_histogram(aes(x = u)) +
  labs(
    title = "Histogram Utility Distribution of Outcomes",
    x = "U",
    y = "Count"
  )

ggsave('images/outcome_hist.jpeg',width = 8, height = 5, dpi = 320)  



#### Opponent plan 

ev_data_opp <- data_joined %>%
  #filter(eny_obj == "Full") %>%
  group_by(key_terrain, mobilize, posture, modernize, eny_obj) %>%
  mutate(f = ifelse(outcome == 'l',1,0)) %>%
  summarise(mean = mean(u, na.rm = T),sd = sd(u, na.rm= TRUE),sdp = mean + sd, sdm = mean-sd,
            num = n(),f_total = sum(f), p_f = f_total/num) %>%
  arrange(p_f) %>%
  #ungroup() %>%
  group_by(eny_obj) %>%
  mutate(rowval = row_number())

ev_data_opp %>%
  filter(p_f == min(p_f))

ggplot(ev_data_opp,aes(x = rowval)) +
  geom_bar(aes(y = p_f, fill = mobilize), stat = 'identity') +
  #coord_flip() +
  ggsci::scale_fill_igv() +
  theme(
    axis.text.x = element_blank(),
    axis.ticks.x = element_blank()
  ) +
  labs(
    x = "Decision Set (Unlabeled)",
    y = "P(F)",
    title = "Probabilities of Failure Given Strategic Decisions",
    subtitle = "Of the strategic decisions, Modernization is the most impactful",
    fill = "Modernization"
  ) +
  facet_wrap(~eny_obj)


ev_data_opp %>%
  top_n(5,-p_f) %>%
  arrange(eny_obj,p_f)

ev_data %>%
  top_n(5,-p_f)


### Bootstrapping distributions
propdf <- data.frame()
for(i in seq(0,100, by = 5)) {
  for(j in seq(i,100,by = 5)){
    propdf <- rbind(propdf,data.frame(Border = i,Full = 100-j,Single = 100-i-(100-j)))
  }
}
propdf <- propdf +1
str(propdf[1,])

as.vector(t(propdf[1,]))

n_samples <- 500

strata_var <- 'eny_obj'

str(c(A = 30, B = 30, C = 30))

library(sampling)

strata(data_joined %>% arrange(eny_obj), stratanames = strata_var, size = as.vector(t(propdf[1,])),method = 'srswr')

dfout <- data.frame()

for(moddec in c('Modernize','Current')){
  for(posdec in c('Forward','Dispersed')) {
    for(mobdec in c('None','Partial','Full')){
      for(ktdec in c('All','Most','Rear')){
        t1 <- Sys.time()
        
        dftemp <- data_joined %>%
          filter(modernize == moddec, posture == posdec, mobilize == mobdec,key_terrain==ktdec) %>%
          ungroup() %>%
          arrange(eny_obj)
        
        for(i in 1:nrow(propdf)){
          
          
          dftemp_prop <- data.frame()
          dftemp1 <- data.frame()
          
          for(j in 1:n_samples){
            
            stratavar <- strata(dftemp, 
                                stratanames = strata_var,
                                size = as.vector(t(propdf[i,])),method = 'srswr')
            #print(stratavar)
            dftemp2 <- dftemp[stratavar$ID_unit,]
            #print(dftemp2)
            dftemp2$group <- j
            dftemp1 <- rbind(dftemp1,dftemp2) 

            
          }
          #print(dftemp1)
          dftemp1 <- dftemp1 %>%
            group_by(group) %>%
            #group_by(key_terrain, mobilize, posture, modernize) %>%
            #ungroup() %>%
            mutate(f = ifelse(outcome == 'l',1,0)) %>%
            summarise(mean = mean(u, na.rm = T),sd = sd(u, na.rm= TRUE),sdp = mean + sd, sdm = mean-sd,
                      num = n(),f_total = sum(f), p_f = f_total/num)
          
          #print(dftemp1)
          #dftemp_prop <- rbind(dftemp_prop,dftemp1)
          
          dftemp1 <- dftemp1 %>%
            ungroup() %>%
            summarise(sd = sd(mean),mean = mean(mean),f_total_mean = mean(f_total),sd_p_f = sd(p_f),p_f = mean(p_f))
          #print(dftemp1)
          dftemp1 <- cbind(data.frame(modernize = moddec, posture = posdec,
                                      mobilize = mobdec,key_terrain=ktdec),
                           dftemp1, propdf[i,])
          

          #print(dftemp1)
          dfout <- rbind(dfout,dftemp1)
        }
        
        

        print(nrow(dftemp))
        print(t1 - Sys.time())
      }
    }
  }

  
}



# test <- data_joined %>%
#   filter(modernize == 'Modernize', posture == 'Dispersed', mobilize == 'Partial',key_terrain=='All') %>%
#   ungroup()
# 
# 
# stratavartest <- strata(test %>% arrange(eny_obj), 
#                     stratanames = strata_var,
#                     size = c(1,5001,1),method = 'srswr') 
# 
# test <- test %>%
#   arrange(eny_obj)
# 
# test1 <- test[stratavartest$ID_unit,] %>%
#   mutate(f = ifelse(outcome == 'l',1,0)) %>%
#   summarise(mean = mean(u, na.rm = T),sd = sd(u, na.rm= TRUE),sdp = mean + sd, sdm = mean-sd,
#             num = n(),f_total = sum(f), p_f = f_total/num)

# test1

dfout1 <- dfout %>%
  mutate(Border = Border/100, Full = Full/100, Single = Single/100) %>%
  mutate(probs = paste(Border,Full,Single, sep = ','),
         dec = paste(modernize,mobilize,posture,key_terrain,sep = ',')) %>%
  group_by(probs) %>%
  filter(p_f == min(p_f))

ggplot(dfout1) +
  geom_tile(aes(x = Border, y = Single, fill = dec))



ev_data_boot <- data_joined %>%
  #filter(eny_obj != "Full") %>%
  group_by(key_terrain, mobilize, posture, modernize) %>%
  sample()



#### VOI for unit strength

#rtri(.05,.2,.15)

library(EnvStats)

low_valm <- qtri(1/3,.075,.175,.15)
hi_valm <- qtri(2/3,.075,.175,.15)
low_valc <- qtri(1/3,.075,.15,.125)
hi_valc <- qtri(2/3,.075,.15,.125)

lanc_single <- data_lanc %>%
  filter(shooter_side == 'f', shooter_class == 'arm',
         target_side == 'e', target_class == 'inf') %>%
  select(sim_id,mod)

ev_data_str <- data_joined %>%
  left_join(lanc_single) %>%
  #filter(eny_obj != "Full") %>%
  mutate(cval = case_when(
    modernize == "Current" & mod < low_valc ~ 'low',
    modernize == "Current" & mod >= low_valc & mod < hi_valc ~ 'mid',
    modernize =="Current" & mod >= hi_valc ~ 'hi',
    modernize == "Modernize" & mod < low_valm ~ 'low',
    modernize == "Modernize" & mod >= low_valm & mod < hi_valm ~ 'mid',
    modernize == "Modernize" & mod >= hi_valm ~ 'hi'
  )) %>%
  group_by(key_terrain, mobilize, posture, modernize, cval) %>%
  mutate(f = ifelse(outcome == 'l',1,0))


### Add in bootstrap sampling
prop_obj <- c(Border = 50, Full = 10, Single = 40)
n_samples <- 500

df_test_out <- data.frame()

for(moddec in c('Modernize','Current')){
  for(posdec in c('Forward','Dispersed')) {
    for(mobdec in c('None','Partial','Full')){
      for(ktdec in c('All','Most','Rear')){
        t1 <- Sys.time()
        
        dftemp <- ev_data_str %>%
          filter(modernize == moddec, posture == posdec, mobilize == mobdec,key_terrain==ktdec) %>%
          ungroup() %>%
          arrange(eny_obj)
        
        #for(i in 1:nrow(propdf)){
          
          
          dftemp_prop <- data.frame()
          dftemp1 <- data.frame()
          
          for(j in 1:n_samples){
            
            stratavar <- strata(dftemp, 
                                stratanames = strata_var,
                                size = prop_obj,method = 'srswr')
            #print(stratavar)
            dftemp2 <- dftemp[stratavar$ID_unit,]
            #print(dftemp2)
            dftemp2$group <- j
            dftemp1 <- rbind(dftemp1,dftemp2) 
            
            
          }
          #print(dftemp1)
          dftemp1 <- dftemp1 %>%
            group_by(cval) %>%
            #group_by(key_terrain, mobilize, posture, modernize) %>%
            #ungroup() %>%
            #mutate(f = ifelse(outcome == 'l',1,0)) %>%
            summarise(mean = mean(u, na.rm = T),sd = sd(u, na.rm= TRUE),sdp = mean + sd, sdm = mean-sd,
                      num = n(),f_total = sum(f), p_f = f_total/num) %>%
            mutate(p_inv = 0.6) %>%
            mutate(p_inv = ifelse(moddec == "Modernize", p_inv + .15,p_inv)) %>%
            mutate(p_inv = ifelse(posdec == "Forward" , p_inv + .05,p_inv)) %>%
            mutate(p_inv = ifelse(mobdec == "Full" , p_inv + 0.1,p_inv)) %>%
            mutate(p_inv = ifelse(mobdec == "Partial" , p_inv + 0.05,p_inv)) %>%
            mutate(p_f_new = p_f * p_inv) %>%
            mutate(rowval = row_number())
          
          #print(dftemp1)
          #dftemp_prop <- rbind(dftemp_prop,dftemp1)
          
          # dftemp1 <- dftemp1 %>%
          #   ungroup() %>%
          #   summarise(sd = sd(mean),mean = mean(mean),f_total_mean = mean(f_total),sd_p_f = sd(p_f),p_f = mean(p_f))
          #print(dftemp1)
          dftemp1 <- cbind(data.frame(modernize = moddec, posture = posdec,
                                      mobilize = mobdec,key_terrain=ktdec),
                           dftemp1)
          
          
          #print(dftemp1)
          df_test_out <- rbind(df_test_out,dftemp1)
        #}
        
        
        
        print(nrow(dftemp))
        print(t1 - Sys.time())
      }
    }
  }
  
  
}


# summary(factor(ev_data_str$cval))
# 
# ev_data_str <- ev_data_str %>%
#   summarise(mean = mean(u, na.rm = T),sd = sd(u, na.rm= TRUE),sdp = mean + sd, sdm = mean-sd,
#             num = n(),f_total = sum(f), p_f = f_total/num) %>%
#   arrange(p_f) %>%
#   ungroup() %>%
#   mutate(rowval = row_number())

  
ev_data_bs2 <- ev_data_bs1 %>%
  filter(outcome == 'l')

ev_data_str2 <- df_test_out %>%
  group_by(cval,modernize) %>%
  filter(p_f_new == min(p_f_new))


ggplot(ev_data_str2) +
  geom_bar(aes(x = modernize,y = (1/3)*p_f_new, fill = cval), stat = 'identity') +
  geom_bar(data = ev_data_bs %>% group_by(modernize) %>%
             filter(p_f_new == min(p_f_new)), aes(x = c('Modernize No info','Current No Info'), y = p_f_new), stat = 'identity') +
  labs(
    title = "Reduction in Failure Probability From Effectiveness Information",
    subtitle = "With knowledge of fighting capability, more optimal decisions can be made.",
    y = "P(F)",
    x = "Modernization Decision",
    fill = "Effectiveness"
  )

ggsave('images/outcome_sense.jpeg',width = 8, height = 5, dpi = 320)  


ev_data_str3 <- df_test_out %>%
  group_by(cval,modernize) %>%
  filter(mean == max(mean))

ev_data_str3 %>%
  group_by(modernize) %>%
  summarise(sum((1/3)*mean))

ggplot(ev_data_str3) +
  geom_bar(aes(x = modernize,y = (1/3)*mean, fill = cval), stat = 'identity') +
  geom_bar(data = ev_data_bs %>% group_by(modernize) %>%
             filter(mean == max(mean)), aes(x = c('Modernize No info','Current No Info'), y = mean), stat = 'identity')

ggplot(df_test_out) +
  geom_point(aes(x = mean, y = p_f_new, color = cval))


# You can use acceptance rejection testing to sample combat str


ptri(ev_data_str[sample(nrow(ev_data_str),1),]$mod,.075,.175,.15)

data_lanc_wide <- data_lanc %>%
  mutate(names = paste(shooter_side,shooter_class,target_side,target_class,sep = '_')) %>%
  select(mod,names,sim_id) %>%
  pivot_wider(id_cols = sim_id, values_from = mod, names_from = names)
