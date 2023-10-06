library(tidyverse)

# The extraction function
extract_between_last_underscore_and_dot <- function(str) {
  str_extract(str, "(?<=_)[^_]+(?=\\.)")
}

meta_files <- paste0('sherlock_single/meta/',list.files('sherlock_single/meta/'))
outcome_files <- paste0('sherlock_single/outcome/',list.files('sherlock_single/outcome/'))
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

write_csv(data_lanc,'sherlock_single/processed/data_lanc.csv')
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

write_csv(data_main,'sherlock_single/processed/data_main.csv')

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

write_csv(data_rein,'sherlock_single/processed/data_rein.csv')

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

write_csv(data_terr,'sherlock_single/processed/data_terr.csv')

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
write_csv(data_unit,'sherlock_single/processed/data_unit.csv')




### Stop here
data_terr <- read_csv('sherlock_single/processed/data_terr.csv', 
                      col_types = cols(sim_id = col_character()))
data_unit <- read_csv('sherlock_single/processed/data_unit.csv', 
                      col_types = cols(sim_id = col_character()))
data_rein <- read_csv('sherlock_single/processed/data_rein.csv', 
                      col_types = cols(sim_id = col_character()))
data_main <- read_csv('sherlock_single/processed/data_main.csv', 
                      col_types = cols(sim_id = col_character()))
data_lanc <- read_csv('sherlock_single/processed/data_lanc.csv', 
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



terr_wt <- .25
unit_wt <- 1-terr_wt



data_joined <- data_unit1_f %>%
  left_join(data_terr1a, by = 'sim_id', suffix = c('_unit','_terr')) %>%
  mutate(u = terr_wt * u_terr + unit_wt * u_unit, x = terr_wt*x_terr + unit_wt*x_unit) %>%
  left_join(data_main_wide) %>%
  left_join(adv_lanc_sum) %>%
  left_join(data_unit_turns) %>%
  left_join(data_util_test) %>%
  filter(num_turns > 10) %>%
  mutate(grp = paste(mobilize,modernize,posture,key_terrain))


z <- 1.96


pf_time <- data_joined %>%
  select(sim_id,outcome) %>%
  mutate(alpha = 1, beta = 1) %>%
  mutate(alpha = ifelse(outcome == 'l',1,0),beta = ifelse(outcome != 'l',1,0)) %>%
  ungroup() %>%
  #sample_frac(size = 1) %>%
  mutate(sim_num = row_number(), alpha = cumsum(alpha)+1, beta = cumsum(beta)+1) %>%
  mutate(lower = qbeta(.025,alpha,beta), upper = qbeta(.975,alpha,beta), expval = alpha/(alpha+beta))
         


 
#          
#          
#          , pct_est = total_f/sim_num,
#          highc = pct_est + z*(sqrt((pct_est*(1-pct_est))/sim_num)),
#          lowc = pct_est - z*sqrt((pct_est*(1-pct_est))/sim_num),
#          range = highc-lowc)
# 
# 
# .2+ z * (sqrt((.2*.8)/10000))
# 
# ((z* sqrt(.2*.8))/.005)^2



tail(pf_time)

ggplot(pf_time) +
  # Add color aesthetic to geom_path
  geom_path(aes(x = sim_num, y = expval, color = "Expected Value")) +
  # Add fill aesthetic to geom_ribbon
  geom_ribbon(aes(x = sim_num, ymin = lower, ymax = upper, fill = "95% CI"), alpha = .5) +
  labs(
    x = "Number of Simulations",
    y = "P(F)"
  ) +
  # Specify legend titles and customize its appearance
  scale_color_manual(name = "", values = c("Expected Value" = "black")) +
  scale_fill_manual(name = "", values = c("95% CI" = "black"))

ggsave('images/sim_conv.jpeg',height = 4, width = 6, dpi = 320)

df <-data.frame()

for(i in seq(0,1, by = .001)) {
  
  pval <- qbeta(i,156,613)
  
  df <- rbind(df, data.frame(x = i, y = pval))
  
}


ggplot(df) + 
  geom_path(aes(x = x, y = y))
