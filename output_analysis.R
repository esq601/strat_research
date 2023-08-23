library(tidyverse)
library(data.table)


files <- list.files('validation/')

dtfull <- data.table()
for(file in files) {
  suppressMessages({
  temp <- read_csv(paste0('validation/',file))
  })
  
  temp$file <- file
  dtfull <- rbind(dtfull,temp)
  
}


dtfull_update <- dtfull %>%
  as.data.frame() %>%
  mutate(algo = str_sub(file,1,15)) %>%
  mutate(team= str_sub(id,1,3))


dtsum <- dtfull_update %>%
  group_by(algo,file,team,turn) %>%
  summarise(total_str = sum(str))


dtupdate
ggplot(dtsum) +
  geom_boxplot(aes(x = turn, y = total_str, color = team)) + 
  facet_wrap(~algo)

dtsum1 <- dtsum %>%
  group_by(file) %>%
  filter(turn == max(turn))

dtsum2 <- dtsum1 %>%
  filter(algo %in% c("process_5k_2pt5","process_upda3k_")) %>%
  filter(total_str == max(total_str)) %>%
  group_by(algo,team) %>%
  summarise(num = n()) %>%
  group_by(algo) %>%
  mutate(pct = num /sum(num))

ggplot(dtsum2) +
  geom_line(aes(x = algo, y = pct, color = team,group = team), size = 1) +
  geom_point(aes(x = algo, y = pct, color = team,group = team), size = 2) +
  labs(x = "Algorithm",
       y = "Winning %",
       color = "Side") +
  scale_y_continuous(limits = c(0,1)) +
  scale_x_discrete(labels = c("Old Version","Current Version")) +
  scale_color_discrete(labels = c("Red","Blue"))

ggsave('output_analysis.jpeg',width = 6, height = 4, dpi = 320)

ggplot(dtsum2) +
  geom_boxplot(aes(x = turn, y = total_str, color = team)) + 
  facet_wrap(~algo)




dtupdate <- dtfull_update %>%
  filter(str_detect(algo,'update')) %>%
  group_by(algo,file,team,turn) %>%
  summarise(total_str = sum(str))

ggplot(dtupdate) +
  geom_path(aes(x = turn, y = total_str, color = team)) +
  facet_wrap(~file)
