library(ggplot2)
library(purrr)
library(ggtern)
library(MCMCpack)

head(data_unit)

data_bayes <- data_unit %>%
  filter(type == 'f') %>%
  mutate(s_t = paste(s,turn,sep = "_")) %>%
  left_join(data_joined %>% dplyr::select(sim_id,outcome)) %>%
  group_by(s_t,outcome) %>%
  summarise(num = n()) 


data_bayes1 <- data_bayes%>%
  filter(!is.na(outcome)) %>%
  pivot_wider(names_from = outcome, values_from = num) %>%
  mutate(alpha = sum(1,l,na.rm=TRUE), beta = sum(1, w, d, na.rm= TRUE))

ddirichlet(1, c(4,4,2))
?ddirichlet

hist(data_bayes$num)




#set.seed(123) # For reproducibility
sampled_data <- data_bayes1[sample(nrow(data_bayes1), 2), ]
sampled_data
# Define a sequence to generate densities for the beta distributions
x_seq <- seq(0, 1, by = 0.01)

# Create a function to generate data for a beta distribution
generate_beta_data <- function(alpha, beta) {
  tibble::tibble(
    x = x_seq,
    y = pbeta(x_seq, alpha, beta),
    alpha = alpha,
    beta = beta
  )
}

# Use purrr::map2_df to apply the function across alpha and beta values
plot_data <- purrr::map2_df(sampled_data$alpha, sampled_data$beta, generate_beta_data)

# Plot the distributions
ggplot(plot_data, aes(x = x, y = y, color = factor(paste("alpha=", alpha, ", beta=", beta)))) +
  geom_line() +
  labs(title = "Failure Probability Distributions of Sampled State Values",
       subtitle = "Alpha corresponds to Failure, Beta otherwise",
       x = "x",
       y = "P(X < x)",
       color = "Parameters") +
  theme_minimal()


ggsave('images/pf_sample.jpeg',width = 7, height = 4, dpi = 320)
p_f_t <- 0.75


data_bayes2 <- data_bayes1 %>%
  mutate(p_f = 1-pbeta(p_f_t,alpha,beta)) %>%
  separate(s_t, into = c("s", "t"), sep = "_(?=[^_]+_)", extra = "merge")
  #mutate(t = as.numeric(t))

  
  hist(data_bayes2$p_f)


source("hex_setup.R")


# Plot of the field

turn <- unique(data_bayes2$t)[16]

turn
hexloc <- hexdf2 %>%
  #select(pos,x_pos,y_pos) %>%
  distinct() %>%
  left_join(data_bayes2 %>% filter(t == turn) %>% select(s,p_f), by = c('pos' = 's'))

ggplot(hexloc, aes (x=x_h, y = y_h)) +
  geom_polygon(color = 'black',aes(group = pos,fill = p_f)) +
  geom_point(data = cities, aes(x = x_pos, y = y_pos), size = 4) +
  geom_text(data = hexloc, aes(x = x_pos, y = y_pos, label = pos)) +
  coord_equal() +
  scale_fill_continuous(limits = c(0,1)) +
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


library(MCMCpack)

X1 <- rdirichlet(100, c(5, 5, 10))

a.mat <- cbind(1:10, 5, 10:1)
a.mat
X2 <- rdirichlet(10, a.mat)
# note how the probabilities in the first an last column relate to a.mat
round(X2, 2)

ddirichlet(X1, c(5, 5, 10))
ddirichlet(X2, a.mat)

ddirichlet(X2[1:3,], c(1, 2, -1))
ddirichlet(X2[1:3,], c(1, 2, -1), sum.up = TRUE)



data_bayes_dir <- data_bayes%>%
  filter(!is.na(outcome)) %>%
  pivot_wider(names_from = outcome, values_from = num) %>%
  mutate(alpha_l = sum(1,l,na.rm=TRUE), alpha_d = sum(1, d, na.rm= TRUE), alpha_w = sum(1, w, na.rm= TRUE))


# Create a sequence from 0 to 1 in increments of 0.01
seq_vals <- seq(0, 1, by = 0.1)

# Find all combinations
combinations <- expand.grid(loss = seq_vals, draw = seq_vals, win = seq_vals)

# Filter combinations where the sum of the values equals 1
valid_combinations <- subset(combinations, loss + draw + win == 1 & loss < 1 & loss > 0 & draw <1 & draw >0 &win <1 & win >0)
valid_combinations <- subset(combinations, loss + draw + win == 1 )

# Convert to matrix
matrix_result <- as.data.frame(valid_combinations)

generate_matrix <- function(increment = 0.01) {
  seq_vals <- seq(0, 1, by = increment)
  matrix_result <- data.frame(x=numeric(0), y=numeric(0), z=numeric(0))
  
  for (i in seq_vals) {
    for (j in seq_vals) {
      if ((i + j) <= 1) {
        k <- 1 - (i + j)
        matrix_result <- rbind(matrix_result, c(i, j, k))
      }
    }
  }
  
  colnames(matrix_result) <- c('loss','draw','win')
  return(matrix_result)
}

matrix_result <- generate_matrix()



#set.seed(123)  # For reproducibility
sampled_row <- data_bayes_dir[sample(nrow(data_bayes_dir), 1), ]

sampled_row

alpha_n <- sum(sampled_row$alpha_l,sampled_row$alpha_w,sampled_row$alpha_d)
cov_wl <- -(sampled_row$alpha_l*sampled_row$alpha_w)/(alpha_n^2* (alpha_n+1))
cov_wl
df <- as.data.frame(matrix_result)



df$probability <- apply(df, 1, function(row) {
  ddirichlet(as.numeric(row), c(sampled_row$alpha_l, sampled_row$alpha_d, sampled_row$alpha_w))
})


1- pbeta(.15,sampled_row$alpha_l,alpha_n-sampled_row$alpha_l)

sum(sum(df[df$loss >= .15,]$probability,na.rm = T) / sum(df$probability,na.rm = T))



ggtern(data = df, aes(x = loss, y = draw, z = win, color = probability)) +
  geom_point(size = 3,shape =17) +
  scale_color_viridis_c() +
  labs(title = "Ternary Diagram with Dirichlet Probabilities",
       subtitle = "State with 147 Wins, 265 Draws, and 278 Losses",
       x = "loss", y = "draw", z = "win") +
  theme_minimal()
?ggtern
?coord_tern

loss_p <- .5
win_p <- .25

data_bayes_dir1 <- data_bayes_dir %>%
  filter(sum(w,d,l,na.rm=TRUE)> 10) %>%
  mutate(p_pf_thr = 1-pbeta(loss_p,alpha_l, alpha_w + alpha_d),
         p_pw_thr = 1-pbeta(win_p,alpha_w, alpha_l + alpha_d),
         p_pf_opp = 1-pbeta(win_p,alpha_l, alpha_w + alpha_d),
         p_pw_opp = 1-pbeta(loss_p,alpha_w, alpha_l + alpha_d)) %>%
  mutate(jp_thr = p_pf_thr*p_pw_thr,jp_opp = p_pf_opp*p_pw_opp) %>%
  mutate(imp = jp_thr-jp_opp) %>%
  separate(s_t, into = c("s", "t"), sep = "_", extra = "merge") %>%
  mutate(t = as.numeric(t))


ggplot(data_bayes_dir1 %>% filter(s == '111106')) +
  geom_line(aes(x = t, y = jp_thr, color = 'Threat')) +
  geom_line(aes(x = t, y = jp_opp, color = 'Opportunity')) +
  scale_color_manual(labels = c("Opportunity",'Threat'), values = c('darkblue','red')) +
  theme_minimal() +
  scale_x_continuous(limits = c(0,24)) +
  labs(
    title = "Threats and Opportunities",
    subtitle = "For Location 111106",
    y = "Joint Probability",
    x = "Time Period",
    color = "Type"
  )

ggsave('images/threatopp.jpeg',width = 6, height = 4, dpi = 320)

ggplot(data_bayes_dir1 %>% filter(s == '061111')) +
  geom_line(aes(x = t, y = jp_thr, color = 'Threat')) +
  geom_line(aes(x = t, y = jp_opp, color = 'Opportunity')) +
  scale_color_manual(labels = c("Opportunity",'Threat'), values = c('darkblue','red')) +
  theme_minimal() +
  scale_x_continuous(limits = c(0,24)) +
  labs(
    title = "Threats and Opportunities",
    subtitle = "For Location 061111",
    y = "Joint Probability",
    x = "Time Period",
    color = "Type"
  )


ggsave('images/threatopp1.jpeg',width = 6, height = 4, dpi = 320)

ggplot(data_bayes_dir1 %>% filter(s == '141507')) +
  geom_line(aes(x = t, y = jp_thr, color = 'Threat')) +
  geom_line(aes(x = t, y = jp_opp, color = 'Opportunity')) +
  scale_color_manual(labels = c("Opportunity",'Threat'), values = c('darkblue','red')) +
  theme_minimal() +
  scale_x_continuous(limits = c(0,24)) +
  labs(
    title = "Threats and Opportunities",
    subtitle = "For Location 141507",
    y = "Joint Probability",
    x = "Time Period",
    color = "Type"
  )

ggsave('images/threatopp2.jpeg',width = 6, height = 4, dpi = 320)



turn <- 14

turn
hexloc <- hexdf2 %>%
  #select(pos,x_pos,y_pos) %>%
  distinct() %>%
  left_join(data_bayes_dir1 %>% dplyr::select(s,t,jp_thr,jp_opp) %>% group_by(s) %>% 
               filter(jp_thr == max(jp_thr)), by = c('pos' = 's')) %>%
  mutate(jp_thr = ifelse(is.na(jp_thr)==TRUE,0,jp_thr),
         jp_opp = ifelse(is.na(jp_opp)==TRUE,0,jp_opp))

ggplot(hexloc, aes (x=x_h, y = y_h)) +
  geom_polygon(color = 'black',aes(group = pos,fill = jp_thr)) +
  #geom_polygon(color = 'black',aes(group = pos,fill = jp_thr),alpha = .5) +
  #geom_point(data = cities, aes(x = x_pos, y = y_pos), size = 4) +
  geom_text(data = hexloc, aes(x = x_pos, y = y_pos, label = pos)) +
  coord_equal() +
  scale_fill_distiller(type = 'seq', direction = 1, palette = 3) +
  #scale_fill_continuous(limits = c(0,1)) +
  theme_void() +
  labs(
    title = "Locations with Threat Probability",
    fill = "max P(Threat)"
  ) +
  theme(
    plot.title = element_text(hjust=.5),
    plot.subtitle = element_text(hjust = 0.5)
  ) 

ggsave('images/max_threat.jpeg', width = 12, height = 10, dpi = 320)

ggplot(hexloc, aes (x=x_h, y = y_h)) +
  geom_polygon(color = 'black',aes(group = pos,fill = jp_thr)) +
  #geom_polygon(color = 'black',aes(group = pos,fill = jp_thr),alpha = .5) +
  geom_point(data = cities, aes(x = x_pos, y = y_pos), size = 4) +
  geom_text(data = hexloc, aes(x = x_pos, y = y_pos, label = pos)) +
  coord_equal() +
  scale_fill_distiller(type = 'seq', direction = 1,palette = 3) +
  #scale_fill_continuous(limits = c(0,1)) +
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


ggsave('images/tern_plot3.jpeg', width = 6, height =6 ,dpi = 320)

ggplot(data_bayes_dir1) +
  geom_point(aes(x = p_pf_thr, y = p_pw_thr))


ggplot(data_bayes_dir1) +
  geom_point(aes(x = jp_thr, y = jp_opp)) +
  theme_minimal() +
  labs(
    title = 'Tactical Threats and Opportunities',
    y = "P(Opportunity)",
    x = "P(Threat)"
  )


ggsave('images/tact_to.jpeg',width = 8, height = 5, dpi = 320)  
data_bayes_dir1 %>%
  filter(jp_thr >=.2 & jp_opp >=.2) %>%
  nrow()

cor(data_bayes_dir1$jp_thr,data_bayes_dir1$jp_opp)
test <- data_bayes_dir1 %>%
  filter(sum(w,d,l,na.rm=TRUE) > 10)
