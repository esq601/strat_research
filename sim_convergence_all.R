
pf_time <- data_joined %>%
  select(sim_id,outcome,grp,modernize,mobilize,posture,key_terrain) %>%
  ungroup() %>%
  #sample_frac(size = 1) %>%
  group_by(grp) %>%
  #arrange(factor(outcome, levels = c('l','w','d'))) %>% #lol
  mutate(alpha = 1, beta = 1) %>%
  mutate(alpha = ifelse(outcome == 'l',1,0),beta = ifelse(outcome != 'l',1,0)) %>%
  
  mutate(sim_num = row_number(), alpha = cumsum(alpha)+1, beta = cumsum(beta)+1) %>%
  mutate(lower = qbeta(.025,alpha,beta), upper = qbeta(.975,alpha,beta), expval = alpha/(alpha+beta))



ggplot(pf_time) +
  # Add color aesthetic to geom_path
  geom_line(aes(x = sim_num, y = expval, group = grp,color = "Expected Value")) +
  # Add fill aesthetic to geom_ribbon
  geom_ribbon(aes(x = sim_num, ymin = lower,group = grp, ymax = upper, fill = "95% CI"), alpha = 1/3) +
  labs(
    x = "Number of Simulations",
    y = latex2exp::TeX("P(F | Invasion, D)")
  ) +
  # Specify legend titles and customize its appearance
  scale_color_manual(name = "", values = c("Expected Value" = "black")) +
  scale_fill_manual(name = "", values = c("95% CI" = "black")) +
  #expand_limits( y = range(c(min(pf_time$lower), max(pf_time$upper)))) +
  #xlim(3, 8) +
  #ylim(.15, .5)+
  facet_wrap(~grp, ncol = 4) +
  theme(
    strip.text = element_blank(),
    legend.position = 'top'
  )

ggsave('images/est_conv.jpeg',height = 10, width = 8, dpi = 320)


ggplot(pf_time) +
  # Add color aesthetic to geom_path
  geom_line(aes(x = sim_num, y = expval, group = grp,color = modernize)) +
  # Add fill aesthetic to geom_ribbon
  geom_ribbon(aes(x = sim_num, ymin = lower,group = grp, ymax = upper, fill = modernize), alpha = .25) +
  labs(
    x = "Number of Simulations",
    y = "P(F)"
  ) +
  facet_wrap(~posture)
