
# chase flights detection algorithm
options(digits.secs = 3) # change options to see milliseconds in DateTime 

library(tidyverse)
library(ggpubr)
# library(lme4)
# library(brms)
# library(bayesplot)
library(broom.mixed)
library(MCMCglmm)

folder <- "analysis/data/"

# File too large to upload to github, so needs to be generated first with script in load.R
allnights <- read.csv2(file = paste0(folder, "EventData.csv"),
                                header = TRUE, sep = ";", dec = ".", na.strings = "NA") 

dur_epsilon <- 200 # 200 ms as a definition of a the upper threshold for the duration of a TR detection caused by chasing event

# are LS ever preceded by Reader events?
# very rarely and never looks like a chase event!!
# allnights_n <- allnights_n %>% 
#   group_by(night) %>% 
#   mutate(postreader = ifelse(lag(loc) == loc & str_detect(lag(unitLabel), "Reader") & str_detect(unitLabel, "LS"), TRUE, FALSE))

allnights_n <- allnights %>% 
  group_by(night) %>% 
  mutate(DateTime = as.POSIXct(DateTime, tz = "UTC"),
         n_event = 1:n(),
         chase = case_when(
           n_event < 3 ~ FALSE, # first two events cannot be chases by definition
           is.na(IdLabel) ~ FALSE, # not a chase if corrupted RFID number detected that cannot be assigned to any ind
           eventDuration >= dur_epsilon ~ FALSE, # not a chase if the duration is higher than the epsilon
           !str_detect(unitLabel, "Re") ~ FALSE, # not a chase if not a Reader event
           !str_detect(lag(unitLabel), "LS") ~ FALSE, # not a chase if previous event not an IR sensor event
           loc != lag(loc) | loc != lag(loc, 2) ~ FALSE, # not a chase if previous two events at different flower
           !str_detect(lag(unitLabel, 2), "Co") ~ FALSE,  # not a chase if next-to-last event prior was not a CondMod
           TRUE ~ TRUE # otherwise chase visit
           ),
         chased = ifelse(chase == TRUE, lag(IdLabel, 2), NA), 
         # dur_chase_seq = ifelse(chase == TRUE, 
         #                        lag(eventDuration, 2)/1000 -
         #                          as.numeric(DateTime - lag(DateTime, 2)), NA),
         patch = case_when(
           loc < 6 ~ 1L,
           loc > 5 ~ 2L,
           TRUE ~ NA_integer_),
         group = factor(group, levels = c("mixed1", "mixed2", "mixed3", "mixed4", "6m", "6f")),
         sex = factor(sex, levels = c("m", "f"))) %>%
  select(chase, chased, dur_chase_seq, everything())

# chased_sexes <- conditions %>%
#   distinct(IdLabel, sex) %>% 
#   rename(chased = IdLabel,
#          chased_sex = sex)
# 
# allnights_n <- allnights_n %>%
#   left_join(chased_sexes) %>% 
#   select(chase, chased, chased_sex, everything()) %>% 
#   ungroup()

phase_durs <- allnights_n %>% 
  filter(n_event == 1 | SystemMsg == "switch") %>% 
  group_by(night) %>% 
  summarise(hours_ph1 = round(max(DateTime) - min(DateTime), 1),
            hours_ph2 = 12 - hours_ph1) %>% 
  pivot_longer(-night, names_to = "phase", values_to = "phase_dur", names_prefix = "hours_ph") %>% 
  mutate(phase = as.integer(phase))

vis_summaries <- allnights_n %>% 
  filter(eventDuration > 300, choice == TRUE, !is.na(IdLabel)) %>% 
  group_by(night, group, group_night, IdLabel, phase, cond, patch, sex) %>% 
  summarise(n = n(), vol_consumed = sum(vol) / 1000) %>% 
  left_join(phase_durs) %>% 
  mutate(vis_hour = n/as.numeric(phase_dur),
         ml_hour = vol_consumed/as.numeric(phase_dur))

write.table(vis_summaries, file = paste0(folder, "VisitSummary.csv"), sep = ";", row.names = FALSE)

chase_summ <- allnights_n %>% 
  filter(cond == "test", !is.na(IdLabel)) %>% 
  group_by(group_night, IdLabel, group, sex, phase) %>% 
  summarise(n_detections = n(),
            n_chases = sum(chase),
            prop_chases = n_chases/n_detections)

chased_n <- allnights_n %>% 
  ungroup() %>%
  filter(cond == "test", !is.na(chased)) %>% 
  count(group_night, chased, group, phase) %>%
  rename(IdLabel = chased,
         n_chased = n) 

bat_info <- read.csv2(file = paste0(folder, "metadata/bats.csv"),
                                          header = TRUE, sep = ";", dec = ".", na.strings = "NA")

chase_summ <- chase_summ %>% 
  left_join(chased_n) %>%
  left_join(bat_info) %>% 
  replace_na(list(n_chased = 0)) %>% 
  mutate(prop_chased = n_chased / n_detections,
         phase = factor(phase), 
         sex = factor(sex))

write.table(chase_summ, file = paste0(folder, "ChaseSummary.csv"), sep = ";", row.names = FALSE)


M_follow1 <- glmer(cbind(n_chases, n_detections) ~ sex * phase + (1|group) + (1|IdLabel) + forearm, 
                   data = chase_summ, family = "binomial")
summary(M_follow1)
M_follow2 <- glmer(cbind(n_chased, n_detections) ~ sex * phase + (1|group) + (1|IdLabel) + forearm, 
                   data = chase_summ, family = "binomial")

summary(M_follow2)

plot(M_follow2)

# m1 <-
#   brm(data = chase_summ, family = binomial,
#       n_chases | trials(n_detections) ~ 1 + sex * phase +
#         (1 | IdLabel) + (1 | group),
#       prior = c(prior(normal(0, 10), class = Intercept),
#                 prior(normal(0, 2), class = b),
#                 prior(cauchy(0, 1), class = sd)),
#       sample_prior = "yes",
#       iter = 2000, warmup = 1000, chains = 4, cores = 6, thin = 3,
#       control = list(adapt_delta = 0.999, max_treedepth = 13),
#       seed = 12)
# summary(m1)
# 
# 
# posterior_summary(m1)
# 
# neff_ratio(m1) %>% 
#   mcmc_neff()  
# 
# 
# mcmc_intervals(m1, 
#                pars = vars(1:4), 
#                point_size = 1.75) + 
#   geom_vline(xintercept = 0) + 
#   theme_bw()
# 
# hypothesis(m1, "sexm:phase2 = 0", alpha = 0.01)
# 
# 
# brm_chases <- m1 %>% tidy()
# write.table(brm_chases, file = paste0(folder, "brmChases.csv"), sep = ";", row.names = FALSE)
# 
# #model summary
# t1 <- fixef(m1) %>% 
#   as_tibble() %>% 
#   mutate("Fixed effect" = c("Intercept", "Sex (male)", "Distributed condition", "Sex-distribution Interaction"), 
#          Estimate = round(Estimate, digits = 2), 
#          Q2.5 = round(Q2.5, digits = 2), 
#          Q97.5 = round(Q97.5, digits = 2)) %>% 
#   # renaming the credibility intervals column
#   mutate("95% Credibility intervals" = paste0("[", Q2.5, " , ", Q97.5, "]"))
# max_xvalue <- t1 %>% 
#   filter(`Fixed effect` != "Intercept") %>% 
#   select(Q97.5) %>% 
#   filter(Q97.5 == max(Q97.5)) %>% 
#   as.numeric() 
# max_xvalue <- max_xvalue + 0.3
# t1_estimates <- t1 %>% 
#   filter(`Fixed effect` != "Intercept") %>% 
#   rename(labels = `Fixed effect`) %>% 
#   select(labels, Estimate, `95% Credibility intervals`) %>% 
#   mutate(All = paste0(Estimate, " ", `95% Credibility intervals`)) %>% 
#   select(labels, All)
# # creating a plot of the slope coefficients 
# p3 <- mcmc_intervals(m1, 
#                      pars = vars(1:4), 
#                      prob_outer = 0.95, 
#                      point_size = 1.75) + 
#   xlim (-0.9, max_xvalue + 0.05) +
#   geom_vline(xintercept = 0) + 
#   scale_y_discrete(
#     labels = c("b_sexm" = "Sex",
#                "b_phase" = "Distributed condition", 
#                "b_sexm:phase" = "Sex - condition interaction"),
#     limits = c("b_Day:block", "b_block", "b_Day")) + 
#   geom_text(data = t1_estimates,
#             aes(x = max_xvalue, y = c(3, 2, 1), label = All, fontface ="bold"), size = 3)
# p3 
# 
# 
# m2 <-
#   brm(data = chase_summ, family = binomial,
#       n_chased | trials(n_detections) ~ 1 + sex * phase +
#         (1 | IdLabel) + (1 | group),
#       prior = c(prior(normal(0, 10), class = Intercept),
#                 prior(normal(0, 10), class = b),
#                 prior(cauchy(0, 1), class = sd)),
#       sample_prior = "yes",
#       iter = 2000, warmup = 1000, chains = 4, cores = 6, thin = 3,
#       control = list(adapt_delta = 0.999, max_treedepth = 12),
#       seed = 12)
# summary(m2)
# 
# brm_chased <- m2 %>% tidy()
# write.table(brm_chased, file = paste0(folder, "brmChased.csv"), sep = ";", row.names = FALSE)


# function for plotting autocorrelations from https://github.com/tmalsburg/MCMCglmm-intro
plot.acfs <- function(x) {
  n <- dim(x)[2]
  par(mfrow = c(ceiling(n/2), 2), mar = c(3, 2, 3, 0))
  for (i in 1:n) {
    acf(x[, i], lag.max = 100, main = colnames(x)[i])
    grid()
  }
}

mcmc_to_tibble <- function(object) { # extract the model summary into a tibble 
  nF <- object$Fixed$nfl
  nL <- object$Fixed$nll
  
  solutions <- cbind(colMeans(object$Sol[, 1:nF, drop = FALSE]), 
                     coda::HPDinterval(object$Sol[, 1:nF, drop = FALSE]), 
                     effectiveSize(object$Sol[, 1:nF, drop = FALSE]), 2 * 
                       pmax(0.5/dim(object$Sol)[1], 
                            pmin(colSums(object$Sol[, 1:nF, drop = FALSE] > 0)/dim(object$Sol)[1],
                                 1 - colSums(object$Sol[, 1:nF, drop = FALSE] > 0)/dim(object$Sol)[1])))
  if (nL > 0) {
    solutions <- rbind(solutions, cbind(colMeans(object$Lambda), 
                                        coda::HPDinterval(object$Lambda), effectiveSize(object$Lambda), 
                                        2 * pmax(0.5/dim(object$Lambda)[1],
                                                 pmin(colSums(object$Lambda > 0)/dim(object$Lambda)[1],
                                                      1 - colSums(object$Lambda > 0)/dim(object$Sol)[1]))))
  }
  colnames(solutions) <- c("estimate", "conf.low", 
                           "conf.high", "eff.samp", "pMCMC")
  solutions %>% 
    as.data.frame() %>% 
    rownames_to_column(var = "term") %>% 
    mutate(across(starts_with("est") | starts_with("conf"), ~round(., 2)))
}

prior.1 <- list(R = list(V = 1, nu = 0.002),
                G = list(G1 = list(V = diag(1), nu = 1, 
                                   alpha.mu = c(0), alpha.V = diag(1)*10000),
                         G2 = list(V = diag(1), nu = 1, 
                                   alpha.mu = c(0), alpha.V = diag(1)*10000)))

#prior.1a<-list(R = list(V=1, nu = 0.002), G = list(G1 = list(V = diag(2), nu = 0.002), 
#G2 = list(V = 1, nu = 0.002)))
set.seed(42) #allows you to replicate simulation
mcmc_chases <- MCMCglmm(cbind(n_chases, n_detections - n_chases) ~ sex * phase + weight, 
                     random = ~IdLabel + group,
                     data = as.data.frame(chase_summ), family = "multinomial2", pr = TRUE,
                     prior = prior.1, verbose = FALSE, saveX = TRUE, saveZ = TRUE,
                     nitt = 200E3, thin = 100, burnin = 100E3)
summary(mcmc_chases)


geweke.plot(mcmc_chases$VCV)
plot.acfs(mcmc_chases$VCV)
plot(mcmc_chases)

est_chases <- mcmc_to_tibble(mcmc_chases) %>% 
  mutate(Model = "Chasing")

set.seed(123)
mcmc_chased <- MCMCglmm(cbind(n_chased, n_detections - n_chased) ~ sex * phase + weight, 
                        random = ~IdLabel + group,
                        data = as.data.frame(chase_summ), family = "multinomial2", pr = TRUE,
                        prior = prior.1, verbose = FALSE, saveX = TRUE, saveZ = TRUE,
                        nitt = 200E3, thin = 100, burnin = 100E3)
summary(mcmc_chased)
geweke.plot(mcmc_chased$VCV)
plot(mcmc_chased)


est_chased <- mcmc_to_tibble(mcmc_chased) %>% 
  mutate(Model = "Being chased")
est_chases <- est_chases %>% 
  bind_rows(est_chased) %>% 
  mutate(CI = paste0("(", conf.low, ", ", conf.high, ")"),
         term = str_replace(term, "sexf", "sex (female)"),
         term = str_replace(term, "phase2", "condition (distributed)"))

write.table(est_chases, file = paste0(folder, "mcmcChases.csv"), sep = ";", row.names = FALSE)


# 
# rev_count <- 
#   vis_summaries %>% 
#   filter(cond == "test") %>% 
#   ungroup() %>% 
#   count(group, group_night) %>% 
#   group_by(group) %>% 
#   mutate(n = n():1) %>% 
#   rename(rev_night = n) # reverse count the nights to find the last two nights (different for each group)

# statuses <- chase_nectar %>%
#   ungroup() %>%
#   mutate(status = factor(case_when(
#     sex == "f" ~ "Females",
#     prop_chases > 0.003 & vol_hr > 0.5 ~ "Dominant males",
#     TRUE ~ "Subdominant males"
#   ), levels = c("Dominant males", "Females", "Subdominant males"))) %>%
#   select(IdLabel, status)
# 
# cons_contrasts <- vis_summaries %>%
#   left_join(rev_count) %>%
#   group_by(group, sex, IdLabel) %>% 
#   mutate(partition = case_when(
#     is.na(rev_night) ~ NA_character_,
#     group_night < 3  & group_night > 0 ~ "First two\n nights",
#     rev_night < 3 ~ "Last two\n nights",
#     TRUE ~ NA_character_
#   ))
# 
# cons_contrasts <- cons_contrasts %>% 
#   ungroup() %>%
#   filter(!is.na(partition)) %>% 
#   group_by(group, IdLabel, phase, partition, sex) %>%
#   summarise(vol_hr = sum(vol_consumed)/sum(phase_dur)) %>% 
#   left_join(statuses) %>% 
#   arrange(partition, IdLabel)
# 
# 
# stat.test <- cons_contrasts %>%
#   group_by(partition, phase) %>%
#   t_test(vol_hr ~ status) %>% 
#   adjust_pvalue(method = "holm") %>%
#   add_significance("p.adj") %>% 
#   add_xy_position(x = "partition", group = "status")
# stat.test 
# 
# ggboxplot(cons_contrasts, x = "partition", y = "vol_hr",
#           color = "status", line.color = "gray", line.size = 0.4,
#           palette = "jco",
#           add = "jitter",
#           facet.by = "phase") +
#   stat_pvalue_manual(
#     stat.test,  label = "p.adj.signif", tip.length = 0.02, hide.ns = TRUE
#   ) +
#   scale_y_continuous(expand = expansion(mult = c(0.05, 0.1))) +
#   labs(x = "", y = "Nectar intake [ml/h]", color = "")


vol_sd_time <- vis_summaries %>% 
  filter(group_night > 0) %>% 
  group_by(group, IdLabel, phase, sex, group_night) %>%
  summarise(vol_hr = sum(vol_consumed)/sum(as.numeric(phase_dur))) %>% 
  group_by(group, group_night, phase, sex) %>% 
  summarise(sd_consumed = sd(vol_hr)) %>% 
  mutate(group2 = paste(group, sex),
         phase = factor(phase))

write.table(vol_sd_time, file = paste0(folder, "IntakeTime.csv"), sep = ";", row.names = FALSE)


prior.2 <- list(R = list(V = 1, nu = 0.002),
                G = list(G1 = list(V = diag(2), nu = 0.002)))

#prior.1a<-list(R = list(V=1, nu = 0.002), G = list(G1 = list(V = diag(2), nu = 0.002), 
#G2 = list(V = 1, nu = 0.002)))
set.seed(815) #allows you to replicate simulation
mcmc_vol_time <- MCMCglmm(sd_consumed ~ sex + phase + group_night +
                            sex:phase + group_night:phase + sex:group_night, 
                        random = ~us(1 + group_night):group,
                        data = as.data.frame(vol_sd_time), family = "gaussian", pr = TRUE,
                        prior = prior.2, verbose = FALSE, saveX = TRUE, saveZ = TRUE,
                        nitt = 200E3, thin = 100, burnin = 100E3)
summary(mcmc_vol_time)

geweke.plot(mcmc_vol_time$VCV)
plot(mcmc_vol_time)

est_vol_time <- mcmc_to_tibble(mcmc_vol_time) %>% 
  mutate(CI = paste0("(", conf.low, ", ", conf.high, ")"),
         term = str_replace(term, "sexf", "sex (female)"),
         term = str_replace(term, "phase2", "condition (distributed)"),
         term = str_replace(term, "group_night", "night"))

write.table(est_vol_time, file = paste0(folder, "mcmcIntakeTime.csv"), sep = ";", row.names = FALSE)


mode_HPD <- function(mcmc) {
  round(c(posterior.mode(mcmc), HPDinterval(mcmc)), 2) %>% set_names(c("estimate", "conf.low", "conf.high"))
}

females_phase2 <- mcmc_vol_time$Sol[, "sexf"] + mcmc_vol_time$Sol[, "sexf:phase2"]
night_phase2 <- mcmc_vol_time$Sol[, "group_night"] + mcmc_vol_time$Sol[, "phase2:group_night"] 
sex_phase2 <- mcmc_vol_time$Sol[, "phase2"] + mcmc_vol_time$Sol[, "sexf:phase2"]
  
intake_interactions <- mode_HPD(females_phase2) %>% # for females, no difference between resource conditions
  rbind(mode_HPD(night_phase2)) %>%  # slope of group_night equals zero in phase 2 (distributed resource condition)
  rbind(mode_HPD(sex_phase2)) %>% # no difference between sexes in phase 2 (distributed resource condition)
  as_tibble() %>%
  mutate(effect = c("females_phase2", "night_phase2", "sex_phase2"))

write.table(intake_interactions, file = paste0(folder, "IntakeInteractions.csv"), sep = ";", row.names = FALSE)

# In males there is an inverse proportion between the frequency of initiating
# a chase event or being chased
chase_summ %>% 
  filter(sex == "m") %>%
  group_by(IdLabel) %>%
  mutate(prop_chases = sum(n_chases)/sum(n_detections),
         prop_chased = sum(n_chased)/sum(n_detections)) %>%
  ggplot(aes(prop_chases, prop_chased, color = group, fill = group)) +
  geom_point(size = 4) +
  scale_color_viridis_d(option = "turbo") +
  theme_serif() +
  xlim(c(0, 0.01)) +
  ylim(c(0, 0.007)) +
  labs(x = "Proportion chasing", y = "Proportion chased")


onlyrewarded <- allnights_n %>% 
  left_join(statuses) %>% # statuses calculation in Rmd file!
  ungroup() %>% 
  filter(vol > 0, cond == "test", !is.na(IdLabel)) %>% 
  select(group_night, IdLabel, status, group, DateTime, loc, phase)

write.table(onlyrewarded, 
            file = paste0(folder,"RewardedVisits.csv"), sep = ";", row.names = FALSE)




### for presentations:

cons_clumped <- cons_group1 %>%
  filter(phase == 1) %>%
  ggplot(aes(x = group_night, y = vol_consumed, shape = Individual, group = Individual)) +
  labs(x = "Night", title = "Clumped condition - one patch",
       y = "Nectar intake [mL]") +
  ylim(c(0, 18)) +
  scale_shape_manual(values = c(21, 22, 24, 21, 22, 24), guide = FALSE) +
  scale_color_manual(values = c(rgb(red = 46, green = 117, blue = 182, max = 255),
                                rgb(red = 184, green = 13, blue = 72, max = 255)),
                     name = "", labels = c("Male", "Female")) +
  scale_fill_manual(name = "", values = c(rgb(red = 46, green = 117, blue = 182, max = 255),
                                          rgb(red = 184, green = 13, blue = 72, max = 255)), guide = FALSE) +
  scale_x_continuous(breaks = c(1, 2, 3, 4, 5, 6, 7), limits = c(0.8, 7)) +
  geom_line(aes(color = sex)) +
  geom_point(aes(color = sex, fill = sex), size = 3) +
  theme_serif() +
  theme(legend.position = c(0.2, 0.98))

cons_distr <- cons_group1 %>% 
  filter(phase == 2, group_night != 4) %>%
  ggplot(aes(x = group_night, y = vol_consumed)) +
  labs(x = "Night", title = "Distributed condition - two patches",
       y = "") +
  scale_shape_manual(values = c(21, 22, 24, 21, 22, 24), guide = FALSE) +
  scale_color_manual(values = c(rgb(red = 46, green = 117, blue = 182, max = 255),
                                rgb(red = 184, green = 13, blue = 72, max = 255)), guide = FALSE) +
  scale_fill_manual(name = "", values = c(rgb(red = 46, green = 117, blue = 182, max = 255),
                                          rgb(red = 184, green = 13, blue = 72, max = 255)), guide = FALSE) +
  scale_x_continuous(breaks = c(1, 2, 3, 4, 5, 6, 7), limits = c(0.8, 7)) +
  geom_line(aes(group = Individual, color = sex)) +
  geom_point(aes(shape = Individual, color = sex, fill = sex), size = 3) +
  ylim(c(0, 18)) +
  theme_serif()

ggarrange(cons_clumped, cons_distr,
          font.label = list(size = 18, family = "serif"))
library(ggbeeswarm)


fig_chases <- chase_summ %>% 
  filter(phase == 1) %>% 
  group_by(sex, group, IdLabel) %>% 
  summarise(prop_chases = mean(prop_chases)) %>% 
  ggplot() +
  geom_beeswarm(aes(group, prop_chases, color = sex),
              alpha = 0.7, cex = 2, size = 4) +
  # geom_boxplot(aes(group, prop_chases, fill = sex),
  #              width = 0.3, size = 0.6, alpha = 0.7,
  #              position = position_dodge(width = 1)) +
  labs(title = "Chasing", x = "", y = "Proportion chasing") +
  theme_serif() +
  scale_color_manual(values = c(rgb(red = 46, green = 117, blue = 182, max = 255),
                                rgb(red = 184, green = 13, blue = 72, max = 255)),
                     name = "", labels = c("Male", "Female")) +
  # scale_fill_manual("", values = c("grey24", "white"), guide = FALSE) +
  scale_x_discrete(labels = c("Mixed \n Group1", "Mixed \n Group2", "Mixed \n Group3",
                              "Mixed \n Group4", "Male \n Group", "Female \n Group")) +
  scale_y_continuous(limits = c(0, 0.016), labels = c(0, "", 0.01, "")) +
  theme(axis.ticks.x = element_line(size = 1.2),
        axis.text.x = element_text(size = 16),
        axis.text.y = element_text(size = 18),
        legend.position = c(0.25, 0.9))

fig_chased <- chase_summ %>%  
  filter(phase == 1) %>% 
  group_by(sex, group, IdLabel) %>% 
  summarise(prop_chased = mean(prop_chased)) %>% 
  ggplot() +
  geom_beeswarm(aes(group, prop_chased, color = sex),
                alpha = 0.7, cex = 2, size = 4) +
  labs(title = "Being chased", x = "", y = "Proportion chased") +
  theme_serif() +
  scale_color_manual(values = c(rgb(red = 46, green = 117, blue = 182, max = 255),
                                rgb(red = 184, green = 13, blue = 72, max = 255)),
                     guide = FALSE) +
  scale_x_discrete(labels = c("Mixed \n Group1", "Mixed \n Group2", "Mixed \n Group3",
                              "Mixed \n Group4", "Male \n Group", "Female \n Group")) +
  scale_y_continuous(limits = c(0, 0.016), labels = c(0, "", 0.01, "")) +
  theme(axis.ticks.x = element_line(size = 1.2),
        axis.text.x = element_text(size = 16),
        axis.text.y = element_text(size = 18))

ggarrange(fig_chases, fig_chased,
          font.label = list(size = 18, family = "serif"))


ggarrange(chase_males, chase_females,
          font.label = list(size = 18, family = "serif"),
          common.legend = TRUE, legend = "none")


stat.test2 <- cons_contrasts %>%
  group_by(partition, status) %>%
  t_test(vol_hr ~ phase) %>% 
  adjust_pvalue(method = "holm") %>%
  add_significance("p.adj") %>% 
  add_xy_position(x = "phase", group = "status", fun = "min") 


ggboxplot(cons_contrasts, x = "phase", y = "vol_hr",
          color = "status", line.color = "gray", line.size = 0.4,
          palette = c("dodgerblue3","grey50","purple"),
          add = "jitter") +
  facet_grid(. ~ partition) +
  # stat_pvalue_manual(
  #   stat.test,  label = "p.adj.signif", tip.length = 0.02, hide.ns = TRUE
  # ) +
  stat_pvalue_manual(
    stat.test,  label = "p.adj", tip.length = 0.02, hide.ns = TRUE
  ) +
   stat_pvalue_manual(
    stat.test2,  label = "p.adj", tip.length = 0.02, hide.ns = TRUE
  ) +
  scale_y_continuous(expand = expansion(mult = c(0.05, 0.1))) +
  labs(x = "", y = bquote("Nectar intake ["~mL~h^-1*"]"), color = "") +
  theme_serif()


flower_distribution(onlyrewarded, "mixed4", phases = "both")
