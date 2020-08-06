library(Hmisc)
library(tidyverse)
library(broom.mixed)
# library(brms)
# library(tidybayes)

folder <- "analysis/data/"

full_experiment <- read.csv2(file = paste0(folder, "EventData.csv"), header = TRUE, sep = ";", dec = ".",
                             na.strings = "NA") %>%
  mutate(DateTime = as.POSIXct(DateTime, tz = "UTC"))

repeat_bats <- c("Ind14", "Ind15", "Ind18", "Ind20")
cages <- tibble(day = 1:68, cage = c(rep(1, 17), rep(2, 17), rep(1, 17), rep(2, 17)))

# focus only on the pokes that have duration of at least 300 ms
onlychoices <- full_experiment %>%
  filter(choice, eventDuration > 300) %>%
  select(-choice, -unitLabel, -SystemMsg) %>%
  arrange(IdLabel, day, DateTime)

# how long was pump inactive per night and how many times it refilled?
pump_inactive <- full_experiment %>%
  filter(str_detect(SystemMsg, "pump")) %>%
  select(-cage) %>%
  left_join(cages) %>%
  group_by(day) %>%
  mutate(pumping = cumsum(pumping)) %>%
  group_by(day, cage, pumping) %>%
  summarise(dur = as.numeric(max(DateTime) - min(DateTime))) %>%
  group_by(day, cage) %>%
  summarise(n_pumpings = max(pumping),
            total_dur = sum(dur)) %>%
  group_by(cage) %>%
  summarise(mean_pumpings = mean(n_pumpings), sd_pumpings = sd(n_pumpings), mean_dur = mean(total_dur), sd_dur = sd(total_dur))



# subset for only data from plasticity experiment
onlychoices_plast <- onlychoices %>% 
  # filter(pumping == 0, cond == "plast") %>% # optional filtering out of the times when pump inactive
  filter(cond == "plast") %>% 
  group_by(day, IdLabel) %>%
  # running visit number, vis_to_first gives 1 for only the rewarding flowers
  mutate(n_vis = 1:n(), 
         vis_to_first = ifelse(rewarding == 1, 1, 0),
         cohort = factor(cohort)) %>% 
  ungroup()

sides <- tibble(loc = 1:12, side = c(rep(1:4, each = 3)),
                x = c(1, 2, 3, 4, 4, 4, 3, 2, 1, 0, 0, 0),
                y = c(0, 0, 0, 1, 2, 3, 4, 4, 4, 3, 2, 1))


onlychoices_plast %>% 
  left_join(sides) %>% 
  group_by(IdLabel, loc, day, side) %>% 
  summarise(rewarding = max(rewarding)) %>% 
  group_by(IdLabel, day, side) %>% 
  summarise(rewarding_side = max(rewarding)) %>% 
  select(IdLabel, day, side, rewarding_side)

# general visit numbers
onlychoices_plast %>%
  count(day, IdLabel, prob_cond) %>% 
  group_by(prob_cond) %>% 
  summarise(mean = round(mean(n)), sd = round(sd(n)))

binned_choices <- onlychoices_plast %>% 
  mutate(vis_bins = cut(n_vis, breaks = seq(0, 3000, 20))) %>% 
  group_by(IdLabel, weight, cond, prob_cond, day, cohort_day, cohort, cage, vis_bins) %>% 
  summarise(n_vis_to_first = sum(vis_to_first),
            n_total = n(),
            max_date = max(DateTime)) %>% 
  group_by(IdLabel, cond, day, cohort_day, cohort, cage, prob_cond) %>%
  mutate(mean_vis_to_first = mean(n_vis_to_first),
         over_mean = as.numeric(n_vis_to_first > mean_vis_to_first),
         over_crit = ifelse(over_mean + lag(over_mean, default = 0) == 2, 1, 0),
         over_crit = cumsum(over_crit))

write.table(binned_choices, file = paste0(folder, "PlasticityBinnedChoices.csv"), sep = ";", row.names = FALSE)

asymptotic_choices <- binned_choices %>% 
  # remove data from bats repeating the experiment
  filter(over_crit > 0, !(cohort == 4 & IdLabel %in% repeat_bats)) %>% 
  select(-over_crit, -over_mean) %>% 
  group_by(IdLabel, weight, cond, day, cohort_day, cohort, cage, prob_cond) %>% 
  summarise(successes = sum(n_vis_to_first),
            failures = sum(n_total) - successes,
            sampling = failures/sum(n_total))

groups <- asymptotic_choices %>% 
  filter(cohort_day %in% 6:9) %>%
  group_by(IdLabel) %>% 
  summarise(prob_first = max(prob_cond)) %>% 
  mutate(group = factor(ifelse(prob_first == 0.83, "50-83-30", "50-30-83"))) %>%
  select(IdLabel, group)

asymptotic_choices <- asymptotic_choices %>% 
  ungroup() %>% 
  left_join(groups) %>% 
  arrange(IdLabel, cohort_day, day) %>% 
  mutate(prob_cen = prob_cond - mean(prob_cond))

write.table(asymptotic_choices, file = paste0(folder, "PlasticityAsymptoticChoices.csv"), sep = ";", row.names = FALSE)

# visualize data for exploration
focal_ind <- "Ind41"
binned_choices %>% 
  filter(IdLabel == focal_ind) %>% 
  mutate(cohort_day = factor(cohort_day)) %>% 
  ggplot(aes(vis_bins, n_vis_to_first, color = cohort_day, group = cohort_day)) +
  geom_line() +
  geom_point() +
  facet_grid(cohort_day~.)

onlychoices_plast %>% 
  filter(IdLabel == focal_ind) %>% 
  left_join(sides) %>%
  count(cohort_day, IdLabel, side, rewarding, x, y, phase) %>% 
  ggplot(aes(x, y)) +
  geom_text(aes(label = rewarding, size = n)) +
  facet_grid(cohort_day ~ phase) +
  theme_void()

library(MCMCglmm)
library(postMCMCglmm)
#### statistics for plasticity
# function for plotting autocorrelations from https://github.com/tmalsburg/MCMCglmm-intro
plot.acfs <- function(x) {
  n <- dim(x)[2]
  par(mfrow = c(ceiling(n/2), 2), mar = c(3, 2, 3, 0))
  for (i in 1:n) {
    acf(x[, i], lag.max = 100, main = colnames(x)[i])
    grid()
  }
}

# summary function for mcmc objects, giving the posterior mode and the highest posterior density intervals 
mode_HPD <- function(mcmc) {
  round(c(posterior.mode(mcmc), HPDinterval(mcmc)), 2) %>% set_names(c("estimate", "conf.low", "conf.high"))
}

# parameter expanded prior
prior.binom = list(R = list(V = diag(1), nu = 0.002),
               G = list(G1 = list(V = diag(2), nu = 1.002, alpha.mu = c(0, 0), 
                              alpha.V = diag(2) * 1e+07)))

# beta_binomial2 <- custom_family(
#   "beta_binomial2", dpars = c("mu", "phi"),
#   links = c("logit", "log"), lb = c(NA, 0),
#   type = "int", vars = "vint1[n]"
# )
# 
# stan_funs <- "
#   real beta_binomial2_lpmf(int y, real mu, real phi, int T) {
#     return beta_binomial_lpmf(y | T, mu * phi, (1 - mu) * phi);
#   }
#   int beta_binomial2_rng(real mu, real phi, int T) {
#     return beta_binomial_rng(T, mu * phi, (1 - mu) * phi);
#   }
#   
# "
# 
# stanvars <- stanvar(scode = stan_funs, block = "functions")
# 
# log_lik_beta_binomial2 <- function(i, prep) {
#   mu <- prep$dpars$mu[, i]
#   phi <- prep$dpars$phi
#   trials <- prep$data$vint1[i]
#   y <- prep$data$Y[i]
#   beta_binomial2_lpmf(y, mu, phi, trials)
# }
# 
# posterior_predict_beta_binomial2 <- function(i, prep, ...) {
#   mu <- prep$dpars$mu[, i]
#   phi <- prep$dpars$phi
#   trials <- prep$data$vint1[i]
#   beta_binomial2_rng(mu, phi, trials)
# }
# 
# posterior_epred_beta_binomial2 <- function(prep) {
#   mu <- prep$dpars$mu
#   trials <- prep$data$vint1
#   trials <- matrix(trials, nrow = nrow(mu), ncol = ncol(mu), byrow = TRUE)
#   mu * trials
# }
# 
# set.seed(42)
# samp_model <- brm(failures | vint(successes + failures) ~  prob_cen + weight + group*cohort +
#                     (1 + prob_cen | IdLabel),
#                   data = asymptotic_choices,
#                   family = beta_binomial2,
#                   stanvars = stanvars
#                   )
# summary(samp_model)
# plot(samp_model)
# mcmc_plot(samp_model, type = "acf_bar")
# 
# mcmc_plot(samp_model, type = "areas", prob = 0.95)
# samp_model %>% 
#   tidy(effects = "fixed", conf.method="HPDinterval")
# VarCorr(samp_model)
# samp_model$fit
# 
# samp_model %>% 
#   tidy(effects = "fixed", conf.method="HPDinterval")
# samp_model %>% 
#   tidy(effects = "ran_pars", robust = TRUE)
# 
#   spread_draws(condition_mean[condition]) %>%
#   median_qi()

set.seed(42)
# FULL MODEL
# whole dataset
UMM.samp <- MCMCglmm(cbind(failures, successes) ~ prob_cen + weight + group*cohort,
                     random = ~us(1 + prob_cen):IdLabel,
                     data = as.data.frame(asymptotic_choices), family = "multinomial2", pr = TRUE,
                     prior = prior.binom, verbose = FALSE, saveX = TRUE, saveZ = TRUE,
                     nitt = 200e+03, thin = 100, burnin = 100e+03)

summary(UMM.samp) #  fixed effects: only prob_cen is significant (DIC = 144316.4)
# Geweke plots
geweke.plot(UMM.samp$VCV)
plot(UMM.samp)
# Autocorrelation
plot.acfs(UMM.samp$VCV)

samp_estimates <- tidy(UMM.samp, conf.int = TRUE) %>% 
  select(-std.error) %>% 
  mutate(across(where(is.numeric), ~round(., 2)))

# repeatabilities for each probability condition
prior.intercept = list(R = list(V = 1, nu = 0.002), 
                    G = list(G1 = list(V = 1, nu = 0.002, alpha.mu = 0, 
                                       alpha.V = 1e+07)))

adj_repeatability <- function(df, test_prob) {
  df <- as.data.frame(df %>% filter(prob_cond == test_prob))
  UMM.samp_subset <- MCMCglmm(cbind(failures, successes) ~ 1,
                              random = ~IdLabel,
                              data = df, family = "multinomial2", pr = TRUE,
                              prior = prior.intercept, verbose = FALSE, saveX = TRUE, saveZ = TRUE,
                              nitt = 200e+03, thin = 100, burnin = 100e+03)
  print(summary(UMM.samp_subset))
  geweke.plot(UMM.samp_subset$VCV)
  
  rep.samp <- UMM.samp_subset$VCV[, "IdLabel"] /
    (UMM.samp_subset$VCV[, "IdLabel"] + UMM.samp_subset$VCV[, "units"] + pi^2/3)
  mode_HPD(rep.samp)
}

# set.seed(1234)
# samp_subset_model <- brm(failures | vint(successes + failures) ~ (1 | IdLabel),
#                   data = asymptotic_choices %>% filter(prob_cond == test_prob),
#                   family = beta_binomial2,
#                   stanvars = stanvars)



set.seed(1234)
repeatabilities <- map(c(0.3, 0.5, 0.83), ~adj_repeatability(asymptotic_choices, .)) %>% 
  set_names(c(0.3, 0.5, 0.83))

repeatabilities <- repeatabilities %>%
  enframe() %>%
  mutate(term = paste0("rep__sampling:", name)) %>% 
  unnest_wider(col = value)

# correlation between intercept and slope
cor_int.slope <- UMM.samp$VCV[, "(Intercept):prob_cen.IdLabel"] / 
  sqrt(UMM.samp$VCV[, "(Intercept):(Intercept).IdLabel"] + UMM.samp$VCV[, "prob_cen:prob_cen.IdLabel"])

cor_int.slope <- tibble(effect = "ran_pars", group = "IdLabel",
                       term = "cor__(Intercept).prob_cen", estimate = mode_HPD(cor_int.slope)[1],
                       conf.low = mode_HPD(cor_int.slope)[2], conf.high = mode_HPD(cor_int.slope)[3])

samp_estimates <- samp_estimates %>%
  filter(!str_detect(term, "cov")) %>%
  bind_rows(cor_intslope, repeatabilities) %>%
  select(-name)

write.table(samp_estimates, file = paste0(folder, "PlasticityEstimates.csv"), sep = ";", row.names = FALSE)

predicted_values <- predict(object = UMM.samp, marginal = NULL, type = "response", use = "all")

# # retreive slopes from model
sol <- UMM.samp$Sol %>%
 tidy()

grand_slope <- sol %>% 
  filter(term == "prob_cen") %>% 
  pull(estimate)

grand_intercept <- sol %>% 
  filter(term == "(Intercept)") %>% 
  pull(estimate)

sol %>% 
  filter(str_detect(term, "prob_cen.")) %>% 
  ggplot(aes(estimate + grand_slope)) +
  geom_histogram()

slopes <- sol %>% 
  filter(str_detect(term, "prob_cen.")) %>% 
  select(-std.error) %>% 
  rename(IdLabel = term, slope = estimate) %>% 
  mutate(IdLabel = str_extract(IdLabel, "Ind[[:digit:]]*$"),
         slope = slope + grand_slope)

intercepts <- sol %>% 
  filter(str_detect(term, "[[:punct:]].{1}IdLabel")) %>% 
  select(-std.error) %>% 
  rename(IdLabel = term, intercept = estimate) %>% 
  mutate(IdLabel =  str_extract(IdLabel, "Ind[[:digit:]]*$"),
         intercept = grand_intercept + intercept)

plasticity_predictions <- asymptotic_choices %>% 
  mutate(predicted_failures = as.numeric(predicted_values),
         predicted_sampling = predicted_failures / (predicted_failures + successes)) %>% 
  left_join(slopes)

plasticity_predictions %>% 
  group_by(prob_cond, IdLabel) %>% 
  summarise(failures = mean(failures), predicted_failures = mean(predicted_failures)) %>% 
  ggplot(aes(failures, predicted_failures, color = IdLabel)) +
  facet_wrap(~prob_cond) +
  geom_abline() +
  geom_point()

# plasticity_predictions %>% 
#   filter(IdLabel == "Ind42") %>% 
#   ggplot(aes(prob_cond, sampling, color = IdLabel, group = IdLabel)) +
#   geom_point() +
#   geom_line(stat = "smooth", method = glm, method.args = list(family = binomial), se = FALSE,
#             color = "black", alpha = 0.3)

write.table(plasticity_predictions, file = paste0(folder, "PlasticityPredictions.csv"),
            sep = ";", row.names = FALSE)


prior.norandom = list(R = list(V = diag(1), nu = 0.002))

set.seed(55)
# model without random effects
UMM.samp_cond <- MCMCglmm(cbind(failures, successes) ~ prob_cen + weight + group*cohort, 
                          data = as.data.frame(asymptotic_choices), family = "multinomial2", pr = TRUE,
                          prior = prior.norandom, verbose = FALSE, saveX = TRUE, saveZ = TRUE,
                          nitt = 200e+03, thin = 100, burnin = 100e+03)

summary(UMM.samp_cond)

UMM.samp$DIC - UMM.samp_cond$DIC #negative difference shows the model with random effects is superior

# how many visits to rewarding flowers until asymptotic phase?

binned_choices %>% 
  filter(prob_cond == 0.5) %>% 
  mutate(cum_visits_to_first = cumsum(n_vis_to_first)) %>% 
  filter(over_crit == 1) %>% 
  group_by(IdLabel, cohort_day) %>%
  summarise(vis_crit = min(cum_visits_to_first)) %>% 
  ungroup() %>% 
  summarise(under_100 = mean(vis_crit <= 100),
            mean_vis_crit = mean(vis_crit),
            sd_vis_crit = sd(vis_crit))

# Flexibilities

# remove data from repeating bats
flex_exp <- onlychoices %>% 
  filter(cond == "flex", !(cohort == 4 & IdLabel %in% repeat_bats)) %>%
  select(-weight, -cond, -prob) %>% 
  group_by(day, IdLabel, phase) %>% 
  mutate(vis_phase = 1:n(),
         rev_vis_phase = n():1,
         vis_to_first = ifelse(rewarding == 1, 1, 0),
         vis_to_second = ifelse(rewarding == 2, 1, 0),
         cum_not_first = cumsum(ifelse(rewarding == 1, 0, 1)),
         cum_rewarded = cumsum(rewarded),
         cohort = factor(cohort))

# graphical overview of individual bat's behaviour in flexibility experiment

binned_flex <- flex_exp %>% 
  group_by(day, IdLabel) %>% 
  mutate(n_vis = 1:n(),
         vis_bins = cut(n_vis, breaks = seq(0, 3000, 20))) %>% 
  group_by(IdLabel, cohort_day, vis_bins) %>% 
  summarise(n_vis_to_first = sum(vis_to_first),
            n_vis_to_second = sum(vis_to_second),
            n_total = n(),
            max_date = max(DateTime),
            phase = factor(round(mean(phase))),
            sum_rewarded = sum(rewarded)) %>% 
  mutate(block = 1:n(),
         total_vis_to_second = sum(n_vis_to_second),
         rewarded = sum_rewarded > 0)

focal_ind <- "Ind44"
binned_flex %>% 
  filter(IdLabel == focal_ind) %>% 
  ggplot() +
  geom_point(aes(block, n_vis_to_first, group = NULL, color = phase)) +
  geom_line(aes(block, n_vis_to_first, group = NULL, color = phase)) +
  geom_line(aes(block, n_vis_to_second, group = NULL), color = "black", alpha = 0.3) +
  labs(y = "Number of visits to feeders that were rewarding in phase 1", title = focal_ind) +
  facet_wrap(~cohort_day)

# where did the focal bat fly to?
flex_exp %>% 
  filter(IdLabel == focal_ind) %>% 
  left_join(sides) %>%
  count(cohort_day, IdLabel, side, rewarding, x, y, phase) %>% 
  ggplot(aes(x, y)) +
  geom_text(aes(label = rewarding, size = n)) +
  facet_grid(cohort_day ~ phase)

# binned_flex2 <- flex_exp %>% 
#   group_by(day, IdLabel, phase) %>%
#   mutate(cum_vis_second = cumsum(vis_to_second)) %>% 
#   filter(cum_vis_second > 1, phase == 2) %>%
#   group_by(day, IdLabel) %>% 
#   mutate(n_vis = 1:n(),
#          vis_bins = cut(n_vis, breaks = seq(0, 3000, 20))) %>% 
#   group_by(IdLabel, cohort_day, vis_bins) %>% 
#   summarise(n_vis_to_first = sum(vis_to_first),
#             n_vis_to_second = sum(vis_to_second),
#             n_total = n(),
#             max_date = max(DateTime),
#             phase = factor(round(mean(phase)))) %>% 
#   mutate(block = 1:n())

# calculate the sampling behaviour in the last 50 visits of phase 1
samp_before_phase2 <- flex_exp %>% 
  filter(phase == 1, rev_vis_phase < 51) %>% 
  group_by(day, IdLabel, cohort, cage) %>%
  summarise(sampling_before_phase2 = 1 - mean(vis_to_first))


n_analysed <- 60 #works with 100 and upwards, but fewer bats made that many visits. Works even with 25 visits.

flex_phase2 <- flex_exp %>% 
  group_by(day, IdLabel, phase) %>%
  mutate(cumsum_rewarded = cumsum(rewarded)) %>% 
  filter(cumsum_rewarded > 0, phase == 2) %>%
  group_by(day, IdLabel) %>% 
  mutate(n_vis = 1:n()) %>% 
  filter(n_vis < n_analysed + 1) %>% 
  group_by(day, cohort_day, IdLabel, cohort, cage) %>% 
  summarise(perseverance = sum(vis_to_first),
            flexibility = max(n_vis) - perseverance,
            n_total = max(n_vis)) %>% 
  left_join(samp_before_phase2) %>% 
  group_by(IdLabel) %>% 
  mutate(dev_pers = abs(median(perseverance) - perseverance)) %>% 
  filter(n_total > n_analysed - 1) # filter out the two bats that didn't make n_analysed visits

# show bats with missing data
flex_phase2 %>% 
  count(IdLabel) %>% 
  filter(n < 4)

# most perseverative (least flexible) and least perseverative (most flexible) bats
flex_phase2 %>% 
  group_by(IdLabel) %>% 
  summarise(perseverance = sum(perseverance),
            sum_flexibility = sum(flexibility),
            n_total = sum(n_total),
            perc_pers = perseverance / n_total,
            mean_flexibility = mean(flexibility)) %>% 
  arrange(desc(perc_pers))

# Data for Table 2
# model
set.seed(55)

flex_model <- MCMCglmm(cbind(flexibility, perseverance) ~ cohort_day + sampling_before_phase2 + cohort,
                       random = ~IdLabel,
                       data = as.data.frame(flex_phase2), family = "multinomial2", pr = TRUE,
                       prior = prior.intercept,
                       verbose = FALSE, saveX = TRUE, saveZ = TRUE,
                       nitt = 200e+03, thin = 100, burnin = 100e+03)

summary(flex_model)
plot(flex_model)
geweke.plot(flex_model$VCV)


prior.interceptslope = list(R = list(V = 1, nu = 0.002), 
                        G = list(G1 = list(V = diag(2), nu = 1.002, alpha.mu = c(0, 0), 
                                               alpha.V = diag(2) * 1e+07)))

summary(flex_model)
plot(flex_model)
geweke.plot(flex_model$VCV)

rep_flex <- flex_model$VCV[,"IdLabel"] /
  (flex_model$VCV[, "IdLabel"] + flex_model$VCV[, "units"] + pi^2/3)

rep_flex <- tibble(term = "rep__flexibility") %>% 
  bind_cols(mode_HPD(rep_flex) %>% 
  t() %>% 
  as_tibble())

# correlation between plasticity and flexibility
# slope_flex <- flex_phase2 %>%
#   select(day, IdLabel, perseverance, flexibility) %>%
#   full_join(slopes %>% mutate(day = 0)) %>% 
#   mutate(neg_slope = -slope) %>% 
#   full_join(intercepts %>% mutate(day = 0))
# 
# asymptotic_choices %>% 
#   group_by(IdLabel) %>% 
#   summarise(mean_sampling = mean(sampling)) %>% 
#   left_join(intercepts) %>% 
#   ggplot(aes(mean_sampling, intercept)) +
#   geom_point()

# samp_flex <- flex_phase2 %>% 
#   select(day, IdLabel, perseverance, flexibility) %>%
#   full_join(samp_changes %>%  mutate(day = 0))

# observed <- plasticity_predictions %>% 
#   filter(prob_cond != 0.5) %>% 
#   group_by(IdLabel, prob_cond) %>%
#   summarise(predicted_sampling = mean(predicted_sampling), sampling = median(sampling)) %>% 
#   pivot_wider(id_cols = IdLabel, names_from = prob_cond, values_from = sampling,
#               names_prefix = "Prob") %>% 
#   mutate(samp_change = Prob0.83 - Prob0.3)
# 
# predicted <- plasticity_predictions %>% 
#   filter(prob_cond != 0.5) %>% 
#   group_by(IdLabel, prob_cond) %>%
#   summarise(predicted_sampling = mean(predicted_sampling), sampling = median(sampling)) %>% 
#   pivot_wider(id_cols = IdLabel, names_from = prob_cond, values_from = predicted_sampling,
#               names_prefix = "Prob") %>% 
#   mutate(samp_change = Prob0.83 - Prob0.3) %>% 
#   select(IdLabel, pred_change = samp_change)

# observed %>% 
#   left_join(predicted) %>%
#   left_join(slopes) %>% 
#   ggplot(aes(samp_change, slope)) +
#   geom_point() +
#   stat_smooth(method = lm)


# model for correlation between flexibility and slope
# because the more negative slopes correspond to the more plastic bats,
# and the the more perseverative bats are by definition the less flexible bats
# the estimated correlation (perseverance vs. slope) is logically equivalent to
# the correlation between plasticity and flexibility
# If the two measures are linked to the same latent variable, then the correlation should be positive

# prior.2dim <- list(R = list(V = diag(2), nu = 1.002),
#                    G = list(G1 = list(V = diag(2), nu = 1.002, alpha.mu = c(0, 0))))
# 
# pers_slope_model <- MCMCglmm(cbind(cbind(flexibility, perseverance), neg_slope) ~ (trait - 1),
#                       random = ~us(trait):IdLabel,
#                       rcov = ~idh(trait):units, # since the two measures were not taken concurrently,
#                       # they cannot be correlated within individuals
#                       family = c("multinomial2", "gaussian"), prior = prior.2dim,
#                       nitt = 200e+03, thin = 100, burnin = 100e+03,
#                       data = as.data.frame(slope_flex), verbose = FALSE)
# 
# summary(pers_slope_model)
# geweke.plot(pers_slope_model$VCV)
# plot(pers_slope_model$VCV)

cor_MCMC <- function(VCV, trait1, trait2, type = "between") {
  
  if (type == "between") {
    suffix = ".IdLabel"
  } else {
    suffix = ".units"
  }
  
  cov <- VCV[, paste0(trait1, ":", trait2, suffix)]
  var1 <- VCV[, paste0(trait1, ":", trait1, suffix)]
  var2 <- VCV[, paste0(trait2, ":", trait2, suffix)]
  
  cov / sqrt(var1 * var2)
}

# more perseverative (less flexible), more negative slope (more plastic)

plast_flex <- asymptotic_choices %>% 
  select(cohort_day, IdLabel, prob_cen, successes, failures, sampling) %>% 
  full_join(flex_phase2 %>%
              select(cohort_day, IdLabel, perseverance, flexibility) %>% 
              mutate(prob_cen = 0))

prior.2dim <- list(R = list(V = diag(2), nu = 1.002),
                   G = list(G1 = list(V = diag(3), nu = 3, alpha.mu = rep(0, 3),
                                      alpha.V = diag(25^2, 3, 3))))
set.seed(2020)
plast_flex_model <- MCMCglmm(cbind(cbind(flexibility, perseverance),
                                   cbind(failures, successes)) ~ (trait - 1) +
                               at.level(trait, 2):prob_cen +
                               at.level(trait, 1):cohort_day,
                              random = ~us(trait + prob_cen:at.level(trait, 2)):IdLabel,
                              rcov = ~idh(trait):units, # since the two measures were not taken concurrently,
                              # they cannot be correlated within individuals
                              family = c("multinomial2", "multinomial2"), prior = prior.2dim,
                              nitt = 200e+03, thin = 100, burnin = 100e+03,
                              data = as.data.frame(plast_flex), verbose = FALSE)

summary(plast_flex_model)
plot(plast_flex_model$VCV)  
geweke.plot(plast_flex_model$VCV)

# flexibility negatively correlates with the intercept (sampling rate)
cor_MCMC(plast_flex_model$VCV, "traitflexibility", "traitfailures", type = "between") %>% 
  mode_HPD()

# flexibility negatively correlates with the negative slope (plasticity)
-cor_MCMC(plast_flex_model$VCV, "traitflexibility", "prob_cen:at.level(trait, 2)", type = "between") %>%
  mode_HPD()

cor_flex.intc <- tibble(term = "cor__flexibility.intercept") %>% 
  bind_cols(mode_HPD(cor_MCMC(plast_flex_model$VCV, "traitflexibility", "traitfailures", type = "between")) %>% 
              t() %>% 
              as_tibble())
# because the negative slope corresponds to more plastic individuals, take the opposite sign here
cor_flex.slope <- tibble(term = "cor__flexibility.plasticity") %>% 
  bind_cols(mode_HPD(-cor_MCMC(plast_flex_model$VCV, "traitflexibility", "prob_cen:at.level(trait, 2)", type = "between")) %>% 
              t() %>% 
              as_tibble())

flex_estimates <- tidy(flex_model, conf.int = TRUE) %>% 
  select(-std.error) %>% 
  mutate(across(where(is.numeric), ~round(., 2))) %>% 
  bind_rows(rep_flex, cor_flex.slope, cor_flex.intc)

write.table(flex_estimates, file = paste0(folder, "FlexibilityEstimates.csv"), sep = ";", row.names = FALSE)

