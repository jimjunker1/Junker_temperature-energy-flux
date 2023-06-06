# This script outlines additional analyses and checks to address reviewer concerns.

source("packages.R")
## How appropriate is the assumption of a log-linear model vs GLM with non-gaussian error when describing the relationship between mean body size or P:B? ----

# this deals with the m_spp_temp_boots and pb_spp_temp_boots models
## load the object from the cache
drake::loadd("temperature_stats")

## extract the bootstrapped summary
sppBootsDf = temperature_stats[['sppBootsDf']]
rm(temperature_stats)

### Inspection of residuals with log-linear models ----

# body size
m_spp_temp_boots = sppBootsDf %>%
  group_by(n_rep) %>%
  do(model = lm(log(M_mg_ind) ~ tempC, data = .),
     fitted = fitted(lm(log(M_mg_ind) ~ tempC, data = .)),
     resids = resid(lm(log(M_mg_ind) ~ tempC, data = .)))

m_spp_temp_resids = m_spp_temp_boots %>% 
  dplyr::select(n_rep, fitted, resids) %>% 
  group_by(n_rep) %>% 
  unnest_longer(c(fitted, resids))
  
ggplot(m_spp_temp_resids) + 
  geom_point(aes(x = fitted, y = resids))+
  geom_hline(aes(yintercept = 0))

### pb
pb_spp_temp_boots = sppBootsDf %>%
  group_by(n_rep) %>%
  do(model = lm(log(pb_y) ~ tempC, data = .),
     fitted = fitted(lm(log(pb_y) ~ tempC, data = .)),
     resids = rstandard(lm(log(pb_y) ~ tempC, data = .)))

pb_spp_temp_resids = pb_spp_temp_boots %>% 
  dplyr::select(n_rep, fitted, resids) %>% 
  group_by(n_rep) %>% 
  unnest_longer(c(fitted, resids))

pb_spp_temp_resid_summ = pb_spp_temp_resids %>% 
  group_by(fitted_id) %>% 
  dplyr::summarise(fitted = median(fitted),
            resids = median(resids))

gaus_resid_fit_PB = ggplot(pb_spp_temp_resids) + 
  geom_point(aes(x = fitted, y = resids), alpha = 0.5, color= 'grey')+
  geom_point(data = pb_spp_temp_resid_summ, aes(x = fitted, resids), size = 4, color = 'black')+
  annotate('text', label = "log-linear", size = 10, x = Inf, y = Inf, vjust = 1, hjust = 1)+
  geom_hline(aes(yintercept = 0))

### Inspection of residuals with GLM with Gaussian log link ----

# body size
m_spp_temp_boots_glm = sppBootsDf %>%
  group_by(n_rep) %>%
  do(model = glm(M_mg_ind ~ tempC, family = gaussian(link = 'log'), data = .),
     fitted = fitted(glm(M_mg_ind ~ tempC, family = gaussian(link = 'log'), data = .)),
     resids = resid(glm(M_mg_ind ~ tempC, family = gaussian(link = 'log'), data = .)))

m_spp_temp_resids_glm = m_spp_temp_boots_glm %>% 
  dplyr::select(n_rep, fitted, resids) %>% 
  group_by(n_rep) %>% 
  unnest_longer(c(fitted, resids))

ggplot(m_spp_temp_resids_glm) + 
  geom_point(aes(x = fitted, y = resids))+
  geom_hline(aes(yintercept = 0))

### pb
pb_spp_temp_boots_glm = sppBootsDf %>%
  group_by(n_rep) %>%
  do(model = glm(pb_y ~ tempC, family = gaussian(link = 'log'), data = .),
     fitted = fitted(glm(pb_y ~ tempC, family = gaussian(link = 'log'), data = .)),
     resids = rstandard(glm(pb_y ~ tempC, family = gaussian(link = 'log'), data = .)))

pb_spp_temp_resids_glm = pb_spp_temp_boots_glm %>% 
  dplyr::select(n_rep, fitted, resids) %>% 
  group_by(n_rep) %>% 
  unnest_longer(c(fitted, resids))

pb_spp_temp_resids_glm_summ = pb_spp_temp_resids_glm %>% 
  group_by(fitted_id) %>% 
  dplyr::summarise(fitted = median(fitted),
                   resids = median(resids))

glm_resid_fit_pb = ggplot(pb_spp_temp_resids_glm) + 
  geom_point(aes(x = fitted, y = resids), alpha = 0.5, color = 'grey')+
  geom_point(data = pb_spp_temp_resids_glm_summ, aes(fitted, resids), size = 4, color = 'black')+
  annotate('text', label = "GLM (link = 'log')", size = 10, x = Inf, y = Inf, vjust = 1, hjust = 1)+
  geom_hline(aes(yintercept = 0));glm_resid_fit_pb

## nls of 
# body size
m_spp_temp_boots_nls = sppBootsDf %>%
  group_by(n_rep) %>%
  do(model = nls(M_mg_ind ~ a *tempC ^b, start = list(a = 1, b = 0.5), control = nls.control(maxiter = 1e3),  data = .),
     fitted = fitted(nls(M_mg_ind ~ a *tempC ^b, start = list(a = 1, b = 0.5), control = nls.control(maxiter = 1e3),  data = .)),
     resids = resid(nls(M_mg_ind ~ a *tempC ^b, start = list(a = 1, b = 0.5), control = nls.control(maxiter = 1e3),  data = .)))

m_spp_temp_resids_nls = m_spp_temp_boots_nls %>% 
  dplyr::select(n_rep, fitted, resids) %>% 
  group_by(n_rep) %>% 
  unnest_longer(c(fitted, resids))

ggplot(m_spp_temp_resids_nls) + 
  geom_point(aes(x = fitted, y = resids))+
  geom_hline(aes(yintercept = 0))

### pb
pb_spp_temp_boots_nls = sppBootsDf %>%
  group_by(n_rep) %>%
  do(model = nls(pb_y ~ a *tempC ^b, start = list(a = 1, b = 0.5), control = nls.control(maxiter = 1e3),  data = .),
     fitted = fitted(nls(pb_y ~ a *tempC ^b, start = list(a = 1, b = 0.5), control = nls.control(maxiter = 1e3),  data = .)),
     resids = rstandard(nls(pb_y ~ a *tempC ^b, start = list(a = 1, b = 0.5), control = nls.control(maxiter = 1e3),  data = .)))

pb_spp_temp_resids_nls = pb_spp_temp_boots_nls %>% 
  dplyr::select(n_rep, fitted, resids) %>% 
  group_by(n_rep) %>% 
  unnest_longer(c(fitted, resids)) %>% 
  dplyr::mutate(fitted_id = seq(1:6))

pb_spp_temp_resids_nls_summ = pb_spp_temp_resids_nls %>% 
  group_by(fitted_id) %>% 
  dplyr::summarise(fitted = median(fitted),
                   resids = median(resids))

nls_resid_fit_PB = ggplot(pb_spp_temp_resids_nls) + 
  geom_point(aes(x = fitted, y = resids), alpha = 0.5, color = 'grey')+
  geom_point(data = pb_spp_temp_resids_nls_summ, aes(x = fitted, resids), size = 4, color = 'black')+
  geom_hline(aes(yintercept = 0))

# output
png("./doc/revision/LMvGLM_plot.png", res = 400, height = 5, width = 5, units = 'in')
gridExtra::grid.arrange(gaus_resid_fit_PB, glm_resid_fit_pb, ncol = 1)
dev.off()
