##' .. content for \description{} (no empty lines) ..
##'
##' .. content for \details{} ..
##'
##' @title
##' @param skew_analysis
##' @param random_rankings
plot_random_skew <- function(skew_analysis, random_rankings) {
  # 
  # random_summ_pb = random_rankings %>% group_by(site) %>%
  #   dplyr::summarise(across(.fns = list(mean = ~mean(.x, na.rm = TRUE),
  #                                            quant2.5 = ~quantile(.x, 0.025, na.rm = TRUE),
  #                                            quant25 = ~quantile(.x, 0.25, na.rm = TRUE),
  #                                            quant50 = ~quantile(.x, 0.50, na.rm = TRUE),
  #                                            quant75 = ~quantile(.x, 0.75, na.rm = TRUE),
  #                                            quant97.5 = ~quantile(.x, 0.975, na.rm = TRUE)))) %>%
  #   dplyr::select(site, matches("mean|quant2.5|quant25|quant50|quant75|quant97.5")) %>%
  #   pivot_longer(-site,names_to = 'variable', values_to = 'value') %>%
  #   dplyr::mutate(stat = stringr::str_extract_all(variable, '(?<=skew_)(.*)','\\1'),
  #                 group = "random", 
  #                 measure = "pb" )
  # random_summ_M = random_summ_pb %>% dplyr::mutate(measure = "M")
  # random_summ = random_summ_M %>% bind_rows(random_summ_pb)

  # skew_long = skew_analysis %>% rlist::list.subset(grepl("summ",names(.), ignore.case = TRUE)) %>% 
  #   map(~.x %>% select(site, matches("mean|quant2.5|quant25|quant50|quant75|quant97.5")) %>% select(-matches("prob"))) %>%
  #   bind_rows %>% pivot_longer(-site, names_to = 'variable', values_to = 'value') %>%
  #   na.omit %>%
  #   dplyr::mutate(stat = stringr::str_extract_all(variable, '(?<=skew_)(.*)','\\1'),
  #                 group = "empirical",
  #                 # stat = stringr::str_remove_all(stat, 'prob_'),
  #                 measure = stringr::str_extract_all(variable, '(^[:alpha:]{1,2})'),
  #                 measure = unlist(measure)) %>%
  #   bind_rows(random_summ) %>%
  #   select(site, measure, group, stat, value) %>% 
  #   pivot_wider(names_from = 'stat', values_from = 'value') %>%
  #   dplyr::mutate(site = factor(site, levels = rev(names(stream_order_list))))
    
# skew_long %>%
#   ggplot(aes(y = mean, x = site)) +
#   geom_errorbar(aes(ymin = quant25, ymax = quant75, color = site), width = 0, size = 1.5, positio = position_dodge(width = 1)) +
#   geom_errorbar(aes(ymin = quant2.5, ymax = quant97.5, color = site), width= 0, size = 1, position = position_dodge(width = 1)) +
#   geom_point(aes(color = site, shape = group), size = 3, position = position_dodge(width = 1) ) +
#   scale_color_manual(values = rev(ocecolors[["temperature"]][oce_temp_pos]), labels = rev(stream_temp_labels))+
#   facet_grid(~measure)
  
  pb_labs = data.frame(site = stream_order,
                       labs = c("B",rep(NA_character_,5)))
  
skew_analysis[['pb_skew_percs']] %>% bind_rows(.id = 'site') %>%
    pivot_longer(-site, names_to = "boot_id", values_to = "skew_perc") %>%
    dplyr::select(-boot_id) %>%
    dplyr::mutate(site = factor(site, levels =names(stream_order_list))) %>%
    ggplot(aes(x = skew_perc )) +
    geom_vline(xintercept = c(0.1,0.9), color = 'red', size = 1, alpha = 0.7)+
    geom_histogram(aes(fill = site, y = ..count..), color = NA, alpha = 0.8, bins = 50)+
    scale_x_continuous(name = "Percentile of permuted set", limits = c(-0.01,1.01), expand = c(0.01,0.01))+
    scale_y_continuous(name = "# of observations", expand = c(0.001,0.001))+
    scale_color_manual(values = ocecolors[["temperature"]][oce_temp_pos], labels = stream_temp_labels) +
    scale_fill_manual(values = ocecolors[["temperature"]][oce_temp_pos], labels = stream_temp_labels) +
  geom_text(data = pb_labs, aes(x = 0, y = Inf, vjust = 1, family = 'serif'), label = c("B","","","","",""))+
    theme_tufte(ticks = TRUE) +
    geom_rangeframe(sides = "lb")+
    theme(legend.position = 'none', axis.title = element_blank()) +
    facet_wrap(~site, ncol = , scales = 'free_y',labeller = as_labeller(stream_temp_labels)) ->pb_perc_plot
    
m_labs = data.frame(site = stream_order,
                    labs = c("A",rep(NA,5)))

  skew_analysis[['M_skew_percs']] %>% bind_rows(.id = 'site') %>%
    pivot_longer(-site, names_to = "boot_id", values_to = "skew_perc") %>%
    dplyr::select(-boot_id) %>%
    dplyr::mutate(site = factor(site, levels =names(stream_order_list))) %>%
    ggplot(aes(x = skew_perc)) +
    geom_vline(xintercept = c(0.1,0.9), color = 'red', size = 1, alpha = 0.7)+
    geom_histogram(aes( fill = site, y = ..count..), color = NA, alpha = 0.8, bins = 50)+
    scale_x_continuous(name = "Percentile of permuted set", limits = c(-0.01,1.01), expand = c(0.01,0.01))+
    scale_y_continuous(name = "# of observations", expand = c(0.001,0.001))+
    scale_color_manual(values = ocecolors[["temperature"]][oce_temp_pos], labels = stream_temp_labels) +
    scale_fill_manual(values = ocecolors[["temperature"]][oce_temp_pos], labels = stream_temp_labels) +
    geom_text(data = pb_labs, aes(x = 0, y = Inf, vjust = 1, family = 'serif'), label = c("A","","","","",""))+
    theme_tufte(ticks = TRUE) +
    geom_rangeframe(sides = "lb")+
    theme(legend.position = 'none', axis.title = element_blank()) +
    facet_wrap(~site, ncol = , scales = 'free_y',labeller = as_labeller(stream_temp_labels)) ->M_perc_plot
  
  
skew_boots = skew_analysis %>% rlist::list.subset(grepl("boots",names(.), ignore.case = TRUE)) %>%
  Reduce(merge, .) %>% dplyr::mutate(site = factor(site, levels = names(stream_order_list)))

random_rankings %>%
  dplyr::mutate(site = factor(site, levels =names(stream_order_list))) %>%
  ggplot(aes(x = rand_rank_skew)) +
  geom_histogram(aes(color = site, y = ..density..), fill = NA, bins = 50)+
  scale_x_continuous(name = expression("Skew statistic"~italic(Sk)))+
  geom_histogram(data = skew_boots, aes(x = pb_y_skew, y = ..density..,color = site, fill = site), linetype = 'dashed', inherit.aes = FALSE, bins = 50, alpha = 0.5)+
  scale_color_manual(values = ocecolors[["temperature"]][oce_temp_pos], labels = stream_temp_labels) +
  scale_fill_manual(values = ocecolors[["temperature"]][oce_temp_pos], labels = stream_temp_labels) +
  guides(linetype = "none")+
  theme_tufte(ticks = TRUE) +
  geom_rangeframe(sides = "lb")+
  theme(legend.position = 'none') +
  facet_wrap(~site, ncol = , scales = 'free_y') -> pb_skew_plot

random_rankings %>%
  dplyr::mutate(site = factor(site, levels =names(stream_order_list))) %>%
  ggplot(aes(x = rand_rank_skew)) +
  geom_histogram(aes(color = site, y = ..density..), fill = NA, bins = 50)+
  scale_x_continuous(name = expression("Skew statistic"~italic(Sk)))+
  geom_histogram(data = skew_boots, aes(x = M_mg_ind_skew, y = ..density..,color = site, fill = site), linetype = 'dashed', inherit.aes = FALSE, bins = 50, alpha = 0.5)+
  scale_color_manual(values = ocecolors[["temperature"]][oce_temp_pos], labels = stream_temp_labels)+
  scale_fill_manual(values = ocecolors[["temperature"]][oce_temp_pos], labels = stream_temp_labels)+
  guides(linetype = "none")+
  theme_tufte(ticks = TRUE) +
  geom_rangeframe(sides = "lb")+
  theme(legend.position = 'none') +
  facet_wrap(~site, ncol = , scales = 'free_y') -> M_skew_plot

random_summ = random_rankings %>%
  group_by(site) %>%
  dplyr::summarise(across(everything(), list(mean = ~mean(.x, na.rm = TRUE),
                                               median = ~median(.x, na.rm = TRUE),
                                               quant2.5 = ~quantile(.x, 0.025, na.rm = TRUE),
                                               quant97.5 = ~quantile(.x, 0.975, na.rm = TRUE),
                                               HPDI_25up = ~rethinking::HPDI(.x, prob = 0.25)[2],
                                               HPDI_25dn = ~rethinking::HPDI(.x, prob = 0.25)[1],
                                               HPDI_50up = ~rethinking::HPDI(.x, prob = 0.5)[2],
                                               HPDI_50dn = ~rethinking::HPDI(.x, prob = 0.5)[1]),
                          .names = "random_{.fn}"))

# random_rankings %>%
# ggplot()+
#   geom_density(aes(x = rand_rank_skew, y = ..scaled..), alpha = 0.5) +
#   facet_wrap(~site)

 return(list(M_skew_plot = M_skew_plot, pb_skew_plot = pb_skew_plot, M_perc_plot = M_perc_plot, pb_perc_plot = pb_perc_plot))
  
}

