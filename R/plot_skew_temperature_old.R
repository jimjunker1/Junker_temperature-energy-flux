##' .. content for \description{} (no empty lines) ..
##'
##' .. content for \details{} ..
##'
##' @title
##' @param temperature_stats
plot_skew_temperature_old <- function(temperature_stats, n_id = 2e2) {
  
  n_draws = sample(1:6000, n_id, replace = FALSE)
  
  # m_skew_summ = temperature_stats[["m_skew_df"]] %>%
  #   group_by(site) %>%
  #   dplyr::summarise(tempC= unique(tempC),
  #                    across(c(M_mg_ind_skew), list(mean = ~mean(.x, na.rm = TRUE),
  #                                              quant2.5 = ~quantile(.x, 0.05, na.rm = TRUE),
  #                                              quant25 = ~quantile(.x, 0.25, na.rm = TRUE),
  #                                              HPDI_50up = ~rethinking::HPDI(.x, 0.5)[2],
  #                                              HPDI_50dn = ~rethinking::HPDI(.x, 0.5)[1],
  #                                              HPDI_90up = ~rethinking::HPDI(.x, 0.9)[2],
  #                                              HPDI_90dn = ~rethinking::HPDI(.x, 0.9)[1],
  #                                              quant50 = ~quantile(.x, 0.50, na.rm = TRUE),
  #                                              quant75 = ~quantile(.x, 0.75, na.rm = TRUE),
  #                                              quant97.5 = ~quantile(.x, 0.95, na.rm = TRUE)))) %>%
  #   dplyr::mutate(site = factor(site, levels = names(stream_order_list)))
  # 
  # m_temp_skew_plot = m_skew_summ %>%
  #   dplyr::mutate(site = factor(site, levels = names(rev(stream_order_list)))) %>%
  #   ggplot()+
  #   geom_errorbar( aes(x = site, ymin = M_mg_ind_skew_HPDI_90dn, ymax = M_mg_ind_skew_HPDI_90up, color = site), size = 1, width = 0, inherit.aes = FALSE)+
  #   geom_errorbar( aes(x = site, ymin = M_mg_ind_skew_HPDI_50dn, ymax = M_mg_ind_skew_HPDI_50up, color = site), size = 1.5, width = 0, inherit.aes = FALSE)+ 
  #   geom_point(aes(x = site, y = M_mg_ind_skew_mean, fill = site), size = 2, color = 'black') +
  #   scale_x_discrete(labels = stream_temp_labels)+
  #   # theme_tufte(ticks = TRUE) +
  #   scale_y_continuous(name = expression(""*italic(Sk[flux])), limits = c(-1,1), expand = c(0,0.004)) +
  #   scale_fill_manual(values = rev(ocecolors[['temperature']][oce_temp_pos]), labels = stream_temp_labels) +
  #   scale_color_manual(values = rev(ocecolors[['temperature']][oce_temp_pos]), labels = stream_temp_labels)+
  #   theme(legend.position = 'none',
  #         axis.title.x = element_blank())#;m_temp_skew_plot
  # 
  # m_probs_df = temperature_stats[["m_probs_df"]]
  # m_probs_summ = m_probs_df %>%
  #   group_by(site) %>%
  #   dplyr::summarise(tempC= unique(tempC),
  #                    across(c(M_skew_prob), list(mean = ~mean(.x, na.rm = TRUE),
  #                                                quant2.5 = ~quantile(.x, 0.025, na.rm = TRUE),
  #                                                quant25 = ~quantile(.x, 0.25, na.rm = TRUE),
  #                                                HPDI_50up = ~rethinking::HPDI(.x, 0.5)[2],
  #                                                HPDI_50dn = ~rethinking::HPDI(.x, 0.5)[1],
  #                                                HPDI_90up = ~rethinking::HPDI(.x, 0.9)[2],
  #                                                HPDI_90dn = ~rethinking::HPDI(.x, 0.9)[1],
  #                                                quant50 = ~quantile(.x, 0.50, na.rm = TRUE),
  #                                                quant75 = ~quantile(.x, 0.75, na.rm = TRUE),
  #                                                quant97.5 = ~quantile(.x, 0.975, na.rm = TRUE)))) %>%
  #   dplyr::mutate(site = factor(site, levels = names(stream_order_list)))
  # 
  # # set.seed(123)
  # m_probs_plot = m_probs_summ %>%
  #   # dplyr::filter(.draw %in% n_draws) %>%
  #   # ggplot(aes(x = tempC, y = .epred, group = .draw)) +
  #   # geom_line(alpha = 0.3, color = 'lightgrey') +
  #   # dplyr::mutate(site = factor(site, levels = names(stream_order_list))) %>%
  #   ggplot() +
  #   geom_line(data = temperature_stats[["m_probs_temp_pred"]] %>%
  #               dplyr::filter(.draw %in% n_draws),
  #             aes(x = tempC, y = .epred, group = .draw), alpha = 0.1, color = 'lightgrey') +
  #   # geom_line(data = pb_probs_concave_med, aes(x= tempC, y = `.fitted`), color = 'black', size = 3, inherit.aes = FALSE)+
  #   geom_errorbar(data = m_probs_summ, aes(x = tempC, ymin = M_skew_prob_HPDI_90dn, ymax = M_skew_prob_HPDI_90up, color = site), size = 1, width = 0, inherit.aes = FALSE)+
  #   geom_errorbar(data = m_probs_summ, aes(x = tempC, ymin = M_skew_prob_HPDI_50dn, ymax = M_skew_prob_HPDI_50up, color = site), size = 1.5, width = 0, inherit.aes = FALSE)+
  #   geom_point(data = m_probs_summ, aes(x = tempC, y = M_skew_prob_mean, fill = site), size = 2, color = 'black', inherit.aes = FALSE) +
  #   scale_x_continuous(name = expression("Temperature ("~degree*C~")"), expand = c(0,0.004))+
  #   scale_y_continuous(name = expression("Pr("~italic(Sk[flux])~">="~italic(x)*")"), expand = c(0,0.004)) +
  #   coord_cartesian(xlim = c(0,30), ylim = c(0,1))+
  #   scale_fill_manual(values = ocecolors[['temperature']][oce_temp_pos], labels = stream_temp_labels)+
  #   scale_colour_manual(values = ocecolors[['temperature']][oce_temp_pos], labels = stream_temp_labels)+
  #   # theme_tufte(ticks = TRUE) +
  #   # geom_rangeframe(data = pb_probs_summ, sides = "lb")+
  #   annotate("text", x = 28, hjust = 1, y = 0.25, vjust = 1, angle = 90, label = "Non-random")+
  #   annotate("segment", x = 29, xend = 29, y = 0.5, yend = 0, size = 1,
  #            arrow = arrow(length = unit(0.4,"cm"), type = 'open'))+
  #   annotate("text", x = 28, hjust = 0, y = 0.75, vjust = 1, angle = 90, label = "Random" )+
  #   annotate('segment', x = 29, xend = 29, y = 0.52, yend = 1, size = 1,
  #            arrow = arrow(length = unit(0.4, "cm"), type = 'open'))+
  #   theme(legend.title = element_blank(),
  #         legend.position = c(0, 0),
  #         legend.justification = c(0,0),
  #         legend.background = element_rect(fill = 'transparent', color = NA));m_probs_plot
  # 
  # m_temp_coef_plot = temperature_stats[['m_probs_temp_coefs']] %>%
  #   ggplot()+
  #   geom_density(aes(x = plogis(b_tempC_stand), y = ..scaled..))+
  #   scale_x_continuous(name= "Temperature coefficient", limits = c(0.49, 0.51))+
  #   scale_y_continuous(expand = c(0.01,0.01))+
  #   theme(axis.title.y = element_blank()); m_temp_coef_plot
  #   
  # m_temp_pred_plot = temperature_stats[["m_skew_temp_pred"]] %>%
  #   dplyr::filter(.draw %in% n_draws) %>%
  #   ggplot(aes(x = tempC, y = .epred, group = .draw)) +
  #   # geom_line(alpha = 0.3, color = 'lightgrey') +
  #   # geom_line(data = pb_probs_concave_med, aes(x= tempC, y = `.fitted`), color = 'black', size = 3, inherit.aes = FALSE)+
  #   geom_errorbar(data = m_skew_summ, aes(x = tempC, ymin = M_mg_ind_skew_HPDI_90dn, ymax = M_mg_ind_skew_HPDI_90up, color = site), size = 1, width = 0, inherit.aes = FALSE)+
  #   geom_errorbar(data = m_skew_summ, aes(x = tempC, ymin = M_mg_ind_skew_HPDI_50dn, ymax = M_mg_ind_skew_HPDI_50up, color = site), size = 1.5, width = 0, inherit.aes = FALSE)+ 
  #   geom_point(data = m_skew_summ, aes(x = tempC, y = M_mg_ind_skew_mean, fill = site), size = 2, color = 'black', inherit.aes = FALSE) +
  #   scale_x_continuous(name = expression("Temperature ("~degree*C~")"), limits = c(0,29), expand = c(0,0.004))+
  #   scale_y_continuous(name = expression("Pr("~italic(Sk[flux])~">="~italic(x)*")"), limits = c(-1,1), expand = c(0,0.004)) +
  #   # coord_cartesian(xlim = c(0,30), ylim = c(0,1))+
  #   scale_fill_manual(values = ocecolors[['temperature']][oce_temp_pos], labels = stream_temp_labels)+
  #   scale_colour_manual(values = ocecolors[['temperature']][oce_temp_pos], labels = stream_temp_labels)+
  #   theme_tufte(ticks = TRUE) +
  #   coord_cartesian(clip= "off")+
  #   geom_rangeframe(sides = "lb", inherit.aes = FALSE)+
  #   # annotate("text", x = 28, hjust = 1, y = 0.15, vjust = 1, angle = 90, label = "Non-random")+
  #   # annotate("segment", x = 29, xend = 29, y = 0.25, yend = 0, size = 1, 
  #   #          arrow = arrow(length = unit(0.4,"cm"), type = 'open'))+
  #   # annotate("text", x = 28, hjust = 1, y = 0.4, vjust = 1, angle = 90, label = "Random" )+
  #   # annotate('segment', x = 29, xend = 29, y = 0.27, yend = 0.5, size = 1,
  #   #          arrow = arrow(length = unit(0.4, "cm"), type = 'open'))+
  #   theme(legend.title = element_blank(),
  #         legend.position = c(0, 1),
  #         legend.justification = c(0,1)); m_temp_pred_plot
  
  pb_skew_summ = temperature_stats[["pb_skew_df"]] %>%
      group_by(site) %>%
      dplyr::summarise(tempC= unique(tempC),
                       across(c(pb_y_skew), list(mean = ~mean(.x, na.rm = TRUE),
                                                    quant2.5 = ~quantile(.x, 0.05, na.rm = TRUE),
                                                    quant25 = ~quantile(.x, 0.25, na.rm = TRUE),
                                                    HPDI_50up = ~rethinking::HPDI(.x, 0.5)[2],
                                                    HPDI_50dn = ~rethinking::HPDI(.x, 0.5)[1],
                                                    HPDI_90up = ~rethinking::HPDI(.x, 0.9)[2],
                                                    HPDI_90dn = ~rethinking::HPDI(.x, 0.9)[1],
                                                    quant50 = ~quantile(.x, 0.50, na.rm = TRUE),
                                                    quant75 = ~quantile(.x, 0.75, na.rm = TRUE),
                                                    quant97.5 = ~quantile(.x, 0.95, na.rm = TRUE)))) %>%
      dplyr::mutate(site = factor(site, levels = names(stream_order_list)))
  
  pb_temp_skew_plot = pb_skew_summ %>%
    dplyr::mutate(site = factor(site, levels = names(rev(stream_order_list)))) %>%
    ggplot()+
    geom_errorbar( aes(x = site, ymin = pb_y_skew_HPDI_90dn, ymax = pb_y_skew_HPDI_90up, color = site), size = 1, width = 0, inherit.aes = FALSE)+
    geom_errorbar( aes(x = site, ymin = pb_y_skew_HPDI_50dn, ymax = pb_y_skew_HPDI_50up, color = site), size = 1.5, width = 0, inherit.aes = FALSE)+ 
    geom_point(aes(x = site, y = pb_y_skew_mean, fill = site), size = 2, color = 'black') +
    scale_x_discrete(labels = stream_temp_labels)+
    # theme_tufte(ticks = TRUE) +
    scale_y_continuous(name = expression(""*italic(Sk[flux])), limits = c(-1,1), expand = c(0,0.004)) +
    scale_fill_manual(values = rev(ocecolors[['temperature']][oce_temp_pos]), labels = stream_temp_labels) +
    scale_color_manual(values = rev(ocecolors[['temperature']][oce_temp_pos]), labels = stream_temp_labels)+
    theme(legend.position = 'none',
          axis.title.x = element_blank());pb_temp_skew_plot
  
  # pb_temp_pred_plot = pb_skew_summ %>%
  #   ggplot() +
  #   geom_line(data = temperature_stats[["pb_probs_temp_pred"]] %>%
  #               dplyr::filter(.draw %in% n_draws),
  #             aes(x = tempC, y = .epred, group = .draw), alpha = 0.1, color = 'lightgrey') +
  #   # geom_line(alpha = 0.3, color = 'lightgrey') +
  #   # geom_line(data = pb_probs_concave_med, aes(x= tempC, y = `.fitted`), color = 'black', size = 3, inherit.aes = FALSE)+
  #   geom_errorbar( aes(x = tempC, ymin = pb_y_skew_HPDI_90dn, ymax = pb_y_skew_HPDI_90up, color = site), size = 1, width = 0, inherit.aes = FALSE)+
  #   geom_errorbar( aes(x = tempC, ymin = pb_y_skew_HPDI_50dn, ymax = pb_y_skew_HPDI_50up, color = site), size = 1.5, width = 0, inherit.aes = FALSE)+ 
  #   geom_point( aes(x = tempC, y = pb_y_skew_mean, fill = site), size = 2, color = 'black', inherit.aes = FALSE) +
  #   scale_x_continuous(name = expression("Temperature ("~degree*C~")"), limits = c(0,29), expand = c(0,0.004))+
  #   scale_y_continuous(name = expression("Pr("~italic(Sk[flux])~">="~italic(x)*")"), limits = c(-1,1), expand = c(0,0.004)) +
  #   # coord_cartesian(xlim = c(0,30), ylim = c(0,1))+
  #   scale_fill_manual(values = ocecolors[['temperature']][oce_temp_pos], labels = stream_temp_labels)+
  #   scale_colour_manual(values = ocecolors[['temperature']][oce_temp_pos], labels = stream_temp_labels)+
  #   theme_tufte(ticks = TRUE) +
  #   coord_cartesian(clip= "off")+
  #   geom_rangeframe(sides = "lb", inherit.aes = FALSE)+
  #   # annotate("text", x = 28, hjust = 1, y = 0.15, vjust = 1, angle = 90, label = "Non-random")+
  #   # annotate("segment", x = 29, xend = 29, y = 0.25, yend = 0, size = 1, 
  #   #          arrow = arrow(length = unit(0.4,"cm"), type = 'open'))+
  #   # annotate("text", x = 28, hjust = 1, y = 0.4, vjust = 1, angle = 90, label = "Random" )+
  #   # annotate('segment', x = 29, xend = 29, y = 0.27, yend = 0.5, size = 1,
  #   #          arrow = arrow(length = unit(0.4, "cm"), type = 'open'))+
  #   theme(legend.title = element_blank(),
  #         legend.position = c(1, 0),
  #         legend.justification = c(1,0)); pb_temp_pred_plot
  
  # m_probs_df = temperature_stats[["m_probs_df"]]
  # m_probs_summ = m_probs_df %>%
  #   group_by(site) %>%
  #   dplyr::summarise(tempC= unique(tempC),
  #                    across(c(M_skew_prob), list(mean = ~mean(.x, na.rm = TRUE),
  #                                                 quant2.5 = ~quantile(.x, 0.025, na.rm = TRUE),
  #                                                 quant25 = ~quantile(.x, 0.25, na.rm = TRUE),
  #                                                 HPDI_50up = ~rethinking::HPDI(.x, 0.5)[2],
  #                                                 HPDI_50dn = ~rethinking::HPDI(.x, 0.5)[1],
  #                                                 HPDI_90up = ~rethinking::HPDI(.x, 0.9)[2],
  #                                                 HPDI_90dn = ~rethinking::HPDI(.x, 0.9)[1],
  #                                                 quant50 = ~quantile(.x, 0.50, na.rm = TRUE),
  #                                                 quant75 = ~quantile(.x, 0.75, na.rm = TRUE),
  #                                                 quant97.5 = ~quantile(.x, 0.975, na.rm = TRUE)))) %>%
  #   dplyr::mutate(site = factor(site, levels = names(stream_order_list)))
  # 
  # set.seed(123)
  # m_probs_plot = m_probs_summ %>%
  #   # dplyr::filter(.draw %in% n_draws) %>%
  #   # ggplot(aes(x = tempC, y = .epred, group = .draw)) +
  #   # geom_line(alpha = 0.3, color = 'lightgrey') +
  #   # dplyr::mutate(site = factor(site, levels = names(stream_order_list))) %>%
  #   ggplot() +
  #   # geom_line(alpha = 0.1, color = 'lightgrey') +
  #   # geom_line(data = pb_probs_concave_med, aes(x= tempC, y = `.fitted`), color = 'black', size = 3, inherit.aes = FALSE)+
  #   geom_errorbar(data = m_probs_summ, aes(x = tempC, ymin = M_skew_prob_HPDI_90dn, ymax = M_skew_prob_HPDI_90up, color = site), size = 1, width = 0, inherit.aes = FALSE)+
  #   geom_errorbar(data = m_probs_summ, aes(x = tempC, ymin = M_skew_prob_HPDI_50dn, ymax = M_skew_prob_HPDI_50up, color = site), size = 1.5, width = 0, inherit.aes = FALSE)+
  #   geom_point(data = m_probs_summ, aes(x = tempC, y = M_skew_prob_mean, fill = site), size = 2, color = 'black', inherit.aes = FALSE) +
  #   scale_x_continuous(name = expression("Temperature ("~degree*C~")"), expand = c(0,0.004))+
  #   scale_y_continuous(name = expression("Pr("~italic(Sk[flux])~">="~italic(x)*")"), expand = c(0,0.004)) +
  #   coord_cartesian(xlim = c(0,30), ylim = c(0,1))+
  #   scale_fill_manual(values = ocecolors[['temperature']][oce_temp_pos], labels = stream_temp_labels)+
  #   scale_colour_manual(values = ocecolors[['temperature']][oce_temp_pos], labels = stream_temp_labels)+
  #   theme_tufte(ticks = TRUE) +
  #   geom_rangeframe(data = pb_probs_summ, sides = "lb")+
  #   annotate("text", x = 28, hjust = 1, y = 0.25, vjust = 1, angle = 90, label = "Non-random")+
  #   annotate("segment", x = 29, xend = 29, y = 0.5, yend = 0, size = 1,
  #            arrow = arrow(length = unit(0.4,"cm"), type = 'open'))+
  #   annotate("text", x = 28, hjust = 0, y = 0.75, vjust = 1, angle = 90, label = "Random" )+
  #   annotate('segment', x = 29, xend = 29, y = 0.52, yend = 1, size = 1,
  #            arrow = arrow(length = unit(0.4, "cm"), type = 'open'))+
  #   theme(legend.title = element_blank(),
  #         legend.position = c(0, 0),
  #         legend.justification = c(0,0));m_probs_plot
  
  pb_probs_df = temperature_stats[["pb_probs_df"]]
  pb_probs_summ = pb_probs_df %>%
    group_by(site) %>%
    dplyr::summarise(tempC= unique(tempC),
                     across(c(pb_skew_prob), list(mean = ~mean(.x, na.rm = TRUE),
                                                        quant2.5 = ~quantile(.x, 0.025, na.rm = TRUE),
                                                        quant25 = ~quantile(.x, 0.25, na.rm = TRUE),
                                                        HPDI_50up = ~rethinking::HPDI(.x, 0.5)[2],
                                                        HPDI_50dn = ~rethinking::HPDI(.x, 0.5)[1],
                                                        HPDI_90up = ~rethinking::HPDI(.x, 0.9)[2],
                                                        HPDI_90dn = ~rethinking::HPDI(.x, 0.9)[1],
                                                        quant50 = ~quantile(.x, 0.50, na.rm = TRUE),
                                                        quant75 = ~quantile(.x, 0.75, na.rm = TRUE),
                                                        quant97.5 = ~quantile(.x, 0.975, na.rm = TRUE)))) %>%
    dplyr::mutate(site = factor(site, levels = names(stream_order_list)))
  
  

  # pb_probs_concave_pred = temperature_stats[["pb_probs_concave_pred"]]
  # pb_probs_concave_med = temperature_stats[["pb_probs_concave_med"]]

  set.seed(123)
  pb_probs_plot = pb_probs_summ %>%
    # dplyr::filter(.draw %in% n_draws) %>%
    # ggplot(aes(x = tempC, y = .epred, group = .draw)) +
    # geom_line(alpha = 0.3, color = 'lightgrey') +
    # dplyr::mutate(site = factor(site, levels = names(stream_order_list))) %>%
  ggplot(aes(x = tempC, y = pb_skew_prob_mean)) +
    # geom_line(alpha = 0.1, color = 'lightgrey') +
    # geom_line(data = pb_probs_concave_med, aes(x= tempC, y = `.fitted`), color = 'black', size = 3, inherit.aes = FALSE)+
    geom_errorbar(data = pb_probs_summ, aes(x = tempC, ymin = pb_skew_prob_HPDI_90dn, ymax = pb_skew_prob_HPDI_90up, color = site), size = 1, width = 0, inherit.aes = FALSE)+
    geom_errorbar(data = pb_probs_summ, aes(x = tempC, ymin = pb_skew_prob_HPDI_50dn, ymax = pb_skew_prob_HPDI_50up, color = site), size = 1.5, width = 0, inherit.aes = FALSE)+
    geom_point(data = pb_probs_summ, aes(x = tempC, y = pb_skew_prob_mean, fill = site), size = 2, color = 'black', inherit.aes = FALSE) +
    scale_x_continuous(name = expression("Temperature ("~degree*C~")"), expand = c(0,0.004))+
    scale_y_continuous(name = expression("Pr("~italic(Sk[flux])~">="~italic(x)*")"), expand = c(0,0.004)) +
    coord_cartesian(xlim = c(0,30), ylim = c(0,1))+
    scale_fill_manual(values = ocecolors[['temperature']][oce_temp_pos], labels = stream_temp_labels)+
    scale_colour_manual(values = ocecolors[['temperature']][oce_temp_pos], labels = stream_temp_labels)+
    theme_tufte(ticks = TRUE) +
    geom_rangeframe(sides = "lb")+
    # annotate("text", x = 28, hjust = 1, y = 0.25, vjust = 1, angle = 90, label = "Non-random")+
    # annotate("segment", x = 29, xend = 29, y = 0.5, yend = 0, size = 1,
    #          arrow = arrow(length = unit(0.4,"cm"), type = 'open'))+
    # annotate("text", x = 28, hjust = 0, y = 0.75, vjust = 1, angle = 90, label = "Random" )+
    # annotate('segment', x = 29, xend = 29, y = 0.52, yend = 1, size = 1,
    #          arrow = arrow(length = unit(0.4, "cm"), type = 'open'))+
    theme(legend.title = element_blank(),
          legend.position = c(0, 0),
          legend.justification = c(0,0));pb_probs_plot
  
  
 top_plot = grid.arrange(pb_temp_skew_plot, pb_probs_plot, pb_temp_coef_plot, layout = c(1,1,2,2,2,3,3))
 return(top_plot)
}
