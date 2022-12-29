##' .. content for \description{} (no empty lines) ..
##'
##' .. content for \details{} ..
##'
##' @title
##' @param temperature_stats
plot_skew_temperature <- function(temperature_stats, n_id = 2e2) {
  set.seed(123)
  n_draws = sample(1:6000, n_id, replace = FALSE)
  
  m_skew_summ = temperature_stats[["m_skew_df"]] %>%
    group_by(site) %>%
    dplyr::summarise(tempC= unique(tempC),
                     across(c(M_mg_ind_skew), list(mean = ~mean(.x, na.rm = TRUE),
                                               quant2.5 = ~quantile(.x, 0.05, na.rm = TRUE),
                                               quant25 = ~quantile(.x, 0.25, na.rm = TRUE),
                                               HPDI_50up = ~rethinking::HPDI(.x, 0.5)[2],
                                               HPDI_50dn = ~rethinking::HPDI(.x, 0.5)[1],
                                               HPDI_90up = ~rethinking::HPDI(.x, 0.89)[2],
                                               HPDI_90dn = ~rethinking::HPDI(.x, 0.89)[1],
                                               median = ~median(.x, na.rm = TRUE),
                                               quant75 = ~quantile(.x, 0.75, na.rm = TRUE),
                                               quant97.5 = ~quantile(.x, 0.95, na.rm = TRUE)))) %>%
    dplyr::mutate(site = factor(site, levels = names(stream_order_list)))
  
  m_temp_skew_plot = ggplotGrob(m_skew_summ %>%
                                   dplyr::mutate(site = factor(site, levels = names(rev(stream_order_list)))) %>%
                                   ggplot()+
                                   geom_errorbar( aes(y = site, xmin = M_mg_ind_skew_HPDI_90dn, xmax = M_mg_ind_skew_HPDI_90up, color = site), size = 1, width = 0)+
                                   geom_errorbar( aes(y = site, xmin = M_mg_ind_skew_HPDI_50dn, xmax = M_mg_ind_skew_HPDI_50up, color = site), size = 1.5, width = 0)+
                                   geom_point(aes(y = site, x = M_mg_ind_skew_median), size = 2, color = 'black')+
                                   scale_x_continuous(name = expression(""*italic(Sk[flux])), limits = c(-1,1), expand = c(0,0.004) ) +
                                   scale_y_discrete(labels = rev(stream_temp_labels))+
                                   annotate('text', label = "A", x = -0.99, y = 1, family = 'serif', hjust = 0, vjust = 0, size = 4)+
                                   scale_color_manual(values = rev(ocecolors[['temperature']][oce_temp_pos]), labels = names(stream_temp_labels))+
                                   theme(legend.position = 'none',
                                         axis.title.y = element_blank()))#;grid.draw(m_temp_skew_plot)
  
  
  m_probs_df = temperature_stats[["m_probs_df"]]

    
    m_probs_summ = m_probs_df %>%
    group_by(site) %>%
    dplyr::summarise(tempC= unique(tempC),
                     across(c(M_skew_prob), list(mean = ~mean(.x, na.rm = TRUE),
                                                  quant2.5 = ~quantile(.x, 0.025, na.rm = TRUE),
                                                  quant25 = ~quantile(.x, 0.25, na.rm = TRUE),
                                                  HPDI_50up = ~rethinking::HPDI(.x, 0.5)[2],
                                                  HPDI_50dn = ~rethinking::HPDI(.x, 0.5)[1],
                                                  HPDI_90up = ~rethinking::HPDI(.x, 0.89)[2],
                                                  HPDI_90dn = ~rethinking::HPDI(.x, 0.89)[1],
                                                  median = ~median(.x,  na.rm = TRUE),
                                                  quant75 = ~quantile(.x, 0.75, na.rm = TRUE),
                                                  quant97.5 = ~quantile(.x, 0.975, na.rm = TRUE)))) %>%
    dplyr::mutate(site = factor(site, levels = names(stream_order_list)))
  
    m_probs_plot_df = temperature_stats[["m_probs_temp_pred"]] %>% dplyr::filter(.draw %in% 41)#n_draws)
    
  set.seed(123)
  m_probs_plot = ggplotGrob(m_probs_summ %>%
                               ggplot(aes(x = tempC, y = M_skew_prob_median)) +
                                # geom_line(data = m_probs_plot_df, aes( x = tempC, y = .prediction, group = .draw))+#, alpha = 0.1, color = 'lightgrey') +
                               geom_errorbar(data = m_probs_summ, aes(x = tempC, ymin = M_skew_prob_HPDI_90dn, ymax = M_skew_prob_HPDI_90up, color = site), size = 1, width = 0, inherit.aes = FALSE)+
                               geom_errorbar(data = m_probs_summ, aes(x = tempC, ymin = M_skew_prob_HPDI_50dn, ymax = M_skew_prob_HPDI_50up, color = site), size = 1.5, width = 0, inherit.aes = FALSE)+
                               geom_point(data = m_probs_summ, aes(x = tempC, y = M_skew_prob_median, fill = site), size = 2, color = 'black', inherit.aes = FALSE) +
                               scale_x_continuous(name = expression("Temperature ("~degree*C~")"), expand = c(0,0.004))+
                               scale_y_continuous(name = expression("Pr("~italic(Sk[flux])~">="~italic(x)*")"), expand = c(0,0.004)) +
                               coord_cartesian(xlim = c(0,30), ylim = c(0,1))+
                               scale_fill_manual(values = ocecolors[['temperature']][oce_temp_pos], labels = stream_temp_labels)+
                               scale_colour_manual(values = ocecolors[['temperature']][oce_temp_pos], labels = stream_temp_labels)+
                               annotate('text', label = "B", x = 0, y = 0, family = 'serif', hjust = 0, vjust = 0, size = 4)+
                               theme_tufte(ticks = TRUE) +
                               geom_rangeframe(sides = "lb")+
                               theme(legend.title = element_blank(),
                                     legend.position = "none",
                                     legend.justification = c(0,0)));#grid.draw(m_probs_plot)
  
  m_temp_coef_plot = ggplotGrob(temperature_stats[["m_probs_temp_coefs"]] %>%
                                   ggplot()+
                                   geom_density(aes(x = b_tempC_stand, y = ..density..), size = 1.0, fill = 'lightgrey', alpha = 0.5)+
                                   geom_vline(aes(xintercept = 0), size = 1.2, color = 'red')+
                                   scale_x_continuous(name = expression("Change in"~italic("Pr")*"("*italic(Sk[flux])~">="~italic("x")*") with increased temperature"),
                                                      expand = c(0.001,0.001), limits = c(NA, NA))+
                                   scale_y_continuous(expand = c(0.001,0.001), limits = c(0,NA))+
                                   theme_tufte(ticks = TRUE));#grid.draw(m_temp_coef_plot)
  
  ## pb -----
  pb_skew_summ = temperature_stats[["pb_skew_df"]] %>%
      group_by(site) %>%
      dplyr::summarise(tempC= unique(tempC),
                       across(c(pb_y_skew), list(mean = ~mean(.x, na.rm = TRUE),
                                                    quant2.5 = ~quantile(.x, 0.05, na.rm = TRUE),
                                                    quant25 = ~quantile(.x, 0.25, na.rm = TRUE),
                                                    HPDI_50up = ~rethinking::HPDI(.x, 0.5)[2],
                                                    HPDI_50dn = ~rethinking::HPDI(.x, 0.5)[1],
                                                    HPDI_90up = ~rethinking::HPDI(.x, 0.89)[2],
                                                    HPDI_90dn = ~rethinking::HPDI(.x, 0.89)[1],
                                                    median = ~median(.x, na.rm = TRUE),
                                                    quant75 = ~quantile(.x, 0.75, na.rm = TRUE),
                                                    quant97.5 = ~quantile(.x, 0.95, na.rm = TRUE)))) %>%
      dplyr::mutate(site = factor(site, levels = names(stream_order_list)))
  
  # pb_temp_skew_plot = pb_skew_summ %>%
  #   dplyr::mutate(site = factor(site, levels = names(rev(stream_order_list)))) %>%
  #   ggplot()+
  #   geom_errorbar( aes(x = site, ymin = pb_y_skew_HPDI_90dn, ymax = pb_y_skew_HPDI_90up, color = site), size = 1, width = 0, inherit.aes = FALSE)+
  #   geom_errorbar( aes(x = site, ymin = pb_y_skew_HPDI_50dn, ymax = pb_y_skew_HPDI_50up, color = site), size = 1.5, width = 0, inherit.aes = FALSE)+ 
  #   geom_point(aes(x = site, y = pb_y_skew_mean, fill = site), size = 2, color = 'black') +
  #   scale_x_discrete(labels = stream_temp_labels)+
  #   # theme_tufte(ticks = TRUE) +
  #   scale_y_continuous(name = expression(""*italic(Sk[flux])), limits = c(-1,1), expand = c(0,0.004)) +
  #   scale_fill_manual(values = rev(ocecolors[['temperature']][oce_temp_pos]), labels = stream_temp_labels) +
  #   scale_color_manual(values = rev(ocecolors[['temperature']][oce_temp_pos]), labels = stream_temp_labels)+
  #   theme(legend.position = 'none',
  #         axis.title.x = element_blank());pb_temp_skew_plot
  
  pb_temp_skew_plot = ggplotGrob(pb_skew_summ %>%
    dplyr::mutate(site = factor(site, levels = names(rev(stream_order_list)))) %>%
    ggplot()+
    geom_errorbar( aes(y = site, xmin = pb_y_skew_HPDI_90dn, xmax = pb_y_skew_HPDI_90up, color = site), size = 1, width = 0)+
    geom_errorbar( aes(y = site, xmin = pb_y_skew_HPDI_50dn, xmax = pb_y_skew_HPDI_50up, color = site), size = 1.5, width = 0)+
    geom_point(aes(y = site, x = pb_y_skew_median), size = 2, color = 'black')+
    scale_x_continuous(name = expression(""*italic(Sk[flux])), limits = c(-1,1), expand = c(0,0.004) ) +
    scale_y_discrete(labels = rev(stream_temp_labels))+
    annotate('text', label = "C", x = -Inf, y = 1, family = 'serif', hjust = 0, vjust = 0, size = 4)+
    scale_color_manual(values = rev(ocecolors[['temperature']][oce_temp_pos]), labels = names(stream_temp_labels))+
    theme(legend.position = 'none',
          axis.title.y = element_blank()));#grid.draw(pb_temp_skew_plot)
    
  pb_probs_df = temperature_stats[["pb_probs_df"]]
  pb_probs_summ = pb_probs_df %>%
    group_by(site) %>%
    dplyr::summarise(tempC= unique(tempC),
                     across(c(pb_skew_prob), list(mean = ~mean(.x, na.rm = TRUE),
                                                        quant2.5 = ~quantile(.x, 0.025, na.rm = TRUE),
                                                        quant25 = ~quantile(.x, 0.25, na.rm = TRUE),
                                                        HPDI_50up = ~rethinking::HPDI(.x, 0.5)[2],
                                                        HPDI_50dn = ~rethinking::HPDI(.x, 0.5)[1],
                                                        HPDI_90up = ~rethinking::HPDI(.x, 0.89)[2],
                                                        HPDI_90dn = ~rethinking::HPDI(.x, 0.89)[1],
                                                        median = ~median(.x,  na.rm = TRUE),
                                                        quant75 = ~quantile(.x, 0.75, na.rm = TRUE),
                                                        quant97.5 = ~quantile(.x, 0.975, na.rm = TRUE)))) %>%
    dplyr::mutate(site = factor(site, levels = names(stream_order_list)))
  
  pb_probs_plot_df = temperature_stats[["pb_probs_temp_pred"]] %>% dplyr::filter(.draw %in% n_draws)
  
  set.seed(123)
  pb_probs_plot = ggplotGrob(pb_probs_summ %>%
  ggplot(aes(x = tempC, y = pb_skew_prob_median)) +
    # geom_line(data = pb_probs_plot_df, aes(x = tempC, y = .prediction, group = .draw),
              # alpha = 0.1, color = 'lightgrey')+
    geom_errorbar(data = pb_probs_summ, aes(x = tempC, ymin = pb_skew_prob_HPDI_90dn, ymax = pb_skew_prob_HPDI_90up, color = site), size = 1, width = 0, inherit.aes = FALSE)+
    geom_errorbar(data = pb_probs_summ, aes(x = tempC, ymin = pb_skew_prob_HPDI_50dn, ymax = pb_skew_prob_HPDI_50up, color = site), size = 1.5, width = 0, inherit.aes = FALSE)+
    geom_point(data = pb_probs_summ, aes(x = tempC, y = pb_skew_prob_median, fill = site), size = 2, color = 'black', inherit.aes = FALSE) +
    scale_x_continuous(name = expression("Temperature ("~degree*C~")"), expand = c(0,0.004))+
    scale_y_continuous(name = expression("Pr("~italic(Sk[flux])~">="~italic(x)*")"), expand = c(0,0.004)) +
    coord_cartesian(xlim = c(0,30), ylim = c(0,1))+
    annotate('text', label = "D", x = 0, y = 0, family = 'serif', hjust = 0, vjust = 0, size = 4)+
    scale_fill_manual(values = ocecolors[['temperature']][oce_temp_pos], labels = stream_temp_labels)+
    scale_colour_manual(values = ocecolors[['temperature']][oce_temp_pos], labels = stream_temp_labels)+
    theme_tufte(ticks = TRUE) +
    geom_rangeframe(sides = "lb")+
    theme(legend.title = element_blank(),
          legend.position = "none",
          legend.justification = c(0,0)));#grid.draw(pb_probs_plot)
  
  pb_temp_coef_plot = ggplotGrob(temperature_stats[["pb_probs_temp_coefs"]] %>%
    ggplot()+
    geom_density(aes(x = b_tempC_stand, y = ..density..), size = 1.0, fill = 'lightgrey', alpha = 0.5)+
    geom_vline(aes(xintercept = 0), size = 1.2, color = 'red')+
    scale_x_continuous(name = expression("Change in"~italic("Pr")*"("*italic(Sk[flux])~">="~italic("x")*") with increased temperature"),
                       expand = c(0.001,0.001), limits = c(-0.04, 0))+
    scale_y_continuous(expand = c(0.001,0.001), limits = c(0,NA))+
    theme_tufte(ticks = TRUE));#grid.draw(pb_temp_coef_plot)
  
  
    
    
  
 top_plot = grid.arrange(grobs = list(m_temp_skew_plot, m_probs_plot), 
                         layout = c(1,1,1,2,2,2,2), nrow = 1)
 bottom_plot = grid.arrange(grobs = list(pb_temp_skew_plot, pb_probs_plot),
                         layout = c(1,1,1,2,2,2,2), nrow = 1)
 
 full_plot = grid.arrange(top_plot, bottom_plot, nrow = 2)
 return(full_plot)
}
