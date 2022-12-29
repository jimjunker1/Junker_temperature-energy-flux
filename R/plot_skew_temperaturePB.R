#' .. content for \description{} (no empty lines) ..
#'
#' .. content for \details{} ..
#'
#' @title
#' @param lorenz_analysis
#' @param spp_rankings_summary
#' @param temperature_stats
#' @param n_id
plot_skew_temperaturePB <- function(lorenz_analysis, spp_rankings_summary,
                                    temperature_stats, n_id = 2000) {

  pb_label = grid::textGrob(expression("Increasing relative"~italic("P:B")), x = unit(0.5, 'npc'), just = "centre",
                            gp = gpar(fontfamily = 'serif', fontsize = 8))
  pb_box = grid::roundrectGrob(x = unit(0.5, 'npc'), y = unit(0.5, 'npc'),
                               width = unit(0.4, 'npc'), height = unit(0.8, 'npc'),
                               just = "centre")
  pb_arrow = segmentsGrob(x0 = unit(0.1,'npc'), x1 = unit(0.9, 'npc'),
                          y0 = unit(0.5,'npc'), y1 = unit(0.5, 'npc'),
                          arrow = arrow(type = 'closed'), gp = gpar(fill = 'black', lex = 0.8))
  pb_header = grid::grobTree(pb_arrow, pb_box, pb_label)
  
  lorenz_analysis[["pb_ranking_boots"]] %>%
    ggplot(aes(x = rel_spp, y = rel_flux)) +
    geom_abline(color = "black")+
    geom_abline(intercept = 1, slope = -1, color = "black", linetype = "dotted")+
    geom_line(aes(group = boot_id), color = "grey", alpha = 0.5) +
    geom_line(data = spp_rankings_summary[["PB_spp_rank"]], aes(x = rel_spp, y = rel_flux, color = site), size = 1.5) +
    scale_x_continuous(name = "Cumulative species", limits = c(0,1), expand = c(0,0.03))+
    scale_y_continuous(name = "Cumulative flux", limits = c(0,1), expand = c(0,0.03), breaks = c(0,0.5,1))+
    scale_colour_manual(values = ocecolors[['temperature']][oce_temp_pos], labels = stream_temp_labels)+
    theme_tufte(ticks = TRUE) +
    geom_rangeframe(sides = "lb")+
    theme(legend.position = 'none',
          legend.justification = c("left", "top"), 
          legend.title = element_blank(),
          legend.spacing.y = unit(0.0003,'cm'),
          legend.key.size = unit(0.4,'cm'),
          strip.text = element_blank(), axis.title = element_blank()) +
    facet_wrap(~site, ncol = 1) -> pb_lorenz_flux_plot
  
  my_tag = c("(a)","","","","","")
  pb_lorenz_flux_plot = egg::tag_facet(pb_lorenz_flux_plot, open = "", close = "",
                                      family = 'serif', size = 4, fontface = "plain",
                                      tag_pool = my_tag)#;pb_lorenz_flux_plot
  
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
  
  set.seed(123)
  n_draws = sample(1:6000, n_id, replace = FALSE)
  
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
                                   geom_errorbar( aes(x = tempC, ymin = pb_y_skew_HPDI_90dn, ymax = pb_y_skew_HPDI_90up, color = site), size = 1, width = 0)+
                                   geom_errorbar( aes(x = tempC, ymin = pb_y_skew_HPDI_50dn, ymax = pb_y_skew_HPDI_50up, color = site), size = 1.5, width = 0)+
                                   geom_point(aes(x = tempC, y = pb_y_skew_median), size = 2, color = 'black')+
                                   scale_y_continuous(name = expression(""*italic(Sk[flux])), limits = c(-1,1), expand = c(0,0.004) ) +
                                   scale_x_continuous(expand = c(0,0.004))+
                                   coord_cartesian(xlim = c(0,30), ylim = c(-1,1), clip = 'off')+
                                   annotate('text', label = "(b)", x = -Inf, y = 1, family = 'serif', hjust = 0, vjust = 1, size = 4)+
                                   theme_tufte(ticks = TRUE) +
                                   geom_rangeframe(aes( x = tempC, y = pb_y_skew_median), sides = "lb", inherit.aes = FALSE)+
                                   scale_color_manual(values = rev(ocecolors[['temperature']][oce_temp_pos]), labels = names(stream_temp_labels))+
                                   theme(legend.position = 'none',
                                         axis.title.x = element_blank()));#grid.draw(pb_temp_skew_plot)
  
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
                               scale_y_continuous(name = expression("Pr("~italic(Sk[flux])~">="~italic(x)*")"), expand = c(0,0.004), labels = c(0.5,0.25,0,0.25,0.5)) +
                               coord_cartesian(xlim = c(0,30), ylim = c(0,1), clip = 'off')+
                               annotate('text', label = "(c)", x = 0, y = 1, family = 'serif', hjust = 0, vjust = 1, size = 4)+
                               geom_hline(aes(yintercept = 0.5), color = 'grey', linetype = 'dashed', linewidth = 1)+
                               scale_fill_manual(values = ocecolors[['temperature']][oce_temp_pos], labels = stream_temp_labels)+
                               scale_colour_manual(values = ocecolors[['temperature']][oce_temp_pos], labels = stream_temp_labels)+
                               theme_tufte(ticks = TRUE) +
                               geom_rangeframe(sides = "lb")+
                               theme(legend.title = element_blank(),
                                     legend.position = "none",
                                     legend.justification = c(0,0)));#grid.draw(pb_probs_plot)
  
  # pb_temp_coef_plot = ggplotGrob(temperature_stats[["pb_probs_temp_coefs"]] %>%
  #                                  ggplot()+
  #                                  geom_density(aes(x = b_tempC_stand, y = ..density..), size = 1.0, fill = 'lightgrey', alpha = 0.5)+
  #                                  geom_vline(aes(xintercept = 0), size = 1.2, color = 'red')+
  #                                  scale_x_continuous(name = expression("Change in"~italic("Pr")*"("*italic(Sk[flux])~">="~italic("x")*") with increased temperature"),
  #                                                     expand = c(0.001,0.001), limits = c(-0.04, 0))+
  #                                  scale_y_continuous(expand = c(0.001,0.001), limits = c(0,NA))+
  #                                  theme_tufte(ticks = TRUE));#grid.draw(pb_temp_coef_plot)
  
  # 
  return(grid.arrange(pb_lorenz_flux_plot,pb_header, pb_temp_skew_plot, pb_probs_plot,
                      layout_matrix = rbind(c(1,1,1,3,3),
                                            c(1,1,1,3,3),
                                            c(1,1,1,3,3),
                                            c(1,1,1,3,3),
                                            c(1,1,1,3,3),
                                            c(1,1,1,4,4),
                                            c(1,1,1,4,4),
                                            c(1,1,1,4,4),
                                            c(1,1,1,4,4),
                                            c(2,2,2,4,4)),
                      left = grid::textGrob(label = "Cumulative organic matter flux", gp = gpar(fontfamily = 'serif'), rot = 90)))
  
}
