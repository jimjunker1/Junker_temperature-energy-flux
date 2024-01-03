#' .. content for \description{} (no empty lines) ..
#'
#' .. content for \details{} ..
#'
#' @title
#' @param spp_rankings_summary
#' @param temperature_stats
plot_traits_temp <- function(flux_summaries,
                             production_boots,
                             spp_rankings_summary,
                             temperature_stats) {

  stream_temps = stream_temp_labels %>% data.frame %>%
    rownames_to_column('site_id') %>% setNames(., c('site_id', 'tempC')) %>%
    dplyr::mutate(tempC = as.numeric(tempC))
  
  new_data = data.frame(tempC = seq((min(stream_temps$tempC)-0.5),(max(stream_temps$tempC)+0.5), length.out = 100)) %>%
    dplyr::mutate(tempC_stand = tempC - mean(tempC, na.rm = TRUE))
  
  ann_comm_sum = flux_summaries[['annual_comm_flux_summary']] %>%
    dplyr::rename(site_id = 'site') %>%
    dplyr::mutate(across(where(is.numeric), ~.x/1000)) %>%
    left_join(stream_temps, by = 'site_id') %>%
    dplyr::mutate(site_id = factor(site_id, levels = stream_order))
  
# add in posterior draws
  flux_temp_boots = temperature_stats[["flux_temp_boots"]]
  
  flux_temp_pred = flux_temp_boots %>%
    junkR::named_group_split(n_rep) %>%
    purrr::map(~.x %>% flatten %>% pluck('model')) %>% 
    purrr::map(~data.frame(tempC = new_data$tempC,
                           flux_mg_m_y = exp(predict(.x, newdata = new_data)))) %>%
    bind_rows(.id = 'n_rep') %>%
    dplyr::filter(n_rep %in% sample(unique(n_rep), 400, replace = FALSE))
  
    
  ann_comm_sum %>%
    ggplot() +
    geom_line(data = flux_temp_pred, aes(x = tempC, y = log(flux_mg_m_y), group = n_rep),
              color = 'lightgray', alpha = 0.5, inherit.aes = FALSE)+
    geom_errorbar(aes(x = tempC, ymin = log(flux_mg_m_y_quant2.5), ymax = log(flux_mg_m_y_quant97.5), color = site_id), width = 0, linewidth = 1) +
    geom_errorbar(aes(x = tempC, ymin = log(flux_mg_m_y_quant25), ymax = log(flux_mg_m_y_quant75), color = site_id), width = 0, linewidth = 2) +
    geom_point(aes(x = tempC, y = log(flux_mg_m_y_mean), fill = site_id), shape =21, size = 2)+
    scale_x_continuous(name = expression("Temperature ("~degree*C~")"),limits = c(0,30), expand = c(0.01,0.01))+
    scale_y_continuous(name = expression("Organic matter flux ( g AFDM "~m^-2~y^-1*")"), expand = c(0.01,0.01),
                       breaks = log(c(1,5,10,50,100, 500)), labels = exp(log(c(1,5,10,50,100,500)))) +
    scale_color_manual(values = ocecolors[["temperature"]][oce_temp_pos], labels = stream_temp_labels) +
    scale_fill_manual(values = ocecolors[["temperature"]][oce_temp_pos], labels = stream_temp_labels) +
    annotate('text', label = '(a)', x = 0.5, y = Inf, family = 'serif', vjust = 1,hjust = 0, size = 4)+
    theme_tufte(ticks = TRUE) +
    geom_rangeframe(aes(x = tempC,y = log(flux_mg_m_y_mean)),sides = "lb")+
    theme(legend.position = "none",
          legend.title = element_blank(),
          legend.justification = c(1,0), axis.title.x = element_blank()) -> ann_comm_flux_plot
    
  
  ann_spp_summ = temperature_stats[["sppBootsDf"]] %>%
    group_by(site_id) %>%
    dplyr::summarise(across(c(pb_y,M_mg_ind), list(mean = ~mean(.x, na.rm = TRUE),
                                                   median = ~median(.x, na.rm = TRUE),
                                                   mode = ~rethinking::chainmode(.x, na.rm = TRUE),
                                                   quant2.5 = ~quantile(.x, 0.025, na.rm = TRUE),
                                                   quant25 = ~quantile(.x, 0.25, na.rm = TRUE),
                                                   quant75 = ~quantile(.x, 0.75, na.rm = TRUE),
                                                   quant97.5 = ~quantile(.x, 0.975, na.rm = TRUE)))) %>%
    left_join(stream_temps) %>%
    dplyr::mutate(site_id = factor(site_id, levels = stream_order))
  
  pb_spp_temp_boots = temperature_stats[["pb_spp_temp_boots"]]
  
  pb_spp_temp_pred = pb_spp_temp_boots %>%
    junkR::named_group_split(n_rep) %>%
    purrr::map(~.x %>% flatten %>% pluck('model')) %>% 
    purrr::map(~data.frame(tempC = new_data$tempC,
                           pb_y = predict(.x, newdata = new_data))) %>%
    bind_rows(.id = 'n_rep') %>%
    dplyr::filter(n_rep %in% sample(unique(n_rep), 400, replace = FALSE))
  
    ggplotGrob(ggplot(ann_spp_summ)+
    geom_line(data = pb_spp_temp_pred, aes(x = tempC, y = pb_y, group = n_rep),
              color = 'lightgray', alpha = 0.5, inherit.aes = FALSE)+
    geom_errorbar(aes(x = tempC, ymin = log(pb_y_quant25), ymax = log(pb_y_quant75), color = site_id),
                  width = 0, linewidth = 2)+
    geom_errorbar(aes(x = tempC, ymin = log(pb_y_quant2.5), ymax = log(pb_y_quant97.5), color = site_id),
                  width = 0, linewidth = 1)+
    geom_point(aes(x = tempC, y = log(pb_y_mean), fill = site_id),shape = 21, color = 'black', size = 2)+
    scale_x_continuous(name = expression("Temperature ("~degree*C~")"),limits = c(0,30), expand = c(0.01,0.01))+
    scale_y_continuous(name = expression("Popn. "~bar(italic(P:B))~"("*y^-1*")"), expand = c(0.01,0.01),
                       breaks = log(c(1,5,10,50,100)), labels = exp(log(c(1,5,10,50,100)))) +
    scale_color_manual(values = ocecolors[["temperature"]][oce_temp_pos], labels = stream_temp_labels) +
    scale_fill_manual(values = ocecolors[["temperature"]][oce_temp_pos], labels = stream_temp_labels) +
    annotate('text', label = '(c)', x = Inf, y = Inf, family = 'serif', vjust = 1,hjust = 1, size = 4)+
    theme_tufte(ticks = TRUE) +
    geom_rangeframe(aes(x = tempC,y = log(pb_y_mean)),sides = "lb")+
    theme(legend.position = 'none',axis.title.x = element_blank())) -> comm_pb_plot;dev.off()
    
  m_spp_temp_boots = temperature_stats[["m_spp_temp_boots"]]
  
  m_spp_temp_pred = m_spp_temp_boots %>%
    junkR::named_group_split(n_rep) %>%
    purrr::map(~.x %>% flatten %>% pluck('model')) %>% 
    purrr::map(~data.frame(tempC = new_data$tempC,
                           M_mg_ind = predict(.x, newdata = new_data))) %>%
    bind_rows(.id = 'n_rep')%>%
    dplyr::filter(n_rep %in% sample(unique(n_rep), 400, replace = FALSE))
  
    ggplotGrob(ggplot(ann_spp_summ)+
    geom_line(data = m_spp_temp_pred, aes(x = tempC, y = M_mg_ind, group = n_rep),
              color = 'lightgray', alpha = 0.5, inherit.aes = FALSE)+
    geom_errorbar(aes(x = tempC, ymin = log(M_mg_ind_quant25), ymax = log(M_mg_ind_quant75), color = site_id),
                  width = 0, linewidth = 2)+
    geom_errorbar(aes(x = tempC, ymin = log(M_mg_ind_quant2.5), ymax = log(M_mg_ind_quant97.5), color = site_id),
                  width = 0, linewidth = 1)+
    geom_point(aes(x = tempC, y = log(M_mg_ind_mean), fill = site_id),shape = 21, color = 'black', size = 2)+
    scale_x_continuous(name = expression("Temperature ("~degree*C~")"), limits = c(0,30), expand = c(0.01,0.01))+
    scale_y_continuous(name = expression("Popn. "~bar(italic(M))~"(mg AFDM "~ind^-1*")"), expand = c(0.01,0.01),
                       breaks = log(c(5,1,0.5,0.1,0.05, 0.01)), labels = exp(log(c(5,1,0.5,0.1,0.05, 0.01)))) +
    scale_color_manual(values = ocecolors[["temperature"]][oce_temp_pos], labels = stream_temp_labels) +
    scale_fill_manual(values = ocecolors[["temperature"]][oce_temp_pos], labels = stream_temp_labels) +
    annotate('text', label = '(b)', x = Inf, y = Inf, family = 'serif', vjust = 1,hjust = 1, size = 4)+
    theme_tufte(ticks = TRUE) +
    geom_rangeframe(aes(x = tempC, y = log(M_mg_ind_mean)), sides = "lb")+
    theme(legend.position = 'none', axis.title.x = element_blank())) -> comm_m_plot;dev.off()
    
  
 heights = max(comm_m_plot$heights, comm_pb_plot$heights)
 comm_m_plot$heights -> comm_pb_plot$heights -> heights
 widths = max(comm_m_plot$widths, comm_pb_plot$widths)
 comm_m_plot$widths -> comm_pb_plot$widths -> widths
 
 m_pbPlot = gridExtra::grid.arrange(comm_m_plot, comm_pb_plot, ncol = 1)
 
 fig1_plot = gridExtra::grid.arrange(ann_comm_flux_plot, m_pbPlot, 
                                     layout_matrix = rbind(c(1,1,2)),
                                     bottom = grid::textGrob(label = expression("Temperature ("~degree*C~")"), gp = gpar(fontfamily= 'serif')))
 
 return(list(fig1_plot = fig1_plot, ann_spp_summ = ann_spp_summ))

}
