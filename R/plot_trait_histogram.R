##' .. content for \description{} (no empty lines) ..
##'
##' .. content for \details{} ..
##'
##' @title
##' @param spp_rankings
plot_trait_histogram <- function(spp_rankings = spp_rankings_summary) {
  
  prod_df = spp_rankings[['prod_spp_rank']] %>% dplyr::filter(prod_mg_m_y_mean > 0 | is.na(prod_mg_m_y_mean)) %>% dplyr::mutate(site = factor(site, levels = rev(stream_order)))
  pb_df = spp_rankings[['PB_spp_rank']] %>%  dplyr::filter(pb_y_mean >0.001 | is.na(pb_y_mean)) %>% dplyr::mutate(site = factor(site, levels = rev(stream_order)))
  bio_df = spp_rankings[['bio_spp_rank']] %>% dplyr::filter(bio_mg_m_mean >0 | is.na(bio_mg_m_mean)) %>% dplyr::mutate(site = factor(site, levels = rev(stream_order)))
  M_df = spp_rankings[['M_spp_rank']] %>% dplyr::filter(M_mg_ind_mean >0 | is.na(M_mg_ind_mean)) %>% dplyr::mutate(site = factor(site, levels = rev(stream_order)))
  
  pb_boxplot = ggplotGrob(ggplot(pb_df, aes(x = site, y = log10(pb_y_mean), fill= site))+
                            geom_flat_violin(position = position_nudge(x = 0.2, y = 0), alpha = 0.8)+
                            geom_point(aes(y = log10(pb_y_mean), color = site), 
                                       position = position_jitter(width = 0.1),
                                       size = 0.8, alpha = 0.6)+
                            geom_boxplot(width = 0.1, outlier.shape = NA, alpha = 0.5, show.legend =FALSE)+
                            scale_y_continuous(name = expression(log[10]*"("*italic("P:B")*","~y^{-1}*")"))+
                            scale_x_discrete(labels = rev(stream_temp_labels))+
                            guides(fill = 'none', color = guide_legend(override.aes = list(size = 2)))+
                            # guides(color = 'none')+
                            scale_fill_manual(values = rev(ocecolors[['temperature']][oce_temp_pos]), labels = rev(stream_temp_labels))+
                            scale_color_manual(values = rev(ocecolors[['temperature']][oce_temp_pos]), labels = rev(stream_temp_labels))+
                            theme(axis.title.x = element_blank(), axis.ticks.x = element_blank(),
                                  legend.title = element_blank(), legend.position = 'none', 
                                  legend.justification = c(0,0),legend.background = element_rect(fill = 'transparent', colour = NA)))#;grid.draw(pb_boxplot)
  M_boxplot = ggplotGrob(ggplot(M_df,aes(x = site, y = log10(M_mg_ind_mean), fill = site))+
                           geom_flat_violin(position = position_nudge(x =0.2, y = 0), alpha = 0.8) +
                           geom_point(aes(y = log10(M_mg_ind_mean), color = site), 
                                          position = position_jitter(width = 0.1),
                                          size = 0.8, alpha = 0.6) +
                           geom_boxplot(width = 0.1, outlier.shape = NA, alpha = 0.5)+
                           scale_y_continuous(name = expression(log[10]*"("*italic("M")*","~mg^{-ind}*")"))+
                           guides(fill = 'none', color = 'none')+
                           scale_fill_manual(values = rev(ocecolors[['temperature']][oce_temp_pos]), labels = rev(stream_temp_labels)) +
                           scale_color_manual(values = rev(ocecolors[['temperature']][oce_temp_pos]), labels = rev(stream_temp_labels)) +
                           theme(axis.title.x = element_blank(), axis.ticks.x = element_blank(),
                                 legend.title = element_blank(), 
                                 legend.position = "none"))#;grid.draw(M_boxplot)
  
  bio_boxplot = ggplotGrob(ggplot(bio_df,aes(x = site, y = log10(bio_mg_m_mean), fill = site))+
                             geom_flat_violin(position = position_nudge(x =0.2, y = 0), alpha = 0.8) +
                             geom_point(aes(y = log10(bio_mg_m_mean), color = site), 
                                        position = position_jitter(width = 0.1),
                                        size = 0.8, alpha = 0.6) +
                             geom_boxplot(width = 0.1, outlier.shape = NA, alpha = 0.5)+
                             scale_y_continuous(name = expression(log[10]*"("*italic("B")*", mg"~m^{-2}*")"))+
                             scale_x_discrete(labels = rev(stream_temp_labels))+
                             guides(fill = 'none', color = 'none')+
                             scale_fill_manual(values = rev(ocecolors[['temperature']][oce_temp_pos]), labels = rev(stream_temp_labels)) +
                             scale_color_manual(values = rev(ocecolors[['temperature']][oce_temp_pos]), labels = rev(stream_temp_labels)) +
                             theme(axis.title.x = element_blank(), legend.title = element_blank(), 
                                   legend.position = "none"))#;grid.draw(bio_boxplot)
  # pb_boxplot = ggplotGrob(ggplot(pb_df,aes(x = site, y = log10(pb_y_mean))) + geom_violin(aes(fill = site), alpha = 0.5) +
  #   geom_jitter(aes( fill = site),color = 'black', size = 2, shape = 21, alpha = 0.5) +
  #   scale_fill_manual(values = rev(ocecolors[['temperature']][oce_temp_pos]), labels = rev(stream_temp_labels)) +
  #   theme(axis.title.x = element_blank(), legend.title = element_blank(), legend.position = c(0,0), legend.justification = c(0,0),
  #         legend.background = element_rect(fill = 'transparent', colour = NA)))
  # pb_boxplot
  
  # M_boxplot = ggplotGrob(ggplot(M_df,aes(x = site, y = log10(M_mg_ind_mean))) + geom_violin(aes(fill = site), alpha = 0.5) +
  #   geom_jitter(aes( fill = site),color = 'black', size = 2, shape = 21, alpha = 0.5) +
  #   scale_fill_manual(values = rev(ocecolors[['temperature']][oce_temp_pos]), labels = rev(stream_temp_labels)) +
  #   theme(axis.title.x = element_blank(), legend.title = element_blank(), legend.position = "none"))
  
  # bio_boxplot = ggplotGrob(ggplot(bio_df,aes(x = site, y = log10(bio_mg_m_mean))) + geom_violin(aes(fill = site), alpha = 0.5) +
  #   geom_jitter(aes( fill = site),color = 'black', size = 2, shape = 21, alpha = 0.5) +
  #   scale_fill_manual(values = rev(ocecolors[['temperature']][oce_temp_pos]), labels = rev(stream_temp_labels)) +
  #   theme(axis.title.x = element_blank(), legend.title = element_blank(),legend.position = "none"))
  
  return(trait_hist = grid.arrange(M_boxplot, pb_boxplot, bio_boxplot, ncol = 1))

}
