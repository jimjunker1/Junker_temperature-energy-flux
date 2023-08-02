##' .. content for \description{} (no empty lines) ..
##'
##' .. content for \details{} ..
##'
##' @title
##' @param spp_rankings
plot_trait_histogram <- function(spp_rankings = spp_rankings_summary,
                                 namesDf = taxonomic_info) {
  
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
                             scale_y_continuous(name = expression(log[10]*"("*italic("B")*", mg "~m^{-2}*")"))+
                             scale_x_discrete(labels = rev(stream_temp_labels))+
                             guides(fill = 'none', color = 'none')+
                             scale_fill_manual(values = rev(ocecolors[['temperature']][oce_temp_pos]), labels = rev(stream_temp_labels)) +
                             scale_color_manual(values = rev(ocecolors[['temperature']][oce_temp_pos]), labels = rev(stream_temp_labels)) +
                             theme(axis.title.x = element_blank(), legend.title = element_blank(), 
                                   legend.position = "none"))
  
# species trait differences
  spp_rankings[["prod_spp_rank"]] %>% 
    dplyr::select(site, taxon, flux_mg_m_y_mean) %>% 
    ungroup %>% 
    left_join(namesDf %>% dplyr::select(site, taxon, cleanGroup, taxaGroup), by = c('site', 'taxon')) %>% 
    group_by(cleanGroup) %>% 
    dplyr::summarise(flux_g_m_y_median = median(flux_mg_m_y_mean/1000, na.rm = TRUE),
                     flux_g_m_y_mad = mad(flux_mg_m_y_mean/1000, na.rm = TRUE)) %>%
    dplyr::mutate(cleanGroup = reorder(cleanGroup, flux_g_m_y_median, decreasing = TRUE)) %>% 
    dplyr::mutate(flux_g_m_y_median_log10 = log10(flux_g_m_y_median),
                  flux_g_m_y_upper = flux_g_m_y_median+flux_g_m_y_mad,
                  flux_g_m_y_lower = flux_g_m_y_median-flux_g_m_y_mad,
                  flux_g_m_y_lower = case_when(flux_g_m_y_lower == 0 ~ 0,
                                               dplyr::between(flux_g_m_y_lower,-3,-0.001) ~ 0.001,
                                               flux_g_m_y_mad != 0 & dplyr::between(flux_g_m_y_median_log10,-4,-3) ~ 0.0001,
                                               flux_g_m_y_mad != 0 & dplyr::between(flux_g_m_y_median_log10,-10,-4) ~ 0.000001,
                                               TRUE ~ flux_g_m_y_lower),
                  flux_g_m_y_lower_log10 = log10(flux_g_m_y_lower),
                  flux_g_m_y_upper_log10 = log10(flux_g_m_y_upper)) %>% 
    na.omit -> spp_rankDf
  
  spp_orderDf = spp_rankDf$cleanGroup
  
    spp_rankDf %>% 
    ggplot()+
    geom_errorbar(aes(x = cleanGroup, ymin = flux_g_m_y_lower_log10, ymax = flux_g_m_y_upper_log10), width = 0, linewidth = 0.6)+
    geom_point(aes(x = cleanGroup, y = flux_g_m_y_median_log10), size = 2)+
    scale_y_continuous(name = expression(atop("OM flux","(g"*m^-2~y^-1*")")))+
    theme(axis.text.x = element_blank(),
          axis.title.x = element_blank()) -> spp_flux_plot#;spp_flux_plot
    
  spp_rankings[["bio_spp_rank"]] %>% 
    dplyr::select(site, taxon, flux_mg_m_y_mean, bio_mg_m_mean) %>% 
    left_join(namesDf %>% dplyr::select(site, taxon, cleanGroup, taxaGroup), by = c('site', 'taxon')) %>%
    ungroup %>% 
    group_by(cleanGroup) %>% 
    dplyr::summarise(flux_g_m_y_median = median(flux_mg_m_y_mean/1000, na.rm = TRUE),
                     flux_g_m_y_mad = mad(flux_mg_m_y_mean/1000, na.rm = TRUE),
                     bio_mg_m_median = median(bio_mg_m_mean, na.rm = TRUE),
                     bio_mg_m_mad = mad(bio_mg_m_mean, na.rm = TRUE)) %>%
    dplyr::mutate(cleanGroup = reorder(cleanGroup, flux_g_m_y_median, decreasing = TRUE)) %>% 
    dplyr::mutate(bio_mg_m_median_log10 = log10(bio_mg_m_median),
                  bio_mg_m_upper = bio_mg_m_median+bio_mg_m_mad,
                  bio_mg_m_lower = bio_mg_m_median-bio_mg_m_mad,
                  bio_mg_m_lower = case_when(bio_mg_m_lower == 0 ~ 0,
                                             bio_mg_m_lower < 0 ~ 0.01,
                                               TRUE ~ bio_mg_m_lower),
                  bio_mg_m_lower_log10 = log10(bio_mg_m_lower),
                  bio_mg_m_upper_log10 = log10(bio_mg_m_upper)) %>% 
    na.omit %>% 
    ggplot()+
    geom_errorbar(aes(x = cleanGroup, ymin = bio_mg_m_lower_log10, ymax = bio_mg_m_upper_log10), width = 0, linewidth = 0.6)+
    geom_point(aes(x = cleanGroup, y = bio_mg_m_median_log10), size = 2)+
    scale_y_continuous(name = expression(atop("Biomass","(mg"*m^-2*")")))+
    theme(axis.text.x = element_blank(),
          axis.title.x = element_blank())+
    NULL -> spp_bio_plot
  
  
  spp_rankings[["n_spp_rank"]] %>% 
    dplyr::select(site, taxon, flux_mg_m_y_mean, n_ind_m_mean) %>% 
    left_join(namesDf %>% dplyr::select(site, taxon, cleanGroup, taxaGroup), by = c('site', 'taxon')) %>% 
    ungroup %>% 
    group_by(cleanGroup) %>% 
    dplyr::summarise(flux_g_m_y_median = median(flux_mg_m_y_mean/1000, na.rm = TRUE),
                     flux_g_m_y_mad = mad(flux_mg_m_y_mean/1000, na.rm = TRUE),
                     n_ind_m_median = median(n_ind_m_mean, na.rm = TRUE),
                     n_ind_m_mad = mad(n_ind_m_mean, na.rm = TRUE)) %>%
    dplyr::mutate(cleanGroup = reorder(cleanGroup, flux_g_m_y_median, decreasing = TRUE)) %>% 
    dplyr::mutate(n_ind_m_median_log10 = log10(n_ind_m_median),
                  n_ind_m_upper = n_ind_m_median+n_ind_m_mad,
                  n_ind_m_lower = n_ind_m_median-n_ind_m_mad,
                  n_ind_m_lower = case_when(n_ind_m_lower == 0 ~ 0,
                                            n_ind_m_lower < 0 ~ 0.1,
                                             TRUE ~ n_ind_m_lower),
                  n_ind_m_lower_log10 = log10(n_ind_m_lower),
                  n_ind_m_upper_log10 = log10(n_ind_m_upper)) %>% 
    na.omit %>% 
    ggplot()+
    geom_errorbar(aes(x = cleanGroup, ymin = n_ind_m_lower_log10 , ymax = n_ind_m_upper_log10), width = 0, linewidth = 0.6)+
    geom_point(aes(x = cleanGroup, y = n_ind_m_median_log10), size = 2) + 
    scale_y_continuous(name = expression(atop("Abundance","(ind."*m^-2*")")))+
    theme(axis.text.x = element_blank(),
          axis.title.x = element_blank())+
    NULL -> spp_n_plot
  
  spp_rankings[["M_spp_rank"]] %>% 
    dplyr::select(site, taxon, flux_mg_m_y_mean, M_mg_ind_mean) %>% 
    left_join(namesDf %>% dplyr::select(site, taxon, cleanGroup, taxaGroup), by = c('site', 'taxon')) %>% 
    ungroup %>% 
    group_by(cleanGroup) %>% 
    dplyr::summarise(flux_g_m_y_median = median(flux_mg_m_y_mean/1000, na.rm = TRUE),
                     flux_g_m_y_mad = mad(flux_mg_m_y_mean/1000, na.rm = TRUE),
                     M_mg_ind_median = median(M_mg_ind_mean, na.rm = TRUE),
                     M_mg_ind_mad = mad(M_mg_ind_mean, na.rm = TRUE)) %>%
    dplyr::mutate(cleanGroup = reorder(cleanGroup, flux_g_m_y_median, decreasing = TRUE)) %>% 
    dplyr::mutate(M_mg_ind_median_log10 = log10(M_mg_ind_median),
                  M_mg_ind_upper = M_mg_ind_median+M_mg_ind_mad,
                  M_mg_ind_lower = M_mg_ind_median-M_mg_ind_mad,
                  M_mg_ind_lower = case_when(M_mg_ind_lower == 0 ~ 0,
                                             M_mg_ind_lower < 0 ~ 0.01,
                                            TRUE ~ M_mg_ind_lower),
                  M_mg_ind_lower_log10 = log10(M_mg_ind_lower),
                  M_mg_ind_upper_log10 = log10(M_mg_ind_upper)) %>%
    na.omit %>% 
    ggplot()+
    geom_errorbar(aes(x = cleanGroup, ymin = M_mg_ind_lower_log10, ymax = M_mg_ind_upper_log10), width = 0, linewidth = 0.6)+
    geom_point(aes(x = cleanGroup, y = M_mg_ind_median_log10), size = 2)+
    scale_y_continuous(name = expression(atop("Body size","("*mg~ind^-1*")")))+
    theme(axis.text.x = element_text(angle = 45, hjust = 1, vjust = 1, size = 8.5),
          axis.title.x = element_blank())+
    NULL -> spp_M_plot
  
return(list(trait_hist = grid.arrange(M_boxplot, pb_boxplot, bio_boxplot, ncol = 1),
              spp_trait_plot = grid.arrange(spp_flux_plot, spp_bio_plot, spp_n_plot, spp_M_plot,
                           layout_matrix = matrix(c(1,1,1,1,
                                                    2,2,2,2,
                                                    3,3,3,3,
                                                    4,4,4,4,
                                                    4,4,4,4), byrow = TRUE), vp=viewport(width=0.95, height=1))))

}
