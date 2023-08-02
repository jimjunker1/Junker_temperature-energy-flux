##' .. content for \description{} (no empty lines) ..
##'
##' .. content for \details{} ..
##'
##' @title
##' @param ann_spp_flux
##' @param stream_temps
plot_spp_flux <- function(ann_spp_flux =
                          flux_summaries[["annual_spp_flux_summary"]],
                          stream_temps= environment_data[["stream_temp_labels"]]) {

  plotting_df <- ann_spp_flux %>% select(site, taxon, contains('mean')) %>% 
    left_join(stream_temps %>% data.frame %>% rownames_to_column('site') %>% setNames(.,nm = c('site', 'tempC'))) %>%
    dplyr::mutate(tempC = as.numeric(tempC)) %>%
    group_by(site, tempC) %>% 
    dplyr::arrange(desc(flux_mg_m_y_mean)) %>% 
    dplyr::mutate(rank = 1:n()) %>%
    ungroup %>%
    dplyr::mutate(site = factor(site, levels = stream_order)) %>%
    dplyr::filter(flux_mg_m_y_mean > 1e-5)
  
  evenness_plot= plotting_df %>%
    na.omit %>%
    dplyr::filter(log(flux_mg_m_y_mean) > -10) %>%
    ggplot(aes(x = rank, y = log(flux_mg_m_y_mean), group = site))+
    geom_line(aes(color = site), linewidth = 2)+
    geom_point(aes(fill = site), shape = 21, size = 2.3, color = 'black') +
    scale_y_continuous(name = expression(log[e]~"Organic matter flux (mg"~m^-2~y^-1*")"),limits = c(-10,NA),
                       expand = c(0.03,0))+
    scale_x_continuous(name = 'Species rank')+
    scale_colour_manual(name = expression("Temperature ("~degree*C~")"), values = ocecolors[['temperature']][oce_temp_pos], labels = stream_temp_labels)+
    scale_fill_manual(name = expression("Temperature ("~degree*C~")"), values = ocecolors[['temperature']][oce_temp_pos], labels = stream_temp_labels)+
    annotate('text', label = "(b)", x = Inf, y = Inf, family = 'serif', vjust = 1, hjust = 1, size = 4)+
    theme_tufte(ticks = TRUE) +
    geom_rangeframe(sides = "lb")+
    theme(legend.position = c(0,0),
          legend.justification = c("left", "bottom"), 
          legend.title = element_text(family = 'serif'),
          legend.spacing.y = unit(0.0003,'cm'),
          legend.key.size = unit(0.4,'cm'));evenness_plot
  
  return(evenness_plot)

}
