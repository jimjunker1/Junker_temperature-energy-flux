##' .. content for \description{} (no empty lines) ..
##'
##' .. content for \details{} ..
##'
##' @title
##' @param diet_predictions
plot_stream_diets <- function(diet_predictions = modeled_diets[["diet_predictions"]]) {

  diet_predictions %>%
    dplyr::mutate(site = factor(site, levels = names(stream_order_list))) %>%
    group_by(site, diet_item) %>%
    sample_n(size = 5000, replace = FALSE) %>%
    ggplot(aes(x = rel_area)) + geom_density(aes(group = diet_item, color = diet_item, fill = diet_item), alpha = 0.5) +
    scale_color_viridis_d(option = 'plasma')+
    scale_fill_viridis_d(option = 'plasma')+
    theme_tufte(ticks = TRUE) +
    geom_rangeframe(sides = "lb")+
    facet_wrap(~site, ncol = 2) -> stream_diets_plot
  
  return(stream_diets_plot)

}
