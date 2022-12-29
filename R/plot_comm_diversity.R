#' .. content for \description{} (no empty lines) ..
#'
#' .. content for \details{} ..
#'
#' @title
#' @param diversity_analysis
plot_comm_diversity <- function(diversity_analysis) {

  
  int_spp_wide = diversity_analysis[['int_spp_wide']]
  NMDS = diversity_analysis[['NMDS']]
  NMDS.scrs = diversity_analysis[['NMDS.scrs']]
  
  stream_temps = data.frame(temp_C = stream_temp_labels,
                            site_id = names(stream_temp_labels))
  
  
  

}
