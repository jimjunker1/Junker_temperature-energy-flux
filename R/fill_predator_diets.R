##' .. content for \description{} (no empty lines) ..
##'
##' .. content for \details{} ..
##'
##' @title
##' @param seasonal_diet_matrices
fill_predator_diets <- function(seasonal_diet_matrices) {
  ## ++++ Helper functions ++++ ##
  # fill in the matrics from lists
  # debugonce(fill_pred_diet)
  fill_pred_diet <- function(mat, predator_names,...){
    if(length(predator_names) > 1){
      predator_matches = purrr::map(predator_names, ~agrep(.x, colnames(mat))) %>% unlist
      mat[rownames(mat) == 'animal', predator_matches] <- 1.0 
      return(mat)
    } else{
      predator_matches = agrep(predator_names, colnames(mat))
      mat[rownames(mat) == "animal", predator_matches] <- 1.0
      return(mat)
    }
  }
  ## ++++ End Helper functions ++++ ##
  
  # name predators 
  predator_lists = list(hver = c("Limnophora.riparia","Sperchon.glandulosus"),
                        st6 = c("Limnophora.riparia","Sperchon.glandulosus","Macropelopia", "Antocha.sp.","Ephydridae.sp."),
                        st9 = c("Limnophora.riparia","Sperchon.glandulosus","Macropelopia", "Antocha.sp.","Ephydridae.sp.", "Clinocera.stagnalis"),
                        st7 = c("Limnophora.riparia","Sperchon.glandulosus", "Dicranota","Clinocera"),
                        oh2 = c("Limnophora.riparia","Sperchon.glandulosus", "Macropelopia","Dicranota","Clinocera"),
                        st14 = c("Limnophora.riparia","Sperchon.glandulosus", "Antocha.sp.")) %>% 
    rlist::list.subset(names(stream_order_list))
  
  
  ## combine the blank matrices and the seasonal matrices
  filled_matrices = vector('list', length = length(seasonal_diet_matrices))
  filled_matrices = filled_matrices %>% setNames(., nm = levels(stream_order)) %>% rlist::list.subset(names(stream_order_list))
  for(i in 1:length(filled_matrices)){
    x = seasonal_diet_matrices[[i]]
    y = predator_lists[[i]]
    
    filled_matrices[[i]] <- map(x, ~fill_pred_diet(.x,y))
  }
  return(final_matrices = filled_matrices)  
  
  # saved code if want to fill out predator-prey connections for predator
  # predator_diet_lists = list(hver = list(c("Simulium.vittatum","Cricotopus.sylvestris","Nais.spp.", "Naididae 1"),c("Nais.spp.","Naididae 1")),
  #                            st6 = list(c("Nais.spp.","Simulium.vittatum","Simulium.spp.", "Micropsectra.sp."),
  #                                       c("Nais.spp.","Naididae.1"),
  #                                       c("Chaetocladius.dentiforceps","Eukiefferiella","Micropsectra.sp.","Orthocladius","Orthocladius.oblidens","Simulium.vittatum","Thienemanniella.sp."),
  #                                       c("Nais.spp.","Simulium.vittatum","Simulium.spp.", "Micropsectra.sp."),
  #                                       c("Nais.spp.","Simulium.vittatum","Simulium.spp.", "Micropsectra.sp.")),
  #                            st9 = list(c("Nais.spp.","Simulium.vittatum","Simulium.spp."),
  #                                       c("Nais.spp.","Tubificid.1"),
  #                                       c("Eukiefferiella.sp.","Micropsectra.sp.","Orthocladius.fridgidus","Simulium.vittatum","Thienemanniella.sp."),
  #                                       c("Nais.spp.","Simulium.vittatum","Simulium.spp."),
  #                                       c("Nais.spp.","Simulium.vittatum","Simulium.spp."),
  #                                       c("Nais.spp.","Simulium.vittatum","Simulium.spp.")),
  #                            st7 = list(c()),
  #                            oh2 = list(c()),
  #                            st14 = list(c())) %>% 
  #   rlist::list.subset(names(stream_order_list))
  # predator_diet_props = list(hver = list(c(0.25,0.25,0.25,0.25), c(0.5, 0.5)),
  #                            st6 = list(c(0.1,0.3,0.3,0.3),
  #                                       c(0.5,0.5),
  #                                       c(rep(1/7,7)),
  #                                       c(0.1,0.3,0.3,0.3),
  #                                       c(0.1,0.3,0.3,0.3)),
  #                            st9 = list(c(0.1,0.7,0.2),
  #                                       c(0.5,0.5),
  #                                       c(rep(1/5),5),
  #                                       c(0.1,0.7,0.2),
  #                                       c(0.1,0.7,0.2),
  #                                       c(0.1,0.7,0.2)),
  #                            st7 = list(),
  #                            oh2 = list(),
  #                            st14 = list())
  
  # fill 
  


}
