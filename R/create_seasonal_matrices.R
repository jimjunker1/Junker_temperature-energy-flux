##' .. content for \description{} (no empty lines) ..
##'
##' .. content for \details{} ..
##'
##' @title
##' @param blank_diet_matrices
##' @param diet_split_list
create_seasonal_matrices <- function(blank_diet_matrices, diet_split_list) {
  ## ++++ Helper functions ++++ ##
    merge_matrices = function(diet,mat,...){
    tax_diet = diet %>%
      group_by(taxon, diet_item) %>%
      dplyr::summarise(rel_area = mean(rel_area))
    
    for(i in 1:nrow(tax_diet)){
      taxon = tax_diet$taxon[i]
      taxa_list = agrep(taxon, colnames(mat))
      diet_item = tax_diet$diet_item[i]
      mat[rownames(mat) %in% diet_item, taxa_list] <- tax_diet$rel_area[i]
    }
    return(mat)
  }
  
  ## ++++ End Helper functions ++++ ##

    ## combine the blank matrices and the seasonal matrices
  filled_matrices = vector('list', length = length(diet_splits))
  filled_matrices = filled_matrices %>% setNames(., nm = levels(stream_order)) %>% rlist::list.subset(names(stream_order_list))
  for(i in 1:length(filled_matrices)){
    x = diet_splits[[i]]
    y = blank_diet_matrices[[i]]
    
    filled_matrices[[i]] <- map(x, ~merge_matrices(.x,y))
  }
  return(filled_matrices = filled_matrices)

}
