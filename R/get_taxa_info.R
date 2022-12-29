#' .. content for \description{} (no empty lines) ..
#'
#' .. content for \details{} ..
#'
#' @title
#' @param ann_spp_flux
get_taxa_info <- function(ann_spp_flux =
                          flux_summaries[["annual_spp_flux_summary"]]) {

  
  clean_names = ann_spp_flux$taxon %>% unique %>% gsub("\\.", " ", .) %>% 
    gsub("Hydra$", "Anthoathecatae",.) %>%
    gsub(" spp."," ", .) %>%
    gsub(" sp$", "",.) %>%
    gsub("_","-",.) %>%
    gsub(" \\w{1}$", "",.) %>%
    trimws()
    
  # taxaList = readRDS(file = "./data/derived-data/taxaList.rds")
  
  cleanKeyVal = setNames(clean_names, nm = unique(ann_spp_flux$taxon))
  
  taxaDf = readRDS("./data/taxaDf.rds") %>%
    dplyr::select(db, species_name, phylum, class, order, family, genus, species,taxaGroup)
  
  taxaKeyVal = setNames(taxaDf$taxaGroup, nm = taxaDf$species_name)
  
  taxaDf = ann_spp_flux %>%
    dplyr::mutate(cleanGroup = recode(taxon, !!!cleanKeyVal),
                  taxaGroup = recode(cleanGroup, !!!taxaKeyVal))
  
  return(taxaDf = taxaDf)
  
}
