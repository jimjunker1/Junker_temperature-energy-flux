##' .. content for \description{} (no empty lines) ..
##'
##' .. content for \details{} ..
##'
##' @title
##' @param ann_spp_flux
##' @param n
create_random_ranks <- function(ann_spp_flux = flux_summaries[["annual_spp_flux_boots"]], n = 1e6,...) {

  #'
  #'
  ## ++++ Helper functions ++++ ##
  random_ordering = function(x, n){
    x = vector('list', length = n)
    taxa_list = unique(x$taxon)
    random_spp = lapply(x, sample(taxa_list, replace = FALSE))
    return(random_spp)
  }
  
  skew_est = function(df, x,y){
    site_name = unique(df$site)
    skew_name = paste0(as.character(y),'_skew')
    df = df %>% dplyr::filter(flux_mg_m_y > 0)
    y = df %>% dplyr::select(all_of(y)) %>% na.omit %>% unlist
    x = df %>% dplyr::select(all_of(x)) %>% na.omit %>% dplyr::filter(. > 0) %>% unlist
    
    y1 = quantile(y, 0.25, na.rm = TRUE)
    y2 = quantile(y, 0.5, na.rm = TRUE)
    y3 = quantile(y, 0.75, na.rm = TRUE)
    
    y1_min = which.min(abs(y - y1))
    y2_min = which.min(abs(y - y2))
    y3_min = which.min(abs(y - y3))
    
    x1 = unname(x[y1_min])
    x2 = unname(x[y2_min])
    x3 = unname(x[y3_min])
    
    skew = (x3 - 2*x2 + x1)/(x3-x1)
    assign(skew_name, skew)
    z = data.frame(site = site_name, eval(as.symbol(skew_name)))
    colnames(z)[2] <- skew_name 
    return(z)
  }
  ## ++++ End Helper functions ++++ ##
  
  set.seed(42);boot_n = sample(unique(ann_spp_flux$boot_id), n, replace = TRUE)
 
  # debugonce(skew_est)
  spp_rank_list = boot_n %>% 
    map(., ~ann_spp_flux %>%
          dplyr::filter(boot_id == .x) %>%
          group_by(site) %>%
          slice_sample(prop = 1.0, replace = FALSE) %>%
          dplyr::filter(flux_mg_m_y > 0) %>% 
          dplyr::mutate(rand_rank = 1:n(), 
                        cumul_flux = cumsum(flux_mg_m_y),
                        rel_flux = cumul_flux/sum(flux_mg_m_y)) %>%
          named_group_split(site) %>%
          map(., ~.x %>% skew_est(., 'rel_flux', 'rand_rank')) %>% 
          bind_rows) %>% bind_rows
    

}
