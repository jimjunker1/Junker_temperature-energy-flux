##' .. content for \description{} (no empty lines) ..
##'
##' .. content for \details{} ..
##'
##' @title
##' @param lorenz_analysis
##' @param random_rankings
analyze_skew <- function(lorenz_analysis, random_rankings) {

  ## ++++ Helper functions ++++ ##
  skew_est = function(df, x,y){
    site_name = unique(df$site)
    # boot_id = unique(df$boot_id)
    skew_name = paste0(as.character(y),'_skew')
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
  
  skew_probs = function(emp_skew, rand_skew, skew_var = NULL,...){
    if(is.null(skew_var)) stop("Error: Must define the skew variable.")
    rand_tot = nrow(rand_skew)
    rand_skew = rand_skew[,grepl("skew",colnames(rand_skew))]
    skew = unlist(emp_skew[,grepl(skew_var, colnames(emp_skew))])
    extreme_vec = sapply(skew, FUN = function(x){
      y = sum(rand_skew >= x, na.rm = TRUE)/rand_tot
      # if(x > 0){
      #   y = sum(rand_skew >= x, na.rm = TRUE)/rand_tot
      # } else if( x < 0){
      #   y = sum(rand_skew <= x, na.rm = TRUE)/rand_tot
      # } else{
      #   y = sum(rand_skew ==0, na.rm =TRUE)/rand_tot
      # }
      y
    })
    extreme_vec
  }
  
  skew_perc = function(emp_skew, rand_skew, skew_var = NULL,...){
    if(is.null(skew_var)) stop("Error: Must define the skew variable.")
    percentile = ecdf(unlist(rand_skew[,grepl("skew",colnames(rand_skew))]))
    skew = unlist(emp_skew[,grepl(skew_var, colnames(emp_skew))])
    skew_percs= sapply(skew, percentile)
    return(skew_percs)
  }
  ## ++++ End Helper functions ++++ ##
   # debugonce(skew_est)
 
  PB_skew_boots = lorenz_analysis[["pb_ranking_boots"]] %>%
    named_group_split(site) %>%
    future_map(~.x %>% named_group_split(boot_id) %>%
          map(~.x %>% skew_est(., 'rel_flux', 'pb_y')) %>% 
          bind_rows(.id = 'boot_id')) %>%
    bind_rows(.id = 'site')
  
  M_skew_boots = lorenz_analysis[["M_ranking_boots"]] %>%
    named_group_split(site) %>%
    future_map(~.x %>% named_group_split(boot_id) %>%
          map(~.x %>% skew_est(., 'rel_flux', 'M_mg_ind')) %>%
          bind_rows(.id = 'boot_id')) %>%
    bind_rows(.id = 'site')
  
  stream_PB_boots = PB_skew_boots %>%
    named_group_split(site) %>% rlist::list.subset(names(stream_order_list))
  stream_M_boots = M_skew_boots %>%
    named_group_split(site) %>% rlist::list.subset(names(stream_order_list))
  stream_rand_boots = random_rankings %>%
    named_group_split(site) %>% rlist::list.subset(names(stream_order_list))
  
  # debugonce(skew_probs)
  PB_skew_probs = future_map2(stream_PB_boots, stream_rand_boots, ~skew_probs(emp_skew = .x, rand_skew = .y, skew_var = "pb_y_skew"))
  M_skew_probs = future_map2(stream_M_boots, stream_rand_boots, ~skew_probs(emp_skew = .x, rand_skew = .y, skew_var = "M_mg_ind_skew"))
  
  # debugonce(skew_perc)
  PB_skew_percs = future_map2(stream_PB_boots, stream_rand_boots, ~skew_perc(emp_skew = .x, rand_skew = .y, skew_var = "pb_y_skew"))
  M_skew_percs = future_map2(stream_M_boots, stream_rand_boots, ~skew_perc(emp_skew = .x, rand_skew = .y, skew_var = "M_mg_ind_skew"))
  
  PB_skew_summ = PB_skew_boots %>%
    group_by(site) %>%
    dplyr::summarise(across(matches('skew'), list(mean = ~mean(.x, na.rm = TRUE),
                                                  quant2.5 = ~quantile(.x, 0.025, na.rm = TRUE),
                                                  quant25 = ~quantile(.x, 0.25, na.rm = TRUE),
                                                  quant50 = ~quantile(.x, 0.50, na.rm = TRUE),
                                                  quant75 = ~quantile(.x, 0.75, na.rm = TRUE),
                                                  quant97.5 = ~quantile(.x, 0.975, na.rm = TRUE)))) %>%
   left_join(PB_skew_probs %>% bind_rows(.id = 'site') %>%
                pivot_longer(-site, names_to = 'boot', values_to = 'pb_skew_prob') %>% select(-boot) %>%
                group_by(site) %>%
                dplyr::summarise(across(matches('skew'), list(mean = ~mean(.x, na.rm = TRUE),
                                                              quant2.5 = ~quantile(.x, 0.025, na.rm = TRUE),
                                                              quant25 = ~quantile(.x, 0.25, na.rm = TRUE),
                                                              quant50 = ~quantile(.x, 0.50, na.rm = TRUE),
                                                              quant75 = ~quantile(.x, 0.75, na.rm = TRUE),
                                                              quant97.5 = ~quantile(.x, 0.975, na.rm = TRUE)))))
  
  M_skew_summ = M_skew_boots %>%
    group_by(site) %>%
    dplyr::summarise(across(matches('skew'), list(mean = ~mean(.x, na.rm = TRUE),
                                                  quant2.5 = ~quantile(.x, 0.025, na.rm = TRUE),
                                                  quant25 = ~quantile(.x, 0.25, na.rm = TRUE),
                                                  quant50 = ~quantile(.x, 0.50, na.rm = TRUE),
                                                  quant75 = ~quantile(.x, 0.75, na.rm = TRUE),
                                                  quant97.5 = ~quantile(.x, 0.975, na.rm = TRUE)))) %>%
    left_join(M_skew_probs %>% bind_rows(.id = 'site') %>%
                pivot_longer(-site, names_to = 'boot', values_to = 'M_skew_prob') %>% select(-boot) %>%
                group_by(site) %>%
                dplyr::summarise(across(matches('skew'), list(mean = ~mean(.x, na.rm = TRUE),
                                                              quant2.5 = ~quantile(.x, 0.025, na.rm = TRUE),
                                                              quant25 = ~quantile(.x, 0.25, na.rm = TRUE),
                                                              quant50 = ~quantile(.x, 0.50, na.rm = TRUE),
                                                              quant75 = ~quantile(.x, 0.75, na.rm = TRUE),
                                                              quant97.5 = ~quantile(.x, 0.975, na.rm = TRUE)))))
  
  return(list(pb_skew_boots = PB_skew_boots, M_skew_boots = M_skew_boots, pb_skew_probs = PB_skew_probs,
              M_skew_probs = M_skew_probs, pb_skew_summ = PB_skew_summ, M_skew_summ = M_skew_summ,
              pb_skew_percs = PB_skew_percs, M_skew_percs = M_skew_percs))
}
