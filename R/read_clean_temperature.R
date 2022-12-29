##' .. content for \description{} (no empty lines) ..
##'
##' .. content for \details{} ..
##'
##' @title

read_clean_temperature <- function() {

  ####    import temperature data    ####
    ann_stream_temps = readRDS(file = "./data/annual_tempkt.rds") #annual mean boltzmann temperature
  names(ann_stream_temps) =  tolower(gsub("(.*)_.*$", "\\1", names(ann_stream_temps))) #setting names of annual temps
  ann_stream_temps = data.frame(t(ann_stream_temps)) %>% rownames_to_column("SITE") %>% 
    dplyr::rename(tempkt =  starts_with('t')) %>% dplyr::mutate(tempC = overkt_to_C(tempkt)) %>% dplyr::mutate(SITE = factor(SITE, levels = stream_order)) #rename to annual temperature to 'tempkt'
  stream_temps_list = readRDS(file = "./data/stream_temps_full.rds") %>% rlist::list.subset(names(stream_order_list)) #interval temperature list
  stream_temps_bound = stream_temps_list %>% bind_rows() %>% dplyr::mutate(SITE = factor(SITE, levels = stream_order)) #interval temperature df
  
  ####    End Temperature data import    ####
  
  return(list(stream_order = stream_order, stream_order_list = stream_order_list, stream_temp_labels = stream_temp_labels, 
              ann_stream_temps = ann_stream_temps, stream_temps_list = stream_temps_list, stream_temps_bound = stream_temps_bound))

}
