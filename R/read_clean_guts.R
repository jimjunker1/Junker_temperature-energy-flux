##' .. content for \description{} (no empty lines) ..
##'
##' .. content for \details{} ..
##'
##' @title

read_clean_guts <- function() {

  #####    Create taxon rename lists   #####
  
  old_names <- list(c("Csylvestris"),
                    c("Radix", "RadixBalthica","Radix.Balthica","Radix.balthica"),
                    c("Eukie","Eminor","Eukiefferiella sp."),
                    c("Ooblidens"),
                    c("Tany"),
                    c("Theinem","Theinemanniella"),
                    c("Simuliumvittatum", "Svitt", "Simulium.vittatum"),
                    c("Oligochaeta.A","Oligochaeta.B","Oligo", "Lumb 3","Lumb"), 
                    c("Ofridgidus", "Orthocladius fridgidus","Orthocladius.fridgidus"),
                    c("Rhecricotopus","Rheocricotpus","Rheocricotopus.effusus","Rheocricotopus"),
                    c("Orthoclad"),
                    c("Ceratopegonid","Ceratopogonidae.A","Ceratopogonidae.B"),
                    c("Chaetocladius dentiforceps","Odentiformes"),
                    c("Diamesa.bohemani_zernyi"),
                    c("Diamesa.bertrami"),
                    c("Eukiefferiella.claripennis"),
                    c("Eukiefferiella.minor","Eukiefferiella.mino"),
                    c("Micropsectra", "Micropsectra.sp."),
                    c("Parochlus"),
                    c("Potomaphylax.cingulatus"),
                    c("Prosimulium","Prosimulium.ursinum"),
                    c("Simulium.vernum"),
                    c("Sperchon"),
                    c("Oligochaeta.A"),
                    c("Clinocera"))
  
  new_names <- list("Cricotopus sylvestris",
                    "Radix balthica",
                    "Eukiefferiella sp.",
                    "Orthocladius oblidens",
                    "Macropelopia",
                    "Thienemanniella sp.",
                    "Simulium vittatum",
                    "Lumbricidae",
                    "Orthocladius frigidus",
                    "Rheocricotopus effusus",
                    "Orthocladius spp.",
                    "Ceratopogonid",
                    "Chaetocladius dentiforceps",
                    "Diamesa bohemani_zernyi",
                    "Diamesa bertrami",
                    "Eukiefferiella sp.",
                    "Eukiefferiella sp.",
                    "Micropsectra sp.",
                    "Parochlus sp.",
                    "Potamophylax cingulatus",
                    "Prosimulium ursinum", 
                    "Simulium vernum",
                    "Sperchon glandulosus",
                    "Lumbricidae",
                    "Clinocera stagnalis")
  
  taxon_name_keyval = setNames(rep(new_names, lengths(old_names)), unlist(old_names))
  #####    Gut Content Data Import ####
  ##read in gut data
  hver_path = "./data/raw-data/gut_data/hver/"
  hver_gut_paths = as.list(dir(hver_path, pattern ="full.csv", full.names = FALSE, include.dirs = FALSE, recursive = TRUE))
  hver_gut_files= suppressMessages(purrr::map(hver_gut_paths, ~read_csv(file = paste0(hver_path, .x))))
  hver_gut_files = purrr::map(hver_gut_files, function(x){ if(dim(x)[2] > 2){
    x = x[,-1];return(x)} else{return(x)}})
  
  hver_gut_columns = purrr::map(hver_gut_paths, ~.x %>%
                           str_replace(.,pattern = "IMG_", "IMG") %>%
                           strsplit(., '[/_.][[:space:]]*') %>%
                           as.data.frame() %>%
                           .[c(2,4:7),] %>%
                           set_names(c("site","date","taxon","id", "pic")))
  hver_gut_columns = bind_rows(hver_gut_columns)
  hver_gut_columns = split(hver_gut_columns, seq(nrow(hver_gut_columns)))
  hver_gut_contents = map2(hver_gut_columns, hver_gut_files, ~.x %>%
                            junkR::cbind_fill(.y))
  
  names = c("site","date","taxon","id", "pic", "diet_item","area")
  hver_gut_contents = lapply(hver_gut_contents, setNames, names)
  
  hver_gut_df = bind_rows(hver_gut_contents)
  
  hver_gut_summ <<- hver_gut_df %>%
   dplyr::mutate(diet_item = str_replace(diet_item, ".*ga.*", "green_algae"),
           diet_item = str_replace(diet_item, ".*cyano.*", "cyanobacteria"),
           diet_item = str_replace(diet_item, ".*vascular.*", "plant_material"),
           diet_item = str_replace(diet_item, ".*dino.*","green_algae"),
           diet_item = str_replace(diet_item, ".*microcystis.*", "cyanobacteria"),
           diet_item = str_replace(diet_item, ".*chaeto.*","green_algae"),
           diet_item = recode(diet_item, amdet = "amorphous_detritus", dia = "diatom",
                              dino = "dinoflaggelate", fila = "filamentous"),
           taxon = recode(taxon, !!!taxon_name_keyval)) %>%
    tidyr::complete(diet_item, nesting(site, date, taxon, id, pic), fill = list(area = 0.01)) %>%
    group_by(site,date,taxon,id,diet_item) %>%
    dplyr::summarise(item_area = sum(area)) %>%
    ungroup() %>% group_by(site, date,id, taxon) %>%
    dplyr::mutate(rel_area = item_area/sum(item_area), 
                  month_id = stringr::str_extract(date, "\\D{3,4}"))%>% droplevels

    ##st6
  st6_path = "./data/raw-data/gut_data/st6/"
  st6_gut_paths = as.list(dir(st6_path, pattern = "full.csv", full.names = FALSE, include.dirs = FALSE, recursive = TRUE))
  st6_gut_files = suppressMessages(map(st6_gut_paths, ~read_csv(file = paste0(st6_path, .x))))
  st6_gut_files = map(st6_gut_files, function(x){ if(dim(x)[2] > 2){
    x = x[,-1];return(x)} else{return(x)}})
  st6_gut_columns = map(st6_gut_paths, ~.x %>%
                          str_replace(.,pattern = "IMG_", "IMG") %>%
                          strsplit(., '[/_.][[:space:]]*') %>%
                          as.data.frame() %>%
                          .[c(2,4:7),] %>%
                          set_names(c("site", "date","taxon","id", "pic")))
  st6_gut_columns = bind_rows(st6_gut_columns)%>% droplevels
  st6_gut_columns = split(st6_gut_columns, seq(nrow(st6_gut_columns)))
  st6_gut_contents = map2(st6_gut_columns, st6_gut_files, ~.x %>%
                            junkR::cbind_fill(.y))
  st6_gut_contents = lapply(st6_gut_contents, setNames, names)
  
  st6_gut_df = bind_rows(st6_gut_contents)
  
  st6_gut_summ <<- st6_gut_df %>%
    dplyr::mutate(diet_item = str_replace(diet_item, ".*ga.*", "green_algae"),
           diet_item = str_replace(diet_item, ".*cyano.*", "cyanobacteria"),
           diet_item = str_replace(diet_item, ".*chaeto.*", "green_algae"),
           diet_item = str_replace(diet_item, ".*vascula.*", "plant_material"),
           diet_item = str_replace(diet_item, ".*dino.*","green_algae"),
           diet_item = str_replace(diet_item, ".*microcystis.*", "cyanobacteria"),
           diet_item = str_replace(diet_item, ".*animal.*","animal"),
           diet_item = recode(diet_item, amdet = "amorphous_detritus", dia = "diatom",
                              dino = "dinoflaggelate", fila = "filamentous"),
           taxon = recode(taxon, !!!taxon_name_keyval)) %>%
    tidyr::complete(diet_item, nesting(site, date, taxon, id, pic), fill = list(area = 0.01)) %>%
    group_by(site,date,taxon,id,diet_item) %>%
    dplyr::summarise(item_area = sum(area)) %>%
    ungroup() %>% group_by(site, date,id, taxon) %>%
    dplyr::mutate(rel_area = item_area/sum(item_area), 
                  month_id = stringr::str_extract(date, "\\D{3,4}"))%>% droplevels
  #st9
  st9_path = "./data/raw-data/gut_data/st9/"
  st9_gut_paths = as.list(dir(st9_path, pattern = ".csv", full.names = FALSE, include.dirs = FALSE, recursive = TRUE))
  st9_gut_files = suppressMessages(map(st9_gut_paths, ~read_csv(file = paste0(st9_path, .x))))
  st9_gut_files = map(st9_gut_files, function(x){ if(dim(x)[2] > 2){
    x = x[,-1];return(x)} else{return(x)}})
  st9_gut_columns = map(st9_gut_paths, ~.x %>%
                          str_replace(.,pattern = "IMG_", "IMG") %>%
                          strsplit(., '[/_.][[:space:]]*') %>%
                          as.data.frame() %>%
                          .[c(2,4:7),] %>%
                          set_names(c("site","date","taxon","id", "pic")))
  st9_gut_columns = bind_rows(st9_gut_columns)%>% droplevels
  st9_gut_columns = split(st9_gut_columns, seq(nrow(st9_gut_columns)))
  st9_gut_contents = map2(st9_gut_columns, st9_gut_files, ~.x %>%
                            junkR::cbind_fill(.y))
  
  st9_gut_contents = lapply(st9_gut_contents, setNames, names)
  
  st9_gut_df = bind_rows(st9_gut_contents)
  
  st9_gut_summ <<- st9_gut_df %>%
    dplyr::mutate(diet_item = str_replace(diet_item, ".*ga.*", "green_algae"),
           diet_item = str_replace(diet_item, ".*cyano.*", "cyanobacteria"),
           diet_item = str_replace(diet_item, ".*vascular.*", "plant_material"),
           diet_item = str_replace(diet_item, ".*leaf.*","plant_material"),
           diet_item = str_replace(diet_item, ".*dino.*","green_algae"),
           diet_item = str_replace(diet_item, ".*microcystis.*", "cyanobacteria"),
           diet_item = str_replace(diet_item, ".*fila.*","filamentous"),
           diet_item = str_replace(diet_item, ".*animal.*","animal"),
           diet_item = recode(diet_item, amdet = "amorphous_detritus", dia = "diatom",
                              dino = "dinoflaggelate", fila = "filamentous"),
           taxon = recode(taxon, !!!taxon_name_keyval)) %>%
    tidyr::complete(diet_item, nesting(site, date, taxon,id, pic), fill = list(area = 0.01)) %>%
    group_by(site,date,taxon,id,diet_item) %>%
    dplyr::summarise(item_area = sum(area)) %>%
    ungroup() %>% group_by(site, date,id, taxon) %>%
    dplyr::mutate(rel_area = item_area/sum(item_area), 
                  month_id = stringr::str_extract(date, "\\D{3,4}"))%>% droplevels
  
  #st14                        
  st14_path = "./data/raw-data/gut_data/st14/"
  st14_gut_paths = as.list(dir(st14_path, pattern = 'full.csv', full.names = FALSE, include.dirs = FALSE, recursive = TRUE))
  st14_gut_files = suppressMessages(map(st14_gut_paths, ~read_csv(file = paste0(st14_path, .x))))
  st14_gut_files = map(st14_gut_files, function(x){ if(dim(x)[2] > 2){
    x = x[,-1];return(x)} else{return(x)}})
  st14_gut_columns = map(st14_gut_paths, ~.x %>%
                           str_replace(.,pattern = "IMG_", "IMG") %>%
                           strsplit(., '[/_.][[:space:]]*') %>%
                           as.data.frame() %>%
                           .[c(2,4:7),] %>%
                           set_names(c("site", "date","taxon","id","pic")))
  st14_gut_columns = st14_gut_columns %>% bind_rows
  st14_gut_columns = split(st14_gut_columns, seq(nrow(st14_gut_columns)))
  st14_gut_contents = map2(st14_gut_columns, st14_gut_files, ~.x %>%
                             junkR::cbind_fill(.y))
  st14_gut_contents = lapply(st14_gut_contents, setNames, names)
  st14_gut_df = bind_rows(st14_gut_contents)
  
  st14_gut_summ <<- st14_gut_df %>%
    dplyr::mutate(diet_item = str_replace(diet_item, ".*ga.*", "green_algae"),
           diet_item = str_replace(diet_item, ".*cyano.*", "cyanobacteria"),
           diet_item = str_replace(diet_item, ".*vascular.*", "plant_material"),
           diet_item = str_replace(diet_item, ".*microcystis.*", "cyanobacteria"),
           diet_item = str_replace(diet_item, ".*fila.*","filamentous"),
           diet_item = str_replace(diet_item, ".*animal.*","animal"),
           diet_item = str_replace(diet_item, ".*dino.*","green_algae"),
           diet_item = str_replace(diet_item, ".*dia.*","diatom"),
           diet_item = str_replace(diet_item, ".*dion.*","chaeto"),
           diet_item = str_replace(diet_item, ".*chaeto.*","green_algae"), 
           diet_item = recode(diet_item, amdet = "amorphous_detritus", dia = "diatom",
                              dino = "dinoflaggelate", fila = "filamentous"),
           taxon = recode(taxon, !!!taxon_name_keyval)) %>%
    tidyr::complete(diet_item, nesting(site, date, taxon, id, pic), fill = list(area = 0.01)) %>%
    group_by(site,date,taxon,id,diet_item) %>%
    dplyr::summarise(item_area = sum(area)) %>%
    ungroup() %>% group_by(site, date,id, taxon) %>%
    dplyr::mutate(rel_area = item_area/sum(item_area), 
                  month_id = stringr::str_extract(date, "\\D{3,4}"))%>% droplevels
  #st7
  st7_gut_summ <<- read.csv(file = "./data/raw-data/gut_data/st7/st7_gut_full.csv", T) %>%
    plyr::rbind.fill(data.frame(diet_item = "green_algae")) %>%
  # st7_gut_summ <<- st7_gut_summ %>% 
    dplyr::mutate(diet_item = str_replace(diet_item, ".*dia.*","diatom"),
                                           diet_item = str_replace(diet_item, ".*dino.*", "green_algae"),
                                           diet_item = str_replace(diet_item, ".*Bryo.*", "plant_material"),
                                           diet_item = str_replace(diet_item, ".*Ulva.*", "filamentous"),
                  taxon = recode(taxon, !!!taxon_name_keyval)) %>%
    group_by(site, date, taxon, diet_item) %>% 
    dplyr::summarise(rel_area = sum(rel_area)) %>% ungroup %>% 
    tidyr::complete(diet_item, nesting(site, date, taxon), fill = list(rel_area = 0.01)) %>%
    dplyr::filter(!is.na(taxon)) %>%
    dplyr::mutate(rel_area = ifelse(rel_area == 0, 0.001, rel_area),
                  `count` = rel_area*1000) %>%
    group_by(site, date, taxon) %>%
    dplyr::mutate(item_count = sum(count), rel_area = count/item_count) %>%
    select(-item_count, -count) %>% ungroup %>%
    dplyr::mutate(month_id = "Jul",
                  `id` = NA) %>% as_tibble
  
  #oh2
  oh2_gut_summ <<- read.csv(file = "./data/raw-data/gut_data/oh2/oh2_gut_full.csv", T) %>%
    plyr::rbind.fill(data.frame(diet_item = "green_algae")) %>%
    # oh2_gut_summ <<- oh2_gut_summ %>% 
    dplyr::mutate(diet_item = str_replace(diet_item, ".*dia.*","diatom"),
                                           diet_item = str_replace(diet_item, ".*dino.*", "green_algae"),
                                           diet_item = str_replace(diet_item, ".*Bryo.*", "plant_material"),
                                           diet_item = str_replace(diet_item, ".*Ulva.*", "filamentous"),
                  taxon = recode(taxon, !!!taxon_name_keyval)) %>%
    group_by(site, date, taxon, diet_item) %>% 
    dplyr::summarise(rel_area = sum(rel_area)) %>% ungroup %>% 
    tidyr::complete(diet_item, nesting(site, date, taxon), fill = list(rel_area = 0.01)) %>%
    dplyr::filter(!is.na(taxon)) %>%
    dplyr::mutate(rel_area = ifelse(rel_area == 0, 0.001, rel_area),
                  count = rel_area*1000) %>%
    group_by(site, date, taxon) %>%
    dplyr::mutate(item_count = sum(count), rel_area = count/item_count) %>% 
    select(-item_count, -count) %>% ungroup %>%
    dplyr::mutate(month_id = "Jul",
                  `id` = NA) %>% as_tibble
  ## create a list of gut data
  diet_list = list(hver = hver_gut_summ, st6 = st6_gut_summ, st9 = st9_gut_summ, st7 = st7_gut_summ, oh2 = oh2_gut_summ, st14 = st14_gut_summ) %>%
    rlist::list.subset(names(stream_order_list))

  ## create list of resource names 
  resource_list = diet_list %>% map(~.x %>% ungroup %>% select(diet_item) %>% unique) %>% bind_rows %>% unique %>%
    rep(.,6) %>% setNames(., nm = levels(stream_order)) %>%rlist::list.subset(names(stream_order_list))
  
  ## fill out each diet with list
  # diet_list = map2(diet_list, resource_list, ~..1 %>% group_by(site, date, taxon) %>%
  #                    tidyr::complete(diet_item, nesting(taxon), fill = list(item_area = 0,
  #                                                                           rel_area = 0)))
  # 
  return(list(diet_list = diet_list, resource_list = resource_list))

}
