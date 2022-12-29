#' .. content for \description{} (no empty lines) ..
#'
#' .. content for \details{} ..
#'
#' @title
#' @param nameme1
plot_spp_relFlux <- function(ann_spp_flux =
                             flux_summaries[["annual_spp_flux_summary"]],
                             taxaDf = taxonomic_info) {
  
  
  taxonNameList = list(c("Cricotopus.sylvestris",
                         "Naidid.1",
                         "Nais.spp.",
                         "Other",
                         "Radix.balthica"),
                       c("Chaetocladius.dentiforceps",
                         "Eukiefferiella.sp.",
                         "Lumbricidae",
                         "Macropelopia",
                         "Micropsectra.sp.",
                         "Nais.spp.",
                         "Orthocladius.oblidens",
                         "Other",
                         "Radix.balthica",
                         "Simulium.vittatum"),
                       c("Eukiefferiella.sp.",
                         "Macropelopia",
                         "Micropsectra.sp.",
                         "Nais.spp.",
                         "Orthocladius.oblidens",
                         "Other",
                         "Radix.balthica",
                         "Thysanoptera"),
                       c("Copepoda",
                         "Diamesa.bertrami",
                         "Diamesa.bohemani_zernyi",
                         "Eukiefferiella.sp.",
                         "Lumbricidae",
                         "Other",
                         "Potamophylax.cingulatus",
                         "Prosimulium.ursinum",
                         "Rheocricotopus.effusus",
                         "Simulium.vernum",
                         "Sperchon.glandulosus"),
                       c("Chaetocladius",
                         "Copepoda",
                         "Diamesa.bertrami",
                         "Diamesa.bohemani_zernyi",
                         "Dicranota",
                         "Eukiefferiella.sp.",
                         "Limnophora.riparia",
                         "Lumbricidae",
                         "Micropsectra.sp.",
                         "Orthocladius.frigidus",
                         "Other",
                         "Potamophylax.cingulatus",
                         "Rheocricotopus.effusus",
                         "Simulium.vernum",
                         "Simulium.vittatum",
                         "Sperchon.glandulosus",
                         "Thienemanniella.sp."),
                       c("Eukiefferiella.sp.",
                         "Limnophora.riparia",
                         "Lumbricidae",
                         "Naidid.1",
                         "Ostracoda",
                         "Other",
                         "Potamophylax.cingulatus",
                         "Prosimulium.ursinum",
                         "Simulium.aureum",
                         "Simulium.vittatum",
                         "Sperchon.glandulosus")) 
  
  
  taxonLabelList = list(c("C syl",
                          "F: Naid",
                          "N comm",
                          "Other",
                          "R balt"),
                        c("C dent",
                          "Euk spp.",
                          "F: Lumb",
                          "Macro sp.",
                          "Micro sp.",
                          "N comm",
                          "O obl",
                          "Other",
                          "R balt",
                          "S vitt"),
                        c("Euk spp.",
                          "Macro sp.",
                          "Micro sp.",
                          "N comm",
                          "O obl",
                          "Other",
                          "R balt",
                          "C: Thys"),
                        c("SC: Cope",
                          "D bert",
                          "D boh-zer",
                          "Euk spp.",
                          "F: Lumb",
                          "Other",
                          "P cing",
                          "P urs",
                          "R eff",
                          "S vern",
                          "S gland"),
                        c("Chaet sp.",
                          "SC: Cope",
                          "D bert",
                          "D boh-zer",
                          "Dicra",
                          "Euk spp.",
                          "Limn rip",
                          "F: Lumb",
                          "Micro sp.",
                          "O frig",
                          "Other",
                          "P cing",
                          "R eff",
                          "S vern",
                          "S vitt",
                          "S gland",
                          "Thien sp."),
                        c("Euk sp.",
                          "Limn rip",
                          "F: Lumb",
                          "N comm",
                          "C: Ost",
                          "Other",
                          "P cing",
                          "P urs",
                          "S aur",
                          "S vitt",
                          "S gland"))
  
  labelList = purrr::map2(taxonNameList,taxonLabelList, ~setNames(.y, nm = .x)) %>% unlist %>% .[!duplicated(names(.))]
  
  namesDf = taxaDf %>%
    ungroup %>%
    dplyr::mutate(taxaGroup = recode(taxaGroup, !!!list(Oligochaeta = 'Lumbricidae',
                                                        Ceratopegonid = 'Ceratopogonidae')),
                  cleanGroup = recode(cleanGroup, Ceratopegonid = 'Ceratopogonidae')) %>%
    dplyr::select(taxon, cleanGroup, taxaGroup) %>% unique %>%
    # dplyr::mutate(shortGroup = fuzzySim::spCodes(cleanGroup, nchar.gen = 4, nchar.sp = 2))
    dplyr::mutate(shortGroup = recode(taxon, !!!labelList))

  taxaDf %>%
    dplyr::select(site, taxon, cleanGroup, taxaGroup, flux_mg_m_y_mean) %>%
    dplyr::mutate(site = factor(site, levels = stream_order)) %>%
    group_by(site) %>%
    dplyr::mutate(rel_flux_mean = flux_mg_m_y_mean/sum(flux_mg_m_y_mean, na.rm = TRUE)) %>%#,
                  # rel_flux_mean = case_when(rel_flux_mean <0.01 ~ 0.0101,
                  #                           TRUE ~ rel_flux_mean)) %>%
    junkR::named_group_split(site) -> ann_spp_fluxList
  
  ann_spp_annotation = purrr::map(ann_spp_fluxList, function(x){
    # browser()
    df = x
    lowFluxN = x %>% dplyr::filter(rel_flux_mean <= 0.0101) %>% nrow
    other_annotation = paste0("(n = ",lowFluxN,")")
  }) 
   ann_spp_fluxList = purrr::map(ann_spp_fluxList, function(x){
     # browser()
     df = x
     lowFluxN = x %>% dplyr::filter(rel_flux_mean <= 0.0101) %>% nrow
     other_label = "Other"
     other_annotation = paste0("(n = ",lowFluxN,")")
     x %>%
       dplyr::mutate(taxon = case_when(rel_flux_mean > 0.0101 ~ taxon,
                                       TRUE ~ other_label)) %>%
       group_by(site, taxon) %>%
       dplyr::summarise(rel_flux_mean = sum(rel_flux_mean, na.rm = TRUE)) %>%
       left_join(namesDf, by = "taxon") %>%
       dplyr::mutate(rel_flux_mean = case_when(rel_flux_mean < 0.01 ~ 0.0101,
                                               TRUE ~ rel_flux_mean),
                     taxon = factor(taxon,
                                    levels = df %>%
                                      dplyr::filter(taxon != other_label) %>%
                                      dplyr::arrange(rel_flux_mean) %>%
                                      dplyr::select(taxon) %>% flatten %>% unlist %>% c(other_label,.)))

   } )


# plotTaxa = ann_spp_fluxList %>%
#   purrr::map(~.x %>% ungroup %>%
#                dplyr::filter(!grepl('Other',taxon, .)) %>%
#                dplyr::select(taxaGroup) %>%
#                unique) %>% unlist %>% unique
# 
# plotTaxaColors = setNames(c(viridis(12, option = 'D', begin = 1, end = 0),"#D3D3D3"), nm = c(plotTaxa, NA))

# values = ocecolors[['temperature']][oce_temp_pos]

 plot_relFlux = function(flux_list = NULL, colors = NULL, plot_annotation = FALSE, plot_axes_text = FALSE, oth_annotation = NULL,...){
   other_label = flux_list$taxon[grepl("Other", unlist(flux_list$taxon))] %>% as.character %>% unique
   names(colors)[length(colors)] = other_label
   
   plotDf = flux_list %>% ungroup %>%
   dplyr::mutate(taxaGroup = case_when(is.na(taxaGroup) ~ as.character(taxon),
                                       TRUE ~ taxaGroup),
                 cleanGroup = case_when(is.na(cleanGroup)~ as.character(taxon),
                                        TRUE ~ cleanGroup),
                 shortGroup = case_when(is.na(shortGroup) ~ as.character(taxon),
                                        TRUE ~ shortGroup),
                 taxon = factor(taxon, levels = flux_list %>%
                                  ungroup %>% dplyr::filter(taxon != other_label) %>%
                                  dplyr::arrange(rel_flux_mean) %>%
                                  dplyr::select(taxon) %>% unlist %>%
                                  as.character %>% c(other_label,.)))
   plot = plotDf %>%
     ggplot() +
     geom_col(aes(x = log10(rel_flux_mean*100), y = taxon), color = 'black', fill = ocecolors[['temperature']][colors])+
     # scale_fill_manual(values = colors[names(colors) %in% unlist(flux_list$taxaGroup)])+
     # scale_fill_manual(values = ocecolors[['temperature']][colors])+
     # scale_color_manual(values = .y)+
     scale_x_continuous(limits = c(0,2), expand = c(0.001,0.001), 
                        breaks = c(0,0.5,1,1.5,2), labels = c(0,3, 10, 33, 100))+
     scale_y_discrete(position = 'left', breaks = plotDf$taxon, labels = plotDf$shortGroup)+
     annotate('text', label = oth_annotation, x = Inf, y = 0, family = 'serif', size =  3, hjust = 1, vjust = 0)+
     theme_tufte()+
     theme(legend.position = 'none',
           axis.title.y = element_blank(),
           axis.title.x = element_blank())
   
   if(plot_annotation){
     plot = plot + annotate('text', label = '(a)', x = Inf, y = Inf, family = 'serif',
                     vjust = 1, hjust = 1, size = 4)
   }
   if(!plot_axes_text){
     plot = plot + theme(axis.text.x = element_blank())
   }
   
   return(plot)
 }

 annotateVec = c(T, F, F, F, F, F)
 axesVec = c(F, F, F, F, F, T)
 fillCol = oce_temp_pos
 # debugonce(plot_relFlux)
 ann_spp_plotList = purrr::pmap(list(ann_spp_fluxList,annotateVec,axesVec,fillCol, ann_spp_annotation), 
                                ~plot_relFlux(flux_list = ..1,
                                              plot_annotation = ..2,
                                              plot_axes_text = ..3,
                                              colors = ..4,
                                              oth_annotation = ..5)) %>%
   purrr::map(~ggplotGrob(.x))
 
 widths = max(ann_spp_plotList[[1]]$widths,
                ann_spp_plotList[[2]]$widths,
                ann_spp_plotList[[3]]$widths,
                ann_spp_plotList[[4]]$widths,
                ann_spp_plotList[[5]]$widths,
                ann_spp_plotList[[6]]$widths
              )
 
ann_spp_plotList[[1]]$widths -> ann_spp_plotList[[2]]$widths -> ann_spp_plotList[[3]]$widths -> ann_spp_plotList[[4]]$widths -> ann_spp_plotList[[5]]$widths -> ann_spp_plotList[[6]]$widths -> widths
 
 full_plot = gridExtra::grid.arrange(ann_spp_plotList[[1]],
                                     ann_spp_plotList[[2]],
                                     ann_spp_plotList[[3]],
                                     ann_spp_plotList[[4]],
                                     ann_spp_plotList[[5]],
                                     ann_spp_plotList[[6]], ncol = 1,
                                     layout_matrix = rbind(1,
                                                           1,
                                                           2,
                                                           2,
                                                           2,
                                                           3,
                                                           3,
                                                           3,
                                                           4,
                                                           4,
                                                           4,
                                                           4,
                                                           5,
                                                           5,
                                                           5,
                                                           5,
                                                           6,
                                                           6,
                                                           6,
                                                           6),
                                                  bottom = textGrob(expression("Relative Organic Matter flux (%)"), 
                                                                               gp = gpar(fontfamily = 'serif')))
 
 return(full_plot)
 
}

# ann_spp_plotList = purrr::map2(ann_spp_fluxList,list(plotTaxaColors),
#                                function(x,y){
#                                  # browser()
#                                  # alter the dataframe 
#                                  other_label = x$taxon[grepl("Other",unlist(x$taxon))] %>% as.character %>% unique
#                                  names(y)[length(y)] <- other_label
#                                  
#                                  plotDf = x %>% ungroup %>%
#                                    dplyr::mutate(taxaGroup = case_when(is.na(taxaGroup) ~ as.character(taxon),
#                                                                        TRUE ~ taxaGroup),
#                                                  cleanGroup = case_when(is.na(cleanGroup) ~ as.character(taxon),
#                                                                         TRUE ~ cleanGroup),
#                                                  taxon = factor(taxon,
#                                                                 levels = x %>% ungroup %>%
#                                                                   dplyr::filter(taxon != other_label) %>%
#                                                                   dplyr::arrange(rel_flux_mean) %>%
#                                                                   dplyr::select(taxon) %>% unlist %>% as.character  %>% c(other_label,.)))
#                                  
#                                  plotDf %>%
#                                    ggplot() +
#                                    geom_col(aes(x = log10(rel_flux_mean*100), y = taxon, fill = taxaGroup), color = 'black')+
#                                    scale_fill_manual(values = y[names(y) %in% unlist(x$taxaGroup)])+
#                                    # scale_color_manual(values = .y)+
#                                    scale_x_reverse(limits = c(2,0), expand = c(0.001,0.001))+
#                                    scale_y_discrete(position = 'right', breaks = plotDf$taxon, labels = plotDf$cleanGroup)+
#                                    theme_tufte()+
#                                    theme(legend.position = 'none',
#                                          axis.title.y = element_blank(),
#                                          axis.title.x = element_blank())
#                                }) %>%
#   purrr::map(~ggplotGrob(.x))