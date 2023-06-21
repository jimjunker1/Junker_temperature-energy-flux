##' .. content for \description{} (no empty lines) ..
##'
##' .. content for \details{} ..
##'
##' @title
##' @param lorenz_analysis
plot_trait_lorenz <- function(lorenz_analysis, spp_rankings_summary) {
    
 # spp_rankings_summary = map(spp_rankings_summary, ~.x %>% bind_rows)
# 
 lorenz_analysis[["relative_flux_df"]] %>%
    ggplot(aes(x = rel_spp, y = rel_flux)) +
    geom_abline(color = "black")+
    geom_abline(intercept = 1, slope = -1, color = "black", linetype = "dotted")+
    geom_line(aes(group = boot_id), linewidth = 1, color = 'grey', alpha = 0.5) +
    geom_line(data = lorenz_analysis[["relative_flux_summary"]], aes(x = rel_spp, y = rel_flux, color = site), linewidth = 1.5)+
    scale_x_continuous(name = "Cumulative species", limits = c(0,1), expand = c(0,0.03))+
    scale_y_continuous(name = "Cumulative flux", limits = c(0,1), expand = c(0,0.03))+
    scale_colour_manual(values = ocecolors[['temperature']][oce_temp_pos], labels = stream_temp_labels)+
    theme_tufte(ticks = TRUE) +
    geom_rangeframe(sides = "lb")+
    theme(legend.position = c(0,1),
          legend.justification = c("left", "top"),
          legend.title = element_blank(),
          legend.spacing.y = unit(0.0003,'cm'),
          legend.key.size = unit(0.4,'cm'),
          strip.text = element_blank()) +
    facet_wrap(~site, ncol = 2) -> lorenz_flux_plot

    plot_legend = cowplot::get_legend(lorenz_flux_plot + guides(color = guide_legend(byrow = TRUE, nrow = 1)))
    plot_legend$grobs <- lapply(plot_legend$grobs, function(z) modifyList(z, list(x = unit(0.5, 'npc'), just = "center")))
    # plot_legend$layout
#     
#     pb_label = grid::textGrob(expression("Increasing"~italic("P:B")), x = unit(0.5, 'npc'), just = "centre",
#                              gp = gpar(fontfamily = 'serif', fontsize = 8))
#     pb_box = grid::roundrectGrob(x = unit(0.5, 'npc'), y = unit(0.5, 'npc'),
#                                 width = unit(0.4, 'npc'), height = unit(0.8, 'npc'),
#                                 just = "centre")
#     pb_arrow = segmentsGrob(x0 = unit(0.1,'npc'), x1 = unit(0.9, 'npc'),
#                            y0 = unit(0.5,'npc'), y1 = unit(0.5, 'npc'),
#                            arrow = arrow(type = 'closed'), gp = gpar(fill = 'black', lex = 0.8))
#     pb_header = grid::grobTree(pb_arrow, pb_box, pb_label)
#     
#     lorenz_analysis[["pb_ranking_boots"]] %>%
#         ggplot(aes(x = rel_spp, y = rel_flux)) +
#         geom_abline(color = "black")+
#         geom_abline(intercept = 1, slope = -1, color = "black", linetype = "dotted")+
#         geom_line(aes(group = boot_id), color = "grey", alpha = 0.5) +
#         geom_line(data = spp_rankings_summary[["PB_spp_rank"]], aes(x = rel_spp, y = rel_flux, color = site), size = 1.5) +
#         scale_x_continuous(name = "Cumulative species", limits = c(0,1), expand = c(0,0.03))+
#         scale_y_continuous(name = "Cumulative flux", limits = c(0,1), expand = c(0,0.03))+
#         scale_colour_manual(values = ocecolors[['temperature']][oce_temp_pos], labels = stream_temp_labels)+
#         theme_tufte(ticks = TRUE) +
#         geom_rangeframe(sides = "lb")+
#         theme(legend.position = 'none',
#               legend.justification = c("left", "top"), 
#               legend.title = element_blank(),
#               legend.spacing.y = unit(0.0003,'cm'),
#               legend.key.size = unit(0.4,'cm'),
#               strip.text = element_blank(), axis.title = element_blank()) +
#         facet_wrap(~site, ncol = 1) -> pb_lorenz_flux_plot
#     
#     M_label = grid::textGrob(expression("Increasing"~italic("M")), x = unit(0.5, 'npc'), just = "centre",
#                              gp = gpar(fontfamily = 'serif', fontsize = 8))
#     M_box = grid::roundrectGrob(x = unit(0.5, 'npc'), y = unit(0.5, 'npc'),
#                            width = unit(0.4, 'npc'), height = unit(0.8, 'npc'),
#                            just = "centre")
#     M_arrow = segmentsGrob(x0 = unit(0.1,'npc'), x1 = unit(0.9, 'npc'),
#                  y0 = unit(0.5,'npc'), y1 = unit(0.5, 'npc'),
#                  arrow = arrow(type = 'closed'), gp = gpar(fill = 'black', lex = 0.8))
#     M_header = grid::grobTree(M_arrow, M_box, M_label)
#     
# 
#     lorenz_analysis[["M_ranking_boots"]] %>%
#         ggplot(aes(x = rel_spp, y = rel_flux)) +
#         geom_abline(color = "black")+
#         geom_abline(intercept = 1, slope = -1, color = "black", linetype = "dotted")+
#         geom_line(aes(group = boot_id), color = "grey", alpha = 0.5) +
#         geom_line(data = spp_rankings_summary[["M_spp_rank"]], aes(x = rel_spp, y = rel_flux, color = site), size = 1.5) +
#         scale_x_continuous(name = "Cumulative species", limits = c(0,1), expand = c(0,0.03))+
#         scale_y_continuous(name = "Cumulative flux", limits = c(0,1), expand = c(0,0.03))+
#         scale_colour_manual(values = ocecolors[['temperature']][oce_temp_pos], labels = stream_temp_labels)+
#         theme_tufte(ticks = TRUE) +
#         geom_rangeframe(sides = "lb")+
#         theme(legend.position = "none",
#               legend.justification = c("left", "top"), 
#               legend.title = element_blank(),
#               legend.spacing.y = unit(0.0003,'cm'),
#               legend.key.size = unit(0.4,'cm'),
#               strip.text = element_blank(), axis.title = element_blank()) +
#         facet_wrap(~site, ncol = 1) -> M_lorenz_flux_plot
# 
#     lorenz_pbM_plot = gridExtra::grid.arrange(plot_legend, M_header,pb_header,
#                                               M_lorenz_flux_plot, pb_lorenz_flux_plot,
#                                               layout_matrix = rbind(c(1,1),
#                                                                     c(2,3),
#                                                                     c(4,5),
#                                                                     c(4,5),
#                                                                     c(4,5),
#                                                                     c(4,5),
#                                                                     c(4,5),
#                                                                     c(4,5),
#                                                                     c(4,5),
#                                                                     c(4,5),
#                                                                     c(4,5),
#                                                                     c(4,5),
#                                                                     c(4,5),
#                                                                     c(4,5),
#                                                                     c(4,5),
#                                                                     c(4,5)),
#                                               ncol = 2,
#                                               bottom = textGrob("Cumulative relative species", vjust = 0, gp = gpar(fontfamily = 'serif')),
#                                               left = textGrob("Cumulative relative flux", rot = 90, vjust = 1, gp = gpar(fontfamily = 'serif')))
        
    lorenz_analysis[["bio_ranking_boots"]] %>%
        ggplot(aes(x = rel_bio, y = rel_flux)) +
        geom_abline(color = "black")+
        geom_abline(intercept = 1, slope = -1, color = "black", linetype = "dotted")+
        geom_line(aes(group = boot_id), color = "grey", alpha = 0.5) +
        geom_line(data = spp_rankings_summary[["bio_spp_rank"]], aes(x = rel_bio, y = rel_flux, color = site), linewidth = 1.5) +
        scale_x_continuous(name = "Cumulative Biomass", limits = c(0,1), expand = c(0,0.03))+
        scale_y_continuous(name = "Cumulative flux", limits = c(0,1), expand = c(0,0.03))+
        scale_colour_manual(values = ocecolors[['temperature']][oce_temp_pos], labels = stream_temp_labels)+
        theme_tufte(ticks = TRUE) +
        geom_rangeframe(sides = "lb")+
        theme(legend.position = c(0,1),
              legend.justification = c("left", "top"), 
              legend.title = element_blank(),
              legend.spacing.y = unit(0.0003,'cm'),
              legend.key.size = unit(0.4,'cm'),
              strip.text = element_blank()) +
        facet_wrap(~site, ncol = 2) -> bio_lorenz_flux_plot
    
    return(list(lorenz_flux_plot = lorenz_flux_plot, bio_lorenz_flux_plot = bio_lorenz_flux_plot))
    
    # return(list(lorenz_flux_plot = lorenz_flux_plot, pb_lorenz_flux_plot = pb_lorenz_flux_plot, M_lorenz_flux_plot = M_lorenz_flux_plot, bio_lorenz_flux_plot = bio_lorenz_flux_plot,
    #             lorenz_pbM_plot = lorenz_pbM_plot))
    
}
