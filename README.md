Environmental warming increases the importance of high-turnover energy
channels in stream food webs
================
2022-12-29

This document was created from `README.Rmd` do not edit it directly.

This repository contains the code and document files for Junker, J. R.,
W. F. Cross, J. M. Hood, J. P. Benstead, A. D. Huryn, D. Nelson, J. S.
Ólafsson, and G. M. Gíslason, “*Environmental warming increases the
importance of high-turnover energy channels in stream food webs*”,
submitted for publication as an article in **Ecology**.

The full workflow to recreate the analyses and manuscript are contained
here and employ the `drake`\[<https://docs.ropensci.org/drake/>\]\*
package to recreate the workflow. The workflow is outlined in the
`plan.R` script and the subsequent scripts within the `R/` folder. The
most updated version of all of the model outputs and objects are cached
in a hidden `.drake/` folder. This allows these objects to be called
without the need to completely recreate them. The workflow and status of
each drake “target” can be viewed with `drake::vis_drake_graph()`. The
entire workflow can be rerun from scratch by running `drake::clean()`
followed by `drake::make()`. Some of the objects, particularly the
Bayesian models and random simulations, can take some time to rerun and
therefore it is recommended to load the object rather than rerun the
analysis from scratch unless necessary.

## Reproducibility

The system configuration of the last rerun was:

<details>
<summary>
Reproducibility receipt
</summary>

``` r
## datetime
Sys.time()
```

    ## [1] "2022-12-29 12:11:01 CST"

``` r
## repository
capture.output(if(requireNamespace('git2r', quietly = TRUE)) {
  git2r::repository()
} else {
  c(
    system2("git", args = c("log", "--name-status", "-1"), stdout = TRUE),
    system2("git", args = c("remote", "-v"), stdout = TRUE)
  )
})[-1]
```

    ## [1] "Remote:   main @ origin (git@github.com:jimjunker1/Junker_temperature-energy-flux.git)"
    ## [2] "Head:     [77eedd4] 2022-12-23: Initial commit"

``` r
## session info
sessionInfo()
```

    ## R version 4.2.1 (2022-06-23 ucrt)
    ## Platform: x86_64-w64-mingw32/x64 (64-bit)
    ## Running under: Windows 10 x64 (build 19045)
    ## 
    ## Matrix products: default
    ## 
    ## locale:
    ## [1] LC_COLLATE=English_United States.utf8 
    ## [2] LC_CTYPE=English_United States.utf8   
    ## [3] LC_MONETARY=English_United States.utf8
    ## [4] LC_NUMERIC=C                          
    ## [5] LC_TIME=English_United States.utf8    
    ## 
    ## attached base packages:
    ##  [1] parallel  stats4    grid      stats     graphics  grDevices utils    
    ##  [8] datasets  methods   base     
    ## 
    ## other attached packages:
    ##  [1] knitr_1.39               junkR_0.2.0              emmeans_1.7.5           
    ##  [4] rsample_1.0.0            RInSp_1.2                hillR_0.5.1             
    ##  [7] tidybayes_3.0.2          brms_2.17.0              Rcpp_1.0.9              
    ## [10] rstan_2.21.5             StanHeaders_2.21.0-7     rriskDistributions_2.1.2
    ## [13] cowplot_1.1.1            magick_2.7.3             ggraph_2.0.5            
    ## [16] igraph_1.3.2             betareg_3.1-4            ggpubr_0.4.0            
    ## [19] ggeffects_1.1.2          ggthemes_4.2.4           bbmle_1.0.25            
    ## [22] broom_1.0.0              viridis_0.6.2            viridisLite_0.4.1       
    ## [25] zoib_1.5.5               abind_1.4-5              Formula_1.2-4           
    ## [28] rjags_4-13               coda_0.19-4              MuMIn_1.46.0            
    ## [31] fluxweb_0.2.0            ggridges_0.5.3           gridExtra_2.3           
    ## [34] TTR_0.24.3               httr_1.4.4               lubridate_1.9.0         
    ## [37] timechange_0.1.1         chron_2.3-57             tictoc_1.0.1            
    ## [40] rmarkdown_2.17           dflow_0.0.0.9000         fuzzySim_4.3            
    ## [43] moments_0.14.1           fnmate_0.0.5             furrr_0.3.1             
    ## [46] future_1.26.1            forcats_0.5.1            stringr_1.4.1           
    ## [49] dplyr_1.0.10             purrr_0.3.5              readr_2.1.2             
    ## [52] tidyr_1.2.1              tibble_3.1.8             ggplot2_3.4.0           
    ## [55] tidyverse_1.3.1          plyr_1.8.8               RCurl_1.98-1.7          
    ## [58] rlist_0.4.6.2            gtools_3.9.2.2           data.table_1.14.4       
    ## [61] drake_7.13.4             dotenv_1.0.3             conflicted_1.1.0        
    ## [64] pacman_0.5.1            
    ## 
    ## loaded via a namespace (and not attached):
    ##   [1] estimability_1.4     msm_1.6.9            dygraphs_1.1.1.6    
    ##   [4] multcomp_1.4-20      inline_0.3.19        generics_0.1.3      
    ##   [7] callr_3.7.3          terra_1.5-34         TH.data_1.1-1       
    ##  [10] usethis_2.1.6        tzdb_0.3.0           base64url_1.4       
    ##  [13] xml2_1.3.3           httpuv_1.6.6         assertthat_0.2.1    
    ##  [16] xfun_0.31            hms_1.1.2            ggdist_3.1.1        
    ##  [19] bayesplot_1.9.0      evaluate_0.15        promises_1.2.0.1    
    ##  [22] fansi_1.0.3          progress_1.2.2       dbplyr_2.2.1        
    ##  [25] readxl_1.4.0         DBI_1.1.3            htmlwidgets_1.5.4   
    ##  [28] tensorA_0.36.2       ellipsis_0.3.2       crosstalk_1.2.0     
    ##  [31] backports_1.4.1      markdown_1.1         RcppParallel_5.1.5  
    ##  [34] vctrs_0.5.0          remotes_2.4.2        cachem_1.0.6        
    ##  [37] withr_2.5.0          ggforce_0.3.3        bdsmatrix_1.3-6     
    ##  [40] checkmate_2.1.0      xts_0.12.1           prettyunits_1.1.1   
    ##  [43] crayon_1.5.2         pkgconfig_2.0.3      tweenr_1.0.2        
    ##  [46] pkgload_1.3.0        nlme_3.1-157         devtools_2.4.4      
    ##  [49] nnet_7.3-17          rlang_1.0.6          globals_0.15.1      
    ##  [52] lifecycle_1.0.3      miniUI_0.1.1.1       colourpicker_1.1.1  
    ##  [55] sandwich_3.0-2       filelock_1.0.2       modelr_0.1.8        
    ##  [58] cellranger_1.1.0     distributional_0.3.0 polyclip_1.10-0     
    ##  [61] matrixStats_0.62.0   lmtest_0.9-40        Matrix_1.4-1        
    ##  [64] loo_2.5.1            mc2d_0.1-21          carData_3.0-5       
    ##  [67] zoo_1.8-10           reprex_2.0.1         base64enc_0.1-3     
    ##  [70] processx_3.7.0       bitops_1.0-7         parallelly_1.32.0   
    ##  [73] shinystan_2.6.0      rstatix_0.7.0        ggsignif_0.6.3      
    ##  [76] scales_1.2.1         memoise_2.0.1        magrittr_2.0.3      
    ##  [79] threejs_0.3.3        compiler_4.2.1       RefManageR_1.3.0    
    ##  [82] rstantools_2.2.0     cli_3.3.0            urlchecker_1.0.1    
    ##  [85] listenv_0.8.0        ps_1.7.1             Brobdingnag_1.2-8   
    ##  [88] MASS_7.3-57          tidyselect_1.2.0     stringi_1.7.8       
    ##  [91] yaml_2.3.6           svUnit_1.0.6         ggrepel_0.9.2       
    ##  [94] bridgesampling_1.1-2 tools_4.2.1          rstudioapi_0.13     
    ##  [97] git2r_0.30.1         posterior_1.2.2      farver_2.1.1        
    ## [100] digest_0.6.29        shiny_1.7.3          storr_1.2.5         
    ## [103] car_3.1-0            later_1.3.0          modEvA_3.5          
    ## [106] colorspace_2.0-3     rvest_1.0.2          fs_1.5.2            
    ## [109] eha_2.10.0           splines_4.2.1        expm_0.999-6        
    ## [112] graphlayouts_0.8.0   shinythemes_1.2.0    flexmix_2.3-18      
    ## [115] sessioninfo_1.2.2    xtable_1.8-4         jsonlite_1.8.3      
    ## [118] tidygraph_1.2.1      modeltools_0.2-23    R6_2.5.1            
    ## [121] profvis_0.3.7        pillar_1.8.1         htmltools_0.5.2     
    ## [124] mime_0.12            txtq_0.2.4           glue_1.6.2          
    ## [127] fastmap_1.1.0        DT_0.23              codetools_0.2-18    
    ## [130] pkgbuild_1.3.1       mvtnorm_1.1-3        utf8_1.2.2          
    ## [133] lattice_0.20-45      numDeriv_2016.8-1.1  arrayhelpers_1.1-0  
    ## [136] curl_4.3.3           shinyjs_2.1.0        survival_3.3-1      
    ## [139] munsell_0.5.0        haven_2.5.0          reshape2_1.4.4      
    ## [142] gtable_0.3.1

</details>

\* The `drake` package has been superseded by the `targets` package.
This repository may be transferred to that workflow and/or placed into a
Docker container upon acceptance.
