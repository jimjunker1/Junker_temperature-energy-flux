Environmental warming increases the importance of high-turnover energy
channels in stream food webs
================
2023-06-18

This document was created from `README.Rmd` do not edit it directly.

This repository contains the code and document files for Junker, J. R.,
W. F. Cross, J. M. Hood, J. P. Benstead, A. D. Huryn, D. Nelson, J. S.
Ólafsson, and G. M. Gíslason, “*Environmental warming increases the
importance of high-turnover energy channels in stream food webs*”,
submitted for publication as an article in **Ecology**.

Package versions dependencies are documented with the `renv` package
version 0.17.3 and explicitly with the `renv.lock` file. The `renv`
auto-loader has been deactivated by removing the .Rprofile file. To
activate the `renv` environment with the appropriate packages, use
`renv::restore()` and choose `yes` when asked to initiate the project.
See `renv` documentation for more information here
\[<https://rstudio.github.io/renv/articles/collaborating.html>\].

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

    ## [1] "2023-06-18 14:26:27 CDT"

``` r
## repository
capture.output(if(requireNamespace('git2r', quietly = TRUE)) {
  git2r::repository()
} else {
  c(
    system2("git", args = c("log", "--name-status", "-1"), stdout = TRUE),
    system2("git", args = c("remote", "-v"), stdout = TRUE)
  )
})[-1]# [-1] to remove local file structure
```

    ## [1] "Remote:   clean-project @ clean-project (git@github.com:jimjunker1/Junker_temperature-energy-flux.git)"    
    ## [2] "Head:     [3667b80] 2023-06-06: first round of changes to revision #1 with additional discussion paragraph"

``` r
## session info
sessionInfo()
```

    ## R version 4.2.3 (2023-03-15 ucrt)
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
    ##  [1] parallel  stats4    grid      stats     graphics  grDevices datasets 
    ##  [8] utils     methods   base     
    ## 
    ## other attached packages:
    ##  [1] rmarkdown_2.21           knitr_1.42               junkR_0.2.0             
    ##  [4] emmeans_1.8.6            rsample_1.1.1            RInSp_1.2.5             
    ##  [7] hillR_0.5.1              tidybayes_3.0.4          brms_2.19.0             
    ## [10] Rcpp_1.0.10              rstan_2.26.13            StanHeaders_2.26.13     
    ## [13] rriskDistributions_2.1.2 cowplot_1.1.1            magick_2.7.4            
    ## [16] ggraph_2.1.0             igraph_1.4.2             betareg_3.1-4           
    ## [19] ggpubr_0.6.0             ggeffects_1.2.3          ggthemes_4.2.4          
    ## [22] bbmle_1.0.25             broom_1.0.4              viridis_0.6.3           
    ## [25] viridisLite_0.4.1        here_1.0.1               MuMIn_1.47.5            
    ## [28] ggridges_0.5.4           gridExtra_2.3            TTR_0.24.3              
    ## [31] httr_1.4.5               chron_2.3-61             tictoc_1.2              
    ## [34] dflow_0.0.0.9000         fuzzySim_4.9.9           moments_0.14.1          
    ## [37] fnmate_0.0.5             furrr_0.3.1              future_1.32.0           
    ## [40] lubridate_1.9.2          forcats_1.0.0            stringr_1.5.0           
    ## [43] dplyr_1.1.1              purrr_1.0.1              readr_2.1.4             
    ## [46] tidyr_1.3.0              tibble_3.2.1             ggplot2_3.4.2           
    ## [49] tidyverse_2.0.0          plyr_1.8.8               RCurl_1.98-1.12         
    ## [52] rlist_0.4.6.2            gtools_3.9.4             data.table_1.14.8       
    ## [55] drake_7.13.5             dotenv_1.0.3             conflicted_1.2.0        
    ## [58] pacman_0.5.1            
    ## 
    ## loaded via a namespace (and not attached):
    ##   [1] estimability_1.4.1   msm_1.7              coda_0.19-4         
    ##   [4] dygraphs_1.1.1.6     inline_0.3.19        generics_0.1.3      
    ##   [7] callr_3.7.3          terra_1.7-29         usethis_2.1.6       
    ##  [10] tzdb_0.3.0           base64url_1.4        xml2_1.3.3          
    ##  [13] httpuv_1.6.9         xfun_0.38            rethinking_2.31     
    ##  [16] hms_1.1.3            ggdist_3.3.0         bayesplot_1.10.0    
    ##  [19] evaluate_0.20        promises_1.2.0.1     fansi_1.0.4         
    ##  [22] progress_1.2.2       DBI_1.1.3            htmlwidgets_1.6.2   
    ##  [25] tensorA_0.36.2       ellipsis_0.3.2       crosstalk_1.2.0     
    ##  [28] backports_1.4.1      V8_4.3.0             permute_0.9-7       
    ##  [31] markdown_1.6         RcppParallel_5.1.7   vctrs_0.6.1         
    ##  [34] cmdstanr_0.5.2       remotes_2.4.2        abind_1.4-5         
    ##  [37] cachem_1.0.7         withr_2.5.0          ggforce_0.4.1       
    ##  [40] bdsmatrix_1.3-6      checkmate_2.1.0      vegan_2.6-4         
    ##  [43] xts_0.13.0           prettyunits_1.1.1    cluster_2.1.4       
    ##  [46] crayon_1.5.2         labeling_0.4.2       pkgconfig_2.0.3     
    ##  [49] tweenr_2.0.2         nlme_3.1-162         pkgload_1.3.2       
    ##  [52] nnet_7.3-18          devtools_2.4.5       rlang_1.1.0         
    ##  [55] globals_0.16.2       lifecycle_1.0.3      miniUI_0.1.1.1      
    ##  [58] colourpicker_1.2.0   sandwich_3.0-2       filelock_1.0.2      
    ##  [61] distributional_0.3.2 rprojroot_2.0.3      polyclip_1.10-4     
    ##  [64] matrixStats_0.63.0   lmtest_0.9-40        Matrix_1.5-4        
    ##  [67] loo_2.6.0            mc2d_0.1-22          carData_3.0-5       
    ##  [70] zoo_1.8-12           base64enc_0.1-3      processx_3.8.0      
    ##  [73] bitops_1.0-7         shape_1.4.6          parallelly_1.35.0   
    ##  [76] shinystan_2.6.0      rstatix_0.7.2        ggsignif_0.6.4      
    ##  [79] scales_1.2.1         memoise_2.0.1        magrittr_2.0.3      
    ##  [82] bibtex_0.5.1         threejs_0.3.3        compiler_4.2.3      
    ##  [85] RefManageR_1.4.0     rstantools_2.3.1     cli_3.6.1           
    ##  [88] urlchecker_1.0.1     listenv_0.9.0        ps_1.7.4            
    ##  [91] Brobdingnag_1.2-9    Formula_1.2-5        mgcv_1.8-42         
    ##  [94] MASS_7.3-58.3        tidyselect_1.2.0     stringi_1.7.12      
    ##  [97] highr_0.10           yaml_2.3.7           svUnit_1.0.6        
    ## [100] ggrepel_0.9.3        bridgesampling_1.1-2 tools_4.2.3         
    ## [103] timechange_0.2.0     rstudioapi_0.14      git2r_0.32.0        
    ## [106] posterior_1.4.1      farver_2.1.1         digest_0.6.31       
    ## [109] shiny_1.7.4          storr_1.2.5          egg_0.4.5           
    ## [112] car_3.1-2            later_1.3.0          modEvA_3.9.3        
    ## [115] colorspace_2.1-0     rvest_1.0.3          fs_1.6.1            
    ## [118] eha_2.10.3           splines_4.2.3        expm_0.999-7        
    ## [121] graphlayouts_1.0.0   renv_0.17.3          shinythemes_1.2.0   
    ## [124] flexmix_2.3-19       sessioninfo_1.2.2    xtable_1.8-4        
    ## [127] jsonlite_1.8.4       tidygraph_1.2.3      modeltools_0.2-23   
    ## [130] R6_2.5.1             profvis_0.3.7        pillar_1.9.0        
    ## [133] htmltools_0.5.5      mime_0.12            txtq_0.2.4          
    ## [136] glue_1.6.2           fastmap_1.1.1        DT_0.27             
    ## [139] codetools_0.2-19     pkgbuild_1.4.0       mvtnorm_1.1-3       
    ## [142] utf8_1.2.3           lattice_0.21-8       numDeriv_2016.8-1.1 
    ## [145] arrayhelpers_1.1-0   curl_5.0.0           shinyjs_2.1.0       
    ## [148] survival_3.5-5       munsell_0.5.0        reshape2_1.4.4      
    ## [151] gtable_0.3.3

</details>

\* The `drake` package has been superseded by the `targets` package.
This repository may be transferred to that workflow and/or accompanied
with a Docker container to ensure backward compatibility. Currently,
`renv` is used to ensure reproducibility for the immediate future.
