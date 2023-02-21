# Connect

Connect is a Wellcome Mental Health data prize funded project that uses latent growth curve modelling to study which and how social connections can affect the development of depression and anxiety worldwide.

Connect is a collaboration between the National Centre for Social Research, the University of Groningen, University College London, ETH Zurich, the University of Amsterdam and the Global Mental Health Peer Network.


You can read more at https://connectdata.org.uk

We are working on a live demo which will be available at https://app.connectdata.org.uk

## Workflow

The data was accessed from the Avon Longitudinal Study of Parents and Children (ALSPAC)

1. The data was converted from Stata format to R and variables were selected and partially cleaned (scripts/0_Convert Stata to R.R; scripts/01_data management.R; scripts/01.1Data Management_Edits following lived experience workshop.R, scripts/01.2_Extract control variables.R)  in the scripts folder

3. A secondary data cleaning was done using "scripts/02_data_clean.R". This produced the "alspac_long_v02.rds" which was used in subsequent scripts.

4. Descriptive analysis of the data were done using "scripts/03_data_describe.R"

5. A number of statistical models were run to model the growth in time. These can be seen at "scripts/04_analysis.R"

6. Model interpretation and visualization of the final model was produced using "scripts/05_desc_analysis.R"

You can find the helper functions created in the project under "scripts/functions" and the main outputs under "output". Two types of reports were produced. The first one explored the data ("reports/wellcome_explore_data_v5.Rmd") the others looked at the statistical models. Analaysis report 5 "reports/wellcome_analysis_v5.Rmd" presents the models based on data exploration while the models presented in "reports/wellcome_analysis_v6.Rmd" presents the theory driven model.

We are also developing a Shiny app to visualize the results. A draft of the code used can be seen in "wellcome_connect_app".



## Session information

```
sessionInfo()
```

```
R version 4.1.0 (2021-05-18)
Platform: x86_64-w64-mingw32/x64 (64-bit)
Running under: Windows 10 x64 (build 19042)

Matrix products: default

locale:
[1] LC_COLLATE=English_United Kingdom.1252 
[2] LC_CTYPE=English_United Kingdom.1252   
[3] LC_MONETARY=English_United Kingdom.1252
[4] LC_NUMERIC=C                           
[5] LC_TIME=English_United Kingdom.1252    

attached base packages:
[1] stats     graphics  grDevices utils     datasets  methods   base     

other attached packages:
 [1] texreg_1.38.6         broom_1.0.1           broom.mixed_0.2.9.4  
 [4] rstanarm_2.21.3       lme4_1.1-31           Matrix_1.3-3         
 [7] glue_1.6.2            rmarkdown_2.17        MplusAutomation_1.1.0
[10] blavaan_0.4-3         Rcpp_1.0.9            lavaan_0.6-12        
[13] viridis_0.6.2         viridisLite_0.4.1     shiny_1.7.3          
[16] ggthemes_4.2.4        gtsummary_1.6.2       gt_0.8.0             
[19] forcats_0.5.2         stringr_1.5.0         dplyr_1.0.10         
[22] purrr_0.3.5           readr_2.1.3           tidyr_1.2.1          
[25] tibble_3.1.8          ggplot2_3.3.6         tidyverse_1.3.2      
[28] knitr_1.40           

loaded via a namespace (and not attached):
  [1] readxl_1.4.1         backports_1.4.1      systemfonts_1.0.4   
  [4] igraph_1.3.5         plyr_1.8.8           splines_4.1.0       
  [7] crosstalk_1.2.0      listenv_0.8.0        rstantools_2.2.0    
 [10] inline_0.3.19        digest_0.6.29        htmltools_0.5.3     
 [13] rsconnect_0.8.29     fansi_1.0.3          magrittr_2.0.3      
 [16] checkmate_2.1.0      googlesheets4_1.0.1  tzdb_0.3.0          
 [19] globals_0.16.2       modelr_0.1.10        RcppParallel_5.1.5  
 [22] matrixStats_0.63.0   xts_0.12.2           sandwich_3.0-2      
 [25] prettyunits_1.1.1    colorspace_2.0-3     rvest_1.0.3         
 [28] textshaping_0.3.6    haven_2.5.1          xfun_0.33           
 [31] callr_3.7.2          crayon_1.5.2         jsonlite_1.8.4      
 [34] survival_3.2-11      zoo_1.8-11           gtable_0.3.1        
 [37] gargle_1.2.1         pkgbuild_1.4.0       rstan_2.21.7        
 [40] future.apply_1.10.0  scales_1.2.1         mvtnorm_1.1-3       
 [43] DBI_1.1.3            miniUI_0.1.1.1       xtable_1.8-4        
 [46] tmvnsim_1.0-2        DT_0.26              stats4_4.1.0        
 [49] StanHeaders_2.21.0-7 htmlwidgets_1.5.4    httr_1.4.4          
 [52] threejs_0.3.3        ellipsis_0.3.2       pkgconfig_2.0.3     
 [55] loo_2.5.1            farver_2.1.1         dbplyr_2.2.1        
 [58] utf8_1.2.2           reshape2_1.4.4       tidyselect_1.2.0    
 [61] labeling_0.4.2       rlang_1.0.6          later_1.3.0         
 [64] munsell_0.5.0        cellranger_1.1.0     tools_4.1.0         
 [67] cli_3.4.1            gsubfn_0.7           generics_0.1.3      
 [70] evaluate_0.17        fastmap_1.1.0        yaml_2.3.5          
 [73] ragg_1.2.4           processx_3.7.0       fs_1.5.2            
 [76] pander_0.6.5         future_1.29.0        nlme_3.1-152        
 [79] mime_0.12            xml2_1.3.3           nonnest2_0.5-5      
 [82] shinythemes_1.2.0    compiler_4.1.0       bayesplot_1.10.0    
 [85] rstudioapi_0.14      reprex_2.0.2         broom.helpers_1.10.0
 [88] pbivnorm_0.6.0       stringi_1.7.12       highr_0.10          
 [91] ps_1.7.1             lattice_0.20-44      markdown_1.4        
 [94] nloptr_2.0.3         shinyjs_2.1.0        vctrs_0.4.2         
 [97] CompQuadForm_1.4.3   furrr_0.3.1          pillar_1.8.1        
[100] lifecycle_1.0.3      data.table_1.14.2    httpuv_1.6.6        
[103] R6_2.5.1             promises_1.2.0.1     gridExtra_2.3       
[106] parallelly_1.32.1    codetools_0.2-18     gtools_3.9.4        
[109] colourpicker_1.2.0   boot_1.3-28          fastDummies_1.6.3   
[112] MASS_7.3-54          assertthat_0.2.1     proto_1.0.0         
[115] withr_2.5.0          shinystan_2.6.0      mnormt_2.1.0        
[118] parallel_4.1.0       hms_1.1.2            grid_4.1.0          
[121] coda_0.19-4          minqa_1.2.5          googledrive_2.0.0   
[124] lubridate_1.8.0      base64enc_0.1-3      dygraphs_1.1.1.6     
```
