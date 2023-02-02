# Connect

Connect is a Wellcome Mental Health data prize funded project that uses latent growth curve modelling to study which and how social connections can affect the development of depression and anxiety worldwide.

Connect is a collaboration between the National Centre for Social Research, the University of Groningen, University College London, ETH Zurich, the University of Amsterdam and the Global Mental Health Peer Network.


You can read more at https://connectdata.org.uk

We are working on a live demo which will be available at https://app.connectdata.org.uk

## Workflow

The data was accessed from the Avon Longitudinal Study of Parents and Children (ALSPAC)

1. The data was converted from Stata format to R 

2. An initial set of variables were selected and renamed 

3. A secondary data cleaning was done using "scripts/02_data_clean.R". This produced the "alspac_long_v01.rds" which was used in subsequent scripts.

4. Descriptive analysis of the data were done using "scripts/03_data_describe.R"



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
[1] LC_COLLATE=English_United Kingdom.1252  LC_CTYPE=English_United Kingdom.1252   
[3] LC_MONETARY=English_United Kingdom.1252 LC_NUMERIC=C                           
[5] LC_TIME=English_United Kingdom.1252    

attached base packages:
[1] stats     graphics  grDevices utils     datasets  methods   base     

other attached packages:
 [1] glue_1.6.2            rmarkdown_2.17        MplusAutomation_1.1.0
 [4] blavaan_0.4-3         Rcpp_1.0.9            lavaan_0.6-12        
 [7] viridis_0.6.2         viridisLite_0.4.1     shiny_1.7.3          
[10] ggthemes_4.2.4        gtsummary_1.6.2       gt_0.8.0             
[13] forcats_0.5.2         stringr_1.5.0         dplyr_1.0.10         
[16] purrr_0.3.5           readr_2.1.3           tidyr_1.2.1          
[19] tibble_3.1.8          ggplot2_3.3.6         tidyverse_1.3.2      
[22] knitr_1.40           

loaded via a namespace (and not attached):
  [1] googledrive_2.0.0    colorspace_2.0-3     ellipsis_0.3.2       fs_1.5.2            
  [5] rstudioapi_0.14      farver_2.1.1         listenv_0.8.0        rstan_2.21.7        
  [9] fansi_1.0.3          mvtnorm_1.1-3        lubridate_1.8.0      xml2_1.3.3          
 [13] codetools_0.2-18     mnormt_2.1.0         texreg_1.38.6        bayesplot_1.10.0    
 [17] jsonlite_1.8.3       broom_1.0.1          dbplyr_2.2.1         compiler_4.1.0      
 [21] httr_1.4.4           backports_1.4.1      assertthat_0.2.1     Matrix_1.3-3        
 [25] fastmap_1.1.0        gargle_1.2.1         cli_3.4.1            later_1.3.0         
 [29] htmltools_0.5.3      prettyunits_1.1.1    tools_4.1.0          coda_0.19-4         
 [33] gtable_0.3.1         cellranger_1.1.0     vctrs_0.4.2          broom.helpers_1.10.0
 [37] xfun_0.33            globals_0.16.2       ps_1.7.1             proto_1.0.0         
 [41] rvest_1.0.3          mime_0.12            CompQuadForm_1.4.3   lifecycle_1.0.3     
 [45] googlesheets4_1.0.1  future_1.29.0        zoo_1.8-11           scales_1.2.1        
 [49] ragg_1.2.4           hms_1.1.2            promises_1.2.0.1     parallel_4.1.0      
 [53] sandwich_3.0-2       inline_0.3.19        yaml_2.3.5           gridExtra_2.3       
 [57] pander_0.6.5         loo_2.5.1            StanHeaders_2.21.0-7 stringi_1.7.6       
 [61] highr_0.9            fastDummies_1.6.3    checkmate_2.1.0      boot_1.3-28         
 [65] pkgbuild_1.4.0       systemfonts_1.0.4    rlang_1.0.6          pkgconfig_2.0.3     
 [69] matrixStats_0.63.0   evaluate_0.17        lattice_0.20-44      labeling_0.4.2      
 [73] rstantools_2.2.0     processx_3.7.0       tidyselect_1.2.0     parallelly_1.32.1   
 [77] plyr_1.8.8           magrittr_2.0.3       R6_2.5.1             generics_0.1.3      
 [81] DBI_1.1.3            gsubfn_0.7           pillar_1.8.1         haven_2.5.1         
 [85] withr_2.5.0          future.apply_1.10.0  modelr_0.1.9         crayon_1.5.2        
 [89] nonnest2_0.5-5       utf8_1.2.2           tmvnsim_1.0-2        tzdb_0.3.0          
 [93] grid_4.1.0           readxl_1.4.1         data.table_1.14.2    pbivnorm_0.6.0      
 [97] callr_3.7.2          reprex_2.0.2         digest_0.6.29        xtable_1.8-4        
[101] httpuv_1.6.6         textshaping_0.3.6    RcppParallel_5.1.5   stats4_4.1.0        
[105] munsell_0.5.0       
```
