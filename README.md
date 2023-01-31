# BIOS-22-0126

This repository is to aid in reproducing the analyses in Callaghan et al. "Experimental evidence that behavioral nudges in citizen science projects can improve biodiversity data".

Because the data were generated using human ethics protocols, the 'raw data' from FrogID can not be reproduced here in full. However, I have added a summarized dataset that is generated from the raw data, and R scripts that use these summarized data to reproduce the analyses.

For any questions, please reach out to Corey Callaghan at callaghan.corey.t@gmail.com.

This code last worked on the following specifications:

R version 4.1.2 (2021-11-01)
Platform: x86_64-w64-mingw32/x64 (64-bit)
Running under: Windows Server x64 (build 17763)

Matrix products: default

locale:
[1] LC_COLLATE=English_United States.1252  LC_CTYPE=English_United States.1252    LC_MONETARY=English_United States.1252
[4] LC_NUMERIC=C                           LC_TIME=English_United States.1252    

attached base packages:
[1] stats     graphics  grDevices utils     datasets  methods   base     

other attached packages:
 [1] lme4_1.1-27.1   Matrix_1.4-0    forcats_0.5.1   stringr_1.4.0   purrr_0.3.4     readr_2.1.2     tidyr_1.1.4     tibble_3.1.6    tidyverse_1.3.1
[10] ggplot2_3.3.5   dplyr_1.0.7    

loaded via a namespace (and not attached):
 [1] Rcpp_1.0.8        lubridate_1.8.0   lattice_0.20-45   assertthat_0.2.1  digest_0.6.29     utf8_1.2.2        R6_2.5.1          cellranger_1.1.0 
 [9] backports_1.4.1   reprex_2.0.1      httr_1.4.2        pillar_1.7.0      rlang_1.0.0       readxl_1.3.1      rstudioapi_0.13   minqa_1.2.4      
[17] nloptr_2.0.0      textshaping_0.3.6 labeling_0.4.2    splines_4.1.2     munsell_0.5.0     broom_0.7.12      compiler_4.1.2    modelr_0.1.8     
[25] janitor_2.1.0     systemfonts_1.0.3 pkgconfig_2.0.3   tidyselect_1.1.1  fansi_1.0.2       viridisLite_0.4.0 crayon_1.4.2      tzdb_0.2.0       
[33] dbplyr_2.1.1      withr_2.4.3       MASS_7.3-55       grid_4.1.2        nlme_3.1-155      jsonlite_1.7.3    gtable_0.3.0      lifecycle_1.0.1  
[41] DBI_1.1.2         magrittr_2.0.2    scales_1.1.1      cli_3.1.1         stringi_1.7.6     farver_2.1.0      fs_1.5.2          snakecase_0.11.0 
[49] xml2_1.3.3        ellipsis_0.3.2    ragg_1.2.2        generics_0.1.2    vctrs_0.3.8       boot_1.3-28       tools_4.1.2       glue_1.6.1       
[57] hms_1.1.1         colorspace_2.0-2  rvest_1.0.2       haven_2.4.3 
