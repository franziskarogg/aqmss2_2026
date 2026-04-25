**Replication files for Assignment 10** 


###Project Description
This project examines the relationship between GDP per capita and corruption 
perceptions across countries using OLS regression. A level-level and a 
level-log model are estimated and compared

--- 

###Running the code and replicating the results

When running the code, the order should respect the taskflow from below.

--- 

**Data & Main analysis:**
-[`analysis`](.): loads the data, two regression models, scatter plots and modelsummary, as well as summary statistics

--- 

**Files corresponding to tables and figures in main text**
-Figure 1: `graphs/scatter.pdf`
-Table 1: `tables/regression_table.tex`
-Table 1: `tables/regression_table.png`

```
R version 4.2.3 (2023-03-15)
Platform: x86_64-apple-darwin17.0 (64-bit)
Running under: macOS Big Sur 11.7.11

Matrix products: default
LAPACK: /Library/Frameworks/R.framework/Versions/4.2/Resources/lib/libRlapack.dylib

locale:
[1] en_US.UTF-8/en_US.UTF-8/en_US.UTF-8/C/en_US.UTF-8/en_US.UTF-8

attached base packages:
[1] stats     graphics  grDevices utils     datasets  methods   base     

other attached packages:
[1] kableExtra_1.4.0       readstata13_0.11.0     marginaleffects_0.32.0
[4] broom_1.0.10           modelsummary_2.5.0     ggplot2_4.0.0         
[7] dplyr_1.1.4           

loaded via a namespace (and not attached):
 [1] sass_0.4.10         tidyr_1.3.1         jsonlite_2.0.0      viridisLite_0.4.2  
 [5] splines_4.2.3       bslib_0.9.0         datawizard_1.3.0    S7_0.2.0           
 [9] tables_0.9.33       bayestestR_0.17.0   globals_0.19.0      pillar_1.11.1      
[13] backports_1.5.0     lattice_0.20-45     glue_1.8.0          digest_0.6.37      
[17] RColorBrewer_1.1-3  promises_1.3.3      checkmate_2.3.4     sandwich_3.1-1     
[21] websocket_1.4.1     htmltools_0.5.8.1   Matrix_1.6-4        webshot2_0.1.2     
[25] pkgconfig_2.0.3     listenv_0.10.1      purrr_1.1.0         scales_1.4.0       
[29] processx_3.8.6      svglite_2.1.3       later_1.3.2         tibble_3.3.0       
[33] mgcv_1.8-42         generics_0.1.4      farver_2.1.2        cachem_1.1.0       
[37] withr_3.0.2         cli_3.6.5           magrittr_2.0.4      evaluate_1.0.5     
[41] ps_1.9.1            future_1.69.0       parallelly_1.46.1   nlme_3.1-162       
[45] xml2_1.4.0          textshaping_1.0.3   tools_4.2.3         data.table_1.17.8  
[49] lifecycle_1.0.4     stringr_1.5.2       tinytable_0.16.0    jquerylib_0.1.4    
[53] compiler_4.2.3      chromote_0.5.1      systemfonts_1.2.3   rlang_1.1.6        
[57] grid_4.2.3          dichromat_2.0-0.1   parameters_0.28.3   rstudioapi_0.17.1  
[61] labeling_0.4.3      rmarkdown_2.29      gtable_0.3.6        codetools_0.2-19   
[65] R6_2.6.1            zoo_1.8-15          knitr_1.50          performance_0.16.0 
[69] fastmap_1.2.0       future.apply_1.20.2 utf8_1.2.6          ragg_1.5.0         
[73] insight_1.4.6       stringi_1.8.7       parallel_4.2.3      Rcpp_1.1.1         
[77] vctrs_0.6.5         tidyselect_1.2.1    xfun_0.53           lmtest_0.9-40    

```