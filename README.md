# rrza
An R package to create abundance maps for Rapid Zooplankton Assessment Data. 

Disclaimer  

This repository is a scientific product and is not official communication of the Alaska Fisheries Science Center, the National Oceanic and Atmospheric Administration, or the United States Department of Commerce. All AFSC Resource Assesment and Conservation Engineering (AFSC-RACE) GitHub project code is provided on an ‘as is’ basis and the user assumes responsibility for its use. AFSC-RACE has relinquished control of the information and no longer has responsibility to protect the integrity, confidentiality, or availability of the information. Any claims against the Department of Commerce or Department of Commerce bureaus stemming from the use of this GitHub project will be governed by all applicable Federal law. Any reference to specific commercial products, processes, or services by service mark, trademark, manufacturer, or otherwise, does not constitute or imply their endorsement, recommendation or favoring by the Department of Commerce. The Department of Commerce seal and logo, or the seal and logo of a DOC bureau, shall not be used in any manner to imply endorsement of any commercial product or activity by DOC or the United States Government.


Documentation
-------------


How to install the package from GitHub
--------------------------------------

``` r
install.packages(remotes)

remotes::install_github("Copepoda/rrza", build = TRUE, 
                        build_opts = c("--no-resave-data", "--no-manual"))
```

Load rrza
-------------

``` r
library(rrza)
```

