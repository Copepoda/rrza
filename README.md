<!-- README.md is generated from README.Rmd. Please edit that file -->
rrza
----

This function takes an RZA .xlxs dataframe and will make either
individual plots of each RZA taxa group or two faceted maps based on 150
and 500 micron mesh. The maps plot the log abundance per cubic meter of
the rza taxa for each station. The map regions are standardized for each
of the major cruises/grids for EcoFOCI.

documentation
-------------

Full documentation of the process is available in vignettes

How to install the package
--------------------------

There are two options for installing the package.

1.  download/clone the project from GitLab and then install to R from
    source
2.  install with the `devtools`, `git2r`, and, `getPass` packages

### download/clone

Download the package as a tar.gz.

``` r

install.packages("download/path/to/rza.tar.gz", type = "source", repos = NULL)
```

After you have installed it this way, you can use

``` r
library(rrza)
```

to load the library .

### install directly from GitLab

After you have setup your ssh on GitLab and added you ssh key. Copy and
paste the code below to intall the `rrza` package.

The package `git2r` creates the ssh credential path to tell GitLab you
are allowed to access the repository. The package `getPass` creates the
pop up prompt for you to enter your LDAP password. Both of these are
needed to access the package.

``` r
library(git2r)
library(getPass)

creds <- git2r::cred_ssh_key(publickey = ssh_path("id_rsa.pub"),
                             privatekey = ssh_path("id_rsa"),
                             passphrase = character(0))
```

The package `devtools` is needed install the `rrza` package.

``` r
library(devtools)
devtools::install_git("git@gitlab.afsc.noaa.gov:Nissa.Ferm/rrza.git",
                      build_vignettes = TRUE,
                      credentials = creds)
```
