<!-- README.md is generated from README.Rmd. Please edit that file -->
FastrCAT
--------

Oceanographic data generated from SeaBird FastCAT CTD’s have been
collected since 1995 under EcoFOCI. The file type which we can view and
collate the data from is a SeaBird proprietary file type called an .up
file. The .up file is a post process file generated from the SeaSoft
software and only includes the data from the upward cast of the device.
Since 1997 there have been various versions of a Perl script which will
append important station header information to the .up file. The Perl
script acts as an interface between the SeaSoft software and the
MasterCOD .db3 file to extract the header information based on the
BON/CALVET/TUCK number. Each station from a cruise has its’ own .up
file. This format isn’t in a traditional tabular format and is not
easily accessed by users. It has been determined that it would be useful
to build an R package for the FastCAT data that starts with a function
to aggregate each cruises .up files into a tabular format. The function
can then be used to generate tabular data and as a QAQC process for the
.up files. Then the tabular data be easily used to create plots and
calculate indices. This R package would be a final step in processing
.up files while out at sea. The steps would be: enter COD form data into
MasterCOD, run Perl script and then the R functions from the command
line.

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

Download the package as a zip.

``` r
pkg_location <- "path/to/directory/where/zipfile/is/located"

install.packages("FastrCAT.zip", lib = pkg_location, repos = NULL)
```

After you have installed it this way, you can use

``` r
library(FastrCAT)
```

to load the library .

### install directly from GitLab

After you have setup your ssh on GitLab and added you ssh key. Copy and
paste the code below to intall the `FastrCAT` package.

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

The package `devtools` is needed install the `FastrCAT` package.

``` r
library(devtools)
devtools::install_git("git@gitlab.afsc.noaa.gov:Nissa.Ferm/FastrCAT/FastrCAT.git",
                      build_vignettes = TRUE,
                      credentials = creds)
```
