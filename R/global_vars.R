#' @docType package
#' @name rrza
#' @importFrom magrittr %$%
#' @importFrom magrittr %>%
#' @importFrom grDevices dev.off
#' @importFrom grDevices png
#' @importFrom stats filter
NULL

## quiets concerns of R CMD check re: the .'s that appear in pipelines
if(getRversion() >= "2.15.1")  utils::globalVariables(c(".","BEAKER_VOLUME",
                                                        "EST_NUM_PERM3",
                                                        "GEAR_NAME",
                                                        "LAT", "LON",
                                                        "RZA_TAXA"))
