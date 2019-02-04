#' @docType package
#' @name rrza
#' @importFrom magrittr %$%
#' @importFrom magrittr %>%
#' @importFrom grDevices dev.off
#' @importFrom grDevices png
#' @importFrom stats filter
#' @importFrom stats na.omit
NULL

## quiets concerns of R CMD check re: the .'s that appear in pipelines
if(getRversion() >= "2.15.1")  utils::globalVariables(c(".","BEAKER_VOLUME",
                                                        "EST_NUM_PERM3",
                                                        "GEAR_NAME",
                                                        "LAT", "LON",
                                                        "RZA_TAXA",
                                                        "CRUISE", "DATE",
                                                        "DEPTH",
                                                        "DEPTH_BOTTOM",
                                                        "FOCI_GRID", "HAUL_ID",
                                                        "HAUL_NAME",
                                                        "JELLY_FOULING",
                                                        "NET",
                                                        "PHYTOPLANKTON_FOULING",
                                                        "SALINITY1",
                                                        "SAMPLE_COLOR",
                                                        "SORTER","STATION_HAUL",
                                                        "STATION_NAME",
                                                        "SUBSAMPLE_COUNT",
                                                        "SUBSAMPLE_VOLUME",
                                                        "TEMPERATURE1",
                                                        "TIME",
                                                        "TOTAL_COUNT",
                                                        "VOLUME_FILTERED"))
