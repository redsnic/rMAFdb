#' rMAFdb Package
#'
#' @description This package creates an easy to use interface to operate with huge MAF files.
#' MAF filed (possibly following GDC standard) can be parsed and loaded into a database
#' directly, data manipulation can be then done with dbplyr so that the related
#' code can be easly adapted from regular "on RAM" operations.
#' The use of databases allows the user to create indexes to esily speed up queries
#' even for smaller data sets.
#'
#' @author Nicolò Rossi \email{olocin.issor@gmail.com}
#' @docType package
#' @name rMAFdb
#'
#' @useDynLib rMAFdb
#'
#' @importFrom purrr map2_chr map_chr map_int map_chr
#' @importFrom dplyr `%>%` pull tibble inner_join select left_join mutate distinct tbl
#' @importFrom readr read_csv
#' @importFrom DBI dbListTables dbSendQuery dbListTables dbListFields
#' @importFrom dbplyr sql
#' @importFrom rprojroot find_root_file has_file
#' @import rly
#'
#'
NULL
