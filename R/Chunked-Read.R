#' Gradually read a file from disk
#'
#' This function reads a text file in batches (or chunks). 
#' It can be used for very large files that cannot fit in RAM.
#'
#' @param file_path path to large file
#'
#' @return a list-object containing the function `read` to 
#' read lines from the given file, and `close` to close the
#' connection to the file stream.
chunk_reader <- function(file_path){
  
  f <- file(file_path)
  open(f)
  counter <- 0;
   
  list(
    # returns the read lines and NULL if there is nothing to read.
    # This function keeps track of the current position in the file;
    # also displays a message whenever a chunk is read.
    read = function(max_chunk = 10000){
      lns <- readLines(f, n = max_chunk)
      counter <<- counter + length(lns)
      # progress message
      cat("\014") #TODO improve this visualization
      print(paste("read ", counter, " lines"))
      lns
    },
    close = function(){
      close(f)
    }
  )
}

#' create table 
#' 
#' create a table on a remote database using 
#' defined names and types
#'
#' @param con DBI connection to database
#' @param tablename name of the table of interest
#' @param names name of the columns of the table
#' @param types sql types of names (in the same order)
#' 
#' @return result of the creation query
create_table <- function(con, tablename, names, types){
  assertthat::are_equal(length(names),length(types))
  query <- paste(
    "CREATE TABLE ", tablename, "(\n",
    paste(map2_chr(names,types,~paste(.x,.y)), collapse=",\n"),
    ");\n",
    sep=""
  )
  dbSendQuery(con, sql(query))
}



