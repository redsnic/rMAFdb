#' Create a full db from a maf file
#'
#' This procedures prepares the structures to load a MAF file into
#' a structured database. The default structure of the database is
#' based on the GDC standard.
#'
#' @param file_path path to MAF file
#' @param table.name name of the main db table
#' @param names names of the columns (even non exhaustive and in any order)
#' @param types types of the columns
#'
#' @return a list object (see code)
maf_db_loader <- function(file_path, table.name, names, types){
  # read in chunck
  cr <- chunk_reader(file_path)

  # manage header
  repeat{
    line <- cr$read(1)
    if (line != "" && !startsWith(line, "#")){
      header <- strsplit(line, "\t")[[1]] %>% tolower()
      break
    }
  }

  should.quote <- function(var){
    if(startsWith(var, "varchar")){
      1L # to be quoted
    }else if(startsWith(var, "table")){
      1L
    }else{
      2L # do not quote
    }
  }

  # default column types and management
  filepath <- ""
  try({filepath <- system.file("extdata", "GDC-MAF-Structure.csv", package = "rMAFdb")})
  if (filepath == ""){ # manage vignette build
    filepath <- find_root_file("extdata", "GDC-MAF-Structure.csv", criterion = has_file("DESCRIPTION"))
  }

  gdc.rules.table <- read_csv(filepath)

  gdc.rules <- gdc.rules.table %>%
    pull(Type) %>%
    map_int(~ ifelse(. == "varchar", 1L, ifelse(. == "float" || . == "integer", 2L, 3L)))

  gdc.names <- gdc.rules.table %>% pull(Column) %>% map_chr(~gsub("^[^-]*- ", "", .)) %>% tolower()

  gdc.rules.table <- tibble(names=gdc.names,
                            types=gdc.rules.table %>% pull(Type),
                            rules=gdc.rules)

  # manage column selection
  sel.df <- gdc.rules.table # defaults

  head.df <- tibble(names = header)

  paired.df <- left_join(
    head.df,
    sel.df,
    by=c("names")
  ) %>% mutate(rules = ifelse(is.na(rules), 1L, rules))

  if(!is.null(names) && !is.null(types)){
    override.df <- tibble(names=names, rules=map_int(types, ~should.quote(.)), types=types)

    paired.df <- paired.df %>%
      left_join(
        override.df,
        by=c("names")
      ) %>% mutate(rules = ifelse(is.na(rules.y), rules.x, rules.y)) %>%
      mutate(types = ifelse(is.na(types.y), types.x, types.y)) %>%
      select(names, rules, types) %>%
      mutate(types=ifelse(is.na(types),"varchar",types))
  }

  print(paired.df)

  # 2 NO QUOTE
  # 1 QUOTE
  # 0 SKIP [not allowed, just quote]
  quote_array <- paired.df %>% pull(rules) %>% map_int(~ifelse(.==0L,1L,.))

  variant.line.number <- 0

  # ---
  list(
    header = header, # MAF header
    main.table.structure = paired.df, # dataframe of colnames, types and rules
    read = function(max_chunk = 10000){ # function to gradually load the data
      next_chunck <- cr$read(max_chunk)

      nrows <- length(next_chunck)
      if(nrows == 0){
        return(NULL)
      }
      # keep track of position
      starting_point <- variant.line.number
      nrows <- length(next_chunck)
      variant.line.number <<- variant.line.number + nrows
      # call C++ function
      maf_db_reader(table.name, next_chunck, header, quote_array, starting_point)
    },
    close = function(){ cr$close() } # colse connection
  )
}

