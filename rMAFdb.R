# rMAFdb
require(DBI)
require(dbplyr)

setClass("MAFdb",
         representation(
           con = "DBIConnection"
         ))


MAFdb <- function(con){
  new("MAFdb", con = con)
}

MAFdb.load <- function(con, path, names=NULL, types=NULL, limit=NULL, max_chunk=10000, reset=FALSE){
  table.name <- "MAF"
  
  # prepare data loader
  loader <- maf_db_loader(path, table.name, names, types)
  
  # --- PREPARE TABLES ---
  
  # MAIN TABLE CREATION QUERY
  
  creation.query <- paste(
    "CREATE TABLE ", table.name, "(\n",
    "DB_INDEX int,\n",
    paste(map2_chr(loader$header, 
                   loader$main.table.structure %>% pull(types) %>%
                     map_chr(~ifelse(is.na(.) | startsWith(.,"table"), "varchar", .)), ~paste(.x,.y)),
          collapse=",\n"),
    ");\n",
    sep=""
  )
  
  # drop all tables
  if(reset){ 
    for(table in dbListTables(con)){
      dbSendQuery(con, sql(paste("DROP TABLE", table)))
    }
    
    # create new tables
    # MAIN
    creation.query <- paste(
      "CREATE TABLE ", table.name, "(\n",
      "DB_INDEX int,\n", # db_index
      paste(map2_chr(loader$header, 
                     loader$main.table.structure %>% pull(types) %>%
                       map_chr(~ifelse(is.na(.) | startsWith(.,"table"), "varchar", .)), ~paste(.x,.y)),
            collapse=",\n"),
      ");\n",
      sep=""
    )
    dbSendQuery(con, sql(creation.query))
    
    column_names <- loader$main.table.structure %>% pull(names)
    is.special <- function(col){
      pos <- grep(paste("^",col,"$", sep=""), column_names)
      if(is.null(pos)){
        FALSE
      }else{
        loader$main.table.structure[pos[1], "rules"] == 3L
      }
    }
    
    # SIMPLE (index, value) LISTS
    
    if(is.special("dbsnp_val_status")){
      dbSendQuery(con, sql(
        "CREATE TABLE dbsnp_val_status (DB_INDEX int, dbsnp_val_status varchar);"
      ))
    }
    
    if(is.special("consequence")){
      dbSendQuery(con, sql(
        "CREATE TABLE consequence (DB_INDEX int, consequence varchar);"
      ))
    }
    
    if(is.special("existing_variation")){
      dbSendQuery(con, sql(
        "CREATE TABLE existing_variation (DB_INDEX int, existing_variation varchar);"
      ))
    }
    
    if(is.special("refseq")){
      dbSendQuery(con, sql(
        "CREATE TABLE refseq (DB_INDEX int, refseq varchar);"
      ))
    }
    
    if(is.special("pubmed")){
      dbSendQuery(con, sql(
        "CREATE TABLE pubmed (DB_INDEX int, pubmed varchar);"
      ))
    }
    
    if(is.special("filter")){
      dbSendQuery(con, sql(
        "CREATE TABLE filter (DB_INDEX int, filter varchar);"
      ))
    }
    
    if(is.special("gdc_filter")){
      dbSendQuery(con, sql(
        "CREATE TABLE gdc_filter (DB_INDEX int, gdc_filter varchar);"
      ))
    }
    
    # domains
    
    if(is.special("domains")){
      dbSendQuery(con, sql(
        "CREATE TABLE domains (DB_INDEX int, key varchar, value varchar);"
      ))
    }
    
    # vcf_info
    
    if(is.special("vcf_info")){
      dbSendQuery(con, sql(
        "CREATE TABLE vcf_info (DB_INDEX int, key varchar, value varchar);"
      ))
    }
    
    # vcf_tumor_gt and vcf_normal_gt
    
    if(is.special("vcf_tumor_gt")){
      dbSendQuery(con, sql(
        "CREATE TABLE vcf_tumor_gt (DB_INDEX int, key varchar, value varchar);"
      ))
    }
    
    if(is.special("vcf_normal_gt")){
      dbSendQuery(con, sql(
        "CREATE TABLE vcf_normal_gt (DB_INDEX int, key varchar, value varchar);"
      ))
    }
    
    # VEP TABLE
    
    if(is.special("all_effects")){
      dbSendQuery(con, sql(
        paste("CREATE TABLE all_effects", 
              "(DB_INDEX int, priority int,",
              "symbol varchar, consequence varchar,",
              "hgvsp_short varchar, transcript_id varchar,",
              "refseq varchar, hgvsc varchar,",
              "impact varchar, canonical varchar, strand int", 
              ");")
      ))
    }
    
    # vep_sift
    
    if(is.special("all_effects")){
      dbSendQuery(con, sql(
        paste("CREATE TABLE sift_vep", 
              "(DB_INDEX int, priority int,",
              "classification varchar, score float",
              ");")
      ))
    }
    
    # vep_polyphen
    
    if(is.special("all_effects")){
      dbSendQuery(con, sql(
        paste("CREATE TABLE polyphen_vep", 
              "(DB_INDEX int, priority int,",
              "classification varchar, score float",
              ");")
      ))
    }
  }
  
  # read data and send it do database
  repeat{
    query <- loader$read(min(limit,max_chunk)) # works even with NULL
    if(!is.null(limit)){
      limit <- limit - min(limit,max_chunk)
    }
    if(is.null(query)){
      break
    }

    dbSendQuery(con, sql(query))
    
    if(!is.null(limit) && limit<=0){
      break
    }
  }
  
  loader$close()  
  
  new("MAFdb", con = con)
}

setMethod("print", signature(x="MAFdb"), function(x){
  for(tab in dbListTables(x@con)){
    print(tab)
    for(field in dbListFields(x@con, tab)){
      print(paste(" |---", field))
    }
  }
})

setMethod("[", signature(x="MAFdb", i="ANY", j="missing"),  function(x,i,j,...){
  if(class(i) == "character" && length(i)==1){
    tbl(x@con, i)
  }else{
    stop(paste("ERROR: must access MAFdb using only a single identifier passed as a string, passed", i))
  }
})