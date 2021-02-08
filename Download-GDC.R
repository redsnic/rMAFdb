require(TCGAbiolinks)

#' download clinical data 
#'
#' This function accesses the GDC data portal to query for
#' TCGA-BRCA clinical data summary files (BCR Biotabs)
#' 
#' @param file.path location where to save downloaded files
#' @param force TRUE to always download files
#' 
get.clinical.data <- function(file.path=NULL, force=F){
  query <- function(){
    GDCquery(
      project = "TCGA-BRCA",
      data.category = "Clinical", 
      data.type = "Clinical Supplement", 
      data.format = "BCR Biotab"
    )
  }
  get.GDC.data(query, file.path, force)
} 

#' download biospecimen data 
#'
#' This function accesses the GDC data portal to query for
#' TCGA-BRCA biospecimen data summary files (BCR Biotabs)
#' 
#' @param file.path location where to save downloaded files
#' @param force TRUE to always download files
#' 
get.biospecimen.data <- function(file.path, force=F){
  query <-function(){
    GDCquery(
      project = "TCGA-BRCA",
      data.category = "Biospecimen", 
      data.type = "Biospecimen Supplement", 
      data.format = "BCR Biotab"
    )}
  get.GDC.data(query, file.path, force)
}


#' download GDC data 
#'
#' This function accesses the GDC data portal and downloads
#' the files associated with the given query
#' 
#' @param query a function rerurnig a GDCquery(...) output
#' @param file.path location where to save downloaded files
#' @param force TRUE to always download files 
#' 
get.GDC.data <- function(query, file.path=NULL, force=FALSE){
  if(force==TRUE || is.null(file.path) || !file.exists(file.path) ){
    # Query GDC
    prepared.query <- query()
    # Download
    GDCdownload(prepared.query)
    # read downloaded data
    output.data <- GDCprepare(prepared.query)
    
    # save raw data for further use
    if(!is.null(file.path)){
      tryCatch(
        saveRDS(output.data, file=file.path),
        warning = function(c){ warning(paste("Cannot save file, do it yourself:", c)) }
      )
    }
    cat(paste("Downloaded files: \n", names(output.data), "\n"))
    output.data
  }else{
    # data already present, just load
    tryCatch(
      loaded.data <- readRDS(file.path),
      error = function(c){ stop(paste("Cannot load data file:", c)) }
    )
    cat(paste("Loaded files: \n", names(loaded.data), "\n"))
    loaded.data
  }
}
  
#' download GDC maf data 
#'
#' This function accesses the GDC data portal and downloads
#' the files associated with the given query
#' 
#' @param project project name, default "BRCA"
#' @param pipelines character vector with pipelines names of the MAFs to be downloaded
#'        NULL to download muse, mutect, somaticsniper and varscan2 outputs.
#' @param file.path location where to save downloaded files
#' @param force TRUE to always download files
#' 
get.maf.data <- function(project="BRCA", pipelines=NULL, file.path=NULL, force=FALSE){
  if(force==TRUE || is.null(file.path) || !file.exists(file.path) ){
    # Query GDC
    if(is.null(pipelines)){
      maf.data <- list()
      maf.data[["muse"]] <- GDCquery_Maf(project, pipelines = "muse")
      maf.data[["mutect"]] <- GDCquery_Maf(project, pipelines = "mutect")
      maf.data[["somaticsniper"]] <- GDCquery_Maf(project, pipelines = "somaticsniper")
      maf.data[["varscan2"]] <- GDCquery_Maf(project, pipelines = "varscan2")
    }else{
      maf.data <- list()
      for(i in 1:length(pipelines)){
        maf.data[[pipelines[i]]] <- GDCquery_Maf(project, pipelines = pipelines[i])
      }
    }
    
    output.data <- maf.data 
    
    # save raw data for further use
    if(!is.null(file.path)){
      tryCatch(
        saveRDS(output.data, file=file.path),
        warning = function(c){ warning(paste("Cannot save file, do it yourself:", c)) }
      )
    }
    cat(paste("Downloaded files: \n", names(output.data), "\n"))
    output.data
  }else{
    # data already present, just load
    tryCatch(
      loaded.data <- readRDS(file.path),
      error = function(c){ stop(paste("Cannot load data file:", c)) }
    )
    cat(paste("Loaded files: \n", names(loaded.data), "\n"))
    loaded.data
  }
}

























