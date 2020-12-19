#' @title package name
#' @description Retrieve package name
#' @param path path to package
#' @export
package_name <- function(path=getwd()){
  desc_path <- normalizePath(file.path(path,'DESCRIPTION'),mustWork = TRUE)
  read.dcf(desc_path)[,'Package'][[1]]
}

current_hash <- function(path = '.', trim = FALSE){
  
  this_path <- getwd()
  on.exit(setwd(this_path))
  
  hash <- NULL
  
  if(is_git()){
    
    setwd(path)
    
    hash <- system('git rev-parse HEAD',intern = TRUE)
    
    if(trim){
      
      hash <- substring(hash,1,7) 
      
    }
    
  }
  
  hash
}

