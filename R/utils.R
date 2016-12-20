#' Title
#'
#' @param neededFields 
#' @param df 
#'
#' @return
checkFields<-function(neededFields,df){
  #Check needed columns
  #neededFields<-c("eunishabitatstypename","alien","locality","eunisspeciesgroups","scientificname")
  
  missingFields<-!neededFields %in% colnames(df)
  if(sum(missingFields)){
    stop(paste('Input data need the following fields:',paste(neededFields[missingFields], collapse=", " )))
  }
}