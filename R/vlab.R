#' test function
#'
#' @param s1 
a<-function(s1){
  return (c(s1,": this is almost a test"))  
}
#

#' Download a dataset from LifeWatch Service Centre dataportal
#' 
#' \code{readLifeWatchDataportal} The method returns the first row of the downloaded dataset
#' In openCPU, when the request is invoked with POST method, the dataset can then be found in /ocpu/tmp/{key}/files/lw_{dataUID}
#' The session {key} is given in "X-ocpu-session" http response header returned by openCPU. 
#' Alternatively, the complete path to session folder ({baseurl}/ocpu/tmp/{key}/) is given in "Location" header
#'
#' @param dataUID unique identifier of the dataset, retrievable through dataportal API or search interface
#' @param user username (LW dataportal account)
#' @param pass password (LW dataportal account)
#' @import curl
#'
#' @export
#' @return downloaded dataset
readLifeWatchDataportal<-function(dataUID,user,pass){
  require(curl)
  h <- new_handle()
  handle_setopt(h, copypostfields = "moo=moomooo");
  handle_setheaders(h,
                    "Content-Type" = "text/moo",
                    "Cache-Control" = "no-cache",
                    "username" = user,
                    "password"= pass
  )
  url<-sprintf("http://www.servicecentrelifewatch.eu/lifewatch-portlet/services/dataset/%s/download",dataUID)
  #tmp<-tempfile(tmpdir=getwd())
  
  # cf. https://github.com/jeroenooms/opencpu/issues/175 "Writing to a unique temporary directory": in openCPU use the getwd dir.
  req<-curl_download(url = url,sprintf("%s/lw_%s",getwd(),dataUID),handle=h)
  
  ## extract just the first row with colnames as an example
  # header <- read.table(req, nrows = 1, header = TRUE, sep =';', stringsAsFactors = FALSE)
  # read and return file
  file<-read.csv(req,sep=";")
    #read.table(req, header = TRUE, sep =';', stringsAsFactors = FALSE)
  return(file)
}

#' The function calculates alien and native species richness in a 
#'   
#'    @param aDatasetWith_eunishabitatstypename_alien_locality_eunisspeciesgroup_scientificname dataset containing
#'    the following columns-ranges (colnames are compliant with lifewatch dataportal harmonization):
#'    "eunishabitatstypename": (string) one of the eunishabitat codes like "C1.1" etc. 
#'      (See: http://eunis.eea.europa.eu/habitats-code-browser.jsp?expand=C#level_C)
#'      The current EEA linked data implementation seems not to dereference properly 
#'      eunishabitatstypenames URIs (of the form http://eunis.eea.europa.eu/eunishabitats/C1.1).
#'      The definition can be retrieved instead by the following service:
#'      http://semantic.eea.europa.eu/factsheet.action?uri=http://eunis.eea.europa.eu/eunishabitats/C1.1
#'      (passing the uri as the parameter value)
#'      or by the sparql query SELECT * WHERE{ <http://eunis.eea.europa.eu/eunishabitats/C1.1> ?p ?o. }
#'      running at the official eea endpoint:
#'      http://semantic.eea.europa.eu/sparql?selectedBookmarkName=&query=SELECT+*+WHERE%7B+%3Chttp%3A%2F%2Feunis.eea.europa.eu%2Feunishabitats%2FC1%3E+%3Fp+%3Fo.+%0D%0A+%7D&format=text%2Fhtml&nrOfHits=20&execute=Execute
#'      
#'    "alien": (boolean) an alien species with respect to the observing context 
#'    "locality": (string) a toponym where the observation occurred 
#'    "eunisspeciesgroups": (string) species group according to eunis species list (cf. http://eunis.eea.europa.eu/species.jsp)
#'    "scientificname": (string) scientific name of the observed species (to be clarified: which harmonization we should expect)
#' 
#'    
#' It results in a new dataset accounting for alien and native species richness, grouping the original data
#' by locality, eunis habitat level-1 code, eunis species group.
#'
#' NOTE: We want to investigate the site vulnerability, distributed across different Eunis habitat, to different taxonomic groups. 
#' So we need to aggregate the data at site level by taxon name and Habita Eunis name
#' 
#' @author Paolo Colangelo (original script author), Paolo Tagliolato (engineering)
#' @import reshape2 vegan
#' 
#' @export
#' @return dataframe accounting for alien and native species richness. 
#' containing the fields: "locality", "EunisL1", "eunisspeciesgroup", "native_richness", "alien_richness"
#' original data are grouped by locality, eunis habitat level-1 code, eunis species group.
alienNativeRichness<-function(aDatasetWith_eunishabitatstypename_alien_locality_eunisspeciesgroup_scientificname){
  ##################################
  ##### step 1: Matrix reshape #####
  ##################################

  # we need to reshape the original matrix downloaded from Lifewatch data repository (as csv file). 
  # We want to investigate the site vulnerability, distributed across different Eunis habitat, to different taxonomic groups. 
  # So we need to aggregate the data at site level by taxon name and Habitat Eunis name
  
  # first load raw data, in this example we'll used the freshwater dataset from Boggero et al. (2016)
  ds<-aDatasetWith_eunishabitatstypename_alien_locality_eunisspeciesgroup_scientificname
  #ds<-read.csv(file="../datasets/Dataset_Biodiversity_AlienSpecies_Freshwaters_2015.csv",sep=";")
  #Check needed columns
  neededFields<-c("eunishabitatstypename","alien","locality","eunisspeciesgroups","scientificname")
  
  missingFields<-!neededFields %in% colnames(ds)
  if(!prod(missingFields)){
    stop(paste('Input data need the following fields:',paste(neededFields[missingFields], collapse=", " )))
  }
  #check this. I received the output Input data need the following fields: eunishabitatstypename, alien, locality, eunisspeciesgroups, scientificname
  # for the following columns: [1] "catalognumber"            "eventdate"               
  # [3] "waterbody"                "locality"                
  # [5] "decimallatitude"          "decimallongitude"        
  # [7] "scientificname"           "namepublishedinyear"     
  # [9] "phylum"                   "class"                   
  # [11] "scientificnameauthorship" "family"                  
  # [13] "order"                    "genus"                   
  # [15] "alien"                    "eunishabitatstypecode"   
  # [17] "eunisspeciesgroups"       "providedscientificname"  
  # [19] "providedscientificname.1"
  # 
  
  #freshwater<-read.csv(file="Dataset_Biodiversity_AlienSpecies_Freshwaters_2015.csv",sep=";")
  
  # we are interested in investigating the Eunis habitat at level-1. We create a new variable.
  #levels(freshwater$eunishabitatstypename)
  ds$EunisL1 <- as.factor(substr(ds$eunishabitatstypename, start = 1, stop = 2)) 
  
  # Additional step.Eunis C3 and J5 were poorly represented. 
  #
  # For a definition of codes see http://semantic.eea.europa.eu/
  # (e.g. for C1) http://eunis.eea.europa.eu/eunishabitats/C1
  # See also the sparql query at the official eea endpoint:
  # SELECT * WHERE{ <http://eunis.eea.europa.eu/eunishabitats/C1> ?p ?o. }
  #  http://semantic.eea.europa.eu/sparql?selectedBookmarkName=&query=SELECT+*+WHERE%7B+%3Chttp%3A%2F%2Feunis.eea.europa.eu%2Feunishabitats%2FC1%3E+%3Fp+%3Fo.+%0D%0A+%7D&format=text%2Fhtml&nrOfHits=20&execute=Execute
  #
  # We can remove any record except those with Eunis c1 and c2 from the dataset. 
  #  summary(ds["EunisL1"])
  # (Take, remove or change this step acording to the dataset)
  # ds<-droplevels(subset(ds,EunisL1=="c1"| EunisL1=="c2"))
  
  # Subsetting
  # we subset the dataset in alien and native
  # alien will contain solely the rows with alien species...
  alien<-subset(ds,alien=="1")
  # ...native the others
  native<-subset(ds,alien!="1")
  
  # Reshaping and richness calculation 
  ## reshape alien and calculate the alien richness
  # (for a gentle introduction to melt see e.g. http://seananderson.ca/2013/10/19/reshape.html)
  #
  # alien.melt is the "long format" of the alien dataset.
  # colnames(alien.melt)
  # unique(alien.melt$variable)
  #
  # Note: without other args, the melt function treats each column with numeric values as a variable column. 
  #   All the others are considered id columns (and the function groups by them)
  alien.melt<-melt(alien)
  # dcast: long->wide format of the dataset.
  # The table will be with the following columns: 
  # locality, EunisL1 (i.e. eunis habitat code of first level), eunisspeciesgroups, [so many columns as the scientific names in the original scientificname column]
  alien_table<-dcast(alien.melt,locality+ EunisL1 + eunisspeciesgroups ~ scientificname)
  
  # maybe we can bypass the melting step (?)
  #alien_table2<-dcast(alien,locality+ EunisL1 + eunisspeciesgroups ~ scientificname)
  
  
  alien_table[is.na(alien_table)]<-0
  alien_richness<-specnumber(alien_table[sapply(alien_table, class)!="factor"])
  alien_richness<-cbind(alien_table[sapply(alien_table, class)=="factor"],alien_richness)
  ## reshape native and calculate the native richness 
  native.melt<-melt(native)
  native_table<-dcast(native.melt, locality+ EunisL1 + eunisspeciesgroups ~ scientificname)
  native_table[is.na(native_table)]<-0
  native_richness<-specnumber(native_table[sapply(native_table, class)!="factor"])
  native_richness<-cbind(native_table[sapply(native_table, class)=="factor"],native_richness)
  ##merge native+aliene
  new_table<-merge(native_richness,alien_richness,all.x=T,all.y=T)
  new_table[is.na(new_table)]<-0
  
  # remove unnecessary files 
  rm("alien_richness")
  rm("native_richness")
  
  return(new_table)
}

#' Title
#'
#' @param alienNativeRichnessData a dataset accounting for alien and native species richness 
#' with the following fields: 
#'    "locality": (string) a toponym where the observation occurred 
#'    "EunisL1": (string) eunis habitat level-1 code
#'    "eunisspeciesgroups": (string) species group according to eunis species list (cf. http://eunis.eea.europa.eu/species.jsp)
#'    "native_richness": (int) richness of native species in the locality for the eunisspeciesgroup
#'    "alien_richness": (int) richness of alien species in the locality for the eunisspeciesgroup
#' @note the parameter, for ocpu execution, should be the previous session id, in order for ocpu to retrieve the data computed by the preceding method
#' @import MuMIn lme4
#' 
#' @export
nn<-function(alienNativeRichnessData){
  #########################################################################################
  #####  Step 2: Generalized Linear Mixed Model (GLMM) fitting usign the lme4 package #####
  #########################################################################################
  new_table<-alienNativeRichnessData
  
  neededFields<-c("locality","EunisL1","eunisspeciesgroups","native_richness","alien_richness")
  missingFields<-!neededFields %in% colnames(ds)
  if(prod(missingFields)){
    stop(paste('Input data need the following fields:',paste(neededFields[missingFields], collapse=", " )))
  }
  
  # now we are ready to fit our model. We will use a generalized linear mixed models in order to take into account the structure of our new dataset. 
  #Taxonomic group and locality are not the focus of our investigation but largely influence our sampling. 
  #We will include these two factor in the random effect. 
  
  # First fit full model (a negative bionomial family is assumed for richness data)
  gfit_Eu_Ri <- glmer.nb(alien_richness ~native_richness+ EunisL1 +(1| eunisspeciesgroups)+(1|locality), data= new_table)
  
  # automatically calculate best model according to AIC
  #library(MuMIn)
  options(na.action = "na.fail")
  ms1<-dredge(gfit_Eu_Ri)
  ms1; # the full model has the highest AICc support 
  
  # fit the best model according to AIC
  mod.fit<-glmer.nb(as.formula(getCall(ms1,1)), data = new_table)
  
  # results
  summary(mod.fit) #table.
  visreg(mod.fit,trans=exp,nn=101,alpha=1,rug=F,partial=T) #graph
  
}
