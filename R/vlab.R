#' test function
#'
#' @param s1 
a<-function(s1){
  return (c(s1,": this is almost a test!"))  
}
#

#' Download a dataset from LifeWatch Service Centre dataportal
#' 
#' \code{tavReadDataportal} The method returns the first row of the downloaded dataset
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
#' @return first row of the downloaded dataset
tavReadDataportal<-function(dataUID,user,pass){
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
  
  # extract just the first row with colnames as an example
  header <- read.table(req, nrows = 1, header = TRUE, sep =';', stringsAsFactors = FALSE)
  
  return(header)
}
