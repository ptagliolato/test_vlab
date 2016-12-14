a<-function(s1){
  return (c(s1,": this is almost a test!"))  
}
#

# The method returns the first row of the downloaded dataset
# In openCPU, when the request is invoked with POST method, the dataset can then be found in /ocpu/tmp/{key}/files/lw_{dataUID}
# The session {key} is given in X-ocpu-session http response header returned by openCPU
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
  
  # extract just the header (colnames)
  header <- read.table(req, nrows = 1, header = TRUE, sep =';', stringsAsFactors = FALSE)
  
  return(header)
}
