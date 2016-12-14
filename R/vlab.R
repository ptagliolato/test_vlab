a<-function(s1){
  return (c(s1,": this is almost a test!"))  
}
#
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
  header <- read.table(req, nrows = 2, header = FALSE, sep =';', stringsAsFactors = FALSE)
  
  return(header)#test
}
