a<-function(s1){
  return (c(s1,": this is a test"))  
}

tav.read.csv<-function(...){
  return (read.csv(...))
}

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
  tmp<-tempfile()
  req<-curl_download(url = url,tmp,handle=h)
  return(req)
}

