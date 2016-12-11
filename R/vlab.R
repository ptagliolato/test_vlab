a<-function(s1){
  return (c(s1,": this is almost a test"))  
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
  tmp<-tempfile(tmpdir=getwd())
  req<-curl_download(url = url,tmp,handle=h)
  return(req)
}

