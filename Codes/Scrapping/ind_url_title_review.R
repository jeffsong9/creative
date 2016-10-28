#### Step 1: Given a restaurant URL, scrap URL of the lastest 10 restaurants ###
review.data=function(ea.rest.url){
  doc=curl(ea.rest.url, handle = new_handle("useragent" = "Mozilla/5.0")) %>%  read_html()
  u.n.t.path=c(ea.url = '//div [@id="REVIEWS"]//div [@class="innerBubble"]',ea.title= '//div [@id="REVIEWS"]//div [@class="innerBubble"]//span [@class="noQuotes"]')
  apply_u.n.t.path=lapply(u.n.t.path, function(x) html_nodes(doc,xpath=x))
  u.n.t=cbind.data.frame(ea.url=gsub(".*?<a href=\"(.*?)\".*", "https://www.tripadvisor.com\\1",apply_u.n.t.path[[1]]), ea.title=apply_u.n.t.path[[2]] %>% html_text(), stringsAsFactors=F)
  r.path <- c(ea.review = '(//div [@id="REVIEWS"]//div [@class="innerBubble"]//p)[1]')
  apply_r.path=lapply(r.path, function(x) sapply(u.n.t[,1], function(y) html_nodes(y %>% read_html(),xpath=x) %>% html_text())) %>% data.frame(stringsAsFactors=F)
  u.t.r.out.put=cbind.data.frame(u.n.t, apply_r.path, stringsAsFactors=F)
  if (nrow(u.t.r.out.put)!=0){
    row.names(u.t.r.out.put)=c(1:nrow(u.n.t))
  } else {u.t.r.out.put=cbind.data.frame(ea.url="No review posted", ea.title="No review posted", ea.review ="No review posted", stringsAsFactors=F)}
  return(u.t.r.out.put)
  Sys.sleep(2)
}



#This is probably an issue with your call to read_html (or html in your case) not properly identifying itself to server it's trying to retrieve content from, which is the default behaviour. Using curl, add a user agent to the handle argument of read_html to have your scraper identify itself.