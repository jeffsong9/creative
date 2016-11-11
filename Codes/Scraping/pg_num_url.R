get.pg.num=function(intial.url){
  doc1=read_html(initial.url)
  pg.num.path=c(pg.url = '//div [@class="pageNumbers"]')
  check=lapply(pg.num.path, function(x) html_nodes(doc1,xpath=x))
  f.page=check$pg.url %>% html_text() %>% gsub(".*\n(.*)\n", "\\1", .)
  complete.initial.url=rbind.data.frame(rest.url=initial.url, data.frame(rest.url=sapply(c(1:(as.numeric(f.page)-1))*30, function(x) gsub("SUB", x,"https://www.tripadvisor.com/Restaurants-g60982-oaSUB-Honolulu_Oahu_Hawaii.html#EATERY_LIST_CONTENTS")), stringsAsFactors = F), stringsAsFactors = F)
  row.names(complete.initial.url)=NULL
  return(complete.initial.url[[1]])
}
