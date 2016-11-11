rest.url=function(pg.num.url){
  doc=read_html(pg.num.url)
  rest.path=c(ea.url = '//div [@id="EATERY_SEARCH_RESULTS"]//h3 [@class="title"] //a [@class="property_title"]',ea.rest.name= '//div [@id="EATERY_SEARCH_RESULTS"]//h3 [@class="title"]')
  apply_rest.path=lapply(rest.path, function(x) html_nodes(doc,xpath=x))
  r_url.r_name=cbind.data.frame(rest.url=gsub(".*?href=\"(.*?)\".*", "https://www.tripadvisor.com\\1",apply_rest.path[[1]]), rest.name=apply_rest.path[[2]] %>% html_text() %>% gsub("\n+([^\n]+?)\n.*", "\\1", .), stringsAsFactors=F)
  return(r_url.r_name)
}
