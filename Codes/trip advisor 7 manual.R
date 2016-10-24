rm(list=ls())
library(rvest)
library(stringr)
library(XML)
library(RCurl)
library(gsubfn)
setwd("C:/Users/Taikgun Song/Desktop/IV Project 2/TripAdvisor v7")

### Get page numbers (pg_num_url.r) ###
initial.url="https://www.tripadvisor.com/Restaurants-g60982-Honolulu_Oahu_Hawaii.html"
doc=read_html(initial.url)
pg.num.path=c(pg.url = '//div [@class="pageNumbers"]')
check=lapply(pg.num.path, function(x) html_nodes(doc,xpath=x))
# Final page number
f.page=check$pg.url %>% html_text() %>% gsub(".*\n(.*)\n", "\\1", .)
#check[[1]] %>% gsub(".*?href=\"(.*?)\".*", "https://www.tripadvisor.com\\1", .)
#check[[1]] %>% gsub(".*?href=\"(.*?)\".*", "https://www.tripadvisor.com\\1", .)
# Note that there are 30 restaurants in each page.
# Thus, the first review of the last page starts as 58*30=1740.
# And the address of each page is has the form
# https://www.tripadvisor.com/Restaurants-g60982-oa1740-Honolulu_Oahu_Hawaii.html#EATERY_OVERVIEW_BOX
# Where the #of the restaurant matches with "oa#+1" in the url address.
# If there is no pattern in the next url, then the following code might help, however, it is going to be slow since it must go through a loop
# Get next.url
# library(gsubfn)
# n.url=unlist(strapply(as.character(check$pg.url[[1]]), pattern="href=\"(.*?)\"", simplify=FALSE)) %>% paste0("https://www.tripadvisor.com", .)
# Update the initial.url: "initial.url=n.url[5]" until all the url are retrieved. 
complete.initial.url=rbind.data.frame(rest.url=initial.url, data.frame(rest.url=sapply(c(1:(as.numeric(f.page)-1))*30, function(x) gsub("SUB", x,"https://www.tripadvisor.com/Restaurants-g60982-oaSUB-Honolulu_Oahu_Hawaii.html#EATERY_LIST_CONTENTS")), stringsAsFactors = F), stringsAsFactors = F)
row.names(complete.initial.url)=NULL
View(complete.initial.url)


#### Step 0: Scrap each restaurant URL Given page numbers (ind_rest_url.r) ####
initial.url="https://www.tripadvisor.com/Restaurants-g60982-Honolulu_Oahu_Hawaii.html"
doc=read_html(initial.url)
rest.path=c(ea.url = '//div [@id="EATERY_SEARCH_RESULTS"]//h3 [@class="title"] //a [@class="property_title"]',ea.rest.title= '//div [@id="EATERY_SEARCH_RESULTS"]//h3 [@class="title"]')
apply_rest.path=lapply(rest.path, function(x) html_nodes(doc,xpath=x))
r_url.r_title=cbind.data.frame(rest.url=gsub(".*?href=\"(.*?)\".*", "https://www.tripadvisor.com\\1",apply_rest.path[[1]]), rest.title=apply_rest.path[[2]] %>% html_text() %>% gsub("\n+([^\n]+?)\n.*", "\\1", .))
View(r_url.r_title)


#### Step 1: Given a restaurant URL, scrap URL of the lastest 10 restaurants (ind_url_title_review.r) ####
# ger url and use html nodes for a given restaurant
ea.rest.url="https://www.tripadvisor.com/Restaurant_Review-g60982-d1028030-Reviews-Lu_Lu_s_Waikiki-Honolulu_Oahu_Hawaii.html"
doc=read_html(ea.rest.url)

### 1) Each URL Path and its Title for a given restaurant
u.n.t.path=c(ea.url = '//div [@id="REVIEWS"]//div [@class="innerBubble"]',ea.title= '//div [@id="REVIEWS"]//div [@class="innerBubble"]//span [@class="noQuotes"]')
apply_u.n.t.path=lapply(u.n.t.path, function(x) html_nodes(doc,xpath=x))
## (1) Url and Title into data frame
u.n.t=cbind.data.frame(ea.url=gsub(".*?<a href=\"(.*?)\".*", "https://www.tripadvisor.com\\1",apply_u.n.t.path[[1]]), ea.title=apply_u.n.t.path[[2]] %>% html_text(), stringsAsFactors=F)


### 2) Review Path using "apply" Loop for one given review url from 1)
r.path <- c(ea.review = '(//div [@id="REVIEWS"]//div [@class="innerBubble"]//p)[1]')
apply_r.path=lapply(r.path, function(x) sapply(u.n.t[,1], function(y) html_nodes(y %>% read_html(),xpath=x) %>% html_text())) %>% data.frame(stringsAsFactors=F)
u.t.r.out.put=cbind.data.frame(u.n.t, apply_r.path)
## (2) Combine everything: The following output consists individual review URL, Title, and Review entry for a single restaurant
row.names(u.t.r.out.put)=c(1:nrow(u.n.t))
# View(u.t.r.out.put)

