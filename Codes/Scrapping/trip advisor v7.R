rm(list=ls())

setwd("C:/Users/Taikgun Song/Desktop/IV Project 2/TripAdvisor v7")
library(rvest)
library(stringr)
library(XML)
library(RCurl)
library(dplyr)


initial.url="https://www.tripadvisor.com/Restaurants-g60982-Honolulu_Oahu_Hawaii.html"
source("Code/pg_num_url.r")
all.url=get.pg.num(intial.url)
source("Code/ind_rest_url.r")
ea.url=lapply(all.url, function(x) rest.url(x)) %>% do.call("rbind",.)
source("Code/ind_url_title_review.r")

fin=nrow(ea.url)
source("Code/review_data_w_pb.r")
final.data=lapply(1:nrow(ea.url), function(x) review.data.w.pb(x) %>% cbind(rep(ea.url[x,2], nrow(.)), .)) %>% do.call("rbind",.)
colnames(final.data)[1]="rest.name"
write.csv(final.data, "Final.csv", row.names = F)
View(final.data)


# Raw code without the progress bar
# final.data=lapply(1:nrow(ea.url), function(x) review.data(ea.url[x,1]) %>% cbind(rep(ea.url[x,2], nrow(.)), .)) %>% do.call("rbind",.)
# colnames(final.data)[1]="rest.name"
# View(final.data)