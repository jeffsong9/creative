pb <- winProgressBar(title = "progress bar", min = 0, max = fin, width = 300)
review.data.w.pb=function(x){
  setWinProgressBar(pb, x, title=paste(round(x/fin*100, 0), "% done"))
  return(review.data(ea.url[x,1]))
}