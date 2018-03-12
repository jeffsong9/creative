ipak <- function(pkg){
  new.pkg <- pkg[!(pkg %in% installed.packages()[, "Package"])]
  if (length(new.pkg)) 
    install.packages(new.pkg, dependencies = TRUE)
  sapply(pkg, require, character.only = TRUE)
}
pkg=c("tidytext","tidyverse", "pander","topicmodels", "tm", "gridExtra", "pipeR", "ggtern", "tidyr", "png", "grid", "ggrepel")
ipak(pkg)
'%!in%' <- function(x,y)!('%in%'(x,y))
# if ("emojidata" %in% installed.packages()[, "Package"]==F){devtools::install_github("TekSong/emojidata")}

StopNStem=function(text){
  text %>>%
    VectorSource() %>>%
    Corpus()->  my.corpus
  my.corpus_copy = my.corpus
  my.corpus = tm_map(my.corpus, removeWords, c("the", stopwords("english"))) 
  my.corpus = tm_map(my.corpus, stemDocument, language="english")
  return(paste(strwrap(my.corpus[[1]]), sep="", collapse=""))
}

cleaning0=function(string){
  string %>%
    gsub("&amp;", "", .) %>%
    gsub("#", "", .) %>%
    gsub("&lt;", "<", .) %>%   # <3
    # gsub(" [:./[:alnum:]]+.com[/[:alnum:]]*", " ", .) %>%
    gsub("[^[:blank:]]+twitter.com/[^[:blank:]]+", "",.) %>%
    gsub(" http[:./[:alnum:]]+", " ", .) %>%
    gsub(" ur ", " your ", .) %>%
    gsub("^ur ", "your ", .) %>%
    gsub(" u ", " you ", .) %>%
    gsub("^u ", "you ", .) %>%
    gsub(" u$", " you", .) %>%
    gsub(" {2,}", " ", .) %>>%
    trimws() %>>%
    return()
}

clean_abb=function(string){
  string %>%
    gsub("aren\'t", "are not", .) %>%
    gsub("can\'t", "cannot", .) %>%
    gsub("couldn\'t", "could not", .) %>%
    gsub("didn\'t", "did not", .) %>%
    gsub("doesn\'t", "does not", .) %>%
    gsub("don\'t", "do not", .) %>%
    gsub("hadn\'t", "had not", .) %>%
    gsub("hasn\'t", "has not", .) %>%
    gsub("haven\'t", "have not", .) %>%
    gsub("he'd", "he would", .) %>% #"he had"
    gsub("he'll", "he will", .) %>% #, "he shall"
    gsub("he's", "he is", .) %>% #"he has", 
    gsub("i'd", "i would", .) %>% #"I had", 
    gsub("i'll", "i will", .) %>% #"I shall", 
    gsub("i'm", "i am", .) %>%
    gsub("i've", "i have", .) %>%
    gsub("isn\'t", "is not", .) %>%
    gsub("it's", "it is", .) %>% #"it has", 
    gsub("let's", "let us", .) %>%
    gsub("mustn\'t", "must not", .) %>%
    gsub("shan\'t", "shall not", .) %>%
    gsub("she'd", "she would", .) %>% #"she had", 
    gsub("she'll", "she will", .) %>% #"she shall", 
    gsub("she's", "she has", .) %>% #"she is", 
    gsub("shouldn\'t", "should not", .) %>%
    gsub("that's", "that is", .) %>% #"that has", 
    gsub("there's", "there is", .) %>% #"there has", 
    gsub("they'd", "they would", .) %>% #"they had", 
    gsub("they'll", "they will", .) %>% #"they shall", 
    gsub("they're", "they are", .) %>%
    gsub("they've", "they have", .) %>%
    gsub("we'd", "we would", .) %>% #"we had", 
    gsub("we're", "we are", .) %>%
    gsub("we've", "we have", .) %>%
    gsub("weren\'t", "were not", .) %>%
    gsub("what'll", "what will", .) %>% #"what shall", 
    gsub("what're", "what are", .) %>%
    gsub("what's", "what is", .) %>% #"what has", 
    gsub("what've", "what have", .) %>%
    gsub("where's", "where is", .) %>% #"where has", 
    gsub("who'd", "who would", .) %>% #"who had", 
    gsub("who'll", "who will", .) %>% #"who shall", 
    gsub("who're", "who are", .) %>%
    gsub("who's", "who is", .) %>% #"who has", 
    gsub("who've", "who have", .) %>%
    gsub("won\'t", "will not", .) %>%
    gsub("wouldn\'t", "would not", .) %>%
    gsub("you'd", "you would", .) %>% #"you had", 
    gsub("you'll", "you will", .) %>% #"you shall", 
    gsub("you're", "you are", .) %>%
    gsub("you've", "you have", .) %>%
    gsub("arent", "are not", .) %>%
    gsub(" cant ", " cannot ", .) %>%
    gsub("^cant ", "cannot ", .) %>%
    gsub(" cant$", " cannot", .) %>%
    gsub("couldnt", "could not", .) %>%
    gsub("didnt", "did not", .) %>%
    gsub("doesnt", "does not", .) %>%
    gsub(" dont ", " do not ", .) %>%
    gsub("^dont ", "do not ", .) %>%
    gsub(" dont$", " do not", .) %>%
    gsub("hadnt", "had not", .) %>%
    gsub("hasnt", "has not", .) %>%
    gsub("havent", "have not", .) %>%
    gsub(" isnt ", " is not ", .) %>%
    gsub("^isnt ", "is not ", .) %>%
    gsub(" isnt$", " is not", .) %>%
    gsub("mustnt", "must not", .) %>%
    gsub("shouldnt", "should not", .) %>%
    gsub("werent", "were not", .) %>%
    gsub(" wont ", " will not ", .) %>%
    gsub("^wont ", "will not ", .) %>%
    gsub(" wont$", " will not", .) %>>%
    trimws() %>>%
    return()
}

cleaning_u_plus=function(string){
  string %>%
    gsub("u\\+[[:alnum:]]+", "", .) %>>%
    return()
}
