# tweeter exemplo
#install the necessary packages
install.packages("twitteR")
install.packages("wordcloud")
install.packages("tm")
library("twitteR")
library("wordcloud")
library("tm")
library("base64enc")
# a instalação abaixo solucionou erro de autenticação que estava ocorrendo
devtools::install_github("jrowen/twitteR", ref = "oauth_httr_1_0")
# autenticando
#my.consumer_key <- "mJk6ykOtgsYE1B8sNPK3563au"
#my.consumer_secret <- "vLHi1zZi4iuY0CYmK84Le77AayDLQvMMQ2GbOPMk40m50GxqIh"
#my.access_token <- "52512560-mBwbCZQApRATjsFcLf2n1av2VW0k5Eima1s7moHGm"
#my.access_secret  <- "RyELCUdByxBWJVD0eRfSBGhu0UgYrxxHod0TAgVdN0AVs"

#setup_twitter_oauth(my.consumer_key, my.consumer_secret, my.access_token, my.access_secret)
#setup_twitter_oauth(my.consumer_key, my.consumer_secret)

#to get your consumerKey and consumerSecret see the twitteR documentation for instructions
options(httr_oauth_cache=T)
consumer_key <- 'mJk6ykOtgsYE1B8sNPK3563au'
consumer_secret <- 'vLHi1zZi4iuY0CYmK84Le77AayDLQvMMQ2GbOPMk40m50GxqIh'
access_token <- '52512560-mBwbCZQApRATjsFcLf2n1av2VW0k5Eima1s7moHGm'
access_secret <- 'RyELCUdByxBWJVD0eRfSBGhu0UgYrxxHod0TAgVdN0AVs'
setup_twitter_oauth(consumer_key,
                    consumer_secret,
                    access_token,
                    access_secret)

#the cainfo parameter is necessary only on Windows
#r_stats <- searchTwitter("#Rstats", n=1500)
#should get 1500
#length(r_stats)
#[1] 1500

#save text
#r_stats_text <- sapply(r_stats, function(x) x$getText())

#create corpus
#r_stats_text_corpus <- Corpus(VectorSource(r_stats_text))

#clean up
#r_stats_text_corpus <- tm_map(r_stats_text_corpus, content_transformer(tolower)) 
#r_stats_text_corpus <- tm_map(r_stats_text_corpus, removePunctuation)
#r_stats_text_corpus <- tm_map(r_stats_text_corpus, function(x)removeWords(x,stopwords()))
#wordcloud(r_stats_text_corpus)

#alternative steps if you're running into problems 
#r_stats<- searchTwitter("#Rstats", n=1500)
r_stats<- searchTwitter("Asserth", 
                        #geocode='-23.5503247,-46.6341896,5mi',
                        n=1500)
#save text
r_stats_text <- sapply(r_stats, function(x) x$getText())
#create corpus
r_stats_text_corpus <- Corpus(VectorSource(r_stats_text))

#if you get the below error
#In mclapply(content(x), FUN, ...) :
#  all scheduled cores encountered errors in user code
#add mc.cores=1 into each function

#run this step if you get the error:
#(please break it!)' in 'utf8towcs'
r_stats_text_corpus <- tm_map(r_stats_text_corpus,
                              content_transformer(function(x) iconv(x, to='UTF-8-MAC', sub='byte')),
                              mc.cores=1
)
r_stats_text_corpus <- tm_map(r_stats_text_corpus, content_transformer(tolower), mc.cores=1)
r_stats_text_corpus <- tm_map(r_stats_text_corpus, removePunctuation, mc.cores=1)
r_stats_text_corpus <- tm_map(r_stats_text_corpus, function(x)removeWords(x,stopwords()), mc.cores=1)
wordcloud(r_stats_text_corpus)# fazer o teste acima com Asserth e Rh99
