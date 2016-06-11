source("prince_data_funcs.R")
articles <- readArticles("articles_1999.csv")
comments <- readComments("comments_1999.csv")

# remove empty articles and empty comments
# remove articles with no comments
# remove comments to articles with no text
article_docs <- transformCorpus(Corpus(VectorSource(articles$article)))
comment_docs <- transformCorpus(Corpus(VectorSource(comments$comment)))
all_docs <- c(article_docs, comment_docs)
prince_dtm <- DocumentTermMatrix(all_docs)
prince_dtm_count <- rowSums(as.matrix(prince_dtm))

all_id <- c(articles$id, comments$id)
all_type_id <- c(rep(1, length(article_docs)), rep(0, length(comment_docs)))
all_articles_id <- all_id[all_type_id==1]
all_comments_id <- all_id[all_type_id==0]
few_idx <- prince_dtm_count <= 0
empty_article_id <- all_id[few_idx & all_type_id==1]
with_comment_idx <- all_id %in% all_comments_id
with_article_idx <- !(all_id %in% empty_article_id)

valid_idx <- !few_idx & with_comment_idx & with_article_idx

type_id <- all_type_id[valid_idx]
prince_id <- all_id[valid_idx]
articles_id <- prince_id[type_id==1]
comments_id <- prince_id[type_id==0]
prince_docs <- transformCorpus(all_docs[valid_idx])
prince_dtm <- DocumentTermMatrix(prince_docs)

article_idx <- sapply(comments_id, FUN=function(x){which(articles_id==x)})

articles_dtm <- prince_dtm[type_id==1,]
comments_dtm <- prince_dtm[type_id==0,]


# run directly on rcpp code ----

prince_actm <- article_comment_tm(articles = as.matrix(articles_dtm), comments = as.matrix(comments_dtm), K = 5, 
                                  article_id = article_idx, alpha = .1, eta_a = .01, eta_c = .01,
                                  iter = 1000)

# debug with smaller corpus ----
# one article and its comments
prince_actm <- article_comment_tm(articles = as.matrix(articles_dtm[6,]), comments = as.matrix(comments_dtm[article_idx==6,]), K = 5, 
                                  article_id = rep(1, nrow(comments_dtm[article_idx==6,])), alpha = .1, eta_a = .01, eta_c = .01,
                                  iter = 100)

# another article and its comments
prince_actm <- article_comment_tm(articles = as.matrix(articles_dtm[33,]), comments = as.matrix(comments_dtm[article_idx==33,]), K = 5, 
                                  article_id = rep(1, nrow(comments_dtm[article_idx==33,])), alpha = .1, eta_a = .01, eta_c = .01,
                                  iter = 1000)

# two articles and their comments
prince_actm <- article_comment_tm(articles = as.matrix(articles_dtm[c(6, 33),]), comments = as.matrix(comments_dtm[article_idx %in% c(6, 33),]), K = 5, 
                                  article_id = c(rep(1, sum(article_idx==6)), rep(2, sum(article_idx==33))), alpha = .1, eta_a = .01, eta_c = .01,
                                  iter = 10)

# run through wrapper ----

prince_actm <- actm(articles_dtm = articles_dtm, comments_dtm = comments_dtm, 
                    article_id = article_idx, K = 15)
prince_topics <- get_topics(prince_actm, 20)

# article comment type test run ----

prince_acttm <- article_comment_types_tm(articles = as.matrix(articles_dtm), comments = as.matrix(comments_dtm), 
                                         article_id = article_idx, K = 5, S = 2, alpha = .1, eta_a = .01, 
                                         eta_c = .01, gamma_c = .01, iter = 200) 

prince_acttm <- article_comment_types_tm(articles = as.matrix(articles_dtm[6,]), comments = as.matrix(comments_dtm[article_idx==6,]), 
                                  article_id = rep(1, nrow(comments_dtm[article_idx==6,])), K = 5, S = 2, alpha = .1, eta_a = .01, eta_c = .01,
                                  gamma_c = .01, iter = 1)
prince_acttm$vocab <- colnames(articles_dtm) # attach vocab manually
prince_topics <- get_topics(prince_acttm, 20)

# one article, one comment
prince_acttm <- article_comment_types_tm(articles = as.matrix(articles_dtm[1,]), comments = as.matrix(comments_dtm[article_idx==1,]), 
                                         article_id = rep(1, nrow(comments_dtm[article_idx==1,])), K = 5, S = 2, alpha = .1, eta_a = .01, eta_c = .01,
                                         gamma_c = .01, iter = 1)

# one article, two comments
prince_acttm <- article_comment_types_tm(articles = as.matrix(articles_dtm[5,]), comments = as.matrix(comments_dtm[article_idx==5,]), 
                                         article_id = rep(1, nrow(comments_dtm[article_idx==5,])), K = 5, S = 2, alpha = .1, eta_a = .01, eta_c = .01,
                                         gamma_c = .01, iter = 1)

prince_acttm <- article_comment_types_tm(articles = as.matrix(articles_dtm[c(6, 33),]), comments = as.matrix(comments_dtm[article_idx %in% c(6, 33),]),  
                                  article_id = c(rep(1, sum(article_idx==6)), rep(2, sum(article_idx==33))), K = 5, S = 2, alpha = .1, eta_a = .01, 
                                  eta_c = .01, gamma_c = .01, iter = 10)
