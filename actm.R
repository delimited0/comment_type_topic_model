actm <- function(articles_dtm, comments_dtm, article_id, K, alpha = 0.1, eta_a = 0.01, eta_c = 0.01,  
                burnin = 0, iter = 2000) {
#   article-comment topic model
#   articles_dtm and comments_dtm are DocumentTermMatrix (see tm package) that 
#     must have same vocabulary
#   article_id - index of article in articles dtm for each comment, 
#                article_id[j] = k -> the jth comment belongs to article k
  
  if (K %% 1 != 0) stop("K must be an integer")
  if (K <= 0) stop("K must be positive")
  if (class(articles_dtm)[1] != "DocumentTermMatrix") 
    stop("articles_dtm must be of class DocumentTermMatrix")
  if (class(comments_dtm)[1] != "DocumentTermMatrix") 
    stop("comments_dtm must be of class DocumentTermMatrix")
  
  result <- article_comment_tm(as.matrix(articles_dtm), as.matrix(comments_dtm), article_id,
                               K, alpha, eta_a, eta_c, iter)
  
  result$vocab <- colnames(articles_dtm)
  return(result)
}