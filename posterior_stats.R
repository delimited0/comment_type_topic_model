Rcpp::sourceCpp('posterior_stats.cpp')

get_topics <- function(x, K) UseMethod("get_topics")

get_topics.article_comment_tm <- function(actm, K) {
  # get top K words by term score for each topic
  term_score_a <- term_score(actm$beta_a, K)
  term_score_c <- term_score(actm$beta_c, K)
  vocab_mat <- matrix(data=actm$vocab, nrow=length(actm$vocab), K)
  vocab_mat_a <- apply(term_score_a, 2, function(x) {
    vocab_mat[order(x)[1:K]]
  })
  vocab_mat_c <- apply(term_score_c, 2, function(x) {
    vocab_mat[order(x)[1:K]]
  })
  
  result <- list()
  result$article_topics <- vocab_mat_a
  result$comment_topics <- vocab_mat_c
  return(result)
}

get_topics.article_comment_types_tm <- function(acttm, K) {
  # get top K words by term score for each topic type
  term_score_a <- term_score(acttm$beta_a, K)
  term_score_cs <- lapply(acttm$beta_cs, function(x) {term_score(x, K)})
  vocab_mat <- matrix(data=acttm$vocab, nrow=length(acttm$vocab), K)
  vocab_mat_a <- apply(term_score_a, 2, function(x) {
    vocab_mat[order(x)[1:K]]
  })
  vocab_mat_cs <- lapply(term_score_cs, function(y) {
    apply(y, 2, function(x) {
      vocab_mat[order(x)[1:K]]
    })
  })
  
  result <- list()
  result$article_topics <- vocab_mat_a
  result$comment_topics <- vocab_mat_cs
  return(result)
}

