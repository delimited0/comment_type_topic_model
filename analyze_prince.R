source("prince_data_funcs.R")
articles <- readArticles("articles_1999.csv")
comments <- readComments("comments_1999.csv")

# remove empty articles and empty comments
article_docs <- transformCorpus(Corpus(VectorSource(articles$article)))
comment_docs <- transformCorpus(Corpus(VectorSource(comments$comment)))
all_docs <- c(article_docs, comment_docs)
all_id <- c(articles$id, comments$id)
all_type_id <- c(rep(1, length(article_docs)), rep(0, length(comment_docs)))
# not_empty_id <- all_id[unlist(Map(function(x) x$content != "", all_docs))]
# not_empty_docs <- all_docs[not_empty_id]
# not_empty_type_id <- all_type_id[unlist(Map(function(x) x$content != "", all_docs))]
prince_dtm <- DocumentTermMatrix(all_docs)
prince_dtm_count <- rowSums(as.matrix(prince_dtm))
few_idx <- prince_dtm_count <= 0
type_id <- all_type_id[!few_idx]
prince_id <- all_id[!few_idx]
with_comment_id <- prince_id[type_id==0]
prince_docs <- transformCorpus(all_docs[!few_idx])
prince_dtm <- DocumentTermMatrix(prince_docs)






