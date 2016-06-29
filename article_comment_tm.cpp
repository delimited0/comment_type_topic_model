// article comment
#include "math.h"
#include <Rcpp.h>
using namespace Rcpp;

/*
 * article_comment_tm
 * modeling articles and their comments
 * article_id - index of article in articles dtm for each comment, 1-indexed because it's coming 
 *              from R. article_id[j] = k -> the jth comment belongs to article k
 */

double log_likelihood(double alpha, double eta_a, double eta_c, NumericMatrix theta, 
                      NumericMatrix beta_a, NumericMatrix beta_c, 
                      NumericVector z_a, NumericVector z_c, 
                      NumericVector word_a, NumericVector word_c,
                      NumericVector doc_a, NumericVector doc_c) {
  int D = theta.nrow();
  int K = theta.ncol();
  double term1 = 0.0;
  for (int d = 0; d < D; d++) {
    term1 += sum(log(theta(d,_)));
  }
  term1 *= (alpha - 1.0);
  double term2 = 0.0;
  for (int n = 0; n < z_a.length(); n++) {
    term2 += log(theta(doc_a[n], z_a[n]));
    term2 += log(beta_a(word_a[n], z_a[n]));
  }
  double term3 = 0.0;
  for (int n = 0; n < z_c.length(); n++) {
    term3 += log(theta(doc_c[n], z_c[n]));
    term3 += log(beta_c(word_c[n], z_c[n]));
  }
  double term4 = 0.0;
  for (int k = 0; k < K; k++) {
    term4 += (eta_a - 1.0) * sum(log(beta_a(_, k)));
    term4 += (eta_c - 1.0) * sum(log(beta_c(_, k)));
  }
  
  return term1 + term2 + term3 + term4;
}
  
// [[Rcpp::export]]
List article_comment_tm(NumericMatrix articles, NumericMatrix comments, NumericVector article_id,
                        int K, double alpha, double eta_a, double eta_c, int iter) {
  int D = articles.nrow();
  int W = articles.ncol();
  int C = comments.nrow();
  NumericMatrix word_topic_count_a(W, K);
  NumericMatrix doc_topic_count_a(D, K);
  NumericVector topic_count_a(K);
  NumericMatrix word_topic_count_c(W, K);
  NumericMatrix doc_topic_count_c(D, K);
  NumericMatrix topic_count_c(K);
  for (int i = 0; i < article_id.length(); i++)
    article_id[i]--;
  
  int tot_words_a = 0;
  for (int d = 0; d < D; d++) {
    tot_words_a += sum(articles(d, _));
  }
  int tot_words_c = 0;
  for (int c = 0; c < C; c++) {
    tot_words_c += sum(comments(c, _));
  }
  
  // initialize article counts
  NumericVector word_assign_a = floor(runif(tot_words_a) * K);
  NumericVector word_id_a(tot_words_a);
  NumericVector doc_id_a(tot_words_a);
  int i = 0;
  for (int d = 0; d < D; d++) {
    for (int w = 0; w < W; w++) {
      int word_count = articles(d, w);
      while (word_count > 0) {
        doc_id_a[i] = d;
        word_id_a[i] = w;
        word_topic_count_a(w, word_assign_a[i])++;
        doc_topic_count_a(d, word_assign_a[i])++;
        word_count--;
        i++;
      }
    }
  }
  for (int k = 0; k < K; k++) {
    topic_count_a[k] = sum(word_topic_count_a(_, k));
  }
  
  // initialize comment counts
  NumericVector word_assign_c = floor(runif(tot_words_c) * K);
  NumericVector word_id_c(tot_words_c);
  NumericVector doc_id_c(tot_words_c); 
  i = 0;
  for (int c = 0; c < C; c++) {
    for (int w = 0; w < W; w++) {
      int word_count = comments(c, w);
      while (word_count > 0) {
        doc_id_c[i] = article_id[c];
        word_id_c[i] = w;
        word_topic_count_c(w, word_assign_c[i])++;
        doc_topic_count_c(doc_id_c[i], word_assign_c[i])++;
        word_count--;
        i++;
      }
    }
  }
  for (int k = 0; k < K; k++) {
    topic_count_c[k] = sum(word_topic_count_c(_, k));
  }
  
  for (int j = 0; j < iter; j++) {
    NumericVector us_a = runif(tot_words_a);
    NumericVector us_c = runif(tot_words_c);
    if ((j+1) % 100 == 0)
      Rcout << "Iteration: " << j << std::endl;
    
    // sample article word assignments
    for (int i = 0; i < tot_words_a; i++) {
      NumericVector Z(K);
      for (int k = 0; k < K; k++) {
        int subtract = 0;
        if (word_assign_a[i] == k)
          subtract = 1;
        Z[k] = (eta_a + word_topic_count_a(word_id_a[i], k) - subtract) * 
          (alpha + doc_topic_count_a(doc_id_a[i], k) + doc_topic_count_c(doc_id_a[i], k) - subtract) /
        (W * eta_a + topic_count_a[k] - subtract);
        if (k != 0)
          Z[k] = Z[k] + Z[k-1];
      }
      for (int k = 0; k < K; k++) {
        if (us_a[i] < (Z[k] / Z[K-1])) {
          word_topic_count_a(word_id_a[i], word_assign_a[i])--;
          doc_topic_count_a(doc_id_a[i], word_assign_a[i])--;
          topic_count_a[word_assign_a[i]]--;
          word_assign_a[i] = k;
          word_topic_count_a(word_id_a[i], k)++;
          doc_topic_count_a(doc_id_a[i], k)++;
          topic_count_a[k]++;
          break;
        }
      }
    }
    
    // sample comment word assignments
    for (int i = 0; i < tot_words_c; i++) {
      NumericVector Z(K);
      for (int k = 0; k < K; k++) {
        int subtract = 0;
        if (word_assign_c[i] == k)
          subtract = 1;
        Z[k] = (eta_c + word_topic_count_c(word_id_c[i], k) - subtract) * 
          (alpha + doc_topic_count_a(doc_id_c[i], k) + doc_topic_count_c(doc_id_c[i], k) - subtract) /
        (W * eta_c + topic_count_c[k] - subtract);
        if (k != 0)
          Z[k] = Z[k] + Z[k-1];
      }
      for (int k = 0; k < K; k++) {
        if (us_c[i] < (Z[k] / Z[K-1])) {
          word_topic_count_c(word_id_c[i], word_assign_c[i])--;
          doc_topic_count_c(doc_id_c[i], word_assign_c[i])--;
          topic_count_c[word_assign_c[i]]--;
          word_assign_c[i] = k;
          word_topic_count_c(word_id_c[i], k)++;
          doc_topic_count_c(doc_id_c[i], k)++;
          topic_count_c[k]++;
          break;
        }
      }
    }
  }
  
  NumericMatrix beta_a(W, K);
  NumericMatrix beta_c(W, K);
  NumericMatrix theta(D, K);
  for (int k = 0; k < K; k++) {
    for (int w = 0; w < W; w++) {
      beta_a(w, k) = (word_topic_count_a(w, k) + eta_a) / (topic_count_a[k] + W * eta_a); 
      beta_c(w, k) = (word_topic_count_c(w, k) + eta_c) / (topic_count_c[k] + W * eta_c);
    } 
    for (int d = 0; d < D; d++) {
      theta(d, k) = (doc_topic_count_a(d, k) + doc_topic_count_c(d, k) + alpha) / 
        (sum(doc_topic_count_a(d, _) + doc_topic_count_c(d, _)) + K * alpha);
    }
  }
  
  List result;
  result["beta_a"] = beta_a;
  result["beta_c"] = beta_c;
  result["theta"] = theta;
  result["z_a"] = word_assign_a;
  result["z_c"] = word_assign_c;
  result["doc_topic_count_a"] = doc_topic_count_a;
  result["doc_topic_count_c"] = doc_topic_count_c;
  result["word_topic_count_a"] = word_topic_count_a;
  result["word_topic_count_c"] = word_topic_count_c;
  result["doc_id_a"] = doc_id_a;
  result["doc_id_c"] = doc_id_c;
  result["log_lik"] = log_likelihood(alpha, eta_a, eta_c, theta, beta_a, beta_c,
                                       word_assign_a, word_assign_c, word_id_a, word_id_c,
                                       doc_id_a, doc_id_c);
  CharacterVector class_names(1); 
  class_names[0] = "article_comment_tm"; 
  result.attr("class") = class_names;
  
  return result;
}