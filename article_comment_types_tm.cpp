// article comment type

#include <RcppArmadillo.h>
// [[Rcpp::depends(RcppArmadillo)]]

using namespace Rcpp;

IntegerVector which_positive(NumericVector x) {
  // return indexes of vector elements > 0
  IntegerVector v = seq(0, x.size()-1);
  return v[x > 0];
}

// [[Rcpp::export]]
List article_comment_types_tm(NumericMatrix articles, NumericMatrix comments, NumericVector article_id,
                        int K, int S, double alpha, double eta_a, double eta_c, double gamma_c, int iter) {
  int D = articles.nrow();
  int W = articles.ncol();
  int C = comments.nrow();
  NumericMatrix word_topic_count_a(W, K);
  NumericMatrix doc_topic_count_a(D, K);
  NumericVector topic_count_a(K);
  arma::cube type_word_topic_count(W, K, S);
  type_word_topic_count.zeros();
  NumericMatrix doc_topic_count_c(D, K);
  NumericMatrix doc_type_count_c(D, S);
  NumericMatrix type_topic_count_c(K, S);
  NumericMatrix comm_topic_count(C, K);
  arma::cube comm_word_topic_count(W, K, C);
  comm_word_topic_count.zeros();
  
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
  NumericVector comm_id_c(tot_words_c);
  NumericVector type_assign_c = floor(runif(C) * S);
  i = 0;
  for (int c = 0; c < C; c++) {
    doc_type_count_c(article_id[c], type_assign_c[c])++;
    for (int w = 0; w < W; w++) {
      int word_count = comments(c, w);
      while (word_count > 0) {
        doc_id_c[i] = article_id[c];
        comm_id_c[i] = c;
        word_id_c[i] = w;
        type_word_topic_count(w, word_assign_c[i], type_assign_c[c])++;
        comm_word_topic_count(w, word_assign_c[i], c)++;
        doc_topic_count_c(doc_id_c[i], word_assign_c[i])++;
        type_topic_count_c(word_assign_c[i], type_assign_c[c])++;
        comm_topic_count(c, word_assign_c[i])++;
        word_count--;
        i++;
      }
    }
  }
  
  for (int j = 0; j < iter; j++) {
    if ((j+1) % 100 == 0)
      Rcout << "Iteration: " << j << std::endl;
    
    NumericVector us_a = runif(tot_words_a);
    NumericVector us_c = runif(tot_words_c);
    NumericVector us_s = runif(C);
      
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
      int type_assign = type_assign_c[comm_id_c[i]];
      for (int k = 0; k < K; k++) {
        int subtract = 0;
        if (word_assign_c[i] == k)
          subtract = 1;
        Z[k] = (eta_c + type_word_topic_count(word_id_c[i], k, type_assign) - subtract) * 
          (alpha + doc_topic_count_a(doc_id_c[i], k) + doc_topic_count_c(doc_id_c[i], k) - subtract) /
        (W * eta_c + type_topic_count_c(k, type_assign) - subtract);
        if (k != 0)
          Z[k] = Z[k] + Z[k-1];
      }
      for (int k = 0; k < K; k++) {
        if (us_c[i] < (Z[k] / Z[K-1])) {
          type_word_topic_count(word_id_c[i], word_assign_c[i], type_assign)--;
          comm_word_topic_count(word_id_c[i], word_assign_c[i], comm_id_c[i])--;
          doc_topic_count_c(doc_id_c[i], word_assign_c[i])--;
          type_topic_count_c(word_assign_c[i], type_assign)--;
          comm_topic_count(comm_id_c[i], word_assign_c[i])--;
          word_assign_c[i] = k;
          type_word_topic_count(word_id_c[i], word_assign_c[i], type_assign)++;
          comm_word_topic_count(word_id_c[i], word_assign_c[i], comm_id_c[i])++;
          doc_topic_count_c(doc_id_c[i], k)++;
          type_topic_count_c(word_assign_c[i], type_assign)++;
          comm_topic_count(comm_id_c[i], word_assign_c[i])++;
          break;
        }
      }
    }
    
    // sample commenter type
    for (int i = 0; i < C; i++) {
      NumericVector Z(S);
      for (int s = 0; s < S; s++) {
        int subtract = 0;
        double term1 = 1.0;
        if (type_assign_c[i] == s) {
          subtract = 1;
          for (int k = 0; k < K; k++) {
            NumericMatrix word_topic_count_s = 
              as<NumericMatrix>(wrap(type_word_topic_count.slice(s)));
            NumericVector all_words_count = word_topic_count_s(_, k);
            NumericMatrix word_topic_count_c = 
              as<NumericMatrix>(wrap(comm_word_topic_count.slice(i)));
            NumericVector comm_words_count = word_topic_count_c(_, k);
            IntegerVector w_c_idx = which_positive(comm_words_count);
            NumericVector all_words_matter = all_words_count[w_c_idx];
            NumericVector comm_words_matter = comm_words_count[w_c_idx];
            for (int w = 0; w < comm_words_matter.size(); w++) {
              double g = 0.0;
              while (g < comm_words_matter[w]) {
                term1 *= (eta_c + all_words_matter[w] - comm_words_matter[w] + g);
                term1 /= (W * eta_c + type_topic_count_c(k, s) - comm_topic_count(i, k) + g);
                g++;    
              }
            }
          }
        }
        Z[s] = term1 * (gamma_c + doc_type_count_c(doc_id_c[i], s) - subtract);
        if (s != 0)
          Z[s] = Z[s] + Z[s-1];
      }

      for (int s = 0; s < S; s++) {
        if (us_s[s] < (Z[s] / Z[S-1])) {
          // you have to change a lot of things with a new assignment!
          for (int k = 0; k < K; k++) {
            for (int w = 0; w < W; w++) {
              type_word_topic_count(w, k, type_assign_c[i]) -= comm_word_topic_count(w, k, i);
            }
            type_topic_count_c(k, type_assign_c[i]) -= comm_topic_count(i, k);
          }
          doc_type_count_c(article_id[i], type_assign_c[i])--;
          type_assign_c[i] = s;
          for (int k = 0; k < K; k++) {
            for (int w = 0; w < W; w++) {
              type_word_topic_count(w, k, type_assign_c[i]) += comm_word_topic_count(w, k, i);
            }
            type_topic_count_c(k, type_assign_c[i]) += comm_topic_count(i, k);
          }
          doc_type_count_c(article_id[i], type_assign_c[i])++;
          break;
        }
      }
    }
  }
  
  NumericMatrix beta_a(W, K);
  List beta_cs(S);
  for (int s = 0; s < S; s++) {
    NumericMatrix beta_c(W, K);
    beta_cs[s] = beta_c;
  }
  NumericMatrix theta(D, K);
  NumericMatrix pi(D, S);
  
  for (int k = 0; k < K; k++) {
    for (int w = 0; w < W; w++) {
      beta_a(w, k) = (word_topic_count_a(w, k) + eta_a) / (topic_count_a[k] + W * eta_a); 
    } 
    for (int d = 0; d < D; d++) {
      theta(d, k) = (doc_topic_count_a(d, k) + doc_topic_count_c(d, k) + alpha) / 
      (sum(doc_topic_count_a(d, _) + doc_topic_count_c(d, _)) + K * alpha);
    }
  }
  
  for (int s = 0; s < S; s++) {
    NumericMatrix beta_c = as<NumericMatrix>(beta_cs[s]);
    for (int k = 0; k < K; k++) {
      for (int w = 0; w < W; w++) {
        beta_c(w, k) = (type_word_topic_count(w, k, s) + eta_c) / 
          (type_topic_count_c(k, s) + W * eta_c);
      }
    }
    for (int d = 0; d < D; d++) {
      pi(d, s) = (doc_type_count_c(d, s) + gamma_c) / 
        (sum(doc_type_count_c(d, _)) + S * gamma_c);
    }
  }
  
  List result;
  result["beta_a"] = beta_a;
  result["beta_cs"] = beta_cs;
  result["theta"] = theta;
  result["pi"] = pi;
  result["z_a"] = word_assign_a;
  result["z_c"] = word_assign_c;
  result["doc_topic_count_a"] = doc_topic_count_a;
  result["doc_topic_count_c"] = doc_topic_count_c;
  result["word_topic_count_a"] = word_topic_count_a;
  result["type_word_topic_count"] = type_word_topic_count;
  result["comm_word_topic_count"] = comm_word_topic_count;
  result["comm_topic_count"] = comm_topic_count;
  result["doc_type_count"] = doc_type_count_c;
  result["type_topic_count_c"] = type_topic_count_c;
  result["doc_id_a"] = doc_id_a;
  result["doc_id_c"] = doc_id_c;
  result["comm_id_c"] = comm_id_c;
  result["types"] = type_assign_c;
  CharacterVector class_names(1); 
  class_names[0] = "article_comment_types_tm"; 
  result.attr("class") = class_names;
  
  return result;
}