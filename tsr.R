kl <- function(p, q) {
  as.numeric(t(p) %*% (log(p) - log(q)))
}

cos_sim <- function(p, q) {
  as.numeric((t(p) %*% q) / (sqrt(sum(p^2)) * sqrt(sum(q^2))))
}

cos_dissim <- function(p , q) 1 - cos_sim(p, q)

cor_dist <- function(p, q) {
  if (sd(q) == 0)
    1
  else
    1 - cor(p, q)
}

uniformity <- function(word_dist, distance) {
  # word_dist - distribution of words for a given topic
  # distance - distance function, taking two distributions as arguments
  unif <- rep(1/length(word_dist), length(word_dist))
  distance(word_dist, unif)
}

vacuity <- function(word_dist, all_word_dist, theta, distance) {
  # word_dist - distribution of words for a given topic
  # all_word_dist distribution matrix (W x K)
  # theta - topic distribution
  # distance - distance function, taking two distributions as arguments
  p_k <- colSums(theta) / nrow(theta)
  overall_dist <- rowSums(all_word_dist %*% diag(p_k))
  
  distance(word_dist, overall_dist)
}

background <- function(theta_k, distance) {
  # theta_k - thetas for all docs given one topic
  # distance - distance function, taking two distributions as arguments
  unif <- rep(1 / length(theta_k), length(theta_k))
  distr <- theta_k / sum(theta_k)
  distance(distr, unif)
}

rescale <- function(scores) {
  # scores - scores for each topic under one distance function
  score_sum <- sum(scores)
  scores * ((score_sum - scores) / score_sum)
}

standardize <- function(scores) {
  (scores - min(scores)) / (max(scores) - min(scores))
}

score_calc <- function(betas, thetas, psi_score, psi_weight) {
  # betas - W x K
  # thetas - D x K
  # psi_score - 2 x 1
  # psi_weight - 3 x 1
  u <- (apply(betas, 2, function(x) {uniformity(x, kl)}) +
          apply(betas, 2, function(x) {uniformity(x, cos_dissim)}) +
          apply(betas, 2, function(x) {uniformity(x, cor_dist)})) / 3
  v <- (apply(betas, 2, function(x) {vacuity(x, betas, thetas, kl)}) +
            apply(betas, 2, function(x) {vacuity(x, betas, thetas, cos_dissim)}) +
            apply(betas, 2, function(x) {vacuity(x, betas, thetas, cor_dist)})) / 3  
  b <- (apply(thetas,  2, function(x) {background(x, kl)}) + 
            apply(thetas,  2, function(x) {background(x, cos_dissim)}) + 
            apply(thetas,  2, function(x) {background(x, cor_dist)})) / 3
  s_hat <- rescale(b) * (psi_score[1] * u + psi_score[2] * v)
  psi_hat <- colSums(psi_weight * 
                       matrix(data=c(standardize(u), 
                                     standardize(v), 
                                     standardize(b)),
                              nrow = 3))
  tsr_a <- psi_hat * s_hat
}

score <- function(x) UseMethod("score")
score.article_comment_tm <- function(actm, psi_score = c(.5, .5), 
                                     psi_weight = c(1/3, 1/3, 1/3)) {
  if (length(psi_score) != 2) error("psi_score must be of length 2")
  if (sum(psi_score) != 1) error("psi_score must sum to 1")
  if (length(psi_weight) != 3) error("psi_weight must be of length 3")
  if (sum(psi_weight) != 1) error("psi_weight must sum to 1")
  
  tsr_a <- score_calc(actm$beta_a, actm$theta, psi_score, psi_weight)
  tsr_c <- score_calc(actm$beta_c, actm$theta, psi_score, psi_weight)
  
  result <- list(tsr_a, tsr_c)
  names(result) <- c("tsr_a", "tsr_c")
  return(result)
}
