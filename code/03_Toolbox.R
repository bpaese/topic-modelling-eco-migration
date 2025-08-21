##############################
# User-written functions for  
# Bruno Paese
# Last updated: July 19, 2025
##############################

# To get the top documents per topic. The arguments are "topic_num" (the topic number) and "n" (the number of documents we wish to display)
get_docs <- function(topic_num, n) {
  top.docs <- posterior(topic_model)$topics[, topic_num]
  sort(top.docs, decreasing = TRUE)[1:n]
}

# To get the posterior distribution of terms per topic. The arguments are "topic_num" (the topic number) and "n" (the number of terms we wish to display)
get_terms <- function(topic_num, n) {
  terms <- posterior(topic_model)$terms[topic_num, ]
  sort(terms, decreasing = T)[1:n]
}

# To calculate the Gini-Simpson Index
gini_simpson <- function(proportion) {
  1 - sum(proportion^2)
}

# To calculate the Shannon-Wiener Index
shannon <- function(proportion) {
  -sum(proportion*log(proportion))
}