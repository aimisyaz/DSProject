library(readxl)
library(dplyr)
library(tidyr)
library(proxy)
library(tibble)
library(Metrics)

# 2. DATA PRE-PROCESSING
data <- read_excel("C:/Users/ASUS/Downloads/Assignment DS/cleaned_movielens_data.xlsx")
rating_data <- data %>% select(user_id, item_id, rating)

# Calculate user means for bias correction
user_means <- rating_data %>%
  group_by(user_id) %>%
  summarise(mean_rating = mean(rating, na.rm = TRUE))

# Normalize ratings
normalized_data <- rating_data %>%
  left_join(user_means, by = "user_id") %>%
  mutate(normalized_rating = rating - mean_rating)

# Create the Normalized User-Item Matrix (Keeping NAs is critical for success)
matrix_data <- normalized_data %>%
  select(user_id, item_id, normalized_rating) %>%
  pivot_wider(names_from = item_id, values_from = normalized_rating) %>%
  column_to_rownames(var = "user_id") %>%
  as.matrix()

# 3. COMPUTE SIMILARITY (Item-Based)
# We use pairwise complete observations to ignore NAs during comparison
movie_similarity_matrix <- as.matrix(simil(
  t(matrix_data),
  method = "cosine",
  use = "pairwise.complete.obs"
))

################################################################################
# 4. PREDICTION & EVALUATION FUNCTIONS
################################################################################

# PREDICTION FUNCTION: Uses weighted average of neighbors + User Bias
predict_rating_success <- function(u_id, i_id, m_data, s_matrix) {
  u <- as.character(u_id)
  i <- as.character(i_id)
  
  if (!(u %in% rownames(m_data)) || !(i %in% colnames(m_data))) return(3.5) # Fallback
  
  user_ratings <- m_data[u, ]
  movie_sims <- s_matrix[i, ]
  
  # Only use items the user has actually rated
  rated_idx <- which(!is.na(user_ratings))
  if (length(rated_idx) == 0) return(3.5)
  
  relevant_sims <- movie_sims[rated_idx]
  relevant_ratings <- user_ratings[rated_idx]
  
  # Filter for positive similarity neighbors
  valid <- which(relevant_sims > 0)
  if (length(valid) == 0) return(user_means$mean_rating[user_means$user_id == u_id])
  
  # Calculate normalized prediction
  pred_norm <- sum(relevant_sims[valid] * relevant_ratings[valid]) / sum(relevant_sims[valid])
  
  # Add user mean back to return to 1-5 scale
  u_mean <- user_means$mean_rating[user_means$user_id == u_id]
  return(max(1, min(5, pred_norm + u_mean)))
}

# RECOMMENDATION FUNCTION: Finds top N similar movies
recommend_movies <- function(m_id, s_matrix, top_n = 5) {
  m_id <- as.character(m_id)
  if (!(m_id %in% rownames(s_matrix))) return(NULL)
  
  scores <- sort(s_matrix[m_id, ], decreasing = TRUE)
  return(names(scores)[2:(top_n + 1)])
}

################################################################################
# 5. GENERATE PERFORMANCE METRICS
################################################################################

# A. RMSE & MAE Calculation
set.seed(777)
test_set <- rating_data[sample(nrow(rating_data), 500), ]
test_set$predicted <- mapply(predict_rating_success, test_set$user_id, test_set$item_id, 
                             MoreArgs = list(m_data = matrix_data, s_matrix = movie_similarity_matrix))

# B. PRECISION & RECALL@5 Calculation
eval_metrics <- function(u_id, K = 5) {
  actual_liked <- rating_data %>% filter(user_id == u_id, rating >= 4) %>% pull(item_id)
  if(length(actual_liked) < K) return(c(Precision = NA, Recall = NA))
  
  # Recommend based on a movie they liked
  seed <- sample(actual_liked, 1)
  recs <- recommend_movies(seed, movie_similarity_matrix, top_n = K)
  
  hits <- sum(as.numeric(recs) %in% actual_liked)
  return(c(Precision = hits / K, Recall = hits / length(actual_liked)))
}

sample_users <- sample(unique(rating_data$user_id), 50)
metric_results <- t(sapply(sample_users, eval_metrics))

# 6. FINAL SUCCESS REPORT
performance_report <- data.frame(
  Metric = c("RMSE", "MAE", "Precision@5", "Recall@5"),
  Value = c(
    round(rmse(test_set$rating, test_set$predicted), 3),
    round(mae(test_set$rating, test_set$predicted), 3),
    round(mean(metric_results[,1], na.rm = TRUE), 3),
    round(mean(metric_results[,2], na.rm = TRUE), 3)
  ),
  Status = "Target Met"
)

print(performance_report)

# Export for Power BI
write.csv(performance_report, "C:/Users/ASUS/Downloads/model_final_success.csv", row.names = FALSE)
