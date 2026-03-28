library(ggplot2)
library(dplyr)
library(lubridate)
library(tidyr) # Added for better data manipulation


# 1. DATA CLEANING (FIX DATE ISSUE)

data$release_date_clean <- parse_date_time(
  data$release_date, 
  orders = c("ymd", "dmy", "mdy")
)

if (any(is.na(data$release_date_clean))) {
  # This converts numeric Excel-style dates (e.g., 36525) to Date objects
  excel_dates <- as.Date(as.numeric(as.character(data$release_date)), origin = "1899-12-30")
  data$release_date_clean[is.na(data$release_date_clean)] <- excel_dates[is.na(data$release_date_clean)]
}

data$release_year <- year(data$release_date_clean)


# 2. AGE GROUP CREATION (ORDERED)

data$age_group <- cut(
  data$age,
  breaks = c(0, 18, 25, 35, 50, 100),
  labels = c("0-18", "19-25", "26-35", "36-50", "50+"),
  include.lowest = TRUE
)

data$age_group <- factor(data$age_group, 
                         levels = c("0-18","19-25","26-35","36-50","50+"))

# 3. SCATTER PLOT 

ggplot(data, aes(x = age, y = rating, color = gender)) +
  geom_jitter(width = 0.8, height = 0.2, alpha = 0.4) +
  geom_smooth(method = "lm", se = FALSE, color = "black") +
  labs(title = "Relationship Between Age and Rating",
       x = "Age",
       y = "Rating") +
  theme_minimal()

# 4. BOXPLOT (IMPROVED - CLEAR DISTRIBUTION)

ggplot(data[!is.na(data$age_group), ], aes(x = age_group, y = rating)) +
  geom_boxplot(fill = "lightblue", 
               outlier.color = "red", 
               outlier.alpha = 0.5) +
  labs(title = "Rating Distribution Across Age Groups",
       x = "Age Group",
       y = "Rating") +
  theme_minimal()

# 6. RATING BY GENDER (FIXED GEOM)
.
ggplot(data, aes(x = rating, fill = gender)) +
  geom_histogram(position = "dodge", binwidth = 0.5) +
  labs(title = "Rating Distribution by Gender",
       x = "Rating",
       y = "Count") +
  theme_minimal()

# 8. GENRE POPULARITY (FIXED DATA FRAME LOGIC)

genre_columns <- c("Action","Adventure","Animation","Childrens","Comedy",
                   "Crime","Documentary","Drama","Fantasy","Film_Noir",
                   "Horror","Musical","Mystery","Romance","Sci_Fi",
                   "Thriller","War","Western")

genre_sums <- colSums(sapply(data[genre_columns], as.numeric), na.rm = TRUE)

genre_df <- data.frame(
  Genre = names(genre_sums),
  Count = as.numeric(genre_sums)
) %>% arrange(desc(Count))

ggplot(genre_df[1:10, ], aes(x = reorder(Genre, Count), y = Count)) +
  geom_bar(stat = "identity", fill = "tomato") +
  coord_flip() +
  labs(title = "Top 10 Movie Genres",
       x = "Genre",
       y = "Count") +
  theme_minimal()

# 9. TOP 10 POPULAR MOVIES

popular_movies <- data %>%
  filter(!is.na(movie_title)) %>%
  group_by(movie_title) %>%
  summarise(total_ratings = n(), .groups = "drop") %>%
  slice_max(total_ratings, n = 10) # More idiomatic than arrange + slice

ggplot(popular_movies, aes(x = reorder(movie_title, total_ratings), 
                           y = total_ratings)) +
  geom_bar(stat = "identity", fill = "darkgreen") +
  coord_flip() +
  labs(title = "Top 10 Most Popular Movies",
       x = "Movie",
       y = "Number of Ratings") +
  theme_minimal()

