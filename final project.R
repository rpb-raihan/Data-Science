# Install required packages
if (!require("pacman")) install.packages("pacman")
pacman::p_load(rvest, dplyr, tm, topicmodels, ggplot2, tidytext,textstem)


# Define the target URL
target_url <- "https://apnews.com/sports"

# Scrape webpage content using rvest
webpage <- read_html(target_url)

# Extract and print headlines using CSS selectors
headlines <- webpage %>%
  html_elements(css = ".PagePromo-media+ .PagePromo-content .PagePromoContentIcons-text , .PageListRightRailA-content .PagePromoContentIcons-text") %>%
  html_text() %>%
  unique()

print(headlines)

# Create a volatile corpus from the headlines
corpus <- VCorpus(VectorSource(headlines))

custom_stopwords <- c(stopwords("en"), "news", "new")
corpus <- tm_map(corpus, content_transformer(tolower))
corpus <- tm_map(corpus, removePunctuation)
corpus <- tm_map(corpus, removeNumbers)
corpus <- tm_map(corpus, removeWords, custom_stopwords)
corpus <- tm_map(corpus, content_transformer(lemmatize_strings))  # Lemmatization
corpus <- tm_map(corpus, stripWhitespace)

# Create a Document-Term Matrix (DTM)
dtm <- DocumentTermMatrix(corpus)
row_totals <- apply(dtm, 1, sum)  # Calculate the sum of each row
dtm <- dtm[row_totals > 0, ]      # Keep rows with non-zero entries

# Check if the DTM is still valid
if (nrow(dtm) == 0) {
  stop("The Document-Term Matrix is empty. Please check your preprocessing steps.")
}
# Inspect the first 5 rows and columns of the DTM
inspect(dtm[1:5, 1:5])

# Set the number of topics for LDA
num_topics <- 9

# Fit the LDA model to the DTM
lda_model <- LDA(dtm, k = num_topics, control = list(seed = 1234))

# Extract term probabilities for each topic using tidy
lda_tidy <- tidy(lda_model, matrix = "beta")

# Ensure no duplicate terms per topic
lda_tidy <- lda_tidy %>%
  distinct(topic, term, .keep_all = TRUE)

# Calculate variance of beta values for each topic
beta_variance <- lda_tidy %>%
  group_by(topic) %>%
  summarize(variance = var(beta))

print(beta_variance)

# Adjust variance threshold
variance_threshold <- 0.00001
high_variance_topics <- beta_variance %>%
  filter(variance > variance_threshold) %>%
  pull(topic)

# Filter terms for high-variance topics and select the top 10 terms per topic
top_terms <- lda_tidy %>%
  filter(topic %in% high_variance_topics) %>%
  group_by(topic) %>%
  slice_max(order_by = beta, n = 10, with_ties = FALSE) %>%  # Ensure exactly 10 terms
  ungroup() %>%
  arrange(topic, -beta)

print(top_terms)

# Visualize filtered topics using ggplot2
if (nrow(top_terms) > 0) {
  ggplot(top_terms, aes(x = reorder(term, beta), y = beta, fill = as.factor(topic))) +
    geom_col(show.legend = FALSE) +
    facet_wrap(~ topic, scales = "free_y", nrow = 2) +  # Adjust layout for better readability
    coord_flip() +
    labs(title = "Top Terms in High-Variance Topics", x = "Terms", y = "Beta Values") +
    theme_minimal(base_size = 12) +  # Use a clean theme for clarity
    theme(axis.text.y = element_text(size = 10))  # Adjust font size for readability
} else {
  message("No terms to plot. Adjust variance threshold or check data.")
}