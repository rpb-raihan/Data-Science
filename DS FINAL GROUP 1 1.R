library(rvest)
library(openxlsx)
library(readxl)
library(writexl)
library(dplyr)
library(tm)
library(textclean)
library(textstem)
library(tokenizers)
library(stringr)
library(tidytext)
library(topicmodels)
library(slam)
library(ggplot2)
library(ggwordcloud)



urls <- c(
  "https://en.dailypakistan.com.pk/20-May-2025/petrol-prices-in-pakistan-to-climb-higher-after-budget-as-govt-plans-rs194-billion-levy-hike",
  "https://en.dailypakistan.com.pk/19-May-2025/gold-price-increases-by-rs4000-to-rs342500-per-tola-in-pakistan",
  "https://en.dailypakistan.com.pk/15-May-2025/pakistan-proposes-zero-tariff-trade-agreement-to-boost-us-economic-ties",
  "https://en.dailypakistan.com.pk/09-May-2025/imf-approves-1-1bn-final-tranche-for-pakistan",
  "https://en.dailypakistan.com.pk/20-May-2025/pakistan-shuts-down-over-3000-youtube-channels-for-anti-state-content",
  "https://en.dailypakistan.com.pk/15-May-2025/global-spotlight-on-chinese-jets-as-j-10c-downs-rafale-in-pakistan-india-stellar-combat",
  "https://en.dailypakistan.com.pk/05-May-2025/pakistani-students-shine-at-international-teknofest",
  "https://en.dailypakistan.com.pk/20-May-2025/pakistan-india-to-de-escalate-border-tensions-by-pulling-back-armed-forces-report",
  "https://en.dailypakistan.com.pk/20-May-2025/currency-rates-in-pakistan-today-us-dollar-euro-pound-dirham-riyal-to-pkr-in-open-market",
  "https://en.dailypakistan.com.pk/19-May-2025/imf-pushes-for-punjabs-e-abiyana-water-pricing-system",
  
  
  "https://www.hindustantimes.com/india-news/rain-karnataka-kerala-tamil-nadu-death-toll-imd-issues-red-alert-yellow-orange-alert-weather-bengaluru-rains-101747739238017.html",
  "https://www.hindustantimes.com/india-news/kannada-actor-ranya-rao-granted-bail-in-gold-smuggling-case-to-stay-in-jail-under-cofeposa-case-101747741474268.html",
  "https://www.hindustantimes.com/india-news/bjp-says-hafiz-saeed-likes-rahul-gandhi-slams-kharges-small-war-remark-on-operation-sindoor-101747744424617.html",
  "https://www.hindustantimes.com/india-news/navy-to-induct-unveil-name-of-recreated-ancient-stitched-ship-voyage-preparations-underway-101747747095028.html",
  "https://www.hindustantimes.com/india-news/2780-freshly-recruited-constables-pass-out-from-delhi-police-academy-101747746974425.html",
  "https://www.hindustantimes.com/india-news/bhopal-sexual-assault-case-religious-conversion-organised-crime-network-in-play-says-ncw-101747746783227.html",
  "https://www.hindustantimes.com/india-news/delhi-to-meerut-in-45-soon-most-of-namo-bharat-corridor-to-open-in-june-101747745822732.html",
  "https://www.hindustantimes.com/india-news/common-man-s-scientist-visionary-and-mentor-scientific-community-mourns-dr-narlikar-s-demise-101747745680081.html",
  "https://www.hindustantimes.com/india-news/indian-travellers-shift-away-from-turkey-azerbaijan-visa-applications-drop-sharply-report-101747739811582.html",
  "https://www.hindustantimes.com/india-news/prime-minister-modi-projects-india-model-at-world-health-assembly-in-geneva-101747742100350.html",
  
  
  "https://www.bnionline.net/en/news/junta-orders-schools-re-open-kyondoe-township-karen-state",
  "https://www.bnionline.net/en/news/three-consecutive-days-airstrikes-mindat-township",
  "https://www.bnionline.net/en/news/aa-frees-more-300-families-pows-buthidaung",
  "https://www.bnionline.net/en/news/muslims-sittwe-going-yangon-medical-care-face-hardships-due-holdup-granting-form-4-travel",
  "https://www.bnionline.net/en/news/400-flee-thailand-myanmar-rebels-strike-junta-base",
  "https://www.bnionline.net/en/news/security-increased-myitkyina-airport",
  "https://www.bnionline.net/en/news/gambling-and-drug-use-rise-muse-town",
  "https://www.bnionline.net/en/news/internet-blackout-adds-ramree-islanders-hardships",
  "https://www.bnionline.net/en/news/airstrikes-hit-lemyethna-township-villages-ayeyarwady-region",
  "https://www.bnionline.net/en/news/71-victims-uxo-year-arakan-state",
  
  
  "https://www.scmp.com/news/world/united-states-canada/article/3310778/trump-says-china-and-walmart-should-eat-tariffs-instead-raising-prices?module=top_story&pgtype=section",
  "https://www.scmp.com/news/china/science/article/3310696/chinese-gyroscope-could-make-navigation-more-stable-aircraft-ships-and-oil-rigs?module=top_story&pgtype=section",
  "https://www.scmp.com/native/business/companies/topics/supporting-businesses-they-grow/article/3307679/hktdc-helps-panopticai-take-its-ai-powered-health-monitoring-app-global-stage?module=top_story&pgtype=homepage",
  "https://www.scmp.com/news/china/science/article/3310963/ancestor-cell-animals-and-plants-lived-hydrogen-not-oxygen-chinese-study?module=top_story&pgtype=section",
  "https://www.scmp.com/news/china/diplomacy/article/3311019/china-pakistan-fms-expected-discuss-security-cooperation-indias-chagrin?module=top_story&pgtype=section",
  
  
  "https://news.cgtn.com/news/2025-05-19/China-s-CERES-1-commercial-rocket-launches-four-satellites-from-sea-1DvjyAS3WX6/p.html",
  "https://news.cgtn.com/news/2025-05-19/World-Health-Assembly-once-again-rejects-Taiwan-related-proposal-1Dvj6MneMmI/p.html",
  "https://news.cgtn.com/news/2025-05-20/President-Xi-stresses-firm-confidence-in-high-quality-development--1DwPtyUUQc8/p.html",
  "https://news.cgtn.com/news/2025-05-19/China-to-cut-gasoline-diesel-retail-prices-1DvmWCiawlq/p.html",
  "https://news.cgtn.com/news/2025-05-20/China-pushes-urban-renewal-to-drive-high-quality-growth-1DwEIDepJT2/p.html"
)

get_source <- function(url) {
  if (grepl("dailypakistan", url)) return("Daily Pakistan")
  else if (grepl("hindustantimes", url)) return("Hindustan Times")
  else if (grepl("bnionline", url)) return("BNI Online")
  else if (grepl("scmp", url)) return("SCMP")
  else if (grepl("cgtn", url)) return("CGTN")
  else return("Unknown")
}

extract_article_text <- function(url) {
  tryCatch({
    page <- read_html(url)
    text <- page %>% html_elements("p") %>% html_text(trim = TRUE)
    full_text <- paste(text, collapse = " ")
    if (nchar(full_text) < 50) return("Content not available or too short.")
    return(full_text)
  }, error = function(e) {
    return(paste("Error:", e$message))
  })
}

all_data <- lapply(urls, function(link) {
  cat("Processing:", link, "\n")
  source <- get_source(link)
  corpus <- extract_article_text(link)
  data.frame(link = link, source = source, corpus = corpus, stringsAsFactors = FALSE)
}) %>% bind_rows()

output_path <- "F:/8th Semester/scraped_articles.xlsx"
write.xlsx(all_data, file = output_path, rowNames = FALSE)
cat("All articles saved to Excel.\n")



preprocess_text <- function(text) {
  text <- tolower(text)
  text <- gsub("<.*?>", "", text)
  text <- replace_contraction(text)
  text <- gsub("http\\S+|www\\S+|https\\S+", "", text)
  text <- gsub("\\S+@\\S+", "", text)
  text <- gsub("@\\w+|#\\w+", "", text)
  text <- gsub("[^[:alnum:][:space:]']", " ", text)
  text <- gsub("\\d+", "", text)
  text <- gsub("\\s+", " ", str_trim(text))
  tokens <- unlist(tokenize_words(text))
  tokens <- tokens[!tokens %in% stopwords("en")]
  tokens <- lemmatize_words(tokens)
  tokens <- tokens[!grepl("[^\x01-\x7F]", tokens)]
  paste(tokens, collapse = " ")
}

input_path <- "F:/8th Semester/scraped_articles.xlsx"
input_path1 <- "F:/8th Semester/scraped_articles_cleaned.xlsx"
xl <- read_excel(input_path)
xl$corpus <- sapply(xl$corpus, preprocess_text)
write_xlsx(xl, input_path1)
cat("Preprocessing complete.\n")



source_country_map <- c(
  "Daily Pakistan" = "Pakistan",
  "Hindustan Times" = "India",
  "BNI Online" = "Myanmar",
  "SCMP" = "China",
  "CGTN" = "China"
)
xl$country <- source_country_map[xl$source]



data("stop_words")


df <- read_excel("F:/8th Semester/scraped_articles_cleaned.xlsx")


df$country <- source_country_map[df$source]

data("stop_words")


run_lda_plot <- function(data, group_label) {
  tidy_docs <- data %>%
    select(link, corpus) %>%
    unnest_tokens(word, corpus) %>%
    anti_join(stop_words, by = "word") %>%
    filter(nchar(word) > 2) %>%
    count(link, word, sort = TRUE) %>%
    ungroup()
  
  word_counts <- tidy_docs %>%
    group_by(word) %>%
    summarise(total = sum(n)) %>%
    filter(total >= 2)
  
  tidy_docs <- tidy_docs %>%
    semi_join(word_counts, by = "word")
  
  dtm <- tidy_docs %>%
    cast_dtm(link, word, n)
  
  if (nrow(dtm) < 6) return(NULL)
  
  lda_model <- LDA(dtm, k = 6, control = list(seed = 1234))
  
  top_terms <- tidy(lda_model, matrix = "beta") %>%
    group_by(topic) %>%
    top_n(10, beta) %>%
    arrange(topic, -beta)
  
  print(
    top_terms %>%
      mutate(term = reorder_within(term, beta, topic)) %>%
      ggplot(aes(term, beta, fill = factor(topic))) +
      geom_col(show.legend = FALSE) +
      facet_wrap(~ topic, scales = "free", ncol = 3) +
      coord_flip() +
      scale_x_reordered() +
      labs(title = paste("Top Terms in Topics -", group_label),
           x = NULL, y = "Beta")
  )
  
  top_terms_summary <- top_terms %>%
    group_by(topic) %>%
    summarise(words = list(term)) %>%
    mutate(country = group_label)
  
  return(top_terms_summary)
}


countries <- c("Pakistan", "India", "Myanmar", "China")
country_topics <- list()

for (country in countries) {
  cat("Processing:", country, "\n")
  country_data <- df %>% filter(country == !!country)
  country_topics[[country]] <- run_lda_plot(country_data, country)
}





plot_country_wordcloud <- function(data, country_name, num_topics = 6) {
  cat("Generating word cloud for:", country_name, "\n")
  
  tidy_docs <- data %>%
    select(link, corpus) %>%
    unnest_tokens(word, corpus) %>%
    anti_join(stop_words, by = "word") %>%
    filter(nchar(word) > 2) %>%
    count(link, word, sort = TRUE) %>%
    ungroup()
  
  word_counts <- tidy_docs %>%
    group_by(word) %>%
    summarise(total = sum(n)) %>%
    filter(total >= 2)
  
  tidy_docs <- tidy_docs %>%
    semi_join(word_counts, by = "word")
  
  dtm <- tidy_docs %>%
    cast_dtm(link, word, n)
  
  if (nrow(dtm) < 6) {
    cat("Not enough documents for:", country_name, "\n")
    return(NULL)
  }
  
  lda_model <- LDA(dtm, k = num_topics, control = list(seed = 1234))
  
  top_words <- tidy(lda_model, matrix = "beta") %>%
    group_by(topic) %>%
    top_n(1, beta) %>%
    ungroup()
  
  gamma <- tidy(lda_model, matrix = "gamma")
  dominant_topics <- gamma %>%
    group_by(document) %>%
    slice_max(gamma) %>%
    ungroup()
  
  topic_freq <- dominant_topics %>%
    count(topic, name = "count")
  
  wordcloud_data <- top_words %>%
    inner_join(topic_freq, by = "topic") %>%
    select(term, count)
  
  
  p <- ggplot(wordcloud_data, aes(label = term, size = count)) +
    geom_text_wordcloud_area(color = "black", family = "sans") +
    scale_size_area(max_size = 20) +
    theme_minimal() +
    labs(title = paste("Dominant Topic Words in", country_name))
  
  print(p)
}



countries <- unique(na.omit(xl$country))

for (country in countries) {
  country_data <- xl %>% filter(country == !!country)
  plot_country_wordcloud(country_data, country_name = country, num_topics = 6)
}


top_terms %>%
  mutate(term = reorder_within(term, beta, topic)) %>%
  ggplot(aes(term, beta, fill = factor(topic))) +
  geom_col(show.legend = FALSE) +
  scale_x_reordered() +
  facet_wrap(~ topic, scales = "free") +
  coord_flip() +
  labs(title = "Top Terms per Topic", x = "Term", y = "Beta (Probability)")




top_words_per_topic <- tidy(lda_model, matrix = "beta") %>%
  group_by(topic) %>%
  top_n(1, beta) %>%     
  ungroup()


dominant_topic_counts <- dominant_topics %>%
  count(topic, name = "count")


wordcloud_data <- top_words_per_topic %>%
  inner_join(dominant_topic_counts, by = "topic") %>%
  select(term, count)


ggplot(wordcloud_data, aes(label = term, size = count)) +
  geom_text_wordcloud_area(color = "black", family = "sans") +
  scale_size_area(max_size = 20) +
  theme_minimal() +
  labs(title = "Most Dominant Topic Words (by Document Frequency)")
