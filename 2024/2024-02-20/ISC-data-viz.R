#
# Author: Andrew Disher
# Date: 2/19/2024
# Topic: Tidy Tuesday - R Consortium ISC Grants
#
# TASK: Create a visualization for historic ISC grant data. 

# ------------------------------
# ----- Libraries/Packages -----
# ------------------------------

box::use(
  dplyr[`%>%`, arrange, case_when, desc, filter, group_by, mutate, n, row_number, summarize, ungroup], 
  ggplot2[...],
  ggtext[...],
  ggwordcloud[...],
  monochromeR[generate_palette],
  tidyr[pivot_longer],
  tidytuesdayR[tt_load],
  udpipe[...]
)

# -------------------------------------
# ----- Negation of %in% function -----
# -------------------------------------

`%notin%` <- Negate(`%in%`)

# ------------------------------------------
# ----- Download Data from GitHub Repo ----- 
# ------------------------------------------

tuesdata <- tt_load('2024-02-20')

isc_grants <- tuesdata$isc_grants

# Create an ID column for each document
isc_grants <- isc_grants %>% 
  mutate(doc_id = row_number())

# ----------------------------------
# ----- Preparing Data for NLP -----
# ----------------------------------

# Store grant summaries in text column of new data frame
text_docs <- data.frame(
  doc_id = 1:nrow(isc_grants),
  year = isc_grants$year,
  text = isc_grants$summary
)

# Download udpipe english model
udmodel <- udpipe_download_model(language = "english")
udmodel <- udpipe_load_model(udmodel$file_model)

# Get words and parts of speech from all books in text_docs
summary_data_df <- udpipe_annotate(x = text_docs$text,
                                   doc_id = paste(text_docs$year, text_docs$doc_id, sep = "_"),
                                   object = udmodel)

summary_data_df <- summary_data_df %>% 
  as.data.frame()

# --------------------------------------
# ----- Remove all Unwanted Tokens -----
# --------------------------------------

# Define function to retrieve year for defining year column
get_year_from_id <- function(doc_id) {return(strsplit(doc_id, split = "_")[[1]][1])}

filtered_df <- summary_data_df %>% 
  filter(upos %in% c("NOUN", "PROPN", "ADJ", "VERB")) %>% 
  mutate(year = lapply(doc_id, get_year_from_id))

# ----------------------------
# ----- Word Collocation -----
# ----------------------------

# Create a data frame to contain phrase data by year
phrase_stats <- data.frame()

# Vector of years to iterate over
years_vector <- c(as.character(2016:2023), "All Years")

# For each year, find the most common topic phrases 
for (current_year in years_vector) {
  if(current_year == "All Years"){
    temp_df <- filtered_df %>% 
      keywords_collocation(term = "token", 
                           group = c("doc_id", "paragraph_id", "sentence_id"),
                           ngram_max = 2) %>% 
      arrange(desc(pmi)) %>% 
      mutate(year = current_year)
  }
  else {
    temp_df <- filtered_df %>% 
      filter(year == current_year) %>% 
      keywords_collocation(term = "token", 
                           group = c("doc_id", "paragraph_id", "sentence_id"),
                           ngram_max = 2) %>% 
      arrange(desc(pmi)) %>% 
      mutate(year = current_year)
  }
  
  phrase_stats <- rbind(phrase_stats, temp_df)
}

# ----------------------------------
# ----- Create Word Cloud Plot -----
# ----------------------------------

# Create subtitle
subtitle <- "Each phrase is made up of two commonly coocurring words and sized according to Pointwise <br></br>Mutual Information, indicating the relative strength of their association."

# Render plot
ggplot(data = phrase_stats[phrase_stats$year != "All Years",], mapping = aes(label = keyword, size = pmi, color = year)) +
  facet_wrap(~year, ncol = 4) + 
  geom_text_wordcloud(seed = 123) + 
  scale_size_area(max_size = 5) +
  theme_minimal(base_size = 20, base_family = "archivo") +
  theme(plot.background = element_rect(fill = "#fafafa"),
        plot.title = element_text(hjust = 0.5,
                                  color = "#4A618C"),
        strip.text = element_textbox(
          size = 12,
          color = "white", box.color = "#8fa2c4", fill = "#8fa2c4",
          halign = 0.5, linetype = 1, r = unit(5, "pt"), width = unit(1, "npc"),
          padding = margin(3, 0, 1, 0), margin = margin(3, 1.5, 3, 1.5)
        ),
        plot.subtitle = element_textbox(
          hjust = 0.5,
          halign = 0.5,
          color = "#8fa2c4",
          lineheight = .5,
          size = 14
        ),
        plot.caption = element_textbox(
          hjust = 0.5,
          halign = 0.5,
          color = "#4A618C",
          size = 10
        )) +
  labs(title = "Popular Topics Within R Consortium ISC Grants",
       subtitle = subtitle,
       caption = "Made by Andrew Disher")





