library("pacman")
p_load("tidyverse", "haven", "stargazer")

# Load data from data/media_topic_salience_index.rds into df_media_salience_index
df_media_salience_index <- readRDS("data/media_topic_salience_index.v2.rds")

# Load data from data/party_topic_salience_index.rds into df_party_salience_index
df_party_salience_index <- readRDS("data/party_topic_salience_index.rds")

# Plot the distribution of df_media_salience_index$media_topic_salience_index with the x axis being filename and the y axis being media_topic_salience_index, color being df_media_salience_index$
df_media_salience_index %>%
    ggplot(aes(x = as.integer(filename), y = media_topic_salience_index, color = major_topic_pred_name)) +
    geom_point() +
    geom_line() +
    theme_minimal() +
    theme(axis.text.x = element_text(angle = 90, hjust = 1)) +
    scale_y_continuous(limits = c(0, 0.2)) +
    labs(
        title = "Distribution of media_topic_salience_index",
        x = "Filename",
        y = "Media Topic Salience Index",
        color = "Major Topic Predicted"
    ) -> p_salience_index

## Plot "International Affairs", "domestic commerce", "Environment", "Government Operations" and "Law and Crime" separately
df_media_salience_index %>%
    filter(major_topic_pred_name %in% c("International Affairs", "Domestic Commerce", "Environment", "Government Operations", "Law and Crime")) %>%
    ggplot(aes(x = as.integer(filename), y = media_topic_salience_index, color = major_topic_pred_name)) +
    geom_point() +
    geom_line() +
    theme_minimal() +
    theme(axis.text.x = element_text(angle = 90, hjust = 1)) +
    scale_y_continuous(limits = c(0, 0.2)) +
    labs(
        title = "Distribution of media_topic_salience_index",
        x = "Filename",
        y = "Media Topic Salience Index",
        color = "Major Topic",
    ) -> p_salience_index_separate

## Calculate important descriptive statistics for media_topic_salience_index grouped by major_topic_pred_name
df_media_salience_index %>%
    group_by(major_topic_pred_name) %>%
    summarise(
        mean = mean(media_topic_salience_index),
        median = median(media_topic_salience_index),
        sd = sd(media_topic_salience_index),
        min = min(media_topic_salience_index),
        max = max(media_topic_salience_index)
    ) -> df_media_salience_index_summary

## Calculate important descriptive statistics for party_topic_salience_index grouped by major_topic_pred_name
df_party_salience_index %>%
    group_by(major_topic_pred_name) %>%
    summarise(
        mean = mean(party_topic_salience_index),
        median = median(party_topic_salience_index),
        sd = sd(party_topic_salience_index),
        min = min(party_topic_salience_index),
        max = max(party_topic_salience_index)
    ) -> df_party_salience_index_summary

## Select top 5 topics with the highest mean media_topic_salience_index for each party
df_party_salience_index %>%
    group_by(party) %>%
    top_n(5, party_topic_salience_index) %>%
    ungroup() %>%
    arrange(party, desc(party_topic_salience_index)) -> df_party_salience_index_top5

view(df_party_salience_index_top5)
