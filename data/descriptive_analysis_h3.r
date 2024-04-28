library("pacman")
p_load("tidyverse", "haven", "stargazer")

# Load data from data/media_topic_salience_index.rds into df_media_salience_index
df_media_salience_index <- readRDS("data/media_topic_salience_index.rds")

# Load data from data/party_topic_salience_index.rds into df_party_salience_index
df_party_salience_index <- readRDS("data/party_topic_salience_index.rds")

# Plot the distribution of df_media_salience_index$media_topic_salience_index with the x axis being filename and the y axis being media_topic_salience_index, color being df_media_salience_index$
df_media_salience_index %>%
    ggplot(aes(x = as.integer(filename), y = media_topic_salience_index, color = major_topic_pred_name)) +
    geom_point() +
    geom_smooth() +
    theme_minimal() +
    theme(axis.text.x = element_text(angle = 90, hjust = 1)) +
    scale_y_continuous(limits = c(0, 0.2)) +
    labs(
        title = "Distribution of media_topic_salience_index",
        x = "Filename",
        y = "Media Topic Salience Index",
        color = "Major Topic Predicted"
    ) -> p_salience_index

## Plot "Domestic Commerce", "International Affairs", "Law and Crime", "Government Operations", "Environment", "Transportation", "Civil Rights", "Culture", "Defense", "Health" topics separately

df_media_salience_index %>%
    filter(major_topic_pred_name %in% c("Domestic Commerce", "International Affairs", "Law and Crime", "Government Operations", "Environment", "Transportation", "Civil Rights", "Culture", "Defense", "Health")) %>%
    ## Recode filname from H3_YYYYMMDD – XXXXXXXX to YYYY-MM-DD
    mutate(filename = as.Date(paste0(
        substr(filename, 4, 7),
        "-",
        substr(filename, 8, 9),
        "-", substr(filename, 10, 11)
        ), "%Y-%m-%d")) %>%
    mutate(date = as.Date(filename, "%Y/%m/%d")) %>%
    ggplot(aes(x = date, y = media_topic_salience_index, color = major_topic_pred_name)) +
    geom_point() +
    geom_line() +
    theme_minimal() +
    theme(axis.text.x = element_text(angle = 90, hjust = 1)) +
    scale_y_continuous(limits = c(0, 0.2)) +
    scale_x_date(date_breaks = "1 month", date_labels = "%b %Y") +
    labs(
        title = "Distribution of MISI for most important topics",
        x = "Date",
        y = "Media Topic Salience Index",
        color = "Major Topic",
    )

## Calculate important descriptive statistics for media_topic_salience_index grouped by major_topic_pred_name
df_media_salience_index %>%
    group_by(major_topic_pred_name) %>%
    summarise(
        mean = round(mean(media_topic_salience_index), digits = 4),
        median = round(median(media_topic_salience_index), digits = 4),
        sd = round(sd(media_topic_salience_index), digits = 4),
        min = round(min(media_topic_salience_index), digits = 4),
        max = round(max(media_topic_salience_index), digits = 4)
    ) %>%
    ungroup() -> df_media_salience_index_summary

write.csv(as.data.frame(df_media_salience_index_summary), "output/media_salience_index_summary.csv")

## Select top 5 topics with the highest mean media_topic_salience_index for each party
df_party_salience_index %>%
    group_by(party) %>%
    top_n(5, party_topic_salience_index) %>%
    ungroup() %>%
    arrange(party, desc(party_topic_salience_index)) -> df_party_salience_index_top5

## Count the number of unique major_topic_pred_name in df_party_salience_index_top5
df_party_salience_index_top5 %>%
    select(major_topic_pred_name) %>%
    distinct() %>%
    nrow()

write.csv(as.data.frame(df_party_salience_index_top5), "output/party_salience_index_top5.csv")
