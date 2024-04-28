library("pacman")
p_load(
    "tidyverse"
)

## Load data from newsmedia/coded/results_H3_prepared_for_babel_machine_1rLmyhIkGD_with_pred.csv into df_coded
df_coded <- read_csv("data/newsmedia/coded/results_H3_prepared_for_babel_machine_1rLmyhIkGD_with_pred.csv")

## Load data from newsmedia/coded/softmax_H3_prepared_for_babel_machine_1rLmyhIkGD_softmax.csv into df_softmax
df_softmax <- read_csv("data/newsmedia/coded/softmax_H3_prepared_for_babel_machine_1rLmyhIkGD_softmax.csv")

## Create unique values of df_coded$filename and store into v_filenames
df_coded %>%
    select(filename) %>%
    distinct() %>%
    pull() -> v_filenames

## Create unique values of df_coded$major_topic_pred and store into v_topics
df_coded %>%
    select(major_topic_pred) %>%
    distinct() %>%
    pull() -> v_topics

## Create a data frame with all possible combinations of v_filenames and v_topics
expand.grid(v_filenames, v_topics) %>%
    set_names(c("filename", "major_topic_pred")) %>%
    mutate(media_topic_salience_index = 0) -> df_media_topic_salience_index

## Select all IDs from df_softmax where df_softmax$label_0_prob > 0.5
df_softmax %>%
    filter(label_0_prob > 0.9) %>%
    select(id) %>%
    pull() -> v_id

nrow(df_coded)
df_coded %>%
    filter(id %in% v_id) -> df_coded
nrow(df_coded)

## Iterate over each row of df_media_topic_salience_index
for (i in 1:nrow(df_media_topic_salience_index)) {
    ## Filter df_coded for the current row's filename and major_topic_pred
    df_coded %>%
        filter(filename == df_media_topic_salience_index$filename[i] & major_topic_pred == df_media_topic_salience_index$major_topic_pred[i]) %>%
        # ## count all rows and divide by the total number of rows with the same filename
        nrow() / nrow(df_coded %>% filter(filename == df_media_topic_salience_index$filename[i])) -> df_media_topic_salience_index$media_topic_salience_index[i]
}

## Find first occurence of each topic in df_coded and pull df_coded$major_topic_pred_name from it and merge it with df_media_topic_salience_index
df_coded %>%
    group_by(major_topic_pred) %>%
    slice(1) %>%
    select(major_topic_pred, major_topic_pred_name) %>%
    ungroup() %>%
    left_join(df_media_topic_salience_index, by = "major_topic_pred") -> df_media_topic_salience_index

## Convert df_media_topic_salience_index$major_topic_pred_name and filename to factor
df_media_topic_salience_index %>%
    mutate(
        major_topic_pred_name = as.factor(major_topic_pred_name),
        filename = as.factor(filename)
    ) -> df_media_topic_salience_index

view(df_media_topic_salience_index)

## Write df_media_topic_salience_index to media_topic_salience_index.v2.rds
saveRDS(df_media_topic_salience_index, "data/media_topic_salience_index.v2.rds")


# ## Pick the top 5 highest media_topic_salience_index for each filename
# df_media_topic_salience_index %>%
#     group_by(filename) %>%
#     top_n(5, media_topic_salience_index) %>%
#     ungroup() %>%
#     arrange(filename, desc(media_topic_salience_index)) -> df_media_topic_salience_index_top5
