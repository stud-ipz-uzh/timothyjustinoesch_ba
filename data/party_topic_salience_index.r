library("pacman")
p_load(
    "tidyverse"
)

## Load data from partypositions/coded/results_partypositions_c8jfwPELgG_with_pred.csv into df_coded
df_coded <- read_csv("data/partypositions/coded/results_partypositions_c8jfwPELgG_with_pred.csv")

## Create unique values of df_coded$party and store into v_parties
df_coded %>%
    select(party) %>%
    distinct() %>%
    pull() -> v_parties

## Create unique values of df_coded$major_topic_pred and store into v_topics
df_coded %>%
    select(major_topic_pred) %>%
    distinct() %>%
    pull() -> v_topics

## Create a data frame with all possible combinations of v_parties and v_topics
expand.grid(v_parties, v_topics) %>%
    set_names(c("party", "major_topic_pred")) %>%
    mutate(party_topic_salience_index = 0) -> df_party_topic_salience_index

## Iterate over each row of df_party_topic_salience_index
for (i in 1:nrow(df_party_topic_salience_index)) {
    ## Count total number of characters in the text column for the current party and store it in v_total_characters
    df_coded %>%
        filter(party == df_party_topic_salience_index$party[i]) %>%
        pull(text) %>%
        str_replace_all("\\s", "") %>%
        ## Remove all whitespace characters
        nchar() %>%
        sum() -> v_total_characters

    ## Count number of characters in the text column for the current party and major_topic_pred
    df_coded %>%
        filter(party == df_party_topic_salience_index$party[i] & major_topic_pred == df_party_topic_salience_index$major_topic_pred[i]) %>%
        pull(text) %>%
        ## Remove all whitespace characters
        str_replace_all("\\s", "") %>%
        nchar() %>%
        sum() -> v_topic_characters

    ## If v_total_characters is not 0, calculate the party_topic_salience_index else set it to 0
    if (v_total_characters != 0) {
        v_topic_characters / v_total_characters -> df_party_topic_salience_index$party_topic_salience_index[i]
    } else {
        0 -> df_party_topic_salience_index$party_topic_salience_index[i]
    }
}

## Find first occurence of each topic in df_coded and pull df_coded$major_topic_pred_name from it and merge it with df_party_topic_salience_index
df_coded %>%
    group_by(major_topic_pred) %>%
    slice(1) %>%
    select(major_topic_pred, major_topic_pred_name) %>%
    ungroup() %>%
    left_join(df_party_topic_salience_index, by = "major_topic_pred") -> df_party_topic_salience_index

## Add a column party_code where SVP=1,SP=2,FDP=3,"Die Mitte"=4,Grüne=5,GLP=6
df_party_topic_salience_index %>%
    mutate(
        party_code = recode(
            party,
            "SVP" = 1,
            "SP" = 2,
            "FDP" = 3,
            "Die Mitte" = 4,
            "GPS" = 5,
            "glp"=6
        )
    ) -> df_party_topic_salience_index

## Convert df_media_topic_salience_index$major_topic_pred_name to factor
df_party_topic_salience_index %>%
    mutate(
        major_topic_pred_name = as.factor(major_topic_pred_name),
        party = as.factor(party)
    ) -> df_party_topic_salience_index

## Write df_media_topic_salience_index to party_topic_salience_index.rds
saveRDS(df_party_topic_salience_index, "data/party_topic_salience_index.rds")
