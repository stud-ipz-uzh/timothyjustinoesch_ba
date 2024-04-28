library("pacman")
p_load(
    "tidyverse"
)

## Load data from party_topic_salience_index.rds into df_party_salience_index
df_party_salience_index <- readRDS("data/party_topic_salience_index.rds")

## Load data from media_topic_salience_index.rds into df_media_salience_index
df_media_salience_index <- readRDS("data/media_topic_salience_index.v2.rds")

## Load data from party_earnings.rds into df_party_earnings
df_party_earnings <- readRDS("data/party_earnings.rds")

## Merge party earnings from df_party_earnings with party salience index from df_party_salience_index through party_code
df_party_salience_index %>%
    left_join(df_party_earnings, by = "party_code") -> df_full

## Merge media salience index from df_media_salience_index with df_final through major_topic_pred
df_full %>%
    left_join(df_media_salience_index, by = "major_topic_pred") -> df_full

## Initialize final dataframe df_final
df_full %>%
    select(
        major_topic_pred_name.x,
        major_topic_pred,
        partei,
        party_code,
        party_topic_salience_index
    ) %>%
    rename(
        major_topic_pred_name = major_topic_pred_name.x,
        party_name = partei,
        party_code = party_code,
        PISI = party_topic_salience_index
    ) %>%
    ## Remove dupilcate rows
    distinct() -> df_final

## Identify all rows in df_full where filename = H3_20230922 – 20231021 and store media_topic_salience_index in df_final
df_full %>%
    filter(filename == "H3_20230922 – 20231021") %>%
    select(
        media_topic_salience_index,
        major_topic_pred,
        party_code
    ) %>%
    ## Merge based on major_topic_pred and party_code
    left_join(df_final, by = c("major_topic_pred" = "major_topic_pred", "party_code" = "party_code")) %>%
    mutate (
        MISI_1m = media_topic_salience_index,
    ) %>%
    ## Remove column media_topic_salience_index
    select(-media_topic_salience_index) -> df_final

## Do the same for filname "H3_20230822 – 20230921", "H3_20230722 – 20230821" and "H3_20230422 – 20230521"
df_full %>%
    filter(filename == "H3_20230822 – 20230921") %>%
    select(
        media_topic_salience_index,
        major_topic_pred,
        party_code
    ) %>%
    left_join(df_final, by = c("major_topic_pred" = "major_topic_pred", "party_code" = "party_code")) %>%
    mutate (
        MISI_2m = media_topic_salience_index,
    ) %>%
    select(-media_topic_salience_index) -> df_final

df_full %>%
    filter(filename == "H3_20230722 – 20230821") %>%
    select(
        media_topic_salience_index,
        major_topic_pred,
        party_code
    ) %>%
    left_join(df_final, by = c("major_topic_pred" = "major_topic_pred", "party_code" = "party_code")) %>%
    mutate (
        MISI_3m = media_topic_salience_index,
    ) %>%
    select(-media_topic_salience_index) -> df_final

df_full %>%
    filter(filename == "H3_20230422 – 20230521") %>%
    select(
        media_topic_salience_index,
        major_topic_pred,
        party_code
    ) %>%
    left_join(df_final, by = c("major_topic_pred" = "major_topic_pred", "party_code" = "party_code")) %>%
    mutate (
        MISI_6m = media_topic_salience_index,
    ) %>%
    select(-media_topic_salience_index) -> df_final

## Identify all rows in df_full where filename = H3_20230922 – 20231021 and store gesamt in df_final as "party_earnings"
df_full %>%
    filter(filename == "H3_20230922 – 20231021") %>%
    select(
        gesamt,
        major_topic_pred,
        party_code
    ) %>%
    left_join(df_final, by = c("major_topic_pred" = "major_topic_pred", "party_code" = "party_code")) %>%
    mutate (
        party_earnings = gesamt,
    ) %>%
    select(-gesamt) -> df_final

## Write df_final to final_dataset.v2.rds
saveRDS(df_final, "data/final_dataset.v2.rds")
