library("pacman")
p_load(
    "tidyverse"
)

## Load data from party_topic_salience_index.rds into df_party_salience_index
df_party_salience_index <- readRDS("data/party_topic_salience_index.rds")

## Load data from media_topic_salience_index.rds into df_media_salience_index
df_media_salience_index <- readRDS("data/media_topic_salience_index.rds")

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

## Do the same for filname "H3_20230822 – 20230921" and "H3_20230422 – 20230521"
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

## Add no_seats column to df_final: Where party_code=1 => no_seats=53, party_code=2 => no_seats=39, party_code=3 => no_seats = 29, party_code=4 => no_seats=25, party_code=5 => no_seats=28, party_code=6 => no_seats=16
df_final %>%
    mutate(
        no_seats = case_when(
            party_code == 1 ~ 53,
            party_code == 2 ~ 39,
            party_code == 3 ~ 29,
            party_code == 4 ~ 25,
            party_code == 5 ~ 28,
            party_code == 6 ~ 16
        )
    ) -> df_final

## Add lrecon and galtan values to df_final:
## The following shows party_code - lrecon combinations: 1=8.181818, 2=1.6363636, 3=8.090909, 4=5.2727275,5=1.5454545,6=6.090909
## Attribution: Ryan Bakker, Liesbet Hooghe, Seth Jolly, Gary Marks, Jonathan Polk, Jan Rovny, Marco Steenbergen, and Milada Anna Vachudova. 2020. “2019 Chapel Hill Candidate Survey.” Version 2019.1. Available on chesdata.eu. Chapel Hill, NC: University of North Carolina, Chapel Hill.

df_final %>%
    mutate(
        lrecon = case_when(
            party_code == 1 ~ 8.181818,
            party_code == 2 ~ 1.6363636,
            party_code == 3 ~ 8.090909,
            party_code == 4 ~ 5.2727275,
            party_code == 5 ~ 1.5454545,
            party_code == 6 ~ 6.090909
        ),
        galtan = case_when(
            party_code == 1 ~ 9.363636,
            party_code == 2 ~ 1.3636364,
            party_code == 3 ~ 5,
            party_code == 4 ~ 5.7272725,
            party_code == 5 ~ 0.72727275,
            party_code == 6 ~ 2.3636363
        )
    ) -> df_final

## Write df_final to final_dataset.rds
saveRDS(df_final, "data/final_dataset.rds")
