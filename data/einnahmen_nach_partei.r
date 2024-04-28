install.packages("pacman")
pacman::p_load(
    tidyverse,
    uuid
)
options(digits = 8)

# Load raw csv "2023_10_22_Nationalratswahlen_2023_Schlussrechnung_über_die_Einnahmen_und_Zuwendungen.csv" into zuwendungen variable
zuwendungen <- read_csv("data/2023_10_22_Nationalratswahlen_2023_Schlussrechnung_über_die_Einnahmen_und_Zuwendungen.csv")

# Create unique list of "akteur"
akteurs <- zuwendungen %>%
    select(Akteur) %>%
    distinct()

# Create table akteurs as columns in a new df_budgets
df_budgets <- tibble(
    akteur = akteurs$Akteur
)

## Find number of occurences of akteur in zuwendungen and add it to df_budgets
df_budgets <- df_budgets %>%
    mutate(
        anzahl_unterstuetze_kandis = map_dbl(akteur, ~ sum(zuwendungen$Akteur == .x))
    )

## Find first instance of "Gesamtbetrag der Einnahmen (in CHF)", "Einnahmen durch monetäre Zuwendungen (in CHF)", Wert der Einnahmen durch nichtmonetäre Zuwendungen (in CHF), Einnahmen durch Veranstaltungen (in CHF), Einnahmen durch den Verkauf von Gütern und Dienstleistungen (in CHF) and Monetäre Eigenmittel (in CHF) per akteur, divide it by "Anzahl unterstützte Kandidaturen" and add it to df_budgets
df_budgets <- df_budgets %>%
    mutate(
        gesamtbetrag_der_einnahmen = map_dbl(akteur, ~ {
            zuwendungen %>%
                filter(Akteur == .x) %>%
                select(`Gesamtbetrag der Einnahmen (in CHF)`) %>%
                slice(1) %>%
                pull()
        }),
        gesamtbetrag_einnahmen_pro_kandi = gesamtbetrag_der_einnahmen / anzahl_unterstuetze_kandis,
        einnahmen_durch_monetaere_zuwendungen = map_dbl(akteur, ~ {
            zuwendungen %>%
                filter(Akteur == .x) %>%
                select(`Einnahmen durch monetäre Zuwendungen (in CHF)`) %>%
                slice(1) %>%
                pull()
        }),
        einnahmen_durch_monetaere_zuwendungen_pro_kandi = einnahmen_durch_monetaere_zuwendungen / anzahl_unterstuetze_kandis,
        wert_der_einnahmen_durch_nicht_monetaere_zuwendungen = map_dbl(akteur, ~ {
            zuwendungen %>%
                filter(Akteur == .x) %>%
                select(`Wert der Einnahmen durch nichtmonetäre Zuwendungen (in CHF)`) %>%
                slice(1) %>%
                pull()
        }),
        wert_der_einnahmen_durch_nicht_monetaere_zuwendungen_pro_kandi = wert_der_einnahmen_durch_nicht_monetaere_zuwendungen / anzahl_unterstuetze_kandis,
        einnahmen_durch_veranstaltungen = map_dbl(akteur, ~ {
            zuwendungen %>%
                filter(Akteur == .x) %>%
                select(`Einnahmen durch Veranstaltungen (in CHF)`) %>%
                slice(1) %>%
                pull()
        }),
        einnahmen_durch_veranstaltungen_pro_kandi = einnahmen_durch_veranstaltungen / anzahl_unterstuetze_kandis,
        einnahmen_durch_den_verkauf_von_gueter_und_dienstleistungen = map_dbl(akteur, ~ {
            zuwendungen %>%
                filter(Akteur == .x) %>%
                select(`Einnahmen durch den Verkauf von Gütern und Dienstleistungen (in CHF)`) %>%
                slice(1) %>%
                pull()
        }),
        einnahmen_durch_den_verkauf_von_gueter_und_dienstleistungen_pro_kandi = einnahmen_durch_den_verkauf_von_gueter_und_dienstleistungen / anzahl_unterstuetze_kandis,
        monetare_eigenmittel = map_dbl(akteur, ~ {
            zuwendungen %>%
                filter(Akteur == .x) %>%
                select(`Monetäre Eigenmittel (in CHF)`) %>%
                slice(1) %>%
                pull()
        }),
        monetare_eigenmittel_pro_kandi = monetare_eigenmittel / anzahl_unterstuetze_kandis
    )

## Add generated columns to zuwendungen based on the akteur
zuwendungen <- zuwendungen %>%
    left_join(df_budgets, by = c("Akteur" = "akteur"))

## Create unique list of "Parteizugehörigkeit (Mutterpartei)", add it to df_parties and add uuid column in the beginning
df_parties <- zuwendungen %>%
    select(`Parteizugehörigkeit (Mutterpartei)`) %>%
    distinct()

## Sum up the generated columns per party and add it to df_parties
df_parties <- df_parties %>%
    mutate(
        gesamtbetrag_einnahmen_pro_kandi = map_dbl(`Parteizugehörigkeit (Mutterpartei)`, ~ sum(zuwendungen$gesamtbetrag_einnahmen_pro_kandi[zuwendungen$`Parteizugehörigkeit (Mutterpartei)` == .x])),
        einnahmen_durch_monetaere_zuwendungen_pro_kandi = map_dbl(`Parteizugehörigkeit (Mutterpartei)`, ~ sum(zuwendungen$einnahmen_durch_monetaere_zuwendungen_pro_kandi[zuwendungen$`Parteizugehörigkeit (Mutterpartei)` == .x])),
        wert_der_einnahmen_durch_nicht_monetaere_zuwendungen_pro_kandi = map_dbl(`Parteizugehörigkeit (Mutterpartei)`, ~ sum(zuwendungen$wert_der_einnahmen_durch_nicht_monetaere_zuwendungen_pro_kandi[zuwendungen$`Parteizugehörigkeit (Mutterpartei)` == .x])),
        einnahmen_durch_veranstaltungen_pro_kandi = map_dbl(`Parteizugehörigkeit (Mutterpartei)`, ~ sum(zuwendungen$einnahmen_durch_veranstaltungen_pro_kandi[zuwendungen$`Parteizugehörigkeit (Mutterpartei)` == .x])),
        einnahmen_durch_den_verkauf_von_gueter_und_dienstleistungen_pro_kandi = map_dbl(`Parteizugehörigkeit (Mutterpartei)`, ~ sum(zuwendungen$einnahmen_durch_den_verkauf_von_gueter_und_dienstleistungen_pro_kandi[zuwendungen$`Parteizugehörigkeit (Mutterpartei)` == .x])),
        monetare_eigenmittel_pro_kandi = map_dbl(`Parteizugehörigkeit (Mutterpartei)`, ~ sum(zuwendungen$monetare_eigenmittel_pro_kandi[zuwendungen$`Parteizugehörigkeit (Mutterpartei)` == .x]))
    )

## Rename columns to more legible names
df_parties <- df_parties %>%
    rename(
        partei = `Parteizugehörigkeit (Mutterpartei)`,
        gesamt = gesamtbetrag_einnahmen_pro_kandi,
        monetar = einnahmen_durch_monetaere_zuwendungen_pro_kandi,
        n_monetar = wert_der_einnahmen_durch_nicht_monetaere_zuwendungen_pro_kandi,
        veranstaltungen = einnahmen_durch_veranstaltungen_pro_kandi,
        verkauf = einnahmen_durch_den_verkauf_von_gueter_und_dienstleistungen_pro_kandi,
        eigenmittel = monetare_eigenmittel_pro_kandi
    )

## Add UUID to df_parties as the first column
df_parties <- df_parties %>%
    mutate(
        uuid = uuid::UUIDgenerate()
    ) %>%
    select(
        uuid,
        everything()
    )

## Add party code to df_parties: "Die Mitte"=4,"FDP.Die Liberalen"=3,"GRÜNE Schweiz"=5,"Grünliberale Partei"=6,"Schweizerische Volkspartei"=1,"Sozialdemokratische Partei der Schweiz"=2
df_parties <- df_parties %>%
    mutate(
        party_code = recode(
            partei,
            "Die Mitte" = 4,
            "FDP.Die Liberalen" = 3,
            "GRÜNE Schweiz" = 5,
            "Grünliberale Partei" = 6,
            "Schweizerische Volkspartei" = 1,
            "Sozialdemokratische Partei der Schweiz" = 2,
            .default = 99
        )
    )

## Write df_parties to .rds file
saveRDS(df_parties, "data/party_earnings.rds")

view(df_parties)
