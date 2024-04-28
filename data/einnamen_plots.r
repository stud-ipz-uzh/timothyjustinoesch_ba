library("pacman")
p_load(
    "tidyverse",
    "reshape2"
)

## load "einnahmen_nach_partei.dta" into df_einnahmen dataframe
df_einnahmen <- haven::read_dta("data/einnahmen_nach_partei.dta")

## Recode party names in accordance with thesis document

df_einnahmen %>%
    mutate(
        partei = case_when(
            partei == "Die Mitte" ~ "Die Mitte",
            partei == "FDP.Die Liberalen" ~ "FDP",
            partei == "GRÜNE Schweiz" ~ "GPS",
            partei == "Sozialdemokratische Partei der Schweiz" ~ "SP",
            partei == "Schweizerische Volkspartei" ~ "SVP",
            partei == "Grünliberale Partei" ~ "glp",
            partei == "Evangelische Volkspartei der Schweiz" ~ "EVP*",
            partei == "Eidgenössisch-Demokratische Union" ~ "EDU*",
            partei == "Lega dei Ticinesi" ~ "Lega*",
            partei == "Übrige politische Parteien" ~ "Other parties",
            partei == "Unabhängig" ~ "Independent"
        )) -> df_einnahmen

# Plot the columns "gesamt", "monetar", "n_monetar", "veranstaltungen", "verkauf", "eigenmittel" in a barplot for each party
plot_einnahmen_nach_partei <- df_einnahmen %>%
    select(partei, gesamt, monetar, eigenmittel) %>%
    ## Rename columns to have better labels
    rename(
        "Total earnings" = gesamt,
        "Monetary earnings" = monetar,
        "Party Capial" = eigenmittel
    ) %>%
    melt(id.vars = "partei") %>%
    ggplot(aes(x = reorder(partei, -value), y = value)) +
        geom_bar(width = .75, stat = "identity", aes(fill = variable), position = "dodge") +
        guides(fill = guide_legend(title = "Earnings source")) +
        theme_bw() +
        scale_fill_grey(start = 0, end = .9) +
        theme(axis.text.x = element_text(angle = 45, hjust = 1)) +
        labs(
            title = "Earnings by party and source",
            x = "Party",
            y = "Earnings (in 1'000'000 CHF)"
        ) +
        scale_y_continuous(
            labels = function(x) {
                format(x / 1000000,
                    big.mark = "'",
                    scientific = FALSE
                )
            }
        ) +
        theme(legend.position = "top")

plot_einnahmen_nach_partei

## Calculate share of df_einnahmen$gesamt where df_einnahmen$partei="Unabhängig" on total df_einnahmen$gesamt
df_einnahmen %>%
    filter(partei == "Unabhängig") %>%
    summarise(
        gesamt_unabhaengig = sum(gesamt)
    ) %>%
    mutate(
        anteil_gesamt = (gesamt_unabhaengig / sum(df_einnahmen$gesamt)) * 100

    )

## Calculate the share of df_einnahmen$monetar + df_einnahmen$n_monetar + df_einnahmen$verkauf + df_einnahmen$verkauf on df_einnahmen$gesamt for each row
df_einnahmen %>%
    mutate(
        earnings_against_ = (monetar + n_monetar + verkauf + veranstaltungen) / eigenmittel
    ) %>%
    view()
