library("pacman")
p_load(
    "tidyverse",
    "stargazer"
)

## Load data from data/final_dataset.rds into df_final
df_final <- readRDS("data/final_dataset.rds")

## Create a linear regression model with only PISI
lm(MISI_1m ~ PISI, data = df_final) -> m1

## Add earnings variable to m2
lm(MISI_1m ~ PISI + party_earnings, data = df_final) -> m2

## Add ideological and party strength controls to m3
lm(MISI_1m ~ PISI + party_earnings + no_seats + lrecon + galtan, data = df_final) -> m3

## Add interaction between PISI and party_earnings
lm(MISI_1m ~ PISI + party_earnings + (PISI * party_earnings) + no_seats + lrecon + galtan, data = df_final) -> m4

## Add MISI_3m as a control
lm(MISI_1m ~ PISI + party_earnings + (PISI * party_earnings) + no_seats + lrecon + galtan + MISI_3m, data = df_final) -> m5

## Replace MISI_3m with MISI_6m
lm(MISI_1m ~ PISI + party_earnings + (PISI * party_earnings) + no_seats + lrecon + galtan + MISI_6m, data = df_final) -> m6

## Full model
lm(MISI_1m ~ PISI + party_earnings + no_seats + lrecon + galtan + MISI_3m + MISI_6m + (PISI * party_earnings), data = df_final) -> m7

stargazer(
    ## Add models to stargazer
    m1, m2, m3, m4, m5, m6, m7,
    ## Change dependent variable name
    dep.var.labels = "MISI within election month",
    ## Replace covariate names
    covariate.labels = c(
        "PISI",
        "Party Earnings",
        "Number of Seats",
        "Left-Right Scale",
        "GAL-TAN Scale",
        "MISI 3 months before ED",
        "MISI 6 bonths before ED",
        "PISI * Party Earnings"
    ),
    ## Group models
    column.labels = c(
        "PISI",
        "+ Earnings",
        "+ Controls",
        "+ Interaction",
        "+ MISI 3m",
        "alt: MISI 6m",
        "Full Model"
    ),
    ## Change stars to include p<0.5
    star.cutoffs = c(0.7, 0.1, 0.05, 0.01),
    star.char = c("・", "*", "**", "***"),
    ## Add custom cutoffs to notes
    notes = "・p<0.7; *p<0.1; **p<0.05; ***p<0.01",
    type = "html", out = "output/tables/table_5.html"
)

stargazer(
    ## Add models to stargazer
    m1, m2, m3, m4, m5, m6, m7,
    ## Change dependent variable name
    dep.var.labels = "MISI within election month",
    column.sep.width = "10pt",
    font.size = "large",
    ## Replace covariate names
    covariate.labels = c(
        "PISI",
        "Party Earnings",
        "Number of Seats",
        "Left-Right Scale",
        "GAL-TAN Scale",
        "MISI 3 months before ED",
        "MISI 6 bonths before ED",
        "PISI * Party Earnings"
    ),
    ## Group models
    column.labels = c(
        "PISI",
        "+ Earnings",
        "+ Controls",
        "+ Interaction",
        "+ MISI 3m",
        "alt: MISI 6m",
        "Full Model"
    ),
    ## Change stars to include p<0.5
    star.cutoffs = c(0.7, 0.1, 0.05, 0.01),
    star.char = c("・", "*", "**", "***"),
    ## Add custom cutoffs to notes
    notes = "・p<0.7; *p<0.1; **p<0.05; ***p<0.01",
    type = "latex", out = "output/tables/table_5.tex"
)
