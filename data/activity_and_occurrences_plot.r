# Read data from party_activity_and_occurrences_in_media.csv
options(scipen = 999)
library("pacman")
p_load(
    "tidyverse",
    "reshape2",
    "egg"
)
activity_and_occurrences <- read.csv("data/party_activity_and_occurrences_in_media.csv")

activity_without_mitte <- activity_and_occurrences %>%
    filter(party != "Die Mitte")

## Plot occurr_6m grouped by party as a bar chart
ggplot(activity_without_mitte, aes(x = reorder(party, -occurr_6m), y = occurr_6m)) +
    geom_bar(stat = "identity") +
    theme_minimal() +
    theme(axis.text.x = element_text(angle = 45, hjust = 1)) +
    labs(
        title = "Occurrences in media within 6 months of ED",
        x = "Party",
        y = "Occurrences in media"
    ) +
    geom_text(
        aes(label = occurr_6m),
        hjust = -.1,
        vjust = -.1,
        size = 3
    ) -> plot_occurr_6m

# Scatter plot where x is earnings_national and y is occurr_1m with #000000, occurr_3m with #A6A6A6
ggplot(activity_without_mitte, aes(x = log(earnings_national), y = occurr_6m, color = "#A6A6A6")) +
    geom_point() +
    geom_smooth() +
    geom_point(aes(y = occurr_1m, color = "#000000")) +
    geom_smooth(aes(y = occurr_1m, color = "#000000")) +
    geom_point(aes(y = occurr_3m, color = "#595959")) +
    geom_smooth(aes(y = occurr_3m, color = "#595959")) +
    labs(
        x = "Log of Earnings of national parties",
        y = "Occurrences in media",
        title = "Earnings vs Occurrences in media",
        color = ""
    ) +
    scale_color_manual(
        values = c("#000000", "#A6A6A6", "#595959"),
        labels = c("Occurrences within 1 month of ED", "Occurrences within 3 months of ED", "Occurrences within 6 months of ED")
    ) +
    theme_minimal() +
    geom_text(
        aes(label = party),
        hjust = -.1,
        vjust = -.1,
        size = 3
    ) -> plot_occurr_v_earnings

## Arrange the plots in two columns with ggarrange
ggarrange(plot_occurr_6m, plot_occurr_v_earnings, ncol = 2, nrow = 1)

## Plot insta_6m grouped by party as a bar chart
ggplot(activity_and_occurrences, aes(x = reorder(party, -insta_6m), y = insta_6m)) +
    geom_bar(stat = "identity") +
    theme_minimal() +
    theme(axis.text.x = element_text(angle = 45, hjust = 1)) +
    labs(
        title = "Instagram posts within 6 months of ED",
        x = "Party",
        y = "Instagram posts"
    ) +
    geom_text(
        aes(label = insta_6m),
        hjust = -.1,
        vjust = -.1,
        size = 3
    ) -> plot_insta_6m

## Plot pr_6m grouped by party as a bar chart
ggplot(activity_and_occurrences, aes(x = reorder(party, -pr_6m), y = pr_6m)) +
    geom_bar(stat = "identity") +
    theme_minimal() +
    theme(axis.text.x = element_text(angle = 45, hjust = 1)) +
    labs(
        title = "Press releases within 6 months of ED",
        x = "Party",
        y = "Press releases"
    ) +
    geom_text(
        aes(label = pr_6m),
        hjust = -.1,
        vjust = -.1,
        size = 3
    ) -> plot_pr_6m

# Scatter plot where x is earnings_national and y is insta_1m with #000000, insta_3m with #A6A6A6
ggplot(activity_and_occurrences, aes(x = log(earnings_national), y = insta_6m, color = "#A6A6A6")) +
    geom_point() +
    geom_line() +
    geom_point(aes(y = insta_1m, color = "#000000")) +
    geom_line(aes(y = insta_1m, color = "#000000")) +
    geom_point(aes(y = insta_3m, color = "#595959")) +
    geom_line(aes(y = insta_3m, color = "#595959")) +
    labs(
        x = "Log of Earnings of national parties",
        y = "Instagram posts",
        title = "Earnings vs Instagram posts",
        color = ""
    ) +
    scale_color_manual(
        values = c("#000000", "#A6A6A6", "#595959"),
        labels = c("Instagram posts within 1 month of ED", "Instagram posts within 3 months of ED", "Instagram posts within 6 months of ED")
    ) +
    theme_minimal() +
    geom_text(
        aes(label = party),
        hjust = -.1,
        vjust = -.1,
        size = 3
    ) -> plot_insta_v_earnings

# Scatter plot where x is earnings_national and y is pr_1m with #000000, pr_3m with #A6A6A6
ggplot(activity_and_occurrences, aes(x = log(earnings_national), y = pr_6m, color = "#A6A6A6")) +
    geom_point() +
    geom_line() +
    geom_point(aes(y = pr_1m, color = "#000000")) +
    geom_line(aes(y = pr_1m, color = "#000000")) +
    geom_point(aes(y = pr_3m, color = "#595959")) +
    geom_line(aes(y = pr_3m, color = "#595959")) +
    labs(
        x = "Log of Earnings of national parties",
        y = "Press releases",
        title = "Earnings vs Press releases",
        color = ""
    ) +
    scale_color_manual(
        values = c("#000000", "#A6A6A6", "#595959"),
        labels = c("Press releases within 1 month of ED", "Press releases within 3 months of ED", "Press releases within 6 months of ED")
    ) +
    theme_minimal() +
    geom_text(
        aes(label = party),
        hjust = -.1,
        vjust = -.1,
        size = 3
    ) -> plot_pr_v_earnings

## Arrange the plots in a grid with ggarange
ggarrange(plot_insta_6m, plot_pr_6m, plot_insta_v_earnings, plot_pr_v_earnings, ncol = 2, nrow = 2)

## Calculate ratio between earnings_national and earnings_total and store them in share_national_total
activity_and_occurrences %>%
    mutate(
        share_national_on_total = earnings_national / earnings_total
    ) -> activity_and_occurrences

## Calculate summary statistics for share_national_on_total
summary(activity_and_occurrences$share_national_on_total)

## Standard deviation of share_national_on_total
var(activity_and_occurrences$share_national_on_total)
