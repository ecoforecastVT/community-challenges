# Olympic Medals CEF Challenge
# February 2026
# Cayelan Carey

# install.packages(c("readr","dplyr"))  # if needed
library(readr)
library(dplyr)

# 1) Read the CSV file with predictions (exported from google form)
medals_raw <- read_csv(
  "olympicmedals2026.csv",
  show_col_types = FALSE,
)

# 2) Clean + ensure numeric medal columns
medals <- medals_raw %>%
  # Keep only rows that have all three medal numbers present
  filter(
    !is.na(goldmedal) & !is.na(silvermedal) & !is.na(bronzemedal)
  ) %>%
  mutate(
    goldmedal   = suppressWarnings(as.numeric(goldmedal)),
    silvermedal = suppressWarnings(as.numeric(silvermedal)),
    bronzemedal = suppressWarnings(as.numeric(bronzemedal))
  )

# 3) function to calculate summed absolute bias for each entry
summed_abs_bias <- function(df, G, S, B) {
  df %>%
    mutate(
      summed_abs_bias = abs(goldmedal - G) +
        abs(silvermedal - S) +
        abs(bronzemedal - B)
    )
}

# Put in updated metals totals
results <- summed_abs_bias(medals, G = 11, S = 12, B = 9)
#update the results as Olympics progress

# Sort by best (lowest) absolute summed bias bias
results_sorted <- results %>% arrange(summed_abs_bias)

# Print results
print(results_sorted %>% select(Name, goldmedal, silvermedal, bronzemedal, summed_abs_bias))
