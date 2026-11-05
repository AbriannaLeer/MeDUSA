library(tidyr)
library(dplyr)

# Feature id has duplicates (NA not shown) so changed to wide format to see the actual data

df <- read.csv("data/abrianna/positive_pascal.csv", stringsAsFactors = FALSE)
df2 <- read.csv("data/abrianna/negative_pascal.csv", stringsAsFactors = FALSE)

wide <- df |>
  select(mz, sample_name, intensity) |>
  pivot_wider(
    names_from  = sample_name,
    values_from = intensity,
    values_fn   = sum,
    values_fill = NA
  )

wide2 <- df2 |>
  select(mz, sample_name, intensity) |>
  pivot_wider(
    names_from  = sample_name,
    values_from = intensity,
    values_fn   = sum,
    values_fill = NA
  )

write.csv(wide, "data/abrianna/positive_pascal_wide.csv", row.names = FALSE)
write.csv(wide2, "data/abrianna/negative_pascal_wide.csv", row.names = FALSE)