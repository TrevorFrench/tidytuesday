library(tidyverse)

# Read in the data
bob_ross <- read_csv(
  "https://raw.githubusercontent.com/jwilber/Bob_Ross_Paintings/master/data/bob_ross_paintings.csv",
) 

glimpse(bob_ross)

# The first column doesn't contain data that we need, so we can remove it
bob_ross <- select(bob_ross, -1)

# Several columns refer to presence/absence of named colors.
bob_ross <- bob_ross |> 
  mutate(
    across(Black_Gesso:Alizarin_Crimson, as.logical)
  )

# Save the data.
write_csv(
  bob_ross,
  here::here(
    "2023", "week-08", "data",
    "bob_ross.csv"
  )
)