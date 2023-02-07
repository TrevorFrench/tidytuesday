# Libraries
library(ggplot2)
library(dplyr)

big_tech_stock_prices <-
  readr::read_csv(
    'https://raw.githubusercontent.com/rfordatascience/tidytuesday/master/data/2023/2023-02-07/big_tech_stock_prices.csv'
  )
big_tech_companies <-
  readr::read_csv(
    'https://raw.githubusercontent.com/rfordatascience/tidytuesday/master/data/2023/2023-02-07/big_tech_companies.csv'
  )

# big_tech_stock_prices |> mutate("dollar_volume" = )
big_tech_stock_prices <-
  big_tech_stock_prices |> mutate(prev_close = lag(adj_close)) |>
  mutate(prev_ticker = lag(stock_symbol))

big_tech_stock_prices <-
  big_tech_stock_prices |> mutate(change = if_else(
    prev_ticker == stock_symbol,
    (big_tech_stock_prices$adj_close - big_tech_stock_prices$prev_close) / big_tech_stock_prices$prev_close,
    NA
  ))

# beg <- big_tech_stock_prices %>% 
#   filter(is.na(change))
beg_date <- '2018-02-07'
beg <- big_tech_stock_prices %>% 
  filter(date==beg_date)

big_tech_stock_prices <- big_tech_stock_prices %>% 
  filter(stock_symbol %in% c("AAPL", "MSFT", "GOOGL", "AMZN", "NVDA", "META")) |>
  filter(date>=beg_date)

prices = merge(x=big_tech_stock_prices,y=beg,by="stock_symbol")

prices <- prices |> mutate(beg_change = (prices$adj_close.x - prices$adj_close.y)/prices$adj_close.y)
# big_tech_stock_prices <-
#   big_tech_stock_prices |> mutate(beg_change = (big_tech_stock_prices$adj_close - beg[beg$stock_symbol == big_tech_stock_prices$stock_symbol,]$adj_close)/ beg[beg$stock_symbol == big_tech_stock_prices$stock_symbol,]$adj_close)



# # This would be a good steam chart
# ts <- big_tech_stock_prices %>% 
#   filter(!is.na(change))
# 
# # Plot
# ts %>%
#   ggplot(aes(x=date, y=change, group=stock_symbol, color=stock_symbol)) +
#   geom_line()

# Plot
prices %>%
  ggplot(aes(x=date.x, y=beg_change, group=stock_symbol, color=stock_symbol)) +
  geom_line()