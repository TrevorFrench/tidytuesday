# Libraries
library(ggplot2)
library(dplyr)
library(grid)

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
beg_date <- '2019-12-30'
end_date <- '2022-12-29'
beg <- big_tech_stock_prices %>%
  filter(date==beg_date)

big_tech_stock_prices <- big_tech_stock_prices %>% 
  filter(stock_symbol %in% c("AAPL", "MSFT", "GOOGL", "AMZN", "NVDA", "META")) |>
  filter(date>=beg_date) |>
  filter(date<=end_date)

prices = merge(x=big_tech_stock_prices,y=beg,by="stock_symbol")

prices <- prices |> mutate(beg_change = (prices$adj_close.x - prices$adj_close.y)/prices$adj_close.y)

temp <- subset(prices, date.x == end_date)
temp['position'] <- c(2.5, .4, .75, -.2, 1.75, 2)

# Plot
p <- prices %>%
  # remove the group, color label for a cool visual
  ggplot(aes(x=date.x, y=beg_change, group=stock_symbol, color=stock_symbol, label=stock_symbol)) +
  geom_line() +
  geom_label(data = temp, aes(label = paste(stock_symbol,",", round((beg_change * 100), digits = 2), sep = ""), colour = stock_symbol, x = date.x, y = position), hjust = -.1, check_overlap = TRUE) +
  scale_x_date(expand=c(0, 180)) +
  theme(legend.position="none") + 
  scale_color_manual(values=c('#737373', '#f99000', '#fa3913' ,'#0265e0', '#00a1f1', '#76b900'))
# p + guides(color = FALSE, size = FALSE)
# Code to turn off clipping
gt <- ggplotGrob(p)
gt$layout$clip[gt$layout$name == "panel"] <- "off"
grid.draw(gt)