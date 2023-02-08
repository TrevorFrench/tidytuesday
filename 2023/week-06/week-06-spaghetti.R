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

big_tech_stock_prices <-
  big_tech_stock_prices |> mutate(prev_close = lag(adj_close)) |>
  mutate(prev_ticker = lag(stock_symbol))

big_tech_stock_prices <-
  big_tech_stock_prices |> mutate(change = if_else(
    prev_ticker == stock_symbol,
    (
      big_tech_stock_prices$adj_close - big_tech_stock_prices$prev_close
    ) / big_tech_stock_prices$prev_close,
    NA
  ))


beg_date <- '2012-05-18'
end_date <- '2022-12-29'
beg <- big_tech_stock_prices %>%
  filter(date == beg_date)

beg$color <-
  c(
    '#737373',
             '#ed2224',
             '#f99000',
             '#00a1e0',
             '#049fd9',
             '#fa3913',
             '#1f70c1',
             '#0068b5',
             '#0265e0',
             '#00a1f1',
             '#d81f26',
             '#76b900',
             '#c74634',
             '#e82127'
  )

big_tech_stock_prices <- big_tech_stock_prices %>%
  filter(date >= beg_date) |>
  filter(date <= end_date)

prices = merge(x = big_tech_stock_prices, y = beg, by = "stock_symbol")

prices <-
  prices |> mutate(beg_change = (prices$adj_close.x - prices$adj_close.y) /
                     prices$adj_close.y)

tmp <- prices %>%
  mutate(ticker = stock_symbol)

p <- tmp %>%
  ggplot(aes(x = date.x, y = adj_close.x)) +
  geom_line(
    data = tmp %>% dplyr::select(-stock_symbol),
    aes(group = ticker),
    color = "grey",
    size = 0.5,
    alpha = 0.5
  ) +
  geom_line(aes(color = stock_symbol),
            color = tmp$color,
            size = 1.2) +
  # geom_label(aes(label = round((beg_change * 100), digits = 2), colour = color, x = date.x, y = 450)) +
  theme(
    legend.position = "none",
    plot.title = element_text(size = 14),
    panel.grid = element_blank(),
    axis.text.x=element_blank(),
    axis.ticks.x=element_blank(),
    axis.text.y=element_blank(),
    axis.ticks.y=element_blank(),
    axis.title.x = element_blank(),
    axis.title.y = element_blank()
  ) +
  ggtitle("10+ years of big tech performance") +
  facet_wrap( ~ paste(stock_symbol,))

p