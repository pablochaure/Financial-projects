# The Libraries that we need
library(tidyquant)
library(tidyverse)
library(matrixStats)

tickets <- c("SPY", "IEF", "GLD", "TLT", "KO", "AAPL", "IBM", "T", "WMT")
n_assets_buy <- 3
n_assets_sell <- 3
momentum_tf <- 30

# DB of Returns
db_global <- tickets %>%
tq_get(from = Sys.Date() - lubridate::years(10),
to = Sys.Date(),
get = "stock.prices",
complete_cases = TRUE) %>%
dplyr::group_by(symbol) %>%
dplyr::mutate(Ret = ROC(adjusted, n = 1, type = "discrete"),
Momentum = ROC(adjusted, n = momentum_tf, type = "discrete")) %>%
dplyr::ungroup()

# Splitting the data according to the type
db_Ret <- db_global %>%
dplyr::select(date, symbol, Ret) %>%
spread(symbol, Ret) %>%
replace(is.na(.), 0)

db_Momentum <- db_global %>%
dplyr::select(date, symbol, Momentum) %>%
spread(symbol, Momentum) %>%
na.omit()

# Analyzing the Momentum
asset_ranking <- db_Momentum %>%
dplyr::select(-date) %>%
as.matrix() %>%
rowRanks(ties.method = "min") %>%
as.data.frame() %>%
dplyr::mutate(date = db_Momentum$date) %>%
dplyr::select(date, everything()) %>%
purrr::set_names(c("date", tickets)) %>%
as_tibble()

# Analyzing Sector Selection
asset_selection <- asset_ranking %>%
mutate_if(is.numeric, ~ + ( . > (length(tickets) - n_assets_buy))) %>%
dplyr::select(-date)

asset_selection <- asset_selection * (1/n_assets_buy)

asset_selection <- asset_selection %>%
dplyr::mutate(date = db_Momentum$date) %>%
dplyr::select(date, everything()) %>%
dplyr::mutate(Rebalance = ifelse(dplyr::lead(lubridate::month(date), n = 1) != lubridate::month(date), "Yes", "No")) %>%
dplyr::filter(Rebalance == "Yes") %>%
dplyr::select(-Rebalance)

# Portfolio Simulations
db_portfolio <- PerformanceAnalytics::Return.portfolio(db_Ret %>%
column_to_rownames(var = "date") %>%
as.xts(),
weights = asset_selection %>%
column_to_rownames(var = "date") %>%
as.xts(),
geometric = TRUE,
rebalance_on = NA,
verbose = TRUE)

plot(cumsum(db_portfolio[["returns"]]), type = "l")
