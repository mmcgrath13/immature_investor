cat("\014") # Clear your console
rm(list = ls()) #clear your environment

########################## Load in header file ######################## #

setwd("~/Desktop/immature_investor/post02")

########################## Load in Libraries ########################## #

library(readr)
library(ggplot2)
library(scales)
library(lubridate)
library(stringr)
library(ggrepel)
library(zoo)
library(Hmisc)
library(igraph)
library(lemon)
library(tidyverse)
library(quantmod)
library(PerformanceAnalytics)
library(xts)
library(tidyr)
library(tidyquant)
library(PMwR)
library(gridExtra)

########################## Load Data Here ######################### #

########################## Start Program Here ######################### #

# set up plot theme for graphs
mike_mcgrath_theme = theme(plot.title = element_text(hjust = 0.5,
                                                     size = 17,
                                                     face = "bold"),
                           plot.subtitle = element_text(hjust = 0.5,
                                                        size = 13),
                           panel.background = element_rect(fill = "white"),
                           plot.caption = element_text(hjust = 0),
                           axis.line.x.bottom = element_line(colour = "#999999"),
                           axis.text = element_text(colour = "black"),
                           axis.ticks.x = element_line(colour = "#999999"),
                           axis.ticks.length.x = unit(.20,
                                                      "cm"),
                           axis.ticks.y = element_blank(),
                           legend.position = "none",
                           panel.grid.major.y = element_line(colour = "#CCCCCC"))

# get index data
sp500_data = getSymbols.yahoo("^GSPC",
                              from = "1928-01-01",
                              to = "2019-12-31",
                              auto.assign = FALSE)
sp500 = sp500_data[ ,6]

dow_data = getSymbols.yahoo("^DJI",
                              from = "1928-01-01",
                              to = "2019-12-31",
                              auto.assign = FALSE)
dow = dow_data[ ,6]

# merge data
data = cbind(sp500, dow)
colnames(data) = c("sp500", "dow")

# yearly returns
daily_return = returns(sp500)
returns(sp500, period = "year")

returns(dow)
returns(dow, period = "year")

vignette("FinTeX", package = "PMwR")
returns = data.frame(returns(data, period = "year"))
barplot(returns$sp500)

# plot yearly returns sp500
returns.df = function(returns) {
  df = data.frame(x = index(returns), coredata(returns))
  colnames(df) = c("Date", colnames(returns))
  return(df)
}
returns = returns.df(returns)

returns$color = ifelse(returns$sp500 > 0, "green", "red") # set color of bar

# plot
source_string = paste0("Source: Yahoo, 1929-2019")
note_string = paste0("Note: S&P 500 data is based on ticker ^GSPC which is a price 
index and does not include dividends.")

returns %>% 
  ggplot(aes(x = Date, y = sp500)) +
  geom_bar(stat = "identity", 
           aes(fill = color)) +
  scale_fill_manual(values = c("green", "red")) +
  scale_y_continuous(labels = percent) +
  labs(title = "S&P 500 Index Yearly Returns",
       subtitle = "1929-2019",
       x = "",
       y = "Percent Return",
       caption = paste0("\n", source_string, "\n", note_string)) +
  mike_mcgrath_theme +
  theme(axis.ticks.x = element_blank(),
        axis.text.x = element_blank(),
        axis.line.x.bottom = element_blank())

# annual returns
returns(data, period = "ann")

# sp500 drawdowns
dd = drawdowns(sp500)
dd_groups = dd[order(dd$max, decreasing = TRUE) [1:30], ] # six worst sorted by size


# plot returns five years after bottom
# get windows needed 
t1 = window(sp500$GSPC.Adjusted, start = "1932-06-01", end = "1937-06-22")
t2 = window(sp500$GSPC.Adjusted, start = "2002-10-09", end = "2007-10-09")
t3 = window(sp500$GSPC.Adjusted, start = "2009-03-09", end = "2014-03-09")

t1.df = function(t1) {
  df = data.frame(x = index(t1), coredata(t1))
  colnames(df) = c("date", "value1")
  return(df)
}
t1 = t1.df(t1)

t2.df = function(t2) {
  df = data.frame(x = index(t2), coredata(t2))
  colnames(df) = c("date", "value2")
  return(df)
}
t2 = t2.df(t2)

t3.df = function(t3) {
  df = data.frame(x = index(t3), coredata(t3))
  colnames(df) = c("date", "value3")
  return(df)
}
t3 = t3.df(t3)

# merge time periods 
t1$day = t1 %>% mutate(count = row_number())
t1 = t1$day

t2$day = t2 %>% mutate(count = row_number())
t2 = t2$day

t3$day = t3 %>% mutate(count = row_number())
t3 = t3$day

prices_after = data.frame(t1$value1, t2$value2, t3$value3)
colnames(prices_after) = c("t1", "t2", "t3")
head(prices_after)
plot.ts(prices_after)

# find period returns
t1_return = window(return, start = "1932-06-01", end = "1937-06-22")
t2_return = window(return, start = "2002-10-09", end = "2007-10-09")
t3_return = window(return, start = "2009-03-09", end = "2014-03-09")

t1_return.df = function(t1_return) {
  df = data.frame(x = index(t1_return), coredata(t1_return))
  colnames(df) = c("date", "return1")
  return(df)
}
t1_return = t1_return.df(t1_return)

t2_return.df = function(t2_return) {
  df = data.frame(x = index(t2_return), coredata(t2_return))
  colnames(df) = c("date", "return2")
  return(df)
}
t2_return = t2_return.df(t2_return)

t3_return.df = function(t3_return) {
  df = data.frame(x = index(t3_return), coredata(t3_return))
  colnames(df) = c("date", "return3")
  return(df)
}
t3_return = t3_return.df(t3_return)

# merge return time periods 
t1_return$day = t1_return %>% mutate(count = row_number())
t1_return = t1_return$day

t2_return$day = t2_return %>% mutate(count = row_number())
t2_return = t2_return$day

t3_return$day = t3_return %>% mutate(count = row_number())
t3_return = t3_return$day

returns_after = data.frame(t1_return$return1, t2_return$return2, t3_return$return3)
colnames(returns_after) = c("t1", "t2", "t3")
head(returns_after)

# calculate cumulative returns
cum_returns = returns_after %>%
  mutate(cr = cumprod(1 + returns_after)) %>%      # using the cumprod function
  mutate(cumulative_returns = cr - 1)

cr  = cumprod(1 + returns_after)
cum_returns = cr -1
head(cum_returns)

cum_returns$day = seq.int(nrow(cum_returns))
as.factor(cum_returns$day)

plot.ts(cum_returns)

total_return = Return.cumulative(returns_after) # total return during periods

# plot
source_string = paste0("Source: Yahoo")
note_string = paste0("Note: S&P 500 data is based on ticker ^GSPC which is a price index 
and does not include dividends.")

cum_returns %>%
  ggplot(aes(x = day / 251.8)) +
  geom_line(aes(y = t1), color = "green") +
  geom_line(aes(y = t2), color = "blue") +
  geom_line(aes(y = t3), color = "purple") +
  scale_y_continuous(labels = percent) +
  labs(title = "Large Declines Lead to Larger Recoveries",
       x = "Years since bottom",
       y = "Percent Gain",
       caption = paste0("\n", source_string, "\n", note_string)) +
  mike_mcgrath_theme  

# simulated growth of $1000
# initial capital
start.capital = 0

# investment returns
annual.mean.return = .09
annual.ret.std.dev = 0

# inflation adjustment
annual.inflation = .02
annual.inf.std.dev = 0 

# salary 
salary = 50000
salary.inflation = salary * (1 + annual.inflation)

# contributions
monthly.contributions = 500

# withdrawals
monthly.withdrawals = 0

# number of observations (in years)
n.obs = 30

# number of simulations
n.sim = 1

# simulation
# number of months to simulate
n.months = 12 * n.obs

# monthly investment and inflation assumptions
monthly.mean.return = annual.mean.return / 12
monthly.ret.std.dev = annual.ret.std.dev / sqrt(12)

monthly.inflation = annual.inflation / 12
monthly.inf.std.dev = annual.inf.std.dev / sqrt(12)


# simulate returns
monthly.invest.returns = matrix(0, n.months, n.sim)
monthly.inflation.returns = matrix(0, n.months, n.sim)

monthly.invest.returns[] = rnorm(n.months * n.sim, mean = monthly.mean.return, sd = monthly.ret.std.dev)
monthly.inflation.returns[] = rnorm(n.months * n.sim, mean = monthly.inflation, sd = monthly.inf.std.dev)

# simulate nav 
nav = data.frame(start.capital, n.months + 1, n.sim)
for (i in 1:n.months) {
  nav[i + 1, ] = nav[i, ] * (1 + monthly.invest.returns[i, ] - monthly.inflation.returns[i, ]) +
    monthly.contributions - monthly.withdrawals
}

nav$month = seq.int(nrow(nav))

# simulate invested capital
inv_cap = data.frame(start.capital, n.months + 1, n.sim)
for (i in 1:n.months) { 
  inv_cap[i + 1, ] = inv_cap[i, ] + monthly.contributions - monthly.withdrawals 
}

nav$inv.cap = inv_cap$start.capital

# plot
source_string = paste0("Source: 2020 (theimmatureinvestor.wordpress.com)")
note_string = paste0("Note: For simplicity, there is no variation in returns.")

p2 = nav %>% 
  ggplot(aes(x = month / 12)) +
  geom_line(aes(y = start.capital), color = "green") +
  geom_ribbon(aes(ymin = 0, ymax = start.capital), fill = "green") +
  geom_line(aes(y = inv.cap), color = "blue") + 
  geom_ribbon(aes(ymin = 0, ymax = inv.cap), fill = "blue") + 
  scale_y_continuous(labels = dollar,
                     breaks = c(0, 500000, 1000000, 1500000)) +
  labs(title = "30 Years of Compound Returns",
       subtitle = "$500 invested every month at 7%",
       x = "Years",
       y = "",
       caption = paste0("\n", source_string, "\n", note_string)) +
  mike_mcgrath_theme +
expand_limits(x = c(0,30), y = c(0, 1500000))

grid.arrange(p1, p2, ncol = 1)
