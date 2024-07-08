rm(list=ls())

library(tidyverse)
library(ggplot2)
library(stargazer)
library(readxl)
library(plotly)
library(forecast)
library(tseries)




# # -------------------------------------------------------------------------
# # Abstract ----------------------------------------------------------------
# 
# # -------------------------------------------------------------------------
# # Introduction ----------------------------------------------------------------
# 
# #Monetary policy is a phenomena that gets a lot of close attention as it is the
# #influencer of a broad spectrum of interest rates, which, in their turn, affect
# #the equity stock market, shareholders, and, hence, households' and individuals'
# #wealth. It is thus essential to know how to monitor the movements in interest 
# #rates and extract as much information as possible from their fluctuations.
# #Additionally, the stock market is a good ex-ante proxy for the economy that is
# #very responsive to the cost of borrowing money (i.e. the interest rate). Thus,
# #when setting interest rates central banks need to take into account maby facotrs such
# #as: 
# #1. How their decision will affect expectations,
# #2. What effect this change will have on the yield curve (shows the relationship
# #between the maturity (expiration horizon) of bonds (securities created by the 
# #government to borrow money) and the yield (return) that these securities will pay)
# #3. And how all of this will eventually affect the return on the stock market.
# 
# #This is important as,  by affecting seemingly different factors, it has an important
# #influence on macroeconomic predictions and decisions.
# 
# # -------------------------------------------------------------------------
# # Literature review ----------------------------------------------------------------
# 
# #The relationship between the interest rates and the equity market has been studied
# #extensively in many aspects. For my project, I tried to understand and replicate,
# #in part, the study of G.Faria and F.Verona of the yield curve and the stock market
# #where the authors have used frequeecy domain method (signal processing) with applications to
# #time series in finance(Faria and Verona 2020).
# #Fama and French (1989) had previously found that the returns
# #on the equity market are positively related to the slope of the yield curve...
# #but on the Italian stock market...
# 
# 
# # -------------------------------------------------------------------------
# # Methodology and Data  ----------------------------------------------------------------
# 
# 
# #To perform this exercise, I had to gather data for 4 indicators: the yield on medium-term
# #bonds (2 years) CTZ in Italy, data on long term-bonds (10 years) BTPs,
# #on the return on the Milan Borsa Italiana (Italian Stock Market, FTSE MIB), and the Euro
# #Interbank Offered rate for 3 months (EUR003M Index) as a proxy for the risk-free rate.
# #All the data was downloaded from the Bloomberg termianl.
# 
# #After the process of cleaning, I was able to analyse these indicators from January 2014
# #to September 2023.
# 
# #After gathering and cleaning these datasets, to calculate other variables for my 
# #analysis. The first one was the risk premium.
# #Usually, people engage with the stock market because they want to earn something above 
# #what is called the "risk-free rate". The latter is the reward that an investor gets
# #when using risk-free money instrument such as Treasury bills. Entering the stock
# #market incurs risks that are usually rewarded wit what is called the risk premium:
# #the difference between the return on the stock market and the risk free rate(
# #in this case:
# #FTSE MIB - Rfree.
# 
# #Then I had to calculate the term spread (TMS):
# 
# #BTP10 - 3mrate
# 
# #The term spread is useful to monitor the yield curve:
# #when the term spread is negative the Yield curve starts inverting, which means
# #the people's expectations about the further future are more pessimistic than 
# #what they expect to happen in the near future: the return on bonds of longer
# #maturities is less than the one on shorter maturities.
# 
# #Next, I calculated the term spreads within one type of bonds: CTZ (2 years) and
# #BTPs (10 years) for different widows: 2-4 months, 4-8 months, 8-16 months and 16-32 months.
# #This will essentially show the volatility of the difference between the different-term
# #expectations of the stability of the economy. 

# DATA -------------------------------------------------------------------
dat = read.csv('datDAFP.csv')

# SUMMARY -----------------------------------------------------------------
dat %>% select(-c(X, X.1)) %>% stargazer(type = 'text')

#I analysed the spreads
#and tried to see whether there were trends.I also

# PLOTS --------------------------------------------------------------------

dat %>% 
  plot_ly(x = ~DATE, y = ~rpremia, name = 'Risk Premia', type = "scatter", mode = "lines", visible = "legendonly")  %>%
  add_trace(y = ~tms, name = 'Term Spread', mode = 'lines+markers', visible = "legendonly") %>%
  add_trace(y = ~spread_2_4_mt, name = 'MT 2-4 spread', mode = 'lines+markers', visible = "legendonly") %>%
  add_trace(y = ~spread_4_8_mt, name = 'MT 4-8 spread', mode = 'lines+markers', visible = "legendonly") %>%
  add_trace(y = ~spread_8_16_mt, name = 'MT 8-16 spread', mode = 'lines+markers', visible = "legendonly") %>%
  add_trace(y = ~spread_16_32_mt, name = 'MT 16-32 spread', mode = 'lines+markers', visible = "legendonly") %>%
  add_trace(y = ~spread_2_4_lt, name = 'LT 2-4 spread', mode = 'lines+markers', visible = "legendonly") %>%
  add_trace(y = ~spread_4_8_lt, name = 'LT 4-8 spread', mode = 'lines+markers', visible = "legendonly") %>%
  add_trace(y = ~spread_8_16_lt, name = 'LT 8-16 spread', mode = 'lines+markers', visible = "legendonly") %>%
  add_trace(y = ~spread_16_32_lt, name = 'LT 16-32 spread', mode = 'lines+markers', visible = "legendonly")


# METHODOLOGY ----------------------------------------------------------------

#When examining the paper, I was able to identify 2 approaches that suited the 
#purpose of this project and my level of preparation. First, I identified that
#the correlation between all my variables was very weak:

dat %>% select(-c(X, X.1, DATE)) %>% cor() %>% stargazer(type = 'text')

#I also ran a linear regression which did not show any statistically significant results:

# OLS ---------------------------------------------------------------------

lm(rpremia ~ tms, data = dat) %>% stargazer(type = 'text')
lm(rpremia ~ spread_2_4_mt, data = dat) %>% stargazer(type = 'text')
lm(rpremia ~ spread_4_8_mt, data = dat) %>% stargazer(type = 'text')
lm(rpremia ~ spread_8_16_mt, data = dat) %>% stargazer(type = 'text')
lm(rpremia ~ spread_16_32_mt, data = dat) %>% stargazer(type = 'text')
lm(rpremia ~ spread_2_4_lt, data = dat) %>% stargazer(type = 'text')
lm(rpremia ~ spread_4_8_lt, data = dat) %>% stargazer(type = 'text')
lm(rpremia ~ spread_8_16_lt, data = dat) %>% stargazer(type = 'text')
lm(rpremia ~ spread_16_32_lt, data = dat) %>% stargazer(type = 'text')


model = lm(rpremia ~ spread_2_4_mt + spread_4_8_mt + spread_8_16_mt + spread_16_32_mt +
             spread_2_4_lt + spread_4_8_lt + spread_8_16_lt + spread_16_32_lt, data = dat) %>% stargazer(type = 'text')

# Campbell and Thompson (2008) argue, however, that a monthly R2 of about 0.5% represents
# an economically relevant degree of return predictability. The monthly R2 s of the statistically
# significant predictors are indeed slightly above that threshold.

# add more? ---------------------------------------------------------------





  




# ARIMA -------------------------------------------------------------------



# However, after several attempts of working with the dataset, I identified that 
# an approach suited for time series would be a better choice for my case. Thus, I have
#run the time series using the ARIMA (Auto Integrated Moving Average) model.

#To being with, I plotted my data over time. I decided to do this with two variables:
#risk premium and the term spread. This choice is dictated by the fact that the risk premium is
#what I am trying to study and estimate, whereas the term spread (3 months - 10 years) is the 
#main proxy for the yield curve slope and the main indicator of expectations.

dat$rpremia %>% plot.ts()
dat$tms %>% plot.ts()

#Consequently, I checked whether my data was stationary, i.e. whether my data has a
#constant mean, variance, and variance over time.  I  did this using the Auto Correlated function (ACF):

Acf(dat$rpremia, main = "Risk premium")
Acf(dat$tms, main = "Term Spread")

#There is no correlation in withing the risk premium, however, the terms spread is
#highly auto correlated and its, thus, not stationary.

Pacf(dat$rpremia, main = "Risk premium")
Pacf(dat$tms, main = "Term Spread")

forecast(dat$rpremia)
forecast(dat$tms)

Arima(dat$rpremia, order = c(2, 0, 0))
Arima(dat$rpremia, order = c(2, 0, 0)) %>% forecast(h = 12) %>% plot()

Arima(dat$tms, order = c(1, 1, 0))
Arima(dat$tms, order = c(1, 1, 0)) %>% forecast(h = 12) %>% plot()

smodel = auto.arima(tms, ic = 'aic', trace = TRUE)

# ARIMA with Agabek -------------------------------------------------------------------








# Spreads -----------------------------------------------------------------
dat$spread_2_4_mt %>% plot.ts()

Acf(dat$spread_2_4_mt, main = "dlkgjhd")
Pacf(dat$spread_2_4_mt)

forecast(dat$spread_2_4_mt)
Arima(dat$spread_2_4_mt, order = c(2, 0, 0))
Arima(dat$spread_2_4_mt, order = c(2, 0, 0)) %>% forecast(h = 12) %>% plot()



# 4-8 ---------------------------------------------------------------------


dat$spread_2_4_lt %>% plot.ts()

Acf(dat$spread_2_4_lt, main = "dlkgjhd")
Pacf(dat$spread_2_4_lt)

forecast(dat$spread_2_4_lt)
Arima(dat$spread_2_4_lt, order = c(2, 0, 0))
Arima(dat$spread_2_4_lt, order = c(2, 0, 0)) %>% forecast(h = 12) %>% plot()