# Libraries ---------------------------------------------------------------

library(tidyverse)
library(ggplot2)
library(stargazer)
library(readxl)
library(plotly)
library(forecast)
library(tseries)


# -------------------------------------------------------------------------
#Index -----------------------------------------------------------------

mib = read_excel('mib.xlsx')
mib$date %>% class

mib = na.omit(mib)

mib$date = as.Date(mib$date)
mib$date %>% class

mib$year = year(mib$date)
mib$month = day(mib$date)
mib$day = '01'

mib$month = str_pad(mib$month, 2, pad = '0')

paste(mib$year, mib$month, mib$day, sep = '-')

mib$DATE = as.Date(paste(mib$year, mib$month, mib$day, sep = '-'))

mib = mib %>% arrange(DATE)

mib$return %>% class

mib = mib %>% mutate(
  return_2 = ((return/lag(return, n=1))-1)*100)


# R premia ----------------------------------------------------------------

mib$rpremia = mib$`return_2` - mib$`3mrate`

# Plot mib ---------------------------------------------------

mib %>% plot_ly(x = ~DATE, y = ~return_2, type = "scatter", mode = "lines")

mib = mib %>% select(c(DATE, return_2, rpremia, `3mrate`))

write.csv(mib, file = "mibDAFP.csv")

# -------------------------------------------------------------------------
# ARIMA -------------------------------------------------------------------

dat$rpremia %>% plot.ts()

Acf(dat$rpremia)
Pacf(dat$rpremia)

forecast(dat$rpremia)
Arima(dat$rpremia, order = c(2, 0, 0))

# -------------------------------------------------------------------------
# MT SPREADS --------------------------------------------------------------------

ctz = read_excel("ctz.xls")

ctz$observation_date %>% class()
ctz = ctz %>% rename(date = observation_date)

ctz$year = year(ctz$date)
ctz$month = month(ctz$date)
ctz$day = '01'

ctz$month = str_pad(ctz$month, 2, pad = '0')
paste(ctz$year, ctz$month, ctz$day, sep = '-')
ctz$DATE = as.Date(paste(ctz$year, ctz$month, ctz$day, sep = '-'))

ctz = ctz %>% arrange(DATE)
ctz = ctz %>% rename(yield_mt = `INTGSTITM193N`)
ctz$yield_mt = as.numeric(ctz$yield_mt)
ctz$yield_mt %>% class

ctz = ctz[seq(dim(ctz)[1],1),]
ctz = ctz %>% select (c(DATE, yield_mt))

ctz %>% plot_ly(x = ~DATE, y = ~yield_mt, type = "scatter", mode = "lines")

# ARIMA -------------------------------------------------------------------
# attach(ctz)
# plot.ts(yield_mt)
# adf.test(yield_mt)
# 
# rn_yield_mt = diff(log(yield_mt))
# plot.ts(rn_yield_mt)
# ctz = ctz %>% na.omit
# adf.test(rn_yield_mt)
# 
# auto.arima(rn_yield_mt)
# modelarima = arima(rn_yield_mt, order = c(2, 0, 0))
# modelarima
# #rn_yield_mt = 0.0091 + (-0.2515)rn_yield_mt(t-1)+0.2411rn_yield_mt(t-2)
# 
# et = residuals(modelarima)
# acf(et)
# plot.ts(et)
# forecastrn = forecast(rn_yield_mt, h=10)
# forecastrn
# plot(forecastrn)
# 
# 

# spread 2-4  ------------------------------------------------------------------

# ARIMA -------------------------------------------------------------------


ctz$spread_2_4_mt = NA

for (i in 1:(nrow(ctz) - 4)) {
  ctz$spread_2_4_mt[i] = ctz$yield_mt[i + 4] - ctz$yield_mt[i + 2]
}


# spread 4-8  ------------------------------------------------------------------


ctz$spread_4_8_mt = NA

for (i in 1:(nrow(ctz) - 8)) {
  ctz$spread_4_8_mt[i] = ctz$yield_mt[i + 8] - ctz$yield_mt[i + 4]
}


# spread 8-16  ------------------------------------------------------------------

ctz$spread_8_16_mt = NA

for (i in 1:(nrow(ctz) - 16)) {
  ctz$spread_8_16_mt[i] = ctz$yield_mt[i + 16] - ctz$yield_mt[i + 8]
}


# spread 16-32  ------------------------------------------------------------------

ctz$spread_16_32_mt = NA

for (i in 1:(nrow(ctz) - 32)) {
  ctz$spread_16_32_mt[i] = ctz$yield_mt[i + 32] - ctz$yield_mt[i + 16]
}

# save --------------------------------------------------------------------

write.csv(ctz, file = "ctzDAFP.csv")



# -------------------------------------------------------------------------
# LT SPREADS --------------------------------------------------------------------
btp10 = read_excel('10yr.xlsx')
btp10$`Observation date` %>% class()
btp10$`Observation date` = as.Date(btp10$`Observation date`)
btp10 = btp10 %>% rename(date = `Observation date`)

btp10$year = year(btp10$date)
btp10$month = month(btp10$date)
btp10$day = '01'

btp10$month = str_pad(btp10$month, 2, pad = '0')
paste(btp10$year, btp10$month, btp10$day, sep = '-')
btp10$DATE = as.Date(paste(btp10$year, btp10$month, btp10$day, sep = '-'))

btp10 = btp10 %>% arrange(DATE)

btp10 = btp10 %>% rename(yield_lt = `Gross yield of benchmark 10-year BTP`)
btp10$yield_lt = as.numeric(btp10$yield_lt)
btp10$yield_lt %>% class

btp10 = btp10[seq(dim(btp10)[1],1),]
btp10 = btp10 %>% select (c(DATE, yield_lt))

btp10 %>% plot_ly(x = ~DATE, y = ~yield_lt, type = "scatter", mode = "lines")


# spread 2-4  ------------------------------------------------------------------

btp10$spread_2_4_lt = NA

for (i in 1:(nrow(btp10) - 4)) {
  btp10$spread_2_4_lt[i] = btp10$yield_lt[i + 4] - btp10$yield_lt[i + 2]
}

# spread 4-8  ------------------------------------------------------------------


btp10$spread_4_8_lt = NA

for (i in 1:(nrow(btp10) - 8)) {
  btp10$spread_4_8_lt[i] = btp10$yield_lt[i + 8] - btp10$yield_lt[i + 4]
}


# spread 8-16  ------------------------------------------------------------------

btp10$spread_8_16_lt = NA

for (i in 1:(nrow(btp10) - 16)) {
  btp10$spread_8_16_lt[i] = btp10$yield_lt[i + 16] - btp10$yield_lt[i + 8]
}


# spread 16-32  ------------------------------------------------------------------

btp10$spread_16_32_lt = NA

for (i in 1:(nrow(btp10) - 32)) {
  btp10$spread_16_32_lt[i] = btp10$yield_lt[i + 32] - btp10$yield_lt[i + 16]
}


# save --------------------------------------------------------------------

write.csv(btp10, file = "btp10DAFP.csv")



# -------------------------------------------------------------------------
# Merging -----------------------------------------------------------------

mib = read.csv("mibDAFP.csv")
ctz = read.csv("ctzDAFP.csv")
btp10 = read.csv("btp10DAFP.csv")


# merging -----------------------------------------------------------------
data = left_join(mib, ctz, by = 'DATE')
dat = left_join(data, btp10, by = 'DATE')


# NAs ---------------------------------------------------------------------

dat = dat %>% na.omit()


# selecting ---------------------------------------------------------------
dat %>% summary
dat = dat %>% select(-c(`X.x`, `X.y`, `X`))



# save --------------------------------------------------------------------

write.csv(dat, file = "datDAFP.csv")

