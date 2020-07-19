library("highcharter")
data(diamonds, mpg, package = "ggplot2")

hchart(mpg, "scatter", hcaes(x = displ, y = hwy, group = class))

library("forecast")

airforecast <- forecast(auto.arima(AirPassengers), level = 95)

hchart(airforecast)

library("quantmod")

x <- getSymbols("^GSPC", auto.assign = FALSE)
y <- getSymbols("^DJI", auto.assign = FALSE)

highchart(type = "stock") %>% 
  hc_add_series(x) %>% 
  hc_add_series(y, type = "ohlc")

# map
data(unemployment)

hcmap("countries/us/us-all-all", data = unemployment,
      name = "Unemployment", value = "value", joinBy = c("hc-key", "code"),
      borderColor = "transparent") %>%
  hc_colorAxis(dataClasses = color_classes(c(seq(0, 10, by = 2), 50))) %>% 
  hc_legend(layout = "vertical", align = "right",
            floating = TRUE, valueDecimals = 0, valueSuffix = "%") 

# autocovariance, autocorrelation
x <- acf(diff(AirPassengers), plot = FALSE)
hchart(x)

# survival
library(survival)

data(lung)
lung <- mutate(lung, sex = ifelse(sex == 1, "Male", "Female"))
fit <- survfit(Surv(time, status) ~ sex, data = lung) 

hchart(fit, ranges = TRUE)

# principle components
hchart(princomp(USArrests, cor = TRUE))

# box plots
hc <- hcboxplot(x = diamonds$x, var = diamonds$color, var2 = diamonds$cut,
          outliers = FALSE) %>% 
  hc_chart(type = "column") # to put box vertical

# Economist Style
hc %>% hc_add_theme(hc_theme_economist())

# FiveThirtyEight Style
hc %>% hc_add_theme(hc_theme_538())
