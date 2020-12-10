library(prophet)
df <- read.csv('https://github.com/gumdropsteve/datasets/raw/master/views.csv')
df
names(df) <- c('ds', 'y')

m <- prophet(df)
future <- make_future_dataframe(m, periods=365)
tail(future)
forecast <- predict(m, future)
tail(forecast)
plot(m, forecast)
tail(forecast[c('ds', 'yhat', 'yhat_lower', 'yhat_upper')])

prophet_plot_components(m, forecast)

# cross validate
df.cv <- cross_validation(m, initial=180, period=60, horizon=120, units='days')
tail(df.cv)
plot(df.cv)

plot_cross_validation_metric(df.cv, metric='mape')
