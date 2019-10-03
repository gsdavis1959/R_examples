library(tsbox)
x.ts <- ts_c(mdeaths, austres)
x.ts
ts_df(x.ts)
ts_na_omit(ts_df(x.ts))


x.df <- ts_df(ts_c(mdeaths, austres))
ts_span(x.df, end = "1999-12-01", extend = TRUE)

x.df <- ts_df(ts_c(mdeaths, austres))
names(x.df) <- c("a fancy id name", "date", "count")
ts_plot(x.df)  # tsbox is fine with that
ts_default(x.df)

ts_summary(ts_c(mdeaths, austres))
ts_summary(austres)$id
ts_summary(austres)$start
