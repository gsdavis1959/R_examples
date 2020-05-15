library(CausalImpact)
library(pageviews)

disclosure_y_pageviews <- article_pageviews(article = "Disclosure_(band)", start = "2015100100", end = "2015112100")
disclosure_x1_pageviews <- article_pageviews(article = "Rudimental", start = "2015100100", end = "2015112100")
disclosure_x2_pageviews <- article_pageviews(article = "Bondax", start = "2015100100", end = "2015112100")
disclosure_x3_pageviews <- article_pageviews(article = "Flume (musician)", start = "2015100100", end = "2015112100")

sia_y_pageviews <- article_pageviews(article = "Sia", start = "2015100100", end = "2015111400")
sia_x1_pageviews <- article_pageviews(article = "Dragonette", start = "2015100100", end = "2015111400")
sia_x2_pageviews <- article_pageviews(article = "PJ Harvey", start = "2015100100", end = "2015111400")
sia_x3_pageviews <- article_pageviews(article = "Serena Ryder", start = "2015100100", end = "2015111400")

sia_y_pageviews[1,]

time.points <- seq.Date(as.Date("2015-10-01"), by = 1, length.out = nrow(disclosure_y_pageviews))
disclosure_data <- zoo(cbind(disclosure_y_pageviews["views"], disclosure_x1_pageviews["views"],
                             disclosure_x2_pageviews["views"], disclosure_x3_pageviews["views"]), time.points)
colnames(disclosure_data) <- c("y", "x1", "x2", "x3")
matplot(disclosure_data, type = "l")

time.points <- seq.Date(as.Date("2015-10-01"), by = 1, length.out = nrow(sia_y_pageviews))
sia_data <- zoo(cbind(sia_y_pageviews["views"], sia_x1_pageviews["views"], sia_x2_pageviews["views"], 
                      sia_x3_pageviews["views"]), time.points)
colnames(sia_data) <- c("y", "x1", "x2", "x3")
matplot(sia_data, type = "l")

# time series
pre.period <- as.Date(c("2015-10-01", "2015-11-14"))
post.period <- as.Date(c("2015-11-16", "2015-11-21"))
disclosure_impact1 <- CausalImpact(disclosure_data, pre.period, post.period)

pre.period <- as.Date(c("2015-10-01", "2015-11-13"))
post.period <- as.Date(c("2015-11-14", "2015-11-21"))
disclosure_impact2 <- CausalImpact(disclosure_data, pre.period, post.period)

pre.period <- as.Date(c("2015-10-01", "2015-11-07"))
post.period <- as.Date(c("2015-11-09", "2015-11-14"))
sia_impact1 <- CausalImpact(sia_data, pre.period, post.period)

pre.period <- as.Date(c("2015-10-01", "2015-11-06"))
post.period <- as.Date(c("2015-11-07", "2015-11-14"))
sia_impact2 <- CausalImpact(sia_data, pre.period, post.period)

# plot
options(warn=-1) # suppress warnings from geom_path about missing row values
plot(disclosure_impact1)
plot(disclosure_impact2)
plot(sia_impact1)
plot(sia_impact2)
options(warn=0) # restore warnings

# summaries
summary(disclosure_impact1)
summary(disclosure_impact1,"report")
disclosure_impact1$summary

summary(disclosure_impact2)
summary(disclosure_impact2,"report")
disclosure_impact2$summary

summary(sia_impact1)
summary(sia_impact1,"report")
sia_impact1$summary

summary(sia_impact2)
summary(sia_impact2,"report")
sia_impact2$summary
