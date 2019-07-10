library(geosphere)

lon1 = -97.040443
lat1 = 32.897480

lon2 = -97.0150
lat2 = 32.9546

distm(c(lon1, lat1), c(lon2, lat2), fun = distHaversine) * 0.000621371
