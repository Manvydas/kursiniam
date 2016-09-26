library("quantmod")

bvp=read.csv("omx.csv")
bvp=bvp[,c(2,3,5)]
data.monthly <- data[ endpoints(data, on="months", k=1), ]

getFX("USD/EUR", start = 2000)   # per mazas laikotarpis, ieskosim kitur
exchange_rate=USDEUR
exchange_rate.monthly <- exchange_rate[ endpoints(exchange_rate, on="months", k=1)]   # daily paverciami i monthly



