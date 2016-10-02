library("quantmod")

istraukimas <- function(salis,folderio_pav,failo_pav){
  setwd(folderio_pav)
  data=read.csv(failo_pav)
  data = data[data$S_ADJ=="Seasonally and calendar adjusted data",]
  data = data[data$UNIT=="Index, 2010=100",]
  data = data[data$GEO == salis,]
  setwd('..')
  return(data[,c(1,7)])
}

setwd("creative/clean data")

omx=read.csv("omx.csv")
data=omx[,c(2,5)]
data=xts(data[, -2], order.by=as.Date(data[,2]))
monthly.omx <- data[ endpoints(data, on="months", k=1), ]

salis="Lithuania"
folderio_pav="Turnover in industry - consumer durables"
failo_pav="sts_intv_m_1_Data.csv"
LT_ilgalaikio_vartojimo_prekės=istraukimas(salis,folderio_pav,failo_pav)

salis="Lithuania"
folderio_pav="Turnover and volume of sales in wholesale and retail trade - monthly data"
failo_pav="sts_trtu_m_1_Data.csv" 
LT_apyvarta=istraukimas(salis,folderio_pav,failo_pav) #Apyvarta ir pardavimų apimtis didmeninės ir mažmeninės prekybos


