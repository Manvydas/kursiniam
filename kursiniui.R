library("quantmod")
library("forecast")

istraukimas <- function(salis,folderio_pav,failo_pav){
  setwd(folderio_pav)
  data=read.csv(failo_pav)
  data = data[data$S_ADJ=="Seasonally and calendar adjusted data",]
  data = data[data$UNIT=="Index, 2010=100",]
  data = data[data$GEO == salis,]
  setwd('..')
  return(data[,c(1,7)])
}


istraukimas2 <- function(salis,folderio_pav,failo_pav,indikatorius){
  setwd(folderio_pav)
  data=read.csv(failo_pav)
  data = data[data$S_ADJ=="Seasonally adjusted data, not calendar adjusted data",]
  data = data[data$GEO == salis,]
  data = data[data$INDIC == indikatorius,]
  setwd('..')
  return(data[,c(1,5)])
}
getwd()
setwd("creative/clean data")      

omx=read.csv("omx.csv")
data=omx[,c(2,5)]
data[,1]
class(data)
data=xts(data[, -2], order.by=as.Date(data[,2]))
monthly.omx <- data[ endpoints(data, on="months", k=1), ]

salis="Lithuania"

folderio_pav="Turnover in industry - consumer durables"
failo_pav="sts_intv_m_1_Data.csv"
ilgalaikio_vartojimo_prekės=istraukimas(salis,folderio_pav,failo_pav)

folderio_pav="Turnover and volume of sales in wholesale and retail trade - monthly data"
failo_pav="sts_trtu_m_1_Data.csv" 
apyvarta=istraukimas(salis,folderio_pav,failo_pav)                              #didmeninės ir mažmeninės prekybos apyvarta ir pardavimų apimtis 

indikatorius="Construction confidence indicator"
folderio_pav="Sentiment indicators - monthly data"
failo_pav="ei_bssi_m_r2_1_Data.csv"
statybu_pasitikejimas=istraukimas2(salis,folderio_pav,failo_pav,indikatorius)

indikatorius="Services Confidence Indicator"
folderio_pav="Sentiment indicators - monthly data"
failo_pav="ei_bssi_m_r2_1_Data.csv"
paslaugu_pasitikejimas=istraukimas2(salis,folderio_pav,failo_pav,indikatorius)

indikatorius="Consumer confidence indicator"
folderio_pav="Sentiment indicators - monthly data"
failo_pav="ei_bssi_m_r2_1_Data.csv"
vartotoju_pasitikejimas=istraukimas2(salis,folderio_pav,failo_pav,indikatorius)


indikatorius="Retail confidence indicator"
folderio_pav="Sentiment indicators - monthly data"
failo_pav="ei_bssi_m_r2_1_Data.csv"
mazmeninis_pasitikejimas=istraukimas2(salis,folderio_pav,failo_pav,indikatorius) #mažmeninės prekybos pasitikėjimo rodiklis

indikatorius="Industrial confidence indicator"
folderio_pav="Sentiment indicators - monthly data"
failo_pav="ei_bssi_m_r2_1_Data.csv"
pramones_pasitikejimas=istraukimas2(salis,folderio_pav,failo_pav,indikatorius) 

indikatorius="Business activity (sales) development over the past 3 months"
folderio_pav="Mazmenine prekyba"
failo_pav="ei_bsrt_m_r2_1_Data.csv"
verslo_aktyvumas=istraukimas2(salis,folderio_pav,failo_pav,indikatorius)

indikatorius="Volume of stocks currently hold"
folderio_pav="Mazmenine prekyba"
failo_pav="ei_bsrt_m_r2_1_Data.csv"
turimos_akcijos=istraukimas2(salis,folderio_pav,failo_pav,indikatorius)

indikatorius="Expectations of the number of orders placed with suppliers over the next 3 months"
folderio_pav="Mazmenine prekyba"
failo_pav="ei_bsrt_m_r2_1_Data.csv"
uzsakymu_lukesciai=istraukimas2(salis,folderio_pav,failo_pav,indikatorius)

indikatorius="Employment expectations over the next 3 months"
folderio_pav="Mazmenine prekyba"
failo_pav="ei_bsrt_m_r2_1_Data.csv"
darbolygio_lukesciai=istraukimas2(salis,folderio_pav,failo_pav,indikatorius)

setwd("Infliacija")
data = read.csv("prc_hicp_mv12r_1_Data.csv")
data = data[data$GEO == salis,]
infliacija = data[,c(1,5)]
setwd('..')


# prasibandymai ir problemos
class(monthly.omx)

monthly.omx["2011/"]

#(blogas)
mod=lm(monthly.omx~ilgalaikio_vartojimo_prekės+apyvarta+statybu_pasitikejimas+paslaugu_pasitikejimas+vartotoju_pasitikejimas+mazmeninis_pasitikejimas
     +pramones_pasitikejimas+verslo_aktyvumas+turimos_akcijos+uzsakymu_lukesciai+darbolygio_lukesciai+infliacija)

#auto arima?
fit <- auto.arima(monthly.omx, xreg=cbind(apyvarta[-c(length(apyvarta),length(apyvarta)-1),2]))
#tslm?
fit <- tslm(y ~ x) 
