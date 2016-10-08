library(quantmod)
library(forecast)
library(ggplot2)
library(dplyr)
library(zoo)

istraukimas <- function(salis,folderio_pav,failo_pav,indikatorius,versija){
  setwd(folderio_pav)
  data=read.csv(failo_pav)
  data = data[data$S_ADJ=="Unadjusted data (i.e. neither seasonally adjusted nor calendar adjusted data)",]
  data = data[data$GEO == salis,]
  setwd('..')
  if(versija == 1){
    data = data[data$UNIT=="Index, 2010=100",]
    return(data[,c(1,7)])
  }
  if(versija == 2){
    data = data[data$INDIC == indikatorius,]
    return(data[,c(1,5)])
  }
  if(versija == 3){
    data = data[data$INDIC == indikatorius,]
    return(data[,c(1,6)])
  }
  else{print("klaida del versiju pasirinkimo")}
}


setwd("creative/clean data")      

#istraukiami Lietuvos duomenys

omx=read.csv("omx.csv")
data=omx[,c(2,5)]
data=xts(data[, -2], order.by=as.Date(data[,2],"%m/%d/%Y"))
monthly.omx <- data[ endpoints(data, on="months", k=1), ]                       # pakeiciami i menesinius duomenis
                                                                                # neapatikrinta ar gerai pakeicia

startpoints <- function (x, on = "months", k = 1) {                             #paima nuo menesio pradzios
  head(endpoints(x, on, k) + 1, -1)
}

period.starts = startpoints(data, 'months', k=1)
data[period.starts,]



salis="Lithuania"
indikatorius=NA                                                                 #pirmuose duomenyse nera indikatoriaus pasirinkimu

folderio_pav="Turnover in industry - consumer durables"
failo_pav="sts_intv_m_1_Data.csv"
ilgalaikio_vartojimo_prekės=istraukimas(salis,folderio_pav,failo_pav,indikatorius,1)

folderio_pav="Turnover and volume of sales in wholesale and retail trade - monthly data"
failo_pav="sts_trtu_m_1_Data.csv" 
apyvarta=istraukimas(salis,folderio_pav,failo_pav,indikatorius,1)                              #didmeninės ir mažmeninės prekybos apyvarta ir pardavimų apimtis 

indikatorius="Construction confidence indicator"
folderio_pav="Sentiment indicators - monthly data"
failo_pav="ei_bssi_m_r2_1_Data.csv"
statybu_pasitikejimas=istraukimas(salis,folderio_pav,failo_pav,indikatorius,2)

indikatorius="Services Confidence Indicator"
folderio_pav="Sentiment indicators - monthly data"
failo_pav="ei_bssi_m_r2_1_Data.csv"
paslaugu_pasitikejimas=istraukimas(salis,folderio_pav,failo_pav,indikatorius,2)

indikatorius="Consumer confidence indicator"
folderio_pav="Sentiment indicators - monthly data"
failo_pav="ei_bssi_m_r2_1_Data.csv"
vartotoju_pasitikejimas=istraukimas(salis,folderio_pav,failo_pav,indikatorius,2)


indikatorius="Retail confidence indicator"
folderio_pav="Sentiment indicators - monthly data"
failo_pav="ei_bssi_m_r2_1_Data.csv"
mazmeninis_pasitikejimas=istraukimas(salis,folderio_pav,failo_pav,indikatorius,2) #mažmeninės prekybos pasitikėjimo rodiklis

indikatorius="Industrial confidence indicator"
folderio_pav="Sentiment indicators - monthly data"
failo_pav="ei_bssi_m_r2_1_Data.csv"
pramones_pasitikejimas=istraukimas(salis,folderio_pav,failo_pav,indikatorius,2) 

indikatorius="Business activity (sales) development over the past 3 months"
folderio_pav="Mazmenine prekyba"
failo_pav="ei_bsrt_m_r2_1_Data.csv"
verslo_aktyvumas=istraukimas(salis,folderio_pav,failo_pav,indikatorius,3)            

indikatorius="Volume of stocks currently hold"
folderio_pav="Mazmenine prekyba"
failo_pav="ei_bsrt_m_r2_1_Data.csv"
turimos_akcijos=istraukimas(salis,folderio_pav,failo_pav,indikatorius,3)             

indikatorius="Expectations of the number of orders placed with suppliers over the next 3 months"
folderio_pav="Mazmenine prekyba"
failo_pav="ei_bsrt_m_r2_1_Data.csv"
uzsakymu_lukesciai=istraukimas(salis,folderio_pav,failo_pav,indikatorius,3)           

indikatorius="Employment expectations over the next 3 months"
folderio_pav="Mazmenine prekyba"
failo_pav="ei_bsrt_m_r2_1_Data.csv"
darbolygio_lukesciai=istraukimas(salis,folderio_pav,failo_pav,indikatorius,3)               

setwd("Infliacija")
data = read.csv("prc_hicp_mv12r_1_Data.csv")
data = data[data$GEO == salis,]
infliacija = data[,c(1,5)]
setwd('..')

setwd("Nedarbas")
data = read.csv("ei_lmhu_m_1_Data.csv")
data = data[data$GEO == salis,]
nedarbas = data[,c(1,6)]
setwd('..')



#cia galima nekreipt demesio
#kintamieji=c(ilgalaikio_vartojimo_prekės,apyvarta,statybu_pasitikejimas,paslaugu_pasitikejimas,vartotoju_pasitikejimas,mazmeninis_pasitikejimas,
#pramones_pasitikejimas,verslo_aktyvumas,turimos_akcijos,uzsakymu_lukesciai,darbolygio_lukesciai,infliacija,nedarbas)
#for(i in kintamieji)
#  i %>% class %>% print




laiko_formatavimas <- function(data){
  data[,1] = data[,1] %>% as.yearmon(format="%YM%m") %>% as.Date()
  return(data[,1])
}


ilgalaikio_vartojimo_prekės[,1] = laiko_formatavimas(ilgalaikio_vartojimo_prekės)           # nesuprantu kaip naudot tapply




data[laikas,]



y <- factor(rep(letters[1:5], each = 4))

data=xts(data[, -2], order.by=as.Date(data[,2],"%m/%d/%Y"))

# prasibandymai ir problemos
ggplot() + 
  geom_point(mapping = aes(x = monthly.omx , y = infliacija))

#(blogas)
mod=lm(monthly.omx~ilgalaikio_vartojimo_prekės+apyvarta+statybu_pasitikejimas+paslaugu_pasitikejimas+vartotoju_pasitikejimas+mazmeninis_pasitikejimas
     +pramones_pasitikejimas+verslo_aktyvumas+turimos_akcijos+uzsakymu_lukesciai+darbolygio_lukesciai+infliacija)

#auto arima?
fit <- auto.arima(monthly.omx, xreg=cbind(apyvarta[-c(length(apyvarta),length(apyvarta)-1),2]))
#tslm?
fit <- tslm(y ~ x) 
