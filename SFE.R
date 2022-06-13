#####################################################################################
# Expansion of Intermittent Renewables: Strategies, pass-through costs, and welfare distribution
# Processing raw data, descriptive tables, graphs of data, and building panel data for Stata and python
# Period 02.01.2017 - 30.09.2018
# Author : GC
# Created: 29.05.20
# Last date updated: 02.05.2022

#####################################################################################
#Input files:
#1.  namechp.xlsx                                              - plant capacitites
#2.  tout.xlsx                                                 - outage ENTSOE of all plants
#3.  chp.xlsx                                                  - chp data consumption and probabilities
#5.  Marginalcosts.xlsx                                        - fuel and Co2 costs
#6.  Realisierter_Stromverbrauch_201701020000_201809302345.csv - Actual electricity demand
#7.  Realisierte_Erzeugung_201701020000_201809302345.csv       - Actual electricity production
#8.  inst.xlsx                                                 - prices of electricity (day ahead and real time), coal, gas, oil
#9.  wind.xlsx                                                 - Hourly wind data per plant
#10. solar.xlsx                                                - Hourly wind data per plant

#####################################################################################
#Output files
#1. prob2.cvs         - outage probabilites 
#2. uno.Rdata         - R data to run SFE method (1)
#3. dos.Rdata         - R data to run SFE method (2)
#4. SFE_one.xlsx      - after SFE, excel for panel data to run SFE regression in Stata (1) 
#5. SFE_two.xlsx      - after SFE, excel for panel data to run SFE regression in Stata (2) 
#6. TA1.csv            - Table 5 of the paper
#7. TA8.xlsx           - Table 1 of the paper
#8. FA2.pdf           - Figure 2 of the paper
#9. FA3.pdf           - Figure 3 of the paper

#####################################################################################

#Package to update R
#install.packages("installr")
#library(installr)
#updateR()


#if(FALSE){ 

  install.packages('tidyverse')
  library('tidyverse')
  library(lubridate)
  library(data.table)
  library(readxl)
  install.packages("reshape2")
  library("reshape2")
  install.packages('xts')
  library('xts')
  install.packages('tibbletime')
  library(tibbletime)
  install.packages("writexl")
  library(writexl)
#}

#cleaning the entire environment
remove(list = ls())
  
  
###########################################################################################
#CAPACITIES multiuse file
#Reading capacities from AURORA, Open Power Project and SMARD dataset (accesed on 02.12.2019).
#https://www.smard.de/home/wiki-article/446/636
p0 <- proc.time()[3]
setwd(dirname(rstudioapi::getActiveDocumentContext()$path)) #directory where the R script is located
namemc <- read_excel("namemchp.xlsx", col_types = c("text", 
                                                  "numeric", "numeric", "numeric", "numeric", 
                                                  "numeric", "numeric", "numeric", "numeric", "numeric", 
                                                  "numeric", "numeric", "numeric", "numeric", "numeric", 
                                                  "numeric", "numeric", "numeric", "numeric", "numeric", 
                                                  "numeric", "numeric", "numeric", "numeric", "numeric", 
                                                  "numeric", "numeric", "numeric", "numeric", "numeric", 
                                                  "numeric", "numeric", "numeric", "numeric", "numeric", 
                                                  "numeric", "numeric", "numeric", "numeric", "numeric", 
                                                  "numeric", "numeric", "numeric", "numeric", "numeric", 
                                                  "numeric", "numeric", "numeric", "numeric", "numeric", 
                                                  "numeric", "numeric", "numeric", "numeric", "numeric", 
                                                  "numeric", "numeric", "numeric", "numeric", "numeric", 
                                                  "numeric", "numeric", "numeric", "numeric", "numeric", 
                                                  "numeric", "numeric", "numeric", "numeric", "numeric", 
                                                  "numeric", "numeric", "numeric", "numeric", "numeric", 
                                                  "numeric", "numeric", "numeric", "numeric", "numeric", 
                                                  "numeric", "numeric", "numeric", "numeric", "numeric", 
                                                  "numeric", "numeric", "numeric", "numeric", "numeric", 
                                                  "numeric", "numeric", "numeric", "numeric", "numeric", 
                                                  "numeric", "numeric", "numeric", "numeric", "numeric", 
                                                  "numeric", "numeric", "numeric", "numeric", "numeric", 
                                                  "numeric", "numeric", "numeric", "numeric", "numeric", 
                                                  "numeric", "numeric", "numeric", "numeric", "numeric", 
                                                  "numeric", "numeric", "numeric", "numeric", "numeric", 
                                                  "numeric", "numeric", "numeric", "numeric", "numeric", 
                                                  "numeric", "numeric", "numeric", "numeric", "numeric", 
                                                  "numeric", "numeric", "numeric", "numeric", "numeric", 
                                                  "numeric", "numeric", "numeric", "numeric", "numeric",
                                                  "numeric"))


#extract first 3rows
first3r <- namemc[c(1:3),]
namemc1 <- namemc[-c(1:4),]
datec <- namemc1[,c(1)]

#PROB OUTAGE - START PROCESS do this once to obtain prob.cvs file or skip to MC CALCULATION
###########################################################################################
#Separating outage blocks in hours (execution time: around 9min)
dat <- data.frame(read_excel("tout.xlsx"))
out1 <- dat[,-c(1:4,6:7)]

#########################
#outage in hours - start
 
start <- as_datetime('2017-01-02')
end <- as_datetime('2018-10-01')
times <- seq.POSIXt(start, end, by = 'hour')

fof <- c(out1$fof)
unit <- c(dat[,"Unit.Name"])
start_time <- as_datetime(c(out1[,"start"])) 
end_time <- as_datetime(c(out1[,"end"]))
df <-  data.table("fof" = fof, "start_time" = start_time, "end_time" = end_time, "unit"=unit)
setkey(df, start_time, end_time)

df_times <- data.table(start = times[-length(times)], end = times[-1])
setkey(df_times, start, end)

df <- foverlaps(df, df_times, by.x = c('start_time', 'end_time'), by.y = c('start', 'end'))
df[, time_occupied := (as.double(difftime(min(.SD[,c(end, end_time)]), max(.SD[,c(start, start_time)]), units = 'mins'))), .(start, fof)]

df[order(fof), .(fof, start, end, time_occupied)]
df$fofs <- (df[,df[,"fof"]*abs(df[,"time_occupied"])])/60

rm(dat, df_times, end, end_time, fof, start, start_time, times, unit)
#########################
#outage in hours - end

detach(package:plyr)
##bug correction - subsetting
df2 <- df[df$time_occupied>0,]
out2 <- df2 %>%
  group_by(start, end, unit) %>%
  summarise(tfof = sum(fofs))
#transpose, colocar las unidades horizontalmente
out2 <- out2[, c("start", "tfof", "unit")]
out3 <- dcast(out2, start~unit, value.var="tfof") #reshape2
out3[is.na(out3)] <- 0 #replace nas
colnames(out3)[1] <- "Date" 
outd1 <- out3[,-c(1)]
outd2 <- cbind.data.frame(datec, outd1)
#substract outage from namemc for the first method
#intersect with data.table
out4 <- setdiff(intersect(names(namemc1), names(outd2)), c("Date"))
#subset, making a list where we get the difference and mapping 
out5 <- setDT(namemc1)[outd2, (out4) := Map(`-`, mget(out4),
                             mget(paste0("i.", out4))), on =.(Date)]
#checking negative numbers
col2 <- out5[,c(1:2)]
out6 <- out5[, c(3:141)]
out6[out6 > 0] <- 0
out7 <- cbind.data.frame(col2, out6)
rm(out6, col2)
#this code is to check is there are any NAS and replace all negative errors from ENTSO-e
out8 <- out5
out8[is.na(out8)] <- 0
out8[out8 < 0] <- 0
#replace first three rows and continue
data1 <- data.frame(rbind.data.frame(first3r, out8))
rm(datec)
rm(df, df2, out1, out2, outd1, outd2, out3,  out4, out5, out7, out8)  


####################################################################################
#FOFs - forced outage factors
t3 <- namemc[-c(1:4),-c(1:2)] - data1[-c(1:3),-c(1:2)]
first3rm <- as.numeric(first3r[-c(2:3),-c(1:2)])

t4 <- rbind.data.frame(first3rm, t3)
t5 <- data.frame(t4[,-c(118:139)])


fof1 <- t5[,t5[1,]==6]
fof2 <- t5[,t5[1,]==9]
fof3 <- t5[,t5[1,]==10]
Huckingenf <- t5[,t5[1,]==13]
fof5 <- t5[,t5[1,]==14]
fof6 <- t5[,t5[1,]==15]
fof7 <- cbind.data.frame(fof1, fof2, Huckingenf, fof3, fof5, fof6)


fof7$foil <- rowSums(fof7[,fof7[1,]==6,] )
fof7$fcoal <- rowSums(fof7[,fof7[1,]==9,])
fof7$fgas1 <- rowSums(fof7[,fof7[1,]==10,])
fof7$fgas2 <- rowSums(fof7[,fof7[1,]==14,])
fof7$fgas3 <- rowSums(fof7[,fof7[1,]==15,])
fof7$fgas <- rowSums(fof7[,c(78:80)])

fof8 <- data.frame(fof7$foil,fof7$fcoal, fof7$fgas)
colnames(fof8) <- c("foil", "fcoal", "fgas")
rm(t3, t4, t5, fof1, fof2, fof3, fof5, fof6, fof7, first3rm, Huckingenf)

#%fofs
bb <- data.frame(namemc[-c(2:4),])

bb$foil1 <- bb[,bb[1,]==23,]
bb$foil2 <- bb[,bb[1,]==24,]
bb$foil3 <- bb[,bb[1,]==25,]
bb$foil <- rowSums(bb[,c("foil1", "foil2", "foil3")])
bb$fcoal <- bb$Steinkohle
bb$fgas <- bb$Erdgas
bb1 <- bb[,c(145:147)]

#calculating the %fof
pf <- fof8[-c(1),] / bb1[-c(1),]

#verifying
pf[pf < 0] <- 0
pf11 <- 1-pf[,]

cols2 <- namemc[-c(1:4),c(1:2)]
pf2 <- cbind.data.frame(cols2, pf11)

write.csv(pf2, "prob2.csv")

rm(bb, bb1, cols2, fof8, pf, pf1, pf11, pf2)

###########################################################################################
#PROB OUTAGE - END PROCESS 


#############################################################################################
#MC CALCULATION - START PROCESS
#############################################################################################

#Adding chp profiles to capacities
namemc <- data.frame(namemc)
chp <- read_excel("chp.xlsx", col_types = c("text","numeric", "numeric"))
chp1 <- data.frame(chp[3])
datm1 <- cbind.data.frame(namemc[-c(4),], chp1)
datm1 <- datm1[-c(2:3),]

dat1 <- datm1[,datm1[1,]==6]
dat2 <- datm1[,datm1[1,]==9]
dat3 <- datm1[,datm1[1,]==10]
Huckingen <- datm1[,datm1[1,]==13]
dat5 <- datm1[,datm1[1,]==14]
dat6 <- datm1[,datm1[1,]==15] 
rest <- datm1[c(132:141)] 
chp2 <- datm1[c(142)] 

datm2 <- cbind.data.frame(dat1, dat2, Huckingen, dat3, dat5, dat6, rest, chp2)

#keeping techs to match the transposed data later
tech <- datm1[c(1),]
tech1 <- data.frame(stack(tech))
tech1 <- tech1[-c(1:2), c(2,1)]
#removing the excess
datm2 <- datm2[-c(1),]
rm(dat1,dat2, dat3, dat5, dat6, rest, Huckingen, chp, chp1, chp2)
#transpose cap for later merit order construction
datm2 <- data.frame(stack(datm2))

summary(datm2)

#adding time in numbers
time <- rep(c(1:15288),86)
datm3 <- cbind.data.frame(datm2, time)
#reordering
datm3 <- datm3[, c(2,3,1)]
#adding tech matching to ind
datm4 <- merge(datm3, tech1, by="ind")
datm4$ix <- with(datm4, paste(ind, time))
datm4 <- datm4[, -c(1)]
#renaming
colnames(datm4) <- c("time", "cap","techc", "ix")
rm(datm1, datm2, datm3, tech, tech1)

###########################################################################################
#OUTAGE PROBABILITIES
#Use this when the file prob2 was already created
prob <- read_csv("prob2.csv")

prob <- prob[,-c(1)]

pr <- namemc[-c(2:4),-c(1:2)]
pr[c(2:15288), c(1:139)] <- 0

cinx <- which( pr[1,] == "9" | pr[1,] == "8" | pr[1,] == "17" | pr[1,] == "18") #coal index
ginx <- which( pr[1,] == "14" | pr[1,] == "15" | pr[1,] == "10" | pr[1,] == "19"| pr[1,] == "20"| pr[1,] == "21"| pr[1,] == "22") #gas index         
oinx <- which( pr[1,] == "13" | pr[1,] == "6" | pr[1,] == "23"| pr[1,] == "24"| pr[1,] == "25"| pr[1,] == "26") #oil index          

prt <- pr[c(1),]
pr <- pr[-c(1),]

pr[,cinx] <- prob$fcoal
pr[,ginx] <- prob$fgas
pr[,oinx] <- prob$foil

pr <- rbind.data.frame(prt, pr)

pr$gaschp <- 1

pr[c(1),] <- round(pr[c(1),],0)

pr1 <- pr[,pr[1,]==6]
pr2 <- pr[,pr[1,]==9]
pr3 <- pr[,pr[1,]==10]
Huckingen <- pr[,pr[1,]==13]
pr5 <- pr[,pr[1,]==14]
pr6 <- pr[,pr[1,]==15]
rest <- pr[c(130:139)] 
gaschp <- pr[c(140)]

prm1 <- cbind( pr1, pr2, Huckingen, pr3, pr5, pr6, rest, gaschp)
prm1 <- prm1[-c(1),] 

rm(pr1,pr2, pr3, pr5, pr6, rest, Huckingen, gaschp, cinx, ginx, oinx)
prm2 <- stack(prm1)
rm(prm1)
prm3 <- prm2[, c(2,1)]
##combining the indexes plant name and time
time <- rep(c(1:15288),86)
prm4 <- cbind.data.frame(prm3, time)
prm4$ix <- with(prm4, paste(ind, time))
prm4 <- prm4[,-c(1,3)]
rm(prm2,prm3)

##renaming
colnames(prm4) <- c( "prob", "ix")
##adding tech matching to ix
datm44 <- merge(datm4, prm4, by="ix")
rm(datm4, pr, prm4, prob, prt)

#COSTS------------------------------------------------------------------------------------
#fcost sheet -fuel prices*HR
b <- data.frame(read_excel("Marginalcosts.xlsm", sheet="Tab1"))
b1 <- b[,b[1,]==6]
b2 <- b[,b[1,]==9]
b3 <- b[,b[1,]==10]
Huckingen <- b[,b[1,]==13]
b5 <- b[,b[1,]==14]
b6 <- b[,b[1,]==15]
rest <- b[c(120:129)] 
chp <- b[c(130)] 

bm1 <- cbind.data.frame(b1, b2, Huckingen, b3, b5, b6, rest,chp)
bm1 <- bm1[-c(1,2),]
rm(b, b1,b2, b3, b5, b6, rest, Huckingen, chp)
##transpose
bm2 <- data.frame(stack(bm1))
rm(bm1)
##reordering
bm3 <- bm2[, c(2,1)]
##combining the indexes plant name and time
bm4 <- cbind.data.frame(bm3, time)
bm4$ix <- with(bm4, paste(ind, time))
bm4 <- bm4[,-c(1,3)]
rm(bm2,bm3)

##renaming
colnames(bm4) <- c( "fcost", "ix")
##adding tech matching to ix
datm5 <- merge(datm44, bm4, by="ix")
rm(bm4, datm44)


#CO2prices cocos sheet -co2 prices*HR*emissions
f <- data.frame(read_excel("Marginalcosts.xlsm", sheet="Tab2"))
f1 <- f[,f[1,]==6]
f2 <- f[,f[1,]==9]
f3 <- f[,f[1,]==10]
Huckingen <- f[,f[1,]==13]
f5 <- f[,f[1,]==14]
f6 <- f[,f[1,]==15]
rest <- f[c(120:129)] 
chp <- f[c(130)]

fm1 <- cbind.data.frame(f1, f2, Huckingen, f3, f5, f6, rest, chp)
fm1 <- fm1[-c(1,2),]
rm(f, f1,f2, f3, f5, f6, rest, Huckingen, chp)
##transpose
fm2 <- data.frame(stack(fm1))
rm(fm1)
##reordering
fm3 <- fm2[, c(2,1)]
##combining the indexes plant name and time
fm4 <- cbind.data.frame(fm3, time)
fm4$ix <- with(fm4, paste(ind, time))
fm4 <- fm4[,-c(1,3)]
rm(fm2,fm3)
##renaming
colnames(fm4) <- c( "ghgc", "ix")
##adding tech matching to ix
datm6 <- merge(datm5, fm4, by="ix")
rm(fm4, datm5)


#RESIDUAL DEMAND-----------------------------------------------------------------------------
#### Reading realisierte Stromverbrauch to obtain the residual demand
## total demand is equal to Generation (ff+ren+imports) - (exports+pump)

demandc <- data.frame(read_delim("Realisierter_Stromverbrauch_201701020000_201809302345.csv"
                                 , ";", escape_double = FALSE, 
                                 locale = locale(decimal_mark = ","), 
                                 trim_ws = TRUE))


demandc$Gesamt.MWh.<- as.numeric(gsub(",", ".", gsub("\\.", "", demandc$Gesamt.MWh.)))

demandc <- demandc[-c(28809:28812),] #day 29.10.2017 duplicate on 02:00
demandc$Datum <- as.Date(demandc$Datum, "%d.%m.%Y")  
demandc$date <- as.POSIXct(paste(demandc$Datum, demandc$Uhrzeit), format="%Y-%m-%d %H:%M:%S", tz='utc')
demandc <- demandc %>% select(date, everything())
demandc <- demandc[,-c(2,3)]
ddc <- create_series('2017-01-02' ~ '2018-09-30', '15 min')
dsmardc1 <- merge(ddc, demandc, by="date", all.x=T)
colnames(dsmardc1)[2] <- 'demand'
rm(demandc)
dsmardc2 <- xts(dsmardc1$demand,
               as.POSIXct(dsmardc1$date))
ends <- endpoints(dsmardc2,'hours',1) 
dsmardc2 <- period.apply(dsmardc2,ends,sum)
colnames(dsmardc2)[1] <- 'demand'

ddhc <- create_series('2017-01-02' ~ '2018-09-30', '1 hour')
dsmardc3 <- data.frame(ddhc)
dsmardc3$demand <- dsmardc2$demand
rm(dsmardc1, dsmardc2, ends)


## Data from SMARD- accesed on 11.06.2020 realisierte Stromerzeugung (RE)
## https://www.smard.de/home/wiki-article/446/636
rsmard <- data.frame(read_delim("Realisierte_Erzeugung_201701020000_201809302345.csv", ";", escape_double = FALSE, 
                                locale = locale(decimal_mark = ","), 
                                trim_ws = TRUE))
rsmard[14] <- sapply(rsmard[14], function(v) {as.numeric(gsub("\\.","", as.character(v)))})


rsmard <- rsmard[-c(28809:28812),] #day 29.10.2017 duplicate on 02:00
rsmard$Datum <- as.Date(rsmard$Datum, "%d.%m.%Y")  
rsmard$date <- as.POSIXct(paste(rsmard$Datum, rsmard$Uhrzeit), format="%Y-%m-%d %H:%M:%S", tz='utc')
rsmard <- rsmard %>% select(date, everything()) 
rsmard <- rsmard[,-c(2,3)]
rsmardc <- merge(ddc, rsmard, by="date", all.x=T)
rsmardc$total <- rowSums(rsmardc[, 2:13])
rsmardc1 <- xts(rsmardc[,-1], order.by=as.POSIXct(rsmardc$date))
ends <- endpoints(rsmardc1,'hours',1) 
rsmardc2 <- period.apply(rsmardc1,ends,colSums)
rsmardc2 <- as.data.frame(rsmardc2)
rownames(rsmardc2) <- 1:nrow(rsmardc2) 

rm(ddc,rsmardc, ddhc,rsmardc1, ends)

Resdm5 <- cbind.data.frame(dsmardc3, rsmardc2)

#reducing must run by a factor
#Resdm5$factor <- Resdm5$demand / Resdm5$total
#must run
Resdm5$must <- rowSums(Resdm5[, c(3:10)])
#Resdm5$mustf <- Resdm5$must * Resdm5$factor
#residual demand
Resdm5$rd <- Resdm5$demand  - Resdm5$must
#renewables 
Resdm5$ren <- Resdm5$Wind.Offshore.MWh. + Resdm5$Wind.Onshore.MWh. + Resdm5$Photovoltaik.MWh.
write_xlsx(Resdm5, "resdm5.xlsx")

time <- c(1:nrow(Resdm5))
rd1 <- cbind.data.frame(Resdm5$rd, time)
colnames(rd1) <- c( "rd", 'time')

datm7 <- merge(datm6, rd1, by="time")

#adding fuel prices and electricity prices
h <- data.frame(read_excel("inst.xlsx"))
datm8 <- merge(datm7, h, by="time")
rm(datm6, datm7, rd1, dsmardc3, rsmardc2, h)



#adding probabilities chp---------------------------------------------------------
prob2 <- data.frame(read_excel("chp.xlsx", sheet="probchp"))
prob2 <- prob2[,-c(1)]

pr <- namemc[-c(1:3),-c(1:2)]
pr[c(2:15289), c(1:139)] <- 1

cchp <- which( pr[1,] == "53") #coal index
gchp <- which( pr[1,] == "55") #gas index         

prt <- data.frame(first3r[c(1),-c(1:2)])
pr <- pr[-c(1),]
pr[,cchp] <- prob2$coalchp
pr[,gchp] <- prob2$gaschp

pr <- rbind.data.frame(prt, pr)

pr$gaschp <- 1

pr1 <- pr[,pr[1,]==6]
pr2 <- pr[,pr[1,]==9]
pr3 <- pr[,pr[1,]==10]
Huckingen <- pr[,pr[1,]==13]
pr5 <- pr[,pr[1,]==14]
pr6 <- pr[,pr[1,]==15]
rest <- pr[c(130:139)] 
gaschp <- pr[c(140)]

prm1 <- cbind( pr1, pr2, Huckingen, pr3, pr5, pr6, rest, gaschp)
prm1 <- prm1[-c(1),] 

rm(pr1,pr2, pr3, pr5, pr6, rest, Huckingen, gaschp, cchp, gchp)
prm2 <- stack(prm1)
rm(prm1)
prm3 <- prm2[, c(2,1)]
##combining the indexes plant name and time
time <- rep(c(1:15288),86)
prm4 <- cbind.data.frame(prm3, time)
prm4$ix <- with(prm4, paste(ind, time))
prm4 <- prm4[,-c(1,3)]
rm(prm2,prm3)

##renaming
colnames(prm4) <- c( "probchp", "ix")
##adding tech matching to ix
datm9 <- merge(datm8, prm4, by="ix")
rm(pr, prm4, prt, rsmard, prob2, datm8)

#--------------------------------------------------------------------------------------------
#adding hourly wind data
wind <- data.frame(read_excel("wind.xlsx"))
wind2<- wind[-c(2),-c(1:2)]

wind3 <- wind2[,wind2[1,]==6]
wind4 <- wind2[,wind2[1,]==9]
wind5 <- wind2[,wind2[1,]==10]
Huckingen <- wind2[,wind2[1,]==13]
wind6 <- wind2[,wind2[1,]==14]
wind7 <- wind2[,wind2[1,]==15]
rest <- wind2[c(118:127)] 
gaschp <- wind2[c(128)]

windm1 <- cbind( wind3, wind4, wind5, Huckingen,wind6, wind7, rest, gaschp)
windm1 <- windm1[-c(1),] 

windm2<- data.frame(stack(windm1))
windm3 <- cbind.data.frame(windm2,time)
windm3$ix <- with(windm3, paste(ind, time))
windm3 <- windm3[,-c(2,3)]

##renaming
colnames(windm3) <- c( "wind", "ix")
##adding tech matching to ix
datm10 <- merge(datm9, windm3, by="ix")
rm(wind2, wind3, wind4, wind5, wind6,wind7,wind, windm1, windm2, windm3, rest, gaschp, Huckingen, datm9)


#--------------------------------------------------------------------------------------------
#adding solar
solar <- data.frame(read_excel("solar.xlsx"))
solar2<- solar[-c(2),-c(1:2)]

solar3 <- solar2[,solar2[1,]==6]
solar4 <- solar2[,solar2[1,]==9]
solar5 <- solar2[,solar2[1,]==10]
Huckingen <- solar2[,solar2[1,]==13]
solar6 <- solar2[,solar2[1,]==14]
solar7 <- solar2[,solar2[1,]==15]
rest <- solar2[c(118:127)] 
gaschp <- solar2[c(128)]

solarm1 <- cbind( solar3, solar4, solar5, Huckingen,solar6, solar7, rest, gaschp)
solarm1 <- solarm1[-c(1),] 

solarm2<- data.frame(stack(solarm1))
solarm3 <- cbind.data.frame(solarm2,time)
solarm3$ix <- with(solarm3, paste(ind, time))
solarm3 <- solarm3[,-c(2,3)]

##renaming
colnames(solarm3) <- c( "solar", "ix")
##adding tech matching to ix
datm11 <- merge(datm10, solarm3, by="ix")
rm(solar2, solar3, solar4, solar5, solar6,solar7,solar, solarm1, solarm2, solarm3, rest, gaschp, Huckingen, datm10)

###################################################################################################
#BASE REGRESSION----------------------------------------------------------------------------------

datm11$tc <- datm11$fcost + datm11$ghgc 

#cleaning empty rd times
sfe1 <- with(datm11, subset(datm11,  !is.na(rd)))
#sfe1 <- na.omit(datm11)
summary(sfe1)
colnames(sfe1)[1]<- "plant"

library(plyr)
fb <- ddply(sfe1,.(time,rd),nrow) #verifying that each time has 87 plants
#aqui me quede, cambiando el tiempo para que corra el algoritmo primero ordenando
sfe2 <- sfe1
sfe2$id <- sub("\\s+[^ ]+$", '', sfe1$plant)

sfe2 <- sfe2 %>% arrange(id, time)
#detach(package:plyr)
real_time <- data.frame(sfe2$time)

time2 <- rep(c(1:nrow(fb)),86)
sfe2$time <- time2

rm(sfe1, time2)
#save(sfe2, file="sfe2.RData")
#load("sfe2.RData")
#--------------------------------------------------------------------------------------------

##Drawing the merit order curve and intersecting graphically on one point---------------------- 
ZZ <- data.frame(sfe2[which(sfe2["time"] == 8227),])
WW <- data.frame(ZZ[order(ZZ$tc),])
YY <- WW %>% mutate(cumsum = cumsum(cap))
##cleaning the environment
rm(ZZ,WW)
#extracting the rd as one value to draw a vertical and horizontal line, we already have this one
#from the original dataset
e <- sfe2[sfe2[,"time"] ==8227,8] #residual demand
p <- sfe2[sfe2[,"time"] ==8227,10] # price

ggplot() +
  geom_line(data=YY, aes(x=cumsum,y=tc, color="Tc")) + 
  geom_line(data=YY, aes(x=cumsum,y=ghgc, color="ETS"))+ 
  geom_vline(xintercept=e, color="black",linetype="dashed") +
  geom_hline(yintercept=p, color="azure4", linetype="dotted") +
  scale_color_manual("", breaks= c("Tc","ETS"), values=c("Tc"="blue", "ETS"="red"))+
  labs(y='\u20ac/MW', x = "MW") +
  scale_x_continuous(breaks = seq(-10000, 90000, 20000), lim = c(-10000, 90000)) +
  scale_y_continuous(breaks = seq(-100,170, 20), lim = c(-100, 170)) +
  theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(),
        panel.background = element_blank(), axis.line = element_line(colour = "azure4"), 
        axis.text = element_text(size = (7)), axis.title = element_text(size = (6)), 
        legend.text = element_text(size = (5.5)), legend.position = c(0.8,0.8), 
        legend.key = element_blank()) 



R = 50
Obs = nrow(fb)
stamp = c(1:Obs)

#seeds
a = 301  
b = a+R

#--------------------------------------------------------------------------------------------
#p0 <- proc.time()[3] #5hours
set.seed(a:b) # R times
lista <- list()
for(k in 1:R){
  prb <- rbinom(length(sfe2$time), 1, prob = sfe2[[5]])
  dat <- sfe2 %>% mutate( capp = as.numeric(sfe2$cap)*prb )
  
  prb2 <- rbinom(length(sfe2$time), 1, prob = sfe2[[18]]) # after the change to resdemand
  datv <- dat %>% mutate( capp2 = as.numeric(dat$capp)*prb2 )
  
  datalist = list()
  for(i in 1:Obs){
    if (nrow(datv)==0) g
    ##check this to reduce time of loop
    ZZ <- data.frame(datv[which(datv["time"] == i ),])
    WW <- data.frame(ZZ[order(ZZ$tc),])
    YY <- WW %>% mutate(cumsum = cumsum(capp2))
    cross <-YY[which.min(abs(YY$rd-YY$cumsum)),]
    cross$i <- i
    datalist[[i]] <- cross
  }
  bd = do.call(rbind, datalist)
  
  lista[[k]] <- bd
}
#print(proc.time()[3]-p0)

rm(cross, datalist, WW, YY, ZZ,i, k, prb,a,b)

Output <- dir.create("Output")

save(lista, file="Output/uno.RData")
#--------------------------------------------------------------------------------------------
 
#load("uno.RData")


#--------------------------------------------------------------------------------------------
#Gathering the averages and constructing the panel data
setwd(dirname(rstudioapi::getActiveDocumentContext()$path)) #directory where the R script is located
R = 50
Obs = nrow(fb)
stamp = c(1:Obs)
#fcosts
suma1 = list()
for(n in 1:R){  
  suma1[[n]] <- lista[[n]]["fcost"]
}
d1 = do.call(cbind, suma1)
sum1 <- data.frame(stamp, fcost =rowMeans(d1), sdvfc = apply(d1,1,sd))

#coa cost
suma2 = list()
for(n in 1:R){  
  suma2[[n]] <- lista[[n]][c("ghgc")]
}
d2 = do.call(cbind, suma2)
sum2 <- data.frame(stamp, ghgc =rowMeans(d2), sdvghg = apply(d2,1,sd))


#total marginal costs
suma = list()
for(n in 1:R){  
  suma[[n]] <- lista[[n]]["tc"]
}
d = do.call(cbind, suma) 
sum3 <- data.frame(stamp, mumc =rowMeans(d), sdvmc = apply(d,1,sd))


#wind average
suma3 = list()
for(n in 1:R){  
  suma3[[n]] <- lista[[n]]["wind"]
}
d3 = do.call(cbind, suma3) 
sum4 <- data.frame(stamp, muwind =rowMeans(d3), sdvwind = apply(d3,1,sd))


#solar average
suma4 = list()
for(n in 1:R){  
  suma4[[n]] <- lista[[n]]["solar"]
}
d4 = do.call(cbind, suma4) 
sum5 <- data.frame(stamp, musolar =rowMeans(d4), sdvsolar = apply(d4,1,sd))


rd <- lista[[1]]["rd"]

price <- lista[[1]]["price"]

coal <- lista[[1]]["coal"]
gas <- lista[[1]]["gas"]
oil <- lista[[1]]["oil"]
coa <- lista[[1]]["coa"]


hour <- lista[[1]]["hour"]


colnames(real_time) <- "time"
real_time <- real_time %>% distinct()

uno <- cbind.data.frame(real_time, sum1, sum2[,-c(1)], sum3[,-c(1)], sum4[,-c(1)], sum5[,-c(1)], rd, price,coal, gas, oil, coa, hour)
uno <- uno[,-c(2)]

rm(d,d1,d2, rd, price, hour, sum1, sum2, sum3, suma1, suma2, suma, n, Obs, R, stamp)


stamp <- namemc1[,c(1:2)]
dayc <- rep(c("Mo", "Tu", "W", "Th", "Fr", "Sa", "Su"),each =24)
day <- rep(c(dayc), 91)
ja <- cbind.data.frame(stamp,day)

attach(ja)
ja$week[day=="Mo" | day=="Tu" | day=="W" | day=="Th" | day=="Fr" ] <- "work"
ja$week[day=="Sa" | day=="Su" ] <- "end"
detach(ja)

aa <- join( real_time, ja, by = "time")
aa$month <- as.numeric(substring(aa$Date,6,7))
aa$year <- as.numeric(substring(aa$Date,1,4))


colnames(uno)[1] <- "time"

sfe_one <- join( uno, aa, by = "time")

attach(sfe_one)
sfe_one$block[6<=hour & hour<=9] <- "peak1"
sfe_one$block[13<=hour & hour<=16] <- "peak2"
sfe_one$block[20<=hour & hour<=21] <- "peak3"
sfe_one$block[10<=hour & hour<=12] <- "sppeak1"
sfe_one$block[17<=hour & hour<=19] <- "sppeak2"
sfe_one$block[1<=hour & hour<=5] <- "off"
sfe_one$block[22<=hour & hour<=24] <- "off"
detach(sfe_one)


attach(sfe_one)
sfe_one$period[4<=month & month<=9] <- "Summer"
sfe_one$period[1<=month & month<=3] <- "Winter"
sfe_one$period[10<=month & month<=12] <- "Winter"
detach(sfe_one)


rm(a, aa, coa, coal, gas, oil, sfe1, uno)

write_xlsx(sfe_one, "Output/sfe_one.xlsx")

#HISTOGRAMS MC-------------------------------------------------------
#histogram
ggplot(data=sfe_one, aes(mumc)) +
  geom_histogram(binwidth=0.5) +
  scale_x_continuous(breaks = seq(0, 140, 30), lim = c(0, 140)) +
  scale_y_continuous(breaks = seq(0, 700, 200), lim = c(0, 700))


###################################################################################################
#RAMPING CASE----------------------------------------------------------------------------------

setwd(dirname(rstudioapi::getActiveDocumentContext()$path)) #directory where the R script is located
##ramping costs
b <- data.frame(read_excel("ramp.xlsx"))

##extracting ff columns
b1 <- b[,b[1,]==6]
b2 <- b[,b[1,]==9]
b3 <- b[,b[1,]==10]
Huckingen <- b[,b[1,]==13]
b5 <- b[,b[1,]==14]
b6 <- b[,b[1,]==15]
rest <- b[c(120:129)] 
gaschp <- b[c(130)] 

bm1 <- cbind.data.frame(b1, b2, Huckingen, b3, b5, b6, rest,gaschp)
bm1 <- bm1[-c(1,2),]
rm(b, b1,b2, b3, b5, b6, rest, Huckingen, gaschp)
##transpose
bm2 <- data.frame(stack(bm1))
bm3 <- cbind.data.frame(bm2,time)
bm3$ix <- with(bm3, paste(ind, time))
bm3 <- bm3[,-c(2,3)]

##renaming
colnames(bm3) <- c( "rampc", "ix")
##adding tech matching to ix
datm11r <- merge(datm11, bm3, by="ix")
rm(bm2, bm3, bm4, bm5, bm6,bm7,bm, bm1, bm2, bm3, rest, gaschp, Huckingen, datm10)

##sumando el costo del combustible mas el costo del carbono
datm11r$tcr <- datm11r$fcost + datm11r$ghgc + datm11r$rampc


#cleaning empty rd times
sfe1r <- with(datm11r, subset(datm11r,  !is.na(rd)))
#sfe1 <- na.omit(datm11)
summary(sfe1r)
colnames(sfe1r)[1]<- "plant"

library(plyr)
fr <- ddply(sfe1r,.(time,rd),nrow) #verifying that each time has 86 plants
#aqui me quede, cambiando el tiempo para que corra el algoritmo primero ordenando
sfe2r <- sfe1r
sfe2r$id <- sub("\\s+[^ ]+$", '', sfe1r$plant)

sfe2r <- sfe2r %>% arrange(id, time)
#detach(package:plyr)
real_time <- data.frame(sfe2r$time)

time2 <- rep(c(1:nrow(fr)),86)
sfe2r$time <- time2

rm(sfe1r, time2)

#Table A1: Descriptive stats 
#####################################################################################
sfe2rt <- na.omit(sfe2r)
summary.t1 = sfe2rt %>%
  summarise_all(funs(mean, sd, min, max))   
sum.t1 <- data.frame(t(summary.t1))
sum.t1$A <- rownames(sum.t1) 
write_xlsx(sum.t1, "Output/TA1.xlsx")
#####################################################################################

#save(sfe2r, file="sfe2r.RData")
#load("sfe2r.RData")


R = 50
Obs = nrow(fr)
stamp = c(1:Obs)

#seeds
a = 301  
b = a+R

#--------------------------------------------------------------------------------------------
#p0 <- proc.time()[3] #5hours
set.seed(a:b) # R times
lista <- list()
for(k in 1:R){
  prb <- rbinom(length(sfe2r$time), 1, prob = sfe2r[[5]])
  dat <- sfe2r %>% mutate( capp = as.numeric(sfe2r$cap)*prb )
  
  prb2 <- rbinom(length(sfe2r$time), 1, prob = sfe2r[[18]]) # after the change to resdemand
  datv <- dat %>% mutate( capp2 = as.numeric(dat$capp)*prb2 )
  
  datalist = list()
  for(i in 1:Obs){
    if (nrow(datv)==0) g
    ##check this to reduce time of loop
    ZZ <- data.frame(datv[which(datv["time"] == i ),])
    WW <- data.frame(ZZ[order(ZZ$tcr),])
    YY <- WW %>% mutate(cumsum = cumsum(capp2))
    cross <-YY[which.min(abs(YY$rd-YY$cumsum)),]
    cross$i <- i
    datalist[[i]] <- cross
  }
  bd = do.call(rbind, datalist)
  
  lista[[k]] <- bd
}
#print(proc.time()[3]-p0)

rm(cross, datalist, WW, YY, ZZ,i, k, prb,a,b)

save(lista, file="Output/dos.RData")
#--------------------------------------------------------------------------------------------

#load("dos.RData")

#--------------------------------------------------------------------------------------------
#Gathering the averages and constructing the panel data
setwd(dirname(rstudioapi::getActiveDocumentContext()$path)) #directory where the R script is located
R = 50
Obs = nrow(fr)
stamp = c(1:Obs)
#fcosts
suma1 = list()
for(n in 1:R){  
  suma1[[n]] <- lista[[n]]["fcost"]
}
d1 = do.call(cbind, suma1)
sum1 <- data.frame(stamp, fcost =rowMeans(d1), sdvfc = apply(d1,1,sd))

#coa cost
suma2 = list()
for(n in 1:R){  
  suma2[[n]] <- lista[[n]][c("ghgc")]
}
d2 = do.call(cbind, suma2)
sum2 <- data.frame(stamp, ghgc =rowMeans(d2), sdvghg = apply(d2,1,sd))

#ramp cost
suma3 = list()
for(n in 1:R){  
  suma3[[n]] <- lista[[n]][c("rampc")]
}
d33 = do.call(cbind, suma3)
sum33 <- data.frame(stamp, rampc =rowMeans(d33), sdvramp = apply(d33,1,sd))


#total marginal costs
suma = list()
for(n in 1:R){  
  suma[[n]] <- lista[[n]]["tcr"]
}
d = do.call(cbind, suma) 
sum3 <- data.frame(stamp, mumc =rowMeans(d), sdvmc = apply(d,1,sd))


#wind average
suma3 = list()
for(n in 1:R){  
  suma3[[n]] <- lista[[n]]["wind"]
}
d3 = do.call(cbind, suma3) 
sum4 <- data.frame(stamp, muwind =rowMeans(d3), sdvwind = apply(d3,1,sd))


#solar average
suma4 = list()
for(n in 1:R){  
  suma4[[n]] <- lista[[n]]["solar"]
}
d4 = do.call(cbind, suma4) 
sum5 <- data.frame(stamp, musolar =rowMeans(d4), sdvsolar = apply(d4,1,sd))


rd <- lista[[1]]["rd"]

price <- lista[[1]]["price"]

coal <- lista[[1]]["coal"]
gas <- lista[[1]]["gas"]
oil <- lista[[1]]["oil"]
coa <- lista[[1]]["coa"]


hour <- lista[[1]]["hour"]


colnames(real_time) <- "time"
real_time <- real_time %>% distinct()

dos <- cbind.data.frame(real_time, sum1, sum2[,-c(1)], sum33[,-c(1)], sum3[,-c(1)], sum4[,-c(1)], sum5[,-c(1)], rd, price,coal, gas, oil, coa, hour)
dos <- dos[,-c(2)]

rm(d,d1,d2, rd, price, hour, sum1, sum2, sum33, sum3, suma1, suma2, suma, n, Obs, R, stamp)


stamp <- namemc1[,c(1:2)]
dayc <- rep(c("Mo", "Tu", "W", "Th", "Fr", "Sa", "Su"),each =24)
day <- rep(c(dayc), 91)
ja <- cbind.data.frame(stamp,day)
attach(ja)
ja$week[day=="Mo" | day=="Tu" | day=="W" | day=="Th" | day=="Fr" ] <- "work"
ja$week[day=="Sa" | day=="Su" ] <- "end"
detach(ja)
aa <- join( real_time, ja, by = "time")
aa$month <- as.numeric(substring(aa$Date,6,7))
aa$year <- as.numeric(substring(aa$Date,1,4))

colnames(dos)[1] <- "time"

sfe_two <- join( dos, aa, by = "time")

attach(sfe_two)
sfe_two$block[6<=hour & hour<=9] <- "peak1"
sfe_two$block[13<=hour & hour<=16] <- "peak2"
sfe_two$block[20<=hour & hour<=21] <- "peak3"
sfe_two$block[10<=hour & hour<=12] <- "sppeak1"
sfe_two$block[17<=hour & hour<=19] <- "sppeak2"
sfe_two$block[1<=hour & hour<=5] <- "off"
sfe_two$block[22<=hour & hour<=24] <- "off"
detach(sfe_two)


attach(sfe_two)
sfe_two$period[4<=month & month<=9] <- "Summer"
sfe_two$period[1<=month & month<=3] <- "Winter"
sfe_two$period[10<=month & month<=12] <- "Winter"
detach(sfe_two)

rm(a, aa, coa, coal, gas, oil, sfe1, dos)

write_xlsx(sfe_two, "Output/sfe_two.xlsx")

#HISTOGRAMS MC-------------------------------------------------------
#histogram
ggplot(data=sfe_two, aes(mumc)) +
  geom_histogram(binwidth=0.5) +
  scale_x_continuous(breaks = seq(0, 140, 30), lim = c(0, 140)) +
  scale_y_continuous(breaks = seq(0, 700, 200), lim = c(0, 700))
print(proc.time()[3]-p0)

#############################################################################################
#MC CALCULATION - END PROCESS
#############################################################################################

#############################################################################################
#SUMMARIES OF DATA and TABLES
#############################################################################################

#Figure 3: Prices
#####################################################################################
remove(list = ls())

f2 <- read_xlsx("inst.xlsx")
d <- create_series('2017-01-02' ~ '2018-09-30', '1 hour')
f2 <- cbind(d, f2)

figure2 <- ggplot(f2, aes(x=date)) + 
  geom_line(aes(y = price, color = "Electricity"), size=0.05) + 
  geom_line(aes(y = coal, color="Coal"), size=0.5)+
  geom_line(aes(y = gas, color="Gas"), size=0.5) +
  geom_line(aes(y = oil, color="Oil"), size=0.5) +
  geom_line(aes(y = coa, color="CO2", size=1), size=1)+
  geom_vline(xintercept = as.numeric(f2$date[c(8737)]), 
             color = "black", linetype="dashed", size=0.5)+
  scale_colour_manual("", 
                      values = c("Electricity"="red", "Coal"="black", "Gas"="purple","Oil"="darkgreen", "CO2"="blue" )) +
  ylim(-100, 150)+
  theme(axis.text.x = element_text(size=18))+
  theme(axis.text.y = element_text(size=18))+
  xlab('Hour')+ ylab('Euro/MWh')+
  theme(legend.position = c(0.8, 0.95),
        legend.direction = "horizontal", legend.text = element_text(size=18),legend.background= element_blank(), panel.background = element_blank(), panel.grid.major=element_line(linetype="solid", colour="gray91"), axis.line = element_line(colour = "black") , plot.title = element_text(hjust = 0.5, size=18), axis.title.x = element_text(size=18),axis.title.y = element_text(size=18))

pdf(file = "Output/FA3.pdf",  
    width = 18, 
    height = 10) 
plot(figure2)
dev.off()


#Figure 4: Hourly consumption
#####################################################################################
remove(list = ls())

f3<- read_xlsx("resdm5.xlsx")
f3 <- f3[,c(2:18)]/1000
d <- create_series('2017-01-02' ~ '2018-09-30', '1 hour')
f3 <- cbind(d, f3)

f3a <- d %>%
  mutate(demand = f3$demand,
         solar = f3$Photovoltaik.MWh.,
         woff = f3$Wind.Offshore.MWh.,
         won = f3$Wind.Onshore.MWh.) %>%
  collapse_by("daily") %>%
  dplyr::group_by(date) %>%
  dplyr::summarise_all(mean)

f3b <- na.omit(f3a)

figure3 <- ggplot(f3b, aes(x=date)) + 
  geom_line(aes(y = (demand), color = "demand"), size=0.2) + 
  geom_line(aes(y = (solar), color="solar") )+
  geom_line(aes(y = (woff), color="Woff")) +
  geom_line(aes(y = (won), color="Won")) +
  geom_vline(xintercept = (f3b$date[c(8737)]), 
             color = "black", linetype="dashed", size=0.5)+
  scale_colour_manual("", 
                      values = c("demand"="black", "solar"="yellow", "Woff"="lightblue","Won"="darkblue" )) +
  theme(legend.position = c(0.8, 0.95),
        legend.direction = "horizontal", legend.text = element_text(size=18), legend.title = element_blank(),legend.background= element_blank(), panel.background = element_blank(), panel.grid.major=element_line(linetype="solid", colour="gray91"), axis.line = element_line(colour = "black") , plot.title = element_text(hjust = 0.5, size=12), axis.title.x = element_text(size=18),axis.title.y = element_text(size=18))+
  theme(axis.text.x = element_text(size=18))+
  theme(axis.text.y = element_text(size=18))+
  labs(x = "Hour",
       y = "GWh",
       color = "Legend")
figure3

pdf(file = "Output/FA4.pdf",  
    width = 18, 
    height = 10) 
plot(figure3)
dev.off()



#Table A8: SFE markups
remove(list = ls())

mk <- read_xlsx("Output/sfe_two.xlsx")
mk[mk =="peak3" ] <- "off"
mk[mk =="sppeak1"] <- "peak1"
mk[mk=="sppeak2"] <- "peak2"
load("Output/dos.RData")
Resdm5 <- read_xlsx("resdm5.xlsx")
#cap
cap = list()
for(n in 1:50){  
  cap[[n]] <- lista[[n]]["cap"]
}
c = do.call(cbind, cap) 
cap1 <- data.frame(lista[[1]]["time"], cap =rowMeans(c))
mk1 <- merge(mk, cap1, by = "time")
mk2 <- with(mk1, subset(mk1,  fcost>1))
must <- Resdm5[,c(1,16)]
must$time <- c(1:15288)
mk3 <- join(mk2, must, by = "time")
mk3$mks <- (mk2$price - mk2$fcost) / mk2$price
mk3$Q <- mk3$must + mk3$cap
mk3$eta <- -125*(mk3$price/mk3$Q)
mk3$theta <- abs(mk3$mks*mk3$eta) 
mk4 <- mk3[, c(26,31,33,34)]
mk5 <- with( mk4, subset(mk4,  !is.na(theta)))

library(plyr)
tablea8 <- mk5 %>%
  group_by(block)%>%
  summarise_all(funs(mean=mean, sd=sd))

write_xlsx(tablea8, "Output/TA8_SFE.xlsx")

rm(c, cap, cap1, lista, mk, mk1, mk2, mk3, mk4, mk5, must, Resdm5, n)







