#####################################################################################
# Expansion of Intermittent Renewables: Strategies, pass-through costs, and welfare distribution
# Processing raw data, descriptive tables, graphs of data, and building panel data for Stata and python
# Period 02.01.2017 - 30.09.2018
# Author : GC
# Created: 25.09.19
# Last date updated: 27.05.2022
#####################################################################################
#Input files:
#1. I.xlsx                - Electricity production per plant 
#2. chp.xlsx              - chp production and probabilities
#3. namemchp.xlsx         - Plant capacities, names, and technologies
#4. firmpy.xlsx           - firms and plants match
#5. resdm5.xlsx           - Electricity production per technology, demand, renewanbles production
#6. inst.xlsx             - Day-ahead hourly electricity prices and daily fuel prices
#7. loadfa.xlsx           - load factors as actual prod in hour / max prod in the whole period of analysis
#8. Marginalcosts.xlsx    - fuel costs and Co2 costs
#9. temper.xlsx           - temperatures per plant location
#10. solar.xlsx           - solar radiation plant location
#11. wind.xlsx            - wind speed plant location
#12. ramp.xlsx            - Ramping costs

#####################################################################################
#Output files:  
#1. sharesl.xlsx           - Total shares per market T
#2. i1rv.cvs               - Cvs data to read in python (changing Carbon costs 25, 60, 100)
#3. TA1_blp                - Additional descriptive data for Table 1 of the paper
#4. TA8_blp                - Additional result data for Table 8 of the paper, blp method
#5. A7,8,9,10,11,12,13,14  - Appendix figures


#####################################################################################
#Required packages
  install.packages('tidyverse')
  library('tidyverse')
  install.packages('tibbletime')
  library(tibbletime)
  library(lubridate)
  install.packages("reshape2")
  library("reshape2")
  install.packages("reshape")
  library("reshape")
  install.packages("data.table")
  library("data.table")
  install.packages('dummies')
  library('dummies')
  install.packages("caret")
  library("caret")
  install.packages("corrplot")
  library("corrplot")
  install.packages('writexl')
  library('writexl')
  library(readxl)
  install.packages("psych")
  library("psych")
  install.packages("gridExtra")
  library("gridExtra")
  install.packages('xts')
  library('xts')
  install.packages("pastecs")
  library("pastecs")
  install.packages("openxlsx")
  library(openxlsx)



remove(list = ls())

#DATA CONSTRUCTION FOR PYTHON PYPBLP - START
#############################################################################################
p0 <- proc.time()[3]
setwd(dirname(rstudioapi::getActiveDocumentContext()$path)) #directory where the R script is located
ener <- data.frame(read_excel("I.xlsx", col_types = c("text", 
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
                                                      "numeric"
                                                      )))




ener1 <- ener[-c(1), c(3:119)]

chp <- data.frame(read_excel("chp.xlsx", col_types = c("text", 
                                            "numeric", "numeric"
)))

gaschp <- chp[-c(1:3),-c(1:2)]
ener2 <- cbind.data.frame(ener1, gaschp)

ener3 <- data.frame(stack(ener2))
ener4 <- ener3[, c(2,1)] 
time <- rep(c(1:15288),118)
ener5 <- cbind.data.frame(ener4, time)
ener5$ix <- with(ener4, paste(ind, time))
ener5 <- ener5[,-c(1)]

##renaming
colnames(ener5) <- c( "ener", "time", "ix")

rm(ener1, ener3, ener4, gaschp)


######################################
#adding probabilities chp
prob2 <- data.frame(read_excel("chp.xlsx", sheet="probchp"))
prob2 <- prob2[,-c(1)]

namemc <- data.frame(read_excel("namemchp.xlsx", col_types = c("text", 
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
                                                    "numeric")))

pr <- namemc[-c(1:3),-c(1:2, 120:141)]
pr[c(2:15289), c(1:117)] <- 1

cchp <- which( pr[1,] == "53") #coal index
gchp <- which( pr[1,] == "55") #gas index         

pr <- pr[-c(1),]
pr[,cchp] <- prob2$coalchp
pr[,gchp] <- prob2$gaschp

pr$gaschp <- 1

prm1 <- stack(pr)
prm2 <- prm1[, c(2,1)]
prm3 <- cbind.data.frame(prm2, time)
prm3$ix <- with(prm3, paste(ind, time))
prm3 <- prm3[,-c(1,3)]
colnames(prm3) <- c( "probchp", "ix")

py1a <- merge(ener5, prm3, by="ix")

rm(pr,prm1, ener, prm2, prob2, cchp, gchp)

set.seed(10)
Fun <- function(k){as.data.frame(rbinom(length(py1a$time), 1, prob = py1a[[4]]))}
a <- lapply(1:10, Fun)

b = list()
for(n in 1:10){ 
  b[[n]] <- a[[n]][1]
}
b = do.call(cbind, b)

c <- as.data.frame(rowMeans(b))
colnames(c) <- c("probr")

py2a <- cbind.data.frame(py1a, c)

#chp proportions running
py2b <- py2a[,-c(5:6)]
py2b$enerchp <- py2b$ener*py2b$probchp
py2b$plant <- gsub('[[:space:]].*', '', py2b$ix)

py3b <- py2b[, c(3,5,6)]
py4b <- py3b[order(py3b$time),] 
id <- rep(c(1:118),15288)
py5b <- cbind.data.frame(id, py4b)
py6b <- acast(py5b, time~plant, value.var = "enerchp")
py7b <- data.frame(py6b)

rm(py1a, py2a, py2b,  py3b, py4b,  py5b,  py6b, n, id, Fun,a,b,c)

m <- c(1:nrow(ener5))
stamp <- rep(c(namemc[-c(1:4),c(1)]),118)
id<- rep(c(1:118), each=15288)
hour <- rep(c(24, 1:23), 75166)
dayc <- rep(c("Mo", "Tu", "W", "Th", "Fr", "Sa", "Su"),each =24)
day <- rep(c(dayc), 10738)
month <- as.numeric(substring(stamp,6,7))
year <- as.numeric(substring(stamp,1,4))
rm(dayc)

#building market_ids
market_idsx <- paste('T',str_pad(time, 5, pad = "0"))
market_ids <- gsub('\\s+', '', market_idsx)

py1 <- cbind.data.frame(m,id,market_ids, time, hour, day, month, year)

#####################################
#firms to built product_ids

firm <- data.frame(read_excel("firmpy.xlsx"))
firm1 <- firm[, -c(1,3)]

py1a <- merge.data.frame(py1, firm1, by="id")
product_idsx <- paste('F',str_pad(py1a$firm_ids, 2, pad = "0"), 'P', str_pad(py1$id, 3, pad = "0"))
product_ids <- gsub('\\s+', '', product_idsx)


py2 <- cbind.data.frame(py1, product_ids)
py3 <- merge(py2, firm1, by="id")

py4 <- py3%>%select(-1,1)
setnames(py4, "id", "plant_ids") 

rm(firm1)
rm(day, hour, market_ids, product_ids, month, stamp, year, time, py1, py2, py1a, prm3, ener5, id)

#####################################
#adding shares
MW <- py7b
MW$sump <- rowSums(MW)

dem <- read_xlsx("resdm5.xlsx")
MW$demand <- dem$demand
MW$out <- MW$demand - MW$sump
sh <- round(MW[1:15288,1:118]/MW[,120],6)
sh1 <- sh[names(ener2)]
sh2 <- data.frame(stack(sh1))
sh2a <- sh2[,c(1)]
sh3 <- cbind.data.frame(sh2a,m)
colnames(sh3) <- c('shares', 'm')
py5 <- merge(py4, sh3, by="m")
rm(sh, sh1, sh2, sh2a, sh3)
rm(MW, py3, py7b)

#####################################
#add prices from first stage stata
fs <- data.frame(read_excel("inst.xlsx"))
py5$prices <-  fs[,"price"]

#####################################
#adding loadf

lf <- data.frame(read_excel("loadfa.xlsx"))
chp1 <- chp[-c(1:3),]
chp1$chplf <- chp1$gaschp/max(chp1$gaschp)
lf2<- lf[-c(1),-c(1:2)]
lf2$gaschp <- chp1$chplf
lf3<- data.frame(stack(lf2))
lf4<- lf3[,-c(2)]
lf5 <- cbind.data.frame(lf4,m)
colnames(lf5) <- c('loadf', 'm')
py6 <- merge(py5, lf5, by="m")
rm(lf, lf2, lf3, lf4, lf5, chp1, chp)

#####################################
#add fuel costs from inst for random logit

fc <- data.frame(read_excel("Marginalcosts.xlsm", sheet="Tab1"))
fc1 <- fc[-c(1:2),-c(1:2, 120:129)]
fc2 <- data.frame(stack(fc1))
fc3 <- fc2[,-c(2)]
fc4 <- cbind.data.frame(fc3,m)
colnames(fc4) <- c('fcost', 'm')

py7 <- merge(py6, fc4, by="m")
rm(fc, fc1, fc2, fc3, fc4)

#####################################
#add CO2 costs from inst for random logit
fco <- data.frame(read_excel("Marginalcosts.xlsm", sheet="Tab2"))
fco1 <- fco[-c(1:2),-c(1:2, 120:129)]
fco2 <- data.frame(stack(fco1))
fco3 <- fco2[,-c(2)]
fco4 <- cbind.data.frame(fco3,m)
colnames(fco4) <- c('fcoa', 'm')
py8 <- merge(py7, fco4, by="m")
rm(fco, fco1, fco2, fco3, fco4, py6)


#####################################
#adding temp
temper <- data.frame(read_excel("temper.xlsx"))
temper2<- temper[-c(1:2),-c(1:2)]
temper3<- data.frame(stack(temper2))
temper4<- temper3[,-c(2)]
temper5 <- cbind.data.frame(temper4,m)
colnames(temper5) <- c('demand_instruments0', 'm')
py9 <- merge(py8, temper5, by="m")
rm(temper2, temper3, temper4, temper5,temper, py7)

#####################################
#adding solar
solar <- data.frame(read_excel("solar.xlsx"))
solar2<- solar[-c(1:2),-c(1:2, 120:129)]
solar3<- data.frame(stack(solar2))
solar4<- solar3[,-c(2)]
solar5 <- cbind.data.frame(solar4,m)
colnames(solar5) <- c('solar', 'm')
py91 <- merge(py9, solar5, by="m")
rm(solar2, solar3, solar4, solar5,solar)

#####################################
#adding wind
wind <- data.frame(read_excel("wind.xlsx"))
wind2<- wind[-c(1:2),-c(1:2, 120:129)]
wind3<- data.frame(stack(wind2))
wind4<- wind3[,-c(2)]
wind5 <- cbind.data.frame(wind4,m)
colnames(wind5) <- c('wind', 'm')
py92 <- merge(py91, wind5, by="m")
rm(wind2, wind3, wind4, wind5,wind)

#####################################
#Adding renewables
ren <- rep(dem$ren,118)
ren2 <- cbind.data.frame(m, ren)
py93 <- merge(py92, ren2, by="m")
rm(ren, ren2)

#####################################
#Adding demand
demand <- rep(dem$demand,118)
dm2 <- cbind.data.frame(m, demand)
py94 <- merge(py93, dm2, by="m")
rm(demand, dm2, py91, py92)


######################################
#technologies
cate <- namemc[c(1), c(3:119)]
cate$gaschp <- 17
cate1 <- stack(cate)
colnames(cate1) <- c("nesting_ids", "plant") 
tech <- c("biom","wasser",	"windon", "windoff",	"solar",	"serne",	"kerne",	"braunk",	"steink",	"gasoc",	"pump", "oil", "skonv" ,"gasst", "gascc" , "gaschp")
techc <- c(1,       2,         4,        3,         5,        12,        7,        8 ,       9 ,      10,    11,     6     , 13      , 14,      15     ,    17  )
techa <- data.frame(tech, techc)
colnames(techa) <- c("tech", "nesting_ids")
techb <- merge(cate1, techa, by="nesting_ids")
firm3 <-firm[,-c(3:4)]
techc <- merge(firm3, techb, by="plant")
colnames(techc)[2] <- "plant_ids"

py10 <- merge(py94, techc, by="plant_ids")

rm(cate, cate1, tech, techb, techc, firm3,m)


#####################################
#fuel prices
attach(py10)
py10$supply_instruments0[tech=="steink"] <- fs$coal
py10$supply_instruments0[tech=="gasoc"] <- fs$coal
py10$supply_instruments0[tech=="gasst"] <- fs$coal
py10$supply_instruments0[tech=="gascc"] <- fs$coal
py10$supply_instruments0[tech=="oil"]   <- fs$coal
py10$supply_instruments0[tech=="skonv"] <- fs$coal
detach(py10)
py10$supply_instruments0[is.na(py10$supply_instruments0)] <- 0


attach(py10)
py10$supply_instruments1[tech=="steink"] <- fs$gas
py10$supply_instruments1[tech=="gasoc"] <- fs$gas
py10$supply_instruments1[tech=="gasst"] <- fs$gas
py10$supply_instruments1[tech=="gascc"] <- fs$gas
py10$supply_instruments1[tech=="oil"]   <- fs$gas
py10$supply_instruments1[tech=="skonv"] <- fs$gas
detach(py10)
py10$supply_instruments1[is.na(py10$supply_instruments1)] <- 0


attach(py10)
py10$supply_instruments2[tech=="steink"] <- fs$coa
py10$supply_instruments2[tech=="gasoc"] <- fs$coa
py10$supply_instruments2[tech=="gasst"] <- fs$coa
py10$supply_instruments2[tech=="gascc"] <- fs$coa
py10$supply_instruments2[tech=="oil"]   <- fs$coa
py10$supply_instruments2[tech=="skonv"] <- fs$coa
py10$supply_instruments2[tech=="braunk"] <- fs$coa
detach(py10)
py10$supply_instruments2[is.na(py10$supply_instruments2)] <- 0

######################################
#ramping costs
ramp <- data.frame(read_excel("ramp.xlsx", col_types = c("text", 
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
                                                           "numeric", "numeric", "numeric", "numeric", "numeric"
                                                           
                                                           
                                                           
)))


ramp1 <- ramp[-c(1:2), -c(1:2, 120:129)]
ramp2 <- data.frame(stack(ramp1))
ramp3 <- ramp2[, c(2,1)]
m <- c(1:1803984)
ramp4 <- cbind.data.frame(ramp3, m)
ramp4 <- ramp4[,-c(1)]
rm(ramp1, ramp2,ramp3)
colnames(ramp4) <- c( "rampc", "m")
py11 <- merge(py10, ramp4, by="m")
py12 <- py11%>%select(-plant_ids,plant_ids)

#cleaning the data
ch <- sapply(py12, function(x) sum(is.na(x)))
ch1 <- py12[!complete.cases(py12), ]
ch2 <- unique(ch1$time)
pyclean <- py12
pyclean = filter(pyclean, !(time %in% ch2))

summary(pyclean) #1695306

#################################################################################
#VERIFICATIONS --
#N=1556877
#taking out shares close equal to 0 or 1: N=1027140
#loss= 1- 1027140/1695306
vf <- with(pyclean, subset(pyclean, shares>0 & shares<0.99))

#taking out periods where the the total sum of shares is 1:
Summary = vf %>%
  group_by(time = floor(time)) %>%
  summarise(shares = sum(shares))

#N= 1 market that sum higher than  0.99 in shares
check <- with(Summary, subset(Summary,  shares>=0.99))

#N= 0 markets that sum 0 in shares
check2 <- with(Summary, subset(Summary,  shares<=0))
b <- c(check$time)
d <- c(check2$time)

#final total N= 1027140, loss = 1 - 1027140/1695306, total final loss of 0.39 data
vf1 <- vf[!vf$time %in% b, ]
vf2v <- vf1[!vf1$time %in% d, ]


Summary2 = vf2v %>%
  group_by(time = floor(time)) %>%
  summarise(shares = sum(shares))

write_xlsx(Summary2, "Output/sharesl.xlsx") #shares that we are using in T markets

check3 <- with(Summary2, subset(Summary2, shares>=0.99)) #zero 
rm(check, check2, Summary, check3)

#cheecking NAs in final dataset vf2
sum(apply(vf2v, 1, anyNA)) #zero 

rm(Summary2, py11, py12, py10, ramp, ramp4,pyclean, vf, vf1)
rm(ch1, firm, fs, py4, py5, py8, py9, py93, py94, ch, ch2, b,d,m)


#################################################################################
#EXPORT TO CVS USE IN PYTHON

attach(vf2v)
vf2v$period[4<=month & month<=9] <- "Summer"
vf2v$period[1<=month & month<=3] <- "Winter"
vf2v$period[10<=month & month<=12] <- "Winter"
detach(vf2v)


attach(vf2v)
vf2v$block[6<=hour & hour<=9] <- "peak1"
vf2v$block[13<=hour & hour<=16] <- "peak2"
vf2v$block[20<=hour & hour<=21] <- "off"
vf2v$block[10<=hour & hour<=12] <- "peak1"
vf2v$block[17<=hour & hour<=19] <- "peak2"
vf2v$block[1<=hour & hour<=5] <- "off"
vf2v$block[22<=hour & hour<=24] <- "off"
detach(vf2v)

#vf2v <- cbind(vf2v, dummy(vf2v$block, sep = "_"))

attach(vf2v)
vf2v$week[day=="Mo" | day=="Tu" | day=="W" | day=="Th" | day=="Fr" ] <- "work"
vf2v$week[day=="Sa" | day=="Su" ] <- "end"
detach(vf2v)


vf2v <- vf2v[,-c(1)]

#Output dir file created when running SFE.R
#Output <- dir.create("Output")
write.csv(vf2v, file="Output/i1rv.cvs")

#################################################################################
#Additional data for Table 1

T1_m <- stat.desc(vf2v) 
T1_m$desc <- rownames(T1_m)
T1_m <- T1_m[,c(ncol(T1_m),1:(ncol(T1_m)-1))]

write_xlsx(T1_m, "Output/TA1_blp.xlsx")

#################################################################################

#COUNTERFACTUALS METHOD 2
  
#################################################################################

remove(list = ls())
### CF2 - replace 25

setwd(dirname(rstudioapi::getActiveDocumentContext()$path)) #directory where the R script is located
ener <- data.frame(read_excel("I.xlsx", col_types = c("text", 
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
                                                      "numeric"
)))




ener1 <- ener[-c(1), c(3:119)]

chp <- data.frame(read_excel("chp.xlsx", col_types = c("text", 
                                                       "numeric", "numeric"
)))

gaschp <- chp[-c(1:3),-c(1:2)]
ener2 <- cbind.data.frame(ener1, gaschp)

ener3 <- data.frame(stack(ener2))
ener4 <- ener3[, c(2,1)] 
time <- rep(c(1:15288),118)
ener5 <- cbind.data.frame(ener4, time)
ener5$ix <- with(ener4, paste(ind, time))
ener5 <- ener5[,-c(1)]

##renaming
colnames(ener5) <- c( "ener", "time", "ix")

rm(ener1, ener3, ener4, gaschp)


######################################
#adding probabilities chp
prob2 <- data.frame(read_excel("chp.xlsx", sheet="probchp"))
prob2 <- prob2[,-c(1)]

namemc <- data.frame(read_excel("namemchp.xlsx", col_types = c("text", 
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
                                                               "numeric")))

pr <- namemc[-c(1:3),-c(1:2, 120:141)]
pr[c(2:15289), c(1:117)] <- 1

cchp <- which( pr[1,] == "53") #coal index
gchp <- which( pr[1,] == "55") #gas index         

pr <- pr[-c(1),]
pr[,cchp] <- prob2$coalchp
pr[,gchp] <- prob2$gaschp

pr$gaschp <- 1

prm1 <- stack(pr)
prm2 <- prm1[, c(2,1)]
prm3 <- cbind.data.frame(prm2, time)
prm3$ix <- with(prm3, paste(ind, time))
prm3 <- prm3[,-c(1,3)]
colnames(prm3) <- c( "probchp", "ix")

py1a <- merge(ener5, prm3, by="ix")

rm(pr,prm1, ener, prm2, prob2, cchp, gchp)

set.seed(10)
Fun <- function(k){as.data.frame(rbinom(length(py1a$time), 1, prob = py1a[[4]]))}
a <- lapply(1:10, Fun)

b = list()
for(n in 1:10){ 
  b[[n]] <- a[[n]][1]
}
b = do.call(cbind, b)

c <- as.data.frame(rowMeans(b))
colnames(c) <- c("probr")

py2a <- cbind.data.frame(py1a, c)

#chp proportions running
py2b <- py2a[,-c(5:6)]
py2b$enerchp <- py2b$ener*py2b$probchp
py2b$plant <- gsub('[[:space:]].*', '', py2b$ix)

py3b <- py2b[, c(3,5,6)]
py4b <- py3b[order(py3b$time),] 
id <- rep(c(1:118),15288)
py5b <- cbind.data.frame(id, py4b)
py6b <- acast(py5b, time~plant, value.var = "enerchp")
py7b <- data.frame(py6b)

rm(py1a, py2a, py2b,  py3b, py4b,  py5b,  py6b, n, id, Fun,a,b,c)

m <- c(1:nrow(ener5))
stamp <- rep(c(namemc[-c(1:4),c(1)]),118)
id<- rep(c(1:118), each=15288)
hour <- rep(c(24, 1:23), 75166)
dayc <- rep(c("Mo", "Tu", "W", "Th", "Fr", "Sa", "Su"),each =24)
day <- rep(c(dayc), 10738)
month <- as.numeric(substring(stamp,6,7))
year <- as.numeric(substring(stamp,1,4))
rm(dayc)

#building market_ids
market_idsx <- paste('T',str_pad(time, 5, pad = "0"))
market_ids <- gsub('\\s+', '', market_idsx)

py1 <- cbind.data.frame(m,id,market_ids, time, hour, day, month, year)

#####################################
#firms to built product_ids

firm <- data.frame(read_excel("firmpy.xlsx"))
firm1 <- firm[, -c(1,3)]

py1a <- merge.data.frame(py1, firm1, by="id")
product_idsx <- paste('F',str_pad(py1a$firm_ids, 2, pad = "0"), 'P', str_pad(py1$id, 3, pad = "0"))
product_ids <- gsub('\\s+', '', product_idsx)


py2 <- cbind.data.frame(py1, product_ids)
py3 <- merge(py2, firm1, by="id")

py4 <- py3%>%select(-1,1)
setnames(py4, "id", "plant_ids") 

rm(firm1)
rm(day, hour, market_ids, product_ids, month, stamp, year, time, py1, py2, py1a, prm3, ener5, id)

#####################################
#adding shares
MW <- py7b
MW$sump <- rowSums(MW)

dem <- read_xlsx("resdm5.xlsx")
MW$demand <- dem$demand
MW$out <- MW$demand - MW$sump
sh <- round(MW[1:15288,1:118]/MW[,120],6)
sh1 <- sh[names(ener2)]
sh2 <- data.frame(stack(sh1))
sh2a <- sh2[,c(1)]
sh3 <- cbind.data.frame(sh2a,m)
colnames(sh3) <- c('shares', 'm')
py5 <- merge(py4, sh3, by="m")
rm(sh, sh1, sh2, sh2a, sh3)
rm(MW, py3, py7b)

#####################################
#add prices from first stage stata
fs <- data.frame(read_excel("inst.xlsx"))
py5$prices <-  fs[,"price"]

#####################################
#adding loadf

lf <- data.frame(read_excel("loadfa.xlsx"))
chp1 <- chp[-c(1:3),]
chp1$chplf <- chp1$gaschp/max(chp1$gaschp)
lf2<- lf[-c(1),-c(1:2)]
lf2$gaschp <- chp1$chplf
lf3<- data.frame(stack(lf2))
lf4<- lf3[,-c(2)]
lf5 <- cbind.data.frame(lf4,m)
colnames(lf5) <- c('loadf', 'm')
py6 <- merge(py5, lf5, by="m")
rm(lf, lf2, lf3, lf4, lf5, chp1, chp)


fc <- data.frame(read_excel("Marginalcosts.xlsm", sheet="Tab1"))
fc1 <- fc[-c(1:2),-c(1:2, 120:129)]
fc2 <- data.frame(stack(fc1))
fc3 <- fc2[,-c(2)]
fc4 <- cbind.data.frame(fc3,m)
colnames(fc4) <- c('fcost', 'm')

py7 <- merge(py6, fc4, by="m")
rm(fc, fc1, fc2, fc3, fc4)


#add CO2 costs from inst for random logit

fco <- data.frame(read_excel("Marginalcosts_25.xlsm", sheet="Tab2"))
fco1 <- fco[-c(1:2),-c(1:2, 121:130)]
fco2 <- data.frame(stack(fco1))
fco3 <- fco2[,-c(2)]
fco4 <- cbind.data.frame(fco3,m)
colnames(fco4) <- c('fcoa', 'm')
py8 <- merge(py7, fco4, by="m")
rm(fco, fco1, fco2, fco3, fco4, py6)
  

#####################################
#adding temp
temper <- data.frame(read_excel("temper.xlsx"))
temper2<- temper[-c(1:2),-c(1:2)]
temper3<- data.frame(stack(temper2))
temper4<- temper3[,-c(2)]
temper5 <- cbind.data.frame(temper4,m)
colnames(temper5) <- c('demand_instruments0', 'm')
py9 <- merge(py8, temper5, by="m")
rm(temper2, temper3, temper4, temper5,temper, py7)

#####################################
#adding solar
solar <- data.frame(read_excel("solar.xlsx"))
solar2<- solar[-c(1:2),-c(1:2, 120:129)]
solar3<- data.frame(stack(solar2))
solar4<- solar3[,-c(2)]
solar5 <- cbind.data.frame(solar4,m)
colnames(solar5) <- c('solar', 'm')
py91 <- merge(py9, solar5, by="m")
rm(solar2, solar3, solar4, solar5,solar)

#####################################
#adding wind
wind <- data.frame(read_excel("wind.xlsx"))
wind2<- wind[-c(1:2),-c(1:2, 120:129)]
wind3<- data.frame(stack(wind2))
wind4<- wind3[,-c(2)]
wind5 <- cbind.data.frame(wind4,m)
colnames(wind5) <- c('wind', 'm')
py92 <- merge(py91, wind5, by="m")
rm(wind2, wind3, wind4, wind5,wind)

#####################################
#Adding renewables
ren <- rep(dem$ren,118)
ren2 <- cbind.data.frame(m, ren)
py93 <- merge(py92, ren2, by="m")
rm(ren, ren2)

#####################################
#Adding demand
demand <- rep(dem$demand,118)
dm2 <- cbind.data.frame(m, demand)
py94 <- merge(py93, dm2, by="m")
rm(demand, dm2, py91, py92)


######################################
#technologies
cate <- namemc[c(1), c(3:119)]
cate$gaschp <- 17
cate1 <- stack(cate)
colnames(cate1) <- c("nesting_ids", "plant") 
tech <- c("biom","wasser",	"windon", "windoff",	"solar",	"serne",	"kerne",	"braunk",	"steink",	"gasoc",	"pump", "oil", "skonv" ,"gasst", "gascc" , "gaschp")
techc <- c(1,       2,         4,        3,         5,        12,        7,        8 ,       9 ,      10,    11,     6     , 13      , 14,      15     ,    17  )
techa <- data.frame(tech, techc)
colnames(techa) <- c("tech", "nesting_ids")
techb <- merge(cate1, techa, by="nesting_ids")
firm3 <-firm[,-c(3:4)]
techc <- merge(firm3, techb, by="plant")
colnames(techc)[2] <- "plant_ids"

py10 <- merge(py94, techc, by="plant_ids")

rm(cate, cate1, tech, techb, techc, firm3,m)


#####################################
#fuel prices
attach(py10)
py10$supply_instruments0[tech=="steink"] <- fs$coal
py10$supply_instruments0[tech=="gasoc"] <- fs$coal
py10$supply_instruments0[tech=="gasst"] <- fs$coal
py10$supply_instruments0[tech=="gascc"] <- fs$coal
py10$supply_instruments0[tech=="oil"]   <- fs$coal
py10$supply_instruments0[tech=="skonv"] <- fs$coal
detach(py10)
py10$supply_instruments0[is.na(py10$supply_instruments0)] <- 0


attach(py10)
py10$supply_instruments1[tech=="steink"] <- fs$gas
py10$supply_instruments1[tech=="gasoc"] <- fs$gas
py10$supply_instruments1[tech=="gasst"] <- fs$gas
py10$supply_instruments1[tech=="gascc"] <- fs$gas
py10$supply_instruments1[tech=="oil"]   <- fs$gas
py10$supply_instruments1[tech=="skonv"] <- fs$gas
detach(py10)
py10$supply_instruments1[is.na(py10$supply_instruments1)] <- 0


attach(py10)
py10$supply_instruments2[tech=="steink"] <- fs$coa
py10$supply_instruments2[tech=="gasoc"] <- fs$coa
py10$supply_instruments2[tech=="gasst"] <- fs$coa
py10$supply_instruments2[tech=="gascc"] <- fs$coa
py10$supply_instruments2[tech=="oil"]   <- fs$coa
py10$supply_instruments2[tech=="skonv"] <- fs$coa
py10$supply_instruments2[tech=="braunk"] <- fs$coa
detach(py10)
py10$supply_instruments2[is.na(py10$supply_instruments2)] <- 0

######################################
#ramping costs
ramp <- data.frame(read_excel("ramp.xlsx", col_types = c("text", 
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
                                                         "numeric", "numeric", "numeric", "numeric", "numeric"
                                                         
                                                         
                                                         
)))


ramp1 <- ramp[-c(1:2), -c(1:2, 120:129)]
ramp2 <- data.frame(stack(ramp1))
ramp3 <- ramp2[, c(2,1)]
m <- c(1:1803984)
ramp4 <- cbind.data.frame(ramp3, m)
ramp4 <- ramp4[,-c(1)]
rm(ramp1, ramp2,ramp3)
colnames(ramp4) <- c( "rampc", "m")
py11 <- merge(py10, ramp4, by="m")
py12 <- py11%>%select(-plant_ids,plant_ids)

#cleaning the data
ch <- sapply(py12, function(x) sum(is.na(x)))
ch1 <- py12[!complete.cases(py12), ]
ch2 <- unique(ch1$time)
pyclean <- py12
pyclean = filter(pyclean, !(time %in% ch2))

summary(pyclean) #1695306

#################################################################################
#VERIFICATIONS --
#N=1556877
#taking out shares close equal to 0 or 1: N=1027140
#loss= 1- 1027140/1695306
vf <- with(pyclean, subset(pyclean, shares>0 & shares<0.99))

#taking out periods where the the total sum of shares is 1:
Summary = vf %>%
  group_by(time = floor(time)) %>%
  summarise(shares = sum(shares))

#N= 1 market that sum higher than  0.99 in shares
check <- with(Summary, subset(Summary,  shares>=0.99))

#N= 0 markets that sum 0 in shares
check2 <- with(Summary, subset(Summary,  shares<=0))
b <- c(check$time)
d <- c(check2$time)

#final total N= 1027140, loss = 1 - 1027140/1695306, total final loss of 0.39 data
vf1 <- vf[!vf$time %in% b, ]
vf2v <- vf1[!vf1$time %in% d, ]


Summary2 = vf2v %>%
  group_by(time = floor(time)) %>%
  summarise(shares = sum(shares))

write_xlsx(Summary2, "Output/sharesl_25.xlsx") #shares that we are using in T markets

check3 <- with(Summary2, subset(Summary2, shares>=0.99)) #zero 
rm(check, check2, Summary, check3)

#cheecking NAs in final dataset vf2
sum(apply(vf2v, 1, anyNA)) #zero 

rm(Summary2, py11, py12, py10, ramp, ramp4,pyclean, vf, vf1)
rm(ch1, firm, fs, py4, py5, py8, py9, py93, py94, ch, ch2, b,d,m)


#################################################################################
#EXPORT TO CVS USE IN PYTHON

attach(vf2v)
vf2v$period[4<=month & month<=9] <- "Summer"
vf2v$period[1<=month & month<=3] <- "Winter"
vf2v$period[10<=month & month<=12] <- "Winter"
detach(vf2v)


attach(vf2v)
vf2v$block[6<=hour & hour<=9] <- "peak1"
vf2v$block[13<=hour & hour<=16] <- "peak2"
vf2v$block[20<=hour & hour<=21] <- "off"
vf2v$block[10<=hour & hour<=12] <- "peak1"
vf2v$block[17<=hour & hour<=19] <- "peak2"
vf2v$block[1<=hour & hour<=5] <- "off"
vf2v$block[22<=hour & hour<=24] <- "off"
detach(vf2v)

#vf2v <- cbind(vf2v, dummy(vf2v$block, sep = "_"))

attach(vf2v)
vf2v$week[day=="Mo" | day=="Tu" | day=="W" | day=="Th" | day=="Fr" ] <- "work"
vf2v$week[day=="Sa" | day=="Su" ] <- "end"
detach(vf2v)


vf2v <- vf2v[,-c(1)]

write.csv(vf2v, file="Output/i1rv25.cvs")
  
#####################
# CF2 - replace 60
#####################


remove(list = ls())
### CF2 - replace 60

setwd(dirname(rstudioapi::getActiveDocumentContext()$path)) #directory where the R script is located
ener <- data.frame(read_excel("I.xlsx", col_types = c("text", 
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
                                                      "numeric"
)))




ener1 <- ener[-c(1), c(3:119)]

chp <- data.frame(read_excel("chp.xlsx", col_types = c("text", 
                                                       "numeric", "numeric"
)))

gaschp <- chp[-c(1:3),-c(1:2)]
ener2 <- cbind.data.frame(ener1, gaschp)

ener3 <- data.frame(stack(ener2))
ener4 <- ener3[, c(2,1)] 
time <- rep(c(1:15288),118)
ener5 <- cbind.data.frame(ener4, time)
ener5$ix <- with(ener4, paste(ind, time))
ener5 <- ener5[,-c(1)]

##renaming
colnames(ener5) <- c( "ener", "time", "ix")

rm(ener1, ener3, ener4, gaschp)


######################################
#adding probabilities chp
prob2 <- data.frame(read_excel("chp.xlsx", sheet="probchp"))
prob2 <- prob2[,-c(1)]

namemc <- data.frame(read_excel("namemchp.xlsx", col_types = c("text", 
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
                                                               "numeric")))

pr <- namemc[-c(1:3),-c(1:2, 120:141)]
pr[c(2:15289), c(1:117)] <- 1

cchp <- which( pr[1,] == "53") #coal index
gchp <- which( pr[1,] == "55") #gas index         

pr <- pr[-c(1),]
pr[,cchp] <- prob2$coalchp
pr[,gchp] <- prob2$gaschp

pr$gaschp <- 1

prm1 <- stack(pr)
prm2 <- prm1[, c(2,1)]
prm3 <- cbind.data.frame(prm2, time)
prm3$ix <- with(prm3, paste(ind, time))
prm3 <- prm3[,-c(1,3)]
colnames(prm3) <- c( "probchp", "ix")

py1a <- merge(ener5, prm3, by="ix")

rm(pr,prm1, ener, prm2, prob2, cchp, gchp)

set.seed(10)
Fun <- function(k){as.data.frame(rbinom(length(py1a$time), 1, prob = py1a[[4]]))}
a <- lapply(1:10, Fun)

b = list()
for(n in 1:10){ 
  b[[n]] <- a[[n]][1]
}
b = do.call(cbind, b)

c <- as.data.frame(rowMeans(b))
colnames(c) <- c("probr")

py2a <- cbind.data.frame(py1a, c)

#chp proportions running
py2b <- py2a[,-c(5:6)]
py2b$enerchp <- py2b$ener*py2b$probchp
py2b$plant <- gsub('[[:space:]].*', '', py2b$ix)

py3b <- py2b[, c(3,5,6)]
py4b <- py3b[order(py3b$time),] 
id <- rep(c(1:118),15288)
py5b <- cbind.data.frame(id, py4b)
py6b <- acast(py5b, time~plant, value.var = "enerchp")
py7b <- data.frame(py6b)

rm(py1a, py2a, py2b,  py3b, py4b,  py5b,  py6b, n, id, Fun,a,b,c)

m <- c(1:nrow(ener5))
stamp <- rep(c(namemc[-c(1:4),c(1)]),118)
id<- rep(c(1:118), each=15288)
hour <- rep(c(24, 1:23), 75166)
dayc <- rep(c("Mo", "Tu", "W", "Th", "Fr", "Sa", "Su"),each =24)
day <- rep(c(dayc), 10738)
month <- as.numeric(substring(stamp,6,7))
year <- as.numeric(substring(stamp,1,4))
rm(dayc)

#building market_ids
market_idsx <- paste('T',str_pad(time, 5, pad = "0"))
market_ids <- gsub('\\s+', '', market_idsx)

py1 <- cbind.data.frame(m,id,market_ids, time, hour, day, month, year)

#####################################
#firms to built product_ids

firm <- data.frame(read_excel("firmpy.xlsx"))
firm1 <- firm[, -c(1,3)]

py1a <- merge.data.frame(py1, firm1, by="id")
product_idsx <- paste('F',str_pad(py1a$firm_ids, 2, pad = "0"), 'P', str_pad(py1$id, 3, pad = "0"))
product_ids <- gsub('\\s+', '', product_idsx)


py2 <- cbind.data.frame(py1, product_ids)
py3 <- merge(py2, firm1, by="id")

py4 <- py3%>%select(-1,1)
setnames(py4, "id", "plant_ids") 

rm(firm1)
rm(day, hour, market_ids, product_ids, month, stamp, year, time, py1, py2, py1a, prm3, ener5, id)

#####################################
#adding shares
MW <- py7b
MW$sump <- rowSums(MW)

dem <- read_xlsx("resdm5.xlsx")
MW$demand <- dem$demand
MW$out <- MW$demand - MW$sump
sh <- round(MW[1:15288,1:118]/MW[,120],6)
sh1 <- sh[names(ener2)]
sh2 <- data.frame(stack(sh1))
sh2a <- sh2[,c(1)]
sh3 <- cbind.data.frame(sh2a,m)
colnames(sh3) <- c('shares', 'm')
py5 <- merge(py4, sh3, by="m")
rm(sh, sh1, sh2, sh2a, sh3)
rm(MW, py3, py7b)

#####################################
#add prices from first stage stata
fs <- data.frame(read_excel("inst.xlsx"))
py5$prices <-  fs[,"price"]

#####################################
#adding loadf

lf <- data.frame(read_excel("loadfa.xlsx"))
chp1 <- chp[-c(1:3),]
chp1$chplf <- chp1$gaschp/max(chp1$gaschp)
lf2<- lf[-c(1),-c(1:2)]
lf2$gaschp <- chp1$chplf
lf3<- data.frame(stack(lf2))
lf4<- lf3[,-c(2)]
lf5 <- cbind.data.frame(lf4,m)
colnames(lf5) <- c('loadf', 'm')
py6 <- merge(py5, lf5, by="m")
rm(lf, lf2, lf3, lf4, lf5, chp1, chp)


fc <- data.frame(read_excel("Marginalcosts.xlsm", sheet="Tab1"))
fc1 <- fc[-c(1:2),-c(1:2, 120:129)]
fc2 <- data.frame(stack(fc1))
fc3 <- fc2[,-c(2)]
fc4 <- cbind.data.frame(fc3,m)
colnames(fc4) <- c('fcost', 'm')

py7 <- merge(py6, fc4, by="m")
rm(fc, fc1, fc2, fc3, fc4)


#add CO2 costs from inst for random logit

fco <- data.frame(read_excel("Marginalcosts_60.xlsm", sheet="Tab2"))
fco1 <- fco[-c(1:2),-c(1:2, 121:130)]
fco2 <- data.frame(stack(fco1))
fco3 <- fco2[,-c(2)]
fco4 <- cbind.data.frame(fco3,m)
colnames(fco4) <- c('fcoa', 'm')
py8 <- merge(py7, fco4, by="m")
rm(fco, fco1, fco2, fco3, fco4, py6)


#####################################
#adding temp
temper <- data.frame(read_excel("temper.xlsx"))
temper2<- temper[-c(1:2),-c(1:2)]
temper3<- data.frame(stack(temper2))
temper4<- temper3[,-c(2)]
temper5 <- cbind.data.frame(temper4,m)
colnames(temper5) <- c('demand_instruments0', 'm')
py9 <- merge(py8, temper5, by="m")
rm(temper2, temper3, temper4, temper5,temper, py7)

#####################################
#adding solar
solar <- data.frame(read_excel("solar.xlsx"))
solar2<- solar[-c(1:2),-c(1:2, 120:129)]
solar3<- data.frame(stack(solar2))
solar4<- solar3[,-c(2)]
solar5 <- cbind.data.frame(solar4,m)
colnames(solar5) <- c('solar', 'm')
py91 <- merge(py9, solar5, by="m")
rm(solar2, solar3, solar4, solar5,solar)

#####################################
#adding wind
wind <- data.frame(read_excel("wind.xlsx"))
wind2<- wind[-c(1:2),-c(1:2, 120:129)]
wind3<- data.frame(stack(wind2))
wind4<- wind3[,-c(2)]
wind5 <- cbind.data.frame(wind4,m)
colnames(wind5) <- c('wind', 'm')
py92 <- merge(py91, wind5, by="m")
rm(wind2, wind3, wind4, wind5,wind)

#####################################
#Adding renewables
ren <- rep(dem$ren,118)
ren2 <- cbind.data.frame(m, ren)
py93 <- merge(py92, ren2, by="m")
rm(ren, ren2)

#####################################
#Adding demand
demand <- rep(dem$demand,118)
dm2 <- cbind.data.frame(m, demand)
py94 <- merge(py93, dm2, by="m")
rm(demand, dm2, py91, py92)


######################################
#technologies
cate <- namemc[c(1), c(3:119)]
cate$gaschp <- 17
cate1 <- stack(cate)
colnames(cate1) <- c("nesting_ids", "plant") 
tech <- c("biom","wasser",	"windon", "windoff",	"solar",	"serne",	"kerne",	"braunk",	"steink",	"gasoc",	"pump", "oil", "skonv" ,"gasst", "gascc" , "gaschp")
techc <- c(1,       2,         4,        3,         5,        12,        7,        8 ,       9 ,      10,    11,     6     , 13      , 14,      15     ,    17  )
techa <- data.frame(tech, techc)
colnames(techa) <- c("tech", "nesting_ids")
techb <- merge(cate1, techa, by="nesting_ids")
firm3 <-firm[,-c(3:4)]
techc <- merge(firm3, techb, by="plant")
colnames(techc)[2] <- "plant_ids"

py10 <- merge(py94, techc, by="plant_ids")

rm(cate, cate1, tech, techb, techc, firm3,m)


#####################################
#fuel prices
attach(py10)
py10$supply_instruments0[tech=="steink"] <- fs$coal
py10$supply_instruments0[tech=="gasoc"] <- fs$coal
py10$supply_instruments0[tech=="gasst"] <- fs$coal
py10$supply_instruments0[tech=="gascc"] <- fs$coal
py10$supply_instruments0[tech=="oil"]   <- fs$coal
py10$supply_instruments0[tech=="skonv"] <- fs$coal
detach(py10)
py10$supply_instruments0[is.na(py10$supply_instruments0)] <- 0


attach(py10)
py10$supply_instruments1[tech=="steink"] <- fs$gas
py10$supply_instruments1[tech=="gasoc"] <- fs$gas
py10$supply_instruments1[tech=="gasst"] <- fs$gas
py10$supply_instruments1[tech=="gascc"] <- fs$gas
py10$supply_instruments1[tech=="oil"]   <- fs$gas
py10$supply_instruments1[tech=="skonv"] <- fs$gas
detach(py10)
py10$supply_instruments1[is.na(py10$supply_instruments1)] <- 0


attach(py10)
py10$supply_instruments2[tech=="steink"] <- fs$coa
py10$supply_instruments2[tech=="gasoc"] <- fs$coa
py10$supply_instruments2[tech=="gasst"] <- fs$coa
py10$supply_instruments2[tech=="gascc"] <- fs$coa
py10$supply_instruments2[tech=="oil"]   <- fs$coa
py10$supply_instruments2[tech=="skonv"] <- fs$coa
py10$supply_instruments2[tech=="braunk"] <- fs$coa
detach(py10)
py10$supply_instruments2[is.na(py10$supply_instruments2)] <- 0

######################################
#ramping costs
ramp <- data.frame(read_excel("ramp.xlsx", col_types = c("text", 
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
                                                         "numeric", "numeric", "numeric", "numeric", "numeric"
                                                         
                                                         
                                                         
)))


ramp1 <- ramp[-c(1:2), -c(1:2, 120:129)]
ramp2 <- data.frame(stack(ramp1))
ramp3 <- ramp2[, c(2,1)]
m <- c(1:1803984)
ramp4 <- cbind.data.frame(ramp3, m)
ramp4 <- ramp4[,-c(1)]
rm(ramp1, ramp2,ramp3)
colnames(ramp4) <- c( "rampc", "m")
py11 <- merge(py10, ramp4, by="m")
py12 <- py11%>%select(-plant_ids,plant_ids)

#cleaning the data
ch <- sapply(py12, function(x) sum(is.na(x)))
ch1 <- py12[!complete.cases(py12), ]
ch2 <- unique(ch1$time)
pyclean <- py12
pyclean = filter(pyclean, !(time %in% ch2))

summary(pyclean) #1695306

#################################################################################
#VERIFICATIONS --
#N=1556877
#taking out shares close equal to 0 or 1: N=1027140
#loss= 1- 1027140/1695306
vf <- with(pyclean, subset(pyclean, shares>0 & shares<0.99))

#taking out periods where the the total sum of shares is 1:
Summary = vf %>%
  group_by(time = floor(time)) %>%
  summarise(shares = sum(shares))

#N= 1 market that sum higher than  0.99 in shares
check <- with(Summary, subset(Summary,  shares>=0.99))

#N= 0 markets that sum 0 in shares
check2 <- with(Summary, subset(Summary,  shares<=0))
b <- c(check$time)
d <- c(check2$time)

#final total N= 1027140, loss = 1 - 1027140/1695306, total final loss of 0.39 data
vf1 <- vf[!vf$time %in% b, ]
vf2v <- vf1[!vf1$time %in% d, ]


Summary2 = vf2v %>%
  group_by(time = floor(time)) %>%
  summarise(shares = sum(shares))

write_xlsx(Summary2, "Output/sharesl_60.xlsx") #shares that we are using in T markets

check3 <- with(Summary2, subset(Summary2, shares>=0.99)) #zero 
rm(check, check2, Summary, check3)

#cheecking NAs in final dataset vf2
sum(apply(vf2v, 1, anyNA)) #zero 

rm(Summary2, py11, py12, py10, ramp, ramp4,pyclean, vf, vf1)
rm(ch1, firm, fs, py4, py5, py8, py9, py93, py94, ch, ch2, b,d,m)


#################################################################################
#EXPORT TO CVS USE IN PYTHON

attach(vf2v)
vf2v$period[4<=month & month<=9] <- "Summer"
vf2v$period[1<=month & month<=3] <- "Winter"
vf2v$period[10<=month & month<=12] <- "Winter"
detach(vf2v)


attach(vf2v)
vf2v$block[6<=hour & hour<=9] <- "peak1"
vf2v$block[13<=hour & hour<=16] <- "peak2"
vf2v$block[20<=hour & hour<=21] <- "off"
vf2v$block[10<=hour & hour<=12] <- "peak1"
vf2v$block[17<=hour & hour<=19] <- "peak2"
vf2v$block[1<=hour & hour<=5] <- "off"
vf2v$block[22<=hour & hour<=24] <- "off"
detach(vf2v)

#vf2v <- cbind(vf2v, dummy(vf2v$block, sep = "_"))

attach(vf2v)
vf2v$week[day=="Mo" | day=="Tu" | day=="W" | day=="Th" | day=="Fr" ] <- "work"
vf2v$week[day=="Sa" | day=="Su" ] <- "end"
detach(vf2v)


vf2v <- vf2v[,-c(1)]

write.csv(vf2v, file="Output/i1rv60.cvs")



#####################
# CF2 - replace 100
#####################


remove(list = ls())
### CF2 - replace 60

setwd(dirname(rstudioapi::getActiveDocumentContext()$path)) #directory where the R script is located
ener <- data.frame(read_excel("I.xlsx", col_types = c("text", 
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
                                                      "numeric"
)))




ener1 <- ener[-c(1), c(3:119)]

chp <- data.frame(read_excel("chp.xlsx", col_types = c("text", 
                                                       "numeric", "numeric"
)))

gaschp <- chp[-c(1:3),-c(1:2)]
ener2 <- cbind.data.frame(ener1, gaschp)

ener3 <- data.frame(stack(ener2))
ener4 <- ener3[, c(2,1)] 
time <- rep(c(1:15288),118)
ener5 <- cbind.data.frame(ener4, time)
ener5$ix <- with(ener4, paste(ind, time))
ener5 <- ener5[,-c(1)]

##renaming
colnames(ener5) <- c( "ener", "time", "ix")

rm(ener1, ener3, ener4, gaschp)


######################################
#adding probabilities chp
prob2 <- data.frame(read_excel("chp.xlsx", sheet="probchp"))
prob2 <- prob2[,-c(1)]

namemc <- data.frame(read_excel("namemchp.xlsx", col_types = c("text", 
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
                                                               "numeric")))

pr <- namemc[-c(1:3),-c(1:2, 120:141)]
pr[c(2:15289), c(1:117)] <- 1

cchp <- which( pr[1,] == "53") #coal index
gchp <- which( pr[1,] == "55") #gas index         

pr <- pr[-c(1),]
pr[,cchp] <- prob2$coalchp
pr[,gchp] <- prob2$gaschp

pr$gaschp <- 1

prm1 <- stack(pr)
prm2 <- prm1[, c(2,1)]
prm3 <- cbind.data.frame(prm2, time)
prm3$ix <- with(prm3, paste(ind, time))
prm3 <- prm3[,-c(1,3)]
colnames(prm3) <- c( "probchp", "ix")

py1a <- merge(ener5, prm3, by="ix")

rm(pr,prm1, ener, prm2, prob2, cchp, gchp)

set.seed(10)
Fun <- function(k){as.data.frame(rbinom(length(py1a$time), 1, prob = py1a[[4]]))}
a <- lapply(1:10, Fun)

b = list()
for(n in 1:10){ 
  b[[n]] <- a[[n]][1]
}
b = do.call(cbind, b)

c <- as.data.frame(rowMeans(b))
colnames(c) <- c("probr")

py2a <- cbind.data.frame(py1a, c)

#chp proportions running
py2b <- py2a[,-c(5:6)]
py2b$enerchp <- py2b$ener*py2b$probchp
py2b$plant <- gsub('[[:space:]].*', '', py2b$ix)

py3b <- py2b[, c(3,5,6)]
py4b <- py3b[order(py3b$time),] 
id <- rep(c(1:118),15288)
py5b <- cbind.data.frame(id, py4b)
py6b <- acast(py5b, time~plant, value.var = "enerchp")
py7b <- data.frame(py6b)

rm(py1a, py2a, py2b,  py3b, py4b,  py5b,  py6b, n, id, Fun,a,b,c)

m <- c(1:nrow(ener5))
stamp <- rep(c(namemc[-c(1:4),c(1)]),118)
id<- rep(c(1:118), each=15288)
hour <- rep(c(24, 1:23), 75166)
dayc <- rep(c("Mo", "Tu", "W", "Th", "Fr", "Sa", "Su"),each =24)
day <- rep(c(dayc), 10738)
month <- as.numeric(substring(stamp,6,7))
year <- as.numeric(substring(stamp,1,4))
rm(dayc)

#building market_ids
market_idsx <- paste('T',str_pad(time, 5, pad = "0"))
market_ids <- gsub('\\s+', '', market_idsx)

py1 <- cbind.data.frame(m,id,market_ids, time, hour, day, month, year)

#####################################
#firms to built product_ids

firm <- data.frame(read_excel("firmpy.xlsx"))
firm1 <- firm[, -c(1,3)]

py1a <- merge.data.frame(py1, firm1, by="id")
product_idsx <- paste('F',str_pad(py1a$firm_ids, 2, pad = "0"), 'P', str_pad(py1$id, 3, pad = "0"))
product_ids <- gsub('\\s+', '', product_idsx)


py2 <- cbind.data.frame(py1, product_ids)
py3 <- merge(py2, firm1, by="id")

py4 <- py3%>%select(-1,1)
setnames(py4, "id", "plant_ids") 

rm(firm1)
rm(day, hour, market_ids, product_ids, month, stamp, year, time, py1, py2, py1a, prm3, ener5, id)

#####################################
#adding shares
MW <- py7b
MW$sump <- rowSums(MW)

dem <- read_xlsx("resdm5.xlsx")
MW$demand <- dem$demand
MW$out <- MW$demand - MW$sump
sh <- round(MW[1:15288,1:118]/MW[,120],6)
sh1 <- sh[names(ener2)]
sh2 <- data.frame(stack(sh1))
sh2a <- sh2[,c(1)]
sh3 <- cbind.data.frame(sh2a,m)
colnames(sh3) <- c('shares', 'm')
py5 <- merge(py4, sh3, by="m")
rm(sh, sh1, sh2, sh2a, sh3)
rm(MW, py3, py7b)

#####################################
#add prices from first stage stata
fs <- data.frame(read_excel("inst.xlsx"))
py5$prices <-  fs[,"price"]

#####################################
#adding loadf

lf <- data.frame(read_excel("loadfa.xlsx"))
chp1 <- chp[-c(1:3),]
chp1$chplf <- chp1$gaschp/max(chp1$gaschp)
lf2<- lf[-c(1),-c(1:2)]
lf2$gaschp <- chp1$chplf
lf3<- data.frame(stack(lf2))
lf4<- lf3[,-c(2)]
lf5 <- cbind.data.frame(lf4,m)
colnames(lf5) <- c('loadf', 'm')
py6 <- merge(py5, lf5, by="m")
rm(lf, lf2, lf3, lf4, lf5, chp1, chp)


fc <- data.frame(read_excel("Marginalcosts.xlsm", sheet="Tab1"))
fc1 <- fc[-c(1:2),-c(1:2, 120:129)]
fc2 <- data.frame(stack(fc1))
fc3 <- fc2[,-c(2)]
fc4 <- cbind.data.frame(fc3,m)
colnames(fc4) <- c('fcost', 'm')

py7 <- merge(py6, fc4, by="m")
rm(fc, fc1, fc2, fc3, fc4)


#add CO2 costs from inst for random logit
fco <- data.frame(read_excel("Marginalcosts_100.xlsm", sheet="Tab2"))
fco1 <- fco[-c(1:2),-c(1:2, 121:130)]
fco2 <- data.frame(stack(fco1))
fco3 <- fco2[,-c(2)]
fco4 <- cbind.data.frame(fco3,m)
colnames(fco4) <- c('fcoa', 'm')
py8 <- merge(py7, fco4, by="m")
rm(fco, fco1, fco2, fco3, fco4, py6)


#####################################
#adding temp
temper <- data.frame(read_excel("temper.xlsx"))
temper2<- temper[-c(1:2),-c(1:2)]
temper3<- data.frame(stack(temper2))
temper4<- temper3[,-c(2)]
temper5 <- cbind.data.frame(temper4,m)
colnames(temper5) <- c('demand_instruments0', 'm')
py9 <- merge(py8, temper5, by="m")
rm(temper2, temper3, temper4, temper5,temper, py7)

#####################################
#adding solar
solar <- data.frame(read_excel("solar.xlsx"))
solar2<- solar[-c(1:2),-c(1:2, 120:129)]
solar3<- data.frame(stack(solar2))
solar4<- solar3[,-c(2)]
solar5 <- cbind.data.frame(solar4,m)
colnames(solar5) <- c('solar', 'm')
py91 <- merge(py9, solar5, by="m")
rm(solar2, solar3, solar4, solar5,solar)

#####################################
#adding wind
wind <- data.frame(read_excel("wind.xlsx"))
wind2<- wind[-c(1:2),-c(1:2, 120:129)]
wind3<- data.frame(stack(wind2))
wind4<- wind3[,-c(2)]
wind5 <- cbind.data.frame(wind4,m)
colnames(wind5) <- c('wind', 'm')
py92 <- merge(py91, wind5, by="m")
rm(wind2, wind3, wind4, wind5,wind)

#####################################
#Adding renewables
ren <- rep(dem$ren,118)
ren2 <- cbind.data.frame(m, ren)
py93 <- merge(py92, ren2, by="m")
rm(ren, ren2)

#####################################
#Adding demand
demand <- rep(dem$demand,118)
dm2 <- cbind.data.frame(m, demand)
py94 <- merge(py93, dm2, by="m")
rm(demand, dm2, py91, py92)


######################################
#technologies
cate <- namemc[c(1), c(3:119)]
cate$gaschp <- 17
cate1 <- stack(cate)
colnames(cate1) <- c("nesting_ids", "plant") 
tech <- c("biom","wasser",	"windon", "windoff",	"solar",	"serne",	"kerne",	"braunk",	"steink",	"gasoc",	"pump", "oil", "skonv" ,"gasst", "gascc" , "gaschp")
techc <- c(1,       2,         4,        3,         5,        12,        7,        8 ,       9 ,      10,    11,     6     , 13      , 14,      15     ,    17  )
techa <- data.frame(tech, techc)
colnames(techa) <- c("tech", "nesting_ids")
techb <- merge(cate1, techa, by="nesting_ids")
firm3 <-firm[,-c(3:4)]
techc <- merge(firm3, techb, by="plant")
colnames(techc)[2] <- "plant_ids"

py10 <- merge(py94, techc, by="plant_ids")

rm(cate, cate1, tech, techb, techc, firm3,m)


#####################################
#fuel prices
attach(py10)
py10$supply_instruments0[tech=="steink"] <- fs$coal
py10$supply_instruments0[tech=="gasoc"] <- fs$coal
py10$supply_instruments0[tech=="gasst"] <- fs$coal
py10$supply_instruments0[tech=="gascc"] <- fs$coal
py10$supply_instruments0[tech=="oil"]   <- fs$coal
py10$supply_instruments0[tech=="skonv"] <- fs$coal
detach(py10)
py10$supply_instruments0[is.na(py10$supply_instruments0)] <- 0


attach(py10)
py10$supply_instruments1[tech=="steink"] <- fs$gas
py10$supply_instruments1[tech=="gasoc"] <- fs$gas
py10$supply_instruments1[tech=="gasst"] <- fs$gas
py10$supply_instruments1[tech=="gascc"] <- fs$gas
py10$supply_instruments1[tech=="oil"]   <- fs$gas
py10$supply_instruments1[tech=="skonv"] <- fs$gas
detach(py10)
py10$supply_instruments1[is.na(py10$supply_instruments1)] <- 0


attach(py10)
py10$supply_instruments2[tech=="steink"] <- fs$coa
py10$supply_instruments2[tech=="gasoc"] <- fs$coa
py10$supply_instruments2[tech=="gasst"] <- fs$coa
py10$supply_instruments2[tech=="gascc"] <- fs$coa
py10$supply_instruments2[tech=="oil"]   <- fs$coa
py10$supply_instruments2[tech=="skonv"] <- fs$coa
py10$supply_instruments2[tech=="braunk"] <- fs$coa
detach(py10)
py10$supply_instruments2[is.na(py10$supply_instruments2)] <- 0

######################################
#ramping costs
ramp <- data.frame(read_excel("ramp.xlsx", col_types = c("text", 
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
                                                         "numeric", "numeric", "numeric", "numeric", "numeric"
                                                         
                                                         
                                                         
)))


ramp1 <- ramp[-c(1:2), -c(1:2, 120:129)]
ramp2 <- data.frame(stack(ramp1))
ramp3 <- ramp2[, c(2,1)]
m <- c(1:1803984)
ramp4 <- cbind.data.frame(ramp3, m)
ramp4 <- ramp4[,-c(1)]
rm(ramp1, ramp2,ramp3)
colnames(ramp4) <- c( "rampc", "m")
py11 <- merge(py10, ramp4, by="m")
py12 <- py11%>%select(-plant_ids,plant_ids)

#cleaning the data
ch <- sapply(py12, function(x) sum(is.na(x)))
ch1 <- py12[!complete.cases(py12), ]
ch2 <- unique(ch1$time)
pyclean <- py12
pyclean = filter(pyclean, !(time %in% ch2))

summary(pyclean) #1695306

#################################################################################
#VERIFICATIONS --
#N=1556877
#taking out shares close equal to 0 or 1: N=1027140
#loss= 1- 1027140/1695306
vf <- with(pyclean, subset(pyclean, shares>0 & shares<0.99))

#taking out periods where the the total sum of shares is 1:
Summary = vf %>%
  group_by(time = floor(time)) %>%
  summarise(shares = sum(shares))

#N= 1 market that sum higher than  0.99 in shares
check <- with(Summary, subset(Summary,  shares>=0.99))

#N= 0 markets that sum 0 in shares
check2 <- with(Summary, subset(Summary,  shares<=0))
b <- c(check$time)
d <- c(check2$time)

#final total N= 1027140, loss = 1 - 1027140/1695306, total final loss of 0.39 data
vf1 <- vf[!vf$time %in% b, ]
vf2v <- vf1[!vf1$time %in% d, ]


Summary2 = vf2v %>%
  group_by(time = floor(time)) %>%
  summarise(shares = sum(shares))

write_xlsx(Summary2, "Output/sharesl_100.xlsx") #shares that we are using in T markets

check3 <- with(Summary2, subset(Summary2, shares>=0.99)) #zero 
rm(check, check2, Summary, check3)

#cheecking NAs in final dataset vf2
sum(apply(vf2v, 1, anyNA)) #zero 

rm(Summary2, py11, py12, py10, ramp, ramp4,pyclean, vf, vf1)
rm(ch1, firm, fs, py4, py5, py8, py9, py93, py94, ch, ch2, b,d,m)


#################################################################################
#EXPORT TO CVS USE IN PYTHON

attach(vf2v)
vf2v$period[4<=month & month<=9] <- "Summer"
vf2v$period[1<=month & month<=3] <- "Winter"
vf2v$period[10<=month & month<=12] <- "Winter"
detach(vf2v)


attach(vf2v)
vf2v$block[6<=hour & hour<=9] <- "peak1"
vf2v$block[13<=hour & hour<=16] <- "peak2"
vf2v$block[20<=hour & hour<=21] <- "off"
vf2v$block[10<=hour & hour<=12] <- "peak1"
vf2v$block[17<=hour & hour<=19] <- "peak2"
vf2v$block[1<=hour & hour<=5] <- "off"
vf2v$block[22<=hour & hour<=24] <- "off"
detach(vf2v)

#vf2v <- cbind(vf2v, dummy(vf2v$block, sep = "_"))

attach(vf2v)
vf2v$week[day=="Mo" | day=="Tu" | day=="W" | day=="Th" | day=="Fr" ] <- "work"
vf2v$week[day=="Sa" | day=="Su" ] <- "end"
detach(vf2v)


vf2v <- vf2v[,-c(1)]

write.csv(vf2v, file="Output/i1rv100.cvs")
print(proc.time()[3]-p0)
#############################################################################################
#DATA CONSTRUCTION FOR PYTHON PYPBLP - END


  


#TABLES AND FIGURES - START 
#############################################################################################

setwd("C:\\Users\\Gloria\\Documents\\1_Paper\\Publication\\Replication\\R_paneldata\\BLP")
plants <- data.frame(read_excel("plants.xlsx")) 
tech <- c("biom","wasser",	"windon", "windoff",	"solar",	"serne",	"kerne",	"braunk",	"steink",	"gasoc",	"pump", "oil", "skonv" ,"gasst", "gascc" , "gaschp")
techc <- c(1,       2,         4,        3,         5,        12,        7,        8 ,       9 ,      10,    11,     6     , 13      , 14,      15     ,    17  )
techa <- data.frame(tech, techc)
colnames(techa) <- c("item", "id")
match <- merge(plants, techa, by="id")
match <- match[,-c(1)]

setwd("C:\\Users\\Gloria\\Documents\\1_Paper\\Publication\\Replication\\Python\\sum") 
pt <- data.frame(read_excel("pt.xlsx")) 
colnames(pt)[1] <- c("concept")

pt_fc <- with(pt, subset(pt, concept=="fcost" & (Case=="base" | Case=="ramp"  )))
pt_fc <- pt_fc[order(pt_fc[,c(10)]),]
pt_fc$type <- substring(pt_fc$Case,1,3)
pt_fc$number <- substring(pt_fc$Case,5,7)


pt_fc2 <- with(pt, subset(pt, concept=="fcost" & (Case=="ramp" | Case=="CF1_10"  | Case=="CF1_20"  )))
pt_fc2 <- pt_fc2[order(pt_fc2[,c(10)]),]
pt_fc2$type <- substring(pt_fc2$Case,1,3)
pt_fc2$number <- substring(pt_fc2$Case,5,7)

pt_fc3 <- with(pt, subset(pt, concept=="fcost" & (Case=="CF2_25"  | Case=="CF2_60"  | Case=="CF2_100") ))
pt_fc3 <- pt_fc3[order(pt_fc3[,c(10)]),]
pt_fc3$type <- substring(pt_fc3$Case,1,3)
pt_fc3$number <- substring(pt_fc3$Case,5,7)




pt_co <- with(pt, subset(pt, concept=="co2" & (Case=="base" | Case=="ramp"   ) ))
pt_co <- pt_co[order(pt_co[,c(10)]),]
pt_co$type <- substring(pt_co$Case,1,3)
pt_co$number <- substring(pt_co$Case,5,7)


pt_co2 <- with(pt, subset(pt, concept=="co2" & ( Case=="ramp" |Case=="CF1_10" | Case=="CF1_20"   ) ))
pt_co2 <- pt_co2[order(pt_co2[,c(10)]),]
pt_co2$type <- substring(pt_co2$Case,1,3)
pt_co2$number <- substring(pt_co2$Case,5,7)



pt_co3 <- with(pt, subset(pt, concept=="co2" & ( Case=="CF2_25"  | Case=="CF2_60"  | Case=="CF2_100") ))
pt_co3 <- pt_co3[order(pt_co3[,c(10)]),]
pt_co3$type <- substring(pt_co3$Case,1,3)
pt_co3$number <- substring(pt_co3$Case,5,7)



p1 <-  ggplot(pt_fc, aes(x=Block, y=coeff, group=Case, linetype=(Case))) +
  geom_line() +
  geom_point()+ 
  scale_y_continuous('Period', limits=c(0,2.3))+
  theme(legend.position = c(0.25, 0.1),
        legend.direction = "horizontal",legend.key.width = unit(0.3, 'cm'), legend.text = element_text(size=5),legend.key.size = unit(0.2, 'cm'),legend.key = element_rect(fill="white"), legend.title = element_blank(),legend.background= element_blank(), panel.background = element_blank(), panel.grid.major=element_line(linetype="solid", colour="gray91"), axis.line = element_line(colour = "black") , plot.title = element_text(hjust = 0.5, size=12), axis.title.x = element_text(size=9),axis.title.y = element_text(size=9))+
        xlab("Pass- through of fuel costs - Base and Ramping cases") 

p2 <-  ggplot(pt_fc2, aes(x=Block, y=coeff, group=Case, color=number, linetype=(type))) +
  geom_line() +
  geom_point()+ 
  scale_y_continuous('Period', limits=c(0,2.3))+
  theme(legend.position = c(0.25, 0.9),
        legend.direction = "horizontal",legend.key.width = unit(0.3, 'cm'),legend.text = element_text(size=5),legend.key.size = unit(0, 'cm'),legend.key = element_rect(fill="white"), legend.title = element_blank(),legend.background= element_blank(), panel.background = element_blank(), panel.grid.major=element_line(linetype="solid", colour="gray91"), axis.line = element_line(colour = "black") , plot.title = element_text(hjust = 0.5, size=12), axis.title.x = element_text(size=9),axis.title.y = element_text(size=9))+
  xlab("Pass- through of fuel costs - Counterfactuals method 1") 


p22 <-  ggplot(pt_fc3, aes(x=Block, y=coeff, group=Case, color=number, linetype=(type))) +
  geom_line() +
  geom_point()+ 
  scale_y_continuous('Period', limits=c(0,3.3))+
  theme(legend.position = c(0.25, 0.9),
        legend.direction = "horizontal",legend.key.width = unit(0.3, 'cm'),legend.text = element_text(size=5),legend.key.size = unit(0, 'cm'),legend.key = element_rect(fill="white"), legend.title = element_blank(),legend.background= element_blank(), panel.background = element_blank(), panel.grid.major=element_line(linetype="solid", colour="gray91"), axis.line = element_line(colour = "black") , plot.title = element_text(hjust = 0.5, size=12), axis.title.x = element_text(size=9),axis.title.y = element_text(size=9))+
  xlab("Pass- through of fuel costs - Counterfactuals method 2")        


p3 <-  ggplot(pt_co, aes(x=Block, y=coeff, group=Case, linetype=(Case))) +
  geom_line() +
  geom_point()+ 
  scale_y_continuous('Period', limits=c(0,3.3))+
  theme(legend.position = c(0.25, 0.1),
        legend.direction = "horizontal",legend.key.width = unit(0.3, 'cm'),legend.text = element_text(size=5),legend.key.size = unit(0.2, 'cm'),legend.key = element_rect(fill="white"), legend.title = element_blank(),legend.background= element_blank(), panel.background = element_blank(), panel.grid.major=element_line(linetype="solid", colour="gray91"), axis.line = element_line(colour = "black") , plot.title = element_text(hjust = 0.5, size=12), axis.title.x = element_text(size=9),axis.title.y = element_text(size=9))+
  xlab("Pass- through of carbon costs - Base and Ramping cases") 

p4 <-  ggplot(pt_co2, aes(x=Block, y=coeff, group=Case, color=number, linetype=(type))) +
  geom_line() +
  geom_point()+ 
  scale_y_continuous('Period', limits=c(0,3.3))+
  theme(legend.position = c(0.25, 0.25),
        legend.direction = "horizontal",legend.key.width = unit(0.3, 'cm'),legend.text = element_text(size=5),legend.key.size = unit(0, 'cm'),legend.key = element_rect(fill="white"), legend.title = element_blank(),legend.background= element_blank(), panel.background = element_blank(), panel.grid.major=element_line(linetype="solid", colour="gray91"), axis.line = element_line(colour = "black") , plot.title = element_text(hjust = 0.5, size=12), axis.title.x = element_text(size=9),axis.title.y = element_text(size=9))+
  xlab("Pass- through of carbon costs - Counterfactuals method 1") 

p44 <-  ggplot(pt_co3, aes(x=Block, y=coeff, group=Case, color=number, linetype=(type))) +
  geom_line() +
  geom_point()+ 
  scale_y_continuous('Period', limits=c(0,3.3))+
  theme(legend.position = c(0.25, 0.25),
        legend.direction = "horizontal",legend.key.width = unit(0.3, 'cm'),legend.text = element_text(size=5),legend.key.size = unit(0, 'cm'),legend.key = element_rect(fill="white"), legend.title = element_blank(),legend.background= element_blank(), panel.background = element_blank(), panel.grid.major=element_line(linetype="solid", colour="gray91"), axis.line = element_line(colour = "black") , plot.title = element_text(hjust = 0.5, size=12), axis.title.x = element_text(size=9),axis.title.y = element_text(size=9))+
  xlab("Pass- through of carbon costs - Counterfactuals method 2")  

pt_total <- grid.arrange(p1, p2, p3, p4, ncol = 2)
pt_cf2 <- grid.arrange(p22, p44, ncol = 2)


pdf(file = "C:\\Users\\Gloria\\Documents\\1_Paper\\Publication\\Replication\\R_paneldata\\Output\\A7.pdf",  
    width = 8, 
    height = 5) 
plot(pt_total)
dev.off()



pdf(file = "C:\\Users\\Gloria\\Documents\\1_Paper\\Publication\\Replication\\R_paneldata\\Output\\A11.pdf",  
    width = 10, 
    height = 5) 
plot(pt_cf2)
dev.off()


setwd("C:\\Users\\Gloria\\Documents\\1_Paper\\Publication\\Replication\\Python\\sum") 
ps <- data.frame(read.csv("ps.csv")) 
ps_a <- ps[(ps$Case %in% c("base", "ramp")), ]
ps_b <- ps[!(ps$Case %in% c("base", "ramp")), ]


ps_a$type <- substring(ps_a$Case,1,3)
ps_a$number <- substring(ps_a$Case,5,7)
ps_b$type <- substring(ps_b$Case,1,3)
ps_b$number <- substring(ps_b$Case,5,7)

ps_cf1 <- with(ps_b, subset(ps_b, !(Case=="base") & !(type=="CF2")))
ps_cf2 <- with(ps_b, subset(ps_b, !(Case=="base" | Case=="ramp") & !(type=="CF1")))




#base and ramp mks
mks <- merge(ps_a, match, by="plant")
mks1 <- subset(mks, (mks>=-1 & mks<=1))

attach(mks1)
mks1$type[item=="braunk" | item=="gascc" | item=="gasoc" | item=="gasst" | item=="oil" | item=="oiloc" | item=="skonv" | item=="steink" ] <- "FF"
mks1$type[item=="gaschp" | item=="kerne" | item=="pump" | item=="serne"| item=="wasser" ] <- "MUST"
mks1$type[item=="windon" | item=="windoff"] <- "REN"
detach(mks1)

mks2 <- mks1 %>%
  dplyr::group_by(type,block, Case) %>%
  dplyr::summarise(
    mks_m = mean(mks, na.rm=T),
    mks_sd = sd(mks, na.rm=T)
  )


#cf1 mks
mks_cf1 <- merge(ps_cf1, match, by="plant")
mks1_cf1 <- subset(mks_cf1, (mks>=-1 & mks<=1))

attach(mks1_cf1)
mks1_cf1$type[item=="braunk" | item=="gascc" | item=="gasoc" | item=="gasst" | item=="oil" | item=="oiloc" | item=="skonv" | item=="steink" ] <- "FF"
mks1_cf1$type[item=="gaschp" | item=="kerne" | item=="pump" | item=="serne"| item=="wasser" ] <- "MUST"
mks1_cf1$type[item=="windon" | item=="windoff"] <- "REN"
detach(mks1_cf1)



mks2_cf1 <- mks1_cf1 %>%
  dplyr::group_by(type,block, Case) %>%
  dplyr::summarise(
    mks_m = mean(mks, na.rm=T),
    mks_sd = sd(mks, na.rm=T)
  )


#cf2 mks
mks_cf2 <- merge(ps_cf2, match, by="plant")
mks1_cf2 <- subset(mks_cf2, (mkscf2>=-1 & mkscf2<=1))

attach(mks1_cf2)
mks1_cf2$type[item=="braunk" | item=="gascc" | item=="gasoc" | item=="gasst" | item=="oil" | item=="oiloc" | item=="skonv" | item=="steink" ] <- "FF"
mks1_cf2$type[item=="gaschp" | item=="kerne" | item=="pump" | item=="serne"| item=="wasser" ] <- "MUST"
mks1_cf2$type[item=="windon" | item=="windoff"] <- "REN"
detach(mks1_cf2)

mks2_cf2 <- mks1_cf2 %>%
  dplyr::group_by(type,block, Case) %>%
  dplyr::summarise(
    mks_m = mean(mkscf2, na.rm=T),
    mks_sd = sd(mkscf2, na.rm=T)
  )

ps_cf2$ps <- ps_cf2$pscf2

#welfare

welf <- rbind.data.frame(ps_a, ps_cf1, ps_cf2) 


welf1 <- welf%>%
  dplyr::group_by(market_ids, block, Case) %>%
  dplyr::summarise(
    ps = sum(ps, na.rm=T)
  )


cs <- data.frame(read_excel("cs.xlsx", col_types = c("numeric", "text", "numeric", "text", "text", "numeric")))
colnames(cs)[2] <- "market_ids"
colnames(cs)[4] <- "block"

welf2 <- merge(welf1, cs, by=c('market_ids', 'block', 'Case'))

hh <- welf %>% 
      distinct(market_ids, hour)

welf3 <- merge(welf2, hh, by="market_ids")

welf4 <- welf3%>%
  dplyr::group_by(hour,Case) %>%
  dplyr::summarise(
    lnps_m = mean(log(ps), na.rm=T),
    lnps_sd = sd(log(ps), na.rm=T),
    lncs_m = mean(log(cs), na.rm=T),
    lncs_sd = sd(log(cs), na.rm=T),
  )

#w1 <- welf4[(welf4$Case %in% c("base", "ramp")), ]
w2 <- welf4[(welf4$Case %in% c("ramp", "CF1_10",  "CF1_20" )), ]
w3 <- welf4[(welf4$Case %in% c("CF2_25", "CF2_60", "CF2_100")), ]


welf1a <- welf%>%
  dplyr::group_by(market_ids, block, Case) %>%
  dplyr::summarise(
    pscf2 = sum(pscf2, na.rm=T)
  )

welf2a <- merge(welf1a, cs, by=c('market_ids', 'block', 'Case'))

hh <- welf %>% 
  distinct(market_ids, hour)

welf3a <- merge(welf2a, hh, by="market_ids")

welf4a <- welf3a%>%
  dplyr::group_by(hour,Case) %>%
  dplyr::summarise(
    lnps_m = mean(log(pscf2), na.rm=T),
    lnps_sd = sd(log(pscf2), na.rm=T),
    lncs_m = mean(log(cscf2), na.rm=T),
    lncs_sd = sd(log(cscf2), na.rm=T),
  )


w3a <- welf4a[(welf4$Case %in% c("CF2_25", "CF2_60", "CF2_100")), ]
w3a$type <- substring(w3a$Case,1,3)
w3a$number <- substring(w3a$Case,5,7)


w2$type <- substring(w2$Case,1,3)
w2$number <- substring(w2$Case,5,7)

w3$type <- substring(w3$Case,1,3)
w3$number <- substring(w3$Case,5,7)



gw2 <-  ggplot(w2, aes(x=hour, y=lnps_m, group=Case, color=number,linetype=(type))) +
  geom_line() +
  #geom_point()+ 
  scale_x_continuous(breaks = c(1,6,13,20,24))+ 
  scale_y_continuous('Period', limits=c(-9,9))+
  theme(legend.position = c(0.25, 0.1),
        legend.direction = "horizontal",legend.key.width = unit(0.3, 'cm'), legend.text = element_text(size=5),legend.key.size = unit(0.2, 'cm'),legend.key = element_rect(fill="white"), legend.title = element_blank(),legend.background= element_blank(), panel.background = element_blank(), panel.grid.major=element_line(linetype="solid", colour="gray91"), axis.line = element_line(colour = "black") , plot.title = element_text(hjust = 0.5, size=12), axis.title.x = element_text(size=9),axis.title.y = element_text(size=9))+
  xlab("Producer welfare")



gw22 <-  ggplot(w3a, aes(x=hour, y=lnps_m, group=Case, color=number,linetype=(type))) +
  geom_line() +
  #geom_point()+ 
  scale_x_continuous(breaks = c(1,6,13,20,24))+ 
  scale_y_continuous('Period', limits=c(-15,9))+
  theme(legend.position = c(0.25, 0.1),
        legend.direction = "horizontal",legend.key.width = unit(0.3, 'cm'), legend.text = element_text(size=5),legend.key.size = unit(0.2, 'cm'),legend.key = element_rect(fill="white"), legend.title = element_blank(),legend.background= element_blank(), panel.background = element_blank(), panel.grid.major=element_line(linetype="solid", colour="gray91"), axis.line = element_line(colour = "black") , plot.title = element_text(hjust = 0.5, size=12), axis.title.x = element_text(size=9),axis.title.y = element_text(size=9))+
 xlab("Producer welfare")



gw4 <-  ggplot(w2, aes(x=hour, y=lncs_m, group=Case, color=number,linetype=(type))) +
  geom_line() +
  #geom_point()+ 
  scale_x_continuous(breaks = c(1,6,13,20,24))+ 
  scale_y_continuous('Period', limits=c(-9,9))+
  theme(legend.position = c(0.25, 0.1),
        legend.direction = "horizontal",legend.key.width = unit(0.3, 'cm'), legend.text = element_text(size=5),legend.key.size = unit(0.2, 'cm'),legend.key = element_rect(fill="white"), legend.title = element_blank(),legend.background= element_blank(), panel.background = element_blank(), panel.grid.major=element_line(linetype="solid", colour="gray91"), axis.line = element_line(colour = "black") , plot.title = element_text(hjust = 0.5, size=12), axis.title.x = element_text(size=9),axis.title.y = element_text(size=9))+
  xlab("Consumer welfare")

gw44 <-  ggplot(w3a, aes(x=hour, y=lncs_m, group=Case, color=number,linetype=(type))) +
  geom_line() +
  #geom_point()+ 
  scale_x_continuous(breaks = c(1,6,13,20,24))+ 
  scale_y_continuous('Period', limits=c(-15,9))+
  theme(legend.position = c(0.25, 0.1),
        legend.direction = "horizontal",legend.key.width = unit(0.3, 'cm'), legend.text = element_text(size=5),legend.key.size = unit(0.2, 'cm'),legend.key = element_rect(fill="white"), legend.title = element_blank(),legend.background= element_blank(), panel.background = element_blank(), panel.grid.major=element_line(linetype="solid", colour="gray91"), axis.line = element_line(colour = "black") , plot.title = element_text(hjust = 0.5, size=12), axis.title.x = element_text(size=9),axis.title.y = element_text(size=9))+
  xlab("Consumer welfare")

welf_total <- grid.arrange(gw2, gw4,  ncol = 2)

welf_total_cf2 <- grid.arrange(gw22, gw44,  ncol = 2)


  pdf(file = "C:\\Users\\Gloria\\Documents\\1_Paper\\Publication\\Replication\\R_paneldata\\Output\\A8.pdf",  
      width = 8, 
      height = 5) 
  plot(welf_total)
  dev.off()



pdf(file = "C:\\Users\\Gloria\\Documents\\1_Paper\\Publication\\Replication\\R_paneldata\\Output\\A12.pdf",  
    width = 8, 
    height = 5) 
plot(welf_total_cf2)
dev.off()


#####

#Table A8_BLP
setwd("C:\\Users\\Gloria\\Documents\\1_Paper\\Publication\\Replication\\Python\\curva") 
curv <- data.frame(read_excel("curva.xlsx")) 

ramp_m <- subset(mks1, Case=="ramp") 

ramp_m1 <- ramp_m%>%
  dplyr::group_by(market_ids, block, Case, type) %>%
  dplyr::summarise(
    mks = mean(mks, na.rm=T)
  )

ramp_m2 <- subset(ramp_m1, type=="FF") 

curv1 <- curv[,-c(1,3,12)]

s4 <- data.frame(read_excel("stats4.xlsx", sheet="two")) 
s5 <- data.frame(read_excel("stats5.xlsx", sheet="two"))
s6 <- data.frame(read_excel("stats6.xlsx", sheet="two"))

sall <- rbind.data.frame(s4,s5,s6)

curv1$market_ids <- sall$X0

ramp_m3 <- merge(ramp_m2, curv1, by="market_ids",all.x=T)


ramp_m3$psi <- ramp_m3$mks*ramp_m3$agg0.1*(-1)

ramp_m4 <- ramp_m3%>%
  dplyr::group_by(block) %>%
  dplyr::summarise(
    mks_m = mean(mks, na.rm=T),
    mks_sd = sd(mks, na.rm=T),
    phi_m = mean(agg0.1, na.rm=T),
    phi_sd = sd(agg0.1, na.rm=T),
    psi_m = mean(psi, na.rm=T),
    psi_sd = sd(psi, na.rm=T)
  )

setwd("C:\\Users\\Gloria\\Documents\\1_Paper\\Publication\\Replication\\R_paneldata\\Output") 
write_xlsx(ramp_m4, "TA8_BLP.xlsx")



#Figure A9
ps2 <- ps[,c(2,4, 7,8, 11, 15, 17, 18, 19 )]

#shares 0-10, 0-20, 0-30 promedios diarios
p22 <- ps2 %>%
  mutate(shares = ps2$shares,
         prices=ps2$prices,
         costs=ps2$costs
  ) %>%
  dplyr::group_by(Case, market_ids) %>%
  dplyr::summarise_all(mean)

p22a <- ps[(ps$Case %in% c("CF1_10", "CF1_20", "ramp")), ]

p22a$type <- substring(p22a$Case,1,3)
p22a$number <- substring(p22a$Case,5,7)

ener <- ener[-c(1), c(1,2)] #this comes from the code above

p22a$time <- as.numeric(str_extract(p22a$market_ids, "[0-9]+"))
p22b <- merge.data.frame(p22a, ener, by="time", all.x = T)
p22b$dia <- substring(p22b$stamp,1,10)

#prices
p22c <- p22b %>%
  dplyr::group_by(Case, dia) %>%
  dplyr::summarise_all(mean)
p22c$type <- substring(p22c$Case,1,3)
p22c$number <- substring(p22c$Case,5,7)

prices1 <-  ggplot(p22c, aes(x=dia, y=prices, group=Case, color=number,linetype=(type))) +
  geom_line() +
  #geom_point()+ 
  #scale_x_continuous(breaks = c(1,6,13,20,24))+ 
  #scale_y_continuous('Period', limits=c(-9,9))+
  theme(legend.position = c(0.25, 0.8),
        legend.direction = "horizontal",legend.key.width = unit(0.3, 'cm'), legend.text = element_text(size=5),legend.key.size = unit(0.2, 'cm'),legend.key = element_rect(fill="white"), legend.title = element_blank(),legend.background= element_blank(), panel.background = element_blank(), panel.grid.major=element_blank(), axis.line = element_line(colour = "black") , plot.title = element_text(hjust = 0.5, size=12), axis.title.x = element_text(size=9),axis.title.y = element_text(size=9))

prices1

shares1 <-  ggplot(p22c, aes(x=dia, y=shares, group=Case, color=number,linetype=(type))) +
  geom_line() +
  #geom_point()+ 
  #scale_x_continuous(breaks = c(1,6,13,20,24))+ 
  #scale_y_continuous('Period', limits=c(-9,9))+
  theme(legend.position = c(0.25, 0.8),
        legend.direction = "horizontal",legend.key.width = unit(0.3, 'cm'), legend.text = element_text(size=5),legend.key.size = unit(0.2, 'cm'),legend.key = element_rect(fill="white"), legend.title = element_blank(),legend.background= element_blank(), panel.background = element_blank(), panel.grid.major=element_blank(), axis.line = element_line(colour = "black") , plot.title = element_text(hjust = 0.5, size=12), axis.title.x = element_text(size=9),axis.title.y = element_text(size=9))



costs1 <-  ggplot(p22c, aes(x=dia, y=costs, group=Case, color=number,linetype=(type))) +
  geom_line() +
  #geom_point()+ 
  #scale_x_continuous(breaks = c(1,6,13,20,24))+ 
  #scale_y_continuous('Period', limits=c(-9,9))+
  theme(legend.position = c(0.25, 0.8),
        legend.direction = "horizontal",legend.key.width = unit(0.3, 'cm'), legend.text = element_text(size=5),legend.key.size = unit(0.2, 'cm'),legend.key = element_rect(fill="white"), legend.title = element_blank(),legend.background= element_blank(), panel.background = element_blank(), panel.grid.major=element_blank(), axis.line = element_line(colour = "black") , plot.title = element_text(hjust = 0.5, size=12), axis.title.x = element_text(size=9),axis.title.y = element_text(size=9))



details <- grid.arrange(prices1,shares1,costs1, ncol = 1)


pdf(file = "C:\\Users\\Gloria\\Documents\\1_Paper\\Publication\\Replication\\R_paneldata\\Output\\A9.pdf",  
    width = 8, 
    height = 5) 
plot(details)
dev.off()



#Figure A10

#attach(ps)
#ps$plant[plant=="Dresden.Nossener.BrÃ¼cke" | plant=="Dresden.Nossener.Brï¿½cke" | plant== "Dresden.Nossener.Brucke"] <- "Dresden.Nossener.Brucke"
#detach(ps)

ps2222 <- merge(ps, match, by="plant", all.x=T)

#distinct(ps2222, source)

#colnames(ps2222)[26] <- 'item'

attach(ps2222)
ps2222$source[item=="braunk" | item=="gascc" | item=="gasoc" | item=="gasst" | item=="oil" | item=="oiloc" | item=="skonv" | item=="steink" ] <- "FF"
ps2222$source[item=="gaschp" | item=="kerne" | item=="pump" | item=="serne"| item=="wasser" ] <- "MUST"
ps2222$source[item=="windon" | item=="windoff"] <- "REN"
detach(ps2222)



#CF1 shares per tech
shtec <- ps2222[(ps2222$Case %in% c("CF1_10", "CF1_20", "ramp")), ]

shtec1 <- shtec%>%
  dplyr::group_by(source, block, year, Case ) %>%
  dplyr::summarise(
    shares = mean(shares)
  )



shtec17 <- shtec1[(shtec1$year %in% c("2017")), ]
shtec18 <- shtec1[(shtec1$year %in% c("2018")), ]

sums <- shtec17%>%
  dplyr::group_by(Case, block) %>%
  dplyr::summarise(
    sums = sum(shares)
  )
sums$id <- c(1:9)
shtec17$id <- c(1,1,1,2,2,2,3,3,3,4,4,4,5,5,5,6,6,6,7,7,7,8,8,8,9,9,9) 
sums <- sums[,c(3:4)]
shtec17a <- merge(shtec17, sums, by="id", all.x=T)
shtec17a$per <- shtec17a$shares/shtec17a$sums 

sums2 <- shtec18%>%
  dplyr::group_by(Case, block) %>%
  dplyr::summarise(
    sums = sum(shares)
  )
shtec18b <- shtec18[order(shtec18$Case, shtec18$block),]

sums2$id <- c(1:9)
shtec18b$id <- c(1,1,1,2,2,2,3,3,3,4,4,4,5,5,5,6,6,6,7,7,7,8,8,8,9,9,9) 
sums2 <- sums2[,c(3:4)]
shtec18b <- merge(shtec18b, sums2, by="id", all.x=T)
shtec18b$per <- shtec18b$shares/shtec18b$sums

d <- c(0.5,0.5, 0.5)
graphfa <- ggplot(shtec17a, aes(x=Case, y=per, fill= Case))+
  geom_bar(stat = "identity", color="black", aes(alpha=source),
           #position =position_dodge(),  
           width = d)+
  facet_wrap(.~block)+
  #geom_errorbar(aes(ymin=lb, ymax=ub, width=0.2),
  #             position=position_dodge(0.5))+
  
  coord_cartesian(ylim=c(0,1))+
  scale_fill_manual("legend", values = c("green", "mediumblue", "red" ))+
  #geom_text(aes(label=n), position=position_dodge(width=0.5), vjust=-3, size=3)+
  labs(title="2017", x = "",
       y = "%")+
  theme(legend.background= element_blank(), panel.background = element_blank(), panel.grid.major=element_line(linetype="solid", colour="gray91"), axis.line = element_line(colour = "black") , plot.title = element_text(hjust = 0.5, size=12), axis.title.x = element_text(size=9),axis.title.y = element_text(size=9))

graphfa

graphfb <- ggplot(shtec18b, aes(x=Case, y=per, fill= Case))+
  geom_bar(stat = "identity", color="black", aes(alpha=source),
           #position =position_dodge(),  
           width = d)+
  facet_wrap(.~block)+
  #geom_errorbar(aes(ymin=lb, ymax=ub, width=0.2),
  #             position=position_dodge(0.5))+
  
  coord_cartesian(ylim=c(0,1))+
  scale_fill_manual("legend", values = c("green", "mediumblue", "red" ))+
  #geom_text(aes(label=n), position=position_dodge(width=0.5), vjust=-3, size=3)+
  labs(title="2018", x = "",
       y = "%")+
  theme(legend.background= element_blank(), panel.background = element_blank(), panel.grid.major=element_line(linetype="solid", colour="gray91"), axis.line = element_line(colour = "black") , plot.title = element_text(hjust = 0.5, size=12), axis.title.x = element_text(size=9),axis.title.y = element_text(size=9))

graphfb

shdet <- grid.arrange(graphfa, graphfb, ncol = 2)


pdf(file = "C:\\Users\\Gloria\\Documents\\1_Paper\\Publication\\Replication\\R_paneldata\\Output\\A10.pdf",  
    width = 12, 
    height = 5) 
plot(shdet)
dev.off()


#Figure A13
#CF2 shares per tech
shtec_cf2 <- ps2222[(ps2222$Case %in% c("CF2_25", "CF2_60", "CF2_100")), ]

shtec_cf2_1 <- shtec_cf2%>%
  dplyr::group_by(source, block, year, Case ) %>%
  dplyr::summarise(
    shares = mean(sharescf2)
  )

shtec17_cf2 <- shtec_cf2_1[(shtec_cf2_1$year %in% c("2017")), ]
shtec18_cf2 <- shtec_cf2_1[(shtec_cf2_1$year %in% c("2018")), ]

sums <- shtec17_cf2%>%
  dplyr::group_by(Case, block) %>%
  dplyr::summarise(
    sums = sum(shares)
  )
shtec17_cf2 <- shtec17_cf2[order(shtec17_cf2$Case, shtec17_cf2$block),]
sums$id <- c(1:9)
shtec17_cf2$id <- c(1,1,1,2,2,2,3,3,3,4,4,4,5,5,5,6,6,6,7,7,7,8,8,8,9,9,9) 
sums <- sums[,c(3:4)]
shtec17a_cf2 <- merge(shtec17_cf2, sums, by="id", all.x=T)
shtec17a_cf2$per <- shtec17a_cf2$shares/shtec17a_cf2$sums 

sums2 <- shtec18_cf2%>%
  dplyr::group_by(Case, block) %>%
  dplyr::summarise(
    sums = sum(shares)
  )
shtec18b_cf2 <- shtec18_cf2[order(shtec18_cf2$Case, shtec18_cf2$block),]

sums2$id <- c(1:9)
shtec18b_cf2$id <- c(1,1,1,2,2,2,3,3,3,4,4,4,5,5,5,6,6,6,7,7,7,8,8,8,9,9,9) 
sums2 <- sums2[,c(3:4)]
shtec18b_cf2 <- merge(shtec18b_cf2, sums2, by="id", all.x=T)
shtec18b_cf2$per <- shtec18b_cf2$shares/shtec18b_cf2$sums

d <- c(0.5,0.5, 0.5)
graphfa2 <- ggplot(shtec17a_cf2, aes(x=Case, y=per, fill= Case))+
  geom_bar(stat = "identity", color="black", aes(alpha=source),
           #position =position_dodge(),  
           width = d)+
  facet_wrap(.~block)+
  #geom_errorbar(aes(ymin=lb, ymax=ub, width=0.2),
  #             position=position_dodge(0.5))+
  
  coord_cartesian(ylim=c(0,1))+
  scale_fill_manual("legend", values = c("green", "mediumblue", "red" ))+
  #geom_text(aes(label=n), position=position_dodge(width=0.5), vjust=-3, size=3)+
  labs(title="2017 CF2", x = "",
       y = "%")+
  theme(legend.background= element_blank(), panel.background = element_blank(), panel.grid.major=element_line(linetype="solid", colour="gray91"), axis.line = element_line(colour = "black") , plot.title = element_text(hjust = 0.5, size=12), axis.title.x = element_text(size=9),axis.title.y = element_text(size=9))

graphfa2

graphfb2 <- ggplot(shtec18b_cf2, aes(x=Case, y=per, fill= Case))+
  geom_bar(stat = "identity", color="black", aes(alpha=source),
           #position =position_dodge(),  
           width = d)+
  facet_wrap(.~block)+
  #geom_errorbar(aes(ymin=lb, ymax=ub, width=0.2),
  #             position=position_dodge(0.5))+
  
  coord_cartesian(ylim=c(0,1))+
  scale_fill_manual("legend", values = c("green", "mediumblue", "red" ))+
  #geom_text(aes(label=n), position=position_dodge(width=0.5), vjust=-3, size=3)+
  labs(title="2018 CF2", x = "",
       y = "%")+
  theme(legend.background= element_blank(), panel.background = element_blank(), panel.grid.major=element_line(linetype="solid", colour="gray91"), axis.line = element_line(colour = "black") , plot.title = element_text(hjust = 0.5, size=12), axis.title.x = element_text(size=9),axis.title.y = element_text(size=9))

graphfb2

shdet2 <- grid.arrange(graphfa2, graphfb2, ncol = 2)


pdf(file = "C:\\Users\\Gloria\\Documents\\1_Paper\\Publication\\Replication\\R_paneldata\\Output\\A13.pdf",  
    width = 14, 
    height = 5) 
plot(shdet2)
dev.off()

rm(graphfa, graphfb, graphfa2, graphfb2, shdet, shdet2, shtec, shtec_cf2, shtec_cf2_1, shtec1, shtec17, shtec17_cf2, shtec17a, shtec17a_cf2, shtec18, shtec18_cf2, shtec18a,shtec18b,shtec18b_cf2, sums, sums2 )



#CF2 producer surpluses in detail
pstec_cf2 <- ps2222[(ps2222$Case %in% c("CF2_25", "CF2_60", "CF2_100")), ]

pstec_cf2_1 <- pstec_cf2%>%
  dplyr::group_by(source, block, year, Case ) %>%
  dplyr::summarise(
    ps = mean(pscf2)
  )

pstec17_cf2 <- pstec_cf2_1[(pstec_cf2_1$year %in% c("2017")), ]
pstec18_cf2 <- pstec_cf2_1[(pstec_cf2_1$year %in% c("2018")), ]

sums <- pstec17_cf2%>%
  dplyr::group_by(Case, block) %>%
  dplyr::summarise(
    sums = sum(ps)
  )
pstec17_cf2 <- pstec17_cf2[order(pstec17_cf2$Case, pstec17_cf2$block),]
sums$id <- c(1:9)
pstec17_cf2$id <- c(1,1,1,2,2,2,3,3,3,4,4,4,5,5,5,6,6,6,7,7,7,8,8,8,9,9,9) 
sums <- sums[,c(3:4)]
pstec17a_cf2 <- merge(pstec17_cf2, sums, by="id", all.x=T)
pstec17a_cf2$per <- pstec17a_cf2$ps/pstec17a_cf2$sums 

sums2 <- pstec18_cf2%>%
  dplyr::group_by(Case, block) %>%
  dplyr::summarise(
    sums = sum(ps)
  )
pstec18b_cf2 <- pstec18_cf2[order(pstec18_cf2$Case, pstec18_cf2$block),]

sums2$id <- c(1:9)
pstec18b_cf2$id <- c(1,1,1,2,2,2,3,3,3,4,4,4,5,5,5,6,6,6,7,7,7,8,8,8,9,9,9) 
sums2 <- sums2[,c(3:4)]
pstec18b_cf2 <- merge(pstec18b_cf2, sums2, by="id", all.x=T)
pstec18b_cf2$per <- pstec18b_cf2$ps/pstec18b_cf2$sums

d <- c(0.5,0.5, 0.5)
graphfa2 <- ggplot(pstec17a_cf2, aes(x=Case, y=per, fill= Case))+
  geom_bar(stat = "identity", color="black", aes(alpha=source),
           #position =position_dodge(),  
           width = d)+
  facet_wrap(.~block)+
  #geom_errorbar(aes(ymin=lb, ymax=ub, width=0.2),
  #             position=position_dodge(0.5))+
  
  coord_cartesian(ylim=c(0,1))+
  scale_fill_manual("legend", values = c("green", "mediumblue", "red" ))+
  #geom_text(aes(label=n), position=position_dodge(width=0.5), vjust=-3, size=3)+
  labs(title="2017 CF2", x = "",
       y = "%")+
  theme(legend.background= element_blank(), panel.background = element_blank(), panel.grid.major=element_line(linetype="solid", colour="gray91"), axis.line = element_line(colour = "black") , plot.title = element_text(hjust = 0.5, size=12), axis.title.x = element_text(size=9),axis.title.y = element_text(size=9))

graphfa2

graphfb2 <- ggplot(pstec18b_cf2, aes(x=Case, y=per, fill= Case))+
  geom_bar(stat = "identity", color="black", aes(alpha=source),
           #position =position_dodge(),  
           width = d)+
  facet_wrap(.~block)+
  #geom_errorbar(aes(ymin=lb, ymax=ub, width=0.2),
  #             position=position_dodge(0.5))+
  
  coord_cartesian(ylim=c(0,1))+
  scale_fill_manual("legend", values = c("green", "mediumblue", "red" ))+
  #geom_text(aes(label=n), position=position_dodge(width=0.5), vjust=-3, size=3)+
  labs(title="2018 CF2", x = "",
       y = "%")+
  theme(legend.background= element_blank(), panel.background = element_blank(), panel.grid.major=element_line(linetype="solid", colour="gray91"), axis.line = element_line(colour = "black") , plot.title = element_text(hjust = 0.5, size=12), axis.title.x = element_text(size=9),axis.title.y = element_text(size=9))

graphfb2

psdet2 <- grid.arrange(graphfa2, graphfb2, ncol = 2)


pdf(file = "C:\\Users\\Gloria\\Documents\\1_Paper\\Publication\\Replication\\R_paneldata\\Output\\A14.pdf",  
    width = 14, 
    height = 5) 
plot(psdet2)
dev.off()


#Curvature with ramping regression
#################################################################################

#curv_m <-  subset(curv, Case=="ramp") 
curv$market_ids <- sall$X0
curv$block <- sall$Block

curv_m <- curv[,-c(1)]

curv_m2 <- curv_m%>%
  dplyr::group_by(block) %>%
  dplyr::summarise(
    agg0.1 = mean(agg0.1, na.rm=T),
    agg0.2 = mean(agg0.2, na.rm=T),
    agg0.3 = mean(agg0.3, na.rm=T),
    agg0.4 = mean(agg0.4, na.rm=T),
    agg0.5 = mean(agg0.5, na.rm=T),
    agg0.6 = mean(agg0.6, na.rm=T),
    agg0.7 = mean(agg0.7, na.rm=T),
    agg0.8 = mean(agg0.8, na.rm=T),
  )
curv_m3 <- t(curv_m2)
colnames(curv_m3) <- curv_m3[c(1),] 
curv_m3 <- data.frame(curv_m3[-c(1),])
curv_m3[] <- lapply(curv_m3, as.numeric)
curv_m3$deltaP <- c(0.1, 0.2, 0.3, 0.4, 0.5, 0.6, 0.7, 0.8)


prices <- ramp_m%>%
  dplyr::group_by(block) %>%
  dplyr::summarise(
    prices = mean(prices, na.rm=T),
  )

dem$hour <- as.numeric(substring(dem$date,12,13))
attach(dem)
dem$block[6<=hour & hour<=9] <- "peak1"
dem$block[13<=hour & hour<=16] <- "peak2"
dem$block[20<=hour & hour<=21] <- "off"
dem$block[10<=hour & hour<=12] <- "peak1"
dem$block[17<=hour & hour<=19] <- "peak2"
dem$block[0<=hour & hour<=5] <- "off"
dem$block[22<=hour & hour<=23] <- "off"
detach(dem)

qs <- dem%>%
  dplyr::group_by(block) %>%
  dplyr::summarise(
    demand = mean(demand, na.rm=T)
  )


curv_m3$deltaQ_off <- (curv_m3$off*curv_m3$deltaP*(as.numeric(qs[1,2])))/(as.numeric(prices[1,2]))
curv_m3$deltaQ_p1 <- (curv_m3$peak1*curv_m3$deltaP*(as.numeric(qs[2,2])))/(as.numeric(prices[2,2]))
curv_m3$deltaQ_p2 <- (curv_m3$peak2*curv_m3$deltaP*(as.numeric(qs[3,2])))/(as.numeric(prices[3,2]))
curv_m3$Q_off <- (as.numeric(qs[1,2]))+curv_m3$deltaQ_off
curv_m3$Q_p1 <- (as.numeric(qs[2,2]))+curv_m3$deltaQ_p1
curv_m3$Q_p2 <- (as.numeric(qs[3,2]))+curv_m3$deltaQ_p2
curv_m3$P_off <- (as.numeric(prices[1,2]))*(1+curv_m3$deltaP)
curv_m3$P_p1 <- (as.numeric(prices[2,2]))*(1+curv_m3$deltaP)
curv_m3$P_p2 <- (as.numeric(prices[3,2]))*(1+curv_m3$deltaP)


setwd("C:\\Users\\Gloria\\Documents\\1_Paper\\Publication\\Replication\\Python\\curva")
write_xlsx(curv_m3, "FA5_part1.xlsx")

############################################################################################
#TABLES AND FIGURES - END

