rm(list=ls())

library(pdfetch)
library(mFilter)
library(stats)
library(ggplot2)
library(tidyverse)
library(hpfilter)
library(openxlsx)
library(neverhpfilter)
library(xts)

#Graf 1
datafred <- pdfetch_FRED(c("GDPC1", "QUSPAM770A", "QUSR628BIS"))
View(datafred)

#
gdp <- log(datafred$GDPC1[4:304,])
ctg <- log(na.omit(datafred$QUSPAM770A))
prop <- log(na.omit(datafred$QUSR628BIS))

gdphp <- mFilter ::hpfilter(gdp, freq = 1600)
ctghp <- mFilter::hpfilter(ctg, freq = 400000)
prophp <- mFilter::hpfilter(prop, freq = 400000)

#smoothing
a <- 100*gdphp$cycle
a <- mFilter::hpfilter(a, freq = 0.5)
gdphpsmooth <- a$trend

b <- 100*ctghp$cycle
b <- mFilter::hpfilter(b, freq = 2)
ctghpsmooth <- b$trend

c <- 100*prophp$cycle
c <- mFilter::hpfilter(c, freq = 2)

#OECD
OECDhp <- mFilter::hpfilter(gdp, freq = 133107.94)
OECDhpcycle <- mFilter::hpfilter(OECDhp$cycle, freq = 13.93)
OECDbc <- OECDhpcycle$trend

#Adding NAs to prop
navec <- rep(NA, 89)
propcyclefinal <- c(navec, 100*prophp$cycle)
prophpsmooth <- c(navec, c$trend)

date1 <- seq(as.Date("1947/12/31"), as.Date("2022/12/31"), by = "quarter")
df1 <- data.frame(date1, 100*gdphp$cycle, 100*ctghp$cycle, propcyclefinal, (100*ctghp$cycle + propcyclefinal)/2, gdphpsmooth, ctghpsmooth, prophpsmooth, (ctghpsmooth + prophpsmooth)/2 , 100*OECDbc) 
colnames(df1) <- c("Time", "GDP_gap", "ctg_gap", "prop_gap", "avg_fin_gap" , "GDP_gap_alt", "ctg_gap_alt", "prop_gap_alt", "avg_fin_gap_alt", "OECDbc")

ggplot(df1, aes(x=Time)) + 
  geom_line(aes(y = GDP_gap), color = "firebrick") +
  #geom_line(aes(y = ctg_gap), color = "darkslategray1") +
  #geom_line(aes(y = prop_gap), color = "palegreen1") +
  geom_line(aes(y = avg_fin_gap), color = "turquoise4") +
  #geom_line(aes(y = GDP_gap_alt), color = "firebrick") +
  #geom_line(aes(y = ctg_gap_alt), color = "darkslategray1") +
  #geom_line(aes(y = prop_gap_alt), color = "palegreen1") +
  #geom_line(aes(y = avg_fin_gap_alt), color = "turquoise4") +
  #geom_line(aes(y = OECDbc), color = "yellow4") +
  geom_hline(yintercept = 0) +
  xlab("Čas") +
  ylab("% odchylka od trendu") +
  ggtitle("Hospodářský a finanční cyklus USA") +
  #ggtitle("Hosp. cyk. - HP 1600 (červená) vs. HP používaný OECD (žlutá)") +
  theme_bw()

#write.xlsx(df1, file = "grafy_zaostřeno.xlsx", sheetName = "graf1", colNames = TRUE, asTable = TRUE)

#-----------------#
# Jednotlivé země #
#-----------------#

#---------#
# Německo #
#---------#

datafredDE <- pdfetch_FRED(c("QDEPAMUSDA", "QDEPAM770A", "QDER628BIS", "DEUGDPDEFQISMEI"))

ctgDE <- as.data.frame(log(na.omit(datafredDE$QDEPAM770A)))
propDE <- as.data.frame(log(na.omit(datafredDE$QDER628BIS)))
creditDE <- as.data.frame(log(na.omit(datafredDE$QDEPAMUSDA)))

credDE <- na.omit(datafredDE$QDEPAMUSDA[61:272,])
deflDE <- na.omit(datafredDE$DEUGDPDEFQISMEI[1:272,])
rcreditDE <- as.data.frame(log((credDE/deflDE)*100))

rcreditDEtrend <- hp1(rcreditDE, lambda = 400000)
rcreditDEcycle <- 100*(rcreditDE - rcreditDEtrend)

ctgDEtrend <- hp1(ctgDE, lambda = 400000)
ctgDEcycle <- 100*(ctgDE - ctgDEtrend)

View(ctgDEcycle)

propDEtrend <- hp1(propDE, lambda = 400000)
propDEcycle <- 100*(propDE - propDEtrend)

date2 <- seq(as.Date("2015/12/31"), as.Date("2022/12/31"), by = "quarter")
dfDE <- data.frame(date2, ctgDEcycle[as.numeric(nrow(ctgDEcycle)-28):as.numeric(nrow(ctgDEcycle)),], propDEcycle[as.numeric(nrow(propDEcycle)-28):as.numeric(nrow(propDEcycle)),], rcreditDEcycle[as.numeric(nrow(rcreditDEcycle)-28):as.numeric(nrow(rcreditDEcycle)),])
colnames(dfDE) <- c("Time", "ctg_gap", "prop_gap", "credit_gap")

ggplot(dfDE, aes(x=Time)) + 
  geom_line(aes(y = ctg_gap), color = "firebrick") +
  geom_line(aes(y = prop_gap), color = "darkslategray3", linetype = "dotted") +
  geom_line(aes(y = credit_gap), color = "indianred2", linetype = "dotted") +
  geom_hline(yintercept = 0) +
  xlab("Čas") +
  ylab("% odchylka od trendu") +
  ggtitle("Odhad finančního cyklu Německa") +
  theme_bw()

#write.xlsx(dfDE, file = "grafy_zaostřeno2.xlsx", sheetName = "graf2", colNames = TRUE, asTable = TRUE)

#---------#
# Francie #
#---------#
library(pdfetch)
library(hpfilter)
library(tidyverse)
library(mFilter)

datafredFR <- pdfetch_FRED(c("QFRPAMUSDA", "QFRPAM770A", "QFRR628BIS"))

ctgFR <- as.data.frame(log(na.omit(datafredFR$QFRPAM770A)))
propFR <- as.data.frame(log(na.omit(datafredFR$QFRR628BIS)))
creditFR <- as.data.frame(log(na.omit(datafredFR$QFRPAMUSDA)))

ctgFRtrend <- hp1(ctgFR, lambda = 400000)
ctgFRcycle <- 100*(ctgFR - ctgFRtrend)

propFRtrend <- hp1(propFR, lambda = 400000)
propFRcycle <- 100*(propFR - propFRtrend)

creditFRtrend <- hp1(creditFR, lambda = 400000)
creditFRcycle <- 100*(creditFR - creditFRtrend)

date2 <- seq(as.Date("2015/12/31"), as.Date("2022/12/31"), by = "quarter")
dfFR <- data.frame(date2, ctgFRcycle[as.numeric(nrow(ctgFRcycle)-28):as.numeric(nrow(ctgFRcycle)),], propFRcycle[as.numeric(nrow(propFRcycle)-28):as.numeric(nrow(propFRcycle)),], creditFRcycle[as.numeric(nrow(creditFRcycle)-28):as.numeric(nrow(creditFRcycle)),])
colnames(dfFR) <- c("Time", "ctg_gap", "prop_gap", "credit_gap")

ggplot(dfFR, aes(x=Time)) + 
  geom_line(aes(y = ctg_gap), color = "firebrick") +
  geom_line(aes(y = prop_gap), color = "darkslategray3", linetype = "dotted") +
  geom_line(aes(y = credit_gap), color = "indianred2", linetype = "dotted") +
  geom_hline(yintercept = 0) +
  xlab("Čas") +
  ylab("% odchylka od trendu") +
  ggtitle("Odhad finančního cyklu Francie") +
  theme_bw()

#write.xlsx(dfFR, file = "grafy_zaostřeno3.xlsx", sheetName = "graf3", colNames = TRUE, asTable = TRUE)

#----#
# UK #
#----#

datafredGB <- pdfetch_FRED(c("QGBPAMUSDA", "QGBPAM770A", "QGBR628BIS"))

ctgGB <- as.data.frame(log(na.omit(datafredGB$QGBPAM770A)))
propGB <- as.data.frame(log(na.omit(datafredGB$QGBR628BIS)))
creditGB <- as.data.frame(log(na.omit(datafredGB$QGBPAMUSDA)))

ctgGBtrend <- hp1(ctgGB, lambda = 400000)
ctgGBcycle <- 100*(ctgGB - ctgGBtrend)

propGBtrend <- hp1(propGB, lambda = 400000)
propGBcycle <- 100*(propGB - propGBtrend)

creditGBtrend <- hp1(creditGB, lambda = 400000)
creditGBcycle <- 100*(creditGB - creditGBtrend)

date2 <- seq(as.Date("2015/12/31"), as.Date("2022/12/31"), by = "quarter")
dfGB <- data.frame(date2, ctgGBcycle[as.numeric(nrow(ctgGBcycle)-28):as.numeric(nrow(ctgGBcycle)),], propGBcycle[as.numeric(nrow(propGBcycle)-28):as.numeric(nrow(propGBcycle)),], creditGBcycle[as.numeric(nrow(creditGBcycle)-28):as.numeric(nrow(creditGBcycle)),])
colnames(dfGB) <- c("Time", "ctg_gap", "prop_gap", "credit_gap")

ggplot(dfGB, aes(x=Time)) + 
  geom_line(aes(y = ctg_gap), color = "firebrick") +
  geom_line(aes(y = prop_gap), color = "darkslategray3", linetype = "dotted") +
  geom_line(aes(y = credit_gap), color = "indianred2", linetype = "dotted") +
  geom_hline(yintercept = 0) +
  xlab("Čas") +
  ylab("% odchylka od trendu") +
  ggtitle("Odhad finančního cyklu Spojeného království") +
  theme_bw()

#write.xlsx(dfGB, file = "grafy_zaostřeno4.xlsx", sheetName = "graf4", colNames = TRUE, asTable = TRUE)

#----------#
# Japonsko #
#----------#

datafredJP <- pdfetch_FRED(c("QJPPAM770A", "QJPR628BIS", "QJPPAMUSDA"))

dateJAPAN <- seq(as.Date("1955/03/31"), as.Date("2022/12/31"), by = "quarter")
dfJAPAN <- data.frame(dateJAPAN, datafredJP$QJPPAM770A, datafredJP$QJPR628BIS, datafred$QUSPAM770A[1:272,], datafred$QUSR628BIS[1:272,])
colnames(dfJAPAN) <- c("Time", "CtG", "Pp", "CtGUSA", "PpUSA")
ggplot(dfJAPAN, aes(x = Time)) +
  geom_line(aes(y = CtG), color = "firebrick") +
  geom_line(aes(y = Pp), color = "turquoise4") +
  geom_line(aes(y = CtGUSA), color = "firebrick", linetype = "dotted") +
  geom_line(aes(y = PpUSA), color = "turquoise4", linetype = "dotted") +
  ylab("") +
  ggtitle("Úvěry/HDP a index cen nemovitostí Japonska")

#write.xlsx(dfJAPAN, file = "grafy_zaostřeno5.xlsx", sheetName = "graf5", colNames = TRUE, asTable = TRUE)

#last
ctgJP <- as.data.frame(log(na.omit(datafredJP$QJPPAM770A)))
propJP <- as.data.frame(log(na.omit(datafredJP$QJPR628BIS)))
creditJP <- as.data.frame(log(na.omit(datafredJP$QJPPAMUSDA)))

ctgJPtrend <- hp1(ctgJP, lambda = 400000)
ctgJPcycle <- 100*(ctgJP - ctgJPtrend)

propJPtrend <- hp1(propJP, lambda = 400000)
propJPcycle <- 100*(propJP - propJPtrend)

creditJPtrend <- hp1(creditJP, lambda = 400000)
creditJPcycle <- 100*(creditJP - creditJPtrend)

date2 <- seq(as.Date("2015/12/31"), as.Date("2022/12/31"), by = "quarter")
dfJP <- data.frame(date2, ctgJPcycle[as.numeric(nrow(ctgJPcycle)-28):as.numeric(nrow(ctgJPcycle)),], propJPcycle[as.numeric(nrow(propJPcycle)-28):as.numeric(nrow(propJPcycle)),], creditJPcycle[as.numeric(nrow(creditJPcycle)-28):as.numeric(nrow(creditJPcycle)),])
colnames(dfJP) <- c("Time", "ctg_gap", "prop_gap", "credit_gap")

ggplot(dfJP, aes(x=Time)) + 
  geom_line(aes(y = ctg_gap), color = "firebrick") +
  geom_line(aes(y = prop_gap), color = "darkslategray3", linetype = "dotted") +
  geom_line(aes(y = credit_gap), color = "indianred2", linetype = "dotted") +
  geom_hline(yintercept = 0) +
  xlab("Čas") +
  ylab("% odchylka od trendu") +
  ggtitle("Odhad finančního cyklu Japonska") +
  theme_bw()

#write.xlsx(dfJP, file = "grafy_zaostřeno6.xlsx", sheetName = "graf6", colNames = TRUE, asTable = TRUE)

#----------#
# Box-graf #
#----------#
rm(list=ls())

library(pdfetch)
library(mFilter)
library(stats)
library(ggplot2)
library(tidyverse)
library(hpfilter)
library(openxlsx)
library(neverhpfilter)
library(xts)

CNBgap <- na.omit(read.csv("data.csv", header = T))
CNBgap <- CNBgap$CDPFgap_lev_qq_nso_412NTF
CNBgap <- CNBgap[1:109]

CZfred <- pdfetch_FRED("CLVMNACSCAB1GQCZ")
gdpCZ <- log(CZfred$CLVMNACSCAB1GQCZ)
gdpCZhp2 <- mFilter ::hpfilter(gdpCZ, freq = 1600)

gdpCZforhp1 <- data.frame(gdpCZ)
gdpCZhp1trend <- hp1(gdpCZforhp1, lambda = 1600)
gdpCZhp1 <- 100*(gdpCZforhp1-gdpCZhp1trend)

# Requires xts format
library(tseries)
library(tsbox)

log_gdp_xts <- ts_xts(gdpCZ)
gdp_HAM <- yth_filter(log_gdp_xts, h= 8, p = 4, output = "cycle")
HAM_gap <- as.vector(gdp_HAM*100)
navec2 <- rep(NA, 11)
HAM_gap1 <- c(navec2, HAM_gap)

#OECD
czOECDhp <- mFilter::hpfilter(gdpCZ, freq = 133107.94)
czOECDhpcycle <- mFilter::hpfilter(czOECDhp$cycle, freq = 13.93)
czOECDbc <- czOECDhpcycle$trend

date3 <- seq(as.Date("1996/03/31"), as.Date("2023/03/31"), by = "quarter")
dfbox <- data.frame(date3, CNBgap, 100*gdpCZhp2$cycle, gdpCZhp1, HAM_gap1, 100*czOECDbc)
colnames(dfbox) <- c("Time", "CNB_gap", "hp2", "hp1", "HAM_gap", "OECD_gap")

ggplot(dfbox, aes(x=Time)) + 
  geom_line(aes(y = CNB_gap), color = "firebrick") +
  geom_line(aes(y = hp2), color = "turquoise4") +
  geom_line(aes(y = hp1), color = "yellow") +
  geom_line(aes(y = HAM_gap), color = "darkslategray1") +
  geom_line(aes(y = OECD_gap), color = "palegreen1") +
  geom_hline(yintercept = 0) +
  xlab("Čas") +
  ylab("% potenciálního HDP") +
  ggtitle("Mezera výstupu - H-P filtr vs. metoda produkční funkce") +
  theme_bw()

write.xlsx(dfbox, file = "grafy_zaostřeno_box.xlsx", sheetName = "grafbox", colNames = TRUE, asTable = TRUE)

#test česko
datacz <- pdfetch_FRED("QCZPAM770A")

a <- hpfilter(datacz, freq = 400000)
plot(a)

ctgcz <- as.data.frame(log(na.omit(datacz)))

ctgcztrend <- hp1(ctgcz, lambda = 400000)
ctgczcycle <- 100*(ctgcz - ctgcztrend)

plot(ctgczcycle$QCZPAM770A, type = "l")

autoplot(datacz)
