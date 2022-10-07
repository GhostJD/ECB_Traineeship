library(haven)
library(readxl)
library(car)
library(lars)
library(mgcv)
library(lmtest)
library(stargazer)
library(foreign)
library(sandwich)
library(plm)

MPC <- read_excel("C:/Users/dodac/Desktop/Thesis/Data/MPC.xlsx")
View(MPC)

###########################
##### without weights #####
###########################

lm1 <- lm(mpc ~ GDPpcgrowth + inf + unemp + GINI + polrate + GDPpctoworld + taxtoGDP + govdebttoGDP + agrictoGDP + ruleoflaw + gsavingstoGDP + powdist+ individ + mascul + unceravoid + ltorient + indulg, data = MPC)
summary(lm1)

bptest(lm1)

##########################
##### adding weights #####
##########################

lm2 <- lm(mpc ~ GDPpcgrowth + inf + unemp + GINI + polrate + GDPpctoworld + taxtoGDP + govdebttoGDP + agrictoGDP + ruleoflaw + gsavingstoGDP + powdist+ individ + mascul + unceravoid + ltorient + indulg, data = MPC, weights = weights)
summary(lm2)

plot(lm2$residuals)

cov1 <- vcovHC(lm2, type = "HC0")

bptest(lm2)

#####################
##### WLS LASSO #####
#####################

#LASSO
mm <- model.matrix(mpc ~ GDPpcgrowth + inf + unemp + GINI + polrate + GDPpctoworld + taxtoGDP + govdebttoGDP + agrictoGDP + ruleoflaw + gsavingstoGDP + powdist+ individ + mascul + unceravoid + ltorient + indulg, data = MPC, weights = weights) 
mm <- mm[,-1]
lasso <- lars(x=mm, y=MPC$mpc, trace=TRUE)
plot(lasso)

#cross-validation
set.seed(9563215)
cv <- cv.lars(x=mm, y=MPC$mpc, K=10)
minMSE <- cv$index[cv$cv == min(cv$cv)]
minMSE
betas <- coef(lasso, s=minMSE, mode="fraction")
nonzerobetas <- betas[betas!=0]
nonzerobetas

#adjusting the model
lm3 <- lm(mpc ~ GDPpcgrowth + inf + unemp + GINI + polrate + GDPpctoworld + govdebttoGDP + ruleoflaw + gsavingstoGDP + powdist + individ + mascul + unceravoid + indulg, data = MPC, weights = weights)
summary(lm3)
#plot(lm3$residuals)

bptest(lm3)

cov2 <- vcovHC(lm3, type = "HC0")

############################
##### WLS mpc_se ###########
############################

lm4 <- lm(mpc ~ mpc_se + GDPpcgrowth + inf + unemp + GINI + polrate + GDPpctoworld + taxtoGDP + govdebttoGDP + agrictoGDP + ruleoflaw + gsavingstoGDP + powdist+ individ + mascul + unceravoid + ltorient + indulg, data = MPC, weights = weights)
summary(lm4)

bptest(lm4)

cov3 <- vcovHC(lm4, type = "HC0")

#############################
##### LASSO WLS mpc_se ######
#############################

#LASSO
mm2 <- model.matrix(mpc ~ mpc_se + GDPpcgrowth + inf + unemp + GINI + polrate + GDPpctoworld + taxtoGDP + govdebttoGDP + agrictoGDP + ruleoflaw + gsavingstoGDP + powdist+ individ + mascul + unceravoid + ltorient + indulg, data = MPC, weights = weights) 
mm2 <- mm2[,-1]
lasso2 <- lars(x=mm2, y=MPC$mpc, trace=TRUE)
plot(lasso2)

#cross-validation
set.seed(9563215)
cv2 <- cv.lars(x=mm2, y=MPC$mpc, K=10)
minMSE2 <- cv2$index[cv2$cv == min(cv2$cv)]
minMSE2
betas2 <- coef(lasso2, s=minMSE2, mode="fraction")
nonzerobetas2 <- betas2[betas2!=0]
nonzerobetas2

#adjusting the model
lm5 <- lm(mpc ~ mpc_se + GDPpcgrowth + inf + unemp + GINI + polrate + GDPpctoworld + govdebttoGDP + agrictoGDP + ruleoflaw + gsavingstoGDP + powdist + individ + mascul + unceravoid + indulg, data = MPC, weights = weights)
summary(lm5)

cov4 <- vcovHC(lm5, type = "HC0")

##########################
#####Publication bias#####
##########################


pbias2 <- lm(mpc/mpc_se ~ 1/mpc_se, data = MPC)
summary(pbias2)

stargazer(pbias2, type = "latex", align = TRUE)
####################################
##### results using Czech data #####
####################################

#WLS
Czech2010 <- c(1,2.13688048446924,-1.42531399212487,7.28000020980835,26.6,0.8333,2.0775888670544,13.6507574751995,37.1,1.54059611257066,0.74,22.6566432862838,57,58,57,74,70.0251889168766,29.4642857142857)
Czech2011 <- c(1,1.55018979013502,-0.0205719580093699,6.71000003814697,26.4,0.75,2.0792462970249,14.5202634059379,39.7,1.98251099186352,0.74,22.4606374472755,57,58,57,74,70.0251889168766,29.4642857142857)
Czech2015 <- c(1,5.18140265379816,0.992276160290274,5.05000019073486,25.9,0.05,1.74403462048631,14.7695604553833,39.7,2.21121387268241,0.74,27.0937750624147,57,58,57,74,70.0251889168766,29.4642857142857)
Czech2016 <- c(1,2.34055226080498,1.14145060120347,3.95000004768372,25.4,0.05,1.80938923237553,14.9127775532102,36.6,2.08704712424115,0.74,26.3493690967401,57,58,57,74,70.0251889168766,29.4642857142857)

#LASSO WLS
Czechlasso2010 <- c(1,2.13688048446924,-1.42531399212487,7.28000020980835,26.6,0.8333,2.0775888670544,37.1,0.74,22.6566432862838,57,58,57,74,29.4642857142857)
Czechlasso2011 <- c(1,1.55018979013502,-0.0205719580093699,6.71000003814697,26.4,0.75,2.0792462970249,39.7,0.74,22.4606374472755,57,58,57,74,29.4642857142857)
Czechlasso2015 <- c(1,5.18140265379816,0.992276160290274,5.05000019073486,25.9,0.05,1.74403462048631,39.7,0.74,27.0937750624147,57,58,57,74,29.4642857142857)
Czechlasso2016 <- c(1,2.34055226080498,1.14145060120347,3.95000004768372,25.4,0.05,1.80938923237553,36.6,0.74,26.3493690967401,57,58,57,74,29.4642857142857)

#WLS PB
Czech2010pb <- c(1,0,2.13688048446924,-1.42531399212487,7.28000020980835,26.6,0.8333,2.0775888670544,13.6507574751995,37.1,1.54059611257066,0.74,22.6566432862838,57,58,57,74,70.0251889168766,29.4642857142857)
Czech2011pb <- c(1,0,1.55018979013502,-0.0205719580093699,6.71000003814697,26.4,0.75,2.0792462970249,14.5202634059379,39.7,1.98251099186352,0.74,22.4606374472755,57,58,57,74,70.0251889168766,29.4642857142857)
Czech2015pb <- c(1,0,5.18140265379816,0.992276160290274,5.05000019073486,25.9,0.05,1.74403462048631,14.7695604553833,39.7,2.21121387268241,0.74,27.0937750624147,57,58,57,74,70.0251889168766,29.4642857142857)
Czech2016pb <- c(1,0,2.34055226080498,1.14145060120347,3.95000004768372,25.4,0.05,1.80938923237553,14.9127775532102,36.6,2.08704712424115,0.74,26.3493690967401,57,58,57,74,70.0251889168766,29.4642857142857)

#LASSO WLS PB
Czech2010lassopb <- c(1,0,2.13688048446924,-1.42531399212487,7.28000020980835,26.6,0.8333,2.0775888670544,37.1,1.54059611257066,0.74,22.6566432862838,57,58,57,74,29.4642857142857)
Czech2011lassopb <- c(1,0,1.55018979013502,-0.0205719580093699,6.71000003814697,26.4,0.75,2.0792462970249,39.7,1.98251099186352,0.74,22.4606374472755,57,58,57,74,29.4642857142857)
Czech2015lassopb <- c(1,0,5.18140265379816,0.992276160290274,5.05000019073486,25.9,0.05,1.74403462048631,39.7,2.21121387268241,0.74,27.0937750624147,57,58,57,74,29.4642857142857)
Czech2016lassopb <- c(1,0,2.34055226080498,1.14145060120347,3.95000004768372,25.4,0.05,1.80938923237553,36.6,2.08704712424115,0.74,26.3493690967401,57,58,57,74,29.4642857142857)

#predicted MPC for Czech Republic - WLS
sum(coef(lm2)*Czech2010)
sum(coef(lm2)*Czech2011)
sum(coef(lm2)*Czech2015)
sum(coef(lm2)*Czech2016)

#predicted MPC for Czech Republic - WLS LASSO model
sum(coef(lm3)*Czech2010lasso)
sum(coef(lm3)*Czech2011lasso)
sum(coef(lm3)*Czech2015lasso)
sum(coef(lm3)*Czech2016lasso)

#predicted MPC for Czech Republic - WLS PB
sum(coef(lm4)*Czech2010pb)
sum(coef(lm4)*Czech2011pb)
sum(coef(lm4)*Czech2015pb)
sum(coef(lm4)*Czech2016pb)

#predicted MPC for Czech Republic - WLS LASSO PB
sum(coef(lm5)*Czech2010lassopb)
sum(coef(lm5)*Czech2011lassopb)
sum(coef(lm5)*Czech2015lassopb)
sum(coef(lm5)*Czech2016lassopb)

################################################
#####confidence intervals of the prediction#####
################################################

#WLS
model_predictWLS2010 <- lm(mpc ~ I(GDPpcgrowth-Czech2010[2]) + I(inf-Czech2010[3]) + I(unemp-Czech2010[4]) + I(GINI-Czech2010[5]) + I(polrate-Czech2010[6]) + I(GDPpctoworld-Czech2010[7]) + I(taxtoGDP-Czech2010[8]) + I(govdebttoGDP-Czech2010[9]) + I(agrictoGDP-Czech2010[10]) + I(ruleoflaw-Czech2010[11]) + I(gsavingstoGDP-Czech2010[12]) + I(powdist-Czech2010[13]) + I(individ-Czech2010[14]) + I(mascul-Czech2010[15]) + I(unceravoid-Czech2010[16]) + I(ltorient-Czech2010[17]) + I(indulg-Czech2010[18]), data = MPC, weights = weights)
summary(model_predictWLS2010)
confint(model_predictWLS2010)["(Intercept)",]

model_predictWLS2011 <- lm(mpc ~ I(GDPpcgrowth-Czech2011[2]) + I(inf-Czech2011[3]) + I(unemp-Czech2011[4]) + I(GINI-Czech2011[5]) + I(polrate-Czech2011[6]) + I(GDPpctoworld-Czech2011[7]) + I(taxtoGDP-Czech2011[8]) + I(govdebttoGDP-Czech2011[9]) + I(agrictoGDP-Czech2011[10]) + I(ruleoflaw-Czech2011[11]) + I(gsavingstoGDP-Czech2011[12]) + I(powdist-Czech2011[13]) + I(individ-Czech2011[14]) + I(mascul-Czech2011[15]) + I(unceravoid-Czech2011[16]) + I(ltorient-Czech2011[17]) + I(indulg-Czech2011[18]), data = MPC, weights = weights)
summary(model_predictWLS2011)
confint(model_predictWLS2011)["(Intercept)",]

model_predictWLS2015 <- lm(mpc ~ I(GDPpcgrowth-Czech2015[2]) + I(inf-Czech2015[3]) + I(unemp-Czech2015[4]) + I(GINI-Czech2015[5]) + I(polrate-Czech2015[6]) + I(GDPpctoworld-Czech2015[7]) + I(taxtoGDP-Czech2015[8]) + I(govdebttoGDP-Czech2015[9]) + I(agrictoGDP-Czech2015[10]) + I(ruleoflaw-Czech2015[11]) + I(gsavingstoGDP-Czech2015[12]) + I(powdist-Czech2015[13]) + I(individ-Czech2015[14]) + I(mascul-Czech2015[15]) + I(unceravoid-Czech2015[16]) + I(ltorient-Czech2015[17]) + I(indulg-Czech2015[18]), data = MPC, weights = weights)
summary(model_predictWLS2015)
confint(model_predictWLS2015)["(Intercept)",]

model_predictWLS2016 <- lm(mpc ~ I(GDPpcgrowth-Czech2016[2]) + I(inf-Czech2016[3]) + I(unemp-Czech2016[4]) + I(GINI-Czech2016[5]) + I(polrate-Czech2016[6]) + I(GDPpctoworld-Czech2016[7]) + I(taxtoGDP-Czech2016[8]) + I(govdebttoGDP-Czech2016[9]) + I(agrictoGDP-Czech2016[10]) + I(ruleoflaw-Czech2016[11]) + I(gsavingstoGDP-Czech2016[12]) + I(powdist-Czech2016[13]) + I(individ-Czech2016[14]) + I(mascul-Czech2016[15]) + I(unceravoid-Czech2016[16]) + I(ltorient-Czech2016[17]) + I(indulg-Czech2016[18]), data = MPC, weights = weights)
summary(model_predictWLS2016)
confint(model_predictWLS2016)["(Intercept)",]

#LASSO
model_predictlasso2010 <- lm(mpc ~ I(GDPpcgrowth-Czechlasso2010[2]) + I(inf-Czechlasso2010[3]) + I(unemp-Czechlasso2010[4]) + I(GINI-Czechlasso2010[5]) + I(polrate-Czechlasso2010[6]) + I(GDPpctoworld-Czechlasso2010[7]) + I(govdebttoGDP-Czechlasso2010[8]) + I(ruleoflaw-Czechlasso2010[9]) + I(gsavingstoGDP-Czechlasso2010[10]) + I(powdist-Czechlasso2010[11]) + I(individ-Czechlasso2010[12]) + I(mascul-Czechlasso2010[13]) + I(unceravoid-Czechlasso2010[14]) + I(indulg-Czechlasso2010[15]), data = MPC, weights = weights)
summary(model_predictlasso2010)
confint(model_predictlasso2010)["(Intercept)",]

model_predictlasso2011 <- lm(mpc ~ I(GDPpcgrowth-Czechlasso2011[2]) + I(inf-Czechlasso2011[3]) + I(unemp-Czechlasso2011[4]) + I(GINI-Czechlasso2011[5]) + I(polrate-Czechlasso2011[6]) + I(GDPpctoworld-Czechlasso2011[7]) + I(govdebttoGDP-Czechlasso2011[8]) + I(ruleoflaw-Czechlasso2011[9]) + I(gsavingstoGDP-Czechlasso2011[10]) + I(powdist-Czechlasso2011[11]) + I(individ-Czechlasso2011[12]) + I(mascul-Czechlasso2011[13]) + I(unceravoid-Czechlasso2011[14]) + I(indulg-Czechlasso2011[15]), data = MPC, weights = weights)
summary(model_predictlasso2011)
confint(model_predictlasso2011)["(Intercept)",]

model_predictlasso2015 <- lm(mpc ~ I(GDPpcgrowth-Czechlasso2015[2]) + I(inf-Czechlasso2015[3]) + I(unemp-Czechlasso2015[4]) + I(GINI-Czechlasso2015[5]) + I(polrate-Czechlasso2015[6]) + I(GDPpctoworld-Czechlasso2015[7]) + I(govdebttoGDP-Czechlasso2015[8]) + I(ruleoflaw-Czechlasso2015[9]) + I(gsavingstoGDP-Czechlasso2015[10]) + I(powdist-Czechlasso2015[11]) + I(individ-Czechlasso2015[12]) + I(mascul-Czechlasso2015[13]) + I(unceravoid-Czechlasso2015[14]) + I(indulg-Czechlasso2015[15]), data = MPC, weights = weights)
summary(model_predictlasso2015)
confint(model_predictlasso2015)["(Intercept)",]

model_predictlasso2016 <- lm(mpc ~ I(GDPpcgrowth-Czechlasso2016[2]) + I(inf-Czechlasso2016[3]) + I(unemp-Czechlasso2016[4]) + I(GINI-Czechlasso2016[5]) + I(polrate-Czechlasso2016[6]) + I(GDPpctoworld-Czechlasso2016[7]) + I(govdebttoGDP-Czechlasso2016[8]) + I(ruleoflaw-Czechlasso2016[9]) + I(gsavingstoGDP-Czechlasso2016[10]) + I(powdist-Czechlasso2016[11]) + I(individ-Czechlasso2016[12]) + I(mascul-Czechlasso2016[13]) + I(unceravoid-Czechlasso2016[14]) + I(indulg-Czechlasso2016[15]), data = MPC, weights = weights)
summary(model_predictlasso2016)
confint(model_predictlasso2016)["(Intercept)",]

#WLS controlling for PB
model_predictWLS2010pb <- lm(mpc ~ I(mpc_se-Czech2010pb[2])+I(GDPpcgrowth-Czech2010pb[3]) + I(inf-Czech2010pb[4]) + I(unemp-Czech2010pb[5]) + I(GINI-Czech2010pb[6]) + I(polrate-Czech2010pb[7]) + I(GDPpctoworld-Czech2010pb[8]) + I(taxtoGDP-Czech2010pb[9]) + I(govdebttoGDP-Czech2010pb[10]) + I(agrictoGDP-Czech2010pb[11]) + I(ruleoflaw-Czech2010pb[12]) + I(gsavingstoGDP-Czech2010pb[13]) + I(powdist-Czech2010pb[14]) + I(individ-Czech2010pb[15]) + I(mascul-Czech2010pb[16]) + I(unceravoid-Czech2010pb[17]) + I(ltorient-Czech2010pb[18]) + I(indulg-Czech2010pb[19]), data = MPC, weights = weights)
summary(model_predictWLS2010pb)
confint(model_predictWLS2010pb)["(Intercept)",]

model_predictWLS2011pb <- lm(mpc ~ I(mpc_se-Czech2011pb[2])+I(GDPpcgrowth-Czech2011pb[3]) + I(inf-Czech2011pb[4]) + I(unemp-Czech2011pb[5]) + I(GINI-Czech2011pb[6]) + I(polrate-Czech2011pb[7]) + I(GDPpctoworld-Czech2011pb[8]) + I(taxtoGDP-Czech2011pb[9]) + I(govdebttoGDP-Czech2011pb[10]) + I(agrictoGDP-Czech2011pb[11]) + I(ruleoflaw-Czech2011pb[12]) + I(gsavingstoGDP-Czech2011pb[13]) + I(powdist-Czech2011pb[14]) + I(individ-Czech2011pb[15]) + I(mascul-Czech2011pb[16]) + I(unceravoid-Czech2011pb[17]) + I(ltorient-Czech2011pb[18]) + I(indulg-Czech2011pb[19]), data = MPC, weights = weights)
summary(model_predictWLS2011pb)
confint(model_predictWLS2011pb)["(Intercept)",]

model_predictWLS2015pb <- lm(mpc ~ I(mpc_se-Czech2015pb[2])+I(GDPpcgrowth-Czech2015pb[3]) + I(inf-Czech2015pb[4]) + I(unemp-Czech2015pb[5]) + I(GINI-Czech2015pb[6]) + I(polrate-Czech2015pb[7]) + I(GDPpctoworld-Czech2015pb[8]) + I(taxtoGDP-Czech2015pb[9]) + I(govdebttoGDP-Czech2015pb[10]) + I(agrictoGDP-Czech2015pb[11]) + I(ruleoflaw-Czech2015pb[12]) + I(gsavingstoGDP-Czech2015pb[13]) + I(powdist-Czech2015pb[14]) + I(individ-Czech2015pb[15]) + I(mascul-Czech2015pb[16]) + I(unceravoid-Czech2015pb[17]) + I(ltorient-Czech2015pb[18]) + I(indulg-Czech2015pb[19]), data = MPC, weights = weights)
summary(model_predictWLS2015pb)
confint(model_predictWLS2015pb)["(Intercept)",]

model_predictWLS2016pb <- lm(mpc ~ I(mpc_se-Czech2016pb[2])+I(GDPpcgrowth-Czech2016pb[3]) + I(inf-Czech2016pb[4]) + I(unemp-Czech2016pb[5]) + I(GINI-Czech2016pb[6]) + I(polrate-Czech2016pb[7]) + I(GDPpctoworld-Czech2016pb[8]) + I(taxtoGDP-Czech2016pb[9]) + I(govdebttoGDP-Czech2016pb[10]) + I(agrictoGDP-Czech2016pb[11]) + I(ruleoflaw-Czech2016pb[12]) + I(gsavingstoGDP-Czech2016pb[13]) + I(powdist-Czech2016pb[14]) + I(individ-Czech2016pb[15]) + I(mascul-Czech2016pb[16]) + I(unceravoid-Czech2016pb[17]) + I(ltorient-Czech2016pb[18]) + I(indulg-Czech2016pb[19]), data = MPC, weights = weights)
summary(model_predictWLS2016pb)
confint(model_predictWLS2016pb)["(Intercept)",]

model_predict2010lassopb <- lm(mpc ~ I(mpc_se-Czech2010lassopb[2])+ I(GDPpcgrowth-Czech2010lassopb[3]) + I(inf-Czech2010lassopb[4]) + I(unemp-Czech2010lassopb[5]) + I(GINI-Czech2010lassopb[6]) + I(polrate-Czech2010lassopb[7]) + I(GDPpctoworld-Czech2010lassopb[8]) + I(govdebttoGDP-Czech2010lassopb[9]) + I(agrictoGDP-Czech2010lassopb[10]) + I(ruleoflaw-Czech2010lassopb[11]) + I(gsavingstoGDP-Czech2010lassopb[12]) + I(powdist-Czech2010lassopb[13]) + I(individ-Czech2010lassopb[14]) + I(mascul-Czech2010lassopb[15]) + I(unceravoid-Czech2010lassopb[16]) + I(indulg-Czech2010lassopb[17]), data = MPC, weights = weights)
summary(model_predict2010lassopb)
confint(model_predict2010lassopb)["(Intercept)",]

model_predict2011lassopb <- lm(mpc ~ I(mpc_se-Czech2011lassopb[2])+ I(GDPpcgrowth-Czech2011lassopb[3]) + I(inf-Czech2011lassopb[4]) + I(unemp-Czech2011lassopb[5]) + I(GINI-Czech2011lassopb[6]) + I(polrate-Czech2011lassopb[7]) + I(GDPpctoworld-Czech2011lassopb[8]) + I(govdebttoGDP-Czech2011lassopb[9]) + I(agrictoGDP-Czech2011lassopb[10]) + I(ruleoflaw-Czech2011lassopb[11]) + I(gsavingstoGDP-Czech2011lassopb[12]) + I(powdist-Czech2011lassopb[13]) + I(individ-Czech2011lassopb[14]) + I(mascul-Czech2011lassopb[15]) + I(unceravoid-Czech2011lassopb[16]) + I(indulg-Czech2011lassopb[17]), data = MPC, weights = weights)
summary(model_predict2011lassopb)
confint(model_predict2011lassopb)["(Intercept)",]

model_predict2015lassopb <- lm(mpc ~ I(mpc_se-Czech2015lassopb[2])+ I(GDPpcgrowth-Czech2015lassopb[3]) + I(inf-Czech2015lassopb[4]) + I(unemp-Czech2015lassopb[5]) + I(GINI-Czech2015lassopb[6]) + I(polrate-Czech2015lassopb[7]) + I(GDPpctoworld-Czech2015lassopb[8]) + I(govdebttoGDP-Czech2015lassopb[9]) + I(agrictoGDP-Czech2015lassopb[10]) + I(ruleoflaw-Czech2015lassopb[11]) + I(gsavingstoGDP-Czech2015lassopb[12]) + I(powdist-Czech2015lassopb[13]) + I(individ-Czech2015lassopb[14]) + I(mascul-Czech2015lassopb[15]) + I(unceravoid-Czech2015lassopb[16]) + I(indulg-Czech2015lassopb[17]), data = MPC, weights = weights)
summary(model_predict2015lassopb)
confint(model_predict2015lassopb)["(Intercept)",]

model_predict2016lassopb <- lm(mpc ~ I(mpc_se-Czech2016lassopb[2])+ I(GDPpcgrowth-Czech2016lassopb[3]) + I(inf-Czech2016lassopb[4]) + I(unemp-Czech2016lassopb[5]) + I(GINI-Czech2016lassopb[6]) + I(polrate-Czech2016lassopb[7]) + I(GDPpctoworld-Czech2016lassopb[8]) + I(govdebttoGDP-Czech2016lassopb[9]) + I(agrictoGDP-Czech2016lassopb[10]) + I(ruleoflaw-Czech2016lassopb[11]) + I(gsavingstoGDP-Czech2016lassopb[12]) + I(powdist-Czech2016lassopb[13]) + I(individ-Czech2016lassopb[14]) + I(mascul-Czech2016lassopb[15]) + I(unceravoid-Czech2016lassopb[16]) + I(indulg-Czech2016lassopb[17]), data = MPC, weights = weights)
summary(model_predict2016lassopb)
confint(model_predict2016lassopb)["(Intercept)",]

##############################
#####information criteria#####
##############################

#AIC
AIC(lm2,lm3,lm4,lm5)

#BIC
BIC(lm2,lm3,lm4,lm5)

###############
#####plots#####
###############
plot1data <- subset(MPC, (1/MPC$mpc_se)<100)
plot(plot1data$mpc,1/(plot1data$mpc_se),main = "Funnel plot", xlab = "Estimate of the MPC", ylab = "Precision of the estimate (1/SE)")

stargazer(lm2, lm3, lm4, lm5, se = list(sqrt(diag(cov1)),sqrt(diag(cov2)),sqrt(diag(cov3)),sqrt(diag(cov4))), title = "Regression Results", align = TRUE, type = "latex",no.space = TRUE, column.sep.width = "1pt", font.size = "small", omit.stat=c("ser","f"), column.labels = c("WLS", "LASSO", "WLS - PB", "LASSO - PB"))
