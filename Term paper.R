#############################                         #############################
############################# PIRMINE DUOMENU ANALIZE #############################
#############################                         ############################# 

# Nagrinėjamai temai surinktus duomenis būtina aprašyti bei išanalizuoti. Tam naudojama pirminė duomenų analizė. 
# Šioje dalyje pateikiama duomenų santrauka (pagrindinės charatkteristikos) lentelių bei grafikų pagalba. 
# Apžvelgiamos matomos tendencijos.

# AKCIJU DUOMENU ISTRAUKIMAS

set.seed(42)
library(quantmod) 
library(dplyr)
library(tidyr) 
library(ggplot2) 
library(readr) 
library(kableExtra)
library(stargazer) 

library(lmtest)
library(plm)


# Section 1 ---------------------------------------------------------------

####          ####
####  2014 m. ####
####          ####

# SP500 imoniu P_adj:

symbols <- c("CHD", "ILMN", "VRSK", "NWS", "UAL",
             "ATVI", "AAP", "JBHT", "UDR", "O", "AAL",
             "EQIX", "HSIC", "SWKS", "HCA")

itrauktos <-lapply(symbols, function(x) {getSymbols(x, from = "2014/01/01", 
                                                    to = "2014/12/31",
                                                    periodicity = "daily",
                                                    auto.assign = FALSE)} )

sp_2014 <- lapply(itrauktos, Ad)
sp_2014 <- do.call(merge, sp_2014)

# grazos skaiciavimas:

# CHD

p0 <- as.numeric(sp_2014$CHD.Adjusted[1])
p1 <- as.numeric(sp_2014$CHD.Adjusted[nrow(sp_2014)]) 
graza_CHD <- ((p1 - p0) / p0) * 100

# ILMN

p0 <- as.numeric(sp_2014$ILMN.Adjusted[1])
p1 <- as.numeric(sp_2014$ILMN.Adjusted[nrow(sp_2014)])
graza_ILMN <- ((p1 - p0) / p0) * 100

# VRSK

p0 <- as.numeric(sp_2014$VRSK.Adjusted[1])
p1 <- as.numeric(sp_2014$VRSK.Adjusted[nrow(sp_2014)]) 
graza_VRSK <- ((p1 - p0) / p0) * 100

# NWS

p0 <- as.numeric(sp_2014$NWS.Adjusted[1]) 
p1 <- as.numeric(sp_2014$NWS.Adjusted[nrow(sp_2014)])
graza_NWS <- ((p1 - p0) / p0) * 100

# UAL

p0 <- as.numeric(sp_2014$UAL.Adjusted[1]) 
p1 <- as.numeric(sp_2014$UAL.Adjusted[nrow(sp_2014)]) 
graza_UAL <- ((p1 - p0) / p0) * 100

# ATVI

p0 <- as.numeric(sp_2014$ATVI.Adjusted[1]) 
p1 <- as.numeric(sp_2014$ATVI.Adjusted[nrow(sp_2014)]) 
graza_ATVI <- ((p1 - p0) / p0) * 100

# AAP

p0 <- as.numeric(sp_2014$AAP.Adjusted[1]) 
p1 <- as.numeric(sp_2014$AAP.Adjusted[nrow(sp_2014)]) 
graza_AAP <- ((p1 - p0) / p0) * 100

# JBHT

p0 <- as.numeric(sp_2014$JBHT.Adjusted[1]) 
p1 <- as.numeric(sp_2014$JBHT.Adjusted[nrow(sp_2014)])
graza_JBHT <- ((p1 - p0) / p0) * 100

# UDR

p0 <- as.numeric(sp_2014$UDR.Adjusted[1]) 
p1 <- as.numeric(sp_2014$UDR.Adjusted[nrow(sp_2014)]) 
graza_UDR <- ((p1 - p0) / p0) * 100

# O

p0 <- as.numeric(sp_2014$O.Adjusted[1]) 
p1 <- as.numeric(sp_2014$O.Adjusted[nrow(sp_2014)]) 
graza_O <- ((p1 - p0) / p0) * 100

# AAL

p0 <- as.numeric(sp_2014$AAL.Adjusted[1])
p1 <- as.numeric(sp_2014$AAL.Adjusted[nrow(sp_2014)]) 
graza_AAL <- ((p1 - p0) / p0) * 100

# EQIX

p0 <- as.numeric(sp_2014$EQIX.Adjusted[1]) 
p1 <- as.numeric(sp_2014$EQIX.Adjusted[nrow(sp_2014)]) 
graza_EQIX <- ((p1 - p0) / p0) * 100

# HSIC

p0 <- as.numeric(sp_2014$HSIC.Adjusted[1]) 
p1 <- as.numeric(sp_2014$HSIC.Adjusted[nrow(sp_2014)])
graza_HSIC <- ((p1 - p0) / p0) * 100

# SWKS

p0 <- as.numeric(sp_2014$SWKS.Adjusted[1]) 
p1 <- as.numeric(sp_2014$SWKS.Adjusted[nrow(sp_2014)]) 
graza_SWKS <- ((p1 - p0) / p0) * 100

# HCA

p0 <- as.numeric(sp_2014$HCA.Adjusted[1]) 
p1 <- as.numeric(sp_2014$HCA.Adjusted[nrow(sp_2014)]) 
graza_HCA <- ((p1 - p0) / p0) * 100

# Section 2 ---------------------------------------------------------------


####          ####
####  2016 m. ####
####          ####


itrauktos <-lapply(c("CHD", "ILMN", "VRSK", "NWS", "UAL",
                     "ATVI", "AAP", "JBHT", "UDR", "O", "AAL",
                     "EQIX", "HSIC", "SWKS", "HCA"), function(x) {getSymbols(x, 
                                                                             from = "2016/01/01", 
                                                                             to = "2016/12/31",
                                                                             periodicity = "daily",
                                                                             auto.assign=FALSE)} )

sp_2016 <- lapply(itrauktos, Ad)
sp_2016 <- do.call(merge, sp_2016)


# CHD

p0 <- as.numeric(sp_2016$CHD.Adjusted[1]) 
p1 <- as.numeric(sp_2016$CHD.Adjusted[nrow(sp_2016)]) 
graza_CHD2 <- ((p1 - p0) / p0) * 100

# ILMN

p0 <- as.numeric(sp_2016$ILMN.Adjusted[1]) 
p1 <- as.numeric(sp_2016$ILMN.Adjusted[nrow(sp_2016)]) 
graza_ILMN2 <- ((p1 - p0) / p0) * 100

# VRSK

p0 <- as.numeric(sp_2016$VRSK.Adjusted[1]) 
p1 <- as.numeric(sp_2016$VRSK.Adjusted[nrow(sp_2016)]) 
graza_VRSK2 <- ((p1 - p0) / p0) * 100

# NWS

p0 <- as.numeric(sp_2016$NWS.Adjusted[1]) 
p1 <- as.numeric(sp_2016$NWS.Adjusted[nrow(sp_2016)]) 
graza_NWS2 <- ((p1 - p0) / p0) * 100

# UAL

p0 <- as.numeric(sp_2016$UAL.Adjusted[1]) 
p1 <- as.numeric(sp_2016$UAL.Adjusted[nrow(sp_2016)]) 
graza_UAL2 <- ((p1 - p0) / p0) * 100

# ATVI

p0 <- as.numeric(sp_2016$ATVI.Adjusted[1]) 
p1 <- as.numeric(sp_2016$ATVI.Adjusted[nrow(sp_2016)]) 
graza_ATVI2 <- ((p1 - p0) / p0) * 100

# AAP

p0 <- as.numeric(sp_2016$AAP.Adjusted[1]) 
p1 <- as.numeric(sp_2016$AAP.Adjusted[nrow(sp_2016)]) 
graza_AAP2 <- ((p1 - p0) / p0) * 100

# JBHT

p0 <- as.numeric(sp_2016$JBHT.Adjusted[1]) 
p1 <- as.numeric(sp_2016$JBHT.Adjusted[nrow(sp_2016)])
graza_JBHT2 <- ((p1 - p0) / p0) * 100

# UDR

p0 <- as.numeric(sp_2016$UDR.Adjusted[1]) 
p1 <- as.numeric(sp_2016$UDR.Adjusted[nrow(sp_2016)])
graza_UDR2 <- ((p1 - p0) / p0) * 100

# O

p0 <- as.numeric(sp_2016$O.Adjusted[1]) 
p1 <- as.numeric(sp_2016$O.Adjusted[nrow(sp_2016)])
graza_O2 <- ((p1 - p0) / p0) * 100

# AAL

p0 <- as.numeric(sp_2016$AAL.Adjusted[1]) 
p1 <- as.numeric(sp_2016$AAL.Adjusted[nrow(sp_2016)]) 
graza_AAL2 <- ((p1 - p0) / p0) * 100

# EQIX

p0 <- as.numeric(sp_2016$EQIX.Adjusted[1]) 
p1 <- as.numeric(sp_2016$EQIX.Adjusted[nrow(sp_2016)]) 
graza_EQIX2 <- ((p1 - p0) / p0) * 100

# HSIC

p0 <- as.numeric(sp_2016$HSIC.Adjusted[1]) 
p1 <- as.numeric(sp_2016$HSIC.Adjusted[nrow(sp_2016)]) 
graza_HSIC2 <- ((p1 - p0) / p0) * 100

# SWKS

p0 <- as.numeric(sp_2016$SWKS.Adjusted[1]) 
p1 <- as.numeric(sp_2016$SWKS.Adjusted[nrow(sp_2016)]) 
graza_SWKS2 <- ((p1 - p0) / p0) * 100

# HCA

p0 <- as.numeric(sp_2016$HCA.Adjusted[1]) 
p1 <- as.numeric(sp_2016$HCA.Adjusted[nrow(sp_2016)]) 
graza_HCA2 <- ((p1 - p0) / p0) * 100


# Section 3 ---------------------------------------------------------------


# Neitrauktu imoniu P_adj: (2014 m.)


neitrauktos <-lapply(c("CNA", "NBIX", "CAR", "TTC", "AFG", "WOLF",
                       "WTRG", "ELS", "Y", "GGG", "UHAL","UTHR", "RS",
                       "DAR", "RPM", "WSO", "FICO", "RGEN",
                       "AZPN", "CSL", "AGCO", "EWBC", "SRPT",
                       "UNM", "LSI", "NOV", "RRX", "FLEX", "JBL", "MTN"), function(x) {getSymbols(x, 
                                                                         from = "2014/01/01", 
                                                                         to = "2014/12/31",
                                                                         periodicity = "daily",
                                                                         auto.assign = FALSE)} )


nonSP_2014 <- lapply(neitrauktos, Ad)
nonSP_2014 <- do.call(merge, nonSP_2014)



# CNA:

p0 <- as.numeric(nonSP_2014$CNA.Adjusted[1]) 
p1 <- as.numeric(nonSP_2014$CNA.Adjusted[nrow(nonSP_2014)]) 
graza_CNA <- ((p1 - p0) / p0) * 100


# NBIX

p0 <- as.numeric(nonSP_2014$NBIX.Adjusted[1]) 
p1 <- as.numeric(nonSP_2014$NBIX.Adjusted[nrow(nonSP_2014)]) 
graza_NBIX <- ((p1 - p0) / p0) * 100

# CAR

p0 <- as.numeric(nonSP_2014$CAR.Adjusted[1]) 
p1 <- as.numeric(nonSP_2014$CAR.Adjusted[nrow(nonSP_2014)]) 
graza_CAR <- ((p1 - p0) / p0) * 100

# TTC

p0 <- as.numeric(nonSP_2014$TTC.Adjusted[1]) 
p1 <- as.numeric(nonSP_2014$TTC.Adjusted[nrow(nonSP_2014)]) 
graza_TTC <- ((p1 - p0) / p0) * 100

# AFG

p0 <- as.numeric(nonSP_2014$AFG.Adjusted[1]) 
p1 <- as.numeric(nonSP_2014$AFG.Adjusted[nrow(nonSP_2014)]) 
graza_AFG <- ((p1 - p0) / p0) * 100

# WOLF

p0 <- as.numeric(nonSP_2014$WOLF.Adjusted[1]) 
p1 <- as.numeric(nonSP_2014$WOLF.Adjusted[nrow(nonSP_2014)]) 
graza_WOLF <- ((p1 - p0) / p0) * 100

# WTRG

p0 <- as.numeric(nonSP_2014$WTRG.Adjusted[1]) 
p1 <- as.numeric(nonSP_2014$WTRG.Adjusted[nrow(nonSP_2014)]) 
graza_WTRG <- ((p1 - p0) / p0) * 100

# ELS

p0 <- as.numeric(nonSP_2014$ELS.Adjusted[1]) 
p1 <- as.numeric(nonSP_2014$ELS.Adjusted[nrow(nonSP_2014)]) 
graza_ELS <- ((p1 - p0) / p0) * 100

# Y

p0 <- as.numeric(nonSP_2014$Y.Adjusted[1]) 
p1 <- as.numeric(nonSP_2014$Y.Adjusted[nrow(nonSP_2014)]) 
graza_Y <- ((p1 - p0) / p0) * 100

# GGG

p0 <- as.numeric(nonSP_2014$GGG.Adjusted[1]) 
p1 <- as.numeric(nonSP_2014$GGG.Adjusted[nrow(nonSP_2014)]) 
graza_GGG <- ((p1 - p0) / p0) * 100

# UHAL

p0 <- as.numeric(nonSP_2014$UHAL.Adjusted[1]) 
p1 <- as.numeric(nonSP_2014$UHAL.Adjusted[nrow(nonSP_2014)]) 
graza_UHAL <- ((p1 - p0) / p0) * 100

# UTHR

p0 <- as.numeric(nonSP_2014$UTHR.Adjusted[1]) 
p1 <- as.numeric(nonSP_2014$UTHR.Adjusted[nrow(nonSP_2014)]) 
graza_UTHR <- ((p1 - p0) / p0) * 100

# RS

p0 <- as.numeric(nonSP_2014$RS.Adjusted[1]) 
p1 <- as.numeric(nonSP_2014$RS.Adjusted[nrow(nonSP_2014)]) 
graza_RS <- ((p1 - p0) / p0) * 100

# DAR

p0 <- as.numeric(nonSP_2014$DAR.Adjusted[1]) 
p1 <- as.numeric(nonSP_2014$DAR.Adjusted[nrow(nonSP_2014)]) 
graza_DAR <- ((p1 - p0) / p0) * 100

# RPM

p0 <- as.numeric(nonSP_2014$RPM.Adjusted[1]) 
p1 <- as.numeric(nonSP_2014$RPM.Adjusted[nrow(nonSP_2014)]) 
graza_RPM <- ((p1 - p0) / p0) * 100

# WSO

p0 <- as.numeric(nonSP_2014$WSO.Adjusted[1]) 
p1 <- as.numeric(nonSP_2014$WSO.Adjusted[nrow(nonSP_2014)]) 
graza_WSO <- ((p1 - p0) / p0) * 100

# FICO

p0 <- as.numeric(nonSP_2014$FICO.Adjusted[1]) 
p1 <- as.numeric(nonSP_2014$FICO.Adjusted[nrow(nonSP_2014)]) 
graza_FICO <- ((p1 - p0) / p0) * 100

# RGEN

p0 <- as.numeric(nonSP_2014$RGEN.Adjusted[1]) 
p1 <- as.numeric(nonSP_2014$RGEN.Adjusted[nrow(nonSP_2014)]) 
graza_RGEN <- ((p1 - p0) / p0) * 100

# AZPN

p0 <- as.numeric(nonSP_2014$AZPN.Adjusted[1]) 
p1 <- as.numeric(nonSP_2014$AZPN.Adjusted[nrow(nonSP_2014)]) 
graza_AZPN <- ((p1 - p0) / p0) * 100

# CSL

p0 <- as.numeric(nonSP_2014$CSL.Adjusted[1]) 
p1 <- as.numeric(nonSP_2014$CSL.Adjusted[nrow(nonSP_2014)]) 
graza_CSL <- ((p1 - p0) / p0) * 100

# AGCO

p0 <- as.numeric(nonSP_2014$AGCO.Adjusted[1]) 
p1 <- as.numeric(nonSP_2014$AGCO.Adjusted[nrow(nonSP_2014)]) 
graza_AGCO <- ((p1 - p0) / p0) * 100

# EWBC

p0 <- as.numeric(nonSP_2014$EWBC.Adjusted[1]) 
p1 <- as.numeric(nonSP_2014$EWBC.Adjusted[nrow(nonSP_2014)]) 
graza_EWBC <- ((p1 - p0) / p0) * 100

# SRPT

p0 <- as.numeric(nonSP_2014$SRPT.Adjusted[1]) 
p1 <- as.numeric(nonSP_2014$SRPT.Adjusted[nrow(nonSP_2014)]) 
graza_SRPT <- ((p1 - p0) / p0) * 100

# UNM

p0 <- as.numeric(nonSP_2014$UNM.Adjusted[1]) 
p1 <- as.numeric(nonSP_2014$UNM.Adjusted[nrow(nonSP_2014)]) 
graza_UNM <- ((p1 - p0) / p0) * 100

# LSI

p0 <- as.numeric(nonSP_2014$LSI.Adjusted[1]) 
p1 <- as.numeric(nonSP_2014$LSI.Adjusted[nrow(nonSP_2014)]) 
graza_LSI <- ((p1 - p0) / p0) * 100

# NOV

p0 <- as.numeric(nonSP_2014$NOV.Adjusted[1]) 
p1 <- as.numeric(nonSP_2014$NOV.Adjusted[nrow(nonSP_2014)]) 
graza_NOV <- ((p1 - p0) / p0) * 100

# RRX

p0 <- as.numeric(nonSP_2014$RRX.Adjusted[1]) 
p1 <- as.numeric(nonSP_2014$RRX.Adjusted[nrow(nonSP_2014)]) 
graza_RRX <- ((p1 - p0) / p0) * 100

# FLEX

p0 <- as.numeric(nonSP_2014$FLEX.Adjusted[1]) 
p1 <- as.numeric(nonSP_2014$FLEX.Adjusted[nrow(nonSP_2014)]) 
graza_FLEX <- ((p1 - p0) / p0) * 100

# JBL

p0 <- as.numeric(nonSP_2014$JBL.Adjusted[1]) 
p1 <- as.numeric(nonSP_2014$JBL.Adjusted[nrow(nonSP_2014)]) 
graza_JBL <- ((p1 - p0) / p0) * 100

# MTN

p0 <- as.numeric(nonSP_2014$MTN.Adjusted[1]) 
p1 <- as.numeric(nonSP_2014$MTN.Adjusted[nrow(nonSP_2014)]) 
graza_MTN <- ((p1 - p0) / p0) * 100



# Section 4 ---------------------------------------------------------------

neitrauktos <-lapply(c("CNA", "NBIX", "CAR", "TTC", "AFG", "WOLF",
                       "WTRG", "ELS", "Y", "GGG", "UHAL","UTHR", "RS",
                       "DAR", "RPM", "WSO", "FICO", "RGEN",
                       "AZPN", "CSL", "AGCO", "EWBC", "SRPT",
                       "UNM", "LSI", "NOV", "RRX", "FLEX", "JBL", "MTN"), function(x) {getSymbols(x, 
                                                                                                  from = "2016/01/01", 
                                                                                                  to = "2016/12/31",
                                                                                                  periodicity = "daily",
                                                                                                  auto.assign = FALSE)} )


nonSP_2016 <- lapply(neitrauktos, Ad)
nonSP_2016 <- do.call(merge, nonSP_2016)



# CNA:

p0 <- as.numeric(nonSP_2016$CNA.Adjusted[1]) 
p1 <- as.numeric(nonSP_2016$CNA.Adjusted[nrow(nonSP_2016)]) 
graza_CNA2 <- ((p1 - p0) / p0) * 100


# NBIX

p0 <- as.numeric(nonSP_2016$NBIX.Adjusted[1]) 
p1 <- as.numeric(nonSP_2016$NBIX.Adjusted[nrow(nonSP_2016)]) 
graza_NBIX2 <- ((p1 - p0) / p0) * 100

# CAR

p0 <- as.numeric(nonSP_2016$CAR.Adjusted[1]) 
p1 <- as.numeric(nonSP_2016$CAR.Adjusted[nrow(nonSP_2016)]) 
graza_CAR2 <- ((p1 - p0) / p0) * 100

# TTC

p0 <- as.numeric(nonSP_2016$TTC.Adjusted[1]) 
p1 <- as.numeric(nonSP_2016$TTC.Adjusted[nrow(nonSP_2016)]) 
graza_TTC2 <- ((p1 - p0) / p0) * 100

# AFG

p0 <- as.numeric(nonSP_2016$AFG.Adjusted[1]) 
p1 <- as.numeric(nonSP_2016$AFG.Adjusted[nrow(nonSP_2016)]) 
graza_AFG2 <- ((p1 - p0) / p0) * 100

# WOLF

p0 <- as.numeric(nonSP_2016$WOLF.Adjusted[1]) 
p1 <- as.numeric(nonSP_2016$WOLF.Adjusted[nrow(nonSP_2016)]) 
graza_WOLF2 <- ((p1 - p0) / p0) * 100

# WTRG

p0 <- as.numeric(nonSP_2016$WTRG.Adjusted[1]) 
p1 <- as.numeric(nonSP_2016$WTRG.Adjusted[nrow(nonSP_2016)]) 
graza_WTRG2 <- ((p1 - p0) / p0) * 100

# ELS

p0 <- as.numeric(nonSP_2016$ELS.Adjusted[1]) 
p1 <- as.numeric(nonSP_2016$ELS.Adjusted[nrow(nonSP_2016)]) 
graza_ELS2 <- ((p1 - p0) / p0) * 100

# Y

p0 <- as.numeric(nonSP_2016$Y.Adjusted[1]) 
p1 <- as.numeric(nonSP_2016$Y.Adjusted[nrow(nonSP_2016)]) 
graza_Y2 <- ((p1 - p0) / p0) * 100

# GGG

p0 <- as.numeric(nonSP_2016$GGG.Adjusted[1]) 
p1 <- as.numeric(nonSP_2016$GGG.Adjusted[nrow(nonSP_2016)]) 
graza_GGG2 <- ((p1 - p0) / p0) * 100

# UHAL

p0 <- as.numeric(nonSP_2016$UHAL.Adjusted[1]) 
p1 <- as.numeric(nonSP_2016$UHAL.Adjusted[nrow(nonSP_2016)]) 
graza_UHAL2 <- ((p1 - p0) / p0) * 100

# UTHR

p0 <- as.numeric(nonSP_2016$UTHR.Adjusted[1]) 
p1 <- as.numeric(nonSP_2016$UTHR.Adjusted[nrow(nonSP_2016)]) 
graza_UTHR2 <- ((p1 - p0) / p0) * 100

# RS

p0 <- as.numeric(nonSP_2016$RS.Adjusted[1]) 
p1 <- as.numeric(nonSP_2016$RS.Adjusted[nrow(nonSP_2016)]) 
graza_RS2 <- ((p1 - p0) / p0) * 100

# DAR

p0 <- as.numeric(nonSP_2016$DAR.Adjusted[1]) 
p1 <- as.numeric(nonSP_2016$DAR.Adjusted[nrow(nonSP_2016)]) 
graza_DAR2 <- ((p1 - p0) / p0) * 100

# RPM

p0 <- as.numeric(nonSP_2016$RPM.Adjusted[1]) 
p1 <- as.numeric(nonSP_2016$RPM.Adjusted[nrow(nonSP_2016)]) 
graza_RPM2 <- ((p1 - p0) / p0) * 100

# WSO

p0 <- as.numeric(nonSP_2016$WSO.Adjusted[1]) 
p1 <- as.numeric(nonSP_2016$WSO.Adjusted[nrow(nonSP_2016)]) 
graza_WSO2 <- ((p1 - p0) / p0) * 100

# FICO

p0 <- as.numeric(nonSP_2016$FICO.Adjusted[1]) 
p1 <- as.numeric(nonSP_2016$FICO.Adjusted[nrow(nonSP_2016)]) 
graza_FICO2 <- ((p1 - p0) / p0) * 100

# RGEN

p0 <- as.numeric(nonSP_2016$RGEN.Adjusted[1]) 
p1 <- as.numeric(nonSP_2016$RGEN.Adjusted[nrow(nonSP_2016)]) 
graza_RGEN2 <- ((p1 - p0) / p0) * 100

# AZPN

p0 <- as.numeric(nonSP_2016$AZPN.Adjusted[1]) 
p1 <- as.numeric(nonSP_2016$AZPN.Adjusted[nrow(nonSP_2016)]) 
graza_AZPN2 <- ((p1 - p0) / p0) * 100

# CSL

p0 <- as.numeric(nonSP_2016$CSL.Adjusted[1]) 
p1 <- as.numeric(nonSP_2016$CSL.Adjusted[nrow(nonSP_2016)]) 
graza_CSL2 <- ((p1 - p0) / p0) * 100

# AGCO

p0 <- as.numeric(nonSP_2016$AGCO.Adjusted[1]) 
p1 <- as.numeric(nonSP_2016$AGCO.Adjusted[nrow(nonSP_2016)]) 
graza_AGCO2 <- ((p1 - p0) / p0) * 100

# EWBC

p0 <- as.numeric(nonSP_2016$EWBC.Adjusted[1]) 
p1 <- as.numeric(nonSP_2016$EWBC.Adjusted[nrow(nonSP_2016)]) 
graza_EWBC2 <- ((p1 - p0) / p0) * 100

# SRPT

p0 <- as.numeric(nonSP_2016$SRPT.Adjusted[1]) 
p1 <- as.numeric(nonSP_2016$SRPT.Adjusted[nrow(nonSP_2016)]) 
graza_SRPT2 <- ((p1 - p0) / p0) * 100

# UNM

p0 <- as.numeric(nonSP_2016$UNM.Adjusted[1]) 
p1 <- as.numeric(nonSP_2016$UNM.Adjusted[nrow(nonSP_2016)]) 
graza_UNM2 <- ((p1 - p0) / p0) * 100

# LSI

p0 <- as.numeric(nonSP_2016$LSI.Adjusted[1]) 
p1 <- as.numeric(nonSP_2016$LSI.Adjusted[nrow(nonSP_2016)]) 
graza_LSI2 <- ((p1 - p0) / p0) * 100

# NOV

p0 <- as.numeric(nonSP_2016$NOV.Adjusted[1]) 
p1 <- as.numeric(nonSP_2016$NOV.Adjusted[nrow(nonSP_2016)]) 
graza_NOV2 <- ((p1 - p0) / p0) * 100

# RRX

p0 <- as.numeric(nonSP_2016$RRX.Adjusted[1]) 
p1 <- as.numeric(nonSP_2016$RRX.Adjusted[nrow(nonSP_2016)]) 
graza_RRX2 <- ((p1 - p0) / p0) * 100

# FLEX

p0 <- as.numeric(nonSP_2016$FLEX.Adjusted[1]) 
p1 <- as.numeric(nonSP_2016$FLEX.Adjusted[nrow(nonSP_2016)]) 
graza_FLEX2 <- ((p1 - p0) / p0) * 100

# JBL

p0 <- as.numeric(nonSP_2016$JBL.Adjusted[1]) 
p1 <- as.numeric(nonSP_2016$JBL.Adjusted[nrow(nonSP_2016)]) 
graza_JBL2 <- ((p1 - p0) / p0) * 100

# MTN

p0 <- as.numeric(nonSP_2016$MTN.Adjusted[1]) 
p1 <- as.numeric(nonSP_2016$MTN.Adjusted[nrow(nonSP_2016)]) 
graza_MTN2 <- ((p1 - p0) / p0) * 100



#####################################
#####################################

# P.S. nuo "HCA" visos likusios akcijos nera itrauktos i SP500!

graza_14_16 <- data.frame(simbolis = c("CHD", "ILMN", "VRSK", "NWS", "UAL", "ATVI", "AAP",
                                                 "JBHT", "UDR", "O", "AAL", "EQIX", "HSIC", "SWKS", "HCA",
                                       "CNA", "NBIX", "CAR", "TTC", "AFG", "WOLF", "WTRG", "ELS", "Y",
                                       "GGG", "UHAL", "UTHR", "RS", "DAR", "RPM", "WSO", "FICO", "RGEN",
                                       "AZPN", "CSL", "AGCO", "EWBC", "SRPT", "UNM", "LSI", "NOV",
                                       "RRX", "FLEX", "JBL", "MTN"), 
                                    
                                    graza_2014 = c(graza_CHD, graza_ILMN, graza_VRSK, graza_NWS, graza_UAL,
                                                   graza_ATVI, graza_AAP, graza_JBHT, graza_UDR, graza_O,
                                                   graza_AAL, graza_EQIX, graza_HSIC, graza_SWKS, graza_HCA,
                                                   graza_CNA, graza_NBIX, graza_CAR, graza_TTC, graza_AFG,
                                                   graza_WOLF, graza_WTRG, graza_ELS, graza_Y, graza_GGG,
                                                   graza_UHAL, graza_UTHR, graza_RS, graza_DAR, graza_RPM,
                                                   graza_WSO, graza_FICO, graza_RGEN, graza_AZPN, graza_CSL,
                                                   graza_AGCO, graza_EWBC, graza_SRPT, graza_UNM, graza_LSI,
                                                   graza_NOV, graza_RRX, graza_FLEX, graza_JBL, graza_MTN),
                                    
                                    graza_2016 = c(graza_CHD2, graza_ILMN2, graza_VRSK2, graza_NWS2, graza_UAL2,
                                                   graza_ATVI2, graza_AAP2, graza_JBHT2, graza_UDR2, graza_O2,
                                                   graza_AAL2, graza_EQIX2, graza_HSIC2, graza_SWKS2, graza_HCA2,
                                                   graza_CNA2, graza_NBIX2, graza_CAR2, graza_TTC2, graza_AFG2,
                                                   graza_WOLF2, graza_WTRG2, graza_ELS2, graza_Y2, graza_GGG2,
                                                   graza_UHAL2, graza_UTHR2, graza_RS2, graza_DAR2, graza_RPM2,
                                                   graza_WSO2, graza_FICO2, graza_RGEN2, graza_AZPN2, graza_CSL2,
                                                   graza_AGCO2, graza_EWBC2, graza_SRPT2, graza_UNM2, graza_LSI2,
                                                   graza_NOV2, graza_RRX2, graza_FLEX2, graza_JBL2, graza_MTN2),
                          
                          ar_itraukta = c(rep(1, 15), rep(0, 30)))


# MODELIO SUDARYMAS

# Keiciami duomenys is 'wide' formato i 'long':


graza_14_16_long <- 
  graza_14_16 %>% # 'wide' formato duomenys
  tidyr::pivot_longer(cols = c(graza_2014, graza_2016), # abejose info apie grąžas dviem laiko momentais
                      names_to = "period", # paimami prieš tai aprašyti pavadinimai ir įrašomi į kintamąjį 'period'
                      values_to = "graza") %>% # grąžų reikšmės sudedamos į kintamąjį 'graza'
  dplyr::mutate(arPeriodasPo_itraukimo = ifelse(period == "graza_2016", 1, 0)) 
# sukuriamas dummy kintamasis kintamajam 'period'

head(graza_14_16_long, 10)


# Grafikas, kuri galima butu panaudoti:


graza_14_16_long %>% 
  dplyr::mutate(period = ifelse(period == "graza_2016", "T1 - Po įtraukimo", "T0 - Prieš įtraukimą"),
                ar_itraukta = ifelse(ar_itraukta == 1, "Įtraukta (D = 1)", "Neįtraukta (D = 0)")) %>%
  dplyr::group_by(period, ar_itraukta) %>% # grupuojama, kad ištraukti grupių vidurkius kiekvienu laiko momentu
  dplyr::mutate(grupes_vid = mean(graza)) %>%  # ištraukiami grupių vidurkiai
  ggplot(., aes(x = graza, fill = factor(ar_itraukta))) +
  geom_density(alpha = 0.5) +
  scale_fill_manual(name = " ", 
                    values = c("#cc0055", "#a7a8aa"),
                    labels = c("Įtraukta", "Neįtraukta")) +
  facet_grid(ar_itraukta~period) + 
  geom_vline(aes(xintercept = grupes_vid), linetype = "longdash") + # pridedama vertikali linija su vidurkiu
  theme_bw() +
  theme(legend.position = "none") +
  labs(x = "Grąža, %",
       y = "Tankis")

# Modelio sudarinejimas neatsizvelgiant i laika:

after_model <- lm(graza_2016 ~ ar_itraukta, data = graza_14_16)
stargazer(after_model, type = "text")

# Rezultatu interpretacija: tu imoniu, kurios buvo itrauktos i SP500 akciju indeksa, graza vidutiniskai
# 10.1% buvo mazesne nei tu, kurios i indeksa nebuvo itrauktos.

# Atsizvelgiant i laika:

graza_14_16 <- graza_14_16 %>%
  dplyr::mutate(skirtumas = graza_2016 - graza_2014) #simple substraction

did_model <- lm(skirtumas ~ ar_itraukta, data = graza_14_16)
stargazer(did_model, after_model, type = "text")

# Interpretacija: tos imoniu akcijos, kurios buvo itrauktos i SP500, vidutiniskai generavo 39.44% mazesne graza nei tos,
# kurios nebuvo itrauktos i pastaraji indeksa.
# graza_2016 = 0.8 + graza_2014 - 39.44(ar_itraukta) + error

# Tesiant su laiku, regresinis modelis:

did_long <- lm(graza ~ ar_itraukta + arPeriodasPo_itraukimo + ar_itraukta * arPeriodasPo_itraukimo, 
               data = graza_14_16_long) # regresinis DiD modelis

summary(did_long)

# Heteroskedastiškumo hipotezė:

lmtest::bptest(did_long)

# Autokoreliacijos testas:

print(lmtest::dwtest(did_long, alternative = "two.sided"))

# 'arPeriodasPo_itraukimo' kintamojo p-reiksme 0.92003 > 0.05. 

# Koreguojamos standartines paklaidos:

did_long_clustered_se <- coeftest(did_long, vcov = vcovHC(did_long, type = "HC0", cluster = "district")) 

# Standartinės paklaidos bei atitinkamos p-reikšmės
print(lmtest::coeftest(did_long, vcov. = did_long_clustered_se))

# Atlikus standartinių paklaidų korekciją, 'arPeriodasPo_itraukimo' kintamojo p-reikšmė ženkliai sumažėjo (0.02047 < 0.05),
# tad jį galime palikti. Likusiųjų kintamųjų p-reikšmės taip pat gerokai sumažėjo, tačiau jų atitinkamos p-reikšmės ir taip
# buvo mažesnės nei 0.05 prieš atliekant HCE testą. Išvada: visi kintamieji yra stat. reikšmingi.

stargazer::stargazer(did_long_clustered_se, type = "text")


# Dar vienas grafikas:

graza_14_16_long %>%
  dplyr::group_by(period, ar_itraukta) %>% 
  dplyr::mutate(grupes_vid = mean(graza)) %>%
  ggplot(aes(x = arPeriodasPo_itraukimo, y = grupes_vid, color = factor(ar_itraukta))) +
  geom_point() +
  geom_line(aes(x = arPeriodasPo_itraukimo, y = grupes_vid)) +
  scale_x_continuous(breaks = c(0, 1)) +
  scale_color_manual(name = " ", 
                     values = c("#a7a8aa", "#cc0055"),
                     labels = c("Neįtraukta", "Įtraukta")) +
  labs(x = "Laiko periodas", y = "Vidutinė metinė grąža, %", color = "Ar įtraukta") +
  theme_minimal() 

# Laiko periodas: 0 ir 1 indikuoja periodo pradžią ir pabaigą, atitinkamai. Tai reiškia, jog 0 yra tas momentas, kai akcija
# nebuvo įtraukta į indeksą, 1 - po įtraukimo. Šiuo atveju matome, jog periodo pabaigoje, t.y., kai T = 1, įtrauktos akcijos
# metinė grąža yra ganėtinai žemiau nei, kad tos, kuri per tą patį laiko tarpą nebuvo įtraukta į indeksą. 