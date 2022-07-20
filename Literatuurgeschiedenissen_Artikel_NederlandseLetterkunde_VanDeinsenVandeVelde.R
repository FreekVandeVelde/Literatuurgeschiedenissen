#########################################
# Text Mining Literatuurgeschiedenissen #
#########################################

library(stringr)
library(dplyr)
library(reshape2)
library(readtext)
library(rstatix)
library(nlme)
library(MuMIn)
library(effects)
library(glmnet)

options(scipen = 10)
rm(list = ls())
set.seed(19790427)

#dataset inlezen-----

#Laad de R workspace van https://github.com/FreekVandeVelde/Literatuurgeschiedenissen/blob/main/Literatuurgeschiedenissen_Artikel_NederlandseLetterkunde_VanDeinsenVandeVelde.RData met de functie load()#

#Modelbouw#-----

#Exploratief-----

head(df.red)
mean(table(df.red$SentenceID)) #gemiddeld aantal lemma's per zin#
sort(table(df.red$Lemma), decreasing=TRUE) 
length(table(df.red$Lemma))
plot(sort(log10(table(df.red$Lemma)),decreasing=TRUE)[1:100], las=3, ylab="frequentie (log 10)", cex.axis=0.75) #Frequentie van de top-100 lemma's"

levels(df.red$Auteur)
mean(droplevels(filter(df.red, Auteur == "Joost_van_den_Vondel"))$PowerMean)
mean(droplevels(filter(df.red, Auteur == "Anna_Bijns"))$PowerMean)
with(droplevels(filter(df.red, Auteur %in% c("Joost_van_den_Vondel","Anna_Bijns"))),wilcox.test(PowerMean ~ Auteur))

#Mixed models-----

str(df.red)
table(df.red$Geslacht)

VJ.glmm <- nlme::lme(ValenceMean ~ Geslacht * poly(Jaartal.c,2), random=list(~1|AuteurSentenceID, ~1|Auteur, ~1|AuteurRed), data=df.red, na.action = na.omit)
summary(VJ.glmm)
plot(allEffects(VJ.glmm), lines=list(multiline=TRUE, lty=1:2), colors="black", confint=list(style="band"), axes=list(grid=TRUE, x=list(rug=TRUE, rotate=0)), xlab="Jaartal (gecentreerd)", ylab="Valentie", main="")

#modelvergelijking#
VJ.glmm.0 <- nlme::lme(ValenceMean ~ Jaartal.c, random=list(~1|AuteurSentenceID, ~1|Auteur, ~1|AuteurRed), data=df.red, na.action = na.omit)
summary(VJ.glmm.0)
VJ.glmm.1 <- nlme::lme(ValenceMean ~ poly(Jaartal.c,2), random=list(~1|AuteurSentenceID, ~1|Auteur, ~1|AuteurRed), data=df.red, na.action = na.omit)
summary(VJ.glmm.1)
VJ.glmm.2 <- nlme::lme(ValenceMean ~ Geslacht, random=list(~1|AuteurSentenceID, ~1|Auteur, ~1|AuteurRed), data=df.red, na.action = na.omit)
summary(VJ.glmm.2)
VJ.glmm.3 <- nlme::lme(ValenceMean ~ Geslacht * Jaartal.c, random=list(~1|AuteurSentenceID, ~1|Auteur, ~1|AuteurRed), data=df.red, na.action = na.omit)
summary(VJ.glmm.3)
VJ.glmm.4 <- nlme::lme(ValenceMean ~ Geslacht * poly(Jaartal.c,3), random=list(~1|AuteurSentenceID, ~1|Auteur, ~1|AuteurRed), data=df.red, na.action = na.omit)
summary(VJ.glmm.4)

AJ.glmm <- nlme::lme(ArousalMean ~ Geslacht * poly(Jaartal.c,2), random=list(~1|AuteurSentenceID, ~1|Auteur, ~1|AuteurRed), data=df.red, na.action = na.omit)
summary(AJ.glmm)
plot(allEffects(AJ.glmm), lines=list(multiline=TRUE, lty=1:2), colors="black", confint=list(style="band"), axes=list(grid=TRUE, x=list(rug=TRUE, rotate=0)), xlab="Jaartal (gecentreerd)", ylab="Opwinding", main="")

#modelvergelijking#
AJ.glmm.0 <- nlme::lme(ArousalMean ~ Jaartal.c, random=list(~1|AuteurSentenceID, ~1|Auteur, ~1|AuteurRed), data=df.red, na.action = na.omit)
summary(AJ.glmm.0)
AJ.glmm.1 <- nlme::lme(ArousalMean ~ poly(Jaartal.c,2), random=list(~1|AuteurSentenceID, ~1|Auteur, ~1|AuteurRed), data=df.red, na.action = na.omit)
summary(AJ.glmm.1)
AJ.glmm.2 <- nlme::lme(ArousalMean ~ Geslacht, random=list(~1|AuteurSentenceID, ~1|Auteur, ~1|AuteurRed), data=df.red, na.action = na.omit)
summary(AJ.glmm.2)
AJ.glmm.3 <- nlme::lme(ArousalMean ~ Geslacht * Jaartal.c, random=list(~1|AuteurSentenceID, ~1|Auteur, ~1|AuteurRed), data=df.red, na.action = na.omit)
summary(AJ.glmm.3)
AJ.glmm.4 <- nlme::lme(ArousalMean ~ Geslacht * poly(Jaartal.c,3), random=list(~1|AuteurSentenceID, ~1|Auteur, ~1|AuteurRed), data=df.red, na.action = na.omit)
summary(AJ.glmm.4)

PJ.glmm <- nlme::lme(PowerMean ~ Geslacht * poly(Jaartal.c,2), random=list(~1|AuteurSentenceID, ~1|Auteur, ~1|AuteurRed), data=df.red, na.action = na.omit)
summary(PJ.glmm)
plot(allEffects(PJ.glmm), lines=list(multiline=TRUE, lty=1:2), colors="black", confint=list(style="band"), axes=list(grid=TRUE, x=list(rug=TRUE, rotate=0)), xlab="Jaartal (gecentreerd)", ylab="Dominantie", main="")

#modelvergelijking#
PJ.glmm.0 <- nlme::lme(PowerMean ~ Jaartal.c, random=list(~1|AuteurSentenceID, ~1|Auteur, ~1|AuteurRed), data=df.red, na.action = na.omit)
summary(PJ.glmm.0)
PJ.glmm.1 <- nlme::lme(PowerMean ~ poly(Jaartal.c,2), random=list(~1|AuteurSentenceID, ~1|Auteur, ~1|AuteurRed), data=df.red, na.action = na.omit)
summary(PJ.glmm.1)
PJ.glmm.2 <- nlme::lme(PowerMean ~ Geslacht, random=list(~1|AuteurSentenceID, ~1|Auteur, ~1|AuteurRed), data=df.red, na.action = na.omit)
summary(PJ.glmm.2)
PJ.glmm.3 <- nlme::lme(PowerMean ~ Geslacht * Jaartal.c, random=list(~1|AuteurSentenceID, ~1|Auteur, ~1|AuteurRed), data=df.red, na.action = na.omit)
summary(PJ.glmm.3)
PJ.glmm.4 <- nlme::lme(PowerMean ~ Geslacht * poly(Jaartal.c,3), random=list(~1|AuteurSentenceID, ~1|Auteur, ~1|AuteurRed), data=df.red, na.action = na.omit)
summary(PJ.glmm.4)

V_ALG.glmm <- nlme::lme(ValenceMean ~ Geslacht * Reeks, random=list(~1|AuteurSentenceID, ~1|Auteur, ~1|AuteurRed), data=df.red, na.action = na.omit)
summary(V_ALG.glmm)
plot(allEffects(V_ALG.glmm), lines=list(multiline=TRUE, lty=1:2), colors="black", confint=list(style="bars"), axes=list(grid=TRUE, x=list(rug=TRUE, rotate=0)), xlab="Reeks", ylab="Valentie", main="")

#modelvergelijking#
V_ALG.glmm.0 <- nlme::lme(ValenceMean ~ Reeks, random=list(~1|AuteurSentenceID, ~1|Auteur, ~1|AuteurRed), data=df.red, na.action = na.omit)
summary(V_ALG.glmm.0)
V_ALG.glmm.1 <- nlme::lme(ValenceMean ~ Geslacht, random=list(~1|AuteurSentenceID, ~1|Auteur, ~1|AuteurRed), data=df.red, na.action = na.omit)
summary(V_ALG.glmm.1)

A_ALG.glmm <- nlme::lme(ArousalMean ~ Geslacht * Reeks, random=list(~1|AuteurSentenceID, ~1|Auteur, ~1|AuteurRed), data=df.red, na.action = na.omit)
summary(A_ALG.glmm)
plot(allEffects(A_ALG.glmm), lines=list(multiline=TRUE, lty=1:2), colors="black", confint=list(style="bars"), axes=list(grid=TRUE, x=list(rug=TRUE, rotate=0)), xlab="Reeks", ylab="Opwinding", main="")

#modelvergelijking#
A_ALG.glmm.0 <- nlme::lme(ArousalMean ~ Reeks, random=list(~1|AuteurSentenceID, ~1|Auteur, ~1|AuteurRed), data=df.red, na.action = na.omit)
summary(A_ALG.glmm.0)
A_ALG.glmm.1 <- nlme::lme(ArousalMean ~ Geslacht, random=list(~1|AuteurSentenceID, ~1|Auteur, ~1|AuteurRed), data=df.red, na.action = na.omit)
summary(A_ALG.glmm.1)

P_ALG.glmm <- nlme::lme(PowerMean ~ Geslacht * Reeks, random=list(~1|AuteurSentenceID, ~1|Auteur, ~1|AuteurRed), data=df.red, na.action = na.omit)
summary(P_ALG.glmm)
plot(allEffects(P_ALG.glmm), lines=list(multiline=TRUE, lty=1:2), colors="black", confint=list(style="bars"), axes=list(grid=TRUE, x=list(rug=TRUE, rotate=0)), xlab="Reeks", ylab="Dominantie", main="")

#modelvergelijking#
P_ALG.glmm.0 <- nlme::lme(PowerMean ~ Reeks, random=list(~1|AuteurSentenceID, ~1|Auteur, ~1|AuteurRed), data=df.red, na.action = na.omit)
summary(P_ALG.glmm.0)
P_ALG.glmm.1 <- nlme::lme(PowerMean ~ Geslacht, random=list(~1|AuteurSentenceID, ~1|Auteur, ~1|AuteurRed), data=df.red, na.action = na.omit)
summary(P_ALG.glmm.1)

#Verklaarde variantie#
r.squaredGLMM(VJ.glmm)
r.squaredGLMM(AJ.glmm)
r.squaredGLMM(PJ.glmm)
r.squaredGLMM(V_ALG.glmm)
r.squaredGLMM(A_ALG.glmm)
r.squaredGLMM(P_ALG.glmm)

#Lasso-----

#Lexicale effecten

str(df.red)

df_wide <- dcast(df.red, AuteurSentenceID + Geslacht + JaartalLGred + PoS ~ Lemma)
str(df_wide)

table(df_wide$Geslacht)

x.matrix <- data.matrix(df_wide[,-c(1:2)])

lasso.model <- cv.glmnet(x.matrix, as.numeric(df_wide$Geslacht)-1, alpha=1, type.measure="deviance", standardize=TRUE, nfold=10, family="binomial")
plot(lasso.model, las=1)
lasso.model$lambda.min
head(coef(lasso.model, lasso.model$lambda.min))

head(Lasso.coef <- coef(lasso.model, s='lambda.min', exact=TRUE)[,1][-c(1:3)])
Lemma <- row.names(coef(lasso.model))[-c(1:3)]
lasso.df <- data.frame(Lemma, Lasso.coef)          
lasso.df$Lemma <- as.factor(lasso.df$Lemma)

head(lasso.df)
lemma_freq <- as.data.frame(table(df.red$Lemma))
lemma_freq <- rename(lemma_freq, "Lemma" = Var1)
head(lemma_freq)
lasso.df <- left_join(lasso.df, lemma_freq)
lasso.df$Binair <- ifelse(lasso.df$Lasso.coef == 0, 0, 1)
cor.test(lasso.df$Lasso.coef, lasso.df$Freq)

lasso.df$color <- ifelse(lasso.df$Lasso.coef < 0, "cyan", ifelse(lasso.df$Lasso.coef > 0, "magenta", "black"))
lasso.df$symbol <- ifelse(lasso.df$Lasso.coef < 0, 0, ifelse(lasso.df$Lasso.coef > 0, 1, 17))
par(mar=c(5.1, 5.1, 4.1, 2.1), mgp=c(3.7,1,0))
plot(lasso.df$Lasso.coef, lasso.df$Freq, las=1, xlab="Lasso coëfficiënt", ylab="Lemmafrequentie", pch=lasso.df$symbol)
dev.off()

#END#-----
