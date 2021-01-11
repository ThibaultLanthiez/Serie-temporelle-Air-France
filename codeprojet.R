readLines("valeurs_mensuelles.csv",n=10)

#Importation des donn?es
Airport = read.table("valeurs_mensuelles.csv",header=FALSE,sep=";",quote="\"",skip=3,stringsAsFactors=FALSE)
head(Airport)

#Cr?ation de la s?rie chronologique
date = paste(rev(Airport$V1),"-01",sep="")
date = as.Date(date,format="%Y-%m-%d")
data = rev(Airport$V2)

install.packages("zoo")
library(zoo)
Airport = zoo(data,order.by=date)

save(Airport,file="Airport.RData")
load("Airport.RData")

# Extraction des 15 derni?res ann?es compl?tes
Airport2003 = window(Airport,start=as.Date("2003-03-01")) 

# Visualisation de la chronique depuis f?vrier 2003

plot(Airport2004,xlab="Mars 2003 - F?vrier 2018",
     ylab="Nombre de passagers (en million)",
     main="Transports de passagers - Air France - Vols int?rieurs",
     lwd=1,lty=1,col="blue",ylim=c(0,1),xaxt="n")
date = seq(as.Date("2003-03-01"),as.Date("2018-03-01"),by="2 years")
axis(side=1,at=date,labels=format(date,"%b %Y"))
abline(v=date,col="gray50",lty=3)

#Valeur atypique : 0.488 - 09/2014

#Caract?ristiques globales
#Min. 1st Qu.  Median    Mean 3rd Qu.    Max. 
#0.4820  0.6410  0.7010  0.6936  0.7500  0.8710 


# Estimation non param?trique de la tendance (lissage)
time = as.numeric(index(Airport2003))
values = coredata(Airport2003)
smooth = loess(values~time,span=0.25,degree=1)
smooth = zoo(smooth$fitted,order.by=index(Airport2003))
plot(Airport2003,lwd=1,lty=1,col="blue",ylim=c(0,1),xaxt="n",
     main="Transports de passagers - Air France - Vols int?rieurs",
     xlab="Mars 2003 - F?vrier 2018",ylab="Nombre de passagers (en million)")
date = seq(as.Date("2003-03-01"),as.Date("2018-02-01"),by="1 years")
axis(side=1,at=date,labels=format(date,"%b %Y"))
abline(v=date,col="gray50",lty=3)
lines(smooth,col="red3",lwd=2)
legend("topleft",legend=c("S?rie brute","S?rie liss?e (span=0.25)"),
       col=c("blue","red3"),lty=1,bty="n")

#Entre 03/2003 et 02/2007 constant
#Entre 03/2007 et 02/2010 d?croissant
#Entre 03/2010 et 02/2013 croissant
#Entre 03/2013 et 02/2018 d?croissant


# D?finition des points de rupture
t1 = as.Date("2007-01-01")
t2 = as.Date("2010-01-01")
t3= as.Date("2013-07-01")
time = index(Airport2003)
Ind1 = time > t1; time1 = (time-t1)*Ind1
Ind2 = time > t2; time2 = (time-t2)*Ind2
Ind3 = time > t3; time3 = (time-t3)*Ind3

# Mod?lisation de la courbe de lisage
values = coredata(smooth)
model1 = lm(values~time+time1+time2+time3)
summary(model1)
trend = zoo(model1$fitted,order.by=index(Airport2003))

# Comparaison lissage et mod?le de r?gression par morceaux
plot(Airport2003,lwd=1,lty=1,col="blue",ylim=c(0,1),xaxt="n",type="n",
     xlab="Mars 2003 - F?vrier 2018",ylab="Nombre de passagers (en million)",
     main="Transports de passagers - Air France - Vols int?rieurs")
date = seq(as.Date("2003-03-01"),as.Date("2018-02-01"),by="1 year")
axis(side=1,at=date,labels=format(date,"%b %Y"))
abline(v=date,col="gray50",lty=3)
lines(smooth,col="green4",lwd=2)
lines(trend,col="red3",lwd=2)
legend("topleft",legend=c("Lissage","Mod?le lin?aire par morceaux"),
       col=c("green4","red3"),lty=1,bty="n")

# Le mod?le lin?aire par morceaux s'adapte ? la perfection ? la courbe de lissage

# Estimation de la composante saisonni?re
season2003 = Airport2003 - trend
plot(season2003,lwd=1,lty=1,col="blue",xaxt="n",
     main="Visualisation des variations saisonni?res",
     xlab="mars 2003 - F?vrier 2018",ylab="Nombre de passagers (en millon)")
date = seq(as.Date("2003-03-01"),as.Date("2018-02-01"),by="1 years")
axis(side=1,at=date,labels=format(date,"%b %Y"))
abline(v=date,col="gray50",lty=3)
abline(h=0,lty=1)

# Visualisation des variations saisonni?res
StatByMonth = aggregate(season2003,by=format(index(Airport2003),"%m"),FUN=summary)
print(StatByMonth)
install.packages("lubridate")
library(lubridate) # Utilisation de la fonction month()
month = format(seq(as.Date("2016-01-01"),as.Date("2016-12-01"),by="month"),"%b")
boxplot(coredata(season2003) ~ month(season2003),col="orange",names=month,
        xlab="Mars 2003 - F?vrier 2018",ylab="Nombre de passagers",
        main="Comportement saisonnier du transports de passagers - Air France - Vols int?rieurs")
points(1:12,StatByMonth$Mean,type="b",col="green4",lwd=2)
abline(h=0,lty=3)

# Les mois favorables au transport de passagers sont juin et juillet
# Les mois d?favorables au transport de passagers sont janvier et f?vrier

# Estimation des coefficients saisonniers
datamonth = rep(month,15)
datamonth = factor(datamonth,levels=month)
model2 = lm(coredata(season2003)~-1+datamonth)
summary(model2)

# Coefficients saisonniers d?finitifs
coefdef = model2$coef - mean(model2$coef)
coefdef

# Visualisation de l'ajustement saisonnier
season = zoo(rep(coefdef,13),order.by=index(Airport2004))
plot(season2003,lwd=1,lty=1,col="blue",xaxt="n",
     main="Transports de passagers - Air France - Vols intérieurs",
     xlab="Mars 2003 - F?vrier 2018",ylab="Nombre de passagers (en million)")
date = seq(as.Date("2003-03-01"),as.Date("2018-02-01"),by="2 years")
axis(side=1,at=date,labels=format(date,"%b %Y"))
abline(v=date,col="gray50",lty=3)
abline(h=0)
lines(season,col="red3",lwd=2,lty=4)

# S?rie ajust?e (en prenant en compte les variations saisonni?res)
adjusted = trend + season
plot(Airport2003,lwd=1,lty=1,col="blue",ylim=c(0,1),xaxt="n",
     main="Transports de passagers - Air France - Vols int?rieurs",
     xlab="Mars 2003 - F?vrier 2018",ylab="Nombre de passagers (en millon)")
date = seq(as.Date("2003-03-01"),as.Date("2018-02-01"),by="2 years")
axis(side=1,at=date,labels=format(date,"%b %Y"))
abline(v=date,col="gray50",lty=3)
lines(adjusted,col="red3",lwd=2)
legend("topleft",legend=c("S?rie brute","S?rie ajust?e"),
       col=c("blue","red3"),lty=1,bty="n")

# S?rie des r?sidus et des r?sidus standardis?s
residual = Airport2003 - adjusted
residualstand = residual/sd(residual)
plot(residualstand,lwd=1.5,lty=1,col="blue",ylim=c(-6,+4),xaxt="n",
     main="Transports de passagers - Air France - Vols int?rieurs",
     xlab="Mars 2003 - F?vrier 2018",ylab="r?sidu standardis?")
date = seq(as.Date("2003-03-01"),as.Date("2018-02-01"),by="2 years")
axis(side=1,at=date,labels=format(date,"%b %Y"))
abline(v=date,col="gray50",lty=3)
abline(h=0,lty=2)
abline(h=c(-2,+2),col="red3",lty=3)

#Manque histogramme densit? des r?sidus normalis?s

# Fonction d'autocorr?lation
acf(as.numeric(residualstand),lag.max=48,type="correlation",col="red4",lwd=2,
    main="Fonction d'autocorr?lation",xlab="Horizon (Max = 4 ans)")

#Mauvaise, on voit encore la saisonnalit?

# Pr?paration pour la pr?vision
time = seq(as.Date("2018-02-01"),as.Date("2020-02-01"),by="months")
t1 = as.Date("2007-01-01")
t2 = as.Date("2010-01-01")
t3= as.Date("2013-07-01")
Ind1 = time > t1 ; time1 = (time-t1)*Ind1
Ind2 = time > t2 ; time2 = (time-t2)*Ind2
Ind3 = time > t3; time3 = (time-t3)*Ind3
df=data.frame(time,time1,time2,time3)

# Pr?paration pour la pr?vision
predict.trend = predict(model1,newdata=df)
predict.trend = zoo(predict.trend,order.by=time)
predict = predict.trend + rep(coefdef,4)
print(round(predict,0))

# Préparation pour la prévision
plot(Airport2004,lwd=1,lty=1,col="blue",ylim=c(0,1),
     main="Fréquentation des passagers - Aéroports de Paris",                                
     xlab="Mars 2003 - Février 2018",ylab="Nombre de passagers (en milliers)",
     xlim=c(as.Date("2003-03-01"),as.Date("2020-02-01")),xaxt="n")
date = seq(as.Date("2003-03-01"),as.Date("2020-02-01"),by="2 years")
axis(side=1,at=date,labels=format(date,"%b %Y"))
abline(v=date,col="gray50",lty=3)
lines(predict,col="red3",lwd=2,lty=3)
legend("topleft",legend=c("Données","Prévision"),col=c("blue","red3"),lty=c(1,3),bty="n")
