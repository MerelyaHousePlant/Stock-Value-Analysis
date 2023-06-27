setwd("C:/Users/Asus/Desktop/Proiecte Personale R/Micro Manageriala")
date_actiuni <- read.csv("date_proiect.csv")
summary(date_actiuni)


CalculRentabilitati <- function(pret_actiuni)
{
  vector_rentabilitati<-c()
  perioada_curenta <- c()
  perioada_baza <- 0 
  for(i in pret_actiuni)
  {
    perioada_curenta <- i
    
    if(perioada_baza == 0 || is.null(perioada_baza))
    {
      perioada_baza = i
    }
    
    else
    {
      vector_rentabilitati <- c(vector_rentabilitati,((perioada_curenta / perioada_baza) - 1))
      perioada_baza = i
    }
  }
  return(vector_rentabilitati)
}

test <- c(5:10)
CalculRentabilitati(test)
date_rentabilitati <- data.frame()
date_rentabilitati <- data.frame(date_actiuni$Date[-1], 
                        round(CalculRentabilitati(date_actiuni$Pret_Activision),6),
                        round(CalculRentabilitati(date_actiuni$Pret_EA),6),
                        round(CalculRentabilitati(date_actiuni$Pret_TTWO),6), 
                        round(CalculRentabilitati(date_actiuni$Pret_SP500),6))
View(date_rentabilitati)
colnames(date_rentabilitati) <- c("Date", "Rentabilitati_Activision", "Rentabilitati_EA", "Rentabilitati_TTWO","Rentabilitati_SP500")
#sd, CV, skewness, kurtosis
library(moments)
sd_Activision <- round(sd(date_actiuni$Pret_Activision),6)
sd_EA <- round(sd(date_actiuni$Pret_EA),6)
sd_TTWO <- round(sd(date_actiuni$Pret_TTWO),6)
sd_SP500 <- round(sd(date_actiuni$Pret_SP500),6)
#library(pryr)
#mem_used()
sk_Activision <- skewness(date_actiuni$Pret_Activision)
sk_EA <- skewness(date_actiuni$Pret_EA)
sk_TTWO <- skewness(date_actiuni$Pret_TTWO)
sk_SP500 <- skewness(date_actiuni$Pret_SP500)

kurt_Activision <- kurtosis(date_actiuni$Pret_Activision)
kurt_EA <- kurtosis(date_actiuni$Pret_EA)
kurt_TTWO <- kurtosis(date_actiuni$Pret_TTWO)
kurt_SP500 <- kurtosis(date_actiuni$Pret_SP500)

cv_Activision <- sd_Activision / mean(date_actiuni$Pret_Activision)
cv_EA <- sd_EA / mean(date_actiuni$Pret_EA)
cv_TTWO <- sd_TTWO / mean(date_actiuni$Pret_TTWO)
cv_SP500 <- sd_SP500 / mean(date_actiuni$Pret_SP500)

#matrice valori
statistici_actiuni <- c()
statistici_actiuni <- cbind(
                      rbind(sd_Activision,sd_EA,sd_TTWO,sd_SP500),
                      rbind(sk_Activision,sk_EA,sk_TTWO,sk_SP500),
                      rbind(kurt_Activision,kurt_EA,kurt_TTWO,kurt_SP500),
                      rbind(cv_Activision,cv_EA,cv_TTWO,cv_SP500))
rownames(statistici_actiuni) <- c("Activision","EA","TTWO","SP500")
colnames(statistici_actiuni) <- c("SD","SKEW","KURT","CV")
View(statistici_actiuni)
library(corrplot)
matr_corelatie <- cor(date_actiuni[-1])
windows()
corrplot(matr_corelatie, addCoef.col = "lightpink3", number.digits = 1, method="pie",
         tl.col="black",number.cex = 0.8, bg="azure", outline = "black")
boxplot(date_actiuni$Pret_SP500, main="Boxplot pret SP500", col="dodgerblue3")

par(mfrow=c(1,4))
outlier_Activision <- boxplot(date_actiuni$Pret_Activision,main="PRETUL ACTIVISION", xlab="Zile", ylab="Pret (USD)", col="red")
outlier_EA <- boxplot(date_actiuni$Pret_EA, main="PRETUL EA", xlab="Zile", ylab="Pret (USD)", col ="yellow")
outlier_TTWO <- boxplot(date_actiuni$Pret_TTWO,main="PRETUL TTWO", xlab="Zile", ylab="Pret (USD)", col = "green")
outlier_Sp500 <- boxplot(date_actiuni$Pret_SP500,main="PRETUL SP500", xlab="Zile", ylab="Pret (USD)", col = "blue")
outlier_Activision$out
date_actiuni[date_actiuni$Pret_Activision == outlier_Activision$out,]


par(mfrow=c(1,4))
hist_Activision <- hist(date_actiuni$Pret_Activision,main="PRETUL ACTIVISION", xlab="Zile", ylab="Pret (USD)", col="red")
hist_EA <- hist(date_actiuni$Pret_EA, main="PRETUL EA", xlab="Zile", ylab="Pret (USD)", col ="yellow")
hist_TTWO <- hist(date_actiuni$Pret_TTWO,main="PRETUL TTWO", xlab="Zile", ylab="Pret (USD)", col = "green")
hist_Sp500 <- hist(date_actiuni$Pret_SP500,main="PRETUL SP500", xlab="Zile", ylab="Pret (USD)", col = "blue")

#Rentabilitati

summary(date_rentabilitati)

sd_rent_Activision <- round(sd(date_rentabilitati$Rentabilitati_Activision),6)
sd_rent_EA <- round(sd(date_rentabilitati$Rentabilitati_EA),6)
sd_rent_TTWO <- round(sd(date_rentabilitati$Rentabilitati_TTWO),6)
sd_rent_SP500<- round(sd(date_rentabilitati$Rentabilitati_SP500),6)

sk_rent_Activision <- skewness(date_rentabilitati$Rentabilitati_Activision)
sk_rent_EA <- skewness(date_rentabilitati$Rentabilitati_EA)
sk_rent_TTWO <- skewness(date_rentabilitati$Rentabilitati_TTWO)
sk_rent_SP500 <- skewness(date_rentabilitati$Rentabilitati_SP500)

kurt_rent_Activision <- kurtosis(date_rentabilitati$Rentabilitati_Activision)
kurt_rent_EA <- kurtosis(date_rentabilitati$Rentabilitati_EA)
kurt_rent_TTWO <- kurtosis(date_rentabilitati$Rentabilitati_TTWO)
kurt_rent_SP500 <- kurtosis(date_rentabilitati$Rentabilitati_SP500)

cv_rent_Activision <- sd_rent_Activision / mean(date_rentabilitati$Rentabilitati_Activision)
cv_rent_EA <- sd_rent_EA / mean(date_rentabilitati$Rentabilitati_EA)
cv_rent_TTWO <- sd_rent_TTWO / mean(date_rentabilitati$Rentabilitati_TTWO)
cv_rent_SP500 <- sd_rent_SP500 / mean(date_rentabilitati$Rentabilitati_SP500)

#matrice valori
statistici_rentabilitati <- c()
statistici_rentabilitati <- cbind(
  rbind(sd_rent_Activision,sd_rent_EA,sd_rent_TTWO,sd_rent_SP500),
  rbind(sk_rent_Activision,sk_rent_EA,sk_rent_TTWO,sk_rent_SP500),
  rbind(kurt_rent_Activision,kurt_rent_EA,kurt_rent_TTWO,kurt_rent_SP500),
  rbind(cv_rent_Activision,cv_rent_EA,cv_rent_TTWO,cv_rent_SP500))
rownames(statistici_rentabilitati) <- c("Activision","EA","TTWO","SP500")
colnames(statistici_rentabilitati) <- c("SD","SKEW","KURT","CV")
View(statistici_rentabilitati)
library(corrplot)
matr_corelatie_rent <- cor(date_rentabilitati[-1])
windows()
corrplot(matr_corelatie_rent, addCoef.col = "lightpink3", number.digits = 1, method="pie",
         tl.col="black",number.cex = 0.8, bg="azure", outline = "black")

par(mfrow=c(1,4))
hist_Activision <- hist(date_rentabilitati$Rentabilitati_Activision,main="RENTABILITATI ACTIVISION", xlab="RATA PROFIT", col="red")
hist_EA <- hist(date_rentabilitati$Rentabilitati_EA,main="RENTABILITATI EA", xlab="RATA PROFIT", col ="yellow")
hist_TTWO <- hist(date_rentabilitati$Rentabilitati_TTWO,main="RENTABILITATI TTWO", xlab="RATA PROFIT",col = "green")
hist_Sp500 <- hist(date_rentabilitati$Rentabilitati_SP500,main="RENTABILITATI SP500", xlab="RATA PROFIT", col = "blue")

par(mfrow=c(1,4))
hist_Activision <- boxplot(date_rentabilitati$Rentabilitati_Activision,main="RENTABILITATI ACTIVISION", xlab="RATA PROFIT", col="red")
hist_EA <- boxplot(date_rentabilitati$Rentabilitati_EA,main="RENTABILITATI EA", xlab="RATA PROFIT", col ="yellow")
hist_TTWO <- boxplot(date_rentabilitati$Rentabilitati_TTWO,main="RENTABILITATI TTWO", xlab="RATA PROFIT",col = "green")
hist_Sp500 <- boxplot(date_rentabilitati$Rentabilitati_SP500,main="RENTABILITATI SP500", xlab="RATA PROFIT", col = "blue")

