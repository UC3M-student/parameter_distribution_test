
mortalidad = read.csv2("https://raw.githubusercontent.com/UC3M-student/Modelo-Interno/main/Mortalidad.csv", header = TRUE)

frecuencia = read.csv2("https://raw.githubusercontent.com/UC3M-student/Modelo-Interno/main/Frecuencia.csv", header = TRUE)$Frecuencia

severidad = read.csv2("https://raw.githubusercontent.com/UC3M-student/Modelo-Interno/main/Severidad.csv", header = TRUE)$Severidad


# Best Estimate -> Provision Tecnica
# Capital Economico -> Lo podemos definir como la cantidad de fondos propios necesarios para anular prácticamente la probabilidad de ruina de la Aseguradora, en el plazo de un año
# VaR
# TVaR


#### FRECUENCIA ####

landa_frecuencia = mean(frecuencia) # VALIDADO POR IOPA

landa_frecuencia

####  SEVERIDAD ####

hist(severidad)
muestra = severidad

max(muestra)

n=length(muestra)
hist(muestra)

h1=1.06*sd(muestra)/(n^(1/5))

f<-c()
punto=seq(0,50000,1)
for (j in 1:length(punto)){
  f[j]=(1/(h1*n))*sum(dnorm((punto[j]-muestra)/h1,0,1)) #dnorm((punto[j]-muestra)/h1,0,1) - >   x - xi / h (normal 0 - 1)
}

sum(f) #SIEMPRE RESULTADO 1 ; SINO COGER MAS VALORES DEL PUNTO
df<-data.frame(cbind(punto,f))
plot(df)
plot(cumsum(f),xaxt='n') 

aux<-cbind(punto,cumsum(f)) 


aux[which(round(aux[,2],5) >= 0.91),1][1] # VaR 95 % 

table(muestra)

min(muestra)

#Media , Mediana para Ajuste de Densidades

aux2 = cbind(punto, f)
esperanza = sum(aux2[,1]*aux2[,2])

#Calcular parametros a traves de ML

muestra2 = muestra[muestra < 33000]


dif<-function(param){ 
  r1<-(param[1]/param[2]-mean(muestra2))^2
  r2<-(param[1]/(param[2]^2)-var(muestra2))^2
  return(r1+r2)
}

MM<-optim(c(mean(muestra2),var(muestra2)),dif,method="L-BFGS-B")$par

mean(muestra2)

var(muestra2)

MM

muestra

param<-c()

LL <- function(param) {
  -sum(dgamma(muestra2, param[1], param[2], log=TRUE)) #Cambiar dnorm por distribucion deseada + elegir numero parametros 
}

ML <- optim(c(mean(muestra2),sd(muestra2)),LL,method="L-BFGS-B", lower = c(0,0))$par

hist(muestra2)
hist(muestra)


media <- mean(muestra2)
varianza <- sd(muestra2)

# Estimación inicial de los parámetros utilizando momentos
shape_inicial <- media^2 / varianza
rate_inicial <- media / varianza

mean(muestra2)

var(muestra2)


library("goftest")

ks.test(muestra3,"pnorm", media_hat ,sd_hat)$statistic

cvm.test(muestra3,"pnorm", media_hat ,sd_hat)$statistic

ad.test(muestra3,"pnorm", media_hat ,sd_hat)$statistic


cvm.test(f,"pgamma", 12,2)

ad.test(f,"pgamma", 12,2)

mean(muestra)

50000/10

pp = rgamma(10000,500,5)

mean(pp)

hist(pp)

hist(muestra)



muestra3 = log(muestra2)


muestra2



muestra55 = exp(muestra3)

hist(muestra55)

