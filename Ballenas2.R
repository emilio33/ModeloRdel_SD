library(shiny)
library(deSolve)
library (triangle)
###Definir parámetros


##Función para calcular la población dinámica
tiempo <- seq(1,100,length=100)

Modelo <- function (Poblacion, Y) {

#Inicializar varaibles
K <- 30000 ##capaciadad de carga

## distribución uniforme para la tasa de crecimiento de ballena gris
r <- runif(1000,0.025,0.032)

p_0 <- Poblacion
radi <- as.numeric (150) ##radio
e <- rtriangle(1000,22,100)  ## distribución triangular para el número de embarcaciones
efect <-as.numeric (0.01) ## efecto del disturbio
ef<- efect
R <- radi
B <- p_0
B_c <- 0
##Inicializar objetos para guardar los resultados de las simulaciones
PoblacionBall <- numeric(Y)
Embarcaciones <- numeric(Y)
Delta <- numeric(Y)
TasaCrec <- numeric (Y)



for (y in 1:Y){  

dB_m <- function(B,r,K,e){    ##función para evaluar pérdida de población. Arroja el delta
  se <- sample(e, 1, replace = TRUE)  ##agarra un valor de embarcaciones
  sr <- sample(r, 1, replace = TRUE)  ##agarra un valor de tasa de crecimiento
  logis <- sr*B*(1-B/K)                ##crecimiento logístico de la población
  habdis<- (11000-((3.1415*(R*R)/10000)*se*ef))  ##hábitat que queda disponible
  cosecha <- (11000 - habdis)*(B/habdis)  ## lo que queda de hábitat multiplicado por la densidad de ballenas (hectareas x ballenas/hectareas= ballenas)
  
  dB <- logis-cosecha ##delta población de ballenas
  return(list(dB=dB, se=se, sr=sr))
}   


tmp <- dB_m(B,r,K,e) ###delta de población, resultado de la ecuación logistica con distintas poblaciones iniciales

delta <- tmp$dB
B_c <- B + delta  ##población al tiempo t+1
B <- B_c ##actualizar el valor de B
Delta[y] <- delta
PoblacionBall[y] <- B
Embarcaciones [y] <- tmp$se
TasaCrec [y] <- tmp$sr
}

res<-list(Delta=Delta, PoblacionBall=PoblacionBall, Embarcaciones=Embarcaciones, TasaCrec=TasaCrec)

return(res)
}


resultadoses1 <- Modelo (1400,100)
resultadoses2 <- Modelo (1500,100)
resultadoses3 <- Modelo (3000,100)

resultadoses1
resultadoses2
resultadoses3

