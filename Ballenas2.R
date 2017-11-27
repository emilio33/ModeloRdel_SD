library(shiny)
library(deSolve)
library (triangle)
###Definir parámetros


dB_m <- function(B,r,K,e,HD){    ##función para evaluar pérdida de población. Arroja el delta
  R <- 150         ##radio
  Conv_ha <- 10000 ## xa convertir metros en hectáreas
  ef <-0.01        ## efecto de disturbio
  se <- sample(e, 1, replace = TRUE)  ##agarra un valor de embarcaciones
  sr <- sample(r, 1, replace = TRUE)  ##agarra un valor de tasa de crecimiento
  
  logis <- sr*B*(1- B/K)                   ##crecimiento logístico de la población
  hd <- (HD - ((pi*(R*R)/Conv_ha)*se*ef))  ##hábitat que queda disponible
  cosecha <- (HD - hd)*(B/hd)              ## lo que se pierde de habitat en relacion a las ballenas (hectareas x ballenas/hectareas= ballenas)
  dB <- logis-cosecha                      ##delta población de ballenas
  return(list(dB=dB, se=se, sr=sr, logis=logis, hd=hd, cosecha=cosecha))
}  


##Función para calcular la población dinámica
tiempo <- seq(1,100,length=100)

Modelo <- function (Poblacion, Y) {

#Inicializar varaibles
K <- 3000 ##capaciadad de carga

## distribución uniforme para la tasa de crecimiento de ballena gris
r <- runif(1000,0.025,0.032)
hist (r)  ##historgrama de la función de probabilidad de la tasa de crecimiento

p_0 <- Poblacion
R <- 150 ##radio
HABi <- 11000  #habitat inicial en hectáreas

e <- rtriangle(1000,22,100)  ## distribución triangular para el número de embarcaciones
hist (e)  ##historgrama de la función de probabilidad de las embarcaciones

B <- p_0
B_c <- 0
HD <- HABi
##Inicializar objetos para guardar los resultados de las simulaciones

PoblacionBall <- numeric(Y)
Embarcaciones <- numeric(Y)
Delta <- numeric(Y)
TasaCrec <- numeric (Y)
Logis <- numeric(Y)
Habitat <- numeric(Y)
Cosecha <- numeric (Y)

for (y in 1:Y){  ##creamos un for loop, es decir iteramos el modelo
 
tmp <- dB_m(B,r,K,e,HD) ###delta de población, resultado de la ecuación logistica con distintas poblaciones iniciales
delta <- tmp$dB
B_c <- B + delta  ##población al tiempo t+1
B <- B_c ##actualizar el valor de B
HD <- tmp$hd ##actualizar el valor de HD


Delta[y] <- delta
PoblacionBall[y] <- B
Embarcaciones [y] <- tmp$se
TasaCrec [y] <- tmp$sr
Habitat[y] <- HD
Logis[y] <-tmp$logis
Cosecha[y] <- tmp$cosecha

}

res<-list(Delta=Delta, PoblacionBall=PoblacionBall, 
          Embarcaciones=Embarcaciones, TasaCrec=TasaCrec, 
          Logis=Logis, Habitat=Habitat, Cosecha=Cosecha)

return(res)  ##al final nos devuelve los resultados
}


resultadoses1 <- Modelo (1400,100)
resultadoses2 <- Modelo (1500,100)
resultadoses3 <- Modelo (3000,100)
resultados4 <- Modelo (3500,100)

resultadoses1
resultadoses2
resultadoses3
resultadoses4
