library(shiny)
library(deSolve)
###Definir parámetros

#Inicializar varaibles

t1 <- runif(1000,0.025,0.032)
t2 <- runif(1000,0.025,0.032)
t3 <- t1+t2

r <-  t3 ##tasa crecmiento
p_0 <- as.numeric(15000)
c_o <- as.numeric (2) ##capturas

em1 <- runif(1000,25,140)##embarcaciones
em2 <- runif(1000,25,140)##embarcaciones
em3 <- em1+em2##embarcaciones
e <- em3 ##embarcaciones
se <- sample(e, 1, replace = TRUE)

v1 <- runif(1000,15,25)##viajes
v2 <- runif(1000,15,25)##viajes
v3 <- v1+v2##viajes
v <- v3 ##viajes
sv <- sample(v, 1, replace = TRUE)

cap <- c_o
K <- 30000 ##capaciadad de carga
B <- p_0
logis <- sample(r, 1, replace = TRUE)*B*(1-B/K)

cosecha <- (cap/(se*sv*B))*se*sv*B
               
cambiopop <- logis-cosecha

##Función para calcular la población dinámica
tiempo <- seq(1,100,length=100)


db_m <- function(B,r,K,v,e){
  sv <- sample(v, 1, replace = TRUE)
  se <- sample(e, 1, replace = TRUE)
  sr <- sample(r, 1, replace = TRUE)
  logis <- sr*B*(1-B/K)
  cosecha <- (cap/(se*sv*B))*se*sv*B
  db <- logis-cosecha ##deltapoblación
  return(db)
}

tiempo <- seq(1,100,length=100)
B_sim <- seq(15000,35000, by=100)
tmp <- numeric(length(B_sim))  
tmp <- db_m(B_sim,r,K,v,e) ###delta de población..resultado de la ecuación logistica con distintas poblaciones iniciales
plot(tmp)



out <- ode(y =B, times= tiempo, func=db_m, parms=NULL,method="rk4")

 