library(shiny)
library(deSolve)
library (triangle)  ## para poder hacer funciones de probabilidad triangulares

####################################################################
#### Función para evaluar pérdida de población. Arroja el delta ####
####################################################################

B_t <- function(tc, B, K, R,  HD, e, ef){     ## Aquí van todos los parámetros (el orden importa) que voy a evaluar
 
  Conv_ha <- 10000                             ## xa convertir metros a hectáreas
 
  logis <- tc*B*(1- B/K)                       ## crecimiento logístico de la población
  hd <- (HD - ((pi*(R*R)/Conv_ha)*e*ef))       ## hábitat que queda disponible
  cosecha <- (HD - hd)*(B/hd)                  ## lo que se pierde de habitat en relacion a las ballenas (hectareas x ballenas/hectareas= ballenas)
  dB <- logis-cosecha                          ## delta población de ballenas
 
   return(list(dB=dB, e=e, tc=tc, logis=logis, hd=hd, cosecha=cosecha))   ## estos son los párametros de los que quiero ver los valores
}  


#### Función xa calcular la población dinámica ####

Modelo <- function (Poblacion, Y) {

   ###############################
   #### Inicializar varaibles ####
   ###############################
  
r <- runif(1000,0.025,0.032)        ## distribución uniforme para la tasa de crecimiento de ballena gris
#hist (r)                           ## historgrama de la función de probabilidad de la tasa de crecimiento
sr <- sample(r, 1, replace = TRUE)  ## agarra un valor de tasa de crecimiento
tc <- sr                            ## valor de la tasa de crecimiento que entra a la función como c.t.

B <- Poblacion
B_c <- 0

K <- 3000       ## capaciadad de carga
R <- 150        ## radio de afectación de las embaracaciones
E <- 22         ## número de embarcaciones inicial
Eadd <- 2       ## número de embarcaciones adicional cada temporada
ef <-0.01       ## efecto de disturbio

HABi <- 11000   ## hábitat inicial en hectáreas
HD <- HABi


#### Inicializar objetos para guardar los resultados de las simulaciones ####


PoblacionBall <- numeric(Y)
Embarcaciones <- numeric(Y)
Delta <- numeric(Y)
TasaCrec <- numeric (Y)
Logis <- numeric(Y)
Habitat <- numeric(Y)
Cosecha <- numeric (Y)

     ##########################################################
     #### creamos un for loop, es decir iteramos el modelo ####
     ##########################################################

for (y in 1:Y){  
 
tmp <- B_t(tc,B,K,R,HD,E,ef) ## aquí es donde se actualiza la función B_t (el orden de los factores es importante y debe ser igual al de la función B_t)
delta <- tmp$dB              ## actualizar el valor de delta, delta de crecimiento de población, resultado de la ecuación logistica con distintas poblaciones iniciales
B_c <- B + delta             ## población al tiempo t+1
B <- B_c                     ## actualizar el valor de B
HD <- tmp$hd                 ## actualizar el valor de HD
E <- tmp$e + Eadd            ## actualizar el valor de E; embarcaciones en operación (embarcaciones en el tiempo anterior más el factor de embarcaciones adicionales)

          
Delta [y] <- delta
PoblacionBall [y] <- B
Embarcaciones [y] <- E
TasaCrec [y] <- tmp$tc
Habitat [y] <- HD
Logis [y] <-tmp$logis
Cosecha [y] <- tmp$cosecha

}

res<-list(Delta=Delta, PoblacionBall=PoblacionBall, 
          Embarcaciones=Embarcaciones, TasaCrec=TasaCrec, 
          Logis=Logis, Habitat=Habitat, Cosecha=Cosecha)

return(res)     ## al final nos devuelve los resultados
}


     ###################################################
     #######         RESULTADOS         ################
     ###################################################

resultadoses1 <- Modelo (1240,100)
resultadoses2 <- Modelo (1500,100)
resultadoses3 <- Modelo (3000,100)
resultados4 <- Modelo (3500,100)

resultadoses1<-as.data.frame(resultadoses1)
tiempo <- seq(1:length(resultadoses1[,1]))
plot(tiempo, resultadoses1$PoblacionBall)
plot(tiempo, resultadoses1$Embarcaciones)
plot(tiempo, resultadoses1$Habitat)


#### Gráfica de ballenas vs tiempo ###########

library(ggplot2)
library(scales)
library(reshape2)

ggplot(resultadoses1, aes(x=tiempo, y=PoblacionBall)) + 
  geom_line(size=1.2) +
  geom_point(size=3,shape=16) +
  theme_bw() + 
  theme(panel.grid.major.y = element_line(linetype="dashed",colour="grey80"),
        panel.grid.major.x = element_blank(),
        panel.grid.minor.x = element_blank(),
        panel.grid.minor.y=element_blank(), 
        panel.border=element_blank(),
        axis.line=element_line(colour="black")) +
  scale_y_continuous (limits=c(0,3000), expand=c(0,0)) +
  scale_x_continuous(limits=c(0,110), expand=c(0,0)) +
  labs(x=expression(" Tiempo (años)")) +
  labs(y=expression("Abundancia de ballenas (individuos)")) 


#### Gráfica de ballenas vs hábitat ###########

ggplot(resultadoses1, aes(x=Habitat, y=PoblacionBall)) + 
  geom_line(size=1.2) +
  geom_point(size=3,shape=16) +
  theme_bw() + 
  theme(panel.grid.major.y = element_line(linetype="dashed",colour="grey80"),
        panel.grid.major.x = element_blank(),
        panel.grid.minor.x = element_blank(),
        panel.grid.minor.y=element_blank(), 
        panel.border=element_blank(),
        axis.line=element_line(colour="black")) +
  scale_y_continuous (limits=c(0,3000), expand=c(0,0)) +
  scale_x_continuous(limits=c(11000,10000), expand=c(11000,10000)) +
  labs(x=expression("Hábitat disponible (ha)")) +
  labs(y=expression("Abundancia de ballenas (individuos)")) +

     scale_x_reverse()


resultadoses2
resultadoses3
resultadoses4
