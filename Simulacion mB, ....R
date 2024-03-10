
#Simulación trayectorias movimiento Browniano

#T := variable "tiempo"
#B0 := inicio del MB

TrayectoriasmB <- function(B0, T, numSubintervalos, numTrayectorias) {
  dt <- T/numSubintervalos
  t <- seq(0, T, dt)
  
  datos<-matrix(rnorm((numSubintervalos+1)*numTrayectorias, 0, sqrt(dt)), 
                nrow = numSubintervalos + 1, ncol = numTrayectorias)
  
  datos[1,] <- B0
  trayectorias<-apply(datos, 2, cumsum)
  Particion_trayectorias<-cbind(t, trayectorias)
  return(Particion_trayectorias)
}



#Trayectorias movimiento Browniano geométrico

#s_0:= precio inicial conocido del activo
#mu:= tendencia
#sigma:= volatilidad > 0

DatosmBg<-function(datos, s0, mu, sigma){
  datos[,-1] <- s0*exp((mu - 0.5*sigma^2)*datos[,1] + sigma*datos[,-1])
  return(datos)
}


#Trayectorias del precio de una opción call europea
# s0 := precio spot del activo
# k := Precio de ejercicio
# r := tasa de interés libre de riesgo
# sigma := volatilidad, > 0

DatosCt <- function(datosmBg, T, s0, K, r, sigma) {
  s <- datosmBg[,-1]
  t <- datosmBg[,1]
  
  d1 <- (log(s/K) + (r + 0.5*sigma^2)*(T-t))/(sigma*sqrt(T-t))
  d2 <- (log(s/K) + (r - 0.5*sigma^2)*(T-t))/(sigma*sqrt(T-t))
  
  ct <- s*pnorm(d1) - K*exp(-r*(T-t))*pnorm(d2) 
  return(ct)
}



# Elección de parámetros para la simulación
sigma <- 0.2; s0 <- 1; T <- 20; K = 50

datosmB <- TrayectoriasmB(B0 = 0, T = 20, numSubintervalos  = 10000,
                          numTrayectorias = 1000)

datosmBg <- DatosmBg(datosmB, s0, mu = 0.15, sigma)
datosCt <- DatosCt(datosmBg, T, s0, K, r=0.1, sigma)

t <- datosmB[,1]
Bmax <- ceiling(max(datosmBg))


colors <- sample(0:255, size = nrow(datosmB), replace = TRUE)

#Gráfica de trayectorias de mB
x11()
matplot(t, datosmB[,-1], type = "l", col = colors, 
        xlab = "t", ylab = "mB(t)")

#Gráfica de trayectorias de mBg
x11()
matplot(t, datosmBg[,-1], type = "l", col = colors,  
        xlab = "t", ylab = "mBg(t)", ylim = c(0,Bmax))
abline(h = K, col = "black")

#Gráfica de trayectorias del precio de una opcion call tipo Europea
x11()
matplot(t, datosCt, type = "l", col = colors, 
        xlab = "t", ylab = "C(t)", ylim = c(0,Bmax))
abline(h = K, col = "black")













