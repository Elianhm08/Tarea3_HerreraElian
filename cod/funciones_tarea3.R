library(dplyr)
library(tidyverse)

#Recibe un dataframe con transacciones, una ventana de días y una k alerta
#Devuelve un df con fecha, numero de transacciones, media,desv y alerta

alerta.movil <- function(df, ventana, k.alerta) {
  conteo <- df %>% 
    group_by(fecha) %>% 
    summarise(n.tx = n(), .groups = "drop") %>% 
    arrange(fecha)
  
n <- nrow(conteo)

#Si se crean en el for y no tendrían resultados no se crean, además alerta es logica
conteo$media <- NA_real_
conteo$desv <- NA_real_
conteo$alerta <- NA

#j=t-v, t-1
#t mayor a v

for (t in seq.int(ventana + 1, n)) {
  
  anteriores <- conteo$n.tx[(t - ventana):(t - 1)]
  
  conteo$media[t] <- mean(anteriores)
  conteo$desv[t] <- sd(anteriores)
  
  conteo$alerta[t] <- (
    conteo$n.tx[t] >
      conteo$media[t] + k.alerta * conteo$desv[t]
  )
}

return(conteo)

}

