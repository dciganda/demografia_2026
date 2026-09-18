################################################################################
# Demografía - Licenciatura en Estadística                                     #
# Daniel Ciganda / Facundo Morini                                              # 
# 6to Laboratorio: Modelos del proceso reproductivo                            # 
# 4 de Septiembre de 2026                                                      #   
################################################################################

# Definimos las funciones para graficar
source("plot_fun.R")

################
# 1era Parte   #
################################################################################
# Modelos del proceso reproductivo - fecundidad natural a fecundidad regulada  #
################################################################################

# El objetivo ahora es aproximarse al modelado de unas trayectorias reproductivas 
# en un contexto de fecundidad regulada. Para esto vamos a incorporar en el
# modelo que veniamos trabajando hasta hora dos elementos nuevos:

# Las preferencias individuales - Nr. Deseado de Hijos
# El uso de anticonceptivos

# La referencia empírica de nuestro modelo ya no será una población con régimen
# de fecundidad natural como la de los Huteritas, sino una población donde si 
# se aplican sistemáticamente métodos voluntarios de control de los nacimientos.
# Tomamos el ejemplo de España.
# Vamos a utilizar las tasas de fecundidad por edad y por cohorte en España
# disponibles en  la Human Fertility Database 

# Ir a www.humanfertility.org y descargar las tasas específicas de fecundiad
# de cohorte para España guardarlas con el nombre asfr_hfd.txt en la carpeta 
# "datos"

# Cargamos los datos
fx_es <- read.table(file.path("datos","asfr_hfd.txt"), skip = 2,
                    header=T, stringsAsFactors = F)

# Graficamos
plot_fx_hfd(dat = fx_es, cohorts = 1940:1970, type = "lines")


# Describir lo que se observa en el gráfico.



#######################################################
# Preferencias individuales: Número Deseado de Hijos  #  
#######################################################

# Comenzamos por modelar las preferencias. Primero vamos a tener que
# hacernos una idea de como se distribuyen las preferencias del
# tamaño deseado de familia.
# Para esto vamos a explorar una distribución empírica, utilizando datos
# de las "Demographic and Health Surveys" (DHS) de Perú, donde se pregunta a las
# mujeres por su número ideal de hijos (Pregunta: el número total de hijos que
# les hubiera gustado tener a lo largo de toda su vida, independientemente de
# los que ya tienen).

# cargamos los datos
obs_d <- read.table(file.path("datos","ideal_nr_children_per.csv"), header = TRUE)

# explorar los datos
head(obs_d)
table(obs_d)
table(obs_d$ideal_nr, useNA = "always")

# Obtener un data.frame con los valores y las frecuencias
obs_d <- as.data.frame(table(obs_d$ideal_nr), stringsAsFactors = F)
names(obs_d) <- c("nr","freq")

# graficar las frequencias
plot(x = obs_d$nr, y = obs_d$freq, type = "p",
     xlab = "Número ideal de hijos", ylab = "Frecuencia")

# graficar las proporciones
plot(obs_d$nr, obs_d$freq/sum(obs_d$freq), type = "p",
     xlab = "Número ideal de hijos", ylab = "Proporción")

# Describa brevemente lo que se observa en el gráfico



# Vamos a modelar el número deseado de hijos con una distirbución lognormal. 
# Para esto vamos a simular 1000 números aleatorios de una distribución lognromal.
# Con los siguientes parámetros:

mu_d <- 2.52
sd_d <- 1.10

sim_d <- round(rlnorm(n = 5000, meanlog = log(mu_d^2/ sqrt(mu_d^2+sd_d^2)),
                      sdlog = sqrt(log(1 + sd_d^2/mu_d^2))),0)
table(sim_d)

# Comparar el ajuste de los datos simulados a los datos obervados
plot(obs_d$nr, obs_d$freq/sum(obs_d$freq), type = "p",
     xlab = "Número deseado de hijos", ylab = "Proporción",
     ylim = c(0, 0.5), xlim = c(0,10))
sim_tab <- table(factor(sim_d, levels = 0:10))
lines(0:10, sim_tab/sum(sim_tab), type = "p", col = "red")

legend("topright", legend = c("Observado","Simulado"),
       pch = 1, col = c("black","red"), bty = "n")


# Describir los que se observa en el gráfico



# Ahora que comprobamos que la distribución lognormal ajusta bien los datos del 
# número ideal de hijos, podemos utilizar esta distribución para asignar
# el número deseado de hijos a las mujeres/parejas en nuestro modelo de
# simulación (abajo)

#######################################################
# Prácticas Anticonceptivas                           #  
#######################################################
# Para modelar el efecto del uso de anticonceptivos vamos a introducir 
# un parámetro "c".

# Ejercicio: Explorar el modelo y completar los comentarios describiendo 
# las operaciones que se realizan en cada caso.
# Para explorar los objetos se pueden definir los argumentos con estos valores:
n <- 10
ns <- 11
beta1 <- 0.56
beta2 <- -0.053
mu <- 22
su <- 1.1
mu_d <- 2.4
sd_d <- 1.1
c <- 0.1

gen_hst_d <- function(n, ns, beta1, beta2, mu, su, mu_d, sd_d, c){
  
  id <- as.numeric(0)
  wt_c <- as.numeric(0)
  
  k <- rep(0,n)
  edad <- 1:600
  
  # fecundabilidad
  a_s <- (0:479) / 479
  b1 <- 3 * a_s * (1 - a_s)^2
  b2 <- 3 * a_s^2 * (1 - a_s)
  fi_t <- c(rep(0, 120), beta1 * b1 + beta2 * b2)
  fi_t <- pmax(fi_t, 0)
  
  # tiempo de espera a la unión
  wt_u <- rlnorm(n, log(mu^2/ sqrt(mu^2+su^2)), sqrt(log(1 + su^2/mu^2))) * 12 #
  
  fi_it <- lapply(1:n, function(x) fi_t[wt_u[x]:length(fi_t)]) #
  
  dk <- round(rlnorm(n, meanlog = log(mu_d^2/ sqrt(mu_d^2+sd_d^2)),
                        sdlog = sqrt(log(1 + sd_d^2/mu_d^2))),0) #
  
  maxt <- max(sapply(fi_it, length))
  
  for(t in 1:maxt){ 
    
    is <- which(runif(n) < sapply(fi_it,`[`, t) * c^(k >= dk)) #  
    
    if(length(is)!=0){ 
      
      wts <- sapply(is, function(x) (wt_u[x]-1) + t) # 
      
      id <- c(id, is) 
      wt_c <- c(wt_c, wts)
      
      fi_it[is] <- lapply(fi_it[is], function(x){x[t:(t+9+ns)] <- NA; return(x)}) #
      
      k[is] <- k[is] + 1L #
      
    }
    
  }
  
  # data
  hst <- as.data.frame(cbind(id = id, wt_c = wt_c))
  hst <- hst[order(hst$id),]
  hst <- as.data.frame(cbind(id = hst$id,
                             edad = (hst$wt_c + 9)/12,
                             nac = rep(table(hst$id), table(hst$id)),
                             paridad = sequence(table(hst$id))))
  
  attr(hst, "n_cohort") <- as.integer(n)
  return(hst)
}

# Ejercicio: Describir como se modela el número ideal de hijos y el efecto 
# de las prácticas anticonceptivas. 
# Que significa que en el nuevo modelo tenemos dos probabilidades de concebir:
# fecundabilidad y fecundabilidad residual?


# Ejercicio: Utilizando el modelo del proceso reproductivo en régimen de 
# fecundidad regulada:
# aproximar las tasas específicas de fecundidad por edad de la cohorte de 1940 
# en España y de la cohorte de 1970. Comparar los datos observados con los
# simulados (utilizando las funciones plot_fx_hfd y plot_fx) y
# describir los cambios introducidos en los parámetros del modelo para aproximar
# una y otra distribución de tasas específicas. con n = 3000


