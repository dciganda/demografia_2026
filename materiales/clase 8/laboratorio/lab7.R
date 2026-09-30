################################################################################
# Demografía - Licenciatura en Estadística                                     #
# Daniel Ciganda / Facundo Morini                                              #
# Laboratorio 7 (Clase 8): Modelos del proceso reproductivo                    #
# 30 de Septiembre de 2026                                                     #
################################################################################

source("plot_fun.R")

# Continuamos trabajando con el modelo del proceso reproductivo en contexto de
# fecundidad regulada definido en el laboratorio anterior

# modelo
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

# Cargamos los datos de España
fx_es <- read.table(file.path("datos","asfr_hfd.txt"), skip = 2, header=T,
                    stringsAsFactors = F)

################################################################################
# Análisis del efecto de las preferencias y prácticas anticonceptivas          #
################################################################################
# El objetivo ahora es realizar un análisis similar al que hicimos con el
# modelo anterior al explorar el efecto de la variación de la edad media a la
# union y el período de no suceptibilidad. Pero esta vez vamos a explorar el
# efecto de las preferencias (nr deseado de hijos) y las prácticas
# anticonceptivas.

# la función "plot_fx" está definida en "plot_fun.R" y devuelve por default
# la sumatoria de las fx (además del gráfico)
# la función "plot_fx_hfd" prepara y gráfica datos provenientes de la
# Human Fertility Database

# Vamos a tomar como referencia empírica las fx de la cohorte de nacidas
# en 1940 en España

plot_fx_hfd(dat = fx_es, cohorts = 1940, type = "points")

# Simular las trayectorias reproductivas para la combinación de parámetros
# que ajusta bien a los datos

hst_ref <-

# Graficar y guardar las tasa global de fecundidad simulada en "tfr_ref"
tfr_ref <- plot_fx(hst_ref, lines  = T)

#######################################################################
# ANALISIS DE SENSIBILIDAD                                            #
#######################################################################
# El objetivo ahora es observar el efecto sobre las tasas específicas
# de fecundidad y sobre el número promedio de hijos por mujer
# de la variación en el valor de los parámetros

# Variando los parámetros de 1 por vez y dejando el resto fijos en los
# valores de la población de referencia (con n = 5000)

#################################
# Prácticas Anticonceptivas     #
#################################
c_vals <- seq(0.01, 0.5, 0.05)

# Lista con datos para cada uno de los valores de c
hst_c_var <-

# graficamos y guardamos la tasa global de fecundidad de la población referencia
ref_tfr_c <-

# graficamos y guardamos la tasa global de fecundidad correspondiente
# a los distintos valores de c
colors <- rainbow(length(c_vals))
var_tfr_c <- lapply(1:length(c_vals), function(x) plot_fx(dat = hst_c_var[[x]],
                                                              lines = T,
                                                              col = colors[x]))
add_legend(c_vals, colors)

# obtenemos las diferencia en la tasa global de fecundidad (TFR)
tfr_dif_c <-

# graficamos
plot(c_vals, tfr_dif_c)

# Describir los resultados obtenidos.




#################################
# Media del nr deseado de hijos #
#################################
mu_d_vals <- seq(1, 5, 0.5)

hst_mu_d_var <-

ref_tfr_mu_d <-

colors <- rainbow(length(mu_d_vals))
var_tfr_mu_d <-

add_legend(mu_d_vals, colors)

tfr_dif_mu_d <-

plot(mu_d_vals, tfr_dif_mu_d)

# Describir los resultados obtenidos.




##################################
# Desvío del nr deseado de hijos #
##################################
sd_d_vals <- seq(0.5, 5, 0.5)
hst_sd_d_var <-

ref_tfr_sd_d <-

colors <- rainbow(length(sd_d_vals))
var_tfr_sd_d <-

add_legend(sd_d_vals, colors)

tfr_dif_sd_d <-

plot(sd_d_vals, tfr_dif_sd_d)

# Describir los resultados obtenidos.


###############################################################################
# Nacimientos No Deseados                                                     #
###############################################################################
# Ejercicio: El objetivo ahora es agregar información sobre el tipo de
# nacimiento en el modelo.
# Para entender la relación entre nacimientos deseados, no deseados y totales.

# Ejercicio: completar en los lugares indicados en el modelo que se presenta
# abajo y luego obtener las graficas para las fx totales, las fx que sólo
# cuentan nacimientos deseados y las fx que sólo cuentan nacimientos no
# deseados

gen_hst_nd <- function(n = 5000,
                       ns = 10,
                       beta1 = 0.56,
                       beta2 = -0.053,
                       mu = 25,
                       su = 3.9,
                       mu_d = 2,
                       sd_d = 1.4,
                       c = 0.03){

  id <-  vector()
  wt_c <- vector()
  k <- rep(0,n)
  edad <- 1:600

  # evolucion de la probabilidad de concebir con la edad
  a_s <- (0:479) / 479
  b1 <- 3 * a_s * (1 - a_s)^2
  b2 <- 3 * a_s^2 * (1 - a_s)
  fi_t <- c(rep(0, 120), beta1 * b1 + beta2 * b2)
  fi_t <- pmax(fi_t, 0)

  # tiempo de espera a la unión
  wt_u <- rlnorm(n, log(mu^2/ sqrt(mu^2+su^2)), sqrt(log(1 + su^2/mu^2))) * 12

  # probabilidad de concebir luego de la union para cada mujer
  fi_it <- lapply(1:n, function(x) fi_t[wt_u[x]:length(fi_t)])

  # número deseado de hijos
  dk <- round(rlnorm(n, meanlog = log(mu_d^2/ sqrt(mu_d^2+sd_d^2)),
                     sdlog = sqrt(log(1 + sd_d^2/mu_d^2))),0)

  maxt <- max(sapply(fi_it, length))

  for(t in 1:maxt){

    is <- which(runif(n) < sapply(fi_it,`[`, t) * c^(k >= dk))

    if(length(is)!=0){

      wts <- sapply(is, function(x) (wt_u[x]-1) + t)

      id <- c(id, is)
      wt_c <- c(wt_c, wts)

      fi_it[is] <- lapply(fi_it[is], function(x){x[t:(t+9+ns)] <- NA; return(x)})

      k[is] <- k[is] + 1L

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

  hst$nd <- hst$paridad > rep(dk, k)

  return(hst)

}

# Obtener resultados y graficar las distintas tasas (negro para fx,
# azul d_fx y rojo nd_fx)
# Describir los resultados obtenidos
out <- gen_hst_nd()

fx <- plot_fx(dat = out, return_fx = T)
d_fx <- plot_fx(out[out$nd==0,], return_fx = T, lines = T, col = "blue")
nd_fx <- plot_fx(out[out$nd==1,], return_fx = T, lines = T, col = "red")


# Reducir la eficacia de los anticonceptivos y comparar resultados
out <- gen_hst_nd(c = 0.1)

fx <- plot_fx(dat = out, return_fx = T)
d_fx <- plot_fx(out[out$nd==0,], return_fx = T, lines = T, col = "blue")
nd_fx <- plot_fx(out[out$nd==1,], return_fx = T, lines = T, col = "red")
