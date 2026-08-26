################################################################################
# Demografía IESTA                                                             #
# Daniel Ciganda / Facundo Morini                                              #
# 3er Laboratorio: Modelos del proceso reproductivo                            #
# 26 de Agosto de 2026                                                         #
################################################################################

# cargar datos de Huteritas
fx_ht <- read.csv(file.path("datos", "asfrs_ht.csv"), header = T)

# En el laboratorio anterior comenzamos a trabajar con un modelo del proceso
# reproductivo para una cohorte, es decir, un modelo que simula las
# trayectorias reproductivas de una cohorte de mujeres desde el matrimonio
# hasta el comienzo de la menopausia

# Repasamos el funcionamiento del modelo y los resultados preliminares:

gen_hst <- function(n, fi, ns, mu, su){

  wt_u <- rlnorm(n, log(mu^2/ sqrt(mu^2+su^2)), sqrt(log(1 + su^2/mu^2))) * 12 #

  wt_c <- lapply(1:50, function(x) rnbinom(n, 1, fi)) #

  wt_b <- list()
  wt_b[[1]] <- wt_u + wt_c[[1]] + 9 #

  hst <- list()
  hst[[1]] <- as.data.frame(cbind(id = 1:n,
                                  edad = wt_b[[1]]/12,
                                  paridad = 1))

  for(i in 2:50){

    wt_b[[i]] <- wt_b[[i-1]] + ns + wt_c[[i]] + 9 #

    nid <- which(wt_b[[i]]>50*12) #

    wt_b[[i]][nid] <- NA

    if(sum(is.na(wt_b[[i]])) == n){break} #

    hst[[i]] <- as.data.frame(cbind(id = rep(1:n,i),
                                    edad = unlist(wt_b)/12,
                                    paridad = rep(1:i, each = n)))
    hst[[i]] <- hst[[i]][!is.na(hst[[i]]$edad),]
  }

  return(hst)

}

# Simulamos las trayectorias reproductivas de una cohorte de
# 1000 mujeres

# n mayor
ls_hst <- gen_hst(n = 1000, fi = 0.2, ns = 11, mu = 18, su = 1.1)

# Graficamos las tasas específicas de fecundidad por edad f(x) teniendo a
# las f(x) de los Huteritas como referencia
plot(fx_ht, col = "violet", pch = 16, ylim = c(0,0.7))


# extraemos el ultimo elemento de la lista con los outputs del modelo
hst <- ls_hst[[length(ls_hst)]]

# Calculamos las tasas espeificas de fecundidad por edad
fx <- table(factor(floor(hst$edad), levels = 10:50)) / max(hst$id)

#graficamos
points(10:50, as.data.frame(fx)[,2], col = "red")

# Las tasas específicas de fecundidad por edad que obtenemos del modelo
# Siguen el patrón esperado en las primeras edades, pero no se observa
# la caída característica a partir de determinada edad donde se alcanza un máximo


################################################################
# Modelos del proceso reproductivo - Modelo con fecundabilidad #
# dependiente de la edad                                       #
################################################################

# Para generar una distribución realista de f(x) tenemos que considerar
# como evoluciona la capacidad biológica de concebir en el tiempo (edad)

# Representamos el patrón por edad mediante dos bases de Bernstein.
# Primero reescalamos las edades reproductivas al intervalo [0, 1].

a_min <- 10 * 12
a_max <- 50 * 12
a_s <- (0:(a_max - a_min - 1)) / (a_max - a_min - 1)

# Bases polinómicas
b1 <- 3 * a_s * (1 - a_s)^2
b2 <- 3 * a_s^2 * (1 - a_s)

plot((a_min:(a_max - 1)) / 12, b1, type = "l", lwd = 2,
     xlab = "Edad", ylab = "Valor de la base", ylim = c(0, 0.5))
lines((a_min:(a_max - 1)) / 12, b2, col = "blue", lwd = 2)
legend("topright", legend = c("B1", "B2"),
       col = c("black", "blue"), lty = 1, lwd = 2, bty = "n")

# Cada coeficiente asigna un peso a una de las bases.
beta1 <- 0.42
beta2 <- -0.06

# Ejercicio: crear fi_t, el vector con la probabilidad mensual de concebir
# desde el nacimiento hasta los 50 años. Antes de los 10 años el riesgo es 0.
fi_t <- # completar

plot((1:a_max) / 12, fi_t, type = "l", lwd = 2, ylim = c(0, 0.25),
     xlab = "Edad", ylab = "Fecundabilidad mensual")

# Ejercicio: explorar cómo cambia el patrón al modificar los coeficientes.
# Comparamos la curva inicial con dos combinaciones alternativas.

beta1_1 <- 0.322
beta2_1 <- -0.008
fi_t_1 <- c(rep(0, a_min), beta1_1 * b1 + beta2_1 * b2)
fi_t_1 <- pmax(fi_t_1, 0)
lines((1:a_max) / 12, fi_t_1, col = "red", lwd = 2)

beta1_2 <- 0.52
beta2_2 <- -0.12
fi_t_2 <- c(rep(0, a_min), beta1_2 * b1 + beta2_2 * b2)
fi_t_2 <- pmax(fi_t_2, 0)
lines((1:a_max) / 12, fi_t_2, col = "blue", lwd = 2)

legend("topright",
       legend = c("Inicial", "Escenario 1", "Escenario 2"),
       col = c("black", "red", "blue"), lty = 1, lwd = 2, bty = "n")

# Modifique primero beta1 dejando beta2 fijo y vuelva a ejecutar el gráfico.
# Luego modifique beta2 dejando beta1 fijo.

# ¿Qué parte del patrón por edad cambia principalmente al modificar beta1?
# ¿Y al modificar beta2?


###############################################################
# Edad al matrimonio y exposición individual                  #
###############################################################

# Como en el laboratorio anterior, en un contexto de fecundidad natural
# el matrimonio marca el inicio de la exposición al riesgo de concebir.

mu_m <- 20.3
su_m <- 1.18

sdlog <- sqrt(log1p((su_m / mu_m)^2))
meanlog <- log(mu_m) - 0.5 * sdlog^2

# Ejercicio:
# Simular las edades al matrimonio de 8 mujeres, expresadas en meses.
# Utilizar números enteros.

n <- 8
wt_m <- # completar

wt_m / 12

# Ejercicio:
# Para la primera mujer, crear el vector de probabilidades mensuales de
# concebir que comienza en el matrimonio.

i <- 1
fi_one <- # completar

plot((wt_m[i]):(length(fi_t)) / 12, fi_one, type = "l", lwd = 3,
     xlab = "Edad", ylab = "Probabilidad mensual de concebir", bty = "n",
     main = "Exposición de una mujer")

abline(v = wt_m[i] / 12, lty = 3, lwd = 2, col = "red")

text(x = wt_m[i] / 12,
     y = max(fi_one, na.rm = TRUE) * 0.7,
     labels = "Matrimonio",
     pos = 4,
     col = "red")


# Ahora que tenemos un modelo para fi_t podemos incorporarlo a nuestro modelo
# del proceso reproductivo

################################################
# Modelo con fecundabilidad dependiente de t   #
################################################

# Ejercicio: Completar la descripción en las líneas indicadas

gen_hst_t <- function(n, ns, beta1, beta2, mu, su){

  id = numeric(0)
  wt_c = numeric(0)

  meses <- 1:(50*12) #

  # fecundabilidad
  a_s <- (0:479) / 479
  b1 <- 3 * a_s * (1 - a_s)^2
  b2 <- 3 * a_s^2 * (1 - a_s)
  fi_t <- c(rep(0, 120), beta1 * b1 + beta2 * b2)
  fi_t <- pmax(fi_t, 0)
  #plot(meses,fi_t)

  wt_u <- round(rlnorm(n, log(mu^2/ sqrt(mu^2+su^2)), sqrt(log(1 + su^2/mu^2))) * 12, 0) # tiempo de espera a la unión


  fi_it <- lapply(1:n, function(x) fi_t[wt_u[x]:length(fi_t)]) #

  maxt <- max(sapply(fi_it, length)) #

  for(t in 1:maxt){

    is <- which(runif(n) < sapply(fi_it, function(x) x[t])) #

    if(length(is)!=0){

      wts <- sapply(is, function(x) (wt_u[x]-1) + t) #

      id <- c(id, is)
      wt_c <- c(wt_c, wts)


      fi_it[is] <- lapply(fi_it[is], function(x){x[t:(t+9+ns)] <- NA; return(x)}) #

    }

  }

  data <- as.data.frame(cbind(id = id, wt_c = wt_c))
  data <- data[order(data$id),]
  hst <- as.data.frame(cbind(id = data$id,
                             edad = (data$wt_c + 9)/12,
                             nac = rep(table(data$id), table(data$id)),
                             paridad = sequence(table(data$id))))
  return(hst)
}


# Ejercicio: Obtener las tásas específicas de fecundidad por edad para una cohorte de
# 5000 mujeres con un perído de no-suceptibilidad de 11 meses, edad media a la union de
# 20 años (y desvío estandar 1.1) utilizando el modelo con fecundabilidad
# dependiente de t), graficar.
n = 5000
ls_hst <- gen_hst(n = n, fi = 0.2, ns = 11, mu = 20, su = 1.1)
hst_t <- # completar

# graficamos sobre los resultados anterirores
plot(fx_ht, col = "violet", pch = 16, ylim = c(0,0.7))

hst <- ls_hst[[length(ls_hst)]]
fx <- table(factor(floor(hst$edad), levels = 10:50)) / n
points(10:50, as.data.frame(fx)[,2], col = "red")

fx_t <- table(factor(floor(hst_t$edad), levels = 10:50)) / n
points(10:50, as.data.frame(fx_t)[,2], col = "blue")

# Describir lo que se observa en el gráfico.
# A qué se deben las diferencias entre los modelos?
# A que se deben las diferencias entre el modelo más reciente
# y los datos de los Huteritas?



# Ejercicio:

# Buscar una combinación de parámetros que genere unas f(x) simuladas similares
# a los datos de la cohorte de Huteritas.

hst_ht_sim <- # completar

# graficamos sobre los resultados anteriores
plot(fx_ht, col = "violet", pch = 16, ylim = c(0,0.7))
fx_ht_sim <- table(factor(floor(hst_ht_sim$edad), levels = 10:50)) / n
points(10:50, as.data.frame(fx_ht_sim)[,2], col = "red")
