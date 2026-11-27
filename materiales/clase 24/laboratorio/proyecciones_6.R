################################################################################
# Demografía - Licenciatura en Estadística                                     #
# Cecilia Waksman / Daniel Ciganda / Facundo Morini                            #
# Laboratorio: Pronóstico de la mortalidad - Modelo Lee-Carter                 #
# Noviembre de 2026                                                            #
################################################################################

###########################################################
# MODELO LEE-CARTER                                       #
###########################################################

# El objetivo es ajustar el modelo Lee-Carter a las tasas de mortalidad por
# edad y año de un país, y utilizarlo para proyectar la mortalidad futura.
#
#   log m(x,t) = a(x) + b(x) * k(t) + e(x,t)
#
# Primero vamos a estimar cada parámetro "a mano", paso a paso, tal como se vio
# en la clase. Luego reuniremos los pasos en una función, evaluaremos qué tan
# bien proyecta el modelo y, por último, compararemos nuestros resultados con
# los de una librería de R.

########################
# EXPLORAR LOS DATOS   #
########################

# Vamos a utilizar los datos de Dinamarca de la Human Mortality Database
# www.mortality.org
# Para ajustar el modelo necesitamos dos insumos que ya vimos:
# - Deaths:    número de defunciones d(x,t)
# - Exposures: años-persona vividos (exposición al riesgo) L(x,t)

# Cargamos los datos
Deaths_all <- read.csv(file.path("datos","Deaths_1x1.txt"),
                       sep = "", skip = 1, header = T)
Expos_all  <- read.csv(file.path("datos","Exposures_1x1.txt"),
                       sep = "", skip = 1, header = T)

head(Deaths_all)

# La última edad está codificada como "110+", por lo que la columna Age es de tipo texto. 
# Como solo vamos a trabajar hasta los 100 años, la convertimos a numérica.
Deaths_all$Age <- as.numeric(ifelse(Deaths_all$Age == "110+", 110, Deaths_all$Age))
Expos_all$Age  <- as.numeric(ifelse(Expos_all$Age == "110+", 110, Expos_all$Age))

# Definimos el período y las edades con las que vamos a trabajar.
# Usaremos las dos poblaciones (hombres y mujeres) juntas: columna "Total".
# Nos quedamos con los años 1950-2019: dejamos afuera los años de la pandemia
# de COVID-19 porque son atípicos.
anios  <- 1950:2019
edades <- 0:100

# La siguiente función arma una matriz con las edades en las filas y los años
# en las columnas a partir de la tabla de la HMD
armar_matriz <- function(dat, sexo = "Total", anios, edades){
  dat <- dat[dat$Year %in% anios & dat$Age %in% edades, ]
  mat <- matrix(dat[, sexo], nrow = length(edades), ncol = length(anios),
                dimnames = list(edades, anios))
  # (los datos están ordenados por año y, dentro de cada año, por edad)
  return(mat)
}

# Matrices de defunciones y de exposición
Dxt <- armar_matriz(Deaths_all, "Total", anios, edades)
Lxt <- armar_matriz(Expos_all,  "Total", anios, edades)

dim(Dxt) # edades x años

# Calculamos las tasas específicas de mortalidad
mxt <- #completar

# El modelo Lee-Carter trabaja con el logaritmo de las tasas. Qué pasa si hay
# alguna edad-año sin defunciones?
sum(mxt == 0)
which(mxt == 0, arr.ind = T)
log(0)

# Este es un problema habitual: hay una combinación edad-año sin defunciones,
# y el log de la tasa no está definido. Una solución simple es suponer que
# hubo media defunción en esa celda
Dxt[Dxt == 0] <- 0.5
mxt <- Dxt / Lxt
lmxt <- log(mxt)

# Graficamos las log-tasas de mortalidad por edad para cada año
source("plot_fun.R")
plot_logmx(lmxt, edades, anios, titulo = "Dinamarca (Total)")

# Describir lo que se observa en el gráfico. Contestando:
# Cuál es la forma de la curva de mortalidad por edad?
# Cuál es la tendencia en el tiempo?
# La mortalidad cambió igual en todas las edades?

#######################################################
# ESTIMACIÓN DEL MODELO: PASO A PASO                  #
#######################################################

# Para poder evaluar la capacidad de proyección del modelo vamos a ajustarlo
# con los años 1950-1999 y dejar los años 2000-2019 para compararlos con lo
# que el modelo hubiera pronosticado.
anios_aj   <- 1950:1999
anios_test <- 2000:2019

D_aj <- Dxt[, as.character(anios_aj)]
L_aj <- Lxt[, as.character(anios_aj)]
lmx_aj <- lmxt[, as.character(anios_aj)]

T_aj <- length(anios_aj) # cantidad de años
n_x  <- length(edades)   # cantidad de edades

###########################
# PASO 1: ESTIMAR a(x)    #
###########################

# a(x) es el promedio, a lo largo del tiempo, de las log-tasas de cada edad.
# Es decir, el promedio de cada fila de la matriz
ax <- #completar

plot(edades, ax, type = "l", lwd = 2, col = "orange",
     xlab = "Edad", ylab = "a(x)")
# Interpretación: exp(ax) es la tasa de mortalidad "típica" de cada edad
# (en realidad, su media geométrica en el período)

# Grafico de las log-tasas observadas de 1950 y de 1999 junto con a(x).
plot(edades, lmx_aj[, "1950"], type = "l", col = "orange", lwd = 1, xlab = "Edad", ylab = "log m(x)")
lines(edades, lmx_aj[, "1999"], col = "red", lwd = 1)
lines(edades, ax, lwd = 3, lty = 2)
legend("bottomright", legend = c("1950", "1999", "a(x)"), col = c("orange", "red", "black"), 
       lty = c(1, 1, 2), lwd = c(1, 1, 3), bty = "n")

###########################################################
# PASO 2: ESTIMAR b(x) Y k(t) CON LA DESCOMPOSICIÓN SVD   #
###########################################################

# Restamos a(x) a cada log-tasa. Nos queda la matriz Z de "desvíos" respecto
# del patrón promedio por edad
Z <- #completar

# Z tiene 101 filas (edades) y 50 columnas (años). Queremos aproximarla por un
# producto b(x) * k(t), es decir, por una matriz de rango 1.
# La función svd() descompone cualquier matriz Z en:  Z = U D V'
sv <- svd(Z)
names(sv)
dim(sv$u); length(sv$d); dim(sv$v)

# sv$d son los valores singulares, ordenados de mayor a menor.
# Qué proporción de la variabilidad de Z explica cada componente?
prop_var <- sv$d^2 / sum(sv$d^2)
round(prop_var[1:5], 4)

# La mejor aproximación de rango 1 de Z (en el sentido de mínimos cuadrados)
# es  Z ~ u1 * d1 * v1'.  Entonces tomamos
b_sv <- #completar # primer vector singular izquierdo (edades)
k_sv <- sv$d[1] * sv$v[, 1]  # primer vector singular derecho (años), multiplicado por d1

###########################################################
# PASO 3: RESTRICCIONES PARA QUE LA SOLUCIÓN SEA ÚNICA    #
###########################################################

# Si multiplicamos b(x) por una constante c y dividimos k(t) por esa misma
# constante, el producto b(x) * k(t) no cambia. Hay infinitas soluciones!
# Para quedarnos con una sola se imponen dos restricciones:
#
#     sum_x b(x) = 1       sum_t k(t) = 0
#
# La segunda ya se cumple porque las filas de Z promedian cero
sum(k_sv) # ≈ 0

# Para la primera, reescalamos: dividimos b(x) por su suma y multiplicamos
# k(t) por la misma cantidad. Así b(x)*k(t) no cambia.
c_norm <- sum(b_sv)
bx <- #completar
kt <- #completar
names(bx) <- edades

sum(bx)
round(sum(kt), 8)

# Verificamos que el producto no cambió
all.equal(bx %o% kt, b_sv %o% k_sv, check.attributes = FALSE)

# Nota: el signo de los vectores singulares es arbitrario (svd() podría
# devolver u1 y v1 con el signo cambiado). Al dividir por sum(b_sv) nos
# aseguramos de que b(x) sume +1 y de que k(t) tenga el signo correcto.

# Graficamos los tres parámetros
names(kt) <- anios_aj
plot_lc_par(ax, bx, kt, edades, anios_aj)


###########################################################
# PASO 4: REAJUSTE DE k(t)                                #
###########################################################

# El ajuste por SVD minimiza el error en el logaritmo de las tasas, y por eso
# da el mismo peso a las edades con pocas defunciones (donde las tasas son muy
# variables) que a las edades con muchas defunciones. Entonces, el número de
# muertes que implica el modelo no tiene por qué coincidir con las observadas.
#
# Comparamos las defunciones observadas con las que predice el modelo
# usando el k(t) de la SVD
muertes_obs <- colSums(D_aj)
muertes_mod <- sapply(1:T_aj, function(t) sum(L_aj[, t] * exp(ax + bx * kt[t])))

plot(anios_aj, muertes_obs, type = "l", lwd = 2, col = "red",
     xlab = "Año", ylab = "Defunciones", ylim = range(muertes_obs, muertes_mod))
lines(anios_aj, muertes_mod, lwd = 2, lty = 2)
legend("bottomright", legend = c("Observadas", "Modelo (k SVD)"),
       col = c("red", "black"), lty = c(1, 2), lwd = 2, bty = "n")

# Para corregirlo se reestima k(t) en cada año t de forma que las defunciones
# totales del año t que implica el modelo coincidan con las observadas:
#
#     sum_x d(x,t) = sum_x L(x,t) * exp( a(x) + b(x) * k(t) )
#
# Para cada año hay que encontrar el k(t) que resuelve esa ecuación (no tiene
# solución cerrada). Lo hacemos con uniroot(), que busca la raíz de una función.

# Ejemplo para el primer año (1950)
t <- 1
f <- function(k) sum(L_aj[, t] * exp(ax + bx * k)) - sum(D_aj[, t])
uniroot(f, interval = c(-100, 100), tol = 1e-10)$root
kt[t]  # k(t) original de la SVD

# Repetimos para todos los años
kt_aj <- sapply(1:T_aj, function(t){
  f <- function(k) sum(L_aj[, t] * exp(ax + bx * k)) - sum(D_aj[, t])
  uniroot(f, interval = c(-100, 100), tol = 1e-10)$root
})
names(kt_aj) <- anios_aj

muertes_mod2 <- sapply(1:T_aj, function(t) sum(L_aj[, t] * exp(ax + bx * kt_aj[t])))
# Verificamos que ahora las defunciones coinciden
all.equal(muertes_obs, muertes_mod2, check.attributes = FALSE)

# Sigue valiendo que la suma de k(t) es cero? Es un problema?
sum(kt_aj)

kt <- kt_aj

###########################################################
# AJUSTE SOBRE LOS AÑOS VISTOS                                     #
###########################################################

# Las log-tasas ajustadas por el modelo son  log-m^(x,t) = a(x) + b(x).k(t)
lmx_hat <- #completar   # el modelo para el log de las tasas (ver el operador %o%)
dimnames(lmx_hat) <- dimnames(lmx_aj)
mx_hat  <- exp(lmx_hat)

# Residuos del modelo en el logaritmo de las tasas: e(x,t) = log m(x,t) - log m^(x,t)
res <- lmx_aj - lmx_hat

# Residuos promedio por edad y por año
op <- par(mfrow = c(1, 2))
plot(edades, rowMeans(res), type = "l", lwd = 2, col = "orange",
     xlab = "Edad", ylab = "Residuo promedio"); abline(h = 0, lty = 2)
plot(anios_aj, colMeans(res), type = "l", lwd = 2, col = "red",
     xlab = "Año", ylab = "Residuo promedio"); abline(h = 0, lty = 2)
par(op)

#######################################################
# FUNCIÓN PARA AJUSTAR EL MODELO LEE-CARTER           #
#######################################################

# La función "fit_lc" reúne los pasos anteriores en una sola función.
# Toma los siguientes argumentos:
# 1 - Dxt: Matriz de defunciones (edades en las filas, años en las columnas)
# 2 - Lxt: Matriz de exposición (años-persona) con la misma forma que Dxt
# La función devuelve una lista con los parámetros ax, bx y kt.

fit_lc <- function(Dxt, Lxt){
  lmx <- log(Dxt / Lxt)
  nt <- ncol(Dxt)
  # Paso 1: a(x)
  ax <- rowMeans(lmx)
  # Paso 2: SVD
  sv <- svd(lmx - ax)
  # Paso 3: restricciones. sum(bx) = 1 y sum(kt) = 0
  bx <- sv$u[, 1] / sum(sv$u[, 1])
  kt <- sv$d[1] * sv$v[, 1] * sum(sv$u[, 1])
  names(bx) <- rownames(Dxt)
  # Paso 4: reajuste de kt para reproducir las defunciones de cada año
  kt <- sapply(1:nt, function(t){
    f <- function(k) sum(Lxt[, t] * exp(ax + bx * k)) - sum(Dxt[, t])
    uniroot(f, interval = c(-100, 100), tol = 1e-10)$root})
  names(kt) <- colnames(Dxt)
  return(list(ax = ax, bx = bx, kt = kt))
}

# Obtenemos los mismos parámetros que calculamos paso a paso
lc <- fit_lc(D_aj, L_aj)
all.equal(lc$ax, ax); all.equal(lc$bx, bx); all.equal(lc$kt, kt)


#######################################################
# PROYECCIÓN DE k(t)                                  #
#######################################################

# Como a(x) y b(x) son fijos, toda la dinámica de la mortalidad futura
# está en k(t). Se proyecta k(t) con un modelo ARIMA; el más habitual es el
# paseo aleatorio con deriva (random walk with drift):
#
#     k(t+1) = k(t) + d + eta(t+1),     eta ~ N(0, sigma^2)
#
# El estimador de la deriva d es el promedio de los cambios anuales de k(t),
# que es simplemente (k(T) - k(1)) / (T - 1)

dk <- diff(kt)
d_hat <- mean(dk)
d_hat
# Es lo mismo que
(kt[T_aj] - kt[1]) / (T_aj - 1)

# Y sigma es la desviación estándar de dk
sigma_hat <- sd(dk)
sigma_hat

# Proyección puntual para un horizonte h = 1, ..., H
#
#     k^(T+h) = k(T) + h * d
H <- length(anios_test)
h <- 1:H
kt_fc <- kt[T_aj] + h * d_hat

# Las bandas de predicción al 95% se abren con la raíz del horizonte:
# Var( k(T+h) ) = h * sigma^2
kt_pred <- cbind(fit = kt_fc,
                 lo  = kt_fc - 1.96 * sigma_hat * sqrt(h),
                 hi  = kt_fc + 1.96 * sigma_hat * sqrt(h))

plot_kt_pred(kt, anios_aj, kt_pred, anios_test)
legend("topright", legend = c("k(t) estimado", "Proyección", "Banda al 95%"),
       col = c("red", "orange", adjustcolor("orange", 0.3)),
       lwd = c(2, 2, 10), lty = c(1, 2, 1), bty = "n")

#######################################################
# PROYECCIÓN DE LAS TASAS Y VALIDACIÓN                #
#######################################################

# Con k^(T+h) proyectamos las tasas de mortalidad
#
#     m^(x,T+h) = exp( a(x) + b(x) * k^(T+h) )
#
lmx_fc <- ax + bx %o% kt_fc #producto exterior, lo mismo que hacer outer(bx, kt_fc)
dimnames(lmx_fc) <- list(edades, anios_test)
mx_fc <- exp(lmx_fc)

# Como dejamos los años 2000-2019 afuera del ajuste, podemos compararlos
# con lo que realmente ocurrió
lmx_test <- lmxt[, as.character(anios_test)]

# Proyección vs. observado para algunas edades
edades_graf <- c(0, 20, 40, 60, 80, 100)
cols <- hcl.colors(length(edades_graf), "Dark 3")
plot(anios_aj, lmx_aj["0", ], type = "n",
     xlim = range(anios), ylim = range(lmxt[as.character(edades_graf), ]),
     xlab = "Año", ylab = "log m(x,t)")
for(i in seq_along(edades_graf)){
  a <- as.character(edades_graf[i])
  points(anios, lmxt[a, ], col = cols[i], pch = 19, cex = 0.5)
  lines(anios_aj, lmx_hat[a, ], col = cols[i], lwd = 2)
  lines(anios_test, lmx_fc[a, ], col = cols[i], lwd = 2, lty = 2)
}
abline(v = 1999.5, lty = 3, col = "grey")
legend("topright", legend = paste("Edad", edades_graf), col = cols, lwd = 2,
       bty = "n", ncol = 3, cex = 0.8)
legend("bottomleft", legend = c("Observado", "Ajustado", "Proyectado"),
       col = "black", pch = c(19, NA, NA), lwd = c(NA, 2, 2),
       lty = c(NA, 1, 2), pt.cex = 0.5, bty = "n")

# Error de proyección en el log de las tasas (raíz del error cuadrático medio)
sqrt(mean((lmx_test - lmx_fc)^2))

# Y por edad
rmse_x <- sqrt(rowMeans((lmx_test - lmx_fc)^2))
plot(edades, rmse_x, type = "l", lwd = 2, col = "orange",
     xlab = "Edad", ylab = "RMSE en log m(x,t)")


#######################################################
# COMPARACIÓN CON UNA LIBRERÍA: vital                 #
#######################################################

# El paquete vital permite ajustar el modelo Lee-Carter y otros modelos de
# mortalidad.

if(!requireNamespace("vital", quietly = TRUE)) install.packages("vital")
library(vital)

# vital trabaja con una tabla en formato largo (una fila por edad y año), con
# las defunciones, la exposición y la tasa. Usamos las mismas defunciones que
# en el ajuste manual (con 0.5 defunciones en las celdas sin defunciones)
dat_vital <- data.frame(Year     = rep(anios_aj, each = n_x),
                        Age      = rep(edades, times = T_aj),
                        Deaths   = as.vector(D_aj),
                        Exposure = as.vector(L_aj))
dat_vital$Mortality <- dat_vital$Deaths / dat_vital$Exposure

dat_vital <- as_vital(dat_vital, index = Year, key = Age, .age = "Age",
                      .deaths = "Deaths", .population = "Exposure")

# Ajuste del modelo. Por defecto: adjust = "dt" (reajuste de k(t) como en
# Lee y Carter) y jump_choice = "fit" (parte de las tasas ajustadas)
lc_vital <- dat_vital |>
  model(lee_carter = LC(log(Mortality)))

# Resumen del modelo: parámetros, modelo para k(t) y varianza explicada
report(lc_vital)

# Extraemos los parámetros
ac <- age_components(lc_vital)  # a(x) y b(x)
tc <- time_components(lc_vital) # k(t)

# Comparamos los parámetros con los que estimamos
op <- par(mfrow = c(1, 3))
plot(edades, lc$ax, type = "l", lwd = 2, col = "orange", xlab = "Edad", ylab = "a(x)")
lines(edades, ac$ax, lty = 2, lwd = 2)
legend("bottomright", legend = c("A mano", "vital"), col = c("orange", "black"),
       lwd = 2, lty = c(1, 2), bty = "n")
plot(edades, lc$bx, type = "l", lwd = 2, col = "orange", xlab = "Edad", ylab = "b(x)")
lines(edades, ac$bx, lty = 2, lwd = 2)
plot(anios_aj, lc$kt, type = "l", lwd = 2, col = "orange", xlab = "Año", ylab = "k(t)")
lines(anios_aj, tc$kt, lty = 2, lwd = 2)
par(op)

# Proyección con vital. Por defecto proyecta k(t) con un paseo aleatorio con
# deriva, igual que hicimos nosotros
lc_fc <- forecast(lc_vital, h = H)
head(lc_fc)

# Cada tasa proyectada es una distribución (porque k(t) es incierto), no un
# número. Para obtener una proyección puntual usamos la mediana, que
# corresponde a exp(a(x) + b(x) * k(T+h)), es decir, a nuestra proyección.
# Ojo: la media de la distribución (columna .mean) es la media de una
# lognormal, que es mayor que exp(a(x) + b(x) * k(T+h)); por eso no coincide.
lc_fc <- lc_fc |>
  dplyr::as_tibble() |>
  dplyr::arrange(Year, Age)
mx_fc_vital <- matrix(quantile(lc_fc$Mortality, 0.5), nrow = n_x, ncol = H,
                      dimnames = list(edades, anios_test))

# Comparamos con nuestras tasas proyectadas
max(abs(mx_fc_vital / mx_fc - 1))

# Y con el error de proyección frente a lo observado (en log de las tasas)
c(propia = sqrt(mean((lmx_test - lmx_fc)^2)),
  vital  = sqrt(mean((lmx_test - log(mx_fc_vital))^2)))

