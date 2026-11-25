################################################################################
# Demografía - Licenciatura en Estadística                                     #
# Daniel Ciganda / Facundo Morini                                              #
# Laboratorio: Proyecciones de Fecundidad                                      #
# 25 de Noviembre de 2026                                                      #
################################################################################
# El objetivo de este laboratorio es completar la fecundidad de cohortes que
# todavía no terminaron su vida reproductiva, con los dos métodos que vimos en
# clase: tasas congeladas y extrapolación lineal.

# Vamos a trabajar con las cohortes de España. Para saber cuánto se equivoca cada
# método nos paramos en el año 2000, usamos sólo lo que se sabía en ese momento
# y comparamos el resultado con lo que pasó después.

# Definimos las funciones para graficar
source("plot_fun.R", encoding = "UTF-8")

#############
#   DATOS   #
#############

# Cargamos las tasas de fecundidad por edad y cohorte de España (Human Fertility
# Database).
es <- read.table("datos/asfr_hfd.txt", skip = 2, header = T, na.strings = ".",
                 stringsAsFactors = F)

# Agregamos nombres a las columnas
names(es) <- c("cohorte", "edad", "fx")

# Algunas edades vienen como texto ("12-", "55+"), las pasamos a número
es$edad <- as.numeric(gsub("[-+]", "", es$edad))

# Nos quedamos con las edades 15 a 49 y las cohortes 1920 a 2007
es <- es[es$edad %in% 15:49 & es$cohorte %in% 1920:2007, ]

# Pasamos los datos a una matriz con una fila por cohorte y una columna por edad
f <- tapply(es$fx, list(es$cohorte, es$edad), sum)

# Guardamos las cohortes y las edades en dos vectores
coh <- as.numeric(rownames(f))
edad <- as.numeric(colnames(f))

# Las filas y las columnas se pueden elegir por su nombre. Por ejemplo, la tasa de
# la cohorte 1970 a los 30 años:
f["1970", "30"]

# Graficamos las tasas por edad de algunas cohortes
plot_tasas(f, cohortes = c(1940, 1950, 1960, 1970))

# Ejercicio: Calcular la fecundidad completa de cada cohorte (CF) y guardarla en un
# objeto "CF". ¿Cuál es la última cohorte con la fecundidad completa?
CF <- #completar



####################################
#   LO QUE SE SABÍA EN EL AÑO 2000 #
####################################

# Definimos el año de partida
t0 <- 2000

# Una tasa es conocida en t0 si cohorte + edad <= t0. Creamos una matriz "f_obs"
# igual a f, pero con NA en las celdas que no se conocían en t0.
# outer(coh, edad, "+") devuelve una matriz con la suma cohorte + edad de cada celda.
f_obs <- f
f_obs[outer(coh, edad, "+") > t0] <- NA

# Verificamos con la matriz que vimos en clase (tasas por mil)
1000 * f_obs[as.character(1961:1972), as.character(29:35)]

# Ejercicio: ¿Hasta qué edad se conoce la cohorte 1965? ¿Y la 1970? ¿Cuál es la
# última cohorte observada a los 33 años?


#######################################
#   MÉTODO 1: TASAS CONGELADAS        #
#######################################

# A cada edad que falta se le asigna la tasa que tenía esa edad en el último año
# observado. En la matriz: en cada columna, la última tasa observada se copia
# hacia abajo.

# Ejercicio: Completar la función "congelar", que toma la matriz f_obs y el año de
# partida t0 y devuelve la matriz completa.

congelar <- function(f_obs, t0) {

  coh <- as.numeric(rownames(f_obs))
  edad <- as.numeric(colnames(f_obs))

  # matriz donde guardamos las tasas completadas
  f_hat <- f_obs

  # recorremos las edades (columnas)
  for(j in 1:length(edad)) {

    # última cohorte observada a esa edad
    u <- #completar

    # cohortes que hay que completar
    id <- which(coh > u)

    # asignar la última tasa observada
    f_hat[id, j] <- #completar
  }

  return(f_hat)
}

f_fr <- congelar(f_obs, t0)

# Ejercicio: Calcular la fecundidad completa de cada cohorte con las tasas congeladas
# y comparar con la observada para la cohorte 1970
CF_fr <- #completar



#######################################
#   MÉTODO 2: EXTRAPOLACIÓN LINEAL    #
#######################################

# En lugar de copiar la última tasa, se prolonga la tendencia de cada edad. La
# pendiente es el cambio medio de los últimos cinco valores observados. Se
# extrapola durante cinco años y después se congela.

# Ejercicio: Completar la función "lineal", que toma la matriz f_obs, el año de
# partida t0 y el horizonte hmax (cuántos años se extrapola).

lineal <- function(f_obs, t0, hmax = 5) {

  coh <- as.numeric(rownames(f_obs))
  edad <- as.numeric(colnames(f_obs))

  f_hat <- f_obs

  for(j in 1:length(edad)) {

    # última cohorte observada a esa edad
    u <- #completar

    # cohortes que hay que completar
    id <- which(coh > u)

    # pendiente: cambio medio entre la cohorte u - 4 y la cohorte u
    b <- #completar

    # años que hay que avanzar para llegar a cada cohorte
    h <- coh[id] - u

    # última tasa observada más la pendiente, hmax años como máximo
    # (ver la función pmin)
    f_hat[id, j] <- #completar
  }

  # una tasa no puede ser negativa
  f_hat[f_hat < 0] <- 0

  return(f_hat)
}

f_lin <- lineal(f_obs, t0)

# Ejercicio: Calcular la fecundidad completa con la extrapolación lineal y comparar
# los dos métodos con la observada para la cohorte 1970
CF_lin <- #completar


# Ejercicio: ¿Qué se obtiene con hmax = 0?


#######################################
#   COMPARAR LOS DOS MÉTODOS          #
#######################################

# Tasas de la cohorte 1970: lo conocido en 2000, lo que pasó y los dos métodos
plot_cohorte(1970, t0, f, f_obs, f_fr, f_lin)

# Ejercicio: ¿A qué edades se equivocan las tasas congeladas? ¿Por qué?


# Ejercicio: ¿Qué hace distinto la extrapolación lineal? ¿Acierta en todas las edades?


# Fecundidad completa de las cohortes 1950 a 1973 y error de cada método
plot_cf(1950:1973, t0, CF, CF_fr, CF_lin)
plot_error(1960:1973, t0, CF, CF_fr, CF_lin)

# Ejercicio: ¿Qué método se equivoca menos? ¿Para qué lado se equivocan? ¿En qué
# cohortes el error es más grande y por qué?
