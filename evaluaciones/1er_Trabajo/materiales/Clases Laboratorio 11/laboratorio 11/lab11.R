################################################################################
# Demografía - Licenciatura en Estadística                                     #
# Daniel Ciganda / Facundo Morini                                              # 
# 11vo Laboratorio: Tablas de Mortalidad                                       # 
# Octubre de 2026                                                              #
################################################################################

###########################################################
# TABLA DE MORTALIDAD DE PERíODO                          #
###########################################################

# En el laboratorio anterior construimos paso a paso la tabla de mortalidad de
# período para los hombres de Dinamarca en 1900 y reunimos esos pasos en la
# función "compute_lt".
# A esa función le falta un detalle: el último intervalo de edad (95 años y más)
# es un intervalo abierto y todavía lo tratamos igual que a los demás.

# El objetivo de este laboratorio es:
# 1 - corregir en "compute_lt" los años-persona vividos en el intervalo abierto
# 2 - validar los resultados con el paquete demogR
# 3 - analizar la evolución de la esperanza de vida al nacer entre 1900 y 2021

source("plot_fun.R")

# Cargamos los datos
Mx_all <- read.csv(file.path("datos","Mx_1x1_Denmark.txt"),
                   sep = "", skip = 1, header = T)

# Extraer las tasas de mortalidad por edad Mx para hombres de 0 a 95 años en 
# Dinamarca en 1900 utilizando la función plot_Mx con argumento return_data = T,
# también especificar log_escale = F

nMx <- plot_Mx(dat = Mx_all, anios = 1900,edades = 0:95,
               sex = "Male", return_data = T, log_escale = F)


#####################################################
# El intervalo abierto                              #
#####################################################
# El último intervalo de la tabla acumula las defunciones y los años-persona
# vividos por encima de los 95 años: es un intervalo abierto.
# En la función del laboratorio 10 calculamos sus años-persona como en el resto
# de las edades, asumiendo que cada persona que fallece aporta 1/2 intervalo.
# Ese supuesto aquí no es aceptable: como todas las personas que llegan a los
# 95 años fallecen en el intervalo, la esperanza de vida a esa edad resultaba
# ser de 1/2 año, sin importar el nivel de la mortalidad.

# Para aproximar los años-persona vividos en el intervalo abierto recordamos
# que la tasa de mortalidad es el cociente entre defunciones y años-persona:
#
#     nMx = ndx / nLx     es decir     nLx = ndx / nMx
#
# En el último intervalo fallecen todas las personas que llegan a él
# (ndx = lx), por lo tanto:
#
#     nLx = lx / nMx

# Traemos la función "compute_lt" del laboratorio anterior y corregimos
# los años-persona en el intervalo abierto (línea marcada con <--) 

compute_lt <- function(nMx, x, sex, tabla = TRUE){
  
  
  nmax <- length(nMx) 
  nax <- vector() 
  get_na0 <- function(nMx, sex){
    
    if(sex ==  "M"){
      
      if (nMx[1] < 0.023){
        
        na0 <- 0.14929 - 1.99545 * nMx[1]
      }else{
        
        if(nMx[1] >= 0.023 & nMx[1] < .08307){
          
          na0 <- .02832 + 3.26021 * nMx[1]
          
        }else{
          
          na0 <- 0.29915
        }
      }
    }
    if(sex == "F"){
      
      if (nMx[1] <  0.01724){
        
        na0 <- .14903 - 2.05527 * nMx[1]
      }else{
        
        if(nMx[1] >= 0.01724 & nMx[1] < 0.06891){
          
          na0 <- 0.04667 + 3.88089 * nMx[1]
          
        }else{
          
          na0 <- .31411
        }
      }  
      
    }
    
    return(na0)
    
  } 
  
  na0 <- get_na0(nMx, sex)# definimos a0 con la ayuda de la función "get_na0"
  nax[1] <- na0
  nax[2:nmax] <- 1/2
  
  nqx <- (1 * nMx)/(1 + (1 - nax) * nMx)
  nqx[nmax] <- 1
  lx <- cumprod(c(1, 1 - nqx))
  lxn <- lx[-1]
  ndx <- -diff(lx)
  nLx <- lxn + ndx * nax
  nLx[nmax] <- lx[nmax] / nMx[nmax] # <-- corrección del intervalo abierto
  Tx <- rev(cumsum(rev(nLx)))
  ex <- Tx/lx[1:nmax]
  
  lt <- data.frame(x, nax = round(nax, 4),
                   nMx = round(nMx,4),
                   nqx = round(nqx[1:nmax], 4), lx = round(lx[1:nmax],4),
                   ndx = round(ndx, 4), nLx = round(nLx, 4), Tx = round(Tx, 
                                                                        2), 
                   ex = round(ex, 2))
  
  if(tabla){
    return(lt)  
  }else{
    return(ex[1])
  }
  
  
}


# Calculamos la tabla con la función corregida y observamos las últimas edades.
# Comparar la última fila con la del laboratorio 10: cambian los años-persona
# (nLx) del intervalo abierto y, con ellos, la esperanza de vida a los 95 años.
lt <- compute_lt(nMx, x = 0:95, sex = "M")
tail(lt)


#####################################################
# Validación                                        #
#####################################################
# El paquete`demogR` contiene una función para calcular una tabla de vida
# vamos a usar esta función para validar los resultados obtenidos 
# con los cálculos anteriores

# Cargamos el paquete
library(demogR)

# La función recibe como inputs un vector de defunciones por edad y un vector 
# con la exposición al riesgo en cada tramo de edad (años-persona).
# A partir de estos inputs calcula las tasas de mortalidad,
# convierte esas tasa en probabilidades de morir y calcula el resto de la tabla.

# Cargamos los datos - obtenidos de Human Mortality Database -
ndx_all <- read.csv(file.path("datos","Deaths_1x1.txt"),
                    sep = "", skip = 1, header = T)
nkx_all <- read.csv(file.path("datos","Exposures_1x1.txt"),
                    sep = "", skip = 1, header = T)

# Obtenemos las defunciones / exposición al riesgo para hombres
ndx <- ndx_all[ndx_all$Year == 1900 & ndx_all$Age %in% 0:95, "Male"]
nkx <- nkx_all[nkx_all$Year == 1900 & nkx_all$Age %in% 0:95, "Male"]

# Calculamos la tabla - `type` refiere a los factores de separación,
# la opción "cd" es la más similar al procedimiento que utilizamos nosotros
ltd <- life.table(x = 0:95, nDx = ndx, nKx = nkx, iwidth = 1, 
                  width12 = c(1,1), type = "cd")

# Comparamos resultados: la esperanza de vida a cada edad obtenida con nuestra
# función (negro) y con demogR (rojo). Si el procedimiento es correcto, las dos
# series deben prácticamente coincidir.
plot(compute_lt(nMx, 0:95, sex = "M", tabla = T)$ex)
points(ltd$ex,col = "red")

#####################################################
# Análisis de la Evolución de la Esperanza de Vida  #
#####################################################

# Con la función corregida podemos calcular la tabla de mortalidad de cualquier
# año. Vamos a utilizarla para analizar cómo evolucionó la esperanza de vida al
# nacer de hombres y mujeres en Dinamarca.

# Obtener las tasas de mortalidad para los hombes de edades 0 a 95
# desde 1900 a 2021. El argumento "as_list" de la función "plot_Mx" tiene
# que estar en "T" para obtener una lista con las tasas para cada año
# en cada elemento de la lista

nMxc_M <- plot_Mx(dat = Mx_all, anios = 1900:2021, edades = 0:95,
                  sex = "Male", return_data = T, as_list = T, log_escale = F)

# Obtener los mismos resultados para Mujeres
nMxc_F <- plot_Mx(dat = Mx_all, anios = 1900:2021, edades = 0:95,
                  sex = "Female", return_data = T, as_list = T, log_escale = F)


# Obtener la esperanza de vida al nacer para cada año: aplicamos "compute_lt"
# con tabla = F a las tasas de cada año
exc_M <- lapply(nMxc_M, function(x) compute_lt(x, x = 0:95, sex = "M", tabla = F))
exc_F <- lapply(nMxc_F, function(x) compute_lt(x, x = 0:95, sex = "F", tabla = F))


# Graficamos la evolución: mujeres en negro, hombres en rojo
plot(1900:2021, exc_F)
points(1900:2021, exc_M, col = "red")

# Describir los resultados 



