################################################################################
# Demografía - Licenciatura en Estadística                                     #
# Daniel Ciganda / Facundo Morini                                              #
# Trabajo 1 - Segunda Parte: datos de Uruguay y tablas de mortalidad           #
# Octubre de 2026                                                              #
################################################################################

# Este script prepara los datos de defunciones y de población de Uruguay,
# calcula las tasas específicas de mortalidad por edad y, a partir de ellas,
# construye las tablas de mortalidad y la esperanza de vida al nacer de cada
# año.


# Paquete para leer archivos de Excel
library(readxl)

###########################################################
# DEFUNCIONES                                             #
###########################################################
# Fuente: estadísticas vitales del Ministerio de Salud Pública (MSP).
# El archivo def.xlsx tiene las defunciones por sexo, edad simple y año de la
# defunción (1996 a 2020): un bloque de filas para cada sexo, con una fila por
# edad y una columna por año.

# Leemos el archivo salteando las 14 filas de encabezado (skip = 14).
def_raw <- as.data.frame(read_excel("datos/def.xlsx", skip = 14))

# HOMBRES: filas 1 a 115 (edades 0 a 120). A continuación vienen las filas
# "No Indicado" (edad sin clasificar) y "Total", que descartamos.
# Columnas: de la 3 (edad) a la penúltima (2020); la última es el total de
# todos los años.
def_m <- def_raw[1:115, 3:(ncol(def_raw)-1)]
names(def_m)[1] <- "edad" 
str(def_m)

# Los datos se leen como texto, así que los convertimos a numéricos. En el
# archivo las celdas sin defunciones tienen un guión ("-"), que al convertir
# queda como NA y que reemplazamos por 0. El aviso "NAs introducidos por
# coerción" se debe a esto: no es un error.
def_m <- apply(def_m, 2, as.numeric)
def_m[is.na(def_m)] <- 0
def_m <- as.data.frame(def_m)


# MUJERES: mismo procedimiento con las filas 119 a 236 (edades 0 a 119)
def_f <- def_raw[119:236, 3:(ncol(def_raw)-1)]
names(def_f)[1] <- "edad" 
str(def_f)
def_f <- apply(def_f, 2, as.numeric)
def_f[is.na(def_f)] <- 0
def_f <- as.data.frame(def_f)

###########################################################
# POBLACION                                               #
###########################################################
# Fuente: estimaciones y proyecciones de población del Instituto Nacional de
# Estadística (INE), revisión 2013. El archivo tiene la población al 30 de
# junio de cada año por sexo y edad simple: edades 0 a 89 y un último grupo
# de "90 y más".

# La población a mitad de año es nuestra aproximación a los años-persona de
# exposición al riesgo, el denominador de las tasas de mortalidad.

# Leemos el archivo salteando las 4 filas de encabezado
exp_raw <- as.data.frame(read_excel("datos/Total_pais_poblacion_por_sexo_y_edad_1996-2050.xls", skip = 4))

# HOMBRES: filas 99 a 189 (edades 0 a 89 y "90 y más").
# Columnas 1 a 26: la edad y los años 1996 a 2020, los mismos años que tenemos
# en las defunciones.
exp_m <- exp_raw[99:189, 1:26]
str(exp_m)
names(exp_m)[1] <- "edad"

# Convertimos la edad a numérica. La etiqueta "90 y más" queda como NA: 
# le asignamos el valor 90, la edad en la que comienza el
# último intervalo.
exp_m$edad <- as.numeric(exp_m$edad)
exp_m$edad[length(exp_m$edad)] <- 90

# MUJERES: mismo procedimiento con las filas 193 a 283
exp_f <- exp_raw[193:283, 1:26]
str(exp_f)
names(exp_f)[1] <- "edad"
exp_f$edad <- as.numeric(exp_f$edad)
exp_f$edad[length(exp_f$edad)] <- 90


###########################################################
# INTERVALO ABIERTO                                       #
###########################################################
# Las defunciones están registradas por edad simple hasta edades muy
# avanzadas, mientras que la población termina en el grupo abierto "90 y más".
# Para calcular las tasas, defunciones y población tienen que estar definidas
# para las mismas edades.

# Sumamos, para cada año, las defunciones de 90 años y más
open_int_m <- apply(def_m[def_m$edad >= 90,2:ncol(def_m)], 2, sum, na.rm = T)  
open_int_f <- apply(def_f[def_f$edad >= 90,2:ncol(def_f)], 2, sum, na.rm = T)  

# Nos quedamos con las edades 0 a 89 y agregamos una última fila (edad 90) con
# las defunciones del intervalo abierto
def_m <- rbind(def_m[def_m$edad %in% 0:89,], c(90, open_int_m))
def_f <- rbind(def_f[def_f$edad %in% 0:89,], c(90, open_int_f))
  
###########################################################
# TASAS ESPECIFICAS DE MORTALIDAD POR EDAD                #
###########################################################
# >> Ver primero: laboratorio 10 y clase.

# nMx = defunciones / años-persona, para cada edad (filas) y cada año
# (columnas). Antes de dividir sacamos de las dos tablas la primera columna,
# que tiene la edad.
nMx_m <- def_m[,-1]/exp_m[,-1] # HOMBRES

nMx_f <- def_f[,-1]/exp_f[,-1] # MUJERES

# Para graficar vamos a usar la función plot_Mx de los laboratorios. Esa
# función espera los datos en el formato de la Human Mortality Database: una
# fila por cada edad y año, con las columnas "Year", "Age" y una columna con
# las tasas del sexo correspondiente ("Male" / "Female").
# Con reshape() pasamos de una columna por año (formato ancho) a una fila por
# edad y año (formato largo); después agregamos la columna con la edad.
nMx_m <- reshape(nMx_m,
                 direction = "long",
                 varying = list(names(nMx_m)),
                 v.names = "Male",
                 timevar = "Year",
                 times = 1996:2020)

nMx_m <- cbind(def_m[,1], nMx_m[,1:2])
names(nMx_m)[1] <- "Age" 

nMx_f <- reshape(nMx_f,
                 direction = "long",
                 varying = list(names(nMx_f)),
                 v.names = "Female",
                 timevar = "Year",
                 times = 1996:2020)

nMx_f <- cbind(def_f[,1], nMx_f[,1:2])
names(nMx_f)[1] <- "Age" 


###########################################################
# GRAFICAR LAS TASAS                                      #
###########################################################
# >> Ver primero: laboratorios 10 y 11, donde usamos plot_Mx con los datos de
#    Dinamarca.

library(ggplot2)

# Grafica las tasas por edad (en escala logarítmica si log_escale = T) y, con
# return_data = T, devuelve las tasas: en una lista con un elemento por año
# si as_list = T.
plot_Mx <- function(dat, anios, sex, edades, smooth = F,
                    spar_val = 0.3, log_escale = T, return_data = F, as_list =F, save = F){
  dat[dat$Age == "110+", ] <- "110"
  dat[,2] <- as.numeric(dat[,2])
  dat <- dat[dat$Age %in% edades & dat$Year %in% anios, c("Year","Age",sex)] 
  dat[,1] <- as.numeric(dat[,1])
  dat[,3] <- as.numeric(dat[,3])
  
  
  if(log_escale){
    dat[,3] <- log(dat[,3])
    ylims <- c(-13, 1)
  }else{ylims <- c(0, 1)}
  
  if(smooth){
    split_dat <- split(dat, dat$Year)
    
    sm_y <- lapply(split_dat, function(x) smooth.spline(x[,3], spar = spar_val)$y)
    
    dat <- cbind(dat[,1:2], unlist(sm_y))
    
  }
  
  names(dat) <- c("Year","Age","Mx")
  
  p <- ggplot(dat, aes(x = Age, y = Mx,
                       group = as.factor(Year),
                       colour = Year))+
    geom_line() +  scale_colour_gradient(low = "orange", high = "red")+
    theme_bw() +
    ylim(ylims)+
    ylab("M(x)") + xlab("Edad")+
    theme(legend.position = c(0.85, 0.3),
          legend.title = element_text(size = 12),
          legend.text = element_text(size = 10)) +
    theme(plot.background = element_rect(fill = "transparent", colour = NA),
          panel.background = element_rect(fill = "transparent", colour = NA))
  
  if(save){
    pdf(file.path("..","..","..","imagenes", "asfr.pdf"), width=6, height=6) 
    print(p)
    dev.off()
  }
  
  print(p)
  
  if(return_data){
    if(as_list){
      
      split_dat <-  split(dat, dat$Year)
      Mx_list <- lapply(split_dat, function(x) x[,3])  
      return(Mx_list)
    }else{
      return(dat[,3])}
  }
}


# Graficamos las tasas de todos los años, para hombres y para mujeres
plot_Mx(dat = nMx_m, anios = 1996:2020,edades = 0:90, sex = "Male")
plot_Mx(dat = nMx_f, anios = 1996:2020,edades = 0:90, sex = "Female")

# En los gráficos el año 1996 se aparta claramente del resto. Las defunciones
# están disponibles desde 1997: para 1996 el archivo sólo incluye unos pocos
# registros (279 defunciones de hombres y 236 de mujeres en todo el año), por
# lo que esas tasas no son válidas. Eliminamos ese año y volvemos a graficar.

nMx_m <- nMx_m[nMx_m$Year>1996,]
plot_Mx(dat = nMx_m, anios = 1997:2020,edades = 0:90, sex = "Male")
nMx_f <- nMx_f[nMx_f$Year>1996,]
plot_Mx(dat = nMx_f, anios = 1997:2020,edades = 0:90, sex = "Female")


###########################################################
# TABLAS DE MORTALIDAD                                    #
###########################################################
# >> Ver primero: laboratorio 10 (construcción de la tabla paso a paso y
#    función get_na0) y laboratorio 11 (corrección del intervalo abierto).

# compute_lt es la función trabajada en los laboratorios: convierte las tasas
# nMx de un año en una tabla de mortalidad o, con tabla = F, devuelve sólo la
# esperanza de vida al nacer.
compute_lt <- function(nMx, x, sex, tabla = TRUE){
  
  # Definimos el número de intervalos
  nmax <- length(nMx)
  
  # Definimos los factores de separación nax 
  
  # creamos un vector vacio para guardar los nax
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
  
  # definimos a0 con la ayuda de la función "get_na0"
  na0 <- get_na0(nMx, sex)
  
  # assignamos los factores a cada intervalo
  nax[1] <- na0
  nax[2:nmax] <- 1/2
  
  # convertimos las nMx en nqx
  nqx <- (1 * nMx)/(1 + (1 - nax) * nMx)
  
  # nos aseguramos que la probabilidad en el último intervalo sea 1
  nqx[nmax] <- 1
  
  # Construimos las lx
  lx <- cumprod(c(1, 1 - nqx))
  
  # creamos un vector con los sobrevivientes en x+n
  lxn <- lx[-1]
  
  # Obtenemos las defunciones
  ndx <- -diff(lx)
  
  # Obtenemos los años persona en el intervalo nLx
  nLx <- lxn + ndx * nax
  
  # Corrección del intervalo abierto (laboratorio 11): en el último intervalo
  # los años-persona son lx / nMx. Está escrita de otra forma pero es la misma
  # operación: lxn[nmax-1] es lx[nmax] y nMx[length(nMx)] es nMx[nmax].
  nLx[nmax] <- lxn[nmax-1]/nMx[length(nMx)]
  
  # Calculamos los años persona por encima de x
  Tx <- rev(cumsum(rev(nLx)))
  
  # Calculamos la esperanza de vida a edad x
  ex <- Tx/lx[1:nmax]
  
  # Creamos la tabla
  lt <- data.frame(x, nax = round(nax, 4),
                   nMx = round(nMx,4),
                   nqx = round(nqx[1:nmax], 4), lx = round(lx[1:nmax],4),
                   ndx = round(ndx, 4), nLx = round(nLx, 4), Tx = round(Tx, 
                                                                        2), ex = round(ex, 2))
  
  if(tabla){
    return(lt)  
  }else{
    return(ex[1])
  }
  
  
}

###########################################################
# EVOLUCION DE LA ESPERANZA DE VIDA AL NACER              #
###########################################################
# >> Ver primero: laboratorio 11

# Obtenemos las tasas de cada año en una lista (as_list = T) y sin logaritmos
# (log_escale = F), para hombres y para mujeres. La edad 90 representa al
# intervalo abierto de 90 años y más.
nMxc_m <- plot_Mx(dat = nMx_m, anios = 1997:2020, edades = 0:90, 
                  sex = "Male", return_data = T, as_list = T, log_escale = F)
nMxc_f <- plot_Mx(dat = nMx_f, anios = 1997:2020, edades = 0:90,
                  sex = "Female", return_data = T, as_list = T, log_escale = F)


# Calculamos la esperanza de vida al nacer de cada año aplicando compute_lt,
# con tabla = F, a las tasas de cada año
exc_M <- lapply(nMxc_m, function(x) compute_lt(x, x = 0:90, sex = "M", tabla = F))
exc_F <- lapply(nMxc_f, function(x) compute_lt(x, x = 0:90, sex = "F", tabla = F))


# Graficamos la evolución: mujeres en negro, hombres en rojo
plot(1997:2020, exc_F, ylim = c(70, 82))
points(1997:2020, exc_M, col = "red")
