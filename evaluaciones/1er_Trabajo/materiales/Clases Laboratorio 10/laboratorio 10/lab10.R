################################################################################
# Demografía - Licenciatura en Estadística                                     #
# Daniel Ciganda / Facundo Morini                                              # 
# 10mo Laboratorio: Tablas de Mortalidad                                       # 
# Octubre de 2026                                                              #
################################################################################

###########################################################
# TABLA DE MORTALIDAD DE PERíODO                          #
###########################################################

# El objetivo es construir una tabla de mortalidad utilizando la información 
# más comunmente disponible en países que cuentan con sistemas de estadísticas
# vitales y censos de población.

########################
# EXPLORAR LOS DATOS   #
########################

# Vamos a utilizar los datos de las tasas específicas de mortalidad por edad,
# Mx, para Dinamarca
# Estos datos se encuentran disponibles en la Human Mortality Database 
# www.mortality.org

# Cargamos los datos
Mx_all <- read.csv(file.path("datos","Mx_1x1_Denmark.txt"),
                   sep = "", skip = 1, header = T)

# Graficamos las Mx utilizando la función plot_Mx para los anios
# 1900:2005 en escala logarítmica para hombres y mujeres 
# (sex = "Male" / "Female") de edades 0 a 95.

source("plot_fun.R")
plot_Mx(dat = Mx_all, anios = 1900:2005,edades = 0:95, sex = "Male")
plot_Mx(dat = Mx_all, anios = 1900:2005,edades = 0:95, sex = "Female")

# Describir lo que se observa en el gráfico. Contestando: 
# Qué indicador estamos utilizando? Qué hay en numerador/denominador?
# Cuáles son las diferencias más imprtantes entre hombres y mujeres?
# Cuál es la tendencia en el tiempo?



#######################################################
# CONSTRUCCION DE LA TABLA DE MORTALIDAD DE PERÏODO   #
#######################################################

# Extraer las tasas de mortalidad por edad Mx para hombres de 0 a 95 años 
# en Dinamarca en 1900 utilizando la función plot_Mx con argumento
# return_data = T, también especificar log_escale = F

nMx <- plot_Mx(dat = Mx_all, anios = 1900,edades = 0:95,
               sex = "Male", return_data = T, log_escale = F)


# La función get_na0 devuelve a0, el factor de separación del primer año de
# vida: el promedio de años vividos por quienes fallecen antes de cumplir un
# año. Estas defunciones se concentran cerca del nacimiento, por lo que no se
# puede asumir a0 = 1/2. En su lugar, a0 se define según el nivel de la
# mortalidad infantil (nMx[1]) y el sexo (males = T para hombres), con las
# fórmulas de Andreev & Kingkade (2015).
get_na0 <- function(nMx, males){
  
  if(males){
    
    # mortalidad infantil baja: a0 aumenta cuando la mortalidad se reduce
    if (nMx[1] < 0.023){
      
      na0 <- 0.14929 - 1.99545 * nMx[1]
    }else{
      
      # mortalidad intermedia: a0 se reduce junto con la mortalidad
      if(nMx[1] >= 0.023 & nMx[1] < .08307){
        
        na0 <- .02832 + 3.26021 * nMx[1]
        
      }else{
        
        # mortalidad alta: a0 constante
        na0 <- 0.29915
      }
    }
  }else{
    
    # mujeres: los mismos tres tramos, con otros umbrales y coeficientes
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


# Definir las edades
x <- 0:95
# Definimos el número de intervalos
nmax <- length(nMx)

# Definimos los factores de separación nax 
# creamos un vector vacio para guardar los nax
nax <- vector()

# definimos a0 con la ayuda de la función "get_na0"
na0 <- get_na0(nMx, T)

# assignamos los factores a cada intervalo
nax[1] <- na0
nax[2:nmax] <- 1/2

# convertimos las nMx en nqx
nqx <- (1 * nMx)/(1 + (1 - nax) * nMx)

# nos aseguramos que la probabilidad en el último intervalo sea 1
nqx[nmax] <- 1

# Construimos las lx - ver cumprod() - Nota: l0 = 1
lx <- cumprod(c(1, 1 - nqx))

# Obtenemos las defunciones
ndx <- -diff(lx)

# creamos un vector con los sobrevivientes en x+n
lxn <- lx[-1]

# Obtenemos los años persona en el intervalo nLx
nLx <- lxn + ndx * nax

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
lt

# La función "compute_lt" reúne los pasos anteriores en una sola función.
# Toma los siguientes argumentos:
# 1 - nMx: Un vector de tásas específicas de mortalidad por edad observadas. 
# 2 - x: Un vector con las edades de inicio de cada intervalo
# 3 - sex: Define si el análisis está hecho para hombres ("M") o mujeres ("F")
# 4 - tabla: si TRUE la función devuelve toda la tabla, si FALSE devuelve sólo el valor de la esperanza de 
# vida al nacer (TRUE por default)

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

# Obtenemos la misma tabla que construimos paso a paso
compute_lt(nMx, x, sex = "M")
