################################################################################
# Demografía - Licenciatura en Estadística                                     #
# Daniel Ciganda / Facundo Morini                                              #
# Laboratorio: Proyecciones de Población                                       #
# 11 de Noviembre de 2026                                                      #
################################################################################


# El objetivo es proyectar una población estructurada por edad, en este caso la
# población de Suecia, utilizando información sobre las razones de
# supervivencia en el intervalo de la proyección y unas tasas específicas de
# fecundidad por edad.

# Vamos a trabajar con una población cerrada y compuesta sólo por mujeres.

# Cargamos los datos
sw <-

# Agregamos nombres a las columnas: "age", "p93", "L", "f"


# Ejercicio: Describir el contenido de cada columna de la tabla

# definimos el sex ratio at birth
srb <- 1.05

# definimos la raíz de la tabla de vida
l0 <- 100000

# Salvamos las columnas relevantes en objetos para facilitar la lectura del código
L <- sw$L
f <- sw$f
N <- sw$p93

# guardamos el largo de L en un objeto "n"
n <-

# calcular las razones de supervivencia en un vector "px" (de largo n-1):
# px[i] = L[i+1] / L[i] 
px <-

# grupo abierto: las mujeres de 80-84 y las de 85+ terminan todas en 85+.
# Las dos últimas posiciones de px llevan la misma razón
px[n-1] <- px[n] <-


# Armamos un vector auxiliar con sobrevivientes en 1998, todavía en la posición
# de su grupo de edad de 1993
# (p98_aux[1] son las de 0-4 en 1993, que en 1998 tienen 5-9)
p98_aux <- N * px

# Armamos la columna con la población en 1998, moviendo cada grupo de
# sobrevivientes al grupo de edad siguiente
sw$p98 <- NA

# grupos 2 a n-1 (5-9 a 80-84): las sobrevivientes de los grupos 1 a n-2
sw$p98[2:(n-1)] <-

# grupo n (85+): las sobrevivientes de 80-84 más las sobrevivientes de 85+
sw$p98[n] <-

# el grupo 1 (0-4) queda vacío por ahora: sale de los nacimientos del período


# definimos un vector vacío "B" para guardar en los próximos pasos los nacimientos
B <- vector()

# Calcular los nacimientos entre 1993 y 1998 por edad de la madre.
# Para cada grupo de edad fértil i:
# tasa de fecundidad x 5 años x promedio de mujeres del grupo i al inicio
# (1993) y al final (1998) del período
idf <- which(f>0)

for(i in idf){
  
  B[i-min(idf-1)] <- #completar
}


# calcular los nacimientos de mujeres
B_f <-

# calcular la población de edad 0-4 en 1998
sw$p98[1] <-



# Ejercicio: repetir el procedimiento para proyectar la población a 2003
# Reproducir la tabla presentada en clase con las poblaciones a cada año y los nacimientos
# por edad

