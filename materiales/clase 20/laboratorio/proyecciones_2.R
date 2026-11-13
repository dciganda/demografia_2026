################################################################################
# Demografía - Licenciatura en Estadística                                     #
# Daniel Ciganda / Facundo Morini                                              # 
# Laboratorio: Proyecciones de Población                                       # 
# 13 de Noviembre de 2026                                                      #   
################################################################################
#######################
#  MATRIZ DE LESLIE   #
#######################

# El procedimiento que utilizamos anteriormente para proyectar una población 
# estructurada por edad, puede pensarse como la multiplicación de una matriz
# (la matriz de Leslie) por un vector con la población base. La subdiagonal 
# inferior contiene las probabilidades de superviviencia y la primera fila
# contiene los coeficientes de maternidad neta: hijas que sobreviven al final
# del intervalo por mujer del grupo de edad al inicio.

# El objetivo de esta sección es construir la matriz y utilizarla para obtener 
# la proyección de una población base.

# Cargamos los datos
sw <- read.table("datos/sweden_data_1993.txt", header=F)

# Agregamos nombres a las columnas: "age", "p93", "L", "f"
names(sw) <- c("age", "p93", "L", "f")

# definimos el sex ratio at birth
srb <- 1.05

# Salvamos las columnas relevantes en objetos para facilitar la lectura del código
L <- sw$L
f <- sw$f
N <- sw$p93

# raíz de la tabla de vida; mantenemos L en sus unidades originales
l0 <- 100000
f_female <- f/(1 + srb) # tasas anuales de nacimientos femeninos

# definir una matriz de largo = ancho = length(L)
n = length(L)
M <- #completar

# asignar las probabilidad de supervivencia en los intervalos en la subdiagonal inferior
px <- #completar
# completar <- px

# asignar la probabilidad de supervivencia para los grupos de edad en el intervalo abierto en t+n
M[n,n-1] <- M[n,n] <- #completar

# asignar los coeficientes de maternidad neta en la primera fila
for(i in 1:(n-1)) {
  # completar
}


M %*% sw$p93

# Ejercicio:
# Crear una función "leslie" que tome L, las tasas de fecundidad f (ambos sexos),
# srb y la raíz l0. Mantener L en sus unidades originales y usar L[1]/l0
# al calcular los coeficientes de maternidad neta.
# Convertir f a tasas de nacimientos femeninos dentro de la función,
# dividiendo por (1 + srb) una sola vez.
# Proyectar la población femenina de Suecia en el 93 al 98, utilizando la matriz

leslie <- function(L, f, srb = 1.05, l0 = 100000) {
  
  # completar
  
}

Ml <- leslie(L, f, srb = srb, l0 = l0)

sw$p98 <- Ml %*% sw$p93



# Ejercicio:
# Crear una función "p_pop" que tome las tasas específicas de fecundidad "f", los años persona "L"
# y un vector incial de población "N0" y proyecte la población hacia el futuro.
# agregar un parametro "iter" con el número de iteraciones / intervalos de proyección.
# Pasar f, srb y l0 a leslie; la conversión a nacimientos femeninos se realiza allí.

p_pop <- function(N0, f, L, iter, srb = 1.05, l0 = 100000){

  # 1. Construir la matriz de Leslie usando la función creada antes.

  # 2. Crear una matriz para guardar la población inicial y las proyecciones

  # 3. Guardar N0 en la primera columna de esa matriz.

  # 4. Repetir la proyección iter veces.

  # 5. Devolver la matriz con todas las poblaciones, incluida la inicial.


}
  
sw_p_pop <- p_pop(N0 = sw$p93, f, L, iter = 2, srb = srb, l0 = l0)
  




  

