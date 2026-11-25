################################################################################
# Funciones para graficar - Laboratorio: Proyecciones de Fecundidad            #
################################################################################

# Tasas por edad de varias cohortes
plot_tasas <- function(f, cohortes) {

  edad <- as.numeric(colnames(f))
  sel <- as.character(cohortes)
  cols <- grey(seq(0.75, 0, length.out = length(sel)))

  matplot(edad, t(f[sel, ]), type = "l", lty = 1, lwd = 2, col = cols,
          xlab = "Edad", ylab = "Tasa", main = "España: tasas por edad y cohorte")
  legend("topright", legend = sel, col = cols, lwd = 2, bty = "n")
}

# Tasas de una cohorte: lo conocido en t0, lo que pasó y los dos métodos
plot_cohorte <- function(c0, t0, f, f_obs, f_fr, f_lin) {

  edad <- as.numeric(colnames(f))
  sel <- as.character(c0)

  plot(edad, f[sel, ], type = "l", lwd = 3, col = "grey70", ylim = c(0, 0.12),
       xlab = "Edad", ylab = "Tasa", main = paste("Cohorte", c0, "desde", t0))
  lines(edad, f_fr[sel, ], lwd = 2, lty = 2, col = "#D55E00")
  lines(edad, f_lin[sel, ], lwd = 2, lty = 3, col = "#009E73")
  lines(edad, f_obs[sel, ], lwd = 3)
  abline(v = t0 - c0, lty = 3)
  legend("topright", bty = "n", lwd = 2, lty = c(1, 1, 2, 3),
         col = c("black", "grey70", "#D55E00", "#009E73"),
         legend = c("conocido", "lo que pasó", "congeladas", "lineal"))
}

# Fecundidad completa por cohorte: observada y con los dos métodos
plot_cf <- function(cohortes, t0, CF, CF_fr, CF_lin) {

  sel <- as.character(cohortes)

  plot(cohortes, CF[sel], type = "l", lwd = 3,
       ylim = range(CF[sel], CF_fr[sel], CF_lin[sel], na.rm = TRUE),
       xlab = "Cohorte", ylab = "Hijos por mujer",
       main = paste("Fecundidad completa, desde", t0))
  lines(cohortes, CF_fr[sel], lwd = 2, lty = 2, col = "#D55E00")
  lines(cohortes, CF_lin[sel], lwd = 2, lty = 3, col = "#009E73")
  legend("topright", bty = "n", lwd = 2, lty = 1:3,
         col = c("black", "#D55E00", "#009E73"),
         legend = c("observada", "congeladas", "lineal"))
}

# Error de cada método por cohorte (completada - observada)
plot_error <- function(cohortes, t0, CF, CF_fr, CF_lin) {

  sel <- as.character(cohortes)
  err_fr <- CF_fr[sel] - CF[sel]
  err_lin <- CF_lin[sel] - CF[sel]

  plot(cohortes, err_fr, type = "b", pch = 16, lwd = 2, col = "#D55E00",
       ylim = range(0, err_fr, err_lin), xlab = "Cohorte", ylab = "Hijos por mujer",
       main = paste("Error de cada método, desde", t0))
  lines(cohortes, err_lin, type = "b", pch = 17, lwd = 2, col = "#009E73")
  abline(h = 0, lty = 3)
  legend("bottomleft", bty = "n", lwd = 2, pch = c(16, 17),
         col = c("#D55E00", "#009E73"), legend = c("congeladas", "lineal"))
}
