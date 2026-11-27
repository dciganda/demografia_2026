library(ggplot2)

# Gráfico de log-tasas de mortalidad por edad, una curva por año.
# lmx:    matriz de log-tasas con edades en las filas y años en las columnas
# edades: vector de edades (filas de lmx)
# anios:  vector de años (columnas de lmx)
plot_logmx <- function(lmx, edades, anios, titulo = ""){

  dat <- data.frame(Age = rep(edades, times = length(anios)),
                    Year = rep(anios, each = length(edades)),
                    logmx = as.vector(lmx))

  p <- ggplot(dat, aes(x = Age, y = logmx,
                       group = as.factor(Year),
                       colour = Year)) +
    geom_line() +
    scale_colour_gradient(low = "orange", high = "red") +
    theme_bw() +
    ylab("log m(x,t)") + xlab("Edad") +
    ggtitle(titulo) +
    theme(legend.title = element_text(size = 12),
          legend.text = element_text(size = 10)) +
    theme(plot.background = element_rect(fill = "transparent", colour = NA),
          panel.background = element_rect(fill = "transparent", colour = NA))

  print(p)
}

# Gráfico de los tres parámetros del modelo Lee-Carter: a_x, b_x y k_t.
plot_lc_par <- function(ax, bx, kt, edades, anios){

  op <- par(mfrow = c(1, 3))

  plot(edades, ax, type = "l", lwd = 2, col = "orange",
       xlab = "Edad", ylab = expression(a[x]), main = expression(a[x]))

  plot(edades, bx, type = "l", lwd = 2, col = "orange",
       xlab = "Edad", ylab = expression(b[x]), main = expression(b[x]))
  abline(h = 0, lty = 2, col = "grey")

  plot(anios, kt, type = "l", lwd = 2, col = "red",
       xlab = "Año", ylab = expression(k[t]), main = expression(k[t]))
  abline(h = 0, lty = 2, col = "grey")

  par(op)
}

# Gráfico del pronóstico de k_t con bandas de predicción.
# kt:      valores de k_t estimados (años observados)
# kt_pred: matriz con columnas "fit", "lo" y "hi" para los años pronosticados
plot_kt_pred <- function(kt, anios, kt_pred, anios_pred){

  plot(c(anios, anios_pred), c(kt, kt_pred[, "fit"]), type = "n",
       ylim = range(kt, kt_pred), xlab = "Año", ylab = expression(k[t]))
  polygon(c(anios_pred, rev(anios_pred)),
          c(kt_pred[, "lo"], rev(kt_pred[, "hi"])),
          col = adjustcolor("orange", 0.3), border = NA)
  lines(anios, kt, lwd = 2, col = "red")
  lines(anios_pred, kt_pred[, "fit"], lwd = 2, lty = 2, col = "orange")
  abline(v = max(anios) + 0.5, lty = 3, col = "grey")
}
