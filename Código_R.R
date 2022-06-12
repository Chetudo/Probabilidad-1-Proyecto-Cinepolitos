datos = data.frame(palomitas=c('chica', 'mediana', 'grande','jumbo','pllevar','nada'),
                   precio=c(59,66,69,73,79,0),
                   porcion_clientes=c(0.02,0.04,0.07,0.3,0.55,0.02),
                   proba_cambio=c(0.5,0.02,0.3,0.5,0.35,0),
                   inversion=c(10,12,15,16,18,0))

transacciones = read.csv("https://drive.google.com/uc?id=1HpHuW6KXaz_JTT0rvnZBvaYB6xI98FG5&export=download&authuser=0")

#vsd es Viernes, Sábado, Domingo
vsd_media = mean(transacciones[transacciones$dia == 'viernes' | 
                                 transacciones$dia == 'sabado' | 
                                 transacciones$dia == 'domingo', 3])

#lmwj es Lunes, Martes, Miércoles, Jueves
lmwj_media = mean(transacciones[transacciones$dia == 'lunes' | 
                                  transacciones$dia == 'martes' | 
                                  transacciones$dia == 'miercoles' |
                                  transacciones$dia == 'jueves', 3])

#Nos apoyamos de un DataFrame para calcular la probabilidad de pérdida por
#cada tipo de palomitas en días altos
probas_vsd = data.frame(datos$palomitas)
probas_vsd$no_ventas = vsd_media * datos[3]
probas_vsd$media_cambios = probas_vsd[2] * datos[4]
probas_vsd$ganancia = probas_vsd[2] * (datos[2] - datos[5])

poisson_vsd = c()
for (i in 1:5) {
  poisson_vsd = c(poisson_vsd,
                  1-ppois(ceiling(as.numeric(probas_vsd[i, 4]/datos[i, 5])), 
                        as.numeric(probas_vsd[i, 3])))
}
poisson_vsd = c(poisson_vsd, 0)

probas_vsd$proba_perdida = poisson_vsd

#DataFrame para calcular probabilidades de días bajos
probas_lmwj = data.frame(datos$palomitas)
probas_lmwj$no_ventas = lmwj_media * datos[3]
probas_lmwj$media_cambios = probas_lmwj[2] * datos[4]
probas_lmwj$ganancia = probas_lmwj[2] * (datos[2] - datos[5])

poisson_lmwj = c()
for (i in 1:5) {
  poisson_lmwj = c(poisson_lmwj,
                  1-ppois(ceiling(as.numeric(probas_lmwj[i, 4]/datos[i, 5])), 
                          as.numeric(probas_lmwj[i, 3])))
}
poisson_lmwj = c(poisson_lmwj, 0)

probas_lmwj$proba_perdida = poisson_lmwj

#Gráficas
par(mfrow=c(2,3))
plot(0:30, dpois(0:30, as.numeric(probas_vsd[1,3])), type='h', asp=100, 
     main='Distr. chicas VSD', ylab = 'probabilidad', xlab='x')
plot(0:30, dpois(0:30, as.numeric(probas_vsd[2,3])), type='h', asp=25, 
    main='Distr. medianas vsd', ylab = 'probabilidad', xlab='x')
plot(0:50, dpois(0:50, as.numeric(probas_vsd[3,3])), type='h', asp=200, 
     main='Distr. grandes vsd', ylab = 'probabilidad', xlab='x')
plot(200:350, dpois(200:350, as.numeric(probas_vsd[4,3])), type='h', asp=1000, 
     main='Distr. jumbo vsd', ylab = 'probabilidad', xlab='x')
plot(200:350, dpois(200:350, as.numeric(probas_vsd[5,3])), type='h', asp=1000, 
     main='Distr. p.llevar vsd', ylab = 'probabilidad', xlab='x')

par(mfrow=c(2,3))
plot(0:30, dpois(0:30, as.numeric(probas_lmwj[1,3])), type='h', asp=100, 
     main='Distr. chicas lmwj', ylab = 'probabilidad', xlab='x')
plot(0:30, dpois(0:30, as.numeric(probas_lmwj[2,3])), type='h', asp=25, 
     main='Distr. medianas lmwj', ylab = 'probabilidad', xlab='x')
plot(0:50, dpois(0:50, as.numeric(probas_lmwj[3,3])), type='h', asp=200, 
     main='Distr. grandes lmwj', ylab = 'probabilidad', xlab='x')
plot(100:250, dpois(100:250, as.numeric(probas_lmwj[4,3])), type='h', asp=1000, 
     main='Distr. jumbo lmwj', ylab = 'probabilidad', xlab='x')
plot(100:250, dpois(100:250, as.numeric(probas_lmwj[5,3])), type='h', asp=1000, 
     main='Distr. para llevar lmwj', ylab = 'probabilidad', xlab='x')


#Porcentaje pérdida de clientes máxima
s = sum(datos[3]*(datos[2]-datos[5]))
ganancias_sin_politica = sum(probas_vsd[4]-probas_vsd[3]*datos[5])
perdida_maxima_clientes = 1 - ganancias_sin_politica/(s*vsd_media)
perdida_maxima_clientes

#rm(list=ls())