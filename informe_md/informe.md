Contraste de correlaciones muestreo secuencial 
========================================================

Simulación con regla de parada fija
--------------------------------------------------------




La regla de parada fija es el procedimiento estándar para el contraste de hipótesis. Se selecciona una muestra de tamaño n en función del tamaño del efecto que se desea detectar y de la potencia deseada para detectarlo. Una vez fijado ese tamaño muestral, se recogen los datos y se analizan. 

### Estructura de la regla fija
En el contexto de esta simulación, la estructura de la regla de muestreo fija es la siguiente: 


```r
SimCorr <- function(n, r) {
    require(MASS)
    muestra <- mvrnorm(n, Sigma = matrix(c(1, r, r, 1), 2), mu = rep(0, 2))
    out.cor <- cor(muestra[, 1], muestra[, 2])
    tstat <- out.cor * sqrt(n - 2)/sqrt(1 - out.cor^2)
    pvalue <- 1 - pt(tstat, n - 2)
    decision <- ifelse(pvalue <= 0.05, 1, 0)
    return(c(out.cor, tstat, pvalue, decision))
}
```


Es decir, se generan dos muestras independientes de tamaño *n* de una distribución normal multivariante $N_2(\mu, \Sigma)$ con $\mu = [0,0]$ con matriz de correlaciones: 

$$
  \begin{aligned}
  \Large
  \begin{pmatrix}
  1 & \rho \\
  \rho & 1
  \end{pmatrix}
  \end{aligned}
$$

Se fijan por tanto las varianzas a $\sigma^2 = 1$ y la correlación entre variables se establecen como $\rho$ en las distintas condiciones de simulación. Para la hipótesis nula se toma un valor de $\rho = 0$. 

Se calcula entonces la correlación de Pearson $r_{XY}$ entre las dos muestras extraídas y se realiza un contraste de hipótesis unilateral derecho; se contrasta la hipótesis nula $H_0: \rho \le 0$ con la hipótesis alternativa en $H_1: \rho > 0$. Se fija la tasa de error tipo I bajo $H_0$ verdadera en $\alpha = 0.05$. 

#### Parámetros de la simulación
Se manipulan fundamentalmente dos parámetros en las simulaciones: el tamaño de la muestra, *n*, y los valores de la correlación poblacional $\rho$. Los valores de los tamaños muestrales fueron los siguientes: 

- Tamaños muestrales: 

  $N = \{20,~30,~40,~50,~60\}$

- Correlaciones poblacionales: 

  $\rho = \{0,~0.1,~0.2,~0.3 ...~0.8,~0.99\}$
  
La elección de $0.99$ como valor máximo para las simulaciones viene dada por la inutilidad de realizar contrastes de hipótesis cuando la relación entre las variables es determinista (i.e., $\rho = 1$).

Esto genera un total de 25 escenarios de simulación, cada uno de los cuales es repetido un total de 10,000 veces, para un total de 250,000 simulaciones. 

Todas las simulaciones fueron realizadas en el programa R version 3.0.3. 

### Evaluación de la simulación
En primer lugar, se evalúan las correlaciones medias tras 10,000 simulaciones para cada regla, para determinar si las correlaciones empíricas generadas convergen hacia la correlación poblacional teórica de la que han sido extraídas. Se construyen también intervalos de confianza para las correlaciones empíricas generadas y se calcula en qué proporción de las 10,000 simulaciones en cada condición el parámetro r se ve incluido dentro del intervalo de confianza. 

Dado que en meta-análisis es más común analizar estudios de correlaciones empleando la transformación de Fisher, se transforman las correlaciones obtenidas a Z: 

$$latex
Z = 0.5 \cdot  \ln \left(\frac {1 + r} {1 - r} \right)
$$

En meta-análisis es común ponderar la Z de Fisher obtenida en un estudio primario por el inverso de su varianza. Dicha varianza viene dada por:

$$latex
var(Z) = \frac{1}{n_i - 3}
$$

Esta varianza puede ser considerada la *varianza teórica* de Z, esto es, la varianza estimada a partir del tamaño de la muestra. Por el contrario, en simulación es posible calcular la *varianza empírica* de Z, como la varianza de los valores de Z calculados en cada una de las simulaciones. Se puede así valorar si esta estimación converge hacia el valor de la varianza empírica. 

Se evalúa además la *tasa empírica de rechazos* de la $H_0$ a lo largo de las 10,000 simulaciones en cada condición. Cuando $H_0$ es verdadera (es decir, cuando $\rho = 0$), dicha tasa corresponde a la probabilidad de error tipo I. Cuando es falsa ($\rho > 0$), corresponde a la potencia de la prueba. 

Se calculan también los *intervalos de confianza* con $\alpha = 0.05$ para los valores de $\rho$. Dichos valores se calculan a partir de las transformaciones Z mediante la fórmula: 

$$latex
IC_{95} = Z \pm \frac{1.96} {\sqrt{n - 3}}
$$

El intervalo de confianza así construído se transforma de vuelta a valores de $\rho$ mediante la fórmula 

$$latex
r = \frac{e^{2z} -1} {e^{2z} + 1}
$$

Se construye así un intervalo de confianza para cada resultado de simulación y se determina si el valor del parámetro $\rho$ se encuentra dentro del intervalo; esto equivale al test de significación mediante intervalos de confianza. Se determina la proporción de casos en los que el valor de r está incluido en el intervalo, es decir, la *cobertura* de la prueba. 

### Resultados
La siguiente tabla muestra los resultados resumidos para la RTP: 

<!-- html table generated in R 3.0.3 by xtable 1.7-3 package -->
<!-- Wed Jun 18 15:59:07 2014 -->
<TABLE border=1>
<TR> <TH>  </TH> <TH> corr_teo </TH> <TH> nfsr </TH> <TH> EPR </TH> <TH> zfisher </TH> <TH> corr_emp </TH> <TH> theo_var_z </TH> <TH> emp_var_z </TH> <TH> cover_ic </TH> <TH> limit_z_inf </TH> <TH> limit_z_sup </TH> <TH> limit_r_inf </TH> <TH> limit_r_sup </TH>  </TR>
  <TR> <TD align="right"> 1 </TD> <TD align="right"> 0.00 </TD> <TD> 20 </TD> <TD align="right"> 0.0511 </TD> <TD align="right"> -0.0006 </TD> <TD align="right"> -0.0006 </TD> <TD align="right"> 0.0588 </TD> <TD align="right"> 0.0586 </TD> <TD align="right"> 0.0491 </TD> <TD align="right"> -0.4760 </TD> <TD align="right"> 0.4747 </TD> <TD align="right"> -0.4237 </TD> <TD align="right"> 0.4228 </TD> </TR>
  <TR> <TD align="right"> 2 </TD> <TD align="right"> 0.00 </TD> <TD> 30 </TD> <TD align="right"> 0.0511 </TD> <TD align="right"> -0.0004 </TD> <TD align="right"> -0.0005 </TD> <TD align="right"> 0.0370 </TD> <TD align="right"> 0.0367 </TD> <TD align="right"> 0.0508 </TD> <TD align="right"> -0.3776 </TD> <TD align="right"> 0.3768 </TD> <TD align="right"> -0.3498 </TD> <TD align="right"> 0.3490 </TD> </TR>
  <TR> <TD align="right"> 3 </TD> <TD align="right"> 0.00 </TD> <TD> 40 </TD> <TD align="right"> 0.0510 </TD> <TD align="right"> 0.0020 </TD> <TD align="right"> 0.0019 </TD> <TD align="right"> 0.0270 </TD> <TD align="right"> 0.0274 </TD> <TD align="right"> 0.0492 </TD> <TD align="right"> -0.3202 </TD> <TD align="right"> 0.3242 </TD> <TD align="right"> -0.3024 </TD> <TD align="right"> 0.3059 </TD> </TR>
  <TR> <TD align="right"> 4 </TD> <TD align="right"> 0.00 </TD> <TD> 50 </TD> <TD align="right"> 0.0494 </TD> <TD align="right"> -0.0011 </TD> <TD align="right"> -0.0010 </TD> <TD align="right"> 0.0213 </TD> <TD align="right"> 0.0212 </TD> <TD align="right"> 0.0496 </TD> <TD align="right"> -0.2870 </TD> <TD align="right"> 0.2848 </TD> <TD align="right"> -0.2740 </TD> <TD align="right"> 0.2721 </TD> </TR>
  <TR> <TD align="right"> 5 </TD> <TD align="right"> 0.00 </TD> <TD> 60 </TD> <TD align="right"> 0.0484 </TD> <TD align="right"> -0.0007 </TD> <TD align="right"> -0.0007 </TD> <TD align="right"> 0.0175 </TD> <TD align="right"> 0.0174 </TD> <TD align="right"> 0.0473 </TD> <TD align="right"> -0.2603 </TD> <TD align="right"> 0.2589 </TD> <TD align="right"> -0.2506 </TD> <TD align="right"> 0.2493 </TD> </TR>
  <TR> <TD align="right"> 6 </TD> <TD align="right"> 0.00 </TD> <TD> 70 </TD> <TD align="right"> 0.0478 </TD> <TD align="right"> -0.0021 </TD> <TD align="right"> -0.0021 </TD> <TD align="right"> 0.0149 </TD> <TD align="right"> 0.0149 </TD> <TD align="right"> 0.0502 </TD> <TD align="right"> -0.2416 </TD> <TD align="right"> 0.2373 </TD> <TD align="right"> -0.2338 </TD> <TD align="right"> 0.2297 </TD> </TR>
  <TR> <TD align="right"> 7 </TD> <TD align="right"> 0.10 </TD> <TD> 20 </TD> <TD align="right"> 0.1125 </TD> <TD align="right"> 0.1029 </TD> <TD align="right"> 0.0972 </TD> <TD align="right"> 0.0588 </TD> <TD align="right"> 0.0601 </TD> <TD align="right"> 0.0496 </TD> <TD align="right"> -0.3725 </TD> <TD align="right"> 0.5782 </TD> <TD align="right"> -0.3391 </TD> <TD align="right"> 0.5002 </TD> </TR>
  <TR> <TD align="right"> 8 </TD> <TD align="right"> 0.10 </TD> <TD> 30 </TD> <TD align="right"> 0.1317 </TD> <TD align="right"> 0.1038 </TD> <TD align="right"> 0.0999 </TD> <TD align="right"> 0.0370 </TD> <TD align="right"> 0.0365 </TD> <TD align="right"> 0.0510 </TD> <TD align="right"> -0.2734 </TD> <TD align="right"> 0.4810 </TD> <TD align="right"> -0.2583 </TD> <TD align="right"> 0.4346 </TD> </TR>
  <TR> <TD align="right"> 9 </TD> <TD align="right"> 0.10 </TD> <TD> 40 </TD> <TD align="right"> 0.1503 </TD> <TD align="right"> 0.1023 </TD> <TD align="right"> 0.0994 </TD> <TD align="right"> 0.0270 </TD> <TD align="right"> 0.0272 </TD> <TD align="right"> 0.0526 </TD> <TD align="right"> -0.2199 </TD> <TD align="right"> 0.4246 </TD> <TD align="right"> -0.2111 </TD> <TD align="right"> 0.3920 </TD> </TR>
  <TR> <TD align="right"> 10 </TD> <TD align="right"> 0.10 </TD> <TD> 50 </TD> <TD align="right"> 0.1717 </TD> <TD align="right"> 0.1004 </TD> <TD align="right"> 0.0980 </TD> <TD align="right"> 0.0213 </TD> <TD align="right"> 0.0216 </TD> <TD align="right"> 0.0503 </TD> <TD align="right"> -0.1855 </TD> <TD align="right"> 0.3862 </TD> <TD align="right"> -0.1797 </TD> <TD align="right"> 0.3615 </TD> </TR>
  <TR> <TD align="right"> 11 </TD> <TD align="right"> 0.10 </TD> <TD> 60 </TD> <TD align="right"> 0.1883 </TD> <TD align="right"> 0.0999 </TD> <TD align="right"> 0.0979 </TD> <TD align="right"> 0.0175 </TD> <TD align="right"> 0.0175 </TD> <TD align="right"> 0.0467 </TD> <TD align="right"> -0.1597 </TD> <TD align="right"> 0.3595 </TD> <TD align="right"> -0.1557 </TD> <TD align="right"> 0.3396 </TD> </TR>
  <TR> <TD align="right"> 12 </TD> <TD align="right"> 0.10 </TD> <TD> 70 </TD> <TD align="right"> 0.2073 </TD> <TD align="right"> 0.1031 </TD> <TD align="right"> 0.1012 </TD> <TD align="right"> 0.0149 </TD> <TD align="right"> 0.0151 </TD> <TD align="right"> 0.0521 </TD> <TD align="right"> -0.1363 </TD> <TD align="right"> 0.3426 </TD> <TD align="right"> -0.1336 </TD> <TD align="right"> 0.3254 </TD> </TR>
  <TR> <TD align="right"> 13 </TD> <TD align="right"> 0.20 </TD> <TD> 20 </TD> <TD align="right"> 0.2193 </TD> <TD align="right"> 0.2125 </TD> <TD align="right"> 0.1985 </TD> <TD align="right"> 0.0588 </TD> <TD align="right"> 0.0605 </TD> <TD align="right"> 0.0540 </TD> <TD align="right"> -0.2629 </TD> <TD align="right"> 0.6878 </TD> <TD align="right"> -0.2438 </TD> <TD align="right"> 0.5747 </TD> </TR>
  <TR> <TD align="right"> 14 </TD> <TD align="right"> 0.20 </TD> <TD> 30 </TD> <TD align="right"> 0.2787 </TD> <TD align="right"> 0.2057 </TD> <TD align="right"> 0.1962 </TD> <TD align="right"> 0.0370 </TD> <TD align="right"> 0.0369 </TD> <TD align="right"> 0.0506 </TD> <TD align="right"> -0.1715 </TD> <TD align="right"> 0.5829 </TD> <TD align="right"> -0.1641 </TD> <TD align="right"> 0.5114 </TD> </TR>
  <TR> <TD align="right"> 15 </TD> <TD align="right"> 0.20 </TD> <TD> 40 </TD> <TD align="right"> 0.3476 </TD> <TD align="right"> 0.2059 </TD> <TD align="right"> 0.1981 </TD> <TD align="right"> 0.0270 </TD> <TD align="right"> 0.0267 </TD> <TD align="right"> 0.0458 </TD> <TD align="right"> -0.1163 </TD> <TD align="right"> 0.5281 </TD> <TD align="right"> -0.1129 </TD> <TD align="right"> 0.4744 </TD> </TR>
  <TR> <TD align="right"> 16 </TD> <TD align="right"> 0.20 </TD> <TD> 50 </TD> <TD align="right"> 0.4036 </TD> <TD align="right"> 0.2044 </TD> <TD align="right"> 0.1977 </TD> <TD align="right"> 0.0213 </TD> <TD align="right"> 0.0212 </TD> <TD align="right"> 0.0504 </TD> <TD align="right"> -0.0815 </TD> <TD align="right"> 0.4903 </TD> <TD align="right"> -0.0796 </TD> <TD align="right"> 0.4470 </TD> </TR>
  <TR> <TD align="right"> 17 </TD> <TD align="right"> 0.20 </TD> <TD> 60 </TD> <TD align="right"> 0.4647 </TD> <TD align="right"> 0.2052 </TD> <TD align="right"> 0.1990 </TD> <TD align="right"> 0.0175 </TD> <TD align="right"> 0.0179 </TD> <TD align="right"> 0.0519 </TD> <TD align="right"> -0.0544 </TD> <TD align="right"> 0.4648 </TD> <TD align="right"> -0.0534 </TD> <TD align="right"> 0.4279 </TD> </TR>
  <TR> <TD align="right"> 18 </TD> <TD align="right"> 0.20 </TD> <TD> 70 </TD> <TD align="right"> 0.5085 </TD> <TD align="right"> 0.2037 </TD> <TD align="right"> 0.1982 </TD> <TD align="right"> 0.0149 </TD> <TD align="right"> 0.0148 </TD> <TD align="right"> 0.0470 </TD> <TD align="right"> -0.0357 </TD> <TD align="right"> 0.4432 </TD> <TD align="right"> -0.0352 </TD> <TD align="right"> 0.4113 </TD> </TR>
  <TR> <TD align="right"> 19 </TD> <TD align="right"> 0.30 </TD> <TD> 20 </TD> <TD align="right"> 0.3677 </TD> <TD align="right"> 0.3175 </TD> <TD align="right"> 0.2925 </TD> <TD align="right"> 0.0588 </TD> <TD align="right"> 0.0583 </TD> <TD align="right"> 0.0483 </TD> <TD align="right"> -0.1579 </TD> <TD align="right"> 0.7928 </TD> <TD align="right"> -0.1485 </TD> <TD align="right"> 0.6392 </TD> </TR>
  <TR> <TD align="right"> 20 </TD> <TD align="right"> 0.30 </TD> <TD> 30 </TD> <TD align="right"> 0.5035 </TD> <TD align="right"> 0.3169 </TD> <TD align="right"> 0.2973 </TD> <TD align="right"> 0.0370 </TD> <TD align="right"> 0.0358 </TD> <TD align="right"> 0.0482 </TD> <TD align="right"> -0.0603 </TD> <TD align="right"> 0.6941 </TD> <TD align="right"> -0.0582 </TD> <TD align="right"> 0.5873 </TD> </TR>
  <TR> <TD align="right"> 21 </TD> <TD align="right"> 0.30 </TD> <TD> 40 </TD> <TD align="right"> 0.5956 </TD> <TD align="right"> 0.3117 </TD> <TD align="right"> 0.2951 </TD> <TD align="right"> 0.0270 </TD> <TD align="right"> 0.0262 </TD> <TD align="right"> 0.0483 </TD> <TD align="right"> -0.0105 </TD> <TD align="right"> 0.6339 </TD> <TD align="right"> -0.0103 </TD> <TD align="right"> 0.5509 </TD> </TR>
  <TR> <TD align="right"> 22 </TD> <TD align="right"> 0.30 </TD> <TD> 50 </TD> <TD align="right"> 0.6917 </TD> <TD align="right"> 0.3122 </TD> <TD align="right"> 0.2968 </TD> <TD align="right"> 0.0213 </TD> <TD align="right"> 0.0212 </TD> <TD align="right"> 0.0495 </TD> <TD align="right"> 0.0263 </TD> <TD align="right"> 0.5981 </TD> <TD align="right"> 0.0257 </TD> <TD align="right"> 0.5278 </TD> </TR>
  <TR> <TD align="right"> 23 </TD> <TD align="right"> 0.30 </TD> <TD> 60 </TD> <TD align="right"> 0.7610 </TD> <TD align="right"> 0.3107 </TD> <TD align="right"> 0.2964 </TD> <TD align="right"> 0.0175 </TD> <TD align="right"> 0.0177 </TD> <TD align="right"> 0.0512 </TD> <TD align="right"> 0.0511 </TD> <TD align="right"> 0.5703 </TD> <TD align="right"> 0.0502 </TD> <TD align="right"> 0.5090 </TD> </TR>
  <TR> <TD align="right"> 24 </TD> <TD align="right"> 0.30 </TD> <TD> 70 </TD> <TD align="right"> 0.8173 </TD> <TD align="right"> 0.3110 </TD> <TD align="right"> 0.2974 </TD> <TD align="right"> 0.0149 </TD> <TD align="right"> 0.0148 </TD> <TD align="right"> 0.0468 </TD> <TD align="right"> 0.0715 </TD> <TD align="right"> 0.5504 </TD> <TD align="right"> 0.0703 </TD> <TD align="right"> 0.4954 </TD> </TR>
  <TR> <TD align="right"> 25 </TD> <TD align="right"> 0.40 </TD> <TD> 20 </TD> <TD align="right"> 0.5611 </TD> <TD align="right"> 0.4354 </TD> <TD align="right"> 0.3916 </TD> <TD align="right"> 0.0588 </TD> <TD align="right"> 0.0578 </TD> <TD align="right"> 0.0493 </TD> <TD align="right"> -0.0400 </TD> <TD align="right"> 0.9107 </TD> <TD align="right"> -0.0380 </TD> <TD align="right"> 0.7021 </TD> </TR>
  <TR> <TD align="right"> 26 </TD> <TD align="right"> 0.40 </TD> <TD> 30 </TD> <TD align="right"> 0.7237 </TD> <TD align="right"> 0.4306 </TD> <TD align="right"> 0.3939 </TD> <TD align="right"> 0.0370 </TD> <TD align="right"> 0.0371 </TD> <TD align="right"> 0.0486 </TD> <TD align="right"> 0.0534 </TD> <TD align="right"> 0.8078 </TD> <TD align="right"> 0.0514 </TD> <TD align="right"> 0.6550 </TD> </TR>
  <TR> <TD align="right"> 27 </TD> <TD align="right"> 0.40 </TD> <TD> 40 </TD> <TD align="right"> 0.8347 </TD> <TD align="right"> 0.4280 </TD> <TD align="right"> 0.3946 </TD> <TD align="right"> 0.0270 </TD> <TD align="right"> 0.0277 </TD> <TD align="right"> 0.0534 </TD> <TD align="right"> 0.1058 </TD> <TD align="right"> 0.7502 </TD> <TD align="right"> 0.1026 </TD> <TD align="right"> 0.6250 </TD> </TR>
  <TR> <TD align="right"> 28 </TD> <TD align="right"> 0.40 </TD> <TD> 50 </TD> <TD align="right"> 0.9009 </TD> <TD align="right"> 0.4266 </TD> <TD align="right"> 0.3954 </TD> <TD align="right"> 0.0213 </TD> <TD align="right"> 0.0217 </TD> <TD align="right"> 0.0508 </TD> <TD align="right"> 0.1407 </TD> <TD align="right"> 0.7125 </TD> <TD align="right"> 0.1369 </TD> <TD align="right"> 0.6041 </TD> </TR>
  <TR> <TD align="right"> 29 </TD> <TD align="right"> 0.40 </TD> <TD> 60 </TD> <TD align="right"> 0.9394 </TD> <TD align="right"> 0.4265 </TD> <TD align="right"> 0.3966 </TD> <TD align="right"> 0.0175 </TD> <TD align="right"> 0.0178 </TD> <TD align="right"> 0.0523 </TD> <TD align="right"> 0.1669 </TD> <TD align="right"> 0.6861 </TD> <TD align="right"> 0.1626 </TD> <TD align="right"> 0.5888 </TD> </TR>
  <TR> <TD align="right"> 30 </TD> <TD align="right"> 0.40 </TD> <TD> 70 </TD> <TD align="right"> 0.9677 </TD> <TD align="right"> 0.4267 </TD> <TD align="right"> 0.3977 </TD> <TD align="right"> 0.0149 </TD> <TD align="right"> 0.0149 </TD> <TD align="right"> 0.0520 </TD> <TD align="right"> 0.1873 </TD> <TD align="right"> 0.6661 </TD> <TD align="right"> 0.1825 </TD> <TD align="right"> 0.5768 </TD> </TR>
  <TR> <TD align="right"> 31 </TD> <TD align="right"> 0.50 </TD> <TD> 20 </TD> <TD align="right"> 0.7599 </TD> <TD align="right"> 0.5654 </TD> <TD align="right"> 0.4918 </TD> <TD align="right"> 0.0588 </TD> <TD align="right"> 0.0572 </TD> <TD align="right"> 0.0488 </TD> <TD align="right"> 0.0900 </TD> <TD align="right"> 1.0408 </TD> <TD align="right"> 0.0853 </TD> <TD align="right"> 0.7608 </TD> </TR>
  <TR> <TD align="right"> 32 </TD> <TD align="right"> 0.50 </TD> <TD> 30 </TD> <TD align="right"> 0.8952 </TD> <TD align="right"> 0.5563 </TD> <TD align="right"> 0.4919 </TD> <TD align="right"> 0.0370 </TD> <TD align="right"> 0.0371 </TD> <TD align="right"> 0.0482 </TD> <TD align="right"> 0.1791 </TD> <TD align="right"> 0.9335 </TD> <TD align="right"> 0.1713 </TD> <TD align="right"> 0.7198 </TD> </TR>
  <TR> <TD align="right"> 33 </TD> <TD align="right"> 0.50 </TD> <TD> 40 </TD> <TD align="right"> 0.9596 </TD> <TD align="right"> 0.5571 </TD> <TD align="right"> 0.4962 </TD> <TD align="right"> 0.0270 </TD> <TD align="right"> 0.0265 </TD> <TD align="right"> 0.0486 </TD> <TD align="right"> 0.2349 </TD> <TD align="right"> 0.8794 </TD> <TD align="right"> 0.2251 </TD> <TD align="right"> 0.6969 </TD> </TR>
  <TR> <TD align="right"> 34 </TD> <TD align="right"> 0.50 </TD> <TD> 50 </TD> <TD align="right"> 0.9834 </TD> <TD align="right"> 0.5527 </TD> <TD align="right"> 0.4946 </TD> <TD align="right"> 0.0213 </TD> <TD align="right"> 0.0216 </TD> <TD align="right"> 0.0519 </TD> <TD align="right"> 0.2668 </TD> <TD align="right"> 0.8385 </TD> <TD align="right"> 0.2556 </TD> <TD align="right"> 0.6773 </TD> </TR>
  <TR> <TD align="right"> 35 </TD> <TD align="right"> 0.50 </TD> <TD> 60 </TD> <TD align="right"> 0.9953 </TD> <TD align="right"> 0.5552 </TD> <TD align="right"> 0.4981 </TD> <TD align="right"> 0.0175 </TD> <TD align="right"> 0.0171 </TD> <TD align="right"> 0.0469 </TD> <TD align="right"> 0.2956 </TD> <TD align="right"> 0.8149 </TD> <TD align="right"> 0.2829 </TD> <TD align="right"> 0.6660 </TD> </TR>
  <TR> <TD align="right"> 36 </TD> <TD align="right"> 0.50 </TD> <TD> 70 </TD> <TD align="right"> 0.9973 </TD> <TD align="right"> 0.5530 </TD> <TD align="right"> 0.4972 </TD> <TD align="right"> 0.0149 </TD> <TD align="right"> 0.0150 </TD> <TD align="right"> 0.0503 </TD> <TD align="right"> 0.3135 </TD> <TD align="right"> 0.7924 </TD> <TD align="right"> 0.2996 </TD> <TD align="right"> 0.6542 </TD> </TR>
  <TR> <TD align="right"> 37 </TD> <TD align="right"> 0.60 </TD> <TD> 20 </TD> <TD align="right"> 0.9034 </TD> <TD align="right"> 0.7133 </TD> <TD align="right"> 0.5912 </TD> <TD align="right"> 0.0588 </TD> <TD align="right"> 0.0595 </TD> <TD align="right"> 0.0516 </TD> <TD align="right"> 0.2380 </TD> <TD align="right"> 1.1887 </TD> <TD align="right"> 0.2216 </TD> <TD align="right"> 0.8149 </TD> </TR>
  <TR> <TD align="right"> 38 </TD> <TD align="right"> 0.60 </TD> <TD> 30 </TD> <TD align="right"> 0.9774 </TD> <TD align="right"> 0.7048 </TD> <TD align="right"> 0.5937 </TD> <TD align="right"> 0.0370 </TD> <TD align="right"> 0.0369 </TD> <TD align="right"> 0.0500 </TD> <TD align="right"> 0.3276 </TD> <TD align="right"> 1.0820 </TD> <TD align="right"> 0.3065 </TD> <TD align="right"> 0.7832 </TD> </TR>
  <TR> <TD align="right"> 39 </TD> <TD align="right"> 0.60 </TD> <TD> 40 </TD> <TD align="right"> 0.9951 </TD> <TD align="right"> 0.7019 </TD> <TD align="right"> 0.5953 </TD> <TD align="right"> 0.0270 </TD> <TD align="right"> 0.0274 </TD> <TD align="right"> 0.0527 </TD> <TD align="right"> 0.3797 </TD> <TD align="right"> 1.0242 </TD> <TD align="right"> 0.3542 </TD> <TD align="right"> 0.7631 </TD> </TR>
  <TR> <TD align="right"> 40 </TD> <TD align="right"> 0.60 </TD> <TD> 50 </TD> <TD align="right"> 0.9986 </TD> <TD align="right"> 0.6978 </TD> <TD align="right"> 0.5950 </TD> <TD align="right"> 0.0213 </TD> <TD align="right"> 0.0211 </TD> <TD align="right"> 0.0507 </TD> <TD align="right"> 0.4119 </TD> <TD align="right"> 0.9837 </TD> <TD align="right"> 0.3833 </TD> <TD align="right"> 0.7478 </TD> </TR>
  <TR> <TD align="right"> 41 </TD> <TD align="right"> 0.60 </TD> <TD> 60 </TD> <TD align="right"> 0.9998 </TD> <TD align="right"> 0.6982 </TD> <TD align="right"> 0.5966 </TD> <TD align="right"> 0.0175 </TD> <TD align="right"> 0.0174 </TD> <TD align="right"> 0.0508 </TD> <TD align="right"> 0.4386 </TD> <TD align="right"> 0.9578 </TD> <TD align="right"> 0.4066 </TD> <TD align="right"> 0.7375 </TD> </TR>
  <TR> <TD align="right"> 42 </TD> <TD align="right"> 0.60 </TD> <TD> 70 </TD> <TD align="right"> 1.0000 </TD> <TD align="right"> 0.6979 </TD> <TD align="right"> 0.5973 </TD> <TD align="right"> 0.0149 </TD> <TD align="right"> 0.0151 </TD> <TD align="right"> 0.0526 </TD> <TD align="right"> 0.4585 </TD> <TD align="right"> 0.9374 </TD> <TD align="right"> 0.4237 </TD> <TD align="right"> 0.7289 </TD> </TR>
  <TR> <TD align="right"> 43 </TD> <TD align="right"> 0.70 </TD> <TD> 20 </TD> <TD align="right"> 0.9777 </TD> <TD align="right"> 0.8854 </TD> <TD align="right"> 0.6895 </TD> <TD align="right"> 0.0588 </TD> <TD align="right"> 0.0576 </TD> <TD align="right"> 0.0495 </TD> <TD align="right"> 0.4101 </TD> <TD align="right"> 1.3608 </TD> <TD align="right"> 0.3711 </TD> <TD align="right"> 0.8647 </TD> </TR>
  <TR> <TD align="right"> 44 </TD> <TD align="right"> 0.70 </TD> <TD> 30 </TD> <TD align="right"> 0.9979 </TD> <TD align="right"> 0.8781 </TD> <TD align="right"> 0.6925 </TD> <TD align="right"> 0.0370 </TD> <TD align="right"> 0.0372 </TD> <TD align="right"> 0.0516 </TD> <TD align="right"> 0.5009 </TD> <TD align="right"> 1.2553 </TD> <TD align="right"> 0.4499 </TD> <TD align="right"> 0.8409 </TD> </TR>
  <TR> <TD align="right"> 45 </TD> <TD align="right"> 0.70 </TD> <TD> 40 </TD> <TD align="right"> 1.0000 </TD> <TD align="right"> 0.8763 </TD> <TD align="right"> 0.6953 </TD> <TD align="right"> 0.0270 </TD> <TD align="right"> 0.0265 </TD> <TD align="right"> 0.0497 </TD> <TD align="right"> 0.5541 </TD> <TD align="right"> 1.1985 </TD> <TD align="right"> 0.4939 </TD> <TD align="right"> 0.8265 </TD> </TR>
  <TR> <TD align="right"> 46 </TD> <TD align="right"> 0.70 </TD> <TD> 50 </TD> <TD align="right"> 1.0000 </TD> <TD align="right"> 0.8751 </TD> <TD align="right"> 0.6965 </TD> <TD align="right"> 0.0213 </TD> <TD align="right"> 0.0211 </TD> <TD align="right"> 0.0517 </TD> <TD align="right"> 0.5892 </TD> <TD align="right"> 1.1610 </TD> <TD align="right"> 0.5215 </TD> <TD align="right"> 0.8157 </TD> </TR>
  <TR> <TD align="right"> 47 </TD> <TD align="right"> 0.70 </TD> <TD> 60 </TD> <TD align="right"> 1.0000 </TD> <TD align="right"> 0.8741 </TD> <TD align="right"> 0.6973 </TD> <TD align="right"> 0.0175 </TD> <TD align="right"> 0.0176 </TD> <TD align="right"> 0.0518 </TD> <TD align="right"> 0.6145 </TD> <TD align="right"> 1.1337 </TD> <TD align="right"> 0.5407 </TD> <TD align="right"> 0.8074 </TD> </TR>
  <TR> <TD align="right"> 48 </TD> <TD align="right"> 0.70 </TD> <TD> 70 </TD> <TD align="right"> 1.0000 </TD> <TD align="right"> 0.8728 </TD> <TD align="right"> 0.6974 </TD> <TD align="right"> 0.0149 </TD> <TD align="right"> 0.0152 </TD> <TD align="right"> 0.0538 </TD> <TD align="right"> 0.6333 </TD> <TD align="right"> 1.1122 </TD> <TD align="right"> 0.5546 </TD> <TD align="right"> 0.8005 </TD> </TR>
  <TR> <TD align="right"> 49 </TD> <TD align="right"> 0.80 </TD> <TD> 20 </TD> <TD align="right"> 0.9990 </TD> <TD align="right"> 1.1186 </TD> <TD align="right"> 0.7915 </TD> <TD align="right"> 0.0588 </TD> <TD align="right"> 0.0557 </TD> <TD align="right"> 0.0432 </TD> <TD align="right"> 0.6432 </TD> <TD align="right"> 1.5940 </TD> <TD align="right"> 0.5469 </TD> <TD align="right"> 0.9128 </TD> </TR>
  <TR> <TD align="right"> 50 </TD> <TD align="right"> 0.80 </TD> <TD> 30 </TD> <TD align="right"> 1.0000 </TD> <TD align="right"> 1.1124 </TD> <TD align="right"> 0.7947 </TD> <TD align="right"> 0.0370 </TD> <TD align="right"> 0.0361 </TD> <TD align="right"> 0.0487 </TD> <TD align="right"> 0.7352 </TD> <TD align="right"> 1.4896 </TD> <TD align="right"> 0.6129 </TD> <TD align="right"> 0.8972 </TD> </TR>
  <TR> <TD align="right"> 51 </TD> <TD align="right"> 0.80 </TD> <TD> 40 </TD> <TD align="right"> 1.0000 </TD> <TD align="right"> 1.1080 </TD> <TD align="right"> 0.7957 </TD> <TD align="right"> 0.0270 </TD> <TD align="right"> 0.0270 </TD> <TD align="right"> 0.0517 </TD> <TD align="right"> 0.7858 </TD> <TD align="right"> 1.4302 </TD> <TD align="right"> 0.6461 </TD> <TD align="right"> 0.8867 </TD> </TR>
  <TR> <TD align="right"> 52 </TD> <TD align="right"> 0.80 </TD> <TD> 50 </TD> <TD align="right"> 1.0000 </TD> <TD align="right"> 1.1089 </TD> <TD align="right"> 0.7976 </TD> <TD align="right"> 0.0213 </TD> <TD align="right"> 0.0213 </TD> <TD align="right"> 0.0490 </TD> <TD align="right"> 0.8230 </TD> <TD align="right"> 1.3948 </TD> <TD align="right"> 0.6690 </TD> <TD align="right"> 0.8801 </TD> </TR>
  <TR> <TD align="right"> 53 </TD> <TD align="right"> 0.80 </TD> <TD> 60 </TD> <TD align="right"> 1.0000 </TD> <TD align="right"> 1.1058 </TD> <TD align="right"> 0.7976 </TD> <TD align="right"> 0.0175 </TD> <TD align="right"> 0.0174 </TD> <TD align="right"> 0.0484 </TD> <TD align="right"> 0.8462 </TD> <TD align="right"> 1.3654 </TD> <TD align="right"> 0.6829 </TD> <TD align="right"> 0.8741 </TD> </TR>
  <TR> <TD align="right"> 54 </TD> <TD align="right"> 0.80 </TD> <TD> 70 </TD> <TD align="right"> 1.0000 </TD> <TD align="right"> 1.1038 </TD> <TD align="right"> 0.7976 </TD> <TD align="right"> 0.0149 </TD> <TD align="right"> 0.0150 </TD> <TD align="right"> 0.0514 </TD> <TD align="right"> 0.8644 </TD> <TD align="right"> 1.3433 </TD> <TD align="right"> 0.6932 </TD> <TD align="right"> 0.8693 </TD> </TR>
  <TR> <TD align="right"> 55 </TD> <TD align="right"> 0.90 </TD> <TD> 20 </TD> <TD align="right"> 0.9999 </TD> <TD align="right"> 1.4992 </TD> <TD align="right"> 0.8952 </TD> <TD align="right"> 0.0588 </TD> <TD align="right"> 0.0587 </TD> <TD align="right"> 0.0510 </TD> <TD align="right"> 1.0239 </TD> <TD align="right"> 1.9746 </TD> <TD align="right"> 0.7535 </TD> <TD align="right"> 0.9578 </TD> </TR>
  <TR> <TD align="right"> 56 </TD> <TD align="right"> 0.90 </TD> <TD> 30 </TD> <TD align="right"> 1.0000 </TD> <TD align="right"> 1.4886 </TD> <TD align="right"> 0.8969 </TD> <TD align="right"> 0.0370 </TD> <TD align="right"> 0.0365 </TD> <TD align="right"> 0.0503 </TD> <TD align="right"> 1.1114 </TD> <TD align="right"> 1.8658 </TD> <TD align="right"> 0.7942 </TD> <TD align="right"> 0.9499 </TD> </TR>
  <TR> <TD align="right"> 57 </TD> <TD align="right"> 0.90 </TD> <TD> 40 </TD> <TD align="right"> 1.0000 </TD> <TD align="right"> 1.4874 </TD> <TD align="right"> 0.8984 </TD> <TD align="right"> 0.0270 </TD> <TD align="right"> 0.0267 </TD> <TD align="right"> 0.0495 </TD> <TD align="right"> 1.1652 </TD> <TD align="right"> 1.8096 </TD> <TD align="right"> 0.8157 </TD> <TD align="right"> 0.9452 </TD> </TR>
  <TR> <TD align="right"> 58 </TD> <TD align="right"> 0.90 </TD> <TD> 50 </TD> <TD align="right"> 1.0000 </TD> <TD align="right"> 1.4816 </TD> <TD align="right"> 0.8982 </TD> <TD align="right"> 0.0213 </TD> <TD align="right"> 0.0212 </TD> <TD align="right"> 0.0502 </TD> <TD align="right"> 1.1957 </TD> <TD align="right"> 1.7675 </TD> <TD align="right"> 0.8269 </TD> <TD align="right"> 0.9411 </TD> </TR>
  <TR> <TD align="right"> 59 </TD> <TD align="right"> 0.90 </TD> <TD> 60 </TD> <TD align="right"> 1.0000 </TD> <TD align="right"> 1.4812 </TD> <TD align="right"> 0.8988 </TD> <TD align="right"> 0.0175 </TD> <TD align="right"> 0.0173 </TD> <TD align="right"> 0.0498 </TD> <TD align="right"> 1.2216 </TD> <TD align="right"> 1.7408 </TD> <TD align="right"> 0.8358 </TD> <TD align="right"> 0.9384 </TD> </TR>
  <TR> <TD align="right"> 60 </TD> <TD align="right"> 0.90 </TD> <TD> 70 </TD> <TD align="right"> 1.0000 </TD> <TD align="right"> 1.4785 </TD> <TD align="right"> 0.8987 </TD> <TD align="right"> 0.0149 </TD> <TD align="right"> 0.0147 </TD> <TD align="right"> 0.0476 </TD> <TD align="right"> 1.2390 </TD> <TD align="right"> 1.7179 </TD> <TD align="right"> 0.8416 </TD> <TD align="right"> 0.9359 </TD> </TR>
  <TR> <TD align="right"> 61 </TD> <TD align="right"> 0.99 </TD> <TD> 20 </TD> <TD align="right"> 1.0000 </TD> <TD align="right"> 2.6723 </TD> <TD align="right"> 0.9893 </TD> <TD align="right"> 0.0588 </TD> <TD align="right"> 0.0578 </TD> <TD align="right"> 0.0515 </TD> <TD align="right"> 2.1970 </TD> <TD align="right"> 3.1477 </TD> <TD align="right"> 0.9727 </TD> <TD align="right"> 0.9959 </TD> </TR>
  <TR> <TD align="right"> 62 </TD> <TD align="right"> 0.99 </TD> <TD> 30 </TD> <TD align="right"> 1.0000 </TD> <TD align="right"> 2.6670 </TD> <TD align="right"> 0.9897 </TD> <TD align="right"> 0.0370 </TD> <TD align="right"> 0.0368 </TD> <TD align="right"> 0.0516 </TD> <TD align="right"> 2.2898 </TD> <TD align="right"> 3.0442 </TD> <TD align="right"> 0.9782 </TD> <TD align="right"> 0.9951 </TD> </TR>
  <TR> <TD align="right"> 63 </TD> <TD align="right"> 0.99 </TD> <TD> 40 </TD> <TD align="right"> 1.0000 </TD> <TD align="right"> 2.6604 </TD> <TD align="right"> 0.9897 </TD> <TD align="right"> 0.0270 </TD> <TD align="right"> 0.0267 </TD> <TD align="right"> 0.0488 </TD> <TD align="right"> 2.3381 </TD> <TD align="right"> 2.9826 </TD> <TD align="right"> 0.9806 </TD> <TD align="right"> 0.9946 </TD> </TR>
  <TR> <TD align="right"> 64 </TD> <TD align="right"> 0.99 </TD> <TD> 50 </TD> <TD align="right"> 1.0000 </TD> <TD align="right"> 2.6585 </TD> <TD align="right"> 0.9898 </TD> <TD align="right"> 0.0213 </TD> <TD align="right"> 0.0210 </TD> <TD align="right"> 0.0494 </TD> <TD align="right"> 2.3726 </TD> <TD align="right"> 2.9443 </TD> <TD align="right"> 0.9820 </TD> <TD align="right"> 0.9942 </TD> </TR>
  <TR> <TD align="right"> 65 </TD> <TD align="right"> 0.99 </TD> <TD> 60 </TD> <TD align="right"> 1.0000 </TD> <TD align="right"> 2.6539 </TD> <TD align="right"> 0.9898 </TD> <TD align="right"> 0.0175 </TD> <TD align="right"> 0.0174 </TD> <TD align="right"> 0.0495 </TD> <TD align="right"> 2.3943 </TD> <TD align="right"> 2.9135 </TD> <TD align="right"> 0.9829 </TD> <TD align="right"> 0.9939 </TD> </TR>
  <TR> <TD align="right"> 66 </TD> <TD align="right"> 0.99 </TD> <TD> 70 </TD> <TD align="right"> 1.0000 </TD> <TD align="right"> 2.6553 </TD> <TD align="right"> 0.9899 </TD> <TD align="right"> 0.0149 </TD> <TD align="right"> 0.0150 </TD> <TD align="right"> 0.0506 </TD> <TD align="right"> 2.4158 </TD> <TD align="right"> 2.8947 </TD> <TD align="right"> 0.9837 </TD> <TD align="right"> 0.9937 </TD> </TR>
   </TABLE>


En primer lugar, se comprueba que las muestras extraídas convergen hacia la correlación teórica. Esto es, se comprueba que para cada condición de simulación la media de las 10,000 correlaciones generadas se aproxima al valor de $\rho$. El siguiente gráfico muestra el ajuste: 

<img src="figure/corr_plot.png" title="plot of chunk corr.plot" alt="plot of chunk corr.plot" style="display: block; margin: auto;" />


La diferencia máxima entre la correlación teórica $\rho$ y la empírica es de 0.0105.

Con la hipótesis nula verdadera ($\rho = 0$), la tasa empírica de rechazos (EPR) se mantiene alrededor del valor nominal de 0,05. El valor de la Z de Fisher se mantiene alrededor de 0. Para los distintos valores de $\rho$ cuando la hipótesis nula es falsa, la tasa empírica de rechazos (es decir, la potencia) aumenta a medida que se incrementa el tamaño de la muestra *n* y el valor de la correlación poblacional. 

Los intervalos de confianza incluyen el valor del parámetro $\rho$ aproximadamente en el 95% de los casos, con aparente independencia del valor de $\rho$ y del tamaño de la muestra. 

La gráfica siguiente muestra la tasa empírica de rechazos con la regla fija: 

<img src="figure/graf_epr.png" title="plot of chunk graf_epr" alt="plot of chunk graf_epr" style="display: block; margin: auto;" />


Los siguientes dos gráficos muestran las varianzas de la Z de Fisher, la teórica (que será constante para todo $\rho$) y la empírica. En general, la varianza empírica de Z se ajusta bastante bien a la varianza teórica para todos los tamaños muestrales. 

<img src="figure/varianzas_z.png" title="plot of chunk varianzas_z" alt="plot of chunk varianzas_z" style="display: block; margin: auto;" />


La cobertura de los intervalos de confianza para la regla del tamaño prefijado se muestra en el gráfico siguiente. Empleando como estadístico la Z transformada de Fisher, se obtiene un intervalo de confianza simétrico centrado en dicha Z. Si en cambio los intervalos de confianza se vuelven a transformar a correlaciones de Pearson, dichos intervalos ya no son simétricos (dado que la transformación Z no es una transformación lineal).

<img src="figure/cobertura11.png" title="plot of chunk cobertura1" alt="plot of chunk cobertura1" style="display: block; margin: auto;" /><img src="figure/cobertura12.png" title="plot of chunk cobertura1" alt="plot of chunk cobertura1" style="display: block; margin: auto;" />


Globalmente, la cobertura de los intervalos de confianza se ajusta bastante bien al valor nominal de 0,95. 

<img src="figure/cobertura2.png" title="plot of chunk cobertura2" alt="plot of chunk cobertura2" style="display: block; margin: auto;" />


Simulación con la regla CLAST
----------------------------------------------------------

La regla CLAST es un procedimiento para contrastar hipótesis mediante "data peeking" de forma que se mantenga bajo control el error tipo I. Se fija en primer lugar el tamaño de la muestra necesario (tamaño de referencia o *nfsr*) para detectar un determinado tamaño del efecto con una potencia deseada. Se toma entonces la mitad de dicho tamaño muestral y se realiza un contraste de hipótesis con dicha muestra. Se definen tres zonas en la distribución del estadístico de contraste: una zona de rechazo, una zona de mantenimiento, y una zona intermedia de incertidumbre. Si el estadístico de contraste cae en esta región, se añade un elemento a la muestra y se realiza un nuevo contraste. Este procedimiento se itera, bien hasta que se alcanza una zona de decisión (rechazo o mantenimiento), o un tamaño muestral máximo igual a 1.5 veces el tamaño de referencia. En este caso, el procedimiento se detiene y se toma una decisión con un alfa convencional de 0.05. 

### Estructura de la regla CLAST


```r
function(n, r, asup, ainf = 0.005) {
    require(MASS)
    muestra <- mvrnorm(n/2, Sigma = matrix(c(1, r, r, 1), 2), mu = rep(0, 2))
    out.cor <- cor(muestra[, 1], muestra[, 2])
    tstat <- out.cor * sqrt(dim(muestra)[1] - 2)/sqrt(1 - out.cor^2)
    pvalue <- 1 - pt(tstat, dim(muestra)[1] - 2)
    while (pvalue < asup && pvalue > ainf) {
        muestra <- rbind(muestra, mvrnorm(1, Sigma = matrix(c(1, r, r, 1), 2), 
            mu = rep(0, 2)))
        out.cor <- cor(muestra[, 1], muestra[, 2])
        tstat <- out.cor * sqrt(n - 2)/sqrt(1 - out.cor^2)
        pvalue <- 1 - pt(tstat, dim(muestra)[1] - 2)
        if (dim(muestra)[1] == n * 1.5) {
            decision <- ifelse(pvalue < 0.05, 1, 0)
            break
        }
    }
    if (dim(muestra)[1] < n * 1.5) {
        decision <- ifelse(pvalue < 0.01, 1, 0)
    }
    return(c(dim(muestra)[1], out.cor, tstat, pvalue, decision))
}

```


La estructura de CLAST es similar a la RTP, con la salvedad de que en caso de que el estadístico de contraste caiga en una región de incertidumbre definida entre $\alpha_s$ y $\alpha_i$ se añaden un par de elementos a la muestra y se realiza un nuevo contraste. 

<img src="figure/regiones_clast.png" title="plot of chunk regiones_clast" alt="plot of chunk regiones_clast" style="display: block; margin: auto;" />




