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
Z = 0.5 \left(  \ln \frac {1 + r} {1 - r} \right)
$$

En meta-análisis es común ponderar la Z de Fisher obtenida en un estudio primario por el inverso de su varianza. Dicha varianza viene dada por:

$$latex
var(Z) = \frac{1}{n_i - 3}
$$

Esta varianza puede ser considerada la varianza teórica de Z, esto es, la varianza estimada a partir del tamaño de la muestra. Por el contrario, en simulación es posible calcular la varianza empírica de Z, como la varianza de los valores de Z calculados en cada una de las simulaciones. Se puede así valorar si esta estimación converge hacia el valor de la varianza empírica. 

Se evalúa además la tasa empírica de rechazos de la $H_0$ a lo largo de las 10,000 simulaciones en cada condición. Cuando $H_0$ es verdadera (es decir, cuando $\rho = 0$), dicha tasa corresponde a la probabilidad de error tipo I. Cuando es falsa ($\rho > 0$), corresponde a la potencia de la prueba. 

Se calculan también los intervalos de confianza con $\alpha = 0.05$ para los valores de $\rho$. Dichos valores se calculan a partir de las transformaciones Z mediante la fórmula: 

$$latex
IC_{95} = Z \pm \frac{1.96} {\sqrt{n - 3}}
$$


- La cobertura de los intervalos de confianza al 95%
