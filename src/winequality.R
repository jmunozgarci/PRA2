#' ---
#' title: "Práctica 2"
#' author: "Javier Muñoz García"
#' date: '`r format(Sys.Date(),"%e de %B, %Y")`'
#' output:
#'   pdf_document:
#'     toc: yes
#'   html_document:
#'     toc: yes
#' ---
#' 
## ----setup, include=FALSE------------------------------------------------
knitr::opts_chunk$set(echo = TRUE)

#' 
## ----load_libraries, include=FALSE---------------------------------------
library(knitr)

#' # Descripción del dataset
#' Para la realización de esta práctica se ha escogido el conjunto de datos propuesto en el enunciado compuesto por 1.599 observaciones y 12 variables.
#' 
#' Este conjunto de datos está relacionado con las variantes tintas del vino portugués "Vinho Verde". En el conjunto de datos solo están disponibles las variables físico-químicas como entradas y la calidad del vino (vairable sensorial) como salida.
#' 
#' Respecto a diferentes características sobre del vino. En concreto, las características son:
#' 
#' * **fixed acidity:** la mayoría de los ácidos involucrados con el vino o fijos o no volátiles (no se evaporan fácilmente).
#' * **volatile acidity:** la cantidad de ácido acético en el vino, que en niveles demasiado altos puede producir un sabor desagradable a vinagre ácido cítrico.
#' * **citric acid:** se encuentra en pequeñas cantidades, puede añadir "frescura" y sabor a los vinos.
#' * **residual sugar:** la cantidad de azúcar que queda después de la parada de la fermentación, es raro encontrar vinos con menos de 1 gramo/litro y los vinos con más de 45 gramos/litro se consideran dulces.
#' * **chlorides:** la cantidad de sal en el vino.
#' * **free sulfur dioxide:** la forma libre de SO2 existe en equilibrio entre el SO2 molecular (como gas disuelto) y el ión bisulfito; impide el crecimiento microbiano y la oxidación del vino.
#' * **total sulfur dioxide:** cantidad de formas libres y ligadas de S02; en bajas concentraciones, el SO2 es mayormente indetectable en el vino, pero en concentraciones libres de SO2 superiores a 50 ppm, el SO2 se hace evidente en la nariz y el sabor del vino.
#' * **density:** la densidad del agua se aproxima a la del agua dependiendo del porcentaje de alcohol y del contenido de azúcar.
#' * **pH:** describe cuán ácido o básico es un vino en una escala de 0 (muy ácido) a 14 (muy básico); la mayoría de los vinos están entre 3-4 en la escala de pH.
#' * **sulphates:** un aditivo para el vino que puede contribuir a los niveles de dióxido de azufre (S02), que actúa como antimicrobiano y antioxidante.
#' * **alcohol:** el porcentaje de contenido de alcohol del vino.
#' * **quality:** puntuación subjetiva del vino (valor de 0 a 10). 
#' 
#' El dataset está propuesto plantea la posiblidad de conocer la calidad de un vino en base a sus características, así como determinar que variables influyen más en la calidad. Bajo este supuesto, se podría clasificar un vino en base a su calidad sin la necesidad de realizar una cata con su posterior calificación humana.
#' 
#' # Integración y selección de los datos de interés a analizar
#' Para comenzar, leeremos el fichero 'winequality.red' descargado de la web de Kaggle y revisaremos los atributos cargados.
## ----read----------------------------------------------------------------
# Carga de datos
winequality.red <- read.csv("C:/Users/Javier/Desktop/PRA2/winequality-red.csv")

# Comprobación de datos cargados
head(winequality.red)

# Tipos de variables
types <- sapply(winequality.red,class)
kable(data.frame(Variables = names(types), Clase = as.vector(types)))

#' 
#' Como podemos observar, todos son atributos númericos y válidos para intentar clasificar la calidad del vino, ya que a priori no conocemos que características o combinación de ellas son adecuadas para elevar la calidad del vino.
#' 
#' # Limpieza de los datos
#' ## Elementos vacíos
#' Una vez hemos cargado los datos, procedemos a realizar la limpieza del mismo, lo primero será revisar valores nulos o falta de información en algún registro de nuestro conjunto de datos.
## ----clean---------------------------------------------------------------
# Búsqueda de valores nulos
nas <- sapply(winequality.red, function(x) sum(is.na(x)))
kable(data.frame(Variables = names(nas), NAs = as.vector(nas)))

#' 
#' Así pues, dado que no hay ningún valor nulo, podemos saltarnos esta parta y revisar si hay algún dato demasiado extremo realizando una búsqueda de los mismos.
#' 
#' ## Valores extremos
## ----outliers------------------------------------------------------------
# Búsqueda de valores extremos
outliers <- sapply(winequality.red, function(x) paste(boxplot.stats(x)$out,collapse=" "))
kable(data.frame(variables=names(outliers),clase=as.vector(outliers)))

summary(winequality.red)

#' Tras el cálculo de outliers o valores extremos realizados por R, vemos que hay un gran número de ellos en todas las variables, sin embargo, no se trata de valores extremos a eliminar o modificar ya que son valores reales de las características de vinos determinados, aún teniendo valores extremos se trata de valores posibles si lo comparamos con el resumen de los datos extraídos, por lo que estos valoren pueden ser los que afecten a nuestro valor objetivo, decidimos dejarlos.
#' 
#' # Análisis de los datos
#' ## Comprobación de normalidad
#' Procedemos a comprobar la normalidad de las características que tenemos, para ello usaremos la prueba de la normalidad de Shapiro Wilk y revisaremos la gráfica quantile-quantile de las características clasificadas como no normales.
## ----norm----------------------------------------------------------------
par(mfrow = c(2, 2))
for (i in 1:ncol(winequality.red)) {
		if (shapiro.test(winequality.red[,i])$p.value < 0.05) {
			qqnorm(winequality.red[,i], main = colnames(winequality.red)[i])
			qqline(winequality.red[,i], col = "red")
			hist(winequality.red[,i], main = colnames(winequality.red)[i], xlab = "Values", freq = FALSE)
		}
}

#' 
#' Consultando el teorema del limite central, es posible aproximar nuestras variables a una distribución normal de media 0 y desviación 1 cuando se tienen más de 30 elementos, nuestro conjunto de datos tiene 1599 observaciones por lo que podemos aplicar el teorema.
#' 
#' ## Aplicación de pruebas estadísticas
#' Nuestro objetivo es intentar conocer la calidad de un vino en base a sus características, pero realmente no conocemos demasiado sobre las mismas, con la descripción del dataset, podemos intuir algunas características *importantes* como el ácido volátil (que puede afectar negativamente al vino en cantidad alta) o el ácido cítrico, que proporciona 'frescura' al vino afectando positivamente al mismo.
#' 
#' Para continuar nuestro estudio, realizaremos una pequeña prueba con el coeficiente de Spearman para ver el grado de correlación de las variables.
## ----correlation---------------------------------------------------------
values <- character(length(colnames(winequality.red))-1)
for (i in 1:(ncol(winequality.red)-1)) {
	values[i] = cor.test(winequality.red[,i], winequality.red[,length(winequality.red)], method = "spearman", exact = F)$estimate
}
kable(data.frame(Variables = colnames(winequality.red[1:(length(winequality.red)-1)]), Estimate = values))

#' 
#' Como se predecía, no solo las variables que se indicaban afectan a la calidad del vino, hemos descubierto que la variable que afecta más a la calidad del mismo es el alcohol y los sulfatos de manera positiva y como bien comentabamos antes, la que más lo perjudica es la ácidez volátil.
#' 
#' Ahora realizaremos una regresión líneal múltiple para determinar si es posible predecir la calidad del vino en base a las variables que disponemos, en primer lugar, separaremos nuestro conjunto de datos en dos, para el entrenamiento y prueba del modelo.
## ----traintest-----------------------------------------------------------
# Establecemos una semilla para separar ambos conjuntos de datos aleatoriamente
set.seed(1)
separator = sample(1:nrow(winequality.red), size = nrow(winequality.red)*0.8)
# Separamos ambos conjuntos
wine_train = winequality.red[separator,]
wine_test = winequality.red[-separator,]

#' 
#' Y creamos nuestro modelo en función de las *mejores* variables descubiertas anteriormente.
#' 
## ----lmodel--------------------------------------------------------------
# Creamos un modelo de regresión lineal múltiple con las variables de más importancia
regModel = lm(quality~alcohol+volatile.acidity+sulphates, data = wine_train)
summary(regModel)

#' 
#' Ahora, como segunda aproximación, intentaremos abarcar el mismo problema pero con un método de clasificación, para ello crearemos una variable objetivo categórica e intentaremos clasificar los vinos entre tres etiquetas de calidad con un árbol de decisión simple.
#' 
## ----clasification-------------------------------------------------------
# Creación de variable categórica
winequality.red[,"label"] <- cut(winequality.red$quality, breaks = c(3,5,6,8), labels = c("LOW", "MEDIUM", "HIGH"))

# Separación de variables y variable objetivo
X = winequality.red[,1:11]
y = winequality.red[,13]

# Establecemos una semilla para separar ambos conjuntos de datos aleatoriamente
set.seed(1)
separator = sample(1:nrow(winequality.red), size = nrow(winequality.red)*0.8)
# Separamos ambos conjuntos
trainX = X[separator,]
trainy = y[separator]
testX = X[-separator,]
testy = y[-separator]

#Creación del árbol 
library(C50)
model <- C50::C5.0(trainX,trainy)

#Extracción de reglas y resultados 
summary(model)

#' 
#' # Representación de resultados
#' Para reprentar los resultados, utilizaremos ambos conjuntos de test generados para este momento, aplicaremos los modelos creados con estos nuevos conjuntos y revisaremos si los modelos son fiables.
## ----lmpredict-----------------------------------------------------------
# Aplicación de modelo creado a conjunto de test
predicted = predict(regModel, wine_test, type = "response")
realVsPred = data.frame(real = wine_test$quality,
                        predecido = predicted,
                        diferencia = wine_test$quality - predicted)
head(realVsPred)

# Media de la diferencia en las predicciones
print(mean(realVsPred$diferencia))

#' 
## ----clasipredict--------------------------------------------------------
#Prueba del modelo de clasificación
p <- predict(model, testX, type="class") 
hit = 0
for (i in 1:length(testX)) {
	if (p[i] == testy[i]) {
		hit = hit + 1
	}
}
hit/length(testX)

#' 
#' # Resolución del problema
#' 
#' Como vemos, en ambos modelos tenemos un porcentaje muy alto de aciertos con lo que podemos concluir que si es posible conocer la calidad de un vino (o al menos una nota subjetiva real) a partir de sus características físico-químicas.
