# Librerías a utilizar

suppressWarnings({
  suppressMessages(library(paqueteMODELOS))
  suppressMessages(library(tidyverse))
  suppressMessages(library(mice))
  suppressMessages(library(plotly))
  suppressMessages(library(kableExtra))
  suppressMessages(library(caret))
  suppressMessages(library(ROCR))
  suppressMessages(library(pROC))
  suppressMessages(library(glmnet))
  suppressMessages(library(lmtest))
  suppressMessages(library(equatiomatic))
})
# Cargar la base de datos
data("rotacion")
glimpse(rotacion)
#Cambiar nombre de la variable Viaje de Negocios
rotacion <- rotacion %>%
  rename(Viaje_negocios = `Viaje de Negocios`)

### 2. Análisis univariado.
resumen_rotacion <- table(rotacion$Rotación) # Crear un resumen de la frecuencia de la variable "Rotación"
df_rotacion <- as.data.frame(resumen_rotacion) # Convertir el resumen en un data frame
colores_pasteles <- c("#a62520", "#ffbeae") # Colores
colnames(df_rotacion) <- c("Rotacion", "Frecuencia") # Renombrar las columnas
# Crear el gráfico de torta interactivo
plot_ly(data = df_rotacion, labels = ~Rotacion, values = ~Frecuencia, type = 'pie',marker = list(colors = colores_pasteles)) %>%
  layout(title = "Porcentaje de Rotación",
         xaxis = list(showgrid = FALSE, zeroline = FALSE, showticklabels = FALSE),
         yaxis = list(showgrid = FALSE, zeroline = FALSE, showticklabels = FALSE))

## Variable Viaje de Negocios.

resumen_viajes <- table(rotacion$Viaje_negocios) # Crear un resumen de la frecuencia de la variable "Viaje de Negocios"
df_viajes <- as.data.frame(resumen_viajes) # Convertir el resumen en un data frame
colnames(df_viajes) <- c("Viaje", "Frecuencia") # Renombrar las columnas
# Crear el gráfico de barras con ggplot2
grafico_ggplot <- ggplot(df_viajes, aes(x = Viaje, y = Frecuencia, fill = Viaje)) +
  geom_bar(stat = "identity") +
  labs(title = "Frecuencia de Viajes de Negocios",
       x = "¿Cada cuánto viaja?",
       y = "Frecuencia") +
  theme_minimal()
grafico_plotly <- ggplotly(grafico_ggplot) # Convertir el gráfico de ggplot2 a plotly
grafico_plotly # Mostrar el gráfico interactivo de plotly

## Creación tabla con fecuencia absoluta y porcentaje

frecuencia_viaje_negocios <- table(rotacion$Viaje_negocios) # Calcular la frecuencia absoluta de cada categoría
porcentaje_viaje_negocios <- prop.table(frecuencia_viaje_negocios) * 100 # Calcular el porcentaje de cada categoría respecto al total
moda_viaje_negocios <- names(frecuencia_viaje_negocios)[which.max(frecuencia_viaje_negocios)] # Moda: encontrar la categoría más frecuente
num_categorias_viaje_negocios <- length(unique(rotacion$Viaje_negocios)) # Número de categorías únicas
# Crear data frame para la tabla
tabla_viaje_negocios <- data.frame(Categoria = names(frecuencia_viaje_negocios),
                                   Frecuencia_Absoluta = as.character(as.integer(frecuencia_viaje_negocios)), # Convertir a entero y luego a caracter
                                   Porcentaje = paste0(format(porcentaje_viaje_negocios, digits = 2), "%")) %>%
  bind_rows(data.frame(Categoria = "Moda", Frecuencia_Absoluta = moda_viaje_negocios, Porcentaje = NA_character_),
            data.frame(Categoria = "Num. Categorias Unicas", Frecuencia_Absoluta = as.character(num_categorias_viaje_negocios), Porcentaje = NA_character_))
# Mostrar la tabla con formato kable
tabla_final_viaje_negocios <- tabla_viaje_negocios %>%
  kable(format = "html", caption = "Indicadores para la variable Viaje de Negocios") %>%
  kable_styling(bootstrap_options = "striped", full_width = FALSE)
tabla_final_viaje_negocios # Mostrar la tabla


## Variable Departamento.

resumen_departamento <- table(rotacion$Departamento)
df_departamento <- as.data.frame(resumen_departamento) # Convertir el resumen en un data frame
colnames(df_departamento) <- c("Departamento", "Frecuencia") # Renombrar las columnas
colores_pasteles <- c("#0B7072", "#F1713F", "#87C19B") # Definir una paleta de colores pasteles
# Crear el gráfico de barras con ggplot2 y especificar los colores
grafico_ggplot <- ggplot(df_departamento, aes(x = Departamento, y = Frecuencia, fill = Departamento)) +
  geom_bar(stat = "identity") +
  labs(title = "Cantidad de personas por Departamento",
       x = "Tipo de Departamento",
       y = "Frecuencia") +
  scale_fill_manual(values = colores_pasteles) +  # Asignar los colores pasteles
  theme_minimal()
grafico_plotly <- ggplotly(grafico_ggplot) # Convertir el gráfico de ggplot2 a plotly
grafico_plotly # Mostrar el gráfico interactivo de plotly

## Creación tabla con frecuencia absoluta y porcentaje -  Variable DEPARTAMENTO

frecuencia_departamento <- table(rotacion$Departamento) # Calcular la frecuencia absoluta de cada categoría
porcentaje_departamento <- prop.table(frecuencia_departamento) * 100 # Calcular el porcentaje de cada categoría respecto al total
moda_departamento <- names(frecuencia_departamento)[which.max(frecuencia_departamento)] # Moda: encontrar la categoría más frecuente
num_categorias <- length(unique(rotacion$Departamento)) # Número de categorías únicas
# Crear data frame para la tabla
tabla_departamento <- data.frame(Categoria = names(frecuencia_departamento),
                                 Frecuencia_Absoluta = as.character(as.integer(frecuencia_departamento)), # Convertir a entero y luego a caracter
                                 Porcentaje = paste0(format(porcentaje_departamento, digits = 2), "%")) %>%
  bind_rows(data.frame(Categoria = "Moda", Frecuencia_Absoluta = moda_departamento, Porcentaje = NA_character_),
            data.frame(Categoria = "Num. Categorias Unicas", Frecuencia_Absoluta = as.character(num_categorias), Porcentaje = NA_character_))
# Mostrar la tabla con formato kable
tabla_final <- tabla_departamento %>%
  kable(format = "html", caption = "Indicadores para la variable Departamento") %>%
  kable_styling(bootstrap_options = "striped", full_width = FALSE)
tabla_final # Mostrar la tabla

## Variable Estado Civil.
resumen_Estado_Civil <- table(rotacion$Estado_Civil)
df_E_C <- as.data.frame(resumen_Estado_Civil) # Convertir el resumen en un data frame
colnames(df_E_C) <- c("Estado_civil", "Frecuencia") # Renombrar las columnas
colores_pasteles <- c("#b52c00", "#fcf7d1", "#8c0005") # Definir una paleta de colores pasteles
# Crear el gráfico de barras con ggplot2 y especificar los colores
grafico_ggplot <- ggplot(df_E_C, aes(x = Estado_civil, y = Frecuencia, fill = Estado_civil)) +
  geom_bar(stat = "identity") +
  labs(title = "Cantidad de personas por Estado civil",
       x = "Estado civil",
       y = "Frecuencia") +
  scale_fill_manual(values = colores_pasteles) +  # Asignar los colores pasteles
  theme_minimal()
grafico_plotly <- ggplotly(grafico_ggplot) # Convertir el gráfico de ggplot2 a plotly
grafico_plotly # Mostrar el gráfico interactivo de plotly

## Creación tabla con frecuencia absoluta y porcentaje -  Variable ESTADO CIVIL

frecuencia_estado_civil <- table(rotacion$Estado_Civil) # Calcular la frecuencia absoluta de cada categoría
porcentaje_estado_civil <- prop.table(frecuencia_estado_civil) * 100 # Calcular el porcentaje de cada categoría respecto al total
moda_estado_civil <- names(frecuencia_estado_civil)[which.max(frecuencia_estado_civil)] # Moda: encontrar la categoría más frecuente
num_categorias_estado_civil <- length(unique(rotacion$Estado_Civil)) # Número de categorías únicas
# Crear data frame para la tabla
tabla_estado_civil <- data.frame(Categoria = names(frecuencia_estado_civil),
                                 Frecuencia_Absoluta = as.character(as.integer(frecuencia_estado_civil)), # Convertir a entero y luego a caracter
                                 Porcentaje = paste0(format(porcentaje_estado_civil, digits = 2), "%")) %>%
  bind_rows(data.frame(Categoria = "Moda", Frecuencia_Absoluta = moda_estado_civil, Porcentaje = NA_character_),
            data.frame(Categoria = "Num. Categorias Unicas", Frecuencia_Absoluta = as.character(num_categorias_estado_civil), Porcentaje = NA_character_))
tabla_final_estado_civil <- tabla_estado_civil %>% # Mostrar la tabla con formato kable
  kable(format = "html", caption = "Indicadores para la variable Estado Civil") %>%
  kable_styling(bootstrap_options = "striped", full_width = FALSE)
tabla_final_estado_civil # Mostrar la tabla

## Variable Edad.

colores_pasteles <- c("#56a292")
# Crea el histograma con ggplot2 y personaliza el tema y los colores
histograma_ggplot <- ggplot(rotacion, aes(x = Edad)) +
  geom_histogram(binwidth = 5, fill = colores_pasteles[1], color = "black") +
  labs(title = "Distribución por edades",
       x = "Edad",
       y = "Frecuencia") +
  theme_minimal() +
  theme(panel.background = element_blank(),
        plot.title = element_text(color = "black", size = 16),
        axis.title.x = element_text(color = "black", size = 12),
        axis.title.y = element_text(color = "black", size = 12))
histograma_plotly <- ggplotly(histograma_ggplot) # Convierte el gráfico de ggplot2 a plotly
histograma_plotly # Muestra el gráfico interactivo de plotly
## Creación tabla con frecuencia absoluta y porcentaje -  Variable EDAD
# Calcula los indicadores estadísticos
estadisticas_edad <- summarise(rotacion,
                               Mediana = median(Edad, na.rm = TRUE),
                               Media = mean(Edad, na.rm = TRUE),
                               Rango = max(Edad, na.rm = TRUE) - min(Edad, na.rm = TRUE),
                               Cuantil_25 = quantile(Edad, 0.25, na.rm = TRUE),
                               Cuantil_75 = quantile(Edad, 0.75, na.rm = TRUE),
                               Moda = names(sort(table(Edad), decreasing = TRUE)[1]),
                               Varianza = var(Edad, na.rm = TRUE),
                               Desv_Estandar = sd(Edad, na.rm = TRUE))
# Crear la tabla con kableExtra
tabla_estadisticas <- kable(estadisticas_edad, format = "html", caption = "Indicadores estadísticos de la variable Edad") %>%
  kable_styling(bootstrap_options = "striped", full_width = FALSE)
tabla_estadisticas # Mostrar la tabla

## Variable Satisfacción Laboral.

resumen_satisfaccion <- table(rotacion$Satisfación_Laboral)
df_satisfa<- as.data.frame( resumen_satisfaccion)
colores_pasteles <- c("#51445f", "#ffbeae", "#492d49","#d1b68d")
colnames(df_satisfa) <- c("Satisfacción", "Frecuencia") # Renombrar las columnas
# Crear el gráfico de torta interactivo
plot_ly(data = df_satisfa, labels = ~Satisfacción, values = ~Frecuencia, type = 'pie',marker = list(colors = colores_pasteles)) %>%
  layout(title = "Porcentaje de Satisfacción Laboral",
         xaxis = list(showgrid = FALSE, zeroline = FALSE, showticklabels = FALSE),
         yaxis = list(showgrid = FALSE, zeroline = FALSE, showticklabels = FALSE))
## Creación tabla con indicadores de centralidad y dispersión
# Calcula los indicadores estadísticos
estadisticas_Satisfación_Laboral <- summarise(rotacion,
                                              Mediana = median(Satisfación_Laboral, na.rm = TRUE),
                                              Media = mean(Satisfación_Laboral, na.rm = TRUE),
                                              Rango = max(Satisfación_Laboral, na.rm = TRUE) - min(Edad, na.rm = TRUE),
                                              Cuantil_25 = quantile(Satisfación_Laboral, 0.25, na.rm = TRUE),
                                              Cuantil_75 = quantile(Satisfación_Laboral, 0.75, na.rm = TRUE),
                                              Moda = names(sort(table(Satisfación_Laboral), decreasing = TRUE)[1]),
                                              Varianza = var(Satisfación_Laboral, na.rm = TRUE),
                                              Desv_Estandar = sd(Satisfación_Laboral, na.rm = TRUE))
# Crear la tabla con kableExtra
tabla_estadisticas <- kable(estadisticas_Satisfación_Laboral, format = "html", caption = "Indicadores estadísticos de la variable Satisfacción Laboral") %>%
  kable_styling(bootstrap_options = "striped", full_width = FALSE)
tabla_estadisticas # Mostrar la tabla

## Variable Ingreso Mensual.

# Crear el histograma con ggplot2
histograma_ggplot <- ggplot(rotacion, aes(x = Ingreso_Mensual)) +
  geom_histogram(binwidth = 1000, fill = "#492d49", color = "black") +
  labs(title = "Distribución del Ingreso Mensual",
       x = "Ingreso Mensual",
       y = "Frecuencia") +
  theme_minimal()
histograma_plotly <- ggplotly(histograma_ggplot) # Convertir el histograma de ggplot2 a plotly
histograma_plotly # Mostrar el gráfico interactivo de plotly

## Creación tabla con indicadores de centralidad y dispersión
# Calcula los indicadores estadísticos
estadisticas_Ingreso_Mensual <- summarise(rotacion,
                                          Mediana = median(Ingreso_Mensual, na.rm = TRUE),
                                          Media = mean(Ingreso_Mensual, na.rm = TRUE),
                                          Rango = max(Ingreso_Mensual, na.rm = TRUE) - min(Edad, na.rm = TRUE),
                                          Cuantil_25 = quantile(Ingreso_Mensual, 0.25, na.rm = TRUE),
                                          Cuantil_75 = quantile(Ingreso_Mensual, 0.75, na.rm = TRUE),
                                          Moda = names(sort(table(Ingreso_Mensual), decreasing = TRUE)[1]),
                                          Varianza = var(Ingreso_Mensual, na.rm = TRUE),
                                          Desv_Estandar = sd(Ingreso_Mensual, na.rm = TRUE))
# Crear la tabla con kableExtra
tabla_estadisticas <- kable(estadisticas_Ingreso_Mensual, format = "html", caption = "Indicadores estadísticos de la variable Ingreso Mensual") %>%
  kable_styling(bootstrap_options = "striped", full_width = FALSE)
tabla_estadisticas # Mostrar la tabla

##### 3. Análisis bivariado.

## Relación Rotación - Viaje de negocios.

resumen_combinaciones <- table(rotacion$Rotación, rotacion$Viaje_negocios) # Contar la frecuencia de cada combinación de valores entre "Rotación" y "Viaje de Negocios"
df_combinaciones <- as.data.frame(resumen_combinaciones) # Convertir el resumen en un data frame
colnames(df_combinaciones) <- c("Rotacion", "Viaje_Negocios", "Frecuencia") # Renombrar las columnas
# Definir una paleta de colores pasteles
colores_pasteles <- c("#FFB6C1", "#FFD700", "#87CEEB", "#98FB98")  # Ejemplo de colores pasteles
# Crear el gráfico de barras con ggplot2 y especificar los colores
grafico_ggplot <- ggplot(df_combinaciones, aes(x = Rotacion, y = Frecuencia, fill = Viaje_Negocios)) +
  geom_bar(stat = "identity", position = "dodge") +
  labs(title = "Frecuencia de Rotación por Viaje de Negocios",
       x = "Rotación",
       y = "Frecuencia") +
  scale_fill_manual(values = colores_pasteles) +  # Asignar los colores pasteles
  theme_minimal()
grafico_plotly <- ggplotly(grafico_ggplot) # Convertir el gráfico de ggplot2 a plotly
grafico_plotly # Mostrar el gráfico interactivo de plotly
## Modelo Logit simple, tener en cuenta que no se interpreta el coeficiente estimado, ya que no tiene lógica.
rotacion$rota_Codifica <- as.numeric(ifelse(rotacion$Rotación == "Si", 1, 0))
modelo_Viaje <- glm(rota_Codifica ~ Viaje_negocios, data = rotacion, family = binomial(link = "logit"))
round(summary(modelo_Viaje)$coefficients,3)

## Relación Rotación - Departamento.

resumen_combinaciones_depar <- table(rotacion$Rotación, rotacion$Departamento) # Contar la frecuencia de cada combinación de valores entre "Rotación" y "Viaje de Negocios"
df_combinaciones_depar <- as.data.frame(resumen_combinaciones_depar) # Convertir el resumen en un data frame
colnames(df_combinaciones_depar) <- c("Rotacion", "Departamento", "Frecuencia") # Renombrar las columnas
# Definir una paleta de colores pasteles
colores_pasteles <- c("#5A87A3", "#FFF69A", "#68FFDA", "#98FB98")  # Ejemplo de colores pasteles
# Crear el gráfico de barras con ggplot2 y especificar los colores
grafico_ggplot <- ggplot(df_combinaciones_depar, aes(x = Rotacion, y = Frecuencia, fill = Departamento)) +
  geom_bar(stat = "identity", position = "dodge") +
  labs(title = "Relación entre rotación y Departamento",
       x = "Rotación",
       y = "Frecuencia") +
  scale_fill_manual(values = colores_pasteles) +  # Asignar los colores pasteles
  theme_minimal()
grafico_plotly <- ggplotly(grafico_ggplot) # Convertir el gráfico de ggplot2 a plotly
grafico_plotly # Mostrar el gráfico interactivo de plotly
## Modelo Logit simple, tener en cuenta que no se interpreta el coeficiente estimado, ya que no tiene lógica.
modelo_Departamento <- glm(rota_Codifica ~ Departamento, data = rotacion, family = binomial(link = "logit"))
round(summary(modelo_Departamento)$coefficients,3)

## Relación Rotación - Estado Civil.

resumen_combinaciones_EC <- table(rotacion$Rotación, rotacion$Estado_Civil) # Contar la frecuencia de cada combinación de valores entre "Rotación" y "Estado Civil"
df_combinaciones_depar_EC <- as.data.frame(resumen_combinaciones_EC) # Convertir el resumen en un data frame
colnames(df_combinaciones_depar_EC) <- c("Rotacion", "Estado_Civil", "Frecuencia") # Renombrar las columnas
# Definir una paleta de colores pasteles
colores_pasteles <- c("#011F26", "#03A688", "#F2668B", "#98FB98")  # Ejemplo de colores pasteles
# Crear el gráfico de barras con ggplot2 y especificar los colores
grafico_ggplot <- ggplot(df_combinaciones_depar_EC, aes(x = Rotacion, y = Frecuencia, fill = Estado_Civil)) +
  geom_bar(stat = "identity", position = "dodge") +
  labs(title = "Relación entre rotación y Estado civil",
       x = "Rotación",
       y = "Frecuencia") +
  scale_fill_manual(values = colores_pasteles) +  # Asignar los colores pasteles
  theme_minimal()
grafico_plotly <- ggplotly(grafico_ggplot) # Convertir el gráfico de ggplot2 a plotly
grafico_plotly # Mostrar el gráfico interactivo de plotly
## Modelo Logit simple, tener en cuenta que no se interpreta el coeficiente estimado, ya que no tiene lógica.
modelo_Estado_Civil <- glm(rota_Codifica ~ Estado_Civil, data = rotacion, family = binomial(link = "logit"))
round(summary(modelo_Estado_Civil)$coefficients,3)

## Relación Rotación - Edad.
# Crear el gráfico de cajas con ggplot2 y especificar los colores pasteles
cajas_edad_ggplot <- ggplot(rotacion, aes(x = as.factor(Rotación), y = Edad, fill = as.factor(Rotación))) +
  geom_boxplot() +
  labs(title = "Distribución de Edad por Rotación",
       x = "Rotación",
       y = "Edad") +
  scale_fill_manual(values = c("#1D438A", "#FEECA9")) +  # Colores pasteles para cada caja
  theme_minimal()
cajas_edad_plotly <- ggplotly(cajas_edad_ggplot) # Convertir el gráfico de ggplot2 a plotly
cajas_edad_plotly # Mostrar el gráfico interactivo de plotly
## Modelo Logit simple, tener en cuenta que no se interpreta el coeficiente estimado, ya que no tiene lógica.
modelo_Edad <- glm(rota_Codifica ~ Edad, data = rotacion, family = binomial(link = "logit"))
round(summary(modelo_Edad)$coefficients,3)

## Relación Rotación - Satisfacción Laboral.

resumen_combinaciones_EC <- table(rotacion$Rotación, rotacion$Satisfación_Laboral) # Calcula la frecuencia de Rotación y la media de Satisfacción Laboral para cada categoría de Rotación
df_combinaciones_depar_EC <- as.data.frame(resumen_combinaciones_EC) # Convertir el resumen en un data frame
colnames(df_combinaciones_depar_EC) <- c("Rotacion", "Satisfacción_Laboral", "Frecuencia") # Renombrar las columnas
# Definir una paleta de colores pasteles
colores_pasteles <- c("#193C40", "#214001", "#A62B1F", "#D96941")  # Ejemplo de colores pasteles
# Crear el gráfico de barras con ggplot2 y especificar los colores
grafico_ggplot <- ggplot(df_combinaciones_depar_EC, aes(x = Rotacion, y = Frecuencia, fill = Satisfacción_Laboral)) +
  geom_bar(stat = "identity", position = "dodge") +
  labs(title = "Relación entre rotación y Satisfacción Laboral",
       x = "Rotación",
       y = "Frecuencia") +
  scale_fill_manual(values = colores_pasteles) +  # Asignar los colores pasteles
  theme_minimal()
grafico_plotly <- ggplotly(grafico_ggplot) # Convertir el gráfico de ggplot2 a plotly
grafico_plotly # Mostrar el gráfico interactivo de plotly
## Modelo Logit simple, tener en cuenta que no se interpreta el coeficiente estimado, ya que no tiene lógica.
modelo_Satisfacción_Laboral <- glm(rota_Codifica ~ Satisfación_Laboral, data = rotacion, family = binomial(link = "logit"))
round(summary(modelo_Satisfacción_Laboral)$coefficients,3)

## Relación Rotación - Ingreso Mensual.
# Crear el gráfico de cajas con ggplot2 y especificar los colores pasteles
cajas_ingreso_ggplot <- ggplot(rotacion, aes(x = as.factor(Rotación), y = Ingreso_Mensual, fill = as.factor(Rotación))) +
  geom_boxplot() +
  labs(title = "Distribución de Ingreso Mensual por Rotación",
       x = "Rotación",
       y = "Ingreso Mensual") +
  scale_fill_manual(values = c("#709A86", "#964083")) +  # Colores pasteles para cada caja
  theme_minimal()
cajas_ingreso_plotly <- ggplotly(cajas_ingreso_ggplot) # Convertir el gráfico de ggplot2 a plotly
cajas_ingreso_plotly # Mostrar el gráfico interactivo de plotly
## Modelo Logit simple, tener en cuenta que no se interpreta el coeficiente estimado, ya que no tiene lógica.
modelo_Ingreso_Mensual <- glm(rota_Codifica ~ Ingreso_Mensual, data = rotacion, family = binomial(link = "logit"))
round(summary(modelo_Ingreso_Mensual)$coefficients,3)

# 4. Estimación del modelo.

rotacion$Rotación <- recode(rotacion$Rotación, "Si" = 1, "No" = 0, .default = -1) # Convertir "Si" a 1 y "No" a 0 en la variable Rotación
names(rotacion)[names(rotacion) == "Viaje de Negocios"] <- "Viaje_negocios" # Cambiar el nombre de la columna 'viaje de negocios' a 'viaje_negocios'
rotacion$Edad_Escalada <- scale(rotacion$Edad)[,1] # Escalar la edad directamente en la base de datos rotacion
rotacion$Ingreso_Mensual_Escalada <- scale(rotacion$Ingreso_Mensual)[,1] # Escalar el ingreso mensual directamente en la base de datos rotacion
set.seed(123) # Definir la semilla para reproducibilidad
# Dividir la base de datos en conjuntos de entrenamiento y prueba (70% - 30%)
indices <- createDataPartition(rotacion$Rotación, p = 0.7, list = FALSE)
datos_entrenamiento <- rotacion[indices, ]
datos_prueba <- rotacion[-indices, ]
# Modelo 1 - Ajustar el modelo logit - Este modelo es el original, sin conjunto de entrenamiento ni prueba
modelo_logit_1 <- glm(Rotación ~ Viaje_negocios + Departamento + Estado_Civil + Edad_Escalada + Satisfación_Laboral + Ingreso_Mensual_Escalada,data = rotacion,binomial(link = "logit"))
summary(modelo_logit_1) # Resumen modelo

# Modelo 2
modelo_logit_2 <- glm(Rotación ~ Viaje_negocios + Departamento + Estado_Civil + Edad_Escalada + Satisfación_Laboral + Ingreso_Mensual_Escalada,data = datos_entrenamiento,binomial(link = "logit"))
summary(modelo_logit_2) # Resumen modelo

# 5. Evaluación.


## Likelihood Ratio Test (Prueba de Razón de Verosimilitud)

# Modelo 1
lr_test_1 <- lrtest(modelo_logit_1)
print(lr_test_1)
# Modelo 2
lr_test_2 <- lrtest(modelo_logit_2)
print(lr_test_2)

## Deviance
# Modelo 1
deviance_1 <- deviance(modelo_logit_1)
print(paste("Deviance Modelo 1", deviance_1))
# Modelo 2
deviance_2 <- deviance(modelo_logit_2)
print(paste("Deviance Modelo 2", deviance_2))

## AIC (Criterio de Información de Akaike) y BIC (Criterio de Información Bayesiano).
# Modelo 1
# Cálculo del AIC y BIC
AIC_modelo_1 <- AIC(modelo_logit_1)
BIC_modelo_1 <- BIC(modelo_logit_1)
print(paste("AIC Modelo 1:", AIC_modelo_1))
print(paste("BIC Modelo 1:", BIC_modelo_1))
# Modelo 2
# Cálculo del AIC y BIC
AIC_modelo_2 <- AIC(modelo_logit_2)
BIC_modelo_2 <- BIC(modelo_logit_2)
print(paste("AIC Modelo 2:", AIC_modelo_2))
print(paste("BIC Modelo 2:", BIC_modelo_2))

## Matriz de confusión
# Modelo 1
predicciones_1 <- predict(modelo_logit_1, newdata = rotacion, type = "response")
clases_predichas_1 <- ifelse(predicciones_1 > 0.5, 1, 0)
rotacion$Rotación<-as.numeric(rotacion$Rotación==1)
matriz_confusion_1 <- confusionMatrix(table(clases_predichas_1, rotacion$Rotación))
print(matriz_confusion_1)
# Modelo 2
set.seed(123)  # Para reproducibilidad
predicciones <- predict(modelo_logit_2, newdata = datos_prueba, type = "response")
clases_predichas <- ifelse(predicciones > 0.5, 1, 0)
matriz_confusion <- confusionMatrix(table(clases_predichas, datos_prueba$Rotación)) # Crear la matriz de confusión
print(matriz_confusion) # Mostrar la matriz de confusión y métricas de evaluación

## ROC-AUC (Área bajo la Curva ROC)
probabilidades_1 <- predict(modelo_logit_1, newdata = rotacion, type = "response") # Calcular las probabilidades de predicción para el Modelo 1
probabilidades_2 <- predict(modelo_logit_2, newdata = datos_prueba, type = "response") # Calcular las probabilidades de predicción para el Modelo 2
curva_roc_1 <- roc(response = rotacion$Rotación, predictor = probabilidades_1) # Calcular la curva ROC para el Modelo 1
curva_roc_2 <- roc(response = datos_prueba$Rotación, predictor = probabilidades_2) # Calcular la curva ROC para el Modelo 2
# Crear un data frame con los valores de sensibilidad y especificidad para el Modelo 1
roc_df_1 <- data.frame(
  especificidad = 1 - curva_roc_1$specificities,
  sensibilidad = curva_roc_1$sensitivities
)
# Crear un data frame con los valores de sensibilidad y especificidad para el Modelo 2
roc_df_2 <- data.frame(
  especificidad = 1 - curva_roc_2$specificities,
  sensibilidad = curva_roc_2$sensitivities
)
# Crear el gráfico de la Curva ROC con ggplot2
ggplot() +
  geom_line(data = roc_df_1, aes(x = especificidad, y = sensibilidad), color = "blue") +
  geom_line(data = roc_df_2, aes(x = especificidad, y = sensibilidad), color = "red") +
  geom_abline(intercept = 0, slope = 1, linetype = "dashed") +
  labs(x = "Tasa de Falsos Positivos (1 - Especificidad)", y = "Tasa de Verdaderos Positivos (Sensibilidad)", title = "Curva ROC") +
  geom_text(aes(x = 0.5, y = 0.3, label = paste("Modelo 1 (AUC =", round(auc(curva_roc_1), 3), ")")), color = "blue") +
  geom_text(aes(x = 0.5, y = 0.25, label = paste("Modelo 2 (AUC =", round(auc(curva_roc_2), 3), ")")), color = "red") +
  scale_color_identity(guide = "legend", labels = c("Modelo 1", "Modelo 2"), breaks = c("blue", "red")) +
  theme_minimal() +
  theme(legend.position = "bottom")

# 6. Predicciones.

'''Vamos a predecir la probabilidad de que un empleado con las siguientes características rote de cargo.

Las características son:
  
  - Viaje de Negocios: Raramente.

- Departamento: Ventas.

- Estado Civil: Soltero.

- Edad: 40.

- Satisfacción Laboral: 3.

- Ingreso Mesnual: 5000 '''

## Estas medidas se sacan para escalar como se encontraban originalmente las variables, ya que las variables Ingreso y Edad estan escaladas
#Estas se utilizan en l apredicción del modelo 1
media_edad <- mean(rotacion$Edad)
sd_edad <- sd(rotacion$Edad)
media_ingreso <- mean(rotacion$Ingreso_Mensual)
sd_ingreso <- sd(rotacion$Ingreso_Mensual)
#Estas se utilizan en l apredicción del modelo 2
media_edad_r <- mean(datos_entrenamiento$Edad)
sd_edad_r <- sd(datos_entrenamiento$Edad)
media_ingreso_r <- mean(datos_entrenamiento$Ingreso_Mensual)
# Individuo para el modelo 1
nuevo_dato <- data.frame(
  Viaje_negocios = "Raramente",
  Departamento = "Ventas",
  Estado_Civil = "Soltero", 
  Edad_Escalada = (40 - media_edad) / sd_edad, # Escalar la edad
  Satisfación_Laboral = 3,
  Ingreso_Mensual_Escalada = (5000 - media_ingreso) / sd_ingreso # Escalar el ingreso mensual
)
# Individuo para el modelo 2
nuevo_dato_1 <- data.frame(
  Viaje_negocios = "Raramente",
  Departamento = "Ventas",
  Estado_Civil = "Soltero",
  Edad_Escalada = (40 - media_edad_r) / sd_edad_r, # Escalar la edad
  Satisfación_Laboral = 3,
  Ingreso_Mensual_Escalada = (5000 - media_ingreso_r) / sd_ingreso_R   # Escalar el ingreso mensual
)
sd_ingreso_R <- sd(datos_entrenamiento$Ingreso_Mensual)
# Realizar la predicción con el modelo logit 1
predic_1 <- predict(modelo_logit_1, newdata = nuevo_dato, type = "response")
# Realizar la predicción con el modelo logit 2
predic_2 <- predict(modelo_logit_2, newdata = nuevo_dato_1, type = "response")

'''Predicción de rotación del empelado 14:

  Tiene las siguientes  características: 
  
- Viaje de Negocios: Raramente.

- Departamento: IyD.

- Estado Civil: Divorciado.

- Edad:  34.

- Satisfacción Laboral: 4.

- Ingreso Mensual: 2661. '''

# Crear datos inventados para un individuo
persona_especifica = rotacion[14, ]

# Realizar predicciones con ambos modelos
prediccion_1 <- predict(modelo_logit_1, newdata = persona_especifica, type = "response")
prediccion_2 <- predict(modelo_logit_2, newdata = persona_especifica, type = "response")
# Formatear las predicciones como porcentajes con dos decimales
print(paste("Predicción modelo 1: ",paste0(formatC(prediccion_1 * 100, format = "f", digits = 2), "%")))
print(paste("Predicción modelo 2: ",paste0(formatC(prediccion_2 * 100, format = "f", digits = 2), "%")))

'''
Predicción de un individuo con las siguentes caracteristicas:

  - Viaje de Negocios: Frecuentemente.

  - Departamento: IyD

  - Estado Civil: Soltero.

  - Edad:  Considerando dos individuos: uno con menos de 35 años (18 años) y otro con 35 años.

  - Satisfacción Laboral: 1.

  - Ingreso Mesnual: 5000 Considerando dos ingresos mensuales: uno menor a 5000 y otro igual a 5000
'''
# Subconjunto de datos para el primer individuo (18 años, salario de 1000)

# Crear el dataframe individuo_1 con los valores
individuo_1 <- data.frame(
  Viaje_negocios = "Frecuentemente",
  Departamento = "IyD",
  Estado_Civil = "Soltero",
  Edad_Escalada = (18 - media_edad) / sd_edad, # Aquí falta asignar el valor de Edad_Escalada
  Satisfación_Laboral = 1,
  Ingreso_Mensual_Escalada = (1000 - media_ingreso) / sd_ingreso
)
# Subconjunto de datos para el segundo individuo (35 años, salario de 5000)
individuo_2 <- data.frame(
  Viaje_negocios = "Frecuentemente",
  Departamento = "IyD",
  Estado_Civil = "Soltero",
  Edad_Escalada = (18 - media_edad_r) / sd_edad_r,
  Satisfación_Laboral = 1,
  Ingreso_Mensual_Escalada = (5000 - media_ingreso_r) / sd_ingreso_R
)
# Realizar predicciones con ambos modelos para el primer individuo
prediccion_1_individuo_1 <- predict(modelo_logit_1, newdata = individuo_1, type = "response")
prediccion_2_individuo_1 <- predict(modelo_logit_2, newdata = individuo_1, type = "response")
# Realizar predicciones con ambos modelos para el segundo individuo
prediccion_1_individuo_2 <- predict(modelo_logit_1, newdata = individuo_2, type = "response")
prediccion_2_individuo_2 <- predict(modelo_logit_2, newdata = individuo_2, type = "response")

# Mostrar las predicciones para el primer individuo
print("Predicciones para el primer individuo (18 años, salario de 1000):")
print(paste("Predicción modelo 1: ",paste0(formatC(prediccion_1_individuo_1 * 100, format = "f", digits = 2), "%")))
print(paste("Predicción modelo 2: ",paste0(formatC(prediccion_2_individuo_1 * 100, format = "f", digits = 2), "%")))

# Mostrar las predicciones para el segundo individuo
print("Predicciones para el segundo individuo (35 años, salario de 5000):")
print(paste("Predicción modelo 1: ",paste0(formatC(prediccion_1_individuo_2 * 100, format = "f", digits = 2), "%")))
print(paste("Predicción modelo 2: ",paste0(formatC(prediccion_2_individuo_2 * 100, format = "f", digits = 2), "%")))