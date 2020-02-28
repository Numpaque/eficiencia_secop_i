##################################################
# Tiempos de contratación por entidad en SECOP I #
##################################################

# Autor: Juan Sebastian Numpaque Cano
# Contacto: jsnumpaquec@unal.edu.co
# Universidad Nacional de Colombia

# 0. Descarga de insumos ====

# La base de datos acá utilizada se descargó el día 25 de febrero a las 21:00
# desde el portal de datos abiertos (datos.gov.co).
# Se tomó la base de SECOP I (https://www.datos.gov.co/Gastos-Gubernamentales/SECOP-I/xvdy-vvsk/data)
# realizando los siguientes filtros desde el aplicativo dispuesto en el portal:
# - "Tipo de contrato" es "Prestación de servicios"
# - "ID Objeto a contratar" es "80000000" (equivalente a "Objeto a Contratar" es "Servicios de Gestión, 
#   Servicios Profesionales de Empresa y Servicios Administrativos")
# - "Nivel Entidad" es "NACIONAL"

# La base resultante cuenta con 721.516 registros, pesa 1,16 GB y se guarda con
# el nombre de "SECOP_I.csv"

# 1. Preparar el entorno de trabajo y leer la base ====

library(rstudioapi)
library(data.table)
library(dplyr)
library(quanteda)
library(wordcloud2)
library(bit64)

setwd(dirname(getActiveDocumentContext()$path)) # Ejecutar desde script
secop <- fread("SECOP_I.csv", encoding = "UTF-8")

# Descomentar esta si se quiere hacer un ejemplo con parte de la base
# secop <- head(secop, 100000)

# Seleccionar columnas y años de interés para el análisis 
columnas <- c("UID", "Anno Firma del Contrato", "Orden Entidad",
              "Nombre de la Entidad", "Código de la Entidad",
              "Detalle del Objeto a Contratar", "Cuantia Proceso",
              "Valor Total de Adiciones", "Objeto del Contrato a la Firma",
              "Fecha Ini Ejec Contrato","Fecha Fin Ejec Contrato")

secop <- secop[, columnas, with = FALSE]
secop <- secop[`Anno Firma del Contrato` <= 2019 & `Orden Entidad` == "NACIONAL CENTRALIZADO"]


# Identificar día del año y año en que se inicia y termina la contratación
secop[,`Fecha Ini Ejec Contrato` := as.POSIXct(`Fecha Ini Ejec Contrato`, format = "%m/%d/%Y")]
secop[,`Fecha Fin Ejec Contrato` := as.POSIXct(`Fecha Fin Ejec Contrato`, format = "%m/%d/%Y")]
secop[, dia_anno_inicio_ejecucion := yday(secop$`Fecha Ini Ejec Contrato`)]
secop[, dia_anno_fin_ejecucion := yday(secop$`Fecha Fin Ejec Contrato`)]
secop[, anno_inicio_ejecucion := year(secop$`Fecha Ini Ejec Contrato`)]
secop[, anno_fin_ejecucion := year(secop$`Fecha Fin Ejec Contrato`)]

# Tomar solo contratos que tengan inicio y fin en la misma vigencia
secop <- secop[anno_inicio_ejecucion == anno_fin_ejecucion]

# Graficar la distribución de inicio y fin de los contratos a lo largo del año
hist(secop$dia_anno_inicio_ejecucion)
hist(secop$dia_anno_fin_ejecucion)

# Calcular duración del contrato
secop[, duracion := (anno_fin_ejecucion-anno_inicio_ejecucion)*365 + 
        dia_anno_fin_ejecucion - dia_anno_inicio_ejecucion]

# Porcentaje de contratos con duración de cero días
sum(secop$duracion == 0) / length(secop$duracion) * 100

porcentaje_contratos_dura_cero_por_anno <- 
  table(secop$`Anno Firma del Contrato`[secop$duracion == 0])/
  table(secop$`Anno Firma del Contrato`)

plot(porcentaje_contratos_dura_cero_por_anno, type = "l")

# Porcentaje de contratos con duración de cero días por entidad
duracion_cero_por_entidad <- secop[, sum(duracion == 0) / .N * 100, by = `Nombre de la Entidad`]

# Tomar solo contratos que duren al menos 7 meses
secop <- secop[duracion > 8*30]

# Graficar duración de los contratos restantes
hist(secop$duracion)

descriptivos <- secop[, list(media = mean(dia_anno_inicio_ejecucion),
                       mediana = quantile(dia_anno_inicio_ejecucion, probs = 0.50),
                       count = .N), by = `Nombre de la Entidad`]

descriptivos_filtrada <- descriptivos %>%
  filter(count >= 20)

View(descriptivos_filtrada)


#################################################
# Agrupación de contratos con objetos similares #
#################################################

# 1. Funciones necesarias ====

# 1.1 Función de limpieza de texto (remoción de números, puntuación, etc.) ----
limpiar_texto <- function(texto, lista_stopwords = NULL, pasar_a_minusculas = TRUE, 
                    borrar_numeros = TRUE, borrar_tildes = TRUE, encoding = "UTF-8",
                    borrar_stopwords_base = TRUE, encoding_stopwords = "UTF-8"){
  
  require(dplyr)
  require(tm)
  
  # Pasa el texto a minúsculas, a menos que el usuario indique lo contrario (pasar_a_minusculas = FALSE)
  if(pasar_a_minusculas) texto <- tolower(texto) 
  
  # Cambia caracteres no imprimibles (tabulaciones, saltos de línea, etc.) por espacios
  texto <- gsub("[^[:print:]]", " ", texto)
  
  # Cambia por espacios todo lo que no sean minúsculas ni espacios (puntuación, símbolos, números, etc.)
  # Si el usuario especifica "borrar_numeros = FALSE", no se eliminarán los números.
  if(borrar_numeros){
    texto <- gsub("[^[:alpha:]^[:space:]]", " ", texto) 
  } else {
    texto <- gsub("[^[:alpha:]^[:space:]^[:digit:]]", " ", texto) 
  }
  
  # Elimina tildes, a menos que el usuario indique lo contrario (borrar_tildes = FALSE)
  if(borrar_tildes){
    if(any(!Encoding(texto) %in% c("unknown", "UTF-8"))) {
      warning("Encoding parece no ser UTF-8. Pueden presentarse problemas al eliminar tildes")
    }
    texto <- iconv(texto, from = encoding, to = "ASCII//TRANSLIT")
  }
  
  # Limpieza de las stopwords ----
  
  # Se limpian las stopwords (si las hay) con los mismos criterios que el texto
  if(borrar_stopwords_base){
    message("Se eliminarán stopwords base del paquete tm y letras únicas (a, b, c, ..., z)")
    lista_stopwords <- c(lista_stopwords, tm::stopwords(kind = "es"), letters)
  } 
  
  if(!is.null(lista_stopwords)){
    
    # Pasa las stopwords a minúscula, a menos que el usuario indique lo contrario (pasar_a_minusculas = FALSE)
    if(pasar_a_minusculas) lista_stopwords <- tolower(lista_stopwords) 
    
    # Cambia caracteres no imprimibles (tabulaciones, saltos de línea, etc.) por espacios
    lista_stopwords <- gsub("[^[:print:]]", " ", lista_stopwords)
    
    # Cambia por espacios todo lo que no sean minúsculas ni espacios (puntuación, símbolos, números, etc.)
    # Si el usuario especifica "borrar_numeros = FALSE", no se eliminarán los números.
    if(borrar_numeros){
      lista_stopwords <- gsub("[^[:lower:]^[:space:]]", " ", lista_stopwords) 
    } else {
      lista_stopwords <- gsub("[^[:lower:]^[:space:]^[:digit:]]", " ", lista_stopwords) 
    }
    
    # Elimina tildes, a menos que el usuario indique lo contrario (borrar_tildes = FALSE)
    if(borrar_tildes){
      if(any(!Encoding(lista_stopwords) %in% c("unknown", "UTF-8"))) {
        warning("Encoding de stopwords parece no ser UTF-8. Pueden presentarse problemas al eliminar tildes")
      }
      lista_stopwords <- iconv(lista_stopwords, from = encoding_stopwords, to = "ASCII//TRANSLIT")
    }
    
    # Se eliminan las stopwords (limpias)
    texto <- removeWords(texto, lista_stopwords)
  }
  
  # Cambia espacios consecutivos por uno solo y elimina espacios al inicio y al final de la cadena
  texto <- gsub("[[:space:]]{1,}", " ", texto)
  texto <-trimws(texto)
  
  return(texto)
}

# 1.2 Calcular similitud coseno ====
#
# 1) Descripción:
#   Función para calcular similitud coseno entre los vectores (filas) de una matriz
# 
# 2) Argumentos: 
#   - matriz_vectores: Una matriz de tamaño n x m que contiene los n vectores de m dimensiones entre los cuales
#     se desea calcular las distancias coseno.
#
# 3) Valor:
#   La función retorna una matriz simétrica de tamaño n x n que contiene las similitudes coseno calculadas
#   entre cada par de elementos. Por ejemplo, la similitud coseno entre los vectores de las filas 5 y 8 se
#   encontrará en la posición [5,8] de la matriz resultante.

calcular_similitudes_coseno <- function(matriz_vectores){
  norma_vectores <- sqrt(apply(matriz_vectores^2, 1, sum))
  matriz_similitudes <- t(matriz_vectores %*% t(matriz_vectores) / norma_vectores) / norma_vectores
  matriz_similitudes[is.na(matriz_similitudes)] <- 0
  return(matriz_similitudes)
}

# 2. Limpieza del texto ====
secop$texto_limpio <- limpiar_texto(secop$`Detalle del Objeto a Contratar`)

# 3. Exploración de las palabras presentes ("frecuentes") en más textos ====

# Nota: Aunque se nombra como "frecuencias", no se evalúa la frecuencia de un
# término en un documento, sino únicamente si el documento contiene o no el término

matriz_bow <- dfm(secop$texto_limpio)
matriz_bow@x <- rep(1, length(matriz_bow@x))

# Se analizan solo los términos presentes en más de un documento
matriz_bow <- dfm_trim(matriz_bow, min_termfreq = 2)

frecuencias_palabras <- colSums(matriz_bow)
frecuencias_palabras <- data.frame(palabra = names(frecuencias_palabras),
                                   frecuencia = as.numeric(frecuencias_palabras),
                                   stringsAsFactors = F)
frecuencias_palabras$lematizada <- stemDocument(frecuencias_palabras$palabra, language = "spanish")

diccionario_lematizada_original <- frecuencias_palabras %>% 
  select(palabra_final = palabra, lematizada) %>%
  filter(!duplicated(lematizada))

frecuencias_palabras <- frecuencias_palabras %>% 
  left_join(diccionario_lematizada_original, by = "lematizada")%>%
  data.table
  
frecuencias_palabras <- frecuencias_palabras[, list(frec = sum(frecuencia)), by = palabra_final, ]
frecuencias_palabras <- frecuencias_palabras[order(frec, decreasing = T)]
wordcloud2(head(frecuencias_palabras, 100))

# 4. Identificación de objetos similares con similitud coseno

secop <- secop[anno_inicio_ejecucion == 2019]

# Se realiza stemming (lematización) con el algoritmo de Porter
secop$texto_limpio <- stemDocument(secop$texto_limpio, language = "spanish")

# Se vectoriza el texto utilizando bag of words, ponderado con tf-idf, y se calcula la similitud coseno
matriz_bow <- dfm(secop$texto_limpio)
matriz_bow@x <- rep(1, length(matriz_bow@x))

# Lo ideal sería dejar min_termfreq = 2, pero esto ayuda a hacer mucho más rápido el procesamiento
matriz_bow <- dfm_trim(matriz_bow, min_termfreq = 10)
matriz_bow <- dfm_tfidf(matriz_bow)
matriz_bow <- convert(matriz_bow, to = "matrix")
matriz_similitudes <- calcular_similitudes_coseno(matriz_bow)
matriz_distancias <- as.dist(1 - matriz_similitudes)

# Agrupación con clustering jerárquico, método de enlace completo
clustering <- hclust(matriz_distancias, method = "complete")
secop$grupos <- cutree(clustering, h = 0.3)

# 5. Variación en cuantías por día para contratos similares
secop[, cuantia_por_dia := `Cuantia Proceso`/duracion]
secop[, promedio_grupo := mean(cuantia_por_dia), by = grupos]
secop[, diferencia_absoluta := cuantia_por_dia - promedio_grupo]
secop[, diferencia_porcentual := diferencia_absoluta/promedio_grupo*100]

# Comparar variación porcentual con adiciones
comparacion_con_adiciones <- data.frame(
  Adiciones = secop$`Valor Total de Adiciones`[secop$`Valor Total de Adiciones`  > 0],
  Diferencia = secop$diferencia_porcentual[secop$`Valor Total de Adiciones`  > 0])

comparacion_con_adiciones <- comparacion_con_adiciones %>%
  filter(Adiciones < quantile(comparacion_con_adiciones$Adiciones, probs = 0.95),
         Diferencia < quantile(comparacion_con_adiciones$Diferencia, probs = 0.95))

cor(comparacion_con_adiciones$Adiciones, comparacion_con_adiciones$Diferencia)
plot(comparacion_con_adiciones)


