if (!require(dplyr)) {
  install.packages("dplyr")
  library(dplyr)
}
library(dplyr)

# Definir la variable 'envios'
envios <- 11000

# Lista para almacenar los dataframes
lista_dataframes <- list()

# Nombres de las carpetas
nombres_carpetas <- c("ZZ6610", "ZZ6612","ZZ6615","ZZ6616", "ZZ661111", "ZZ661112")

# Iterar sobre las carpetas y archivos
for (nombre_carpeta in nombres_carpetas) {
  # Obtener la lista de archivos que comienzan con "pred_" en la carpeta actual
  archivos_en_carpeta <- list.files(path = file.path("~/buckets/b1/exp", nombre_carpeta),
                                    pattern = "^pred_.*\\.csv$", full.names = TRUE)
  
  # Iterar sobre los archivos en la carpeta actual
  for (archivo in archivos_en_carpeta) {
    # Leer el archivo con encabezados y separador 
    df <- read.csv(archivo, header = TRUE, sep = "") %>%
      select(-foto_mes, -clase_ternaria)
    lista_dataframes[[length(lista_dataframes) + 1]] <- df
  }
}

# Hacer el join por la columna 'numero_de_cliente'
df_final <- lista_dataframes %>%
  purrr::reduce(inner_join, by = "numero_de_cliente") %>%
  # Calcular la columna promedio de la prediccion
  mutate(promedio_prediccion = rowMeans(select(., starts_with("prob")), na.rm = TRUE)) %>%
  # Ordenar el dataframe por la columna promedio_prediccion de mayor a menor
  arrange(desc(promedio_prediccion)) %>%
  # Agregar un número de índice
  mutate(indice = row_number()) %>%
  # Crear la nueva columna 'predicted'
  mutate(predicted = ifelse(indice >= envios, 0, 1))

# Seleccionar las columnas 'numero_de_cliente' y 'predicted'
df_export <- df_final %>% select(numero_de_cliente, predicted)

# Crear una nueva carpeta si no existe
nueva_carpeta <- file.path("~/buckets/b1/exp", "ensamble_experimento_final")
dir.create(nueva_carpeta, showWarnings = FALSE)

# Crear el nombre del archivo con la cantidad de envíos
nombre_archivo <- paste0("resultados_ensamble_", envios, "_envios.csv")
archivo_salida <- file.path(nueva_carpeta, nombre_archivo)

# Escribir el dataframe en un archivo CSV en la nueva carpeta
write.csv(df_export, archivo_salida, row.names = FALSE)

# Imprimir la ubicación del archivo de salida
cat("Resultados exportados a:", archivo_salida, "\n")