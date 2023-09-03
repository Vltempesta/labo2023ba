# Esqueleto de grid search
rm(list = ls()) # Borro todos los objetos
gc() # Garbage Collection

require("data.table")
require("rpart")
require("parallel")

PARAM <- list()
# Reemplazar por las propias semillas
PARAM$semillas <- c(100129, 200003, 300007, 400009, 500029)

#------------------------------------------------------------------------------
# Función para particionar el dataset y modificar la variable de clase
particionar <- function(data, division, agrupa = "", campo = "fold", start = 1, seed = NA) {
  if (!is.na(seed)) set.seed(seed)
  
  bloque <- unlist(mapply(function(x, y) {
    rep(y, x)
  }, division, seq(from = start, length.out = length(division))))
  
  data[, (campo) := sample(rep(bloque, ceiling(.N / length(bloque))))[1:.N],
       by = agrupa
  ]
}

#------------------------------------------------------------------------------

ArbolEstimarGanancia <- function(semilla, param_basicos) {
  # particiono estratificadamente el dataset
  particionar(dataset, division = c(7, 3), agrupa = "clase_binaria", seed = semilla)
  
  # genero el modelo
  # quiero predecir clase_binaria a partir del resto
  modelo <- rpart(clase_binaria ~ .,
                  data = dataset[fold == 1], # fold==1  es training, el 70% de los datos
                  xval = 0,
                  control = param_basicos
  ) # aquí van los parámetros del árbol
  
  # aplico el modelo a los datos de testing
  prediccion <- predict(modelo, # el modelo que generé recién
                        dataset[fold == 2], # fold==2  es testing, el 30% de los datos
                        type = "prob"
  ) # type= "prob"  es que devuelva la probabilidad
  
  # calculo la ganancia en testing que es fold==2
  ganancia_test <- dataset[
    fold == 2,
    sum(ifelse(prediccion[, "BAJA+2"] > 0.025,
               ifelse(clase_binaria == "BAJA+2", 117000, -3000),
               0
    ))
  ]
  
  # escalo la ganancia como si fuera todo el dataset
  ganancia_test_normalizada <- ganancia_test / 0.3
  
  return(ganancia_test_normalizada)
}

#------------------------------------------------------------------------------

ArbolesMontecarlo <- function(semillas, param_basicos) {
  # la función mcmapply llama a la función ArbolEstimarGanancia
  # tantas veces como valores tenga el vector semillas
  ganancias <- mcmapply(ArbolEstimarGanancia,
                        semillas, # paso el vector de semillas
                        MoreArgs = list(param_basicos), # aquí paso el segundo parámetro
                        SIMPLIFY = FALSE,
                        mc.cores = 1
  ) # se puede subir a 5 si posee Linux o Mac OS
  
  ganancia_promedio <- mean(unlist(ganancias))
  
  return(ganancia_promedio)
}

#------------------------------------------------------------------------------

# Establezco el Working Directory
setwd("~/buckets/b1/") 

# Cargo los datos
dataset <- fread("./datasets/dataset_pequeno.csv")

# Trabajo solo con los datos con clase, es decir 202107
dataset <- dataset[clase_ternaria != ""]

# Reemplazar la variable de clase ternaria con una variable binaria
dataset$clase_binaria <- ifelse(dataset$clase_ternaria == "BAJA+2", "BAJA+2", "NEGATIVA")

# Elimino la columna original de clase ternaria
dataset[, clase_ternaria := NULL]


# Genero el archivo para Kaggle
dir.create("./exp/", showWarnings = FALSE)
dir.create("./exp/HT2020/", showWarnings = FALSE)
archivo_salida <- "./exp/HT2020/gridsearch_binario.txt"

# Escribo los títulos al archivo donde van a quedar los resultados
cat(
  file = archivo_salida,
  sep = "",
  "cp", "\t",
  "max_depth", "\t",
  "min_split", "\t",
  "min_bucket", "\t",
  "ganancia_promedio", "\n"
)

# Itero por los loops anidados para cada hiperparámetro
for (vmax_depth in c(4, 8, 12, 20)) {
  for (vmin_split in c(1500, 1000, 500, 200, 50, 30)) {
    for (vcp in c(-0.35, -0.15, 0, 0.5)) {
      for (vmin_bucket in c(200, 100, 50, 20, 10, 5)) {
        # vminsplit  mínima cantidad de registros en un nodo para hacer el split
        param_basicos <- list(
          "cp" = vcp, # complejidad mínima
          "minsplit" = vmin_split,
          "minbucket" = vmin_bucket, # mínima cantidad de registros en una hoja
          "maxdepth" = vmax_depth
        ) # profundidad máxima del árbol
        
        # Un solo llamado, con la semilla 17
        ganancia_promedio <- ArbolesMontecarlo(PARAM$semillas, param_basicos)
        
        # Escribo los resultados al archivo de salida
        cat(
          file = archivo_salida,
          append = TRUE,
          sep = "",
          vmax_depth, "\t",
          vmin_split, "\t",
          vcp, "\t",
          vmin_bucket, "\t",
          ganancia_promedio, "\n"
        )
      }
    }
  }
}
