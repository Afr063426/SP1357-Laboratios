#Se programa una funcion que permita obtener todo lo solicitado en el punto 1
require(tidyverse)


#Se programa una funcion para estimar lo soliticitado en el punto 1
fn_clusters_km <- function(kmeans, etiquetas = NA){

    #Se extraen los cluster
    clusters <- kmeans$informacion_general$mejor_km$cluster

    df <- as.matrix(kmeans$informacion_general$df)

    #Si no se suministran las etiquetas entonces
    #se coloca NA
    if(sum(is.na(etiquetas)) == 1){
        etiquetas <- rep(NA, nrow(df))
    }


    #Se aplica un acp a la tabla de datos
    acp <- prcomp(df)
    df_pc <- as.data.frame(df %*% as.matrix(acp$rotation))

    plot_clusters <- ggplot(, aes(x = df_pc$PC1, y = df_pc$PC2, color = factor(clusters), label = etiquetas)) +
        geom_point(size = 3) + 
        geom_label(size = 5)+
        scale_color_brewer(palette="Dark2") + 
        geom_hline(yintercept = 0) +
        geom_vline(xintercept = 0) +
        theme_minimal() +
        # theme(text = element_text(size = 16)) +
        labs(x = "PC1", y = "PC2", caption = paste("Porcentaje de inercia:",
            sum((acp$sdev)[c(1, 2)]^(2)) / sum(acp$sdev^(2)),
            sep = " "
        ), color = "Cluster")

    #Se retorna una lista de parametros de relevancia para el laboratior
    return(list(plot_clusters = plot_clusters
        ))


}

