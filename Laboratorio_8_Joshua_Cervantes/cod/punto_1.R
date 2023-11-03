#Se programa una funcion que permita obtener todo lo solicitado en el punto 1

require(tidyverse)

#Se programa una funcion para estimar lo soliticitado en el punto 1
fn_punto_1 <- function(df = iris[,-5], k = 4, nstart = 100, ...){

    df_kmeans <- data.frame(k = numeric(), 
        totss = numeric(), 
        tiempo = numeric())


    #Se realizan 100 iteraciones de k means
    for(i in 1:nstart){

    #Se ejecuta el algoritmo
    km <- kmeans(df, centers = k, ...)

    df_aux <- data.frame(k = k, 
        totss = km$totss, 
        tiempo = NA)

        if(i==1){ 
            mejor_optimo <- km$totss
            mejor_km <- km
        }else if(mejor_optimo > km$totss){
            mejor_optimo <- km$totss
            mejor_km <- km
        }
    
    }
    optimo_promedio <- mean(df_kmeans$totss)
    atraccion_mejor_optimo <- sum(df_kmeans$totss == mejor_optimo)/nrow(df_kmeans)*100
    plot_optimos <- df_kmeans %>%
        ggplot(aes(x = totss)) +
        geom_histogram() + 
        theme_minimal() +
        labs(x = "Optimos locales", 
        y = "Cantidad")

    return(list(df_kmeans, 
        plot_optimos,
        optimo_promedio,
        mejor_optimo, 
        atraccion_mejor_optimo, 
        mejor_km
        ))


}
fn_punto_1()


