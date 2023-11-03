#Se programa una funcion que permita obtener todo lo solicitado en el punto 1

require(tidyverse)

#Se programa una funcion para estimar lo soliticitado en el punto 1
fn_punto_1_extr_info <- function(summary_punto_1){

    return(list(plot_optimos = summary_punto_1$plot_optimos,
        optimo_promedio = summary_punto_1$optimo_promedio,
        mejor_optimo = summary_punto_1$mejor_optimo, 
        atraccion_mejor_optimo = summary_punto_1$atraccion_mejor_optimo
        ))


}

