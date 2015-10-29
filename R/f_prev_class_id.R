#' funçao que retorna probabilidade de ID informado
require("caret")
require("dplyr")
require("pROC")
f_prev_class_id <- function(i_models, i_useDescr, i_ID) {
    # criando vetor com os identificadores a partir dos dados de uso
    v_id <- i_useDescr[,1]
    l_prv_id <- list()
    # carrega lista com um data.frame de probabilidades de classe para cada modelo
    for (i in 1:length(i_models)) {
        #print (i_models[[i]])
        df_probs <- predict(i_models[[i]], i_useDescr[,-1], type = "prob") 
        # monta lista de data.frames com ID e probabilidade positiva (classe = "m")
        l_id_probs <- data.frame(ID = v_id, probClass = df_probs$m)
        
        # seleciona a probabilidade do ID desejado para cada modelo
        l_prv_id [[i]]<- 
            l_id_probs %>%
            filter (ID == i_ID)
    }
    
    #l_id_probs.names <- names(i_models)
    # retorna data.frame de probabilidades rankeadas até cutoof
    return(l_prv_id)
}