#'funcao que retorna previsão sobre dados sem target definido
#'
require("caret")
require("pROC")
# recebe lista de modelos, dados para previsao, lista de valores de cutoff para cada modelo
f_prev_rank_class <- function(i_models, i_useDescr, i_valor_cutoff) {
   
    # criando vetor com os identificadores
    v_id <- i_useDescr[,1]
    
    # OBS.: Com alternativa, pode ser usado abaixo para somente um modelo dando a mesma saída
    # probValues <- predict(i_model, useDescr[,-1], type = "prob")
    # seleciona apenas o subset com o modelo desejado
    #probValues <- subset(
    #    probValues,
    #    model == i_tipo_mod)
    l_id_probs <- list()

    # carrega lista com um data.frame de probabilidades de classe para cada modelo
    for (i in 1:length(i_models)) {
        #print (i_models[[i]])
        df_probs <- predict(i_models[[i]], i_useDescr[,-1], type = "prob") 
        # monta lista de data.frames com ID e probabilidade positiva (classe = "m")
        l_id_probs[[i]] <- data.frame(ID = v_id, probClass = df_probs$m)
        
        # ordenando em ordem decrescente as probabilidades
        # e aplicando cutoff ao dataframe
        l_id_probs[[i]] <-
            l_id_probs[[i]] %>%
            arrange(desc(probClass)) %>%
            filter(probClass >= i_valor_cutoff[[i]]) # filtra dados até valor de cutoff
    }
    #l_id_probs.names <- names(i_models)
    # retorna data.frame de probabilidades rankeadas até cutoof
    return(l_id_probs)
}