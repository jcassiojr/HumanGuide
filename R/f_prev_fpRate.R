#'funcao que retorna previsão sobre dados sem target definido
#'
require("caret")
require("pROC")

f_prev_fpRate <- function(i_models, i_useDescr, i_tipo_mod, i_valor_cutoff) {
   
    # criando vetor com os identificadores
    v_id <- i_useDescr[,1]
    # obtendo as probabilidades
    probValues <- extractProb(i_models, unkX = i_useDescr[,-1]) # OK
    # seleciona apenas o subset com o modelo desejado
    probValues <- subset(
        probValues,
        model == i_tipo_mod)
    # monta data.frame com ID e probabilidade positiva (classe = "m")
    df_prob <- data.frame(ID = v_id, probClass = probValues$m)
    # ordenando em ordem decrescente as probabilidades
    df_prob <-
        df_prob %>%
        arrange(desc(probClass)) %>%
        filter(probClass >= i_valor_cutoff) # filtra dados até valor de cutoff

    # retorna data.frame de probabilidades rankeadas até cutoof
    return(df_prob)
}