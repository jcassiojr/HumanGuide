#' funcao de classificação do respondente em relação a campo profisisonal
#' entradas: nome do respondente, vetor de scores do respondente, data.frame de campos x PC.medio, limites de classificacao
#' saída: dataframe com campos e respectivas correlações, classificacao do respondente em relacao
#' a intensidade da correlacao e direçao da correlacao (positivo ou negativo)
#' a correlação entre as assinaturas indica o quanto a combinação dos scores do respondente se
#' aproxima da combinação de scores do campo profissional. Correlação positiva indica que o respondente
#' tem assinatura próxima do campo profissional. Correlação negativa indica que ele não tem assinatura
#' próxima do campo em questão. Atenção: o sinal da correlação aqui não está relacionada ao score positivo
#' ou negativo em relação ao componente! Apenas indica o quanto se aproxima ou afasta do campo profissional

f_classifica_HG <- function(c_nome.in, v_scores.medios.in, df_cpo_pc.in, v_lim_class.in) {
    # inserindo no dataframe os scores do respondente
    df_cpo_pc.in$alex <- v_scores.medios.in
    
    # calcula a correlação entre o score do respondente e o score médio para cada campo profissional
    df.out <- data.frame(campo.prof = c("CFM", "CFQ", "CCF", "COA", "CJS", "CCP", "CSL", "CMA", "CCE", "CBS"),
                          corr = c(cor(df_cpo_pc.in[,c(1,13)])[1,2],
                                   cor(df_cpo_pc.in[,c(2,13)])[1,2],
                                   cor(df_cpo_pc.in[,c(3,13)])[1,2],
                                   cor(df_cpo_pc.in[,c(4,13)])[1,2],
                                   cor(df_cpo_pc.in[,c(5,13)])[1,2],
                                   cor(df_cpo_pc.in[,c(6,13)])[1,2],
                                   cor(df_cpo_pc.in[,c(7,13)])[1,2],
                                   cor(df_cpo_pc.in[,c(8,13)])[1,2],
                                   cor(df_cpo_pc.in[,c(9,13)])[1,2],
                                   cor(df_cpo_pc.in[,c(10,13)])[1,2]
                          ))
    
    # inserindo direcao da correlação (positiva ou negativa)
    df.out <-
        df.out %>%
        mutate(direcao = ifelse(corr >= 0,"positivo",
                                "negativo"
        ))
    # inserindo classificaçao e direcao (positiva ou negativa) PODE HAVER ERRO AQUI AO CONSIDERAR ABS!!
    df.out <-
        df.out %>%
        mutate(corr.abs = abs(corr),
               class.campo = ifelse(corr.abs > lims.corr[1],"marcante",
                                    ifelse(corr.abs <= lims.corr[1] & corr.abs > lims.corr[2] ,"moderado",
                                           ifelse(corr.abs <= lims.corr[2] & corr.abs > lims.corr[3] ,"fraco",
                                                  "inexpressivo"
                                           )))) %>%
        select(-corr.abs)
    return(df.out)
}