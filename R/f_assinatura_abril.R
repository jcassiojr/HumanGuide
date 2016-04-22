#'funcao que retorna o plot de intensidade prevista para cada area 
#'de atuação de respondente selecionado nos dados de uso
#' input: linha de probabilidade de pertencer para cada area de atuacao obtidas para o respondente
#' output: plot com assinatura do respondente
#' 

f_assinatura_abril <- function(in.df_resp){
    my.respondente <- as.character(in.df_resp$nomerespondente) # salva nome do respondente
    in.df_resp <- in.df_resp[,-9] # deixa somente as colunas para previsao 
    resp.t <- as.data.frame(t(in.df_resp))
    resp.t <- add_rownames(resp.t, "area.prof")
    names(resp.t)[2] <- my.respondente
    resp.t$area.prof <- factor(resp.t$area.prof, 
                               levels = c("administração.e.negócio",
                               "saúde",
                               "artes.e.design",
                               "ciências.exatas.e.informática",
                               "ciências.humanas.e.sociais",
                               "comunicação.e.informação",
                               "engenharia",
                               "meio.ambiente.e.ciências.agrárias"
                             ))
    colnames(resp.t)  <- c("area.prof", "intensidade.neg")
    
    out.pl_area.prof <- ggplot(resp.t, aes(x=area.prof, y=intensidade.neg)) +
        geom_bar(aes (fill= factor(area.prof)), stat="identity") +
        scale_fill_manual(values=c("royalblue", "royalblue1", "royalblue2", "royalblue3","royalblue4",
                                   "springgreen1", "springgreen2", "springgreen3")) +
        ggtitle(paste("(", my.respondente,")")) +
        xlab("Área Profisisonal Human Guide") +
        ylab("intensidade negativa") +
        theme(axis.text.x = element_text(angle = 45, hjust = 1))
    
    return(out.pl_area.prof)
}