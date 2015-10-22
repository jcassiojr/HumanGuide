# simula dados Human Guide com dados categóricos numéricos
f_simula_dados_HG2 <- function() {
    # dados pessoais de 815 amostras
    set.seed(1)
    idade <- round(runif(815, 18, 60), digits = 0)
    sexo <- sample(x=c("m","f"), size=815, replace=TRUE, prob=c(51.3/100, 48.7/100))
    # superior completo, superior incompleto, posgraduacao completo,
    # posgraduação incompleto, ensino médio completo/incompleto
    escolaridade <- sample(x=c("sc","si","pc", "pi", "ec", "ei"), size=815,
                           replace=TRUE, prob=c(470/815, 11.5/100, 133/815, 0.5/100, 11.7/100, 2.6/100))
    # humanas, exatas, biológicas, não informado
    formacao <- sample(x=c("h","e","b", NA), size=815,
                       replace=TRUE, prob=c(46/100, 33/100, 6/100, 13.5/100))
    # área de atuação: navegação, TI, autopeças, juridico, saneamento público,
    # bancario, audiovisual, logística, educacional, saúde, químico, construcao civil, NA, outros
    segmento <- sample(x=c("nv","ti","ap","ju","sp","bn","av","lg","ed","sd","qu","cc", NA, "ou"), size=815,
                       replace=TRUE, prob=c(27.5/100, 16.2/100, 3.7/100, 3.3/100, 2.8/100, 5.9/100, 4.6/100,
                                            4.6/100, 3/100, 1.7/100, 2.2/100, 2.5/100, 7.9/100, 30.3/100))
    # procedência
    procedencia <- sample(x=c("sp.cap","sp.abc","sp.int", "sp.lit", "mg", "sc", "rg", "rj",
                              "am", "ba", "ce", "df", "go", "pe", "pi", "pr"), size=815,
                          replace=TRUE, prob=c(43.2/100, 9/100, 10.6/100, 4.2/100,
                                               6.5/100, 2.3/100, 1.6/100, 1.2/100,
                                               1/100, 0.2/100, 1/100, 0.2/100, 1/100, 0.2/100,
                                               1/100,0.2/100))
    # cargos
    # assistente, analista, auxiliar, coordenador, gerente, consultor interno/externo
    # diretor, estagiários/trainees, advogado, psicólogo, engenheiro, técnico, vendedor
    # programador, planejador, NA
    cargo <- sample(x=c("as","an","au", "co", "ge", "cs", "di", "es",
                        "ad", "ps", "en", "tc", "ve", "pr", "pl", NA), size=815,
                    replace=TRUE, prob=c(17.1/100, 19.4/100, 6.7/100, 7.5/100,
                                         6.3/100, 3.7/100, 2/100, 5.3/100,
                                         2/100, 1.5/100, 2/100, 2.2/100, 1.2/100, 1.2/100,
                                         1.2/100,6.2/100))
    # turnover (mais simples: s/n. depois sofisticar para considerar número de meses de permanência)
    # considerando balanceada
    turnover <- sample(x=c("s","n"), size=815,
                       replace=TRUE, prob=c(1/2,1/2))
    # features do teste HG sando os resultados da tese
    # 72 colunas com valores p ("1") (positivo), n ("3") (negativo) ou i ("2") (indiferente)
    forca <- sample(x=seq(-9, 9, by = 1), size=815, replace=TRUE, prob=rep(1/19,19))
    exposicao <- sample(x=seq(-9, 9, by = 1), size=815, replace=TRUE, prob=rep(1/19,19))
    estrutura <- sample(x=seq(-9, 9, by = 1), size=815, replace=TRUE, prob=rep(1/19,19))
    imaginacao <- sample(x=seq(-9, 9, by = 1), size=815, replace=TRUE, prob=rep(1/19,19))
    contatos <- sample(x=seq(-9, 9, by = 1), size=815, replace=TRUE, prob=rep(1/19,19))
    estabilidade <- sample(x=seq(-9, 9, by = 1), size=815, replace=TRUE, prob=rep(1/19,19))
    sensibilidade <- sample(x=seq(-9, 9, by = 1), size=815, replace=TRUE, prob=rep(1/19,19))
    qualidade <- sample(x=seq(-9, 9, by = 1), size=815, replace=TRUE, prob=rep(1/19,19))
    
    # criando o dataframe total
    #df_hg <- data.frame(turnover = turnover, idade = idade, sexo = sexo, escolaridade = escolaridade, formacao = formacao,
    #                    segmento = segmento, procedencia = procedencia, cargo = cargo,
    #                    f_11s, f_12e, f_13h, f_14k, f_15p, f_16hy, f_17d, f_18m, f_21h, f_22e, f_23k, f_24d, f_25m, f_26p,
    #                    f_27s, f_28hy, f_31h, f_32e, f_33hy, f_34k, f_35s, f_36p, f_37d, f_38m, f_41hy, f_42s, f_43e,
    #                    f_44k, f_45h, f_46m, f_47d, f_48p, f_51e, f_52s, f_53hy, f_54k, f_55d, f_56h, f_57p, f_58m,
    #                    f_61m, f_62s, f_63e, f_64hy, f_65k, f_66p, f_67d, f_68h, f_71m, f_72k, f_73s, f_74p, f_75hy, f_76h,
    #                    f_77e, f_78d, f_81p, f_82h, f_83e, f_84s, f_85m, f_86k, f_87hy, f_88d, f_91e, f_92m, f_93p,
    #                    f_94d, f_95k, f_96s, f_97h, f_98hy)
    # criando o dataframe somente com target e 72 features como categoricas numericas
    # para testes iniciais
    df_hg <- data.frame(turnover, forca, exposicao, estrutura, imaginacao, contatos,
                        estabilidade, sensibilidade, qualidade)
    # Prepare Data - listwise deletion of missing (should I standardize variables?)
    df_hg <- na.omit(df_hg) # listwise deletion of missing
    return (df_hg)
}