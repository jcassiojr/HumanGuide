# simula dados Human Guide com dados categóricos numéricos
f_simula_dados_HG <- function() {
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
    f_11s <- sample(x=c("1","2", "3"), size=815, replace=TRUE, prob=c(62.4/100, 17.7/100, 19.9/100))
    f_12e <- sample(x=c("1","2", "3"), size=815, replace=TRUE, prob=c(52.2/100, 35.5/100, 12.4/100))
    f_13h <- sample(x=c("1","2", "3"), size=815, replace=TRUE, prob=c(68.3/100, 15.1/100, 16.7/100))
    f_14k <- sample(x=c("1","2", "3"), size=815, replace=TRUE, prob=c(61.8/100, 28/100, 10.2/100))
    f_15p <- sample(x=c("1","2", "3"), size=815, replace=TRUE, prob=c(43.5/100, 24.7/100, 31.6/100))
    f_16hy <- sample(x=c("1","2", "3"), size=815, replace=TRUE, prob=c(33.9/100, 39.2/100, 26.9/100))
    f_17d <- sample(x=c("1","2", "3"), size=815, replace=TRUE, prob=c(41.4/100, 23.7/100, 34.9/100))
    f_18m <- sample(x=c("1","2", "3"), size=815, replace=TRUE, prob=c(34.4/100, 18.3/100, 47.3/100))
    f_21h <- sample(x=c("1","2", "3"), size=815, replace=TRUE, prob=c(59.7/100, 32.3/100, 8.1/100))
    f_22e <- sample(x=c("1","2", "3"), size=815, replace=TRUE, prob=c(70.4/100, 27.4/100, 2.2/100))
    f_23k <- sample(x=c("1","2", "3"), size=815, replace=TRUE, prob=c(24.2/100, 15.1/100, 60.8/100))
    f_24d <- sample(x=c("1","2", "3"), size=815, replace=TRUE, prob=c(12.9/100, 15.6/100, 71.5/100))
    f_25m <- sample(x=c("1","2", "3"), size=815, replace=TRUE, prob=c(58.6/100, 38.2/100, 3.2/100))
    f_26p <- sample(x=c("1","2", "3"), size=815, replace=TRUE, prob=c(79/100, 16.1/100, 4.8/100))
    f_27s <- sample(x=c("1","2", "3"), size=815, replace=TRUE, prob=c(64/100, 26.9/100, 9.1/100))
    f_28hy <- sample(x=c("1","2", "3"), size=815, replace=TRUE, prob=c(30.1/100, 28/100, 41.9/100))
    f_31h <- sample(x=c("1","2", "3"), size=815, replace=TRUE, prob=c(51.1/100, 40.3/100, 8.6/100))
    f_32e <- sample(x=c("1","2", "3"), size=815, replace=TRUE, prob=c(91.4/100, 7.5/100, 1.1/100))
    f_33hy <- sample(x=c("1","2", "3"), size=815, replace=TRUE, prob=c(5.9/100, 7/100, 87.1/100))
    f_34k <- sample(x=c("1","2", "3"), size=815, replace=TRUE, prob=c(76.3/100, 15.6/100, 8.1/100))
    f_35s <- sample(x=c("1","2", "3"), size=815, replace=TRUE, prob=c(45.2/100, 32.3/100, 22.6/100))
    f_36p <- sample(x=c("1","2", "3"), size=815, replace=TRUE, prob=c(62.4/100, 31.7/100, 5.9/100))
    f_37d <- sample(x=c("1","2", "3"), size=815, replace=TRUE, prob=c(32.3/100, 31.7/100, 36/100))
    f_38m <- sample(x=c("1","2", "3"), size=815, replace=TRUE, prob=c(37.6/100, 33.9/100, 28.5/100))
    f_41hy <- sample(x=c("1","2", "3"), size=815, replace=TRUE, prob=c(54.3/100, 36/100, 9.7/100))
    f_42s <- sample(x=c("1","2", "3"), size=815, replace=TRUE, prob=c(59.1/100, 17.2/100, 23.7/100))
    f_43e <- sample(x=c("1","2", "3"), size=815, replace=TRUE, prob=c(72.6/100, 15.1/100, 12.4/100))
    f_44k <- sample(x=c("1","2", "3"), size=815, replace=TRUE, prob=c(70.4/100, 18.3/100, 11.3/100))
    f_45h <- sample(x=c("1","2", "3"), size=815, replace=TRUE, prob=c(40.3/100, 33.9/100, 25.8/100))
    f_46m <- sample(x=c("1","2", "3"), size=815, replace=TRUE, prob=c(26.3/100, 28/100, 45.7/100))
    f_47d <- sample(x=c("1","2", "3"), size=815, replace=TRUE, prob=c(52.7/100, 34.9/100, 12.4/100))
    f_48p <- sample(x=c("1","2", "3"), size=815, replace=TRUE, prob=c(23.7/100, 17.7/100, 58.6/100))
    f_51e <- sample(x=c("1","2", "3"), size=815, replace=TRUE, prob=c(57.8/100, 19.4/100, 4.8/100))
    f_52s <- sample(x=c("1","2", "3"), size=815, replace=TRUE, prob=c(24.2/100, 23.7/100, 52.2/100))
    f_53hy <- sample(x=c("1","2", "3"), size=815, replace=TRUE, prob=c(39.2/100, 31.2/100, 29.6/100))
    f_54k <- sample(x=c("1","2", "3"), size=815, replace=TRUE, prob=c(47.8/100, 19.9/100, 32.3/100))
    f_55d <- sample(x=c("1","2", "3"), size=815, replace=TRUE, prob=c(32.3/100, 20.4/100, 47.3/100))
    f_56h <- sample(x=c("1","2", "3"), size=815, replace=TRUE, prob=c(51.1/100, 38.2/100, 10.8/100))
    f_57p <- sample(x=c("1","2", "3"), size=815, replace=TRUE, prob=c(58.6/100, 31.2/100, 10.2/100))
    f_58m <- sample(x=c("1","2", "3"), size=815, replace=TRUE, prob=c(71.5/100, 16.7/100, 11.8/100))
    f_61m <- sample(x=c("1","2", "3"), size=815, replace=TRUE, prob=c(84.9/100, 11.3/100, 3.8/100))
    f_62s <- sample(x=c("1","2", "3"), size=815, replace=TRUE, prob=c(36/100, 18.3/100, 45.7/100))
    f_63e <- sample(x=c("1","2", "3"), size=815, replace=TRUE, prob=c(16.1/100, 19.9/100, 64/100))
    f_64hy <- sample(x=c("1","2", "3"), size=815, replace=TRUE, prob=c(56.5/100, 19.9/100, 23.7/100))
    f_65k <- sample(x=c("1","2", "3"), size=815, replace=TRUE, prob=c(65.5/100, 21.5/100, 12.9/100))
    f_66p <- sample(x=c("1","2", "3"), size=815, replace=TRUE, prob=c(40.3/100, 43/100, 16.7/100))
    f_67d <- sample(x=c("1","2", "3"), size=815, replace=TRUE, prob=c(38.7/100, 36.6/100, 24.7/100))
    f_68h <- sample(x=c("1","2", "3"), size=815, replace=TRUE, prob=c(62.9/100, 29/100, 8.1/100))
    f_71m <- sample(x=c("1","2", "3"), size=815, replace=TRUE, prob=c(21.5/100, 23.1/100, 55.4/100))
    f_72k <- sample(x=c("1","2", "3"), size=815, replace=TRUE, prob=c(45.7/100, 40.9/100, 13.4/100))
    f_73s <- sample(x=c("1","2", "3"), size=815, replace=TRUE, prob=c(70.4/100, 23.7/100, 5.9/100))
    f_74p <- sample(x=c("1","2", "3"), size=815, replace=TRUE, prob=c(89.8/100, 10.2/100, 0/100))
    f_75hy <- sample(x=c("1","2", "3"), size=815, replace=TRUE, prob=c(28/100, 25.3/100, 46.8/100))
    f_76h <- sample(x=c("1","2", "3"), size=815, replace=TRUE, prob=c(51.6/100, 41.4/100, 7/100))
    f_77e <- sample(x=c("1","2", "3"), size=815, replace=TRUE, prob=c(84.4/100, 13.4/100, 2.2/100))
    f_78d <- sample(x=c("1","2", "3"), size=815, replace=TRUE, prob=c(8.6/100, 21.5/100, 69.9/100))
    f_81p <- sample(x=c("1","2", "3"), size=815, replace=TRUE, prob=c(39.2/100, 33.3/100, 27.4/100))
    f_82h <- sample(x=c("1","2", "3"), size=815, replace=TRUE, prob=c(72.6/100, 24.7/100, 2.7/100))
    f_83e <- sample(x=c("1","2", "3"), size=815, replace=TRUE, prob=c(88.7/100, 9.1/100, 2.2/100))
    f_84s <- sample(x=c("1","2", "3"), size=815, replace=TRUE, prob=c(57.5/100, 25.3/100, 17.2/100))
    f_85m <- sample(x=c("1","2", "3"), size=815, replace=TRUE, prob=c(30.1/100, 29.9/100, 40.9/100))
    f_86k <- sample(x=c("1","2", "3"), size=815, replace=TRUE, prob=c(61.8/100, 19.9/100, 18.3/100))
    f_87hy <- sample(x=c("1","2", "3"), size=815, replace=TRUE, prob=c(14.5/100, 14.5/100, 71/100))
    f_88d <- sample(x=c("1","2", "3"), size=815, replace=TRUE, prob=c(37.1/100, 43.5/100, 19.4/100))
    f_91e <- sample(x=c("1","2", "3"), size=815, replace=TRUE, prob=c(94.1/100, 4.8/100, 1.1/100))
    f_92m <- sample(x=c("1","2", "3"), size=815, replace=TRUE, prob=c(74.2/100, 22/100, 3.8/100))
    f_93p <- sample(x=c("1","2", "3"), size=815, replace=TRUE, prob=c(58.6/100, 28/100, 13.4/100))
    f_94d <- sample(x=c("1","2", "3"), size=815, replace=TRUE, prob=c(38.2/100, 34.9/100, 26.9/100))
    f_95k <- sample(x=c("1","2", "3"), size=815, replace=TRUE, prob=c(44.6/100, 28/100, 27.4/100))
    f_96s <- sample(x=c("1","2", "3"), size=815, replace=TRUE, prob=c(42.5/100, 23.1/100, 34.4/100))
    f_97h <- sample(x=c("1","2", "3"), size=815, replace=TRUE, prob=c(33.3/100, 37.6/100, 29/100))
    f_98hy <- sample(x=c("1","2", "3"), size=815, replace=TRUE, prob=c(14.5/100, 21.5/100, 64/100))
    
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
    df_hg <- data.frame(turnover = turnover,  
                        f_11s, f_12e, f_13h, f_14k, f_15p, f_16hy, f_17d, f_18m, f_21h, f_22e, f_23k, f_24d, f_25m, f_26p,
                        f_27s, f_28hy, f_31h, f_32e, f_33hy, f_34k, f_35s, f_36p, f_37d, f_38m, f_41hy, f_42s, f_43e,
                        f_44k, f_45h, f_46m, f_47d, f_48p, f_51e, f_52s, f_53hy, f_54k, f_55d, f_56h, f_57p, f_58m,
                        f_61m, f_62s, f_63e, f_64hy, f_65k, f_66p, f_67d, f_68h, f_71m, f_72k, f_73s, f_74p, f_75hy, f_76h,
                        f_77e, f_78d, f_81p, f_82h, f_83e, f_84s, f_85m, f_86k, f_87hy, f_88d, f_91e, f_92m, f_93p,
                        f_94d, f_95k, f_96s, f_97h, f_98hy)
    # Prepare Data - listwise deletion of missing (should I standardize variables?)
    df_hg <- na.omit(df_hg) # listwise deletion of missing
    return (df_hg)
}