# funcao que trata acentos
f_acentos_nomcol <- function(df_in) { # funcao que tira os acentos e deixa tudo em minuscula
    # acertando os nomes das colunas
    names(df_in) <-tolower(names(df_in)) # forçando minúsculas
    names(df_in) <- gsub("á", "a", names(df_in))
    names(df_in) <- gsub("é", "e", names(df_in))
    names(df_in) <- gsub("í", "i", names(df_in))
    names(df_in) <- gsub("ó", "o", names(df_in))
    names(df_in) <- gsub("ú", "u", names(df_in))
    names(df_in) <- gsub("ã", "a", names(df_in))
    names(df_in) <- gsub("õ", "o", names(df_in))
    names(df_in) <- gsub("ç", "c", names(df_in))
    names(df_in) <- gsub("â", "a", names(df_in))
    names(df_in) <- gsub("ê", "e", names(df_in))
    names(df_in) <- gsub("ô", "o", names(df_in))
    return (df_in)
}