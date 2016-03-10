# funcao que trata acentos
f_acentos <- function(df_in) { # funcao que tira os acentos e deixa tudo em minuscula
    df_in <- mutate_each(df_in, funs(tolower)) # forçando minúsculas
    df_in <- as.data.frame(sapply(df_in, FUN = function(x) as.character(gsub("á", "a", x))))
    df_in <- as.data.frame(sapply(df_in, FUN = function(x) as.character(gsub("é", "e", x))))
    df_in <- as.data.frame(sapply(df_in, FUN = function(x) as.character(gsub("í", "i", x))))
    df_in <- as.data.frame(sapply(df_in, FUN = function(x) as.character(gsub("ó", "o", x))))
    df_in <- as.data.frame(sapply(df_in, FUN = function(x) as.character(gsub("ú", "u", x))))
    df_in <- as.data.frame(sapply(df_in, FUN = function(x) as.character(gsub("ã", "a", x))))
    df_in <- as.data.frame(sapply(df_in, FUN = function(x) as.character(gsub("õ", "o", x))))
    df_in <- as.data.frame(sapply(df_in, FUN = function(x) as.character(gsub("ç", "c", x))))
    df_in <- as.data.frame(sapply(df_in, FUN = function(x) as.character(gsub("â", "a", x))))
    df_in <- as.data.frame(sapply(df_in, FUN = function(x) as.character(gsub("ê", "e", x))))
    df_in <- as.data.frame(sapply(df_in, FUN = function(x) as.character(gsub("ô", "o", x))))
    df_in <- as.data.frame(sapply(df_in, FUN = function(x) as.character(gsub("  ", " ", x))))
    return (df_in)
}