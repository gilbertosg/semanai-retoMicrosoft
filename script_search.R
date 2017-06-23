####################################################################
# Semana I
#
# Script en R para extraer los tweets de un día
#
#################################################################### 
#
# Equipo 5
# Integrantes:
## Gilberto Silva – Ing. en Tecnologías Computacionales
## Eric Zuchovicki - Ing. en Tecnologías Computacionales
## Alex Pineda – Ing. en Negocios y Tecnologías de la Información
## Damian Scarinci – Lic. en Administración de Empresas
## Alonso Alfaro – Ing. Industrial y de Sistemas

# Verificar paquetes
if(!require("twitteR")){install.packages("twitteR")}
if(!require("base64enc")){install.packages("base64enc")}
if(!require("stringr")){install.packages("stringr")}

# Inicializar la API
init_cred <- function(){
  twitter_cred <- list()
  twitter_cred$consumer_key <- "key_here"
  twitter_cred$consumer_secret <- "key_here"
  twitter_cred$access_token <- "key_here"
  twitter_cred$access_secret <- "key_here"
  write.csv(twitter_cred, file="C:/Users/user_here/Documents/twitter_cred.csv")
}

twitter_cred <- read.csv("twitter_cred.csv", stringsAsFactors = FALSE)
setup_twitter_oauth(twitter_cred$consumer_key, twitter_cred$consumer_secret, twitter_cred$access_token, twitter_cred$access_secret)
ds <- searchTwitter("#upgradeyourworldmx",n=20000, since="2015-09-19", until="2015-09-20", locale = NULL)#,  since = "2015-09-01", until = "2015-09-02")

# Definir dataFrame
df1 <- twListToDF(ds)   # Comentar si no se extraen tweets

############
# Leer de un csv en vez de extraer tweets
#test = read.csv("regular_expressions.csv")
#df1 <- test
###########

# obtener el texto del tweet con su respectiva columna
tweetText <- df1[,c("text")]

################################################################################################
##### Expresiones Regulares

# Pobreza
df1$palabraPobreza <- grepl("pobre[(za)]?",tweetText, ignore.case = TRUE) + 0
# Niño / Niña
df1$palabraNinos <- grepl("niñ[oa]s?",tweetText, ignore.case = TRUE) + 0
# Cáncer
df1$palabraCancer <- grepl("c[aá]ncer",tweetText, ignore.case = TRUE) + 0
# Discapacidad
df1$palabraDiscapacidad <- grepl("discapaci",tweetText, ignore.case = TRUE) + 0
# Auditiva
df1$palabraAuditiva <- grepl("auditiv[ao]?|auditividad|escuch",tweetText, ignore.case = TRUE) + 0
# Animal
df1$palabraAnimal <- grepl("animal[(es)]?",tweetText, ignore.case = TRUE) + 0
# Leyes
df1$palabraLeyes <- grepl("ley[(es)]?",tweetText, ignore.case = TRUE) + 0
# Joven
df1$palabraJoven <- grepl("j[óo]ven[(es)]?|juventud",tweetText, ignore.case = TRUE) + 0
# Calle
df1$palabraCalle <- grepl("calle|callejer[oa]?[s]*",tweetText, ignore.case = TRUE) + 0
# Medio Ambiente
df1$palabraMedioAmbiente <- grepl("medio(\\s)?ambiente|ecol[oó]g[ií]",tweetText, ignore.case = TRUE) + 0
# Arte
df1$palabraArte <- grepl("art[e(ista)]?[s]?",tweetText, ignore.case = TRUE) + 0
# Paralisis
df1$palabraParalisis <- grepl("par[aá]lisis",tweetText, ignore.case = TRUE) + 0
# Cerebral
df1$palabraCerebral <- grepl("cerebr(al|o)",tweetText, ignore.case = TRUE) + 0
# Seguridad
df1$palabraSeguridad <- grepl("segur(o|a|(idad)])?[s]?",tweetText, ignore.case = TRUE) + 0
# Transplante
df1$palabraTransplante <- grepl("tra[n]?splante[s]?",tweetText, ignore.case = TRUE) + 0
# Igualdad
df1$palabraIgualdad <- grepl("igualdad|equidad",tweetText, ignore.case = TRUE) + 0
# Rescate
df1$palabraRescate <- grepl("rescat[e|(ar)]?[s]?",tweetText, ignore.case = TRUE) + 0
# Alzheimer
df1$palabraAlzheimer <- grepl("alzheimer",tweetText, ignore.case = TRUE) + 0
# Hambre
df1$palabraHambre <- grepl("hambr([e|(iento)|(ienta)])|alimenta([r|(ci[oó]n)])|hambruna",tweetText, ignore.case = TRUE) + 0
# EducaciÃ³n
df1$palabraEducacion <- grepl("educa((ci[oó]n)?[r]?)?|enseña[(nza)|r]?|apendizaje",tweetText, ignore.case = TRUE) + 0
# Vivienda
df1$palabraVivienda <- grepl("vivienda[s]?",tweetText, ignore.case = TRUE) + 0
# Periodista
df1$palabraPeriodista <- grepl("periodis[(mo)]?|periodis[(ta)[s]?)]?",tweetText, ignore.case = TRUE) + 0
# Indígena
df1$palabraIndigena <- grepl("ind[ií]gena[s]?",tweetText, ignore.case = TRUE) + 0
# Mujer
df1$palabraMujer <- grepl("mujer[es]?",tweetText, ignore.case = TRUE) + 0
# Violencia
df1$palabraViolencia <- grepl("violen[(to)|(ta)|(cia)]?[s]?",tweetText, ignore.case = TRUE) + 0
# Deporte
df1$palabraDeporte <- grepl("deport[e(ista)][s]?",tweetText, ignore.case = TRUE) + 0
# Salud
df1$palabraSalud <- grepl("salud((\\s)|($))",tweetText, ignore.case = TRUE) + 0

#####################################################################################################
###### Limpieza de datos

# Remover columnas innecesarias
df1$id <- NULL
df1$favorited <- NULL
df1$replyToSN <- NULL
df1$truncated <- NULL
df1$replyToSID <- NULL
df1$replyToSN <- NULL
df1$replyToUID <- NULL
df1$longitude <- NULL
df1$latitude <- NULL
df1$isRetweet <- NULL
df1$retweeted <- NULL

# Extraer plataforma (puro string)
platformString <- df1[,c("statusSource")]
df1$statusSource <- gsub(".*\">", "", platformString)
platformString2 <- df1[,c("statusSource")]
df1$statusSource <- gsub("</a>$", "", platformString2)

# Obtener el voto y limpiar los invalidos
df1$votoPor <- sub("^.*@\\w+\x85$", "VOTO INVALIDO",tweetText, ignore.case = TRUE, perl = TRUE)

#Considera los casos en los que #votopor esta inmediatamente seguido por la fundacion
df1$votoPor <- sub("^.*#votopor\\s*@(\\w+)\\W.*$", "\\1",df1$votoPor, ignore.case = TRUE)
df1$votoPor <- sub("^.*#votopor\\s*@(\\w+)$", "\\1",df1$votoPor, ignore.case = TRUE)
df1$votoPor <- sub("^.*#voto\\s*@(\\w+)\\W.*$", "\\1",df1$votoPor, ignore.case = TRUE)
df1$votoPor <- sub("^.*#voto\\s*@(\\w+)$", "\\1",df1$votoPor, ignore.case = TRUE)
df1$votoPor <- sub("^.*#yovotopor\\s*@(\\w+)\\W.*$", "\\1",df1$votoPor, ignore.case = TRUE)
df1$votoPor <- sub("^.*#yovotopor\\s*@(\\w+)$", "\\1",df1$votoPor, ignore.case = TRUE)
df1$votoPor <- sub("^.*vot[eo]\\s?por\\s+@(\\w+)\\W.*$", "\\1",df1$votoPor, ignore.case = TRUE)
df1$votoPor <- sub("^.*vot[eo]\\s?por\\s+@(\\w+)$", "\\1",df1$votoPor, ignore.case = TRUE)
df1$votoPor <- sub("^.*#yovoto\\s*@(\\w+)\\W.*$", "\\1",df1$votoPor, ignore.case = TRUE)
df1$votoPor <- sub("^.*#yovoto\\s*@(\\w+)$", "\\1",df1$votoPor, ignore.case = TRUE)
df1$votoPor <- sub("^.*#votapor\\s*@(\\w+)\\W.*$", "\\1",df1$votoPor, ignore.case = TRUE)
df1$votoPor <- sub("^.*#votapor\\s*@(\\w+)$", "\\1",df1$votoPor, ignore.case = TRUE)

#Considera los casos en los que #votopor no esta inmediatamente seguido por la fundacion
df1$votoPor <- sub("^.*#votopor\\s*.*@(\\w+)\\W.*$", "\\1",df1$votoPor, ignore.case = TRUE)
df1$votoPor <- sub("^.*#votopor\\s*.*@(\\w+)$", "\\1",df1$votoPor, ignore.case = TRUE)
df1$votoPor <- sub("^.*@(\\w+)\\W.*#votopor\\s*.*$", "\\1",df1$votoPor, ignore.case = TRUE)
df1$votoPor <- sub("^.*#voto\\s*.*@(\\w+)\\W.*$", "\\1",df1$votoPor, ignore.case = TRUE)
df1$votoPor <- sub("^.*#voto\\s*.*@(\\w+)$", "\\1",df1$votoPor, ignore.case = TRUE)
df1$votoPor <- sub("^.*#yovotopor\\s*.*@(\\w+)\\W.*$", "\\1",df1$votoPor, ignore.case = TRUE)
df1$votoPor <- sub("^.*#yovotopor\\s*.*@(\\w+)$", "\\1",df1$votoPor, ignore.case = TRUE)
df1$votoPor <- sub("^.*vot[eo]\\s?por\\s+.*@(\\w+)\\W.*$", "\\1",df1$votoPor, ignore.case = TRUE)
df1$votoPor <- sub("^.*vot[eo]\\s?por\\s+.*@(\\w+)$", "\\1",df1$votoPor, ignore.case = TRUE)
df1$votoPor <- sub("^.*#yovoto\\s*.*@(\\w+)\\W.*$", "\\1",df1$votoPor, ignore.case = TRUE)
df1$votoPor <- sub("^.*#yovoto\\s*.*@(\\w+)$", "\\1",df1$votoPor, ignore.case = TRUE)
df1$votoPor <- sub("^.*#votapor\\s*.*@(\\w+)\\W.*$", "\\1",df1$votoPor, ignore.case = TRUE)
df1$votoPor <- sub("^.*#votapor\\s*.*@(\\w+)$", "\\1",df1$votoPor, ignore.case = TRUE)

# Eliminar los votos inválidos
df1 <- df1[!df1$votoPor == "VOTO INVALIDO", ]

# Pasar a minúsculas la columna voto por
fundacionLower <- df1[,c("votoPor")]
df1$votoPor <- tolower(fundacionLower)

# Sustituye los acentos
df1$votoPor <- sub("á", "a",df1$votoPor, ignore.case = TRUE)
df1$votoPor <- sub("é", "e",df1$votoPor, ignore.case = TRUE)
df1$votoPor <- sub("í", "i",df1$votoPor, ignore.case = TRUE)
df1$votoPor <- sub("ó", "o",df1$votoPor, ignore.case = TRUE)
df1$votoPor <- sub("ú", "u",df1$votoPor, ignore.case = TRUE)

# Eliminar duplicados
user <- df1[,c("screenName")]
df1 <- df1[!duplicated(user), ]

#####################################################################################################
##### Escribimos en un CSV el resultado final del día
write.csv(df1, file=paste(getwd(),"/csv_dia_sep.csv",sep=""))