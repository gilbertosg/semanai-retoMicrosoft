####################################################################
# Semana I
#
# Script en R para extraer la información de un usuario
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

if(!require("twitteR")){install.packages("twitteR")}
if(!require("base64enc")){install.packages("base64enc")}
if(!require("stringr")){install.packages("stringr")}

######################
# Leer CSV con todos los usuarios
csvFile = read.csv("dataset_final_users.csv", stringsAsFactors = FALSE)
dfus <- csvFile

# Se cambiarán las keys dinámicamente por lo que se pone en false
options(httr_oauth_cache=F)

##########################################

# Keys gil (primer integrante)
api_key <- "key_here"
api_secret <- "key_here"
access_token <- "key_here"
access_token_secret <- "key_here"
setup_twitter_oauth(api_key,api_secret,access_token,access_token_secret)

for (i in 1:180) {
  tryCatch({
    tuser <-getUser(dfus$screenName[i])  # obtiene el usuario
    dfus$followersCount[i] <- tuser$followersCount
    dfus$verified[i] <- tuser$verified + 0
    dfus$location[i] <- tuser$location
    dfus$friendsCount[i] <- tuser$friendsCount
    dfus$protected[i] <- tuser$protected + 0
  }, warning = function(w) {
    print(w) 
  }, error = function(e) {
    print(e) 
  }, finally = {
    next
  })
}

##########################################################################################
#Cambio de keys y vuelvo a ejcutar

# Keys alex (segundo integrante)
api_key <- "key_here"
api_secret <- "key_here"
access_token <- "key_here"
access_token_secret <- "key_here"
setup_twitter_oauth(api_key,api_secret,access_token,access_token_secret)

for (i in 181:360) {
  tryCatch({
    tuser <-getUser(dfus$screenName[i])
    dfus$followersCount[i] <- tuser$followersCount
    dfus$verified[i] <- tuser$verified + 0
    dfus$location[i] <- tuser$location
    dfus$friendsCount[i] <- tuser$friendsCount
    dfus$protected[i] <- tuser$protected + 0
  }, warning = function(w) {
    print(w) 
  }, error = function(e) {
    print(e) 
  }, finally = {
    next
  })
}

##########################################################################################
#Cambio de keys y vuelvo a ejcutar

# Keys erick (tercer integrante)
api_key <- "key_here"
api_secret <- "key_here"
access_token <- "key_here"
access_token_secret <- "key_here"
setup_twitter_oauth(api_key,api_secret,access_token,access_token_secret)

for (i in 361:540) {
  tryCatch({
    tuser <-getUser(dfus$screenName[i])
    dfus$followersCount[i] <- tuser$followersCount
    dfus$verified[i] <- tuser$verified + 0
    dfus$location[i] <- tuser$location
    dfus$friendsCount[i] <- tuser$friendsCount
    dfus$protected[i] <- tuser$protected + 0
  }, warning = function(w) {
    print(w) 
  }, error = function(e) {
    print(e) 
  }, finally = {
    next
  })
}

##########################################################################################
#Cambio de keys y vuelvo a ejcutar

# Keys alonso (cuarto integrante)
api_key <- "key_here"
api_secret <- "key_here"
access_token <- "key_here"
access_token_secret <- "key_here"
setup_twitter_oauth(api_key,api_secret,access_token,access_token_secret)

for (i in 541:720) {
  tryCatch({
    tuser <-getUser(dfus$screenName[i])
    dfus$followersCount[i] <- tuser$followersCount
    dfus$verified[i] <- tuser$verified + 0
    dfus$location[i] <- tuser$location
    dfus$friendsCount[i] <- tuser$friendsCount
    dfus$protected[i] <- tuser$protected + 0
  }, warning = function(w) {
    print(w) 
  }, error = function(e) {
    print(e) 
  }, finally = {
    next
  })
}

##########################################################################################
#Cambio de keys y vuelvo a ejcutar

# Keys damian (sexto integrante)
api_key <- "key_here"
api_secret <- "key_here"
access_token <- "key_here"
access_token_secret <- "key_here"
setup_twitter_oauth(api_key,api_secret,access_token,access_token_secret)

for (i in 721:900) {
  tryCatch({
    tuser <-getUser(dfus$screenName[i])
    dfus$followersCount[i] <- tuser$followersCount
    dfus$verified[i] <- tuser$verified + 0
    dfus$location[i] <- tuser$location
    dfus$friendsCount[i] <- tuser$friendsCount
    dfus$protected[i] <- tuser$protected + 0
  }, warning = function(w) {
    print(w) 
  }, error = function(e) {
    print(e) 
  }, finally = {
    next
  })
}

##########################################################################################
#Cambio de keys y vuelvo a ejcutar

# Keys pato (cuenta externa)
api_key <- "key_here"
api_secret <- "key_here"
access_token <- "key_here"
access_token_secret <- "key_here"
setup_twitter_oauth(api_key,api_secret,access_token,access_token_secret)

for (i in 901:1080) {
  tryCatch({
    tuser <-getUser(dfus$screenName[i])
    dfus$followersCount[i] <- tuser$followersCount
    dfus$verified[i] <- tuser$verified + 0
    dfus$location[i] <- tuser$location
    dfus$friendsCount[i] <- tuser$friendsCount
    dfus$protected[i] <- tuser$protected + 0
  }, warning = function(w) {
    print(w) 
  }, error = function(e) {
    print(e) 
  }, finally = {
    next
  })
}

##########################################################################################
#Cambio de keys y vuelvo a ejcutar

# Keys laura (cuenta externa)
api_key <- "key_here"
api_secret <- "key_here"
access_token <- "key_here"
access_token_secret <- "key_here"
setup_twitter_oauth(api_key,api_secret,access_token,access_token_secret)

for (i in 1081:1260) {
  tryCatch({
    tuser <-getUser(dfus$screenName[i])
    dfus$followersCount[i] <- tuser$followersCount
    dfus$verified[i] <- tuser$verified + 0
    dfus$location[i] <- tuser$location
    dfus$friendsCount[i] <- tuser$friendsCount
    dfus$protected[i] <- tuser$protected + 0
  }, warning = function(w) {
    print(w) 
  }, error = function(e) {
    print(e) 
  }, finally = {
    next
  })
}

#####################################################################################################
##### Escribimos en un CSV
write.csv(dfus, file=paste(getwd(),"/dataset_final_users.csv",sep=""))