## library("googlesheets4")

## # contains magic links
## source("./not_for_git/doc_link.R")

## # public access
## sheets_deauth()

## dados <- sheets_read(doc_link, 1)

## d <- data.frame(day=as.Date(dados[,1]$day), state=unlist(dados[,2]),
##                 suspect.cases=as.numeric(as.character(dados[,3]$suspect.cases)),
##                 confirmed.cases=as.numeric(as.character(dados[,4]$total.confirmed.cases)),
##                 discarded.cases=as.numeric(as.character(dados[,5]$discarded.cases)),
##                 deaths=as.numeric(as.character(dados[,6]$deaths)),
##                 local.transmission=as.character(dados[,7]$local.transmission)
##                 )

## write.csv(d, file = "./dados/covid_estados_manual.csv", row.names = FALSE)

## brasil_manual <- read.csv(doc_brasil)

## write.csv(brasil_manual, file = "./dados/covid_brasil_manual.csv", row.names = FALSE)

###########################################
# Atualizando tabela do ministério da saúde

#system("curl -s 'https://xx9p7hp1p7.execute-api.us-east-1.amazonaws.com/prod/PortalMapa' -H 'X-Parse-Application-Id: unAFkcaNDeXajurGB7LChj8SgQYS2ptm' -H 'TE: Trailers' | jq -r '.results | (map(keys) | add | unique) as $cols | map(. as $row | $cols | map($row[.])) as $rows | $cols, $rows[] | @csv' > ./dados/covid_estados_auto_curr.csv")

#dados_brasil <- read.csv("./dados/covid_estados_auto.csv", as.is = TRUE)
#dados_curr <- dados_curr[order(dados_curr$nome),]

## dados_brasil$day <- as.Date(dados_brasil$day)
## dados_curr$day <- max(as.Date(sapply(strsplit(dados_curr$updatedAt, split = "T"), function(x){x[1]})))

## curr_casos <- data.frame(nome = dados_curr$nome, qtd_confirmado = dados_curr$qtd_confirmado)
## dados_curr$casos_novos <- curr_casos$qtd_confirmado - dados_brasil$qtd_confirmado[c(nrow(dados_brasil)-26):nrow(dados_brasil)]

## if(max(dados_curr$day) != max(dados_brasil$day)){
##     dados_brasil <- rbind(dados_brasil, dados_curr)
## }

## dados_brasil <- dados_brasil[!duplicated(dados_brasil),]

## Sys.setlocale(locale = "pt_BR.UTF-8")
## dados_curr <- read.csv(paste0("https://covid.saude.gov.br/assets/files/COVID19_", format(Sys.Date(), "%Y%m%d"), ".csv"), as.is = TRUE, sep = ";")
## dados_curr$data <- as.Date(dados_curr$data, format = "%d/%m/%y")

## write.csv(dados_curr, file = paste0("./dados/BRnCov19_", format(max(dados_curr$data), "%Y%m%d"), ".csv"), row.names = FALSE)


file <- paste0("~/Downloads/COVID19_", format(Sys.Date(), format = "%Y%m%d"), ".csv")
if(length(grep(",", readLines(file, n = 3))) == 0){
    dados.raw <- read.csv(file, as.is = TRUE, sep = ";")
} else {
    dados.raw <- read.csv(file, as.is = TRUE, sep = ",")
}

#ordem <- sapply(dados.raw[, 4:7], function(x){sum(diff(x) < 0)})

#dados.full <- cbind(dados.raw[, 1:3], dados.raw[, c(names(sort(ordem))[c(4, 2, 3, 1)])])
dados.full <- dados.raw

names(dados.full) <- c("regiao", "estado", "data", "novos.casos", "casos.acumulados", "obitos.novos", "obitos.acumulados")

if(sum(is.na(as.Date(dados.full$data, format = "%d/%m/%Y"))) == 0){
    dados.full$data <- as.Date(dados.full$data, format = "%d/%m/%Y")
} else {
    dados.full$data <- as.Date(dados.full$data, format = "%Y-%m-%d")
}

write.table(dados.full, file = paste0("./dados/BRnCov19_", format(Sys.Date(), format = "%Y%m%d"), ".csv"), sep = ",", row.names = FALSE)

write.table(dados.full, file = "./dados/EstadosCov19.csv", sep = ",", row.names = FALSE)


dados.br <- aggregate(dados.full[, 4:7], by = list(dados.full$data), FUN = sum)
names(dados.br)[1] <- "data"

write.table(dados.br, file = "./dados/BrasilCov19.csv", sep = ",", row.names = FALSE)
