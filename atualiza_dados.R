library(googlesheets4)

# contains magic links
source('not_for_git/doc_link.R')

# public access
sheets_deauth()

dados <- sheets_read(doc_link, 1)

d <- data.frame(day=as.Date(dados[,1]$day), state=unlist(dados[,2]),
                suspect.cases=as.numeric(as.character(dados[,3]$suspect.cases)),
                confirmed.cases=as.numeric(as.character(dados[,4]$total.confirmed.cases)),
                discarded.cases=as.numeric(as.character(dados[,5]$discarded.cases)),
                deaths=as.numeric(as.character(dados[,6]$deaths)),
                local.transmission=as.character(dados[,7]$local.transmission)
                )

write.csv(d, file='dados/states2.csv', row.names=FALSE)

