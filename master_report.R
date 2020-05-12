#!/usr/bin/env Rscript
# Written by:
# Gustavo Burin <gustavoburin@usp.br>
# In collaboration with:
# Equipe Observatório COVID-19 BR


#####################################
#####################################
### B-Diversitree is a module that runs a bayesian implementation of the Musse, Geosse and Classe models present in the package Diversitree.
###
###
###
###
#####################################
#####################################

# Setting locale to Brasil
Sys.setlocale(category = "LC_TIME", locale = "pt_BR.UTF-8")

# Loading Libraries Required
options(warn=1)
suppressPackageStartupMessages(library("optparse"))
suppressPackageStartupMessages(library("rmarkdown"))
suppressPackageStartupMessages(library("knitr"))
suppressPackageStartupMessages(library("dplyr"))
suppressPackageStartupMessages(library("ggplot2"))
suppressPackageStartupMessages(library("patchwork"))
knitr::opts_chunk$set(echo = FALSE, warning=FALSE, message=FALSE)
source("https://raw.githubusercontent.com/covid19br/covid19br.github.io/master/_src/funcoes.R")


#####################################
#####################################

#####################################
# Parsing Command Line Options

option_list <- list(
    make_option("--u", default = "p",
        help = ("Unidade (país/estado/cidade) de interesse para gerar relatório. p para Brasil, e para Estado, c para cidades."),
        metavar = "Unidade"),

    make_option("--n", default = "Brasil",
        help = ("Sigla da unidade de interesse."),
        metavar = "Nome_Unidade"),

    make_option("--d", default = "t",
       help = ("Data para confecção do relatório. O padrão é t para hoje (today), com formato AAAA-MM-DD"),
       metavar = "Data")##,

    ## make_option("--p", type="integer", default=100,
    ##     help=("Print frequency [default %default]."),
    ##     metavar="Prnt_freq"),

    ## make_option("--d", type="double", default=0.05,
    ##     help=("Window size update parameters [default %default]."),
    ##     metavar="D-Range"),

    ## make_option("--r", type="integer", default=5,
    ##     help=("Rate to use for the calculation of the prior [default %default]."),
    ##     metavar="Rate"),

    ## make_option("--t", type="integer", default=1,
    ##     help=("Number of trees [default %default]."),
    ##     metavar="Tree"),

    ## make_option("--rho",  default= NULL,
    ##     help=("Taxon sampling (in quotations, space separated) [default %default]."),
    ##     metavar="Sampling"),

    ## make_option("--w", default="1.5 1.5 2 2",
    ##     help=("Window size for lambda, mu, q and gamma (shape parameters for the hyperpriors) [default %default]."        ),metavar="Window_Size"),

    ## make_option("--c", default="NULL",
    ##     help="Parameters to be constrained. Introduce \"lamdas\", \"mus\" or \"qs\". \n\t\t Alternatively a string representing the desired contraints can be introduced. \n\t\t E.g. For three states under a Musse model: \"1,2,2,4,5,6,7,8,9,10,8,12\" \n\t\t This indicates lamda3 ~ lamda2 and q31 ~ q13 [default %default]",
    ##     metavar="Constraints")

    )

parser_object <- OptionParser(usage = "Rscript %prog [Opções] [ARQUIVO]\n", 
                              option_list = option_list, 
                              description = "Script para compilar reports personalizados. Caso deseje gerar um relatório para o país, usar opção --u 'p'; caso queira algum estado em particular, usar opções --u 'e' --n '[NOME_ESTADO]'. Caso deseje usar uma tabela externa, indicar o caminho para o arquivo .csv após as opções. A tabela deverá conter obrigatoriamente ao menos duas colunas: 'day' e 'total.confirmed.cases'")
opt <- parse_args(parser_object, args = commandArgs(trailingOnly = TRUE), positional_arguments=TRUE)

#####################################
# Handling User Input and/or Option Errors

if(opt$options$u != "p" & opt$options$n == "Brasil"){
   cat("Erro: informar sigla do Estado desejado.\n\n"); print_help(parser_object); quit(status=1)
}


#####################################
# Defining Variables and Objects

#set.seed(2)
unid <- opt$options$u
nome_unid <- opt$options$n
tempo <- opt$options$d

if(unid=="e")
    nome_titulos  <-  paste("Estado de/da", nome_unid)
if(unid=="m")
    nome_titulos  <-  paste("Município de", nome_unid)
if(unid=="p")
        nome_titulos  <-  "Brasil"

if(length(opt$args) == 0){
    dados.full <- read.csv(paste0("./dados/BRnCov19_", ifelse(tempo == "t", format(Sys.Date(), "%Y%m%d"), format(as.Date(tempo), "%Y%m%d")), ".csv"), as.is = TRUE, sep = ",")
    #names(dados.full)[grep("dat", names(dados.full))] <- "data"
    #names(dados.full)[2] <- "estado"
    #names(dados.full)[grep("estad", names(dados.full))] <- "estado"
    #dados.full$data <- rep(seq.Date(from = as.Date("2020/01/30", format = "%Y/%m/%d"), to = as.Date("2020/04/06", format = "%Y/%m/%d"), by = 1), times = length(unique(dados.full$estado)))
    #dados.full$data <- as.Date(dados.full$data, format = "%d/%m/%Y")
    #names(dados.full) <- c("regiao", "estado", "data", "novos.casos", "casos.acumulados", "obitos.novos", "obitos.acumulados")
    #write.table(dados.full, file = paste0("./dados/BRnCov19_", format(Sys.Date(), "%Y%m%d"), ".csv"), row.names = FALSE, sep = ",")
    #write.table(dados.full, file = "./dados/EstadosCov19.csv", row.names = FALSE, sep = ",")
} else {
    dados.full <- read.csv(paste0(opt$args[1]), as.is = TRUE)
    #dados.full[rowSums(is.na(dados.full)) != 5,]
}

if(unid == "p"){
    dados.br <- read.csv("./dados/BrasilCov19.csv", as.is = TRUE)
    #names(dados.br)[1] <- "data"
    #write.table(dados.br, file = "./dados/BrasilCov19.csv", row.names = FALSE, sep = ",")
    dados.clean <- as.data.frame(aggregate(dados.full$casos.acumulados, by = list(dados.full$data), FUN = sum, na.rm = TRUE))
    #dados.full[rowSums(is.na(dados.full)) != 5,]
    names(dados.clean) <- c("day", "confirmed.cases")

    nconf <- dados.clean[!duplicated(dados.clean),]
    nconf.zoo <- zoo(nconf[,-1], as.Date(nconf$day)) %>%
        diazero(limite = 1)
    ## Projecoes
    exp.5d <- forecast.exponential(nconf.zoo,
                                   start = length(time(nconf.zoo))-4,
                                   days.forecast = 5)
    data.final <- format(time(exp.5d)[5], format="%d de %B")
} else if(unid == "e"){
    dados.full <- read.csv("./dados/EstadosCov19.csv", as.is = TRUE)
    dados.filter <- dados.full[dados.full$estado == nome_unid,]
    dados.clean <- as.data.frame(aggregate(dados.filter$casos.acumulados, by = list(dados.filter$data), FUN = sum, na.rm = TRUE))
    ## Removendo os últimos dias caso estejam em branco
    ## if(sum(dados.clean[, 1] >= Sys.Date()) != 0 & sum(dados.clean[, 2] == 0) != 0){
    ##     dados.clean <- dados.clean[-which((dados.clean[, 1] >= Sys.Date()) != 0 & sum(dados.clean[, 2] == 0) != 0),]
    ## }
    names(dados.clean) <- c("day", "confirmed.cases")

    nconf <- dados.clean[!duplicated(dados.clean),]
    nconf.zoo <- zoo(nconf[,-1], as.Date(nconf$day)) %>%
        diazero(limite = 1)
    ## Projecoes
    exp.5d <- forecast.exponential(nconf.zoo,
                                   start = length(time(nconf.zoo))-4,
                                   days.forecast = 5)
    data.final <- format(time(exp.5d)[5], format = "%d de %B")
}

render(input = "./projecoes_observatorio_modelo.Rmd",
       output_file = paste0("./relatorios_gerados/relatorio_", gsub(" ", "_", nome_unid), "_", ifelse(tempo == "t", format(Sys.time(), '%d-%m-%Y_%Hh%Mmin%Ss'), format(as.Date(tempo), "%d-%m-%Y")), ".pdf"),
       encoding = "utf8")
