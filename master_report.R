#!/usr/bin/env Rscript
# Written by:
# Gustavo Burin <gustavoburin@usp.br>
# In colaboration with:
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
knitr::opts_chunk$set(echo = FALSE, warning=FALSE, message=FALSE)
source("https://raw.githubusercontent.com/covid19br/covid19br.github.io/master/_src/funcoes.R")


#####################################
#####################################

#####################################
# Parsing Command Line Options

option_list <- list(
    make_option("--u", default = "p",
        help=("Unidade (país/estado/cidade) de interesse para gerar relatório. p para Brasil, e para Estado, c para cidades."),
        metavar="Unidade"),

    make_option("--n", default = "Brasil",
        help="Nome da unidade de interesse.",
        metavar="Nome_Unidade")## ,

    ## make_option("--b", type="integer", default=1000,
    ##     help=("Skip results of the first # of iterations [default %default]."),
    ##     metavar="Burnin"),

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
   cat("Erro: informar nome do Estado desejado.\n\n"); print_help(parser_object); quit(status=1)
}


#####################################
# Atualizando tabela do ministério da saúde

#system("curl -s 'https://xx9p7hp1p7.execute-api.us-east-1.amazonaws.com/prod/PortalMapa' -H 'X-Parse-Application-Id: unAFkcaNDeXajurGB7LChj8SgQYS2ptm' -H 'TE: Trailers' | jq -r '.results | (map(keys) | add | unique) as $cols | map(. as $row | $cols | map($row[.])) as $rows | $cols, $rows[] | @csv' > ./dados/covid_saude_gov_temp.csv; tail -n +2 -q ./dados/covid_saude_gov_temp.csv >> ./dados/covid_saude_gov.csv; rm ./dados/covid_saude_gov_temp.csv")

#####################################
# Defining Variables and Objects

#set.seed(2)
unid <- opt$options$u
nome_unid <- opt$options$n
if(unid=="e")
    nome_titulos  <-  paste("Estado de/da",nome_unid)
if(unid=="m")
    nome_titulos  <-  paste("Município de",nome_unid)
if(unid=="p")
        nome_titulos  <-  "Brasil"
    

if(unid == "p"){
    if(length(opt$args) == 0){
        dados.full <- read.csv("./dados/brasil.csv", as.is = TRUE)
        dados.full[,1] <- as.Date(dados.full[,1], format = "%d-%m-%y")
} else {
    dados.full <- read.csv(paste0(opt$args[1]), as.is = TRUE)
}
    dados.clean <- as.data.frame(aggregate(dados.full$casos.acumulados, by = list(dados.full$dia), FUN = sum, na.rm = TRUE))
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
    if(length(opt$args) == 0){
    dados.full <- read.csv("./dados/states2.csv", as.is = TRUE)
} else {
    dados.full <- read.csv(paste0(opt$args[1]), as.is = TRUE)
    }
    dados.filter <- dados.full[dados.full$state == nome_unid,]
    dados.clean <- as.data.frame(aggregate(dados.filter$confirmed.cases, by = list(dados.filter$day), FUN = sum, na.rm = TRUE))
    ## Removendo último dia caso esteja em branco
    if(dados.clean[nrow(dados.clean),1] != Sys.Date()){
        dados.clean <- dados.clean[-nrow(dados.clean),]
    }
    names(dados.clean) <- c("day", "confirmed.cases")

    nconf <- dados.clean[!duplicated(dados.clean),]
    nconf.zoo <- zoo(nconf[,-1], as.Date(nconf$day)) %>%
        diazero(limite = 1)
    ## Projecoes
    exp.5d <- forecast.exponential(nconf.zoo,
                                   start = length(time(nconf.zoo))-4,
                                   days.forecast = 5)
    data.final <- format(time(exp.5d)[5], format="%d de %B")
}

render(input = "./projecoes_observatorio_modelo.Rmd",
       output_file = paste0("./relatorios_gerados/relatorio_", gsub(" ", "_", nome_unid), "_", format(Sys.time(), '%d-%m-%Y_%Hh%Mmin%Ss'), ".pdf"),
       encoding = "utf8")
