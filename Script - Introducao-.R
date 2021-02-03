# <>-------------------------------------------------------------
# APP: Introducao a WebScraping com R
# data: 03.02.2021
# Objetivo: Obter dados de paginas web atraves de funcoes do R
#
# Obs.: Serao ocultados os acentos por questao da codificacao
# <>-------------------------------------------------------------
# <>-------------------------------------------------------------
# # fonte: https://gist.github.com/pimentel/256fc8c9b5191da63819
# head.list <- function(obj, n = 6L, ...)
# {
#   stopifnot(length(n) == 1L)
#   origN <- n
#   n <- if (n < 0L)
#     max(length(obj) + n, 0L)
#   else min(n, length(obj))
#   lapply(obj[seq_len(n)], function(x)
#   {
#     tryCatch({
#       head(x, origN, ...)
#     }, error = function(e) {
#       x
#     })
#   })
# }
# environment(head.list) <- asNamespace('utils')
# <>-------------------------------------------------------------
# <>-------------------------------------------------------------
#
# Instalacoes previas necessarias
# install.packages("devtools")
# library(devtools)
# devtools::install_github("tidyverse/rvest")
#
# Lendo o pacote
library(rvest)
# <>-------------------------------------------------------------
# <>-------------------------------------------------------------
# EXEMPLO 1 : SITE JARBAS
#
# 1- LER URL
url <- "https://jarbas.serenata.ai/dashboard/chamber_of_deputies/reimbursement/"
jarbas_webpage <- read_html(url)
# <>----------------------------
# <>----------------------------
# Scraping usando classe css 'field-congressperson_name'
jarbas_names_html <-html_nodes(jarbas_webpage, '.field-congressperson_name')
jarbas_names <- html_text(jarbas_names_html)
head(jarbas_names,3)
#
#SUBQUOTA TRANSLATED
jarbas_subquota_html <-html_nodes(jarbas_webpage, '.field-subquota_translated')
jarbas_subquota <- html_text(jarbas_subquota_html)
head(jarbas_subquota,3)
#
#Fornecedor
jarbas_provider_html <-html_nodes(jarbas_webpage, '.field-supplier_info')
jarbas_provider <- html_text(jarbas_provider_html)
head(jarbas_provider,3)
#
#Valores em Real
jarbas_value_html <-html_nodes(jarbas_webpage, '.field-value')
jarbas_value <- html_text(jarbas_value_html)
head(jarbas_value,3)
#
str(jarbas_value)
# <>----------------------------
# <>----------------------------
# Convertendo em numerico
library(stringr)
#Conversão para tipo numerico
jarbas_value <- as.numeric(sub(",",".",
               str_extract(jarbas_value[1:10],pattern = "\\d+,\\d+")))
head(jarbas_value,3)
# <>----------------------------
# <>----------------------------
#Combinando todas as caracteristicas obtidas
#Retirando primeiro nome
jarbas_names <- str_extract(jarbas_names,pattern = boundary("word"))
#
jarbas_df <- data.frame(
  Name = jarbas_names,
  Subquota = jarbas_subquota,
  Provider = jarbas_provider,
  Value = jarbas_value
)
#Mostrando tipo das variaveis
str(jarbas_df)
# <>----------------------------
View(jarbas_df)
# <>----------------------------
# <>----------------------------
# Geracao de um grafico
library(ggplot2)
#
jarbas_df <- jarbas_df#[1:50,]
ggplot(
  jarbas_df, aes(Value,Name,colour=Subquota)) +
  geom_point() +
  labs(title="", x ="Pedidos de reembolso (R$)",
       y = "Deputados",colour="SUBQUOTA TRANSLATED")
# <>-------------------------------------------------------------
# <>-------------------------------------------------------------
#EXEMPLO 2: Texto de um WebSite
# 1- LER URL
library(rvest)
url <- "https://brasil.elpais.com/brasil/2021-01-26/todos-os-brasileiros-estao-com-seus-dados-a-venda-e-ha-muito-pouco-o-que-se-pode-fazer-para-se-proteger.html"
webpage <- read_html(url)
# <>----------------------------
# <>----------------------------
# Passo 2: Extraindo texto de Site
#Scraping  usando classe css ‘p’
names_html <-html_nodes(webpage, 'p')
names <- html_text(names_html)
head(names)
# <>----------------------------
# <>----------------------------
# Passo 3: Preparação dos dados
#Convertendo a lista em vetor
dados_str <- unlist(names)
# inicio do código para Nuvem de palavras
library(tm)
library(wordcloud)
texto <- tolower(dados_str) # Colocando as palavras em minusculos
lista_palavras <- strsplit(texto, "\\W+")
vetor_palavras <- Corpus(VectorSource(unlist(lista_palavras)))
# <>----------------------------
# <>----------------------------
# Passo 4: Geração de um gráfico de Nuvem de Tags
# Abrindo Janela para Gráfico
X11()
wordcloud(words = vetor_palavras, min.freq = 3, random.order = TRUE, colors = yarrr::piratepal("basel"), use.r.layout = TRUE, rot.per = 0.5)
# <>----------------------------
# <>----------------------------
# Extra:
vetor <- Corpus(VectorSource(texto))
myTable <- TermDocumentMatrix(vetor)
myTable
#Convertendo em Matriz
matriz_palavras <- as.matrix(myTable)
matriz_palavras
#ordenando por frequência
matriz_palavras <- sort(rowSums(matriz_palavras), decreasing = TRUE)
dataframe_palavras <- data.frame(palavras = names(matriz_palavras), frequencia = matriz_palavras, row.names = NULL)
head(dataframe_palavras)
