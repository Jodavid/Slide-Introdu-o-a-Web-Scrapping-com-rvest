# <>-------------------------------------------------------------
# APP: Introducao a WebScraping com R
# data: 19.09.2021
# Objetivo: Obter dados de paginas web atraves de funcoes do R
#
# Obs.: Serao ocultados os acentos por questao da codificacao
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
# jarbas_names <- str_extract(jarbas_names,pattern = boundary("word"))
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
# -------
# Pacote Necessário para Scraping
library(rvest)
# -------
# URL
url <- "https://brasil.elpais.com/brasil/2021-01-26/todos-os-brasileiros-estao-com-seus-dados-a-venda-e-ha-muito-pouco-o-que-se-pode-fazer-para-se-proteger.html"
# -------
# Lendo a Página
webpage <- read_html(url)
# <>----------------------------
# <>----------------------------
# Passo 2: Extraindo texto de Site
#Scraping  usando classe css ‘p’
names_html <-html_nodes(webpage, 'p')
# -------
# Retirando o texto retornados da classe 'p'
names <- html_text(names_html)
# -------
head(names)
# <>----------------------------
# <>----------------------------
# Passo 3: Preparação dos dados
# Início do código para Nuvem de palavras
library(tm)
# -------
# Colocando as palavras em minusculos
# -------
texto <- tolower(names)
# -------
# Removendo as stopwords
texto <- removeWords(texto, stopwords(kind = "portuguese"))
# -------
# Transformando em formato Corpus
texto <- Corpus(VectorSource(texto),
                readerControl = list(reader = reader(VectorSource(texto)),
                                     language = "pt"))
# -------
# Removendo as stopwords de arquivo.txt
# -------
file <- url("https://jodavid.github.io/Slide-Introdu-o-a-Web-Scrapping-com-rvest/stopwords_pt_BR.txt")
stopwords_ptBR <- read.table(file)
stopwords_ptBR <- unlist(stopwords_ptBR, use.names = FALSE)
texto <- tm_map(texto, removeWords, stopwords_ptBR)

# -------
# Descobrindo tamanho da lista
n <- length(texto)
# -------
# Removendo pontuacao
for(i in 1:n)
{
  # Remover pontuacao, preservando abreviacoes
  texto[[i]] <- removePunctuation(texto[[i]],
                                  preserve_intra_word_contractions = TRUE,
                                  preserve_intra_word_dashes = TRUE)
  # Removendo quotations
  texto[[i]] <- stringr::str_replace_all(texto[[i]], setNames(c('','','',''), c('“ ', '“',' ”','”')))
}
# -------
# <>----------------------------
# <>----------------------------
# Passo 4: Geração de um gráfico de Nuvem de Tags
# Abrindo Janela para Gráfico
X11()
library(wordcloud)
library(yarrr)
wordcloud(words = texto, min.freq = 2, random.order = TRUE,
          colors = yarrr::piratepal("basel"), use.r.layout = TRUE, rot.per = 0.5)
# <>----------------------------
# <>----------------------------
# Extra:
vetor <- texto#Corpus(VectorSource(texto))
myTable <- TermDocumentMatrix(vetor)
myTable
#Convertendo em Matriz
matriz_palavras <- as.matrix(myTable)
matriz_palavras
#ordenando por frequência
vetor_palavras <- sort(rowSums(matriz_palavras), decreasing = TRUE)
dataframe_palavras <- data.frame(palavras = names(vetor_palavras),
                                 frequencia = vetor_palavras, row.names = NULL)
head(dataframe_palavras)
# <>----------------------------
# <>----------------------------
# Análise de Sentimentos
#
# <>----------------------------
# Pacotes Necessários
# 
# library(devtools)
# install_github("abhy/Rstem")
# install_github("abhy/sentiment")
# install.packages("lexiconPT")
library(sentimentBR)
library(lexiconPT)

# <>----------------------------
# <>----------------------------
dataframe <- data.frame(text = sapply(texto, as.character), stringsAsFactors = FALSE)
# Utilizando o pacote "sentiment" para classificar as emocoes
#emotions <- classify_emotion(dataframe, algorithm = 'bayes', prior = 0.1)
emotions <- classify_emotion(textColumns = dataframe, algorithm = 'bayes', lang = "pt")
View(emotions)
# Utilizando o pacote "sentiment" para classificar as polaridades
polarities <- classify_polarity(textColumns = dataframe, algorithm = "bayes")
View(polarities)
# Transformando os resultados em data.frame
df <- data.frame(paragrafos = dataframe, emocoes = emotions[,'BEST_FIT'],
                 polaridades = polarities[,'BEST_FIT'])
# Transformando os NA em N.A.
df[is.na(df)] <- "N.A."
# <>----------------------------
# <>----------------------------
# Gráfico de Barras com polaridades
library(ggplot2)
X11()
ggplot(df, aes(polaridades,fill=polaridades)) +
  geom_bar() +
  labs(title="", x ="Polaridades",
       y = "Quantidades") + 
  theme_minimal()
View(df)
# <>----------------------------
# <>----------------------------
# Gráfico de Setores com emocoes
emotions <- data.frame(emotions)
ggplot(emotions, aes(x=factor(1), fill=factor(BEST_FIT))) +
  geom_bar(width = 1) +
  coord_polar(theta = "y") +
  labs(title="", x ="x",
       y = "y")
# <>----------------------------
# <>----------------------------
library(dplyr)
# Gráfico de Nuvem de tags com polaridades
#
# Dividindo o texto nas polaridades
polaridades_cat <- unique(df$polaridades)
# Separando os textos de postivo
positivo <- summarise(df,
                      texto2 = paste(df$text[which(df$polaridades==polaridades_cat[1])],
                                     collapse = " "))
# Separando os textos de Negativo
negativo <- summarise(df,
                      texto2 = paste(df$text[which(df$polaridades==polaridades_cat[2])],
                                     collapse = " "))
# Separando os textos de Neutro
neutro <- summarise(df,
                    texto2 = paste(df$text[which(df$polaridades==polaridades_cat[3])],
                                   collapse = " "))
# Juntando em um data.frame
df2 <- data.frame(polaridades = polaridades_cat,
                  pasted = c(positivo$texto2,negativo$texto2,neutro$texto2))
View(df2)
# ------------
corpus <- Corpus(VectorSource(df2$pasted))
corpus <- tm_map(corpus, removeNumbers)
tdm <- TermDocumentMatrix(corpus)
tdm <- as.matrix(tdm)
colnames(tdm) <- unique(df2$polaridades)

# <>----------------------------
# <>----------------------------
X11()
comparison.cloud(tdm,
                 colors = yarrr::piratepal("basel"),
                 scale = c(3,.5),random.order = F)
# <>----------------------------
