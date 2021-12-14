library(tidyverse)
library(tidytext) 
library(syuzhet) 
library(wordcloud) 
library(wordcloud2) 
library(patchwork) 
library(pdftools) 
library(readxl) 
library(stringr) 
library(ggplot2) 
library(readr) 
library(tm)
library(textdata)
library(png)
library(grid)
library(qdapRegex) 
library(quanteda)
library(quanteda.textmodels) 
library(stm)
library(igraph)
library(ggraph)
library(SnowballC)
library(htmlwidgets)

# Candidatos --------------------------------------------------------------

boric <- scan(file = "Candidatos_TXT/BORIC_TXT.txt",
              fileEncoding = "UTF-8",
              what = "char",
              sep = "\n")

kast <- scan(file = "Candidatos_TXT/KAST_TXT.txt",
             fileEncoding = "UTF-8",
             what = "char",
             sep = "\n")

# Pasar a tibble ----------------------------------------------------------

boric_2021 <- dplyr::tibble(boric) %>% 
  tidytext::unnest_tokens(output = "palabra",
                          input = boric,
                          strip_numeric = FALSE) %>% 
  dplyr::count(palabra, sort = TRUE) %>% 
  # dplyr::mutate_all(qdapRegex::rm_non_ascii) %>% # remueve emojis
  dplyr::mutate(palabra = stringr::str_remove_all(palabra, "[\\d\\.,_\\@]+"), # Cualquier @
                palabra = stringr::str_remove_all(palabra, "http[\\w[:punct:]]+")) #URL
                # palabra = stringr::str_remove_all(palabra, "\\@[[:alnum:]]+"))  # caracteres especiales


kast_2021 <- dplyr::tibble(kast) %>% 
  tidytext::unnest_tokens(output = "palabra",
                          input = kast,
                          strip_numeric = FALSE) %>% 
  dplyr::count(palabra, sort = TRUE)  %>% 
  # dplyr::mutate_all(qdapRegex::rm_non_ascii) %>%
  dplyr::mutate(palabra = stringr::str_remove_all(palabra, "[\\d\\.,_\\@]+"),
                palabra = stringr::str_remove_all(palabra, "http[\\w[:punct:]]+"))
                # palabra = stringr::str_remove_all(palabra, "\\@[[:alnum:]]+"))

# Eliminar stopwords ------------------------------------------------------

lista_stopwords_1 <- readr::read_csv("https://raw.githubusercontent.com/7PartidasDigital/AnaText/master/datos/diccionarios/vacias.txt")
# lista_stopwords_2 <- readr::read_csv("stopwords/stopwords.txt", col_names = TRUE)

# lista_stopwords <- lista_stopwords_1 %>% 
#   dplyr::left_join(lista_stopwords_2, by = c("palabra" = "algún"))

stopwords_extra_1 <- tibble::tibble(palabra =  c("de", "no", "y", "null"))
# stopwords_extra_2 <- tibble::tibble(palabra = tm::stopwords(kind = "spanish")[-c(7, 57)])

# stopwords <- dplyr::bind_rows(stopwords_extra_1, stopwords_extra_2)

boric_2021 <- boric_2021 %>% 
  dplyr::anti_join(lista_stopwords_1) %>% 
  dplyr::anti_join(stopwords_extra_1) 

kast_2021 <- kast_2021 %>% 
  dplyr::anti_join(lista_stopwords_1) %>% 
  dplyr::anti_join(stopwords_extra_1)

# Steeming ----------------------------------------------------------------

# El ‘stemming’ es un método para reducir una palabra a su raíz o morfema.
# Esto se hace con el objetivo de analizar variaciones de una palabra como una sola

boric_2021 %>% 
  dplyr::mutate(stem = wordStem(palabra, "spanish"))

kast_2021 %>% 
  dplyr::mutate(stem = wordStem(palabra, "spanish"))

# Posicionamiento de temas en el debate político  ---------------------------------------

# Creamos una nueva variable dummy que toma el valor "1" cada vez que la variable 
# de cadena de caracter coincide con alguna de las expresiones regulares.

boric_2021 %>% 
  dplyr::mutate(dummy = dplyr::case_when(stringr::str_detect(palabra, "mujer") ~ 1,
                                         stringr::str_detect(palabra, "aborto") ~ 1,
                                         stringr::str_detect(palabra, "embarazo") ~ 1,
                                         stringr::str_detect(palabra, "g[é|e]nero") ~ 1,
                                         stringr::str_detect(palabra, "identidad") ~ 1,
                                         stringr::str_detect(palabra, "feminismo") ~ 1,
                                         TRUE ~ 0),
                dummy = as.character(dummy)) %>% 
  dplyr::count(dummy) %>% 
  dplyr::summarise(dummy = dummy,
                   n = n,
                   porcentaje = round(n/sum(n)*100, digits = 2))


kast_2021 %>% 
  dplyr::mutate(dummy = dplyr::case_when(stringr::str_detect(palabra, "mujer") ~ 1,
                                         stringr::str_detect(palabra, "aborto") ~ 1,
                                         stringr::str_detect(palabra, "embarazo") ~ 1,
                                         stringr::str_detect(palabra, "g[é|e]nero") ~ 1,
                                         stringr::str_detect(palabra, "identidad") ~ 1,
                                         stringr::str_detect(palabra, "feminismo") ~ 1,
                                         TRUE ~ 0),
                dummy = as.character(dummy)) %>% 
  dplyr::count(dummy) %>% 
  dplyr::summarise(dummy = dummy,
                   n = n,
                   porcentaje = round(n/sum(n)*100, digits = 2))

# Gráfico términos frecuentes -------------------------------------------------------

# Boric

img <- readPNG("img", "fotos/boric3.png")
g <- rasterGrob(img, interpolate= TRUE)

get_png <- function(filename) {
  grid::rasterGrob(png::readPNG(filename), interpolate = TRUE)
}

l <- get_png("fotos/boric3.png")
t <- grid::roundrectGrob()

frecuencia_boric <- boric_2021 %>% 
  dplyr::top_n(15) %>% 
  ggplot2::ggplot(aes(y = reorder(palabra, n), x = n)) +
  ggplot2::geom_col(fill = "steelblue") +
  hrbrthemes::theme_ipsum() +
  ggplot2::labs(title = "Palabras más frecuentes",
       subtitle = "Programa presidencial Gabriel Boric",
       x = "Frecuencia",
       y = "Palabras") +
  ggplot2::geom_text(aes(label = n), size = 3, hjust = -0.4) +
  annotation_custom(l) 

frecuencia_boric

# Kast 

img <- readPNG("img", "fotos/kast_3.png")
g <- rasterGrob(img, interpolate= TRUE)

get_png <- function(filename) {
  grid::rasterGrob(png::readPNG(filename), interpolate = TRUE)
}

l <- get_png("fotos/kast_3.png")
t <- grid::roundrectGrob()

frecuencia_kast <- kast_2021 %>% 
  dplyr::top_n(15) %>% 
  ggplot2::ggplot(aes(y = reorder(palabra, n), x = n)) +
  ggplot2::geom_col(fill = "steelblue") +
  hrbrthemes::theme_ipsum() +
  ggplot2::labs(title = "Palabras más frecuentes",
       subtitle = "Programa presidencial José Antonio Kast",
       x = "Frecuencia",
       y = "Palabras") +
  ggplot2::geom_text(aes(label = n), size = 3, hjust = -0.4) +
  annotation_custom(l, xmin = 100, ymin = 2) 

frecuencia_kast

# Nubes de palabras ---------------------------------------------------------------

# Boric 
corpus_boric <- tm::Corpus(tm::VectorSource(boric)) 
corpus_boric <- tm::tm_map(corpus_boric, removeWords, stopwords("spanish"))

wordcloud(corpus_boric, 
          random.order = TRUE, 
          colors = brewer.pal(8, "Dark2"), 
          rot.per = 0.50, 
          min.freq = 1,
          max.words= 200, 
          scale=c(5,0.5))

wordcloud2::wordcloud2(data = boric_2021)

# Kast

corpus_kast <- tm::Corpus(tm::VectorSource(kast)) 
corpus_kast <- tm::tm_map(corpus_kast, removeWords, stopwords("spanish"))

wordcloud(corpus_kast, 
          random.order = TRUE, 
          colors = brewer.pal(8, "Dark2"), 
          rot.per = 0.50, 
          min.freq = 1,
          max.words= 200, 
          scale=c(5,0.5))

wordcloud2::wordcloud2(data = kast_2021)

# Asociaciones de las palabras ------------------------------------------------------------

# La matemática de findAssoc () se basa en la función estándar cor () en el paquete 
# de estadísticas de R.Dados dos vectores numéricos, cor () calcula su covarianza dividida
# por ambas desviaciones estándar.

# Boric 

corpus_boric <- tm::Corpus(tm::VectorSource(boric)) 
corpus_boric <- tm::tm_map(corpus_boric, removeWords, stopwords("spanish"))
dtm_boric <- tm::TermDocumentMatrix(corpus_boric)
cor_boric <- tm::findAssocs(dtm_boric, terms = "mujer", corlimit = 0.35)
cor_boric  

correlaciones_boric <- boric_frecuencias %>% 
  widyr::pairwise_cor(item = palabra, feature = n) %>% 
  dplyr::arrange(desc(correlation)) 

# Kast

corpus_kast <- tm::Corpus(tm::VectorSource(kast)) 
corpus_kast <- tm::tm_map(corpus_kast, removeWords, stopwords("spanish")) 
dtm_kast <- tm::TermDocumentMatrix(corpus_kast)
cor_kast <- findAssocs(dtm_kast, terms = "mujer", 
                       corlimit = 0.35)

cor_kast <- as.data.frame(cor_kast) %>% 
            tibble::rownames_to_column() 

cor_kast %>% 
  dplyr::rename(palabra = rowname) %>% 
  dplyr::mutate(palabra = stringr::str_remove_all(palabra, pattern = "[[:punct:]]"))

# Análisis términos más frecuentes Kast y Boric ---------------------------

terminos_boric <- tibble::tibble(palabras = tm::findFreqTerms(dtm_boric, lowfreq = 6))
terminos_kast <- tibble::tibble(palabras = tm::findFreqTerms(dtm_kast, lowfreq = 6))

# Análisis TF-IDF ---------------------------------------------------------

# Es el acrónimo de “Term Frequency times Inverse Document Frequency”.

# Mide con qué frecuencia aparece un término o frase dentro de un documento determinado, 
# y lo compara con el número de documentos que mencionan ese término dentro de una colección entera 
# de documentos.

# Objetivo: Cuáles son las palabras características de cada texto. 
# Medir la importancia de un término dentro de un documento es utilizando la frecuencia 
# con la que aparece 

# El estadístico tf-idf mide cómo de importante es un término en un documento teniendo en cuenta 
# la frecuencia con la que ese término aparece en otros documentos.

# Para eso, se le da más peso a las palabras que aparecen con mayor frecuencia 
# en uno de los textos y se les quita peso a las que son frecuentes en ambos. 
# Debido a esto, este tipo de análisis no requiere que saquemos las stopwords. 
# Como son palabras frecuentes en ambos textos, no son características de ninguno de los dos.

boric_frecuencias <- boric_2021 %>% 
  dplyr::mutate(programa = "Boric 1era vuelta 2021")

kast_frecuencias <-  kast_2021 %>% 
  dplyr::mutate(programa = "Kast 1era vuelta 2021")

# Luego, unimos ambos objetos:

programas_candidatos <- dplyr::bind_rows(boric_frecuencias, kast_frecuencias)
programas_candidatos <- dplyr::select(programas_candidatos, programa, palabra, n)
programas_candidatos

# Utilizar función TF IDF
programas_candidatos <- programas_candidatos %>% 
  tidytext::bind_tf_idf(term = palabra, document = programa, n = n) 

# term: en qué columna están las palabras.
# document: qué texto es (columna que diferenciará entre los discursos).
# n: dónde están las frecuencia de esas palabras.

# tf: frecuencia del término, respecto del total. 
# Frecuencia del término "calidad" en el total de palabras del discurso cuenta publica 2019.

# idf: peso que le da. Como es frecuente la palabra calidad, le entrega un peso cero.
# palabra que no permite caracterizar el discurso. Como es multiplicacion, el tf_idf también será 0.

# Las palabras características de cada discurso son las que tienen el tf_idf más alto. 
# Ordenemos.

programas_candidatos <- programas_candidatos %>% 
  dplyr::arrange(desc(tf_idf)) %>% 
  dplyr::group_split(programa) 

programas_candidatos 

tf_idf_boric <- programas_candidatos[[1]] %>% 
  dplyr::slice(1:20) %>% 
  ggplot2::ggplot(aes(x = reorder(palabra, tf_idf), y = tf_idf)) +
  ggplot2::geom_bar(stat = "identity", fill = "#7CAE00") +
  ggplot2::coord_flip() +
  hrbrthemes::theme_ipsum() +
  ggplot2::labs(title = "Boric",
                x = "Palabra",
                y = "TF-IDF")

tf_idf_kast <- programas_candidatos[[2]] %>% 
  dplyr::slice(1:20) %>% 
  ggplot2::ggplot(aes(x = reorder(palabra, tf_idf), y = tf_idf)) +
  ggplot2::geom_bar(stat = "identity", fill = "darkblue") +
  ggplot2::coord_flip() +
  hrbrthemes::theme_ipsum() +
  ggplot2::labs(title = "Kast",
                x = "Palabra",
                y = "TF-IDF")

# Visualizar ambos TDF - IDF
tf_idf_boric + tf_idf_kast

# Nube de palabras según programa -----------------------------------------

ggplot2::ggplot(programas_candidatos %>% dplyr::filter(n > 40),
       aes(label = palabra, size = n, color = programa)) + 
  ggwordcloud::geom_text_wordcloud() +
  ggplot2::scale_size_area(max_size = 6) + 
  ggplot2::facet_wrap(~programa)

# Network bigrams ---------------------------------------------------------

bigrama_boric <- programas_candidatos[[1]] %>%
  dplyr::filter(n > 20) %>%
  igraph::graph_from_data_frame()

a <- grid::arrow(type = "closed", length = unit(.15, "inches"))

ggraph::ggraph(bigrama_boric , layout = "fr") +
  ggraph::geom_edge_link(aes(edge_alpha = n), show.legend = FALSE,
                 arrow = a, end_cap = circle(.07, 'inches')) +
  ggraph::geom_node_point(color = "lightblue", size = 5) +
  ggraph::geom_node_text(aes(label = name), vjust = 1, hjust = 1) +
  ggplot2::theme_void()

bigrama_kast <- programas_candidatos[[2]] %>%
  dplyr::filter(n > 20) %>%
  igraph::graph_from_data_frame()

b <- grid::arrow(type = "closed", length = unit(.15, "inches"))

ggraph::ggraph(bigrama_kast , layout = "fr") +
  ggraph::geom_edge_link(aes(edge_alpha = n), show.legend = FALSE,
                         arrow = b, end_cap = circle(.07, 'inches')) +
  ggraph::geom_node_point(color = "lightblue", size = 5) +
  ggraph::geom_node_text(aes(label = name), vjust = 1, hjust = 1) +
  ggplot2::theme_void()

bigrama_general <- programas_candidatos %>%
  dplyr::filter(n > 60) %>%
  igraph::graph_from_data_frame()

a <- grid::arrow(type = "closed", length = unit(.15, "inches"))

ggraph::ggraph(bigrama_general , layout = "fr") +
  ggraph::geom_edge_link(aes(edge_alpha = n), show.legend = FALSE,
                         arrow = a, end_cap = circle(.07, 'inches')) +
  ggraph::geom_node_point(color = "lightblue", size = 5) +
  ggraph::geom_node_text(aes(label = name), vjust = 1, hjust = 1) +
  ggplot2::theme_void()





