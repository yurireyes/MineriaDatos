install.packages("gsubfn")
install.packages("openNLPmodels.es", repos = "http://datacube.wu.ac.at/", type = "source")
install.packages("rJava")
install.packages("openNLP")
library(rJava)
library(openNLP)
library(openNLPmodels.en)
#library(openNLPmodels.es)
library(gsubfn)
library(XML)
library(NLP)
library(tm) #Text mining
library(SnowballC)
library(plyr) #Función Laply
library(stringr) #Función str_trim
#library(e1071) #Librería SVM
library(wordcloud) #Nube de palabras
library(RColorBrewer) #Gráfico

corpusWithout <- read.csv('C:/Users/Citlali/Documents/Universidad/9SW/Minería de datos/Corpus550 - bueno.csv') #Se carga el corpus
corpus <- laply(corpusWithout$text, function(sentence){ #Separa el corpus en oraciones
  sentence <- gsub('[[:punct:]]', '', sentence) #Remueve puntuación
  sentence <- gsub('[[:cntrl:]]', '', sentence) #Remueve caracteres de control
  sentence <- gsub('\\d+', '', sentence) #Remueve dígitos
  sentence <- tolower(sentence) #Convierte mayus a minus
  sentence <- str_trim(sentence, side = "both") #Remueve espacios en blanco
})
corpus <- Corpus(VectorSource(corpus)) #Creación de vector

#Carga archivo de palabras & las convierte en ASCII
sw <- readLines(con = "C:/Users/Citlali/Documents/Universidad/9SW/Minería de datos/stopwords.csv", encoding = "UTF-8")
sw = iconv(sw, to="ASCII//TRANSLIT")
corpus = tm_map(corpus, removeWords, sw) #Elimina stopwords de archivo cargado
cuantif <- readLines(con = "C:/Users/Citlali/Documents/Universidad/9SW/Minería de datos/Cuantificadores.csv", encoding = "UTF-8")
cuantif = iconv(cuantif, to="ASCII//TRANSLIT")
corpus = tm_map(corpus, removeWords, cuantif) #Elimina cuantificadores
corpusSaved = corpus #Guarda corpus para posterior conversión a csv
wordcloud(corpus) #Realiza la nube de palabras
#corpus <- tm_map(corpus, stemDocument, language="spanish") #Steamming
#Guardar corpus limpio
corpusSaved<-data.frame(text=unlist(sapply(corpus, `[`, "content")), stringsAsFactors=F) #Crea dataframe p/exportar a csv
write.csv(corpusSaved, file = "C:/Users/Citlali/Documents/Universidad/9SW/Minería de datos/Corpus550-limpio.csv", row.names = F) #Exporta
corpusSaved

ap.tdm <- TermDocumentMatrix(corpus) #Guarda corpus en termdocumentmatrix
ap.m <- as.matrix(ap.tdm) #TDM es guardado como matrix para poder usarlo
ap.v <- sort(rowSums(ap.m),decreasing=TRUE) #Se toma la matrix para realizar sumas
ap.d <- data.frame(word = names(ap.v),freq=ap.v) #Se toma un data frame para tener las palabras y su frecuencia
frecuency <- table(ap.d$freq) #Se muestra en una tabla
frecuency

pal2 <- brewer.pal(8,"Dark2") #Tomar los colores
png("wordcloud.png", width=1280,height=800) #Guardar wordcloud en imagen
wordcloud(ap.d$word,ap.d$freq, scale=c(8,.2),min.freq=2, #Se aplican los colore para aplicarlos a la wordcloud
          max.words=Inf, random.order=FALSE, rot.per=.15, colors=pal2)
dev.off()
corpus4token = corpusSaved
corpus4token <- laply(corpus4token$text, function(sentence){ #Separa el corpus en oraciones
  sentence <- strsplit(sentence, "") #toquenización
  sentence <- as.String(sentence) #Convertir corpus en strings
})

# Start actual PoS-tagging
# apply annotators to Corpus
Corpus.tagged <- lapply(corpus4token, function(x){
  sent_token_annotator <- Maxent_Sent_Token_Annotator(language = "es")
  word_token_annotator <- Maxent_Word_Token_Annotator()
  pos_tag_annotator <- Maxent_POS_Tag_Annotator()
  y1 <- annotate(x, list(sent_token_annotator, word_token_annotator))
  y2 <- annotate(x, pos_tag_annotator, y1)
  #  y3 <- annotate(x, Maxent_POS_Tag_Annotator(probs = TRUE), y1)
  y2w <- subset(y2, type == "word")
  tags <- sapply(y2w$features, '[[', "POS")
  r1 <- sprintf("%s/%s", x[y2w], tags)
  r2 <- paste(r1, collapse = " ")
  return(r2)  }  )

# inspect results
Corpus.tagged
#Guardar corpu con POST Tagger
Corpus.tagged<-data.frame(text=unlist(sapply(corpus, `[`, "content")), stringsAsFactors=F) #Crea dataframe p/exportar a csv
write.csv(corpusSaved, file = "C:/Users/Citlali/Documents/Universidad/9SW/Minería de datos/Corpus550-limpio-tagged.csv", row.names = F) #Exporta


