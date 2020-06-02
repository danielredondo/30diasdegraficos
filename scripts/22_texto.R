#  ----- Día 22: datos textuales -----

# Modificación de http://www.sthda.com/upload/rquery_wordcloud.r

rquery.wordcloud <- function(x, type = c("text", "url", "file"),
                             lang = "english", excludeWords = NULL,
                             textStemming = FALSE, colorPalette = "Dark2",
                             min.freq = 3, max.words = 200) {
  library("tm")
  library("SnowballC")
  library("wordcloud")
  library("RColorBrewer")

  if (type[1] == "file") {
    text <- readLines(x, encoding = "UTF-8")
    text <- gsub("¿", "", text)
    text <- gsub("¡", "", text)
  }
  else if (type[1] == "url") {
    text <- html_to_text(x)
  } else if (type[1] == "text") text <- x

  # Load the text as a corpus
  docs <- Corpus(VectorSource(text))
  # Convert the text to lower case
  docs <- tm_map(docs, content_transformer(tolower))
  # Remove numbers
  docs <- tm_map(docs, removeNumbers)
  # Remove stopwords for the language
  docs <- tm_map(docs, removeWords, stopwords(lang))
  # Remove punctuations
  docs <- tm_map(docs, removePunctuation)
  # Eliminate extra white spaces
  docs <- tm_map(docs, stripWhitespace)
  # Remove your own stopwords
  if (!is.null(excludeWords)) {
    docs <- tm_map(docs, removeWords, excludeWords)
  }
  # Text stemming
  if (textStemming) docs <- tm_map(docs, stemDocument)
  # Create term-document matrix
  tdm <- TermDocumentMatrix(docs)
  m <- as.matrix(tdm)
  v <- sort(rowSums(m), decreasing = TRUE)
  d <- data.frame(word = names(v), freq = v)
  # check the color palette name
  if (!colorPalette %in% rownames(brewer.pal.info)) {
    colors <- colorPalette
  } else {
    colors <- brewer.pal(8, colorPalette)
  }
  # Plot the word cloud
  set.seed(1234)
  wordcloud(d$word, d$freq,
    min.freq = min.freq, max.words = max.words,
    random.order = FALSE, rot.per = 0.35,
    use.r.layout = FALSE, colors = colors
  )

  invisible(list(tdm = tdm, freqTable = d))
}

#++++++++++++++++++++++
# Helper function
#++++++++++++++++++++++
# Download and parse webpage
html_to_text <- function(url) {
  library(RCurl)
  library(XML)
  # download html
  html.doc <- getURL(url)
  # convert to plain text
  doc <- htmlParse(html.doc, asText = TRUE)
  # "//text()" returns all text outside of HTML tags.
  # We also donâ€™t want text such as style and script codes
  text <- xpathSApply(doc, "//text()[not(ancestor::script)][not(ancestor::style)][not(ancestor::noscript)][not(ancestor::form)]", xmlValue)
  # Format text vector into one character string
  return(paste(text, collapse = " "))
}



# source('http://www.sthda.com/upload/rquery_wordcloud.r')
filePath <- "https://raw.githubusercontent.com/vladwelt/buscador/master/documentos/Romancero%20gitano%20-%20Federico%20Garcia%20Lorca.txt"
res <- rquery.wordcloud(filePath, type = "file", lang = "spanish", max.words = 100)
png("22.png", width = 6, height = 6, units = "in", res = 900)
res <- rquery.wordcloud(filePath, type = "file", lang = "spanish", max.words = 100)
dev.off()
