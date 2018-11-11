library(keras)

GLOVE_DIR <- 'glove.6B'
TEXT_DATA_DIR <- '20_newsgroup'
MAX_SEQUENCE_LENGTH <- 1000
MAX_NUM_WORDS <- 20000
EMBEDDING_DIM <- 100
VALIDATION_SPLIT <- 0.2


embeddings_index <- new.env(parent = emptyenv())
lines <- readLines(file.path("Data", "glove.6B.300d.txt"))
for (line in lines) {
  values <- strsplit(line, ' ', fixed = TRUE)[[1]]
  word <- values[[1]]
  coefs <- as.numeric(values[-1])
  embeddings_index[[word]] <- coefs
}
save(embeddings_index,file = "Data/embeding_index.rda")

load("Data/embeding_index.rda")

texts <- character()
labels <- integer()
labels_index <- list()

for (name in list.files("Data/20_newsgroup")) {
  path <- file.path("Data/20_newsgroup", name)
  if (file_test("-d", path)) {
    label_id <- length(labels_index)
    labels_index[[name]] <- label_id
    for (fname in list.files(path)) {
      if (grepl("^[0-9]+$", fname)) {
        fpath <- file.path(path, fname)
        t <- readLines(fpath, encoding = "utf8")
        t <- paste(t, collapse = "\n")
        i <- regexpr(pattern = "\n\n", t, fixed = TRUE)[[1]]
        if (i != -1L)
          t <- substring(t, i)
        texts <- c(texts, t)
        labels <- c(labels, label_id)
      }
    }
  }
}
save(texts,labels,label_id,file = "Data/newsdata.rda")

load("Data/newsdata.rda")


tokenizer <- text_tokenizer(num_words=MAX_NUM_WORDS)
tokenizer %>% fit_text_tokenizer(texts)


sequences <- texts_to_sequences(tokenizer, texts)

word_index <- tokenizer$word_index

data <- pad_sequences(sequences, maxlen=MAX_SEQUENCE_LENGTH)
labels <- to_categorical(labels)


