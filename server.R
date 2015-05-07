library(RColorBrewer)
library(rJava)
library(shiny)
library(wordcloud) 
library(Rwordseg)
shinyServer(function(input,output){
  txt <-reactive({
    infile=input$data
    test <-readLines(infile$datapath)
    test1 <- test[nchar(test)>20]
    test1 <- gsub("[0-9%]","",test1)
    segmentCN(test1)
  })
  stopwords <-reactive({
    infile=input$stopword
    unlist(read.table(infile$datapath))
  })
  wordsNum <- reactive({
    remove <- function(x) {
      x[!(x%in%stopwords())]
    }
    test2 <-lapply(txt(),remove)
    wordsNum <- table(unlist(test2))
    sort(wordsNum)
  })
  output$频数 <- renderPrint({
    sort(wordsNum(),decreasing = T)
  })
  output$词云 <- renderPlot({ 
    wordsData <- data.frame(words =names(wordsNum()), freq = wordsNum())
    weibo.top150 <- tail(wordsData,150)
    colors <- brewer.pal(8,"Dark2")
    wordcloud(weibo.top150$words,weibo.top150$freq,scale=c(5,0.1),colors=colors,random.order=F)
  })
})
