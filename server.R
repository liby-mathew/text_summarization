library(shiny)
library(tm)
library(wordcloud)
library(memoise)
library(udpipe)
library(textrank)
library(lattice)
library(igraph)
library(ggraph)
library(ggplot2)
library(wordcloud)
library(stringr)
library(wordcloud)
library(shinycssloaders)

options(shiny.maxRequestSize=30*1024^2)
shinyServer(function(input, output) {
  
  
  output$plot <- renderPlot({
    if(is.null(input$txtFile)) {
      datapath = "https://github.com/liby-mathew/text_summarization/blob/master/Chandrayaan2.txt"
    } else {
#      if(is.null(input$modelFile)){
 #       datapath = "https://github.com/liby-mathew/text_summarization/blob/master/english-ud-2.1-20180111.udpipe"
  #    }
      textData = readLines(file(input$txtFile$datapath), encoding = "UTF-8")
      textData  =  str_replace_all(textData, "<.*?>", "") 
      model = udpipe_load_model(input$modelFile$datapath)  
      
      x <- udpipe_annotate(model, x = textData) 
      x <- as.data.frame(x)
      
      print(input$checkbox)
      print(c("NOUN", "ADJ"))
      print(typeof(input$checkbox))
      print(typeof(c("NOUN", "ADJ")))
      #print(gsub("  ", " ", chckVal))
      
      n_cooc <- cooccurrence(     #$ try `?cooccurrence` for parm options
        x = subset(x, upos %in% c(input$checkbox)), 
        term = "lemma", 
        group = c("doc_id", "paragraph_id", "sentence_id"))  # 0.02 secs
      wordnetwork <- head(n_cooc, 50)
      wordnetwork <- igraph::graph_from_data_frame(wordnetwork) # needs edgelist in first 2 colms.
      
      ggraph(wordnetwork, layout = "fr") +  
        geom_edge_link(aes(width = cooc, edge_alpha = cooc), edge_colour = "orange") +  
        geom_node_text(aes(label = name), col = "blue", size = 4) +
        theme_graph(base_family = "Calibri") +  
        theme(legend.position = "none") +
        labs(title = "Cooccurrences within 3 words distance", subtitle = "Graph")
      
    }
  })

    
  # output$wcloud <- renderPlot({
  #   if(is.null(input$txtFile) || is.null(input$modelFile)){
  #     return(NULL)
  #   } else {
  #     textData = readLines(file(input$txtFile$datapath), encoding = "UTF-8")
  #     textData  =  str_replace_all(textData, "<.*?>", "") 
  #     model = udpipe_load_model(input$modelFile$datapath)  
  #     
  #     x <- udpipe_annotate(model, x = textData) 
  #     x <- as.data.frame(x)
  #     
  #     all_words = x %>% subset(., upos %in% c(input$checkbox)); all_words$token[1:20]
  #     
  #     all_words = txt_freq(all_words$lemma)
  #     head(all_words$key, 20) 
  #     
  #     
  #     wordcloud(words = all_words$key, 
  #               freq = all_words$freq, 
  #               min.freq = 2, 
  #               max.words = 100,
  #               random.order = FALSE,
  #               scale=c(4,2),
  #               colors = brewer.pal(8, "Dark2"),
  #               main="Title")
  #   }
  # }, height = 800, width = 1200 )
  # 
  # output$datatable <- renderDataTable({
  #   if(is.null(input$txtFile) || is.null(input$modelFile)){
  #     return(NULL)
  #   } else {
  #     textData = readLines(file(input$txtFile$datapath), encoding = "UTF-8")
  #     textData  =  str_replace_all(textData, "<.*?>", "") 
  #     model = udpipe_load_model(input$modelFile$datapath)  
  #     
  #     x <- udpipe_annotate(model, x = textData) 
  #     x <- as.data.frame(x)
  #     out = data.frame(row_name = row.names(x),x)
  #     out
  #   }
  # })
  
})
