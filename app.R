library(shiny)
library(ggplot2)
library(stringr)
library(htmltools)
library(dplyr) 
library(tm)
library(NLP)

#setwd("C:/Users/TOSHIBA-PC/Desktop/R_Programming/Capstone Project/TTT/Final_Capstone_Project")

bi_words  <- readRDS("./bi_words_fast.rds")
tri_words <- readRDS( "./tri_words_fast.rds")
quad_words <- readRDS("./quad_words_fast.rds")
quint_words <- readRDS("./quint_words_fast.rds")
sext_words <- readRDS("./sext_words_fast.rds")


bigram <- function(input_words){
    num <- length(input_words)
    filter(bi_words,
           word1==input_words[num]) %>% 
        top_n(1, n) %>%
        filter(row_number() == 1L) %>%
        select(num_range("word", 2)) %>%
        as.character() -> out
    ifelse(out =="character(0)", "?", return(out))
}


trigram <- function(input_words){
    num <- length(input_words)
    filter(tri_words, 
           word1==input_words[num-1], 
           word2==input_words[num])  %>% 
        top_n(1, n) %>%
        filter(row_number() == 1L) %>%
        select(num_range("word", 3)) %>%
        as.character() -> out
    ifelse(out=="character(0)", bigram(input_words), return(out))
}


quadgram <- function(input_words){
    num <- length(input_words)
    filter(quad_words,
           word1==input_words[num-2], 
           word2==input_words[num-1], 
           word3==input_words[num])  %>% 
        top_n(1, n) %>%
        filter(row_number() == 1L) %>%
        select(num_range("word", 4)) %>%
        as.character() -> out
    ifelse(out=="character(0)", trigram(input_words), return(out))
}

quintdgram <- function(input_words){
    num <- length(input_words)
    filter(quint_words,
           word1==input_words[num-3],
           word2==input_words[num-2], 
           word3==input_words[num-1], 
           word4==input_words[num])  %>% 
        top_n(1, n) %>%
        filter(row_number() == 1L) %>%
        select(num_range("word", 5)) %>%
        as.character() -> out
    ifelse(out=="character(0)", quadgram(input_words), return(out))
}

sextdgram <- function(input_words){
    num <- length(input_words)
    filter(sext_words,
           word1==input_words[num-4],
           word2==input_words[num-3],
           word3==input_words[num-2], 
           word4==input_words[num-1], 
           word5==input_words[num])  %>% 
        top_n(1, n) %>%
        filter(row_number() == 1L) %>%
        select(num_range("word", 6)) %>%
        as.character() -> out
    ifelse(out=="character(0)", quintgram(input_words), return(out))
}


ngrams <- function(input){
    # Create a dataframe
    input <- data_frame(text = input)
    # Clean the Inpput
    replace_reg <- "[^[:alpha:][:space:]]*"
    input <- input %>%
        mutate(text = str_replace_all(text, replace_reg, ""))
    # Find word count, separate words, lower case
    input_count <- str_count(input, boundary("word"))
    input_words <- unlist(str_split(input, boundary("word")))
    input_words <- tolower(input_words)
    # Call the matching functions
    out <- ifelse(input_count == 1, bigram(input_words), 
                  ifelse (input_count == 2, trigram(input_words), quadgram(input_words)))
    # Output
    return(out)
}


Unicloud <- base64enc::dataURI(file="unicloud.png", mime="image/png")
Bicloud <- base64enc::dataURI(file="bicloud.png", mime="image/png")
Tricloud <- base64enc::dataURI(file="tricloud.png", mime="image/png")
Quadcloud <- base64enc::dataURI(file="quadcloud.png", mime="image/png")
Quintcloud <- base64enc::dataURI(file="quintcloud.png", mime="image/png")
Sextcloud <- base64enc::dataURI(file="sextcloud.png", mime="image/png")

Unigrams <- base64enc::dataURI(file="unigrams.png", mime="image/png")
Biigrams <- base64enc::dataURI(file="biigrams.png", mime="image/png")
Trigrams <- base64enc::dataURI(file="trigrams.png", mime="image/png")
Quadgrams <- base64enc::dataURI(file="quadgrams.png", mime="image/png")
Quintgrams <- base64enc::dataURI(file="quintgrams.png", mime="image/png")
Sextgrams <- base64enc::dataURI(file="sextgrams.png", mime="image/png")

Uniwords <- read.csv("uni_words.csv", sep=",")
Biwords <- read.csv("bi_words.csv", sep=",")
Triwords <- read.csv("tri_words.csv", sep=",")
Quadwords <- read.csv("quad_words.csv", sep=",")
Quintwords <- read.csv("quint_words.csv", sep=",")
Sextwords <- read.csv("sext_words.csv", sep=",")

ui <- fluidPage(
    
    titlePanel(title = h3(strong("R DATA SCIENCE CAPSTONE - NLP WORD PREDICTION"), align = "center")),
    sidebarLayout(
        sidebarPanel(
            h4("Instructions:"),
            h5("1. Enter a word or phrase or sectnce in the text box."),
            h5("2. The predicted next word prints below it in blue."),
            h5("3. A question mark means no prediction."),
            h5("4. Ngrams frequency table, just choose Nword from the drop down menu to display."),
            h5("5. Additional tab for Wordcloud and plots of the top ngrams in the dataset."),
            a("github Source Code", href = "https://github.com/HayfordTT/R_Capstone-Project"),
            
            
            selectInput("freq", "Choose a word Frequency: ",
                        choices = c("Uniwords", "Biwords",
                                    "Triwords", "Quadwords",
                                    "Quintwords","Sextwords")),
            
            downloadButton("downloadData", "Download"),
            
            
        ),
        mainPanel(width = 8, 
                  tabsetPanel(type="tab", 
                              
                              tabPanel(title = 'Prediction Model',
                                       textInput("inputText", h4("Enter a Word or Phrase Here:", style="color: darkblue"),
                                                 value = " You word  or phrase here!"),
                                       
                                       strong(code(verbatimTextOutput("placeholder", placeholder = TRUE))),
                                       
                                       br(),
                                       h3("Predicted Next Word: " , style  = "color: darkred"),
                                       #h4(textOutput("prediction"), style="color:blue"),
                                       (strong(code(verbatimTextOutput("prediction"))))),
                              br(),
                              
                              tabPanel(title = 'NWord Frequency Table',
                                       DT::dataTableOutput("table")),
                              
                              br(),
                              
                              tabPanel(title = 'Ngrams Plots',
                                       
                                       img(src = Unigrams, height = 450, width = 720), 
                                       img(src = Biigrams, height = 450, width = 720),
                                       img(src = Trigrams, height = 450, width = 720),
                                       img(src = Quadgrams, height = 450, width = 720),
                                       img(src = Quintgrams, height = 450, width = 720),
                                       img(src = Sextgrams, height = 750, width = 720)),
                              br(),
                              
                              tabPanel(title = 'Wordcloud',
                                       
                                       img(src = Unicloud, height = 450, width = 750), 
                                       img(src = Bicloud, height = 450, width = 750),
                                       img(src = Tricloud, height = 450, width = 750),
                                       img(src = Quadcloud, height = 450, width = 750),
                                       img(src = Quintcloud, height = 450, width = 750),
                                       img(src = Sextcloud, height = 450, width = 750))
                              
                  )
        )
    )
    
    
    
)



server <- function(input, output) {
    
    
    output$prediction <- renderPrint({
        result <- ngrams(input$inputText)
        
        result
    });
    
    output$placeholder <- renderText({ input$inputText})
    
    
    freqInput <- reactive({
        switch(input$freq,
               "Uniwords" = Uniwords,
               "Biwords" = Biwords,
               "Triwords" = Triwords,
               "Quadwords" = Quadwords,
               "Quintwords" = Quintwords,
               "Sextwords" = Sextwords)
    })
    
    output$table <- DT::renderDataTable(DT::datatable({
        
        freqInput()
    }))
    
    # Downloadable csv of selected dataset 
    output$downloadData <- downloadHandler(
        filename = function() {
            paste(input$freq, ".csv", sep = "")
        },
        content = function(file) {
            write.csv(freqInput(), file, row.names = FALSE)
        })
    
    
}

shinyApp(ui, server)

