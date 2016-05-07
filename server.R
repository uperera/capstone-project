library(shiny)
library(tm)
library(RWeka)
library(SnowballC)
library(caret)
library(stringr)
library(dplyr)
#load("c:/temp/capstone-project/data/unidf")
#load("c:/temp/capstone-project/data/bidf")
#load("c:/temp/capstone-project/data/tridf")
#load("c:/temp/capstone-project/data/fourdf")
#load("c:/temp/capstone-project/data/fivedf")
load("data/unidf")
load("data/bidf")
load("data/tridf")
load("data/fourdf")
load("data/fivedf")
funi <- unidf[unidf$counts >0,]
fbi <- bidf[bidf$count>0,]
ftri <- tridf[tridf$count >0,]
ffour <- fourdf[fourdf$count >0,]
ffive <- fivedf[fivedf$count >0,]
#mystopwords <- c("but")
#mystopwords <- c("and","i","we","they","he","she","it","the","at")
mystopwords <- c("and","i","we","they","he","she")
pat4 <- "[a-z]{1,} [a-z]{1,} [a-z]{1,} [a-z]{1,}$"
pat3 <- "[a-z]{1,} [a-z]{1,} [a-z]{1,}$"
pat2 <- "[a-z]{1,} [a-z]{1,}$"
pat1 <- "[a-z]{1,}$"
dis <- 0.75





       
predictwd <- function(phrase){
        phrase_txt <- VCorpus(VectorSource(phrase))
        phrase_txt <- tm_map(phrase_txt, content_transformer(tolower))      #converts to lower case
        #phrase_txt <- tm_map(phrase_txt, removeWords, words=mystopwords)
        phrase_txt <- tm_map(phrase_txt, removeWords, stopwords("english"))
        phrase_txt <- tm_map(phrase_txt, removePunctuation)                 #removes punctuation
        phrase_txt <- tm_map(phrase_txt, removeNumbers)                     #removes numerics
        phrase_txt <- tm_map(phrase_txt, stemDocument)                      #strips word suffixes retaining basic value
        phrase_txt <- tm_map(phrase_txt, stripWhitespace)                   #removes whitespace
        test <- as.character(phrase_txt[[1]])
        fourphrase <- paste("^",str_extract(test, pat4)," ",sep="")
        triphrase <- paste("^",str_extract(test, pat3)," ",sep="")
        biphrase <- paste("^",str_extract(test, pat2)," ",sep="")
        uniphrase <- paste("^",str_extract(test, pat1)," ",sep="")
        
        fivehist <- str_extract(test,pat4)
        fourhist <- str_extract(test,pat3)
        trihist <- str_extract(test, pat2)
        bihist <- str_extract(test,pat1)
        
        
        fivect <- ffive[grep(fourphrase,ffive$names) ,]
        fourct <- ffour[grep(triphrase,ffour$names) ,]
        trict <- ftri[grep(biphrase,ftri$names) ,]
        bict  <- fbi[grep(uniphrase, fbi$names),]
        finword <- ""
        finprob <- 0
        if (nrow(fivect) !=0) {
                for (i in fivect$names){
                        word5 <- str_extract(i,"[a-z]{1,}$")
                        for (j in fourct$names) {
                                word4 <-  str_extract(j,"[a-z]{1,}$")
                                if (word5==word4){
                                        teswd <- paste(j,"$",sep="")
                                        prob <- (fivect[fivect$names==i,]$counts -dis)/fourdf[fourdf$names==fivehist,]$counts +(dis*fivect[fivect$names==i,]$counts/fourdf[fourdf$names==fivehist,]$counts) *sum(fivedf[grep(teswd,fivedf$names),]$counts)/sum(fivedf$counts)
                                        if (finprob < prob){
                                                finword <-word5
                                                finprob <- prob
                                                
                                       }
                                          
                                }
                                
                        }
                }
                
        } else if(nrow(fourct)!=0)  {
       if(nrow(fourct)!=0)  {
                for (i in fourct$names){
                        word4 <- str_extract(i,"[a-z]{1,}$")
                        for (j in trict$names) {
                                word3 <-  str_extract(j,"[a-z]{1,}$")
                                if (word4==word3){
                                        teswd <- paste(j,"$",sep="")
                                        prob <- (fourct[fourct$names==i,]$counts -dis)/tridf[tridf$names==fourhist,]$counts +(dis*fourct[fourct$names==i,]$counts/tridf[tridf$names==fourhist,]$counts) *sum(fourdf[grep(teswd,fourdf$names),]$counts)/sum(fourdf$counts)
                                        if (finprob < prob){
                                                finword <-word4
                                                finprob <- prob
                                        
                                        }
                                        
                                        
                                }
                                
                        }
                }
       }
                
        } else if(nrow(trict) != 0) {
                for (i in trict$names){
                        word3 <- str_extract(i,"[a-z]{1,}$")
                        for (j in bict$names) {
                                        word2 <-  str_extract(j,"[a-z]{1,}$")
                                        if (word3==word2){
                                        teswd <- paste(j,"$",sep="")
                                        prob <- (trict[trict$names==i,]$counts -dis)/bidf[bidf$names==trihist,]$counts +(dis*trict[trict$names==i,]$counts/bidf[bidf$names==trihist,]$counts) *sum(tridf[grep(teswd,tridf$names),]$counts)/sum(tridf$counts)
                                        if (finprob < prob){
                                                finword <-word3
                                                finprob <- prob
                                                
                                        }
                                        
                                        
                                }
                                
                        }
                }
        }else if(nrow(bict) != 0) {
                
                for (i in bict$names){
                        word3 <- str_extract(i,"[a-z]{1,}$")
                        for (j in funi$names) {
                                word2 <-  j                                        
                                if (word3==word2){
                                        teswd <- paste(j,"$",sep="")
                                        prob <- (bict[bict$names==i,]$counts -dis)/unidf[unidf$names==bihist,]$counts +(dis*bict[bict$names==i,]$counts/unidf[unidf$names==bihist,]$counts) *sum(bidf[grep(teswd,bidf$names),]$counts)/sum(bidf$counts)
                                        if (finprob < prob){
                                                finword <-word3
                                                finprob <- prob
                                                
                                        }
                                }
                                
                                
                        }
                        
                        
                }
        }else{
                if(phrase!=""){
                      n <- sample(50:5000,1)
                      finword <- as.character(unidf$names[n])
                      return(finword)
                      
                }
        }
        
        
        
        
        
        #if(finword==""){
               # finword <- "unk"
        #}
        return(finword)
        
        
}

shinyServer(
        function(input,output){
                output$phrase <- renderPrint({input$phrase})
                output$word <- renderPrint(predictwd(input$phrase))
                #output$mu <- renderPrint({input$mu})
                #output$plot2 <- renderPlot({smean(input$ss,input$mu)})
                
        }
        )