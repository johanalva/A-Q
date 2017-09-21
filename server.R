library(shiny)
library(plotly)
library(rpivotTable)
library(DT)
library(shinydashboard)
library(httr)
library(plyr)
library(magrittr)
library(lubridate)
library(trelloR)
library(jsonlite)
library(rpivotTable)
library(dplyr)
library(stringr)
library(ggplot2)
library(Hmisc)

# Define server logic required to draw a histogram
myShiny <- shinyServer(function(input, output, session) {
    
    observeEvent(input$do, {
        
        ae <- "https://trello.com/b/i4u41Lhr/analytics-sdr.json"
        key <- 'd00e8afb53f716936477840488ad72f5'
        secret <- '7e46b4cff5903e5c74d88cc26fa262a1b347edacfabe7b95157f6ca52c67aa7f'
        
        key <- input$key
        secret <- input$secret
        
        my_token <- trello_get_token(key = key, secret = secret, appname = "trello")
        req <- httr::GET(ae, my_token, paging = TRUE)
        raws <- rawToChar(req$content)
        this.content <- fromJSON(raws)
        cards <- this.content$cards
        
        cardsDisplay <- select(cards, id, name, desc, closed, due, dueComplete,
                               labels, dateLastActivity, idMembers, idList)
        
        # Time mapping
        cardsDisplay$due <- as.POSIXct(strptime(paste0(cardsDisplay$due), 
                                                tz = "UTC", "%Y-%m-%dT%H:%M:%OSZ"))-(6*3600)
        cardsDisplay$dateLastActivity <- as.POSIXct(strptime(paste0(cardsDisplay$dateLastActivity), 
                                                             tz = "UTC", "%Y-%m-%dT%H:%M:%OSZ"))-(6*3600)
        
        # Cycle Time Calculation ----------------------------------------
        cardsDisplay$creationDate <- as.POSIXct(strtoi(paste0('0x',substr(cards$id,1,8))), 
                                                origin="1970-01-01")
        
        # Important: pass dates to same metric unit to create the cycle time
        cardsDisplay$creationDate <- as.character(cardsDisplay$creationDate)
        cardsDisplay$dateLastActivity <- as.character(cardsDisplay$dateLastActivity)
        
        for(i in 1:nrow(cardsDisplay)){
            if(cardsDisplay$closed[i] == TRUE){
                cardsDisplay$cycleTimeDays[i] <- round(difftime(cardsDisplay$dateLastActivity[i], cardsDisplay$creationDate[i], 
                                                                units = "days"), digits = 4)
            }else{
                cardsDisplay$cycleTimeDays[i] <- round(difftime(Sys.time(), cardsDisplay$creationDate[i], 
                                                                units = "days"), digits = 4)
            }
        }
        
        #cards$cycleTimeDays <- (cards$dateLastActivity - cards$creationDate)
        cardsDisplay$WeekEnding <- week(cards$dateLastActivity)
        cardsDisplay$Year <- year(cards$dateLastActivity)
        
        # Squad Creation
        for(i in 1:nrow(cardsDisplay)){
            if(dim(cardsDisplay$labels[[i]]) !=0){
                if(any(unlist(cardsDisplay$labels[[i]][3]) %in% "Account DA")){
                    cardsDisplay$Squad[i] <- "Account DA"
                }else if(any(unlist(cardsDisplay$labels[[i]][3]) %in% "LDA")){
                    cardsDisplay$Squad[i] <- "LDA"
                }else if(any(unlist(cardsDisplay$labels[[i]][3]) %in% "NA QA")){
                    cardsDisplay$Squad[i] <- "NA QA"
                }else if(any(unlist(cardsDisplay$labels[[i]][3]) %in% "PASIR")){
                    cardsDisplay$Squad[i] <- "PASIR"
                }else if(any(unlist(cardsDisplay$labels[[i]][3]) %in% "Service Engineering / BPO")){
                    cardsDisplay$Squad[i] <- "Service Engineering / BPO"
                }else if(any(unlist(cardsDisplay$labels[[i]][3]) %in% "IPC T&T SME")){
                    cardsDisplay$Squad[i] <- "IPC T&T SME"
                }
            }else{
                cardsDisplay$Squad[i] <- "No Squad Selected"
            }
        }
        
        
        # # List Creation
        for(i in 1:nrow(cardsDisplay)){
            if(cardsDisplay$idList[i] %in% this.content$lists$id[1]){
                cardsDisplay$List[i] <- "Templates"
            }else if(cardsDisplay$idList[i] %in% this.content$lists$id[2]){
                cardsDisplay$List[i] <- "Backlog"
            }else if(cardsDisplay$idList[i] %in% this.content$lists$id[3]){
                cardsDisplay$List[i] <- "WIP [38]"
            }else if(cardsDisplay$idList[i] %in% this.content$lists$id[4]){
                cardsDisplay$List[i] <- "On Hold"
            }else if(cardsDisplay$idList[i] %in% this.content$lists$id[5]){
                cardsDisplay$List[i] <- "Accepted"
            }
        }
        
        # Metrics Creation
        cardsDisplay$Green <- "n"
        cardsDisplay$Amber <- "n"
        cardsDisplay$Red <- "n"
        cardsDisplay$Overtime <- "n"
        cardsDisplay$Re_Work <- "n"
        cardsDisplay$Task <- "n"
        cardsDisplay$Highlight <- "n"
        cardsDisplay$Lowlight <- "n"
        cardsDisplay$Blocker <- "n"
        cardsDisplay$HighImportance <- "n"
        cardsDisplay$Automation <- "n"
        cardsDisplay$Project_P1 <- "n"
        cardsDisplay$Project_P2 <- "n"
        cardsDisplay$Project_P3 <- "n"
        cardsDisplay$Project_P4 <- "n"
        
        metricsEvaluate <- function(x){
            if(any(unlist(cardsDisplay$labels[[i]][3]) %in% x)){
                metric <- x
            }else{
                metric <- "n"
            }
        }
        
        for(i in 1:nrow(cardsDisplay)){
            if(dim(cardsDisplay$labels[[i]]) !=0){
                cardsDisplay$Green[i] <- metricsEvaluate("Green")
                cardsDisplay$Amber[i] <-metricsEvaluate("Amber")
                cardsDisplay$Red[i] <- metricsEvaluate("Red")
                cardsDisplay$Overtime[i] <- metricsEvaluate("Overtime")
                cardsDisplay$Re_Work[i] <- metricsEvaluate("Re-Work")
                cardsDisplay$Task[i] <- metricsEvaluate("Task")
                cardsDisplay$Highlight[i] <- metricsEvaluate("Highlight")
                cardsDisplay$Lowlight[i] <- metricsEvaluate("Lowlight")
                cardsDisplay$Blocker[i] <- metricsEvaluate("Blocker")
                cardsDisplay$HighImportance[i] <- metricsEvaluate("High Importance")
                cardsDisplay$Automation[i] <- metricsEvaluate("Automation")
                cardsDisplay$Project_P1[i] <- metricsEvaluate("Project_P1")
                cardsDisplay$Project_P2[i] <- metricsEvaluate("Project_P2")
                cardsDisplay$Project_P3[i] <- metricsEvaluate("Project_P3")
                cardsDisplay$Project_P4[i] <- metricsEvaluate("Project_P4")
            }
        }
        
        # Members:
        cardsDisplay$Assignee1 <- 'n'
        cardsDisplay$Assignee2 <- 'n'
        cardsDisplay$Assignee3 <- 'n'
        cardsDisplay$Assignee4 <- 'n'
        cardsDisplay$Assignee5 <- 'n'
        
        # Functions to obtain Assignee name
        assigneeMember <- function(i, pos){
            if(any(unlist(cardsDisplay$idMembers[[i]][pos]) %in% this.content$members$id)){
                id.Identify <- (unlist(cardsDisplay$idMembers[[i]][pos]))
                memberName <- subset(this.content$members, id == id.Identify)
                memberName <- memberName[6]
            }else
                memmberName <- 'n'
        }
        
        # Cycle to fill Members columns
        for(i in 1:nrow(cardsDisplay)){
            if(length(cardsDisplay$idMembers[[i]]) !=0){
                cardsDisplay$Assignee1[i] <- as.character(assigneeMember(i,1))
                cardsDisplay$Assignee2[i] <- as.character(assigneeMember(i,2))
                cardsDisplay$Assignee3[i] <- as.character(assigneeMember(i,3))
                cardsDisplay$Assignee4[i] <- as.character(assigneeMember(i,4))
                cardsDisplay$Assignee5[i] <- as.character(assigneeMember(i,5))
            }
            else{
                cardsDisplay$Assignee1[i] <- 'No any member assignee to this card'
            }
        }
        
        cardsDisplay <- select(cardsDisplay,-c(labels, idList, idMembers))
        
        
        # Agile Metrics -----------------------------------------------------------
        # Subset variables
        agile.metrics <- select(subset(cardsDisplay, closed == TRUE), WeekEnding, 
                                Year, Squad, cycleTimeDays)
        # Throughput calculations
        Throughput <- count_(agile.metrics, vars = c("Squad", "WeekEnding", "Year"))
        
        # Cycle Time Calculation
        cycleT <- aggregate(cycleTimeDays ~ WeekEnding + Year + Squad, agile.metrics, mean)
        cycleT$cycleTimeDays <- round(cycleT$cycleTimeDays, digits = 2)
        # Mergin Data
        agile.metrics <- merge(Throughput, cycleT, by = c("Year", "WeekEnding", "Squad"))
        colnames(agile.metrics)[4] <- "Throughput"
        
        # Delivery Rate calculation
        agile.metrics$Delivery_Rate <- 
            round(agile.metrics$Throughput / agile.metrics$cycleTimeDays, digits = 2)
        
        # Sort data
        agile.metrics <- agile.metrics[order(agile.metrics$Year,agile.metrics$WeekEnding,
                                             agile.metrics$Squad),]
        
        output$responseText <- renderText({
            "Your data is ready to download"
        })
        
        # To download data
        output$downloadData <- downloadHandler(
            filename = function(){paste('Analytics.csv')},
            content = function(file){
                write.csv(cardsDisplay, file)
            }
        )
        
        output$downloadTP <- downloadHandler(
            filename = function(){paste('Analytics-AgileMetric.csv')},
            content = function(file){
                write.csv(agile.metrics, file)
            }
        )
        
        #close the R session when Chrome closes
        session$onSessionEnded(function() {
            stopApp()
            q("no")
        })
        
    })
})
