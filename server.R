
# This is the server logic for a Shiny web application.
# You can find out more about building applications with Shiny here:
#
# http://shiny.rstudio.com
#

library(shiny)

shinyServer(function(input, output, session) {

  datatables <- reactiveValues()
  
  #Establish reactive observers to read table data
  observeEvent(input$FBAFeeTable, {
    if(is.null(input$FBAFeeTable[["datapath"]]) == FALSE){
    datatables$FBAFeeTable <- read.csv(file = input$FBAFeeTable[["datapath"]], header = TRUE)
    }
  })

  observeEvent(input$ShipFeeTable, {
    if(is.null(input$ShipFeeTable[["datapath"]]) == FALSE){
      datatables$ShipFeeTable <- read.csv(file = input$ShipFeeTable[["datapath"]], header = TRUE)
    }
  })
  
#   #Establish button tracker for start/stop buttons
#   runbutton <- reactiveValues(data = FALSE)
#   
#   observeEvent(input$deals_start, {
#     runbutton$data <- TRUE
#   })
#   
#   observeEvent(input$deals_stop, {
#     runbutton$data <- FALSE
#   })  
  
  #Establish starting data frame to be continually updated
  dealdataframe <- reactiveValues()
  dealdataframe$df <- data.frame()
  
  DealsCurrent <- reactive({
    
    newdealsdataframe <- data.frame()
    notanalyzeddataframe <- data.frame()
    
    if(input$deals_start > 0){
      
      #Get emails from slickdeals alert emails
      slickemails <- newmessages(searchterm = "slickdeals")
      
      if(length(slickemails != 0 )){
      
          for(i in 1:min(length(slickemails), 4)){        
          newdeals <- data.frame()
      
      try(newdeals <- newslick(message.id = slickemails[i], FBAFeeTable = datatables$FBAFeeTable, ShipFeeTable = datatables$ShipFeeTable, UPSrate = as.numeric(input$UPSrate),
                               AWSAccessKeyId = input$AWSAccessKeyId, MarketplaceId = input$MarketplaceId,
                               SellerId = input$SellerId, AWSSecretKey = input$AWSSecretKey))
      
      
      
      #If margin is above a certain percent and modelmatch and weightedmatch are above a certain threshhold, send an email alert with deal content
      if(length(newdeals$slickanalyzed) > 0){
        
        
        alertframe <- data.frame(newdeals$slickanalyzed, stringsAsFactors = FALSE)
        
        #Look for deals meeting certain minimum alert criteria
        alert <- intersect(which(alertframe[["Margin"]] > input$MarginAlert), intersect(which(alertframe[["ModelMatch"]] > input$ModelMatchAlert), which(alertframe[["WeightedMatch"]] > input$WeightedMatchAlert)))
        
        #If deals matching this criteria are found, send an alert email with the deal information to the email addresses specified by the shiny UI inputs.
        if(length(alert) > 0){
        
        alerttext <- paste(colnames(alertframe), ": ", apply(alertframe[alert,], MARGIN = 1, FUN = "["), "\n")
            
        alertmessage <- mime(From= "psbox1123@gmail.com", To = input$alertemail, subject = "Deals Found",
                             body = alerttext)
        
        send_message(alertmessage)
        
        }
        
      }
      
      newdealsdataframe <- rbind.data.frame(data.frame(newdealsdataframe, stringsAsFactors = FALSE), data.frame(newdeals$slickanalyzed, stringsAsFactors = FALSE))
      notanalyzeddataframe <- rbind.data.frame(data.frame(notanalyzeddataframe, stringsAsFactors = FALSE), data.frame(newdeals$slicknotanalyzed, stringsAsFactors = FALSE))
    
      
      #Rerun this reactive function in 600000 milliseconds if this code is executed when the reactive expression is run
      
      
        }
      
      }
      
      isolate(filetime <- gsub(":", "-", Sys.time(), fixed = TRUE))
      
      if(is.null(newdealsdataframe$slicklink) == FALSE){
      
      write.csv(newdealsdataframe, file = paste0("dealmatrix", filetime, ".csv"))
      
      }
      
      if(is.null(notanalyzeddataframe$slicklink) == FALSE){
      
      write.csv(notanalyzeddataframe, file = paste0("nodealmatrix", filetime, ".csv"))
      
      }
    
      #Rerun this reactive section after xxx milliseconds
      invalidateLater(600000, session)
    
    }
    
    
    return(newdealsdataframe)
    
  })
  
  DealMatrixCombine <- reactive({
    
    
    
    if(isolate(input$deals_start == 0) | nrow(DealsCurrent()) == 0){
      
      isolate(dealframe <- dealdataframe$df)
      
    }
    else{
    
#       if(isolate(nrow(DealsCurrent() == 0)){
#         
#         dealframe <- data.frame()
#         
#       }
#       else{
#         
#         isolate(dealframe <- DealsCurrent())
#         
#       }
      
      
    dealframe <- DealsCurrent()
    isolate(dealframe <- rbind.data.frame(data.frame(dealdataframe$df, stringsAsFactors = FALSE), data.frame(dealframe, stringsAsFactors = FALSE)))
    isolate(dealdataframe$df <- dealframe)
    isolate(filetime <- Sys.time())
    isolate(write.csv(dealdataframe$df, file = "LiveDeals.csv"))
    
    }
    
    return(dealframe)
    
  })
  
  
#   DealMatrixStore <- reactive({
#     
#     if(nrow(DealMatrixCombine()) == 0){
#       
#       dealframe <- data.frame()
#       
#     }
#     else{
#       dealframe <- DealMatrixCombine()
#     }
#     
#     return(dealframe)
#     
#   })
  
  
  
  output$dealtable <- renderDataTable({
    
    DealMatrixCombine()
    
  })


})
