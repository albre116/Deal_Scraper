
newslick <- function(message.id, FBAFeeTable, ShipFeeTable, UPSrate, AWSAccessKeyId, MarketplaceId, SellerId, AWSSecretKey, knowndeals = c()){
  #Function to read new emails from slickdeals deal alerts and return price, matching ASIN, and slickdeals thread attributes
  #authorize access using the following command:
  #gmail_auth("Gmailaccessfile.json", scope = "full")
  #Inputs:
  #message.id: Gmail message id as string
  #FBAFeeTable: Table of FBA selling commission fees broken down by category, including an abbreviation to match the categories used on FBA
  #ShipFeeTable: Table of FBA shipping and handling fees broken down by product size and weight
  #UPSrate: shipping rate per lb charged by UPS for inbound shipping to amazon FBA warehouse
  #AWSAccessKeyId: Amazon Product MWS API credential
  #MarketplaceId: Amazon Product MWS API credential
  #SellerId: Amazon Product MWS API credential
  #AWSSecretKey: Amazon Product MWS API credential
  
  library(httr)
  library(rvest)
  library(qdap)
  library(gmailr)
  library(XML)
  
  #meta slickdeals steps:
  #create blank list to hold output analysis for items where price was found
  slickanalyzed <- list()
  
  #Create blank list to hold output analysis for items where price was not found but link was found
  slicknotanalyzed <- list()
      
      #Get details of first message in list and delete message
      slickmessage <- messageget(message.id = message.id, deleteread = TRUE)
  
#   #debug#
#   slickmessage <- messageget(message.id = message.id, deleteread = FALSE)
#   #debug#
  
      #Get deal alert links from slickdeals
      slicklink <- slickmaillink(emailbody = slickmessage$body)
      
      if(length(slicklink) > 0){
        for(j in 1:length(slicklink)){
          
          ##UPGRADE to get details for each link in email that has price and ignore others and create loop or better yet FUNCTION
          slickdetails <- slickget(slickURL = slicklink[j])
          
          #debug
          print("Done Getting Slickdeals Site")
          #debug
          
          if(length(slickdetails$deallinks) > 0){
            for(k in 1:length(slickdetails$deallinks)){
              
              collectiontime <- as.character(as.POSIXct(Sys.time(), tz = Sys.timezone))
              
              #If no price found for link, dump details into slicknotanalyzed list
              if(is.na(slickdetails$price[[k]]) == TRUE){
                
                slicknotanalyzed$slicklink[length(slicknotanalyzed$slicklink) + 1] <- slicklink[j]
                slicknotanalyzed$dealtitle[length(slicknotanalyzed$dealtitle) + 1] <- slickdetails$dealtitle
                slicknotanalyzed$deallink[length(slicknotanalyzed$deallink) + 1] <- slickdetails$deallinks[[k]]
                slicknotanalyzed$whynotanalyzed[length(slicknotanalyzed$whynotanalyzed) + 1] <- "Could not match price to link in slickdeals"
                slicknotanalyzed$timeanalyzed[length(slicknotanalyzed$timeanalyzed) + 1] <- collectiontime
              }
              else{
                
                #search with specific deal text first.
                #UPGRADE# Loop again with deal title if no matches are found
                #split string into components
                amzsearchstring <- unlist(strsplit(slickdetails$pricetext[[k]], split = " "))
                
                ##Remove tagged price string
                amzsearchstring[grep("\\$", amzsearchstring)] <- ""
                
                #Remove stop words (words that do not contain any real information due to being too common, uses top 25 word list from qdap)
                amzsearchstring <- gsub(paste(Top25Words, collapse = "|"), "", tolower(amzsearchstring), fixed = TRUE)
                
                #Remove all punctuation characters that may create problems with amazon product search
                amzsearchstring <- strip(amzsearchstring, char.keep = c(".", "-"), digit.remove = FALSE)
                
                #Remove all zero length remaining strings
                amzsearchstring <- amzsearchstring[which(nchar(amzsearchstring) > 0)]
                
                amzsearchparsed <- c()
                
                #debug
                print("Start Amazon Product Search")
                #debug
                
                #loop amazon search constantly shortening search string from the right until the maximum number of search results (10) is returned
                for (l in 1:length(amzsearchstring)){
                  
                  amzsearchstringloop <- amzsearchstring[1:(length(amzsearchstring) + 1 - l)]
                  
                  #Send deal title to amazon listmatching product
                  amzsearchreturn <- amazon.MWSPost(AWSAccessKeyId = AWSAccessKeyId, Action = "ListMatchingProducts", MarketplaceId = MarketplaceId, SellerId = SellerId, AWSSecretKey = AWSSecretKey, Query = paste(amzsearchstringloop, collapse = " "))
                  
                  #Exit function if amazon MWS post timed out
                  if(is.null(amzsearchreturn) == TRUE){stop("Timeout")}
                    
                  #Parse list matching product response
                  amzreturn <- amazon.searchparse(amzsearchreturn)
                  
                  amzsearchloop <- data.frame(amzreturn, stringsAsFactors = FALSE)
                  
                  #Bind results together
                  amzsearchparsed <- rbind(amzsearchparsed, amzsearchloop)
                  
                  if(length(amzsearchloop[[1]]) > 9){l <- (length(amzsearchstring))}
                  
                  Sys.sleep(5)
                  
                }
                
                #debug
                print("Done with Amazon Product Search")
                #debug
                
                #Remove duplicate values from product search results
                amzsearchparsed <- subset(amzsearchparsed, !duplicated(amzsearchparsed$ASIN))
                
                #Eliminate model and part numbers listed in amazon as NA
                amzsearchparsed$Model[which(amzsearchparsed$Model == "NA")] <- ""
                amzsearchparsed$PartNumber[which(amzsearchparsed$PartNumber == "NA")] <- ""
                
                #Get raw response from store webpage
                rawscrape <- capture.output(htmlParse(GET(slickdetails$deallinks[[k]])))
                
                #debug
                print("Done Getting Store Website")
                #debug
                
                #Calculate best ASIN match and return as likelyASIN
                
                likelyASIN <- c()
                
                #First try entire deal title. If no model match, then remove numbers from title and search again. If still no match, report partial match as winner
                
                if(length(amzsearchparsed$ASIN) == 0){
                  
                  slicknotanalyzed$slicklink[length(slicknotanalyzed$slicklink) + 1] <- slicklink[j]
                  slicknotanalyzed$dealtitle[length(slicknotanalyzed$dealtitle) + 1] <- slickdetails$dealtitle
                  slicknotanalyzed$deallink[length(slicknotanalyzed$deallink) + 1] <- slickdetails$deallinks[[k]]
                  slicknotanalyzed$whynotanalyzed[length(slicknotanalyzed$whynotanalyzed) + 1] <- "Zero results returned for Amazon MWS ListMatchingProduct"
                  slicknotanalyzed$timeanalyzed[length(slicknotanalyzed$timeanalyzed) + 1] <- collectiontime
                  
                }
                else{
                  
                  #Calculate percent match of strings for each amazon search attribute to scraped store page
                  storematch <- data.frame(strmatchpercent(amzsearchparsed, rawscrape))
                  
                  #Calculate percent match of strings for each amazon search attribute to specific link deal text
                  slickmatchspecific <- data.frame(strmatchpercent(amzsearchparsed, slickdetails$pricetext[[k]]))
                  
                  #Calculate percent match of strings for each amazon search attribute to slickdeals title plus body text
                  slickmatch <- data.frame(strmatchpercent(amzsearchparsed, paste(slickdetails$dealtitle, slickdetails$dealtext)))
                  
                  #########Calculate weighted match percentage based off of individual categories ##UPGRADE### - TUEN THIS!! based on training data
                  matchvect <- c(rep(0, times = length(amzsearchparsed$ASIN)))
                  
                  matchvect <- storematch$TitleMatch + storematch$ASINMatch + storematch$ManufacturerMatch + pmax(storematch$ModelMatch, storematch$PartNumberMatch) +
                    slickmatchspecific$TitleMatch + slickmatchspecific$ASINMatch + slickmatchspecific$ManufacturerMatch +pmax(slickmatchspecific$ModelMatch, slickmatchspecific$PartNumberMatch) +
                    slickmatch$TitleMatch + slickmatch$ASINMatch + slickmatch$ManufacturerMatch +pmax(slickmatch$ModelMatch, slickmatch$PartNumberMatch)
                  
                  amzsearchparsed$WeightedMatch <- matchvect
                  amzsearchparsed$ModelMatchStore <- pmax(storematch$ModelMatch, storematch$PartNumberMatch)
                  
                  
                  #Identify most likely match of all amazon search results that had matching model number
                  partmodelmatch  <- amzsearchparsed[(storematch$ModelMatch %in% 1)|(storematch$PartNumberMatch %in% 1),]
                  likelyASIN <- partmodelmatch[which.max(partmodelmatch$WeightedMatch),]
                  
                  #TUNE THIS##If no exact matches found, remove requirement to match model number exactly and find the maximum weighted value match
                  
                  if(length(likelyASIN$ASIN) == 0){
                    
                    likelyASIN <- amzsearchparsed[which.max(amzsearchparsed$WeightedMatch),]
                    
                  }
                  
                  #Get price and rank details for identified ASIN match
                  amzpricereturn <- amazon.MWSPost(ASIN = likelyASIN$ASIN, AWSAccessKeyId = AWSAccessKeyId, Action = "GetCompetitivePricingForASIN", MarketplaceId = MarketplaceId, SellerId = SellerId, AWSSecretKey = AWSSecretKey)
                  
                  #debug
                  print("Done with Amazon Price Get")
                  #debug
                  
                  #Exit function if amazon MWS post timed out
                  if(is.null(amzpricereturn) == TRUE){stop("Timeout")}
                  
                  amzpriceparsed <- amazon.pricerankparse(amzpricereturn)
                  
                  
                  #Get product details for identified ASIN match
                  amzdetailsreturn <- amazon.MWSPost(ASIN = likelyASIN$ASIN, AWSAccessKeyId = AWSAccessKeyId, Action = "GetMatchingProduct", MarketplaceId = MarketplaceId, SellerId = SellerId, AWSSecretKey = AWSSecretKey)
                  
                  #debug
                  print("Done with Amazon Details Get")
                  #debug
                  
                  #Exit function if amazon MWS post timed out
                  if(is.null(amzdetailsreturn) == TRUE){stop("Timeout")}
                  
                  amzdetailsparsed <- amazon.productdetailsparse(amzdetailsreturn)
                  
                  #Get FBA shipping and commission fees
                  fees <- FBAcosts(ASIN = likelyASIN$ASIN, FBAFeeTable = FBAFeeTable, ShipFeeTable = ShipFeeTable, UPSrate = UPSrate, amzpriceparsed = amzpriceparsed, amzdetailsparsed = amzdetailsparsed)
                  
                  #Calculate margin, first set as 0 in case there is a product that returns no price or otherwise
                  margin <- 0
                  try(margin <- (amzpriceparsed$price + amzpriceparsed$shipping - slickdetails$price[[k]] - fees$Commission - fees$Shipping)/(amzpriceparsed$price + amzpriceparsed$shipping))
                  
                  ##STOPPED WORK HERE## Get expected cashback. Use site cashbackmonitor or evreward as cashbackholic disallows robots
                  
                  #Dump deal analysis into output slickanalyzed
                  
                  #######################Test dump loop for amazon string comparison tuning##################
#                   amzsearchtest[[length(amzsearchtest)+1]] <- amzsearchparsed
#                   storescrapetest[[length(storescrapetest)+1]] <- rawscrape
                  
                  #Amazon details
                  slickanalyzed$Margin[length(slickanalyzed$Margin) + 1] <- margin
                  slickanalyzed$ASIN[length(slickanalyzed$ASIN) + 1] <- likelyASIN$ASIN
                  slickanalyzed$AmzTitle[length(slickanalyzed$AmzTitle) + 1] <- likelyASIN$Title
                  slickanalyzed$AmzModel[length(slickanalyzed$AmzModel) + 1] <- likelyASIN$Model
                  slickanalyzed$AmzPN[length(slickanalyzed$AmzPN) + 1] <- likelyASIN$PartNumber
                  slickanalyzed$AmzManufacturer[length(slickanalyzed$AmzManufacturer) + 1] <- likelyASIN$Manufacturer
                  
                  #Percent match of amazon page to product details from store and slickdeals
                  slickanalyzed$ModelMatch[length(slickanalyzed$ModelMatch) + 1] <- likelyASIN$ModelMatchStore
                  slickanalyzed$WeightedMatch[length(slickanalyzed$WeightedMatch) + 1] <- likelyASIN$WeightedMatch
                  
                  #Expected income from selling and sale rank of product
                  slickanalyzed$saleprice[length(slickanalyzed$saleprice) + 1] <- amzpriceparsed$price
                  slickanalyzed$saleshipping[length(slickanalyzed$saleshipping) + 1] <- amzpriceparsed$shipping
                  slickanalyzed$rank[length(slickanalyzed$rank) + 1] <- amzpriceparsed$rank
                  slickanalyzed$rankcat[length(slickanalyzed$rankcat) + 1] <- amzpriceparsed$rankcat
                  
                  #Amazon fees for selling (including inbound shipping to amazon)
                  slickanalyzed$ProdCost[length(slickanalyzed$ProdCost) + 1] <- slickdetails$price[[k]]
                  slickanalyzed$AmzCommission[length(slickanalyzed$AmzCommission) + 1] <- fees$Commission
                  slickanalyzed$ShipInCost[length(slickanalyzed$ShipInCost) + 1] <- fees$Shipping
                  
                  #Details of slickdeals source post
                  slickanalyzed$thumbs[length(slickanalyzed$thumbs) + 1] <- slickdetails$thumbs
                  slickanalyzed$slicklink[length(slickanalyzed$slicklink) + 1] <- slicklink[j]
                  slickanalyzed$dealtitle[length(slickanalyzed$dealtitle) + 1] <- slickdetails$dealtitle
                  slickanalyzed$pricetext[length(slickanalyzed$pricetext) + 1] <- slickdetails$pricetext[[k]]
                  slickanalyzed$deallink[length(slickanalyzed$deallink) + 1] <- slickdetails$deallinks[[k]]
                  slickanalyzed$timeanalyzed[length(slickanalyzed$timeanalyzed) + 1] <- collectiontime
                  
                }
              }
            }
          }
          #5 second delay until next message process to prevent excessive website calls all at once
          Sys.sleep(5)
          
        }
      }
  
  #1. Add in capability to differentiate points back or gift card back to slickget function. Add as column to deal tracker spreadsheet and calculate margin with and without this.
  #2. Scrape cashback site (not cashbackholic as they don't allow robots)
  #9. Calculate margin and add to table.
  #10. Send email if above a certain margin
  #11. Add in sears scraper for member exclusive deals
  #12.
  ##Perhaps use dictionary to check for text strings that aren't words as a starting point to identify brand and model?
  ##qdap package has dictionary and spell checker that can be used for this potentially: here is process:
  
      #debug, moves delete statement here if we want to diagnose why an email is causing problems
  #delete_message(id = message.id)    
      
  slickout <- list("slickanalyzed" = slickanalyzed, "slicknotanalyzed" = slicknotanalyzed)
  
  return(slickout)
  
}

FBAcosts <- function(ASIN, FBAFeeTable, ShipFeeTable, UPSrate, amzpriceparsed, amzdetailsparsed){
  #Function to calculate FBA fees (including inbound shipping estimate, FBA shipping and handling, and amazon sale commissions)
  #Inputs:
  #FBAFeeTable: Table containing commission fee tiers for each product category
  #ShipFeeTable: Table containing amazon shipping and handing fee tiers for each package size category
  #UPSrate: Estimated shipping cost per lb for inbound shipping to amazon
  #amzpriceparsed: parsed result from MWS Get competitive pricing call
  #amzdetailsparsed: parsed result from MWS get matching product call
  #Output: list with Commission and Shipping elements defining shipping and commission fees
  
  #Convert price to 0 from NA for function if price is NA
  if(is.na(amzpriceparsed$price)){amzpriceparsed$price <- 0}
  
  #Calculate package category dims
  dims <- sort(c(amzdetailsparsed$length, amzdetailsparsed$width, amzdetailsparsed$height), decreasing = TRUE)
  
  #Set 'estimate' variable as FALSE
  alldims <- TRUE
  
  #replace NA dims with "0" to at least get an estimate for shipping
  dims[which(is.na(dims[1:3]))] <- 0
  
  dimweight <- prod(dims)/166
  lengthgirth <- dims[1] +2*sum(dims[2:3])
  
  #check to make sure all dims provided by amazon to provide specific shipping calculations, if not, store 'estimate' field as TRUE
  if(sum(is.na(c(amzdetailsparsed$length, amzdetailsparsed$width, amzdetailsparsed$height, amzdetailsparsed$weight))) > 0){
    alldims <- FALSE
    
    
  }
  
  #Store amazon weight. Replace with 0 if NA to get shipping estimate at a minimum
  
  amzweight <- amzdetailsparsed$weight
  if(is.na(amzweight) == TRUE){
    amzweight <- 0
  }
  
  #Determine proper shipping fee category
  lengthcat <- which(ShipFeeTable$MaxLength > dims[1])
  widthcat <- which(ShipFeeTable$MaxWidth > dims[2])
  heightcat <- which(ShipFeeTable$MaxHeight > dims[3])
  weightcat <- which(ShipFeeTable$MaxWeight > amzweight)
  dimweightcat <- which(ShipFeeTable$MaxDimWeight > dimweight)
  lengthgirthcat <- which(ShipFeeTable$MaxLengthGirth > lengthgirth)
  
  sizeind <- min(Reduce(intersect, list(lengthcat, widthcat, heightcat, weightcat, dimweightcat, lengthgirthcat)))
  
  shipweight <- max(amzweight, dimweight, na.rm = TRUE)
  shipweighttiers <- rbind(c(ShipFeeTable$WeightMax1[sizeind], ShipFeeTable$WeightMax2[sizeind], ShipFeeTable$WeightMax3[sizeind]),
                           c(ShipFeeTable$FeePerLb1[sizeind], ShipFeeTable$FeePerLb2[sizeind], ShipFeeTable$FeePerLb3[sizeind]))
  #Calculate shipping fee (fulfillment and outbound to amazon assuming ~ UPSrate per lb and $0.30 per item for stickering)
  shipfees <- ShipFeeTable$FixedFee[sizeind] + shipweight*UPSrate + 0.30 +
    min(shipweighttiers[1,1], shipweight)*shipweighttiers[2,1] +
    min(shipweighttiers[1,2]-shipweighttiers[1,1], max(shipweight- shipweighttiers[1,1], 0))*shipweighttiers[2,2] + 
    min(shipweighttiers[1,3]-shipweighttiers[1,2], max(shipweight- shipweighttiers[1,2], 0))*shipweighttiers[2,3]
  
  #Remove FBA shipping and handling fees if sale price is over zero fulfillment fee limit for categoey
  if(amzpriceparsed$price > ShipFeeTable$ZeroFeeAt[sizeind]){shipfees <- shipweight*UPSrate + 0.30}
  
  #Calculate amazon commission fee for sale based on product category
  comind <- grep(paste(amzpriceparsed$rankcat), as.character(FBAFeeTable$FBACatName), ignore.case = TRUE)
  if(length(comind) == 0){comind <- 38}
  
  salefeetiers <- rbind(c(0, FBAFeeTable$UpBound1[comind], FBAFeeTable$UpBound2[comind]),
                        c(FBAFeeTable$FeePercent1[comind], FBAFeeTable$FeePercent2[comind], FBAFeeTable$FeePercent3[comind]))
  
  salefees <- min(salefeetiers[1,2] - salefeetiers[1,1], amzpriceparsed$price)*salefeetiers[2,1] +
    min(salefeetiers[1,3] - salefeetiers[1,2], max(amzpriceparsed$price- salefeetiers[1,2], 0))*salefeetiers[2,2] + 
    max(amzpriceparsed$price - salefeetiers[1,3], 0)*salefeetiers[2,3]
  
  fees <- list("Commission" = salefees, "Shipping" = shipfees, "AllDimensionsAvailable" = alldims)
  
  return(fees)
  
}

strmatchpercent <- function(amzsearchparsed, rawscrape, matchpartialstring = FALSE){
  #Function to analyze a list of ASIN search matches based on keyword, model#, etc and return a percent word match for each attribute to the scraped store webpage content
  #Inputs:
  #amzsearchparsed: list containing amazon search attribute names (Title, model, etc.) and vector of elements for each. Each attribute must have the same number of responses.
  #rawscrape: screen scrape of store webpage to compare amazon search results to. This should be formatted by capture.output(htmlParse(GET(URL)))
  #matchpartialstring: indicates whether partial string matching should be used (terms being searched for can match part of a word instead of the whole word only)
  #Output:
  #input list based on input list with additional "columns", one for each attribute name (called nameMatch) displaying percent match of the strings in each attribute to the rawscrape page
  
  library(qdap)
  
  matchresult <- c()
  
  if(matchpartialstring == FALSE){wordbreak <- "\\b"}else{wordbreak = ""}
  
  for (i in 1:length(amzsearchparsed)){
    
    #Create category name for percent match storage
    
    percentname <- paste0(names(amzsearchparsed)[i], "Match")
    
    #Check for % match of each category to the store page
    #percentname
    
    #Reset vector of matches
    matchvect <- c(rep(0, times = length(amzsearchparsed[[1]])))
    
    for (j in 1:length(amzsearchparsed[[1]])){
      
      
      if(nchar(amzsearchparsed[[i]][j]) > 0){
        #split item into vector of individual strings
        splitvect <- strip(unlist(strsplit(amzsearchparsed[[i]][j], split = " ")), char.keep = c(".", "-", "/", "#"), digit.remove = FALSE)
        
        #reset word match counter
        matchedwords <- 0
        
        for(k in 1:length(splitvect)){
          
          #try to find each word in results of store webpage
          if(length(grep(paste0(wordbreak, splitvect[k],wordbreak), rawscrape, ignore.case = TRUE)) > 0){
            matchedwords <- matchedwords + 1
          }
          
        }
        
        #calculate percentage of words that could be found on store webpage
        matchvect[j] <- matchedwords/length(splitvect)
      }
      
    }
    
    #append percent match results to amazon search parsed results list
    matchresult[[percentname]] <- matchvect
    
  }
  
  return(matchresult)
  
}

pricegrab <- function(pricestring){
  #Function to strip a list of prices (as numeric) from a string or vector of strings
  #Inputs:
  #pricestring: a string or vector of strings containing prices to be stripped and converted to numeric
  
  library(qdap)
  
  price <- c()
  
  #Add a space in front of every $ to make sure someone didn't forget to leave a space to prevent bad price calculations
  price <- gsub("$", " $", pricestring, fixed = TRUE)
  
  #Split strings into individual elements and find ones with dollar sign in them
  price <- try(grep("\\$", unlist(strsplit(price, split = " ")), value = TRUE))
  
  
  #Remove remaining letters that may be attached to prices and remove other html artifacts if applicable
  price <- try(clean(gsub("[a-z]", " ", price, ignore.case = TRUE)))
  
  #split again to keep only $ containing strings
  price <- try(grep("\\$", unlist(strsplit(price, split = " ")), value = TRUE))
  
  #Strip remaining special characters
  price <- try(strip(price, digit.remove = FALSE, char.keep = c(".")))
  
  #Split strings by spaces again
  price <- try(unlist(strsplit(price, split = " ")))
  
  #Remove hanging decimal points that are not attached to a number
  price <- try(gsub("^[.]$", "", price))
  
  #Remove remaining zero length strings
  price <- try(price[which(nchar(price) > 0)])
  
  #Convert to numeric
  price <- try(as.numeric(price))
  
  return(price)
  
}

newmessages <- function(searchterm = c(), sender = c()){
  
  #Function pulls messages from gmail inbox that result from a search term and specific sender (if only one specified, will search based on one, if both specified, will produce intersection of the two)
  
  #Inputs:
  #JSONPermisloc: location of JSON permission file. This specifies both the mailbox to access and provides permission
  #searchterm: term to search for in subject and body of email (currently assumes search entire message)
  #sender: return all emails from specific sender or restrict searchterm to only one sender or list of senders
  #Outputs:
  #Messageframe: list of gmail message IDs that matched search terms
  
  library(gmailr)
  library(RCurl)
  
  #Perform search and save message list
  messearch <- c()
  messageframe <- c()
  
  #Get messages corresponding to search terms. Search INBOX only
  messearch <- (messages(paste0(searchterm, " from:", sender), label_ids = "INBOX"))[[1]]["messages"]
  
  #Coerce result into vector of message IDs
  if(length(messearch) != 0){
    
    messageframe <- data.frame(messearch)
    messageframe <- messageframe[grep("messages.id", colnames(messageframe))]
    messageframe <- unlist(messageframe)
    
  }
  
  return(messageframe)
  
}

messageget <- function(message.id, deleteread){
  #Function to get contents of a gmail message based on message.id
  #Inputs:
  #message.id: gmail message ID of message to fetch as string
  #deleteread: whether to delete messages that are returned by this search after they are read into the application
  #Output: list of $subject and $body containing the text of the email subject and body
  
  library(gmailr)
  library(RCurl)
  
  messagereturn <- c()
  messagebody <- c()
  messagesubject <- c()
  
  #Get subject and body of message and place into lists
  messageraw <- message(paste(message.id))
  
  #Delete message before running remainder of function to ensure that messages that cause execution errors are still deleted
  if(deleteread == TRUE){
    delete_message(message.id)}
  
  bodyraw <- base64Decode(messageraw$payload$parts[[1]]$parts[[1]]$body$data)
  messagesubject <- subject(messageraw)
  messagebody <- capture.output(htmlParse(bodyraw, asText = TRUE))
  
  #Bind subject and body results into one list
  messagereturn <- list("subject" = messagesubject, "body" = messagebody)
  
  return(messagereturn)
  
}

slickmaillink <- function(emailbody){
  #Function to take single message body from slickdeals deal alert and return the link to the deal
  #Input: Message body coerced into text format with most html formatting removed
  #output: list of email link(s)
  
  library(qdap)
  
  #create empty vector in case no links are found
  dealURLs <- c()
  
  dealURLs <- clean(grep("slickdeals.net/da", emailbody, value = TRUE))
  dealURLs <- gsub(" ", "", dealURLs)  
  
  return(dealURLs)
  
}

slickscrape <- function(hotdeals = TRUE, dealtalk = TRUE, dealframe = c()){
  #Function to scrape slickdeals forum pages for new deals that may not have triggered deal alerts
  #Inputs:
  #hotdeals: whether to scrape the hotdeals page
  #dealtalk: whether to scrape the dealtalk page
  #dealframe: optional dataframe of currently analyzed deals to filter out responses for ones that have already been analyzed
}

slickget <- function(slickURL, dealframe = c()){
  #Function that takes input of link to slickdeals deal and parses it for price, thumbs, and product link
  #Inputs:
  #dealURL: link to a slickdeals deal page
  #dealframe: optional dataframe of currently analyzed deals to filter out responses for ones that have already been analyzed if desired
  #scrapeforums
  
  library(httr)
  library(XML)
  library(rvest)
  library(qdap)
  
  price <- NA 
  pricetext <- NA
  thumbs <- NA
  dealtitle <- NA
  dealtext <- NA
  deallinks <- NA
  
  #Get html page
  slickhtml <- htmlParse(GET(slickURL))
  
  #Make sure page is in expected format or do not execute css selection parsing to prevent errors
  if(length(html_node(slickhtml, ".firstthread_title")) > 0){
    
    #get deal title from first post of deal
    dealtitle <- try(clean(html_text(html_node(slickhtml, ".firstthread_title"))))
    
    #get text from first post of deal
    dealtext <- try(html_text(html_node(slickhtml, ".post_message div")))
    
    #get all website links from first post of deal without including links to "other deals from this store"
    deallinks <- html_attr(html_nodes(slickhtml, ".post_message div a"), "href")
    #remove links that are to other deals on slickdeals from post
    deallinks <- deallinks[!(deallinks %in% grep("http://slickdeals.net/f", deallinks, value = TRUE))]
    
    #Remove duplicate links
    deallinks <- unique(deallinks)
    
    #If only one deal link found, use price from title or body (but first price listed)
    pricetitle <- pricegrab(dealtitle)
    pricebody <- pricegrab(dealtext)
    uniqueprices <- unique(c(pricetitle, pricebody))
    
    #If there is only one deal link or only one unique price in deal title and body, grab the first/only price and associate it with all deal links
    if((length(deallinks) == 1)|(length(uniqueprices) < 2)){
      
      price <- pricetitle
      pricetext <- dealtitle
      
      #if price can't be found in title, try in dealtext
      if(length(pricetitle) == 0){
        
        price <- pricebody
        price <- pricetext <- dealtitle
      }
      
      price <- rep(price[1], times = length(deallinks))
      pricetext <- rep(pricetext[1], times = length(deallinks))
      
    }
    
    #If there is more than 1 unique deal link AND more than 1 unique price
    if((length(deallinks) > 1)&(length(uniqueprices) > 1)){
      
      price <- rep(NA, times = length(deallinks))
      pricetext <- rep(NA, times = length(deallinks))
      
      #Try to find one price per deal link in each double returned line of deal body
      parasplit <- try(unlist(strsplit(capture.output(html_node(slickhtml, ".post_message div")), split = "\\<br /><br />")))
      
      #Reduce to all double returned string group that contains deal link
      #parasplit <- grep("http://slickdeals.net/?pv", parasplit, value = TRUE, fixed = TRUE) - perhaps add this back in if needed. May be problematic if slickdeals changes link format
      parasplit <- grep("http://", parasplit, value = TRUE, fixed = TRUE)
      parasplit <- parasplit[!(parasplit %in% grep("http://slickdeals.net/f", parasplit, value = TRUE))]
      
      pricelinkmismatch <- rep(FALSE, times = length(parasplit))
      
      for (i in 1:length(parasplit)){
        
        loopparse <- htmlParse(parasplit[i], asText = TRUE)
        
        looplinks <- unique(html_attr(html_nodes(loopparse, "body a"), "href"))
        looplinks <- looplinks[!(looplinks %in% grep("http://slickdeals.net/f", looplinks, value = TRUE))]
        
        loopprices <- pricegrab(html_text(loopparse))
        
        if((length(looplinks) == length(loopprices))|(length(loopprices) == 1)){
          
          price[which(deallinks %in%looplinks)] <- loopprices
          pricetext[which(deallinks %in%looplinks)] <- try(html_text(loopparse))
          
        }
        else{pricelinkmismatch[i] <- TRUE}
        
      }
      
      #If not all prices can be traced in the individual double break lines, search in single break lines
      if(length(parasplit[pricelinkmismatch]) != 0){
        
        #single out remaining double break blocks that had a price/link count mismatch
        
        parasplit <- parasplit[pricelinkmismatch]
        
        #break down into single lines. Last matching attempt
        parasplit <- try(unlist(strsplit(parasplit, split = "\\<br />")))
        
        #Reduce to all single lines that contain a deal link
        #parasplit <- grep("http://slickdeals.net/?pv", parasplit, value = TRUE, fixed = TRUE) - perhaps add this back in if needed. May be problematic if slickdeals changes link format
        parasplit <- grep("http://", parasplit, value = TRUE, fixed = TRUE)
        parasplit <- parasplit[!(parasplit %in% grep("http://slickdeals.net/f", parasplit, value = TRUE))]
        
        for (i in 1:length(parasplit)){
          
          loopparse <- htmlParse(parasplit[i], asText = TRUE)
          
          looplinks <- unique(html_attr(html_nodes(loopparse, "body a"), "href"))
          looplinks <- looplinks[!(looplinks %in% grep("http://slickdeals.net/f", looplinks, value = TRUE))]
          
          loopprices <- pricegrab(html_text(loopparse))
          
          if((length(looplinks) == length(loopprices))|(length(loopprices) == 1)){
            
            price[which(deallinks %in%looplinks)] <- loopprices
            pricetext[which(deallinks %in%looplinks)] <- try(html_text(loopparse))
            
          }
        }
      }
      
      #If no price matches have still been found, see if number of prices in various elements exactly match the length of the links list
      if(prod(is.na(price)) == 1){
        
        #compare to number of prices in deal title, then body, then in unique price list. Match in that order
        if(length(price) == length(pricetitle)){
          
          price <- pricetitle
          pricetext[] <- dealtitle
          
        }
        else if(length(price) == length(pricebody)){
          
          price <- pricebody
          pricetext[] <- dealtext
        }
        else if(length(price) == length(uniqueprices)){
          
          price <- uniqueprices
          pricetext[] <- paste(dealtitle, dealtext, sep = " ")
          
        }
        
        
      }
      
    }
    
    #get thumbs up/down numeric value
    try(thumbs <- as.numeric(html_text(html_node(slickhtml, ".vote_score label"))))
    
    #get date deal posted as text string
    try(posttimedate <- html_text(html_node(slickhtml, ".postdate b")))
    
    #Clean special characters out of deal text since they are no longer needed for deal parsing
    try(dealtext <- try(clean(dealtext)))
    
  }
  
  dealstuff <- list("price" = price, "thumbs" = thumbs, "dealtitle" = dealtitle, "dealtext" = dealtext, "deallinks" = deallinks, "pricetext" = pricetext)
  
  return(dealstuff)
  
}

#1. Pull email text string
#2. Run clean() {qdap} function to remove escaped characters (line breaks, etc) if necessary
#3. Identify location of price, collect, and remove from string for tokenization.
#4. Tokenize text (break into word groupings based on NLP. Package {tm} can do this or {wordnet} or {oNLP}). Alternately, search outward x number of words from misspelled words if tokenization won't work.
#5. Rank tokenized text groupings based on misspelled words using which_misspelled {qdap} to identify spelling errors
#6. Poll amazon MWS for matching product list with remaining word or word chunk candidates
#7. Perform match to product website page

##Function to scrape product webpage and get key indicators##

storeinfograb <- function(targetURL, keywordlist, maxrespchar= 100, ignorecase = TRUE, wholewordonly = TRUE){
  
  library(httr)
  library(XML)
  library(rvest)
  
  ##Function to scrape product webpage by following link and getting lines that correspond to the keyword/keyword list
  #Inputs:
  #targetURL: target URL to scrape as character
  #keywordlist: list of key terms to scrape page for and return matching elements
  #maxrespchar: maximum length (in characters) for a valid returned string. This upper limit helps prevent long text strings being passed that will not return good search results
  #ignorecase: (logical) should search terms passed be case sensitive?
  #wholewordonly: (logical) should search results return the whole word only or can a match of the search term within a word be valid
  #Outputs:
  #resultlist: single dimension list of results
  
  resultlist <- c()
  
  #Create escape string for front and back of keyword to make sure only whole word matches count
  wholeword <- c()
  if(wholewordonly == TRUE){wholeword <- "\\b"}
  
  rawscrape <- capture.output(htmlParse(GET(targetURL)))
  for (i in 1:length(keywordlist)){
    
    #Get all lines of html matching the keyword list
    searchresult <- grep(paste0(wholeword, keywordlist[[i]], wholeword), rawscrape, ignore.case = ignorecase, value = TRUE)
    
    if(length(searchresult) != 0){
      
      for (j in 1:length(searchresult)){
        
        #Convert scrape results to actual values
        try(searchresult[j] <- html_text(html(searchresult[j])))
        
      }
    }
    
    #Add results from this keyword search to compiled search results list
    resultlist <- c(resultlist, searchresult)
    
  }
  
  #remove results that are zero length (due to html conversion) or are too long based on maxrespchar
  
  resultlist <- resultlist[which(nchar(resultlist) != 0)]
  resultlist <- resultlist[which(nchar(resultlist) < maxrespchar)]
  
  return(resultlist)
  
}

amazon.searchparse <- function(URLXMLResp){
  
  library(XML)
  library(rvest)
  
  Titles <- as.character(c())
  ASINs <- as.character(c())
  Manufacturers <- as.character(c())
  Models <- as.character(c())
  PNs <- as.character(c())
  
  XMLresult <- xmlParse(gsub("ns2:", "", gsub(":ns2", "2", gsub("xmlns", "redact", capture.output(URLXMLResp)))))
    
  if (length(XMLresult["//Product"]) > 0){
    
    for (i in 1:length(XMLresult["//Product"])){
      
      try(XMLsub <- xmlParse(capture.output(XMLresult["//Product"][[i]])))
      
      try(Titles[i] <- c(html_text(XMLsub["//Title"])))
      try(ASINs[i] <- c(html_text(XMLsub["//Product/Identifiers/MarketplaceASIN/ASIN"])))
      try(Manufacturers[i] <- c(html_text(XMLsub["//Manufacturer"])))
      try(Models[i] <- c(html_text(XMLsub["//Model"])))
      try(PNs[i] <- c(html_text(XMLsub["//PartNumber"])))
      
      #If any fields are blank, return a zero length character string instead to preserve order
      try(if(is.na(Titles[i]) == TRUE){ Titles[i] <- ""})
      try(if(is.na(ASINs[i]) == TRUE){ ASINs[i] <- ""})
      try(if(is.na(Manufacturers[i]) == TRUE){ Manufacturers[i] <- ""})
      try(if(is.na(Models[i]) == TRUE){ Models[i] <- ""})
      try(if(is.na(PNs[i]) == TRUE){ PNs[i] <- ""})
      
    }
    
  }
  
  output <- list("Title" = Titles, "ASIN" = ASINs, "Manufacturer" = Manufacturers, "Model" = Models, "PartNumber" = PNs)
  
  return(output)
  
}



##Function to parse XML MWS response for GetCompetitivePricing from amazon to get buy box price, rank, and rank category###
amazon.pricerankparse <- function(URLXMLResp){
  
  library(XML)
  
  #parse XML to R readable format
  XMLprice <- xmlParse(gsub("ns2:", "", gsub(":ns2", "2", gsub("xmlns", "redact", capture.output(URLXMLResp)))))
  
  rank <- NA
  rankcat <- NA
  price <- NA
  shipping <- NA
  
  #get desired attributes as properly formatted values
  try(rank <- as.numeric(xmlValue(XMLprice["//Rank"][[1]])))
  try(rankcat <- xmlValue(XMLprice["//SalesRank/ProductCategoryId"][[1]]))
  try(price <- as.numeric(xmlValue(XMLprice["//LandedPrice/Amount"][[1]])))
  try(shipping <- as.numeric(xmlValue(XMLprice["//Shipping/Amount"][[1]])))
  
  #Remove _display_on_website tag from amazon rank category if necessary
  rankcat <- gsub("_display_on_website", "", rankcat)
  
  #if values are blank, return null string
  try(if(is.na(rank) == TRUE){ rank <- NA})
  try(if(is.na(rankcat) == TRUE){ rankcat <- NA})
  try(if(is.na(price) == TRUE){ price <- NA})
  try(if(is.na(shipping) == TRUE){ shipping <- NA})
  
  output <- list("price" = price, "shipping" = shipping, "rank" = rank, "rankcat" = rankcat)
  
  return(output)
  
}

##Function to parse XML MWS response for GetMatchingProduct from amazon to get product package dimensions###
amazon.productdetailsparse <- function(URLXMLResp){
  
  library(XML)
  
  #set dimensions as NA in case no package dimensions have been found yet
  height <- NA
  length <- NA
  width <- NA
  weight <- NA
  
  #parse to readable xml result
  XMLresult <- try(xmlParse(gsub("ns2:", "", gsub(":ns2", "2", gsub("xmlns", "redact", capture.output(URLXMLResp))))))
  
  
  #get package dimensions
  try(height <- as.numeric(xmlValue(XMLresult["//PackageDimensions/Height"][[1]])))
  try(length <- as.numeric(xmlValue(XMLresult["//PackageDimensions/Length"][[1]])))
  try(width <- as.numeric(xmlValue(XMLresult["//PackageDimensions/Width"][[1]])))
  try(weight <- as.numeric(xmlValue(XMLresult["//PackageDimensions/Weight"][[1]])))
  
  output <- list("height" = height, "length" = length, "width" = width, "weight" = weight)
  
  return(output)
  
}


##Function to create signed call to amazon MWS server. See section below for input definitions. Output is raw result from MWS###
amazon.MWSPost <- function(AWSAccessKeyId, Action, MarketplaceId, SellerId, AWSSecretKey, ASIN = NA, IdList = NA, IdType = NA, ItemCondition = NA, Query = NA, QueryContextId = NA){
  library(digest)
  library(RCurl)
  library(httr)
  
  base.html.string <- "https://mws.amazonservices.com/Products/2011-10-01?"
  
  version.request = '2011-10-01'
  if(!is.character(AWSSecretKey)){
    message('The AWSsecretkey should be entered as a character vector')
  }
  
  pb.txt <- Sys.time()
  
  pb.date <- as.POSIXct(pb.txt, tz = Sys.timezone)
  
  Timestamp = strtrim(format(pb.date, tz = "GMT", usetz = TRUE, "%Y-%m-%dT%H:%M:%S.000Z"), 24)
  
  #######Eventually create dynamic listing with parameters sorted canonically######
  
  parameterarray <- data.frame(matrix(nrow = 14, ncol = 2))
  
  parameterarray[1,] <- c(paste0("ASINList.ASIN.1=",ASIN), ASIN) #Required for Action(s): GetMatchingProducts, GetCompetitivePricingForASIN,GetLowestOfferListingsForASIN, GetMyPriceForASIN, GetProductCategoriesForASIN
  parameterarray[2,] <- c(paste0("AWSAccessKeyId=",AWSAccessKeyId), AWSAccessKeyId) #Always Required
  parameterarray[3,] <- c(paste0("Action=", Action), Action)  #Always Required. See other parameters for all actions and other required parameters for each action
  parameterarray[4,] <- c(URLencode(paste0("IdList=", IdList)), IdList)  #Required for Action(s): GetMatchingProductForId
  parameterarray[5,] <- c(paste0("IdType=", IdType), IdType)  #Required for Action(s): GetMatchingProductForId
  parameterarray[6,] <- c(paste0("ItemCondition=", ItemCondition), ItemCondition)  #Required for Action(s): GetLowestOfferListingsForASIN, GetMyPriceForASIN
  parameterarray[7,] <- c(paste0("MarketplaceId=", MarketplaceId), MarketplaceId) #Required for Action(s): ListMatchingProducts, GetMatchingProducts, GetCompetitivePricingForASIN, GetLowestOfferListingsForASIN, GetMyPriceForASIN, GetProductCategoriesForASIN
  parameterarray[8,] <- c(URLencode(paste0("Query=", Query)), Query) #Required for Action(s): ListMatchingProducts
  parameterarray[9,] <- c(URLencode(paste0("QueryContextId=", QueryContextId)), QueryContextId) #Optional for Action(s): ListMatchingProducts
  parameterarray[10,] <- c(paste0("SellerId=", SellerId), SellerId) #Always Required
  parameterarray[11,] <- c(paste0("SignatureMethod=", "HmacSHA256"), "HmacSHA256") #Always Required
  parameterarray[12,] <- c(paste0("SignatureVersion=", "2"), 2) #Always Required
  parameterarray[13,] <- c(paste0("Timestamp=", gsub('%2E','.',gsub('%2D', '-', curlEscape(Timestamp)))),gsub('%2E','.',gsub('%2D', '-', curlEscape(Timestamp)))) #Always Required
  parameterarray[14,] <- c(paste0("Version=", version.request), version.request) #Always Required
  
  #Check for null parameters and create exclusion vector based on these parameters
  
  nullvect <- rep(TRUE, times = nrow(parameterarray))
  nullvect[which(is.na(parameterarray[,2]))] <- FALSE
  
  #Paste query string together to calculate signature and to create final call that will be sent to Amazon API
  
  URLparam <- paste(parameterarray[,1][nullvect], collapse = "&")
  
  #Create request string to be used to create signature
  str <- paste0('POST\nmws.amazonservices.com\n/Products/2011-10-01\n', URLparam)
  
  
  #     #Create request string to be used to create signature
  #     str = paste('POST\nmws.amazonservices.com\n/Products/2011-10-01\n',
  #                 'ASINList.ASIN.1=', ASIN,
  #                 '&AWSAccessKeyId=', AWSAccessKeyId,
  #                 '&Action=', Action,
  #                 '&MarketplaceId=', MarketplaceId,
  #                 '&SellerId=', SellerId,
  #                 '&SignatureMethod=HmacSHA256',
  #                 '&SignatureVersion=2',
  #                 '&Timestamp=', gsub('%2E','.',gsub('%2D', '-', curlEscape(Timestamp))),
  #                 '&Version=', version.request,
  #                 sep = '')
  
  #Create signature using canonize request above and secret key
  Signature = curlEscape(base64(hmac( enc2utf8((AWSSecretKey)), enc2utf8(str), algo = 'sha256', serialize = FALSE,  raw = TRUE)))
  
  #Create URL Request
  AmazonURL <- paste0(base.html.string, URLparam, "&Signature=", Signature)
  
  
  #Create URL request
  #     AmazonURL <- paste(base.html.string,
  #                        'ASINList.ASIN.1=', ASIN,
  #                        '&AWSAccessKeyId=', AWSAccessKeyId,
  #                        '&Action=', Action,
  #                        '&MarketplaceId=', MarketplaceId,
  #                        '&SellerId=', SellerId,
  #                        '&SignatureMethod=HmacSHA256',
  #                        '&SignatureVersion=2',
  #                        '&Timestamp=', gsub('%2E','.',gsub('%2D', '-', curlEscape(Timestamp))),
  #                        '&Version=', version.request,
  #                        '&Signature=', Signature,
  #                        sep = '')
  
  #Sent request to Amazon and decode result into XML tree with pointers. Alternatively, can use as(AmazonResult, "character") to convert to characters for different XML parser
  #AmazonResult <- content(POST(AmazonURL))
  tryCatch(AmazonResult <- content(POST(AmazonURL, timeout(120))), error = function(e){AmazonResult <- NULL})
  return(AmazonResult)
}

#Slickdeals website parser goes here - Optional if original gmail parse is not successful ##Not Started##

#Cashbackholic web scraper goes here ##Not Started##

#Margin calculation loop goes here ##Not Started##

#Deal ranker and database documenting function goes here ##Not Started##

#Gmail email alert generator goes here ##Not Started##

######Analysis Module##############

#Camelcamelcamel image processing function goes here ##Not Started##

#Input is ASIN, price/rank prediction time length, which should be All, 3 months, or 1 month
camelProcess <- function(ASIN, timelength){
  
  ##Construct URL to locate file
  #Here is process overall
  #1. Create general URL for camel page
  #2. Scrape general camel page for current price and current rank (make sure to wrap in try)
  #2. Create URL for price graph over last x. Force graph to go to zero axis (no up-close view on graph option) for second reference point beyond current price to calculate value of each point
  #3. Repeat process for rank (current rank can be obtained from last page, probably even if hidden)
  #4. Make sure to include try() wrap around functions as sometimes rank data isn't present
  
  
}

PNGtrendlinemine <- function(targetURL, targettype = "URL", xstopgap = 0){
  
  #Inputs:
  #targetURL is target PNG URL or target file location with targettype set to "File"
  #targettype is PNG for URL of PNG. Defaults to "URL"
  #xstopgap is maximum percent gap in width vs width of image that is allowed before line is trimmed at that gap. Defaults to 0
  
  #Outputs:
  #matrix of x-y positions tracing the line that match the pixel x-y position on the graph
  
  
  #Ready for use, updates could potentially be used
  
  ##UPGRADES##
  ##Add median slope filter with option to do minimum absolute line variation for step functions as well
  ##Add option to set middle percent distance that initial pixel filter is conducted over - perhaps also add where median should be centered
  ##Add option for background contrast filtering (current options compare biggest difference from basic white or grey)
  ##Add option for maximum allowed break in trend line before remainder is discarded
  ##Add option for number of trendlines to find
  ##Change dotted line filter to allow for configuration (configurable percent of columns that must have pixels of this color to be a valid line)
  
  
  if(require("png") != TRUE){library("png")}
  
  if(targettype == "File"){
    
    tempmatx <- readPNG(targetURL)
    
  }
  
  if(targettype == "URL"){
    
    tempimg <- tempfile()
    
    #Download image from target URL
    #UPGRADE ##Add error handler for failed download length verification## seems to happen often, even when image downloads properly
    download.file(url = targetURL, destfile = tempimg, mode = "wb")
    
    #Convert PNG file to 4 layer 2 dimensional array with layers ordered as (RGBA)
    tempmatx <- readPNG(tempimg)
    
    #Remove downloaded file since it is no longer needed
    file.remove(tempimg)
  }
  #calculate summed squared distance from white for RGB arrays
  
  Distfromwhite <- tempmatx[,,1:3]-1
  Distfromwhite <- Distfromwhite[,,1]^2+Distfromwhite[,,2]^2+Distfromwhite[,,3]^2
  
  #Calculate summed squared distance from average pixel value (distance from grey)
  
  Distfromavg <- (tempmatx[,,1]+tempmatx[,,2]+tempmatx[,,3])/3
  Distfromavg <- (tempmatx[,,1]-Distfromavg)^2+(tempmatx[,,2]-Distfromavg)^2+(tempmatx[,,3]-Distfromavg)^2
  
  #Find line matching least grey color across middle ~24% pixels of graph. Perhaps repeat this a number of times to find all lines then continue processing below?
  
  #Define column numbers for middle 100 pixels of image
  
  leftmidpix <- round(ncol(Distfromavg)/2-.12*ncol(Distfromavg),0)
  rightmidpix <- round(leftmidpix + .24*ncol(Distfromavg),0)
  pixvect <- c(leftmidpix:rightmidpix)
  
  #Define initial matrix of values to include in maximum value search (will be modified later to remove incorrect lines)
  
  Includemat <- matrix(nrow = nrow(Distfromavg), ncol = ncol(Distfromavg), data = TRUE)
  linefind <- matrix(nrow = 1, ncol = 2)
  #Find least grey color across middle 100 pixels of graph
  
  #create while loop to do this until continuous line is found
  #UPGRADES
  #1. Include breakout for repeat loops without solution
  #2. Reject lines that account for more than ~5 - 10% of the vertical pixel count over the line length (prevent bars or background from being selected)
  #3. Assess top 5 candidates (most continuous, then most vertical var, etc) and return best candidate to make more robust
  while(length(pixvect) > length(intersect(pixvect,linefind[,2]))){
    
    
    mostdiffval <- max(Distfromavg[,leftmidpix:rightmidpix][Includemat[,leftmidpix:rightmidpix]])
    
    linefind <- which(Distfromavg == mostdiffval, arr.ind = TRUE)
    
    #Check to make sure line is continuous over middle 100 pixels to eliminate dotted lines. If dotted line found, exclude those points from next least grey/white value found
    
    if(length(intersect(pixvect,linefind[,2])) < length(pixvect)){
      
      Includemat[linefind] <- FALSE
      
    }
  }
  
  #Determine start and end of linefind x values and calculate split gap based on input value xstopgap
  xgapmax <- xstopgap*(max(linefind[,2]) - min(linefind[,2])) + 1
  
  #Break linefind vector based on xstopgap value input. Store segment ID in 3rd column and initialize 1st segment
  
  xsegment <- 1
  linefind <- cbind(linefind, vector(mode = "numeric", length = nrow(linefind)))
  linefind[1,3] <- xsegment
  
  for(i in 2:nrow(linefind)){
    
    if(linefind[i,2] - linefind[i-1,2] > xgapmax){xsegment = xsegment + 1}
    
    linefind[i,3] <- xsegment
    
  }
  
  #Select most common continuous segment with gaps smaller than xgapmax
  
  summaryseg <- summary(as.factor(linefind[,3]))
  
  longestsegment <- as.numeric(which(summaryseg == max(summaryseg)))
  
  linefind <- linefind[which(linefind[,3]==longestsegment),]
  
  #4. Filter out unique euclidian distance values that have no horizontal variation (eliminate grid lines or no price change items)
  #5. Additional filtering to remove text of same color by searching between pixel rows to make sure selected price line is continuous
  #6. Follow line to start and end and assign vertical position values from 0 for low to 1 for high.
  #7. Done
  
  #Convert linefind to column of time from 0 to 1 (start to end) and value from 0 to 1 (lowest to highest price or worst (highest number) to best (lowest number) sales rank)
  
  optiline <- matrix(nrow = nrow(linefind), ncol = 2)
  colnames(optiline) <- c("Time", "Value")
  
  #convert x (time) axis
  optiline[,1] <- (linefind[,2]-min(linefind[,2]))/(max(linefind[,2]) - min(linefind[,2]))
  
  #convert y (price or rank) axis. Note that axis must be inverted to match image properly due to how PNG is read into system (starts at bottom left corner instead of top left for rowxcolumn index)
  optiline[,2] <- 1 - (linefind[,1]-min(linefind[,1]))/(max(linefind[,1]) - min(linefind[,1]))
  
  return(optiline)
}


#Prediction function goes here ##Not Started##