#install.packages("rvest")

library(rvest)



for(G in 2:2){
  
  if(G == 1){
    
    sex = "M"
    
  }  else {
    
    sex = "W"
  
  }
  
  dfmain <- data.frame(Number=numeric(),
                       Name=character(), 
                       AG=character(),
                       Club=character(),
                       Distance=character(),
                       Finish=character(),
                       stringsAsFactors=FALSE)

  web <- read_html(paste0("http://results.prudentialridelondon.co.uk/2017/?page=1&num_results=100&pid=search&search%5Bsex%5D=",sex))

  endpage <- html_nodes(web,'.pages a')

  pagelength <- (nchar(as.character(endpage[4]))-72)/2

  endpage <- as.integer(substring(endpage[4],
                                  nchar(as.character(endpage[4]))-pagelength,
                                  nchar(as.character(endpage[4]))-4))

  for(j in 1:endpage){
    
    cat(paste0(sex," ",j,"\n"))

    web <- read_html(paste0("http://results.prudentialridelondon.co.uk/2017/?page=",j,"&num_results=100&pid=search&search%5Bsex%5D=",sex))

    nodetable <- html_nodes(web,'tr td')
  
    ridertimes <- data.frame(matrix(as.character(nodetable), nrow=length(nodetable)/7,ncol = 7, byrow = T),stringsAsFactors=FALSE)
    
    ridertimes <- ridertimes[,1:6]
    
    ridertimes[,1] <- substring(ridertimes[,1],5,nchar(as.character(ridertimes[,1]))-5)
      
    ridertimes[,2] <- substring(ridertimes[,2],regexpr('search_event', as.character(ridertimes[,2]))+15, nchar(as.character(ridertimes[,2]))-10)
    
    ridertimes[,3] <- substring(ridertimes[,3],5,nchar(as.character(ridertimes[,3]))-5)
      
    ridertimes[,4] <- substring(ridertimes[,4],5, nchar(as.character(ridertimes[,4]))-5)
      
    ridertimes[,5] <- substring(ridertimes[,5],5, nchar(as.character(ridertimes[,5]))-5)
      
    ridertimes[,6] <- substring(ridertimes[,6],19, nchar(as.character(ridertimes[,6]))-5)
    
    dfmain <- rbind(dfmain, ridertimes[,1:6])
  
  }

  colnames(dfmain) <- c("Number","Name","AG","Club","Distance","Finish")
  
  if(G == 1){
  
    write.csv(dfmain,"Prudential Ride London 100 Male.csv", row.names = FALSE)

  }

  if(G == 2){
  
    write.csv(dfmain,"Prudential Ride London 100 Female.csv", row.names = FALSE)

  }

}