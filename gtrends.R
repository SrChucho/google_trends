# install.packages("drat")       # easier repo access + creation
# drat:::add("ghrr")             # make it known
# install.packages("gtrendsR")  

rm(list = ls())
#library(curl)
library(gtrendsR)

country <- c('MX')
time <- c("2004-01-01 2020-06-30")
channel <- "web"


path <- "C:/Users/jesus.lopezp/Desktop/ICC/ICC_2020/"
bcd <- read.csv(paste(path, "inputs/gtrends/bcd-cat.csv", sep = ""))
non_durables <- read.csv(paste(path, "inputs/gtrends/no-duraderos-cat.csv", 
                               sep = ""))
services <- read.csv(paste(path, "inputs/gtrends/servicios-cat.csv", sep = ""))

# funcion para extraer busquedas por categoria
get_export_gt_data <- function(x, country, time, channel){
  for(j in 1:ncol(x)){ # j <- 1; i <- 1; x <- bcd
    
    check <- c()
    my_words <- c(as.character(x[,j]))
    my_words <- my_words[!(my_words == "")]
    
    resultslist <- vector("list", length(my_words))
    for(i in 1:length(my_words)){
      check <- try(gtrends(my_words[i], gprop =channel,geo=country, 
                  time = time), silent = TRUE)
      if(class(check)[1] != "try-error"){
      trends <- gtrends(my_words[i], gprop =channel,geo=country, 
                       time = time)
      resultslist[[i]] <- trends$interest_over_time$hits
      }
    }
    names(resultslist) <- my_words
    resultslist <- resultslist[!sapply(resultslist, length) == 0 ]
    
      
    df_gt <- data.frame(do.call(cbind, resultslist))
    df_gt <- cbind(trends$interest_over_time$date, df_gt)
    colnames(df_gt) <- c("date", names(resultslist))
    write.csv(df_gt, paste(path, "inputs/gtrends/",
                           colnames(x)[j], ".csv", sep = ""), 
              row.names = FALSE)
    
  }
}


# get_export_gt_data(bcd, output = "bcd", country = country, 
#                    time = time, channel = channel)
# get_export_gt_data(non_durables, output = "non-durables",
#                    country = country, 
#                    time = time, channel = channel)
# get_export_gt_data(services, output = "services", 
#                    country = country, 
#                    time = time, channel = channel)

# funcion para extraer todo un conjunto de palabras relacionadas
get_export_gt_data2 <- function(x, output, country, time, channel){
  # x <- bcd; output <- "BCD"; country <- country; time = time; channel = channel
    
    check <- c()
    my_words <- unlist(x)
    my_words <- c(as.character(my_words[!(my_words == "")]))
    
    all_queries <- c()
    for(i in 1:length(my_words)){
      check <- try(gtrends(my_words[i], gprop =channel,geo=country, 
                           time = time), silent = TRUE)
      if(class(check)[1] != "try-error"){
        rel_queries <- gtrends(my_words[i], gprop =channel,geo=country, 
                          time = time)
        queries <- rel_queries$related_queries[1:5,"value"]
        all_queries <- c(all_queries,queries)
      }
    }
    
    resultslist <- vector("list", length(my_words))
    for(i in 1:length(all_queries)){
      check <- try(gtrends(all_queries[i], gprop =channel,geo=country, 
                           time = time), silent = TRUE)
      if(class(check)[1] != "try-error"){
        trends <- gtrends(all_queries[i], gprop =channel,geo=country, 
                          time = time)
        resultslist[[i]] <- trends$interest_over_time$hits
      }
    }
    names(resultslist) <- all_queries
    resultslist <- resultslist[!sapply(resultslist, length) == 0 ]
    
    df_gt <- data.frame(do.call(cbind, resultslist))
    row.names(df_gt) <- trends$interest_over_time$date
    colnames(df_gt) <- names(resultslist)
    
    write.csv(df_gt, paste(path, "inputs/gtrends/",
                           output, ".csv", sep = ""), 
              row.names = FALSE)
    
  
}

get_export_gt_data2(bcd, "BCD", country = country, 
                   time = time, channel = channel)
get_export_gt_data2(non_durables,"non-durables", country = country, 
                   time = time, channel = channel)
get_export_gt_data2(services, "services", country = country, 
                   time = time, channel = channel)



