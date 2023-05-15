library(shiny)
library(tidyverse)


ui <- fluidPage(
  fluidRow(
    column(8, align= "centre", offset =2,
           textInput("caption", "Write a few sentences about the wine you are craving", "", width = "100%")
           )),
  verbatimTextOutput("value")
)


server <- function(input, output) {
  
  output$value <- renderText({
    Text_input <- input$caption
    
    # The input caption captured by shiny ui is not automatically converted to a vector.
    # it therefore must be made into a list by using the strsplit function. This will return a list
    # which is flattened using unlist()
    wine_words_2 <- unlist(str_extract_all(toupper(Text_input), paste0(toupper(wine_words), collapse = "|")))
    wine_words_2 <- unlist(strsplit(wine_words_2, " "))
    
    similarity_finder <- function(description, similarity_count, reg_ex){
      
      similarity_count_ <- if_else(str_detect(description,
                                              eval(substitute(reg_ex)))==TRUE, similarity_count + 1,
                                   similarity_count)
      
      return(similarity_count_)
    }
       
     
     if(length(wine_words_2 > 1)){
       # Creating a list to store the loop results
       res_2 <- vector("list", length(wine_words_2))
      for(i in 1:length(wine_words_2)){
        res_2[[i]] <- similarity_finder( description = description_one, similarity_count = similarity_count_one,
                                           reg_ex = wine_words_2[i])
      }
        
      data2 <- bind_cols(res_2) %>% 
        mutate(total_similarity = rowSums(.)) %>% select("total_similarity") %>%
        cbind(Data2$variety,.) %>% group_by(.[1]) %>%
        mutate(sum_similarity = mean(total_similarity)) %>%
        distinct(sum_similarity) %>%
        arrange(desc(sum_similarity))
        
     
          #max_num <- as.integer(nrow(data2[data2$sum_similarity == max(data2$sum_similarity), ]))
  #     #print(data2[1, runif(1, 1, max_num)])
        print(unlist(data2[1,1]))
     }
     else{
       print("Please enter a longer description")
     }
     
  })
   
}


shinyApp(ui, server)
