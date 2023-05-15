similarity_finder <- function(description, similarity_count, reg_ex){
  
  similarity_count_ <- if_else(str_detect(description, reg_ex),
                                          similarity_count + 1,
                               similarity_count)
  
  return(similarity_count_)
}


