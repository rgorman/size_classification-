rm(list = ls())

library(tidyverse)
library(caret)
library(magrittr)
library(stringr)


################# make folds

input_dir <- "./size_and_classif/data_2"
files.v <- dir(path=input_dir, pattern=".*RDS")


drop_index.v <- c(13:15, 22, 24:26)

files.v <- files.v[-drop_index.v]


part_tibs <- function(tib, tokens) { # function to create n-fold partition of input tibble
  k_folds <- tib %>%
    nrow() %>%
    divide_by(tokens) %>%
    ceiling()
  
  p <- tib %>%
    nrow() %>%
    seq_len() %>%
    createFolds(k_folds)
  
  return(p)
  
}


add_chunk <- function(token_name, chunk_number) {
  gsub("token.*", "", token_name)
  
}

############ start here !
collected_xtabs.l <- list()

chunk_names.v <- NULL

counter.v <- 1

total_start_time <- Sys.time()

for(i in seq_along(files.v)) {
  start_time_1 <- Sys.time()
  
  working.tib <- readRDS(file = paste(input_dir, files.v[i], sep = "/"))
  
  
  tib_part <- working.tib %>%
    part_tibs(50)
  
  folds <- length(tib_part)
  
  end_time_1 <-Sys.time()
  print(paste("folds have been calculated for",  files.v[i]))
  print(end_time_1 - start_time_1)
  
  chunk.list <- vector(mode = "list", folds)
  
  for(j in seq_along(tib_part)) {
    start_time_2 <- Sys.time()
    
    
    # chunk.list[[1]]
    
    chunk.list[[j]] <- working.tib[tib_part[[j]], ]
    
    chunk_id <- paste0(sapply(chunk.list[[j]][, 1], add_chunk), "chunk_", j)
    
    chunk_names.v <- append(chunk_names.v, chunk_id[1])
    
    chunk.list[[j]] <- chunk.list[[j]][, 2:ncol(chunk.list[[j]])]
    
    chunk.list[[j]] <- chunk_id %>%
      as_tibble() %>%
      bind_cols(., chunk.list[[j]])
    
    var_names <- colnames(chunk.list[[j]])
    var_names[1] <- "chunk_id"
    chunk.list[[j]] <- set_colnames(chunk.list[[j]], var_names)
    
    
    var_mean <- chunk.list[[j]][, 2:ncol(chunk.list[[j]])] %>%
      sum() %>%
      divide_by(nrow(chunk.list[[j]])) %>%
      round(digits = 3)
    
    if (counter.v == 1) {
      
      meta_data.tib <- tibble(chunk_id = chunk_id[1], vars_per_token = var_mean, token_number = nrow(chunk.list[[j]]) )  
      
    } else {
      
      x.tib <- tibble(chunk_id = chunk_id[1], vars_per_token = var_mean, token_number = nrow(chunk.list[[j]]) )  
      meta_data.tib <- bind_rows(meta_data.tib, x.tib)
      
    }
    
    
    
    counter.v <- counter.v + 1
    
    
    end_time_2 <-Sys.time()
    print(paste("fold", j, "has been populated for",  files.v[i]))
    print(end_time_2 - start_time_2)
    
  }
  
  
  start_time_3 <- Sys.time()
  for(k in seq_along(chunk.list)) {
    
    freq.tib <- gather(chunk.list[[k]], variable_name, variable_value, -chunk_id, na.rm = TRUE) %>%
      
      dplyr::select(chunk_id, variable_name, variable_value) %>%
      group_by(chunk_id, variable_name) 
    
    rel_freq.tib <- freq.tib
    
    rel_freq.tib$variable_value <- freq.tib$variable_value %>%
      divide_by(nrow(chunk.list[[k]])) %>%
      multiply_by(100)
    
    chunk.list[[k]] <- rel_freq.tib
    
    
    
    
  }
  
  combined_rel_freq.tib <- do.call(bind_rows, chunk.list)
  
  result <- xtabs(variable_value ~ chunk_id+variable_name, data = combined_rel_freq.tib, sparse = FALSE)
  
  result <- apply(result, 2, as.numeric)
  
  result.tib <- as_tibble(result)
  
  collected_xtabs.l[[i]] <- result.tib
  
  end_time_3 <- Sys.time()
  
  print(paste("xtab completed for", files.v[i]))
  print(end_time_3 - start_time_3)
  
}





total_end_time <- Sys.time()
print(total_end_time - total_start_time)


final.tib <- do.call(bind_rows, collected_xtabs.l)

final.tib <- add_column(final.tib, chunk_id = chunk_names.v, .before = TRUE)  


##############

find_author <- function(x) {
  if (str_detect(x, "Aeschines") == TRUE) {
    result <- "Aeschines"
  }
  
  if (str_detect(x, "Aeschylus") == TRUE) {
    result <- "Aeschylus"
  }
  
  if (str_detect(x, "Demosthenes") == TRUE) {
    result <- "Demosthenes"
  }
  
  if (str_detect(x, "Diodorus") == TRUE) {
    result <- "Diodorus"
  }
  
  if (str_detect(x, "Herodotus") == TRUE) {
    result <- "Herodotus"
  }
  
  if (str_detect(x, "Hesiod") == TRUE) {
    result <- "Hesiod"
  }
  
  if (str_detect(x, "Iliad") == TRUE) {
    result <- "Hom_Iliad"
  }
  
  
  if (str_detect(x, "Odyssey") == TRUE) {
    result <- "Hom_Odyssey"
  }
  
  if (str_detect(x, "Lysias") == TRUE) {
    result <- "Lysias"
  }
  
  if (str_detect(x, "Plutarch") == TRUE) {
    result <- "Plutarch"
  }
  
  if (str_detect(x, "Polybius") == TRUE) {
    result <- "Polybius"
  }
  
  if (str_detect(x, "Sophocles") == TRUE) {
    result <- "Sophocles"
  }
  
  if (str_detect(x, "Thucydides") == TRUE) {
    result <- "Thucydides"
  }
  
  if (str_detect(x, "Xenophon") == TRUE) {
    result <- "Xenophon"
  }
  
  
  return(result)
  
}


authors.v <- sapply(final.tib$chunk_id, find_author) %>%
  unlist() %>%
  as.factor()


final.tib <-  add_column(final.tib, authors = authors.v, .before = TRUE)

###################### figure probs

author_count <- nlevels(final.tib$authors)

author_prob <- 1 / author_count

combined_chunk_props <- NULL

author_prob / 11


for (i in seq_len(nlevels(final.tib$authors))) {
  
  
  chunk_count <- which(final.tib$authors == levels(final.tib$authors)[i]) %>%
    length()
  
  chunk_prob <- author_prob / chunk_count
  
  chunk_probs <- rep(chunk_prob, chunk_count)
  
  combined_chunk_props <- append(combined_chunk_props, chunk_probs)
  
  
}

final.tib <-  add_column(final.tib, probs = combined_chunk_props, .before = TRUE)

total_end_time <- Sys.time()
print(total_end_time - total_start_time)

