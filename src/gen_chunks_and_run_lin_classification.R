

sum_holder.v <- NULL
t_loop_start_time <- Sys.time()

for (t in 1:9) {
  
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
  
  
  ################## start here !
  
  
  target.tib <- final.tib
  
  
  
  
  
  
  type2.results.l <- vector(mode = "list", 100)
  type2.error.matrix.l <- vector(mode = "list", 100)
  truth.l <- vector(mode = "list", 100)
  test_chunks.l <- vector(mode = "list", 100)
  guesses.l <- vector(mode = "list", 100)
  falsity.l <- vector(mode = "list", 100)
  
  
  
  print(Sys.time())
  total_start_time <- Sys.time()
  for (i in 1:100) {
    start_time <- Sys.time()
    #create vector of random integers = 10% of obs in smaller.df
    testing.index.v <- sample (seq (1, nrow(target.tib)), 1166, prob=target.tib$probs)
    
    
    
    #create training and testing data matrices using testing.index.v and its inverse
    testing.data <- target.tib[testing.index.v, 4:ncol(target.tib)]
    training.data <- target.tib[-testing.index.v, 4:ncol(target.tib)]
    
    #create vectors of factors giving classes (here = authors) of each row in testing.data and training.data
    training.classes <- target.tib$authors[-testing.index.v] 
    testing.classes <- target.tib$authors[testing.index.v]
    
    
    
    
    
    start_time_2 <- Sys.time() 
    model_liblin <- LiblineaR(training.data, training.classes, type = 2, cost = 1)
    print(end_time_2 - start_time_2)
    
    
    type2.results.l[[i]]  <- predict(model_liblin, testing.data)
    type2.error.matrix.l[[i]] <- errormatrix(testing.classes, unlist(type2.results.l[[i]]))
    
    test_chunks.l[[i]] <- target.tib[testing.index.v, 3] %>%
      unlist() %>%
      as.character()
    
    guesses.l[[i]] <-  type2.results.l[[i]]   %>%
      unlist() %>%
      as.character()
    
    
    truth.l[[i]] <- testing.classes  %>%
      as.character()
    
    falsity.l[[i]] <- which(!(guesses.l[[i]] == truth.l[[i]]))
    
    
    end_time_2 <- Sys.time()
    print(end_time_2 - start_time_2)
    
    
    # store_iterations_tib <- bind_rows(store_iterations_tib, store_iterations_tib)
    
    # saveRDS(store_iterations_tib, file = "./size_and_classif/2018-6-11_omnibus_result_itbble.RDS")
    
    
    end_tme <- Sys.time()
    print(end_tme - start_time)
    print(paste("end of loop", i))
    
    
  }
  
  
  
  total_end_time <- Sys.time()
  print(Sys.time())
  print(total_end_time - total_start_time)
  
  
  
  collected_errors <- sapply(type2.error.matrix.l, get_total_errors)
  
  
  
  
  sum_holder.v <- append(sum_holder.v, sum(collected_errors)) 
  
  
  
  
  ##########################
  
  
  
}

t_loop_end_time <- Sys.time()
print(Sys.time())
print(t_loop_end_time - t_loop_start_time)

