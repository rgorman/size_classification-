

library(LiblineaR)
library(klaR)


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
  testing.index.v <- sample (seq (1, nrow(target.tib)), 30, prob=target.tib$probs)
  
  
  
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
  
  
  store_iterations_tib <- bind_rows(store_iterations_tib, store_iterations_tib)
  
  saveRDS(store_iterations_tib, file = "./size_and_classif/2018-6-11_omnibus_result_itbble.RDS")
  
  
  end_tme <- Sys.time()
  print(end_tme - start_time)
  print(paste("end of loop", i))
  
  
}



total_end_time <- Sys.time()
print(Sys.time())
print(total_end_time - total_start_time)








get_total_errors <- function(list) {
  
  list[14,14]
}




##################



collected_errors <- sapply(type2.error.matrix.l, get_total_errors)




sum(collected_errors)



(3000 - sum(collected_errors) ) / 3000




store_iterations_tib <-  tibble(guesses = guesses.l, truth = truth.l, wrong_answers = falsity.l, chunks_tested = test_chunks.l)
###

store_iterations_tib <- bind_rows(store_iterations_tib, store_iterations_tib)



type2.error.matrix.l[[5]]




type2.results.l <- vector(mode = "list", 100)
type2.error.matrix.l <- vector(mode = "list", 100)
truth.l <- vector(mode = "list", 100)
test_chunks.l <- vector(mode = "list", 100)
guesses.l <- vector(mode = "list", 100)
falsity.l <- vector(mode = "list", 100)


try_it.tib$guesses[[100]]

falsity.l

guesses.l[[36]]
truth.l[[36]]

test_chunks.l[[91]]

which( !(guesses.l[[91]] == truth.l[[91]]) )

which(collected_errors > 0)

which(falsity.l > 0)
which( !(guesses.l[[36]] == truth.l[[36]]) )


guesses.l[[36]]
test_chunks.l[[36]]



type2.results.l[[i]]  <- predict(model_liblin, testing.data)
type2.error.matrix.l[[3]] <- errormatrix(testing.classes, unlist(type2.results.l[[i]]))
answers.l[[i]] <- testing.classes


type2.error.matrix.l[[5]]


delta_out <- perform.delta(training.set = training.data.m, test.set = testing.data.m, classes.training.set = training.classes.v, classes.test.set = testing.classes.v)


training.data.m <- as.matrix(training.data)
testing.data.m <- as.matrix(testing.data)
training.classes.v <- as.character(training.classes)
testing.classes.v <- as.character(testing.classes)



colnames(training.data)

sd(collected_errors)

summary(collected_errors)

summary(collected_errors) %>%
  divide_by(34)


dimnames(testing.data) %>%
  length

dimnames(training.data) %>%
  length

type2.error.matrix.l[[2]]
confusionMatrix(type2.results.l[[2]]$predictions, answers.l[[2]])

######################

2802 - 346

6019 - (346 + 41)

2456 / 5632

aggie <- as_tibble(1:414)

x <- as_tibble(type7.results.l[[10]])

aggie <- bind_cols(aggie, x)

aggie <- aggie[, 2:ncol(aggie)]

aggie

for(i in 1:nrow(aggie)) {
  
  votes <- aggie[i,] %>%
    unlist() %>%
    as.factor()
  
  y <- summary(votes)
  
  
  if (i == 1) {
    
    scores.df <- data_frame_(y)
    
  } else {
    
    y.df <- data_frame_(y)
    scores.df <- bind_rows(scores.df, y.df)
    
  }
  
}


rights <- testing.classes %>%
  as.character()

scores_2.df <- cbind(rights, scores.df)



for (i in seq_len(nrow(scores_2.df))) {
  
  elected_col <- which(scores_2.df[i, 2:10] == max(scores_2.df[i, 2:10])) 
  
  
  elected_name <- colnames(scores_2.df[elected_col + 1])
  
  if (i == 1) {
    
    counter <- elected_name == rights[i]
    
  }else {
    
    if (i <= 413) {
      
      counter <- append(counter, elected_name == rights[i])
      
    }
    
    
    
  }
  
  
  
  
}

x <- type2.results.l[[5]] %>%
  unlist() %>%
  as.factor()

confusionMatrix(type2.results.l[[5]]$predictions, testing.classes)


type2.error.matrix.l[[5]]

errormatrix(testing.classes, type2.results.l[[5]]$predictions)


which(type2.results.l[[5]]$predictions == "Aeschines")

which(testing.classes == "Aeschines")

x[1] == "Polybius"
which(x == "Polybius")

result

errormatrix(testing.classes, result$predictions)

target.tib[209:230, 1:3]
