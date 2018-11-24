
# majority of the code borrowed from https://github.com/pspachtholz/BookRecommender/

source("helpers/cf_algorithm.R")
source("helpers/similarity_measures.R")

library(recommenderlab)
library(shiny)
library(data.table)

# define functions
get_user_ratings <- function(value_list) {
  dat <- data.table(book_id = sapply(strsplit(names(value_list), "_"), function(x) ifelse(length(x) > 1, x[[2]], NA)),
                    rating = unlist(as.character(value_list)))
  dat <- dat[!is.null(rating) & !is.na(book_id)]
  dat[rating == " ", rating := 0]
  dat[, ':=' (book_id = as.numeric(book_id), rating = as.numeric(rating))]
  dat <- dat[rating > 0]
  
  # get the indices of the ratings
  # add the user ratings to the existing rating matrix
  user_ratings <- sparseMatrix(i = dat$book_id, 
                               j = rep(1,nrow(dat)), 
                               x = dat$rating, 
                               dims = c(nrow(ratingmat), 1))
}

# read in data
books <- fread('files/books.csv')
# load rating matrix that has been created 
load("files/ratingmat.Rdata")

shinyServer(function(input, output, session) {
  

  
  # show the books to be rated
  output$ratings <- renderUI({
    num_rows <- 20
    num_books <- 6 # books per row
    
    lapply(1:num_rows, function(i) {
      list(fluidRow(lapply(1:num_books, function(j) {
        list(box(width = 2, background = "black",
                 div(style = "text-align:center", img(src = books$image_url[(i - 1) * num_books + j], style = "max-height:150")),
                 div(style = "text-align:center; font-size: 80%", books$authors[(i - 1) * num_books + j]),
                 div(style = "text-align:center;", books$title[(i - 1) * num_books + j]),
                 div(style = "text-align:center; font-size: 150%; color: #f0ad4e;", ratingInput(paste0("select_", books$book_id[(i - 1) * num_books + j]), label = "", dataStop = 5)))) #00c0ef
      })))
    })
  })
  
  
  # Calculate recommendations when the sbumbutton is clicked
  df <- eventReactive(input$myButton,{
    
    # get the user's rating data
    value_list <- reactiveValuesToList(input)
    user_ratings <- get_user_ratings(value_list)
    
    # add user's ratings as first column to rating matrix
    rmat <- cbind(user_ratings, ratingmat)
    
    # predict all books the current user has not yet rated
    items_to_predict <- which(rmat[, 1] == 0)
    prediction_indices <- as.matrix(expand.grid(items_to_predict, 1))
    
    # run the ibcf-alogrithm
    res <- predict_cf(rmat, prediction_indices, "ibcf", TRUE, cal_cor, 4, FALSE, 2000, 1000)
    
    # sort, organize, and return the results
    user_results <- sort(res[, 1], decreasing = TRUE)[1:20]
    
    #convert into data frame 
    top20 <- as.data.frame(as.numeric(names(user_results)))
    names(top20) <- "book_id"
    
    # merge with books.csv to get books details 
    recommended_books = left_join(top20, books, by = "book_id")
    
    # # user x book 
    # trmat<- t(rmat)
    # normalized_ratings <- as(trmat, "realRatingMatrix")
    
    # #Building the UBCF model 
    # rec_mod = Recommender(normalized_ratings, method = "UBCF", param=list(method="cosine",nn=4))
    # 
    # # Make predictions for the first user and store it in a list. 
    # user <- 1
    # predictions = predict(rec_mod, normalized_ratings[user,], type = "ratings")
    # pred_list = as(predictions, "list")
    # 
    # #Convert list to data frame, add book_id to it as well. 
    # predictions_df <- as.data.frame(pred_list)
    # names(predictions_df) <- "rating"
    # predictions_df$book_id <- row.names(predictions_df)
    # 
    # # extracting the top 20 recommendations 
    # top20 <- predictions_df %>%
    #   arrange(desc(rating)) %>%
    #   top_n(20, wt = rating) %>%
    #   select(book_id, rating)
    # 
    # # converting book_id to numeric 
    # top20$book_id <- as.numeric(top20$book_id)
    # 
    # # merging tables in order to get the book titles 
    # recommended_books = left_join(top20, books, by = "book_id")
    
  }) 
  # display the recommendations
  output$results <- renderUI({
    num_rows <- 4
    num_books <- 5
    recommended_books <- df()
    
    lapply(1:num_rows, function(i) {
      list(fluidRow(lapply(1:num_books, function(j) {
        list(box(width = 2, background = "black",
                 div(style = "text-align:center", img(src = recommended_books$image_url[(i - 1) * num_books + j], style = "max-height:150")),
                 div(style = "text-align:center;font-size: 80%", recommended_books$authors[(i - 1) * num_books + j]),
                 div(style = "text-align:center", recommended_books$title[(i - 1) * num_books + j]),
                 div(style = "text-align:center; font-size: 150%; color: #f0ad4e;", ratingInput(paste0("select_", recommended_books$book_id[(i - 1) * num_books + j]), label = "", dataStop = 5)))) #00c0ef
      })))
    })
    
  }) # renderUI function
  
})





