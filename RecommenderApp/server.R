
# some of the code borrowed from https://github.com/pspachtholz/BookRecommender/
library(recommenderlab)
library(shiny)
library(data.table)

#function to get the users ratings:
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
  user_ratings <- sparseMatrix(i = rep(1,nrow(dat)),
                               j = dat$book_id, 
                               x = dat$rating, 
                               dims = c(1, nrow(ratingmat)))
}
# reading in the files 
books <- fread('files/books.csv')
ratings <- read.csv('files/ratings.csv')

# reshape to user x books  matrix 
ratingmat <- sparseMatrix( ratings$user_id, ratings$book_id, x=ratings$rating) # user x book matrix
ratingmat <- ratingmat[unique(summary(ratingmat)$i), ] # remove users with no ratings
dimnames(ratingmat) <- list(user_id = as.character(sort(unique(ratings$user_id))), book_id = as.character(1:10000))

shinyServer(function(input, output, session) {
  

  
  # show the books to be rated
  output$ratings <- renderUI({
    num_rows <- 20
    num_books <- 6 # books per row
    
    lapply(1:num_rows, function(i) {
      list(fluidRow(lapply(1:num_books, function(j) {
        list(box(width = 2,
                 div(style = "text-align:center", img(src = books$image_url[(i - 1) * num_books + j], style = "max-height:150")),
                 div(style = "text-align:center; color: #999999; font-size: 80%", books$authors[(i - 1) * num_books + j]),
                 div(style = "text-align:center; color:#999999;", books$title[(i - 1) * num_books + j]),
                 div(style = "text-align:center; font-size: 150%; color: #f0ad4e;", ratingInput(paste0("select_", books$book_id[(i - 1) * num_books + j]), label = "", dataStop = 5)))) #00c0ef
      })))
    })
  })
  
  
  # Calculate recommendations when the sbumbutton is clicked
  df <- eventReactive(input$myButton,{
    
    # get the user's rating data
    value_list <- reactiveValuesToList(input)
    user_ratings <- get_user_ratings(value_list)
    
    # add user's ratings as first row to ratings matrix
    rmat <- rbind(user_ratings, ratingmat)
    
    #Building the UBCF model 
    rec_mod = Recommender(normalized_ratings, method = "UBCF", param=list(method="cosine",nn=4))
    
    # Make predictions for the first user and store it in a list. 
    user <- 1
    predictions = predict(rec_mod, normalized_ratings[user,], type = "ratings")
    pred_list = as(predictions, "list")
    
    #Convert list to data frame, add book_id to it as well. 
    predictions_df <- as.data.frame(pred_list)
    names(predictions_df) <- "rating"
    predictions_df$book_id <- row.names(predictions_df)
    
    # extracting the top 20 recommendations 
    top20 <- predictions_df %>%
      arrange(desc(rating)) %>%
      top_n(20, wt = rating) %>%
      select(book_id, rating)
    
    # converting book_id to numeric 
    top20$book_id <- as.numeric(top20$book_id)
    
    # merging tables in order to get the book titles 
    recommended_books = left_join(top20, books, by = "book_id")
    
  }) 
  # display the recommendations
  output$results <- renderUI({
    num_rows <- 4
    num_books <- 5
    recommended_books <- df()
    
    lapply(1:num_rows, function(i) {
      list(fluidRow(lapply(1:num_books, function(j) {
        list(box(width = 2,
                 div(style = "text-align:center", img(src = recommended_books$image_url[(i - 1) * num_books + j], style = "max-height:150")),
                 div(style = "text-align:center; color: #999999; font-size: 80%", recommended_books$authors[(i - 1) * num_books + j]),
                 div(style = "text-align:center; color:#999999;", recommended_books$title[(i - 1) * num_books + j]),
                 div(style = "text-align:center; font-size: 150%; color: #f0ad4e;", ratingInput(paste0("select_", recommended_books$book_id[(i - 1) * num_books + j]), label = "", dataStop = 5)))) #00c0ef
      })))
    })
    
  }) # renderUI function
  
})





