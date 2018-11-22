#
# This is the server logic of a Shiny web application. You can run the 
# application by clicking 'Run App' above.
#
# Find out more about building applications with Shiny here:
# 
#    http://shiny.rstudio.com/
#

library(shiny)

shinyServer(function(input, output, session) {
  
  books <- read.csv('/Users/eshna/updated_books.csv')
  books<-books[1:500,]
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
})