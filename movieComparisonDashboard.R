InstallCandidates <-c("XML"
                      ,"stringr"
                      ,"rvest"
                      ,"plyr"
                      ,"RCurl"
                      ,"DT"
                      ,"plotly"
                      ,"gridExtra"
                      ,"sqldf"
                      ,"rJava"
                      ,"ggplot2"
                      ,"dplyr"
                      ,"shiny"
                      ,"shinydashboard"
                      ,"DBI"
                      ,"RJDBC"
                      ,"RODBC" )

#installing the packages from install candidates and mark them in the library
toInstall<-InstallCandidates[!InstallCandidates %in% library()$results[,1]]
if(length(toInstall) !=0){install.packages(toInstall,repos="http://cran.r-project.org")}
lapply(InstallCandidates,library,character.only=TRUE)
rm("InstallCandidates","toInstall")

library(rJava)
library(ggplot2)
library(RODBC)
library(RJDBC)
library(dplyr)
library(shiny)
library(shinydashboard)
library(DBI)
library(shinythemes)
library(plyr)
library(shinyWidgets)
library(sqldf)
library(tidyr)
library(markdown)

#defining the connection to the mssql server
drv <- JDBC("com.microsoft.sqlserver.jdbc.SQLServerDriver", "opt/sqljdbc4-2.0.jar")
conn <- dbConnect(drv, "jdbc:sqlserver://localhost;databaseName=odenhoven;user=SA;password=Welkom123")




######################################## ROTTEN TOMATOES ######################################

ui <- fluidPage(theme = shinytheme("journal"),
                
                
                navbarPage("MovieDB comparisor",
                    tabPanel("Showdown IMDb vs RT",
                        fluidRow(
                          column(width = 12,
                                 align = "center",
                                 titlePanel(HTML('<span style="color:#000000; font-size: 30px; font-weight:bolder; font-family:Verdana; align=center; ">Ultimate showdown<span>')),
                                 plotlyOutput("plot2")
                            
                            
                          )),  
                        fluidRow(
                          column(width = 12,
                                 align = "center",
                                 HTML("<br><br>"),
                                 sliderInput("rankslider", "Select amount of ranks to show",
                                             min = 1, max = 100, value = 100, width = 1200
                                 ))),
                        
                        
                        
                        fluidRow(
                            
                            column(width = 6,
                                   align = "center",
                                   br(),
                                   br(),
                                   tableOutput('headtoheadrotten')
                              
                              
                            ),
                            
                            column(width = 6,
                                   align = "center",
                                   br(),
                                   br(),
                                   tableOutput('headtoheadimdb')
                                   )),
                        
                        fluidRow(
                          
                          column(width = 3,
                                 align = "center",
                                 br(),
                                 br(),
                                 titlePanel(HTML('<span style="color:#000000; font-size: 15px; font-weight:bolder; font-family:Verdana; align=center; ">Shared Horror Movies In Top 100<span>')),
                                 br(),
                                 tableOutput("imdbInRt")
                                 
                                 
                          ),
                          
                          column(width = 9,
                                 align = "center",
                                 br(),
                                 br(),
                               
                                 titlePanel(HTML('<span style="color:#000000; font-size: 15px; font-weight:bolder; font-family:Verdana; align=center; ">Ratings Shared Horror Movies: IMDb vs Rotten Tomatoes<span>')),
                                 
                                 plotOutput("bar"),
                                 br(),
                                 downloadButton('export', "Export this graph to PDF")
                                                                            
                                 
                                 
                          ))
                        ),
                  
                  
                tabPanel("Browse horror movies",
                fluidRow(
                  column(width = 12,
                         align = "center",
                         titlePanel(HTML('<span style="color:#000000; font-size: 30px; font-weight:bolder; font-family:Verdana; align=center; ">Click the specific movie database you want to see<span>')),
                         HTML("<br><br>"),
                         actionButton(inputId = "norm", label = "Rotten Tomatoes", width = 400, icon = icon("bar-chart-o")),
                         actionButton(inputId = "unif", label = "IMDb", width = 400, icon = icon("bar-chart-o")),
                         HTML("<br><br>")
                         
                  )),
                
                fluidRow(
                  column(width = 6, 
                         plotlyOutput("plot1")
                         
                  ),
                  column(width = 6,
                         align = "center",
                         #verbatimTextOutput("info")
                         # htmlOutput("inc")
                         tableOutput('topten')
                  )),
                
                fluidRow(
                  column(width = 12,
                         align = "center",
                         HTML("<br>"),
                         titlePanel(titlePanel(HTML('<span style="color:#000000; font-size: 20px; font-weight:bolder; font-family:Verdana; align=center; ">Top 100 best rated horror movies list<span>'))),
                         dataTableOutput('table')
                         
                  ))),
                tabPanel("Settings", side = "right",
                         fluidRow(
                           column(width = 12,
                                  HTML("<br>"),
                                  shinythemes::themeSelector()
                                  
                           ))),
                
                navbarMenu("Sources",
                tabPanel("IMDb",
                         fluidRow(
                           column(width = 12,
                                htmlOutput("imdbsource")
                                  
                           ))),
                
                tabPanel("Rotten Tomatoes",
                         fluidRow(
                           column(width = 12,
                                htmlOutput("rottentomatoessource")
                                  
                           )))
                
                )
                
                
                )
                
)


server <- function(input, output) {
  
  #create empty dataframe
  rottentomatoes <- data.frame()
  
  rottentomatoesurl_list <- c("https://www.rottentomatoes.com/top/bestofrt/top_100_horror_movies/")
  
  for (i in rottentomatoesurl_list) {
    web_page <-read_html(i)
    web_table <- html_table(web_page,fill = TRUE,header=TRUE)
    web_df <-data.frame(web_table[3])
    
    # # strip out % from ratings so that we can do something with that number
    web_df$RatingTomatometer <- as.numeric(gsub("%", "", web_df$RatingTomatometer))
    # # add type of movie to data frame
    web_df$type <- as.character(gsub("http://www.rottentomatoes.com/top/bestofrt/top_100_", "", i))  
    # # append to a master data frame
    rottentomatoes  <- rbind(rottentomatoes , web_df)
   
    #ordering the ratings from highest to lowest
    order(rottentomatoes$RatingTomatometer)
    order(rottentomatoes$RatingTomatometer, decreasing = TRUE)   
    rottentomatoes <-  rottentomatoes[order(rottentomatoes$RatingTomatometer, decreasing = TRUE),]
  }
  
  # cleaning up some data
  rottentomatoes$type <- as.character(gsub("_movies", "",  rottentomatoes$type))
  rottentomatoes$type <- as.character(gsub("http://www.rottentomatoes.com/top/bestofrt/", "alltime",  rottentomatoes$type))
  rottentomatoes$type <- as.character(gsub("_", " ",  rottentomatoes$type))
  rottentomatoes$name <- "Rotten Tomatoes"
  rottentomatoes$rottenrank <- 1:nrow(rottentomatoes)
  ratingtomato <- rottentomatoes$RatingTomatometer 
  
  #top ten movies rotten tomatoes
  toptenrotten <- rottentomatoes[1:10,2:3]
  
  
  ####################################### IMDB ###########################################
  
  #loads imdb csv into R
  imdb <- read.csv("data/IMDB-Movie-Data.csv")
  
  #creates empty dataframe
  imdb_df <- data.frame()
  
  #puts the csv into a dataframe and selects relevant columns
  imdb_df <- rbind(imdb_df, imdb[,c(1,2,3,5,7,9,10)])
  
  #changes the column names of the dataframe
  colnames(imdb_df) <- c("Rank", "Title", "Genre", "Director", "Year", "Rating", "Votes")
  
  #executes a stored procedure which creates the imdb database

  #writes the csv to the database
      dbWriteTable(conn, name = "imdb", value =imdb_df, row.names = FALSE)
  
  #calls a stored procedure which retrieves all the data from imdb database just in case I will need it
  query <- paste0("exec FETCHIMDBDATA")
  imdb_data <- dbGetQuery(conn, query)
  imdb_data_df <- data.frame()
  imdb_data_df <- rbind(imdb_data_df, imdb_data)
  
  
  
  #calls a stored procedure to get the top 100 best rated horror movies of imdb
  query <- paste0("exec TOPHUNDREDIMDB")
  tophundredimdb <- dbGetQuery(conn, query)
  tophundredimdb_df <- data.frame()
  tophundredimdb_df <- rbind(tophundredimdb_df, tophundredimdb)
  tophundredimdb_df$Type <- "IMDB"
  tophundredimdb_df$imdbRank <- 1:nrow(tophundredimdb_df) 
  tophundredimdb_df$Rating <- tophundredimdb_df$Rating * 10
  tophundredimdb_df$Title <- paste(tophundredimdb_df$Title, " (",tophundredimdb_df$Year,")", sep="")
  
  
  #top ten movies imdb
  toptenimdb <- tophundredimdb_df[1:10,1:9]
  toptenimdb_good <- toptenimdb[ , c(6, 2)]
  imdbrating <- tophundredimdb_df$Rating * 10
  
  #tophundredimdb_df$Title %in% rottentomatoes$Title
  
  imdbInRT <- tophundredimdb_df$Title[tophundredimdb_df$Title %in% rottentomatoes$Title]
  rtInImdb <- rottentomatoes$Title[rottentomatoes$Title %in% tophundredimdb_df$Title]
  
  
  # merge the two dataframes into 1
  merge <- data.frame()
  merge <- cbind(tophundredimdb_df, rottentomatoes)
  
  
  #reactive 
  movie <- reactiveValues(data = merge)
  observeEvent(input$norm, {  output$plot1 <- renderPlotly({
    
    #reactive plot for best rated horror movies of IMDB and Rotten Tomatoes
    ggplot(iris, tooltip = "text") + 
      ggtitle(movie$data3) +
      xlab("Rank") +
      ylab("Rating") +
      geom_point(data=rottentomatoes, aes(x=rottentomatoes$Rank, y=rottentomatoes$RatingTomatometer, text=paste(rottentomatoes$Title)), color='orange') +

      theme(
           panel.border = element_blank(),
            legend.key = element_blank(),
            axis.ticks = element_blank(),
            panel.grid = element_blank(),
           panel.grid.minor = element_blank(),
           panel.grid.major = element_blank(),
            panel.background = element_blank(),
            plot.background = element_rect(fill = "transparent",colour = NA)
           
           )
    
       })
  })
  #Reactive table top 10 horror movies, imdb and rotten tomatoes
  observeEvent(input$norm, { output$topten <- renderTable({toptenrotten}, caption="Top 10 leaderboard of best rated horror movies")})
  observeEvent(input$norm, { movie$data3 <- print("Graph overview of Rotten Tomatoes top 100 horror movies")})
  observeEvent(input$norm, { output$table <- renderDataTable(rottentomatoes[2:4])})
  
  observeEvent(input$unif, {
    
    #reactive plot for best rated horror movies of IMDB and Rotten Tomatoes
    output$plot1 <- renderPlotly({
        
        ggplot(iris, tooltip = "text") + 
        ggtitle(movie$data3) +
        xlab("Rank") +
        ylab("Rating") +
        geom_point(data=tophundredimdb_df, aes(x=tophundredimdb_df$imdbRank, y=tophundredimdb_df$Rating, text=paste(tophundredimdb_df$Title)), color='orange')  +
        theme(
             panel.border = element_blank(),
              legend.key = element_blank(),
              axis.ticks = element_blank(),
              panel.grid = element_blank(),
              panel.grid.minor = element_blank(),
              panel.grid.major = element_blank(),
              panel.background = element_blank(),
              plot.background = element_rect(fill = "transparent",colour = NA)) 

    })
  })
  
  observeEvent(input$unif, { output$topten <- renderTable({toptenimdb_good}, caption="Top 10 leaderboard of best rated horror movies")})
  observeEvent(input$unif, { movie$data3 <- print("Graph overview of IMDb top 100 horror movies")})
  observeEvent(input$unif, { output$table <- renderDataTable(tophundredimdb_df[,c(2,6,7)]) } )
 
  observeEvent(input$rankslider, { } )
   
  
  
  #not in use temporarily
  getPage<-function() {
    return((HTML(readLines('http://www.imdb.com/'))))
  }
  output$imdbsource<-renderUI({
    getPage()
  })
  
  getPage2<-function() {
    return((HTML(readLines('https://www.rottentomatoes.com'))))
  }
  output$rottentomatoessource<-renderUI({
    getPage2()
  })
  

  
  observeEvent(c(input$rankslider,input$ratingslider), { 
  var <- input$rankslider
  var2 <- input$ratingslider
  output$plot2 <- renderPlotly({
    
    #fun loader for the teacher 
    withProgress(message = 'Making plot for Peter Odenhoven', value = 0, {
      
      n <- 10
    
      for (i in 1:n) {
        
        incProgress(1/n, detail = paste("Preparing data", i))
        print("progress")
        
        Sys.sleep(0.05)
        
      }
    })
    
    ggplotly(
    ggplot(iris, tooltip = "text") + 
      ggtitle("IMDb versus Rotten Tomatoes") +
      xlab("Rank") +
      ylab("Rating") +
      geom_point(data=tophundredimdb_df[(1:var),], aes(x=tophundredimdb_df[(1:var), 9], y=tophundredimdb_df[(1:var), 6], shape = tophundredimdb_df[(1:var), 8], text=paste(tophundredimdb_df[(1:var), 2])), color='orange')  +
      geom_point(data=rottentomatoes[(1:var),], aes(x=rottentomatoes[(1:var), 7], y=rottentomatoes[(1:var), 2], shape = rottentomatoes[(1:var),6], text=paste(rottentomatoes[(1:var), 3])), color='red') +
      labs(shape='Movie Database') +
      

      
      theme(
        legend.position = "center",
        legend.title.align = "center",
        panel.border = element_blank(),
       # legend.key = element_blank(),
        axis.ticks = element_blank(),
        panel.grid = element_blank(),
        panel.grid.minor = element_blank(),
        panel.grid.major = element_blank(),
        panel.background = element_blank(),
        plot.background = element_rect(fill = "transparent",colour = NA))

    )
    
  })
  
  } )
  
  
  
  #tells what the slider has to do
  observeEvent(input$rankslider, { 
    
    rottennew <- rottentomatoes[rottentomatoes$rottenrank == input$rankslider, ]
    output$headtoheadrotten <- renderTable({rottennew[c(7,3,2)]}, caption=paste0("RottenTomatoes movie that ends the line with rank ",input$rankslider )) 
    imdbnew <- tophundredimdb_df[tophundredimdb_df$imdbRank == input$rankslider, ]   
    output$headtoheadimdb <- renderTable({imdbnew[c(9,2,6)]}, caption=paste0("IMDb movie that ends the line with rank ",input$rankslider ))
    
    })
  
  #creating the impossible... both imdb and rotten tomatoes in one single visualization
  output$imdbInRt <- renderTable({imdbInRT}, hover = TRUE, colnames = FALSE, caption="Movies that appear in both Rotten Tomatoes as well as in IMDb top 100")
  
  imdbInRTInfo <- tophundredimdb_df[tophundredimdb_df$Title %in% imdbInRT,]
  rtInImdbInfo <- rottentomatoes[rottentomatoes$Title %in% imdbInRT,]
  
  imdbInRTInfoClean <- imdbInRTInfo[,c(2,6)]
  rtInImdbInfoClean <- rtInImdbInfo[,c(3,2)]
  
  colnames(rtInImdbInfoClean) <- c("Title", "RottenTomatoes")
  colnames(imdbInRTInfoClean) <- c("Title", "IMDB")
  
  rtInImdbInfoClean <- with(rtInImdbInfoClean,  rtInImdbInfoClean[order(rtInImdbInfoClean$Title) , ])
  imdbInRTInfoClean<- with(imdbInRTInfoClean,  imdbInRTInfoClean[order(imdbInRTInfoClean$Title) , ])
  
  imdbInRTInfoClean$RottenTomatoes <- rtInImdbInfoClean$RottenTomatoes
  
  
  df1 <- gather(imdbInRTInfoClean, Type, value, -Title)

  
  
  #finally, the visualization of data from both movie databases
  output$bar <- renderPlot({
    
    output$export = downloadHandler(
      filename = function() {"sharedmoviegraph.pdf"},
      content = function(file) {
        ggsave(file, device = "pdf", width=11, height=8.5)
      }
    )
   
    ggplot(df1, aes(Title, value)) +   
      geom_bar(aes(fill = Type), position = "dodge", stat = "identity") +
      geom_text(aes(label=value, group=Type), position=position_dodge(width=0.9), vjust=-0.25) +
      xlab("Movie") +
      ylab("Rating") +
     

      
    theme(
      panel.border = element_blank(),
      # legend.key = element_blank(),
      axis.ticks = element_blank(),
      panel.grid = element_blank(),
      panel.grid.minor = element_blank(),
      panel.grid.major = element_blank(),
      panel.background = element_blank(),
      plot.background = element_rect(fill = "transparent",colour = NA))
   
   
  
  })
  


  
}

shinyApp(ui, server)


