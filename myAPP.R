library(shiny)
library(shinythemes)
library(httr)
library(tidyverse)
library(jsonlite)

# all_player <- c()
# 
# for (i in 1:33) {
#   url_text <- paste0('https://www.balldontlie.io/api/v1/players?per_page=100&page=',i)
#   temp <- GET(url_text)
#   temp_json <- content(temp, as = "text", encoding = "UTF-8")
#   temp_result <- fromJSON(temp_json)
#   tempdata <- temp_result$data
#   tempdata <- cbind(tempdata[,c(1:6)],tempdata$team)
#   rownames(tempdata) <- NULL
#   rownames(all_player) <- NULL
#   all_player <- rbind(all_player,tempdata) 
# }
#save(all_player,file = "all_player.Rdata")

load(file = 'all_player.Rdata')
all_player$player_fullname <- paste0(all_player$first_name," ",all_player$last_name)
names(all_player) <- c(names(all_player)[1:6],"team_id",names(all_player)[8:14])

# Define UI for random distribution app ----
ui <- fluidPage(
  # App title ----
  titlePanel("My NBA Shiny App"),
  tabsetPanel(type = "tabs",
              tabPanel("Player",sidebarPanel(sliderInput("input11","select the seasons",min = 2000,max = 2018,value = 2009),
                                             selectInput("input12","select the Player",choices = unique(all_player$player_fullname),
                                                         selected = "Kobe Bryant")),
                       mainPanel(column(tableOutput("data11"),width = 6),column(tableOutput("data12"),width = 6),
                                 br(),
                                 h4("The season averages"),
                                 tableOutput("data13"))),
              tabPanel("Team",sidebarPanel(selectInput("input21","select the team",choices = unique(all_player$full_name))),
                       mainPanel(
                         h4("The team information"),
                         tableOutput("data21"))),
              tabPanel("Score",sidebarPanel(
                sliderInput("input31","select the seasons",min = 2000,max = 2018,value = 2009),
                selectInput("input32","select the team1",choices = unique(all_player$full_name)),
                uiOutput("visitor")),
                mainPanel(tableOutput("data31")))  ,
              # tabPanel("Rank",sidebarPanel(radioButtons("input41", "Conference:",
              #                                                 c("West", "East"))),mainPanel()),
              tabPanel("Mamba out",HTML('<iframe width="760" height="515" src="https://www.youtube.com/embed/Eg0mxPXIpLY" frameborder="0" allow="accelerometer; autoplay; encrypted-media; gyroscope; picture-in-picture" allowfullscreen></iframe>'),
                       h3("Remembering Kobe Bryant’s farewell speech after his last NBA game. Youtube, uploaded by ESPN, 27 January 2020,
https://www.youtube.com/watch?v=Eg0mxPXIpLY"))
  )
)

server <- function(input, output,session) {
  ###########player
  paneldata11 <- reactive({
    the_name <- input$input12
    the_id <- all_player[all_player$player_fullname==the_name,]$id
    url_text <- paste0('https://www.balldontlie.io/api/v1/players/',the_id)
    data2 <- GET(url_text)
    player_json <- content(data2, as = "text", encoding = "UTF-8")
    player_result <- fromJSON(player_json)
    player_information <- data.frame(unlist(player_result[c("first_name","height_feet","height_inches","last_name","position","weight_pounds")]))
    names(player_information) <- "player_information"
    team_information <- data.frame(unlist(player_result["team"]))
    names(team_information) <- "team_information"
    result <- list(player_information,team_information)
    result
  })

  output$data11 <- renderTable({
    paneldata11()[[1]]
  },rownames = TRUE)

  output$data12 <- renderTable({
    paneldata11()[[2]]
  },rownames = TRUE)

  paneldata12 <-  reactive({
    the_name <- input$input12
    the_id <- all_player[all_player$player_fullname==the_name,]$id
    the_season <- input$input11
    url_text <- paste0("https://www.balldontlie.io/api/v1/season_averages?season=",the_season
                       ,"&player_ids[]=",the_id)
    temp <- GET(url_text)
    player_json <- content(temp, as = "text", encoding = "UTF-8")
    player_result <- fromJSON(player_json)
    player_result$data
  })

  output$data13 <- renderTable({
    paneldata12()
  },rownames = TRUE)

  ###########team
  paneldata21 <- reactive({
    the_name <- input$input21
    the_id <- all_player[all_player$full_name==the_name,]$team_id
    url_text <- paste0('https://www.balldontlie.io/api/v1/teams/',the_id[1])
    data2 <- GET(url_text)
    player_json <- content(data2, as = "text", encoding = "UTF-8")
    player_result <- fromJSON(player_json)
    data.frame(player_result)
  })

  output$data21 <- renderTable({
    paneldata21()
  })
  ###########score
  paneldata31 <- reactive({
    the_season <- input$input31
    the_name <- input$input32
    team_ids <- all_player[all_player$full_name==the_name,]$team_id
    url_text <- paste0('https://www.balldontlie.io/api/v1/games?seasons[]=',
                       the_season,'&team_ids[]=',team_ids[1],"&per_page=100")
    data2 <- GET(url_text)
    player_json <- content(data2, as = "text", encoding = "UTF-8")
    player_result <- fromJSON(player_json)
    player_result$data
  })

  output$visitor <- renderUI({
    temp <- paneldata31()
    choices <- unique(temp$visitor_team$full_name)
    number <- which(choices==input$input32)
    selectInput("input33","select the team2",choices = choices[-number])
  })

  output$data31 <- renderTable({
    gamedata <- paneldata31()
    gamedata1 <-gamedata[,c(1,2,4:9,11)]
    gamedata2 <- gamedata$home_team
    names(gamedata2) <- paste0("home.",names(gamedata2))
    gamedata3 <- gamedata$visitor_team
    names(gamedata3) <- paste0("visitor.",names(gamedata3))
    gamedata <- cbind(gamedata1,gamedata2)
    gamedata <- cbind(gamedata,gamedata3)
    home <- input$input32
    visitor <- input$input33
    the_game <- subset(gamedata,(home==gamedata$home.full_name&visitor==gamedata$visitor.full_name)|
                         (visitor==gamedata$home.full_name&home==gamedata$visitor.full_name))
    the_game[,c(2,15,3,9,22)]
  })
  
}

shinyApp(ui, server)