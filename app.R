# Load Packages ----
library(shiny)
library(shinydashboard)
library(shinyBS)
library(shinyWidgets)
library(boastUtils)

# Define global constants and load question banks ----
questionBank1 <- read.csv("questionBank1.csv", header = TRUE)
questionBank1 <- na.omit(questionBank1)

# Define UI for App ----
ui <- list(
  ## Create the app page ----
  dashboardPage(
    skin = "black",
    ### Create the app header ----
    dashboardHeader(
      title = "Representations", 
      titleWidth = 250,
      tags$li(class = "dropdown", actionLink("info", icon("info"))),
      tags$li(
        class = "dropdown",
        boastUtils::surveyLink(name = "Repersentations_of_Games")
      ),
      tags$li(
        class = "dropdown",
        tags$a(href = 'https://shinyapps.science.psu.edu/',
               icon("house")
        )
      )
    ),
    ### Create the sidebar/left navigation menu ----
    dashboardSidebar(
      width = 250,
      sidebarMenu(
        id = "pages",
        menuItem("Overview", tabName = "overview", icon = icon("gauge-high")),
        menuItem("Prerequisites", tabName = "prerequisites", icon = icon("book")),
        menuItem("Examples", tabName = "examples", icon = icon("book-open-reader")),
        menuItem("Explore", tabName = "explore", icon = icon("wpexplorer")),
        menuItem("Challenge", tabName = "challenge", icon = icon("gears")),
        menuItem("References", tabName = "references", icon = icon("leanpub"))
      ),
      tags$div(
        class = "sidebar-logo",
        boastUtils::sidebarFooter()
      )
    ),
    ### Create the content ----
    dashboardBody(
      tabItems(
        #### Set up the Overview Page ----
        tabItem(
          tabName = "overview",
          withMathJax(),
          h1("Repersentations of Games"),
          p("This is an app that plays extensive form and normal form representations
            of the games to explore how decision markers such as firms or players
            will choose the strategies in a conflict situaion and react to the strategy 
            of their rivals."),
          h2("Instructions"),
          tags$ol(
            tags$li("Review key topics of components and consumption of a game."),
            tags$li("Explore the differences of games ins extensive form and normal 
                    form."),
            tags$li("Challenge yourself by play the game as one of the players to 
                    select the best strategy for you."),
            tags$li("Play the game to test how far you've come.")
          ),
          br(),
          div(
            style = "text-align: center;",
            bsButton(
              inputId = "goToExam",
              label = "GO!",
              size = "large",
              icon = icon("bolt"),
              style = "default"
            )
          ),
          ##### Create two lines of space
          br(),
          br(),
          h2("Acknowledgements"),
          p(
            "This version of the app was developed and coded by Luqi Jiao Emanuele.",
            br(),
            br(),
            "Cite this app as:",
            br(),
            boastUtils::citeApp(),
            br(),
            br(),
            div(class = "updated", "Last Update: 7/11/2023 by LJE.")
          )
        ),
        #### Set up the Prerequisites Page ----
        tabItem(
          tabName = "prerequisites",
          withMathJax(),
          h2("Prerequisites"),
          p("Three elements of a game:"),
          tags$ul(
            tags$li("A set of players."),
            tags$li("A set of strategies for each player."),
            tags$li("A payoff function with a payoff to each player with all possible 
                    strategies.")
          ),
          br(),
          p("Assumptions of payoff matrix:"),
          tags$ul(
            tags$li("The players will play their strategies simultaneously."),
            tags$li("This is one-shot game, meaning all players paly only once.")
          ),
          box(
            title = strong("Extensive Form"),
            status = "primary",
            collapsible = TRUE,
            collapsed = FALSE,
            width = '100%',
            "We will use game tree in extensive form.",
            tags$figure(
              class = "centerFigure",
              tags$img(
                scr = "www/prere1.jpg",
                alt = "Example of extensive form in game tree."
                )
              ),
            br(),
            "There are three sets of objects in extensive form:",
            tags$ol(
              tags$li("Nodes"),
              tags$li("Branches"),
              tags$li("Information sets")
              )
            ),
          box(
            title = strong("Normal Form"),
            status = "primary",
            collapsible = TRUE,
            collapsed = FALSE,
            width = '100%',
            "We will use payoff matrix in normal form.",
            tags$figure(
              class = "centerFigure",
              tags$img(
                scr = "www/prere2.jpg",
                alt = "Example of normal form in payoff matrix."
                )
              ),
            br(),
            "There are three sets of objects in normal form:",
            tags$ol(
              tags$li("Players"),
              tags$li("Strategies"),
              tags$li("Payoffs")
              )
            )
          ),
        #### Set up the Examples Page ----
        tabItem(
          tabName = "examples",
          fluidPage(
            tabsetPanel(
              #####First tab ----
              tabPanel(
                title = "Extensive Form",
                br(),
                h3("Game Tree"),
                br(),
                p("Test your understanding by trying out these questions."),
                fluidRow(
                  uiOutput(outputId = "questionPlot"),
                  wellPanel(
                  br(),
                  selectInput(
                    inputId = "response",
                    label = "Select your answer", 
                    choices = list("A", "B", "C", ""), 
                    selected = ""
                  ),
                  bsButton(
                    inputId = "submit",
                    label = "Submit",
                    size = "large",
                    style = "default"
                  ),
                  br(),
                  br(),
                  p("Feedback"),
                  uiOutput("icon"),
                  uiOutput("answer"),
                  bsButton(
                    inputId = "newChallenge",
                    label = "New Challenge",
                    size = "large",
                    style = "default"
                  )
                  )
                )
              ),
              #####Second tab ----
              tabPanel(
                title = "Normal Form",
                br(),
                h3("Payoff Matrix"),
                br(),
                p(""),
                
              ),
            )
          )
          
        ),
        #### Set up an Explore Page ----
        tabItem(
          tabName = "explore",
          fluidPage(
            tabsetPanel(
              #####First tab ----
              tabPanel(
                title = "Extensive Form",
                br(),
                h3("Game Tree"),
                br(),
                p("")
              ),
              #####Second tab ----
              tabPanel(
                title = "Normal Form",
                br(),
                h3("Payoff Matrix"),
              ),
            )
          ),
          div(
            style = "text-align: center;",
            bsButton(
              inputId = "goToChallenge",
              label = "Challenge!",
              size = "large",
              icon = icon("bolt"),
              style = "default"
            )
          ),
        ),
        #### Set up a Challenge Page ----
        tabItem(
          tabName = "challenge",
          withMathJax(),
          h2("Challenge Yourself"),
          p("The general intent of a Challenge page is to have the user take
            what they learned in an Exploration and apply that knowledge in new
            contexts/situations. In essence, to have them challenge their
            understanding by testing themselves."),
          p("What this page looks like will be up to you. Something you might
            consider is to re-create the tools of the Exploration page and then
            a list of questions for the user to then answer.")
        ),
        #### Set up a Simulation Page ----
        tabItem(
          tabName = "game",
          withMathJax(),
          h2("Practice/Test Yourself with [Type of Game]"),
          p("On this type of page, you'll set up a game for the user to play.
            Game types include Tic-Tac-Toe, Matching, and a version Hangman to
            name a few. If you have ideas for new game type, please let us know.")
        ),
        #### Set up the References Page ----
        tabItem(
          tabName = "references",
          withMathJax(),
          h2("References"),
          p(
            class = "hangingindent",
            "Bailey, E. (2015). shinyBS: Twitter bootstrap components for shiny.
            (v0.61). [R package]. Available from
            https://CRAN.R-project.org/package=shinyBS"
          ),
          p("https://www.investopedia.com/terms/m/matching-pennies.asp#:~:text=Matching%20Pennies%20is%20a%20basic%20game%20theory%20example,to%20the%20players%20that%20are%20not%20the%20same."),
          br(),
          br(),
          br(),
          boastUtils::copyrightInfo()
        )
      )
    )
  )
)

# Define server logic ----
server <- function(input, output, session) {

  ## Set up Info button ----
  observeEvent(
    eventExpr = input$info,
    handlerExpr = {
      sendSweetAlert(
        session = session,
        type = "info",
        title = "Information",
        text = "This App Template will help you get started building your own app"
      )
    }
  )
  
  ## Move to Exam page ----
  observeEvent(
    eventExpr = input$goToExam,
    handlerExpr = {
      updateTabItems(
        session = session,
        inputId = "pages",
        selected = "examples"
      )
    }
  )
  
  ## Examples Page Buttons ----
  ### Payoff Matrix ----
  challengeElements <- reactiveValues(
    # Creates a vector of shuffled integers which we can use for the id column
    promptIds = sample(1:nrow(questionBank), size = nrow(questionBank), replace = FALSE),
    # Create current index
    currentIndex = 1,
    # Create a flag if current question has been answered
    answered = FALSE,
    # Create a flag for first visit to page
    firstTime = TRUE
  )
  
  ### Set watcher to iterate current question
  observeEvent(
    eventExpr = c(input$pages, input$newChallenge),
    handlerExpr = {
      if (input$pages == "challenge" & challengeElements$firstTime) {
        challengeElements$firstTime <- FALSE
      } else if (challengeElements$currentIndex == nrow(questionBank1)) {
        sendSweetAlert(
          session = session,
          type = "error",
          title = "End of Game",
          text = "You've played through all of the questions."
        )
      } else {
        challengeElements$currentIndex <- challengeElements$currentIndex + 1
      }
    }
  )
  
  ### Display challenge plot and question ----
  scoreLevel <- reactiveVal(0)
  
  output$questionPlot <- renderUI(
    expr = {
      questionId <- challengeElements$promptIds[challengeElements$currentIndex]
      tagList(
        h3("Question Plot"),
        HTML(questionBank1$extraOutput[questionId]),
        p(questionBank1$question[questionId])
      )
    }
  )
  
  random_order <- reactiveVal()
  
  random_choice <- function() {
    choices <- c("1", "2", "3")
    random_order <- sample(choices)
    return(random_order)
  }
  
  random_choice()
  
  observe({
    random_order_val <- random_order()
    
    output$choiceA <- renderUI({
      questionId <- challengeElements$promptIds[challengeElements$currentIndex]
      tagList(
        HTML(questionBank1[[paste0("A")]][questionId])
      )
    })
    
    output$choiceB <- renderUI({
      questionId <- challengeElements$promptIds[challengeElements$currentIndex]
      tagList(
        HTML(questionBank1[[paste0("B")]][questionId])
      )
    })
    
    output$choiceC <- renderUI({
      questionId <- challengeElements$promptIds[challengeElements$currentIndex]
      tagList(
        HTML(questionBank1[[paste0("C")]][questionId])
      )
    })
  })
  
  
  
  current_question <- reactiveVal()
  
  random_question <- function() {
    random_index <- sample(nrow(questionBank1), 1)
    current_question(questionBank1[random_index, ])
  }
  
  random_question()
  
  output$question <- renderText({
    current_question()$question
  })
  
  output$extraOutput <- renderImage({
    if (!is.null(current_question()$extraOutput) && nchar(current_question()$extraOutput) > 0) 
    {
      return(list(src = current_question()$extraOutput))
    }
  },
  deleteFile = FALSE)
  
  output$A <- renderText({
    if (!is.null(current_question()$A) && nchar(current_question()$A) > 0) 
    {
      return(list(src = current_question()$A))
    }
  })
  
  output$B <- renderText({
    if (!is.null(current_question()$B) && nchar(current_question()$B) > 0) 
    {
      return(list(src = current_question()$B))
    }
  })
  
  output$C <- renderText({
    if (!is.null(current_question()$C) && nchar(current_question()$C) > 0) 
    {
      return(list(src = current_question()$C))
    }
  })
  
  observeEvent(
    eventExpr = input$submit, 
    handlerExpr = {
      user_answer <- input$response
      correct_answer <- current_question()$answer
    }
  )
  
  ### Get new challenge and reset feedback ----
  observeEvent(
    eventExpr = input$newChallenge,
    handlerExpr = {
      random_choice()
      random_question()
      scoreLevel()
      
      updateSelectInput(
        session = session,
        inputId = "answer",
        selected = ""
      )
      
      output$icon <- renderIcon()
      output$response <- renderUI(NULL)
      
    }
  )

}

# Boast App Call ----
boastUtils::boastApp(ui = ui, server = server)
