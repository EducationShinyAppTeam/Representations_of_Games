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
            div(class = "updated", "Last Update: 10/8/2023 by LJE.")
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
                title = "Normal Form ",
                br(),
                h3("Payoff Matrix "),
                p("Test your understanding by trying out these questions."),
                fluidRow(
                  uiOutput(outputId = "questionPlot"),
                  br(),
                  h4("Answer Choices:"),
                  p("A:"),
                  uiOutput("choiceA"),
                  br(), 
                  p("B:"),
                  uiOutput("choiceB"),
                  br(),
                  p("C:"),
                  uiOutput("choiceC"),
                  br(),
                  wellPanel(
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
                  p("Feedback"),
                  uiOutput("icon"),
                  uiOutput("answer"),
                  br(),
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
                title = "Extensive Form",
                br(),
                h3("Game Tree"),
                br(),
                p(""),
                plotOutput("exploreGraph")
              ),
            )
          ),
          div(
            style = "text-align: center;",
            bsButton(
              inputId = "goToExplore",
              label = "Explore!",
              size = "large",
              icon = icon("bolt"),
              style = "default"
            )
          ),
        ),
        #### Set up an Explore Page ----
        tabItem(
          tabName = "explore",
          fluidPage(
            tabsetPanel(
              #####First tab ----
              tabPanel(
                title = "Normal Form",
                br(),
                h3("Payoff Matrix"),
                br(),
                p("According to the situation, input payoffs in the table below 
                  for each player to create the payoff matrix.")
              ),
              #####Second tab ----
              tabPanel(
                title = "Extensive Form",
                br(),
                h3("Game Tree"),
                br(),
                p("According to the situation, input payoffs in the table below 
                  for each player to create the game tree.")
              ),
              #####Third tab ----
              tabPanel(
                title = "Transform",
                br(),
                h3("Payoff Matrix vs Game Tree"),
                br(),
                p("Given the situation with the information about the strategies 
                  of two players in the game tree, input the payoffs in the payoff matrix or verse versa.")
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
          fluidPage(
            tabsetPanel(
              #####First tab ----
              tabPanel(
                title = "Two Player Game",
                br(),
                p("The following questions will test your knowledge by playing 
                  a two player games.")
              ),
              #####Second tab ----
              tabPanel(
                title = "Three Player Game",
                br(),
                p("The following questions will test your knowledge by playing 
                  a three player games.")
              ),
              #####Third tab ----
              tabPanel(
                title = "Mixed Strategy",
                br(),
                p("The following questions will test your knowledge of two player 
                  games with a mixed strategy..")
              ),
            )
          )
        ),
        #### Set up the References Page ----
        tabItem(
          tabName = "references",
          withMathJax(),
          h2("References"),
          p(class = "hangingindent",
            "Bailey, E. (2022). shinyBS: Twitter bootstrap components for shiny.
            (v 0.61.1). [R package]. Available from 
            https://CRAN.R-project.org/package=shinyBS"
          ),
          p(class = "hangingindent",
            "Carey, R. and Hatfield, N. J. (2022). boastUtils: BOAST Utilities.
            (v 0.1.12.3). [R package]. Available from
            https://github.com/EducationShinyAppTeam/boastUtils"
          ),
          p(class = "hangingindent",
            "Chang, W. and Borges Ribeio, B. (2021). shinydashboard: Create
            dashboards with 'Shiny'. (v. 0.7.2). [R package]. Available from
            https://CRAN.R-project.org/package=shinydashboard"
          ),
          p(class = "hangingindent",
            "Chang, W., Cheng, J., Allaire, J., Sievert, C., Schloerke, B., Xie, Y.,
            Allen, J., McPherson, J., Dipert, A., and Borges, B. (2021). shiny: 
            Web application framework for R, R package. (v 1.7.1). [R package]. 
            Available from https://CRAN.R-project.org/package=shiny"
          ),
          p(class = "hangingindent",
            "Perrier, V., Meyer, F., and Granjon, D. (2022), shinyWidgets: Custom 
            Inputs Widgets for Shiny. (v 0.7.0). [R package]. Available from
            https://CRAN.R-project.org/package=shinyWidgets"
          ),
          p(class = "hangingindent",
            "Wickham, W. (2016), ggplot2: Elegant graphics for data analysis,
            R Package. Springer-Verlag New York. (v 3.3.6). [R package].
            Available from https://ggplot2.tidyverse.org"
          ),
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
  
  ## Move to Example page ----
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
  
  ## Move to Explore page ----
  observeEvent(
    eventExpr = input$goToExplore,
    handlerExpr = {
      updateTabItems(
        session = session,
        inputId = "pages",
        selected = "explore"
      )
    }
  )
  
  ## Move to Challenge page ----
  observeEvent(
    eventExpr = input$goToChallenge,
    handlerExpr = {
      updateTabItems(
        session = session,
        inputId = "pages",
        selected = "challenge"
      )
    }
  )
  
  ## Examples Page Buttons ----
  ### Payoff Matrix ----
  challengeElements <- reactiveValues(
    # Creates a vector of shuffled integers which we can use for the id column
    promptIds = sample(1:nrow(questionBank1), size = nrow(questionBank1), replace = FALSE),
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
  
  ### Display challenge plot and question
  output$questionPlot <- renderUI(
    expr = {
      questionId <- challengeElements$promptIds[challengeElements$currentIndex]
      tagList(
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
      
      if (user_answer == correct_answer) {
        output$icon <- renderIcon("correct", width = 40)
      } else {
        output$icon <- renderIcon("incorrect", width = 40)
      }
      
    }
  )
  
  ### Get new challenge and reset feedback
  observeEvent(
    eventExpr = input$newChallenge,
    handlerExpr = {
      random_choice()
      random_question()
      
      updateSelectInput(
        session = session,
        inputId = "answer",
        selected = ""
      )
      
      output$icon <- renderIcon()
      output$response <- renderUI(NULL)
      
    }
  )
  
  ### Game Tree ----
  exploreDF <- reactive({
    # Adjusts the vertex labels for the leaf nodes if weights are being shown
    probs <- c(
      edgeLabels()[1] * edgeLabels()[3],
      edgeLabels()[1] * edgeLabels()[4],
      edgeLabels()[2] * edgeLabels()[5],
      edgeLabels()[2] * edgeLabels()[6]
    )
    probs <- format(
      x = round(x = probs, digits = 4),
      scientific = F
    )
    edgeLabelsNice <- format(
      x = round(x = edgeLabels(),  digits = 2),
      scientific = F
    )
    # Paste probabilities at bottom of tree
    for(i in 3:6){
      to[i] <- paste("\n", to[i], "\n", probs[i-2])
    }
    # Make actual data frame
    data.frame(from = from, to = to, weight = edgeLabelsNice)
  })
  
  output$exploreGraph <- renderPlot({
    df <- exploreDF()
    # Add labels
    labels <- c("a", "b", "c",
                "d", "c", "d")
    # Adjust spacing (making it consistent with challenge side where this is
    # necessary to give labels more room)
    for (edge in 1:6) {
      if (edge %% 2 == 1) {
        df$weight[edge] <- paste(
          "\n",
          labels[edge],
          "      \n",
          df$weight[edge],
          "      "
        ) # Move right labels further right
      } else {
        df$weight[edge] <- paste(
          "\n      ",
          labels[edge],
          "\n      ",
          df$weight[edge]
        ) # Move left labels further left
      }
    }
    
    # Make actual plot (the plot command is overridden by igraph)
    par(mar = c(0.5, 0, 0.5, 0))
    plot(
      igraph::graph_from_data_frame(df, directed = F),
      label = TRUE,
      edge.label = df$weight,
      edge.color = "#000000",
      edge.width = 1.5,
      vertex.label.color = "#000000",
      edge.label.color = "#000000",
      label.cex = 1.2,
      edge.label.cex = 1.2,
      vertex.color = rep(x = "#E69F0080", times = 6), # Assigns the correct color to all 6 nodes
      layout = igraph::layout_as_tree(
        igraph::graph_from_data_frame(df),
        root = 1
      )
    )
  })

}

# Boast App Call ----
boastUtils::boastApp(ui = ui, server = server)
