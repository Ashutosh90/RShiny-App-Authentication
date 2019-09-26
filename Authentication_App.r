## Authentication
## This is a small app to demonstrate user-managed authentication using a hash to encode passwords.
## Users are stored in a SQL database with passwords along with roles.
## Once a user is logged in the shiny app responds to the user's role.
## In order to use in a real setting, additional code for password management,
## changing and resetting would need to be implemented.

library(shiny)
library(RSQLite)
library(sodium)

setwd("C:/Users/ashutosh/Desktop/user")

## create the initial password database
## This code should be run once to create the initial database of users, passwords and roles
##
# db.pw <- data.frame(user = c('Augustin', 'Matt', 'Harvey'), role = c('Manager', 'User', 'User'), password = c('ABC', 'DEF', 'GHI'))
# db.pw$encrypt <- apply(db.pw, 1, function(x) password_store(x['password']))
# db = dbConnect(SQLite(), dbname = 'auth_hash.sqlite')
# dbSendQuery(db, 'CREATE TABLE pw (user TEXT, password TEXT, role TEXT)')
# apply(db.pw, 1, function(x) dbSendQuery(db, paste0('INSERT INTO pw VALUES("', x['user'], '", "', x['encrypt'], '", "', x['role'], '")')))
# dbDisconnect(db)

## Connect to the database (may be a remote connection)
db = dbConnect(SQLite(), dbname = 'auth_hash.sqlite')

server <- function(input, output, session) {
  
  ## Initialize - user is not logged in
  user <- reactiveValues(login = FALSE, name = NULL, role = NULL, header = NULL)
  ## Display login modal
  observe({
    showModal(modalDialog(
      title = "Enter Login Details",
      textInput('userInp', 'PSID'),
      passwordInput('pwInp', 'Password'),
      actionButton('butLogin', 'Login', class = 'btn action-button btn-success', icon = icon('sign-in')),
      br(),
      br(),
      actionButton('butRegisterA', 'Register', class = 'btn action-button btn-info', icon = icon('user-plus')),
      actionButton('butFPA', 'Forgot Password', class = 'btn action-button btn-warning', icon = icon('key')),
      size = 'm',
      style="padding-left:25%",
      easyClose = FALSE,
      footer = NULL
    ))
  })
  
  
  ## Login Redirect
  observeEvent(input$butLoginA, {  ## login button pressed from another page
    showModal(modalDialog(
      title = "Enter Login Details",
      textInput('userInp', 'PSID'),
      passwordInput('pwInp', 'Password'),
      actionButton('butLogin', 'Login', class = 'btn action-button btn-success', icon = icon('sign-in')),
      br(),
      br(),
      actionButton('butRegisterA', 'Register', class = 'btn action-button btn-info', icon = icon('user-plus')),
      actionButton('butFPA', 'Forgot Password', class = 'btn action-button btn-warning', icon = icon('key')),
      size = 'm',
      style="padding-left:25%",
      easyClose = FALSE,
      footer = NULL
    ))
    
  })
  
  
  
  ## Login query
  observeEvent(input$butLogin, {  ## login button pressed
    req(input$userInp, input$pwInp)  ## ensure we have inputs
    removeModal()  ## remove the modal
    pw_out <- dbGetQuery(db, paste0('SELECT password FROM pw WHERE user = \"', input$userInp, '\"'))  ## query database
    if (nrow(pw_out) == 0) {  ## user does not exist
      user$login <- FALSE
      user$header <- 'ERROR - UNKNOWN USER'
    } else {
      pw <- as.character(pw_out$password)[[1]]  ## grab password from database
      passwordVerified <- password_verify(pw, input$pwInp)  ## check that it matches user input
      if (passwordVerified) {  ## match
        user$login <- TRUE
        user$name <- input$userInp
        user$role <- db.pw[db.pw$user == input$userInp, 'role']
        user$header <- paste0(user$name, ' (', user$role, ')')
      } else {  ## no match
        user$login <- FALSE
        user$header <- 'ERROR - INCORRECT PASSWORD'
      }
    }
  })
  
  
  ## Register Redirect
  observeEvent(input$butRegisterA, {  ## register button pressed from another page
    showModal(modalDialog(
      title = "Enter Registration Details",
      textInput('userInp','PSID'),
      radioButtons('roleInp', 'Role', choices = list("RM" = "RM", "Test User" = "tester"), selected= "RM"),
      passwordInput('pwInp', 'Password'),
      passwordInput('cpwInp', 'Confirm Password'),
      dateInput('sq1Inp', 'Security Question 1: When did you join HSBC?'),
      textInput('sq2Inp', 'Security Question 2: What is your favorite food?'),
      actionButton('butRegister', 'Register', class = 'btn action-button btn-info', icon = icon('user-plus')),
      br(),
      br(),
      actionButton('butLoginA', 'Login', class = 'btn action-button btn-success', icon = icon('sign-in')),
      actionButton('butFPA', 'Forgot Password', class = 'btn action-button btn-warning', icon = icon('key')),
      size = 'm',
      style="padding-left:25%",
      easyClose = FALSE,
      footer = NULL
    ))
    
  })
  
  
  
  ## Forgot Password
  observeEvent(input$butFPA, {  ## Forgot password button pressed from another page
    showModal(modalDialog(
      title = "Enter Details for Password Reset",
      textInput('userInp', 'PSID'),
      dateInput('sq1Inp', 'Sequrity Question 1: When did you join HSBC?'),
      textInput('sq2Inp', 'Sequrity Question 2: What is your favorite food?'),
      passwordInput('pwInp', 'New Password'),
      passwordInput('cpwInp', 'Confirm New Password'),
      actionButton('butFP', 'Reset Password', class = 'btn action-button btn-warning', icon = icon('key')),
      br(),
      br(),
      actionButton('butLoginA', 'Login', class = 'btn action-button btn-success', icon = icon('sign-in')),
      actionButton('butRegisterA', 'Register', class = 'btn action-button btn-info', icon = icon('user-plus')),
      size = 'm',
      style="padding-left:25%",
      easyClose = FALSE,
      footer = NULL
    ))
    
  })
  
  
  

  
  ## close database on exit
  session$onSessionEnded(function(){
    dbDisconnect(db)
  })
  
  output$data <- renderUI({
    h4(user$header)
  })
  
  output$myPlot <- renderPlot({
    req(user$login)
    if (user$role == 'Manager') {  ## If manager role, display iris plot
      plot(iris$Sepal.Length, iris$Sepal.Width)
    } else {  ## If user role, display mtcars plot
      plot(mtcars$mpg, mtcars$cyl)
    }
  })
  
}

ui <- fluidPage(
  uiOutput('data'),
  plotOutput('myPlot')
)

shinyApp(ui = ui, server = server)