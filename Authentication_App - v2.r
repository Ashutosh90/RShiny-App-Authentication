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
db = dbConnect(SQLite(), dbname = 'RMP.sqlite')

#Comment out after first step
# dbSendQuery(db, 'CREATE TABLE Register (
#   userInp TEXT PRIMARY KEY,
#   roleInp TEXT NOT NULL,
#   pwInp TEXT NOT NULL,
#   cpwInp TEXT NOT NULL,
#   sq1Inp TEXT NOT NULL,
#   sq2Inp TEXT NOT NULL)')
  


server <- function(input, output, session) {
  
  
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
  
  
  ## Initialize - user is not logged in
  loginuser <- reactiveValues(login = FALSE, name = NULL, role = NULL, header = NULL)
  
  ## Login query
  observeEvent(input$butLogin, {  ## login button pressed
    req(input$userInp, input$pwInp)  ## ensure we have inputs
    removeModal()  ## remove the modal
    pwr_out <- dbGetQuery(db, paste0('SELECT pwInp, roleInp FROM Register WHERE userInp = \"', input$userInp, '\"'))  ## query database
    if (nrow(pwr_out) == 0) {  ## user does not exist
      loginuser$login <- FALSE
      loginuser$header <- 'ERROR - UNKNOWN USER'
    } else {
      # pw <- pwr_out$pwInp  ## grab password from database
      passwordVerified <- password_verify(pwr_out$pwInp,input$pwInp)  ## check that it matches user input
      if (passwordVerified) {  ## match
        loginuser$login <- TRUE
        loginuser$name <- input$userInp
        loginuser$role <- pwr_out$roleInp
        loginuser$header <- paste0(input$userInp, ' Role -',' (', as.character(pwr_out$roleInp), ')')
      } else {  ## no match
        loginuser$login <- FALSE
        loginuser$header <- 'ERROR - INCORRECT PASSWORD'
      }
    }
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
  
  
  Registeruser <- reactiveValues(register = FALSE, name = NULL, role = NULL, header = NULL)
  ## Register query
  observeEvent(input$butRegister, {  ## register button pressed
    req(input$userInp, input$pwInp, input$roleInp, input$cpwInp, input$sq1Inp, input$sq2Inp)  ## ensure we have inputs
    removeModal()  ## remove the modal
    user_out <- dbGetQuery(db, paste0('SELECT * FROM Register WHERE userInp = \"', input$userInp, '\"'))  ## query database
    if (nrow(user_out) > 0) {  ## user exist
      Registeruser$register <- FALSE
      Registeruser$header <- 'ERROR - USER ALREADY REGISTERED'
    } else {
      dbSendQuery(db, paste0('INSERT INTO Register VALUES("', as.character(input$userInp), '", "', as.character(input$roleInp), '", "', password_store(input$pwInp), '", "', password_store(input$cpwInp), '", "', as.character(input$sq1Inp), '", "', as.character(input$sq2Inp), '")'))
      passwordVerified <- identical(input$pwInp, input$cpwInp) 
      if (passwordVerified) {  ## match
        Registeruser$register <- TRUE
        Registeruser$name <- input$userInp
        Registeruser$role <- input$roleInp
        Registeruser$header <- paste0(Registeruser$name, ' REGISTERED AS ', Registeruser$role, '.')
      } else {  ## no match
        Registeruser$register <- FALSE
        Registeruser$header <- "ERROR - PASSWORD DOESN'T MATCH"
      }
    }
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
    h4(loginuser$header)
    
  })
  
  output$data1 <- renderUI({
    h4(Registeruser$header)
    
  })
  

  
  output$myPlot <- renderPlot({
    req(loginuser$login)
    if (loginuser$role == 'RM') {  ## If manager role, display iris plot
      plot(iris$Sepal.Length, iris$Sepal.Width)
    } else {  ## If user role, display mtcars plot
      plot(mtcars$mpg, mtcars$cyl)
    }
  })
  
}

ui <- fluidPage(
  uiOutput('data'),
  uiOutput('data1'),
  plotOutput('myPlot')
)

shinyApp(ui = ui, server = server)