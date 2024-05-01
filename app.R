library(shiny)
library(bslib)
library(DBI)
library(magrittr)

# Funktion zum laden der DB Tabelle
get_table <- function(tbl_name = "ausgaben",
                      db = db_path) {
  conn <- dbConnect(drv = RSQLite::SQLite(),
                    db)
  df_all <- dbGetQuery(conn,
                       paste0("SELECT ROWID, * FROM ", tbl_name))
  dbDisconnect(conn)
  df_all %>% 
    dplyr::mutate(Betrag = stringr::str_replace(Betrag, ",", "."),
                  Betrag = as.numeric(Betrag))
}

# DB Pfad
if (file.exists("Ausgaben.sqlite")) {
  db_path <- "Ausgaben.sqlite"
} else {
  db_path <- "Ausgaben_temp.sqlite"
}

# User definieren
user1 <- "Bettina"
user2 <- "Michi"

# Korrekturfaktor fuer Berechnung
korr_factor <- 0.45

# Monat und Jahr fuer Tabellenfilter
months <- c("Jänner", "Februar", "März", "April", "Mai", "Juni", "Juli",
            "August", "September", "Oktober", "November", "Dezember")
yrs <- c(2022:format(Sys.Date(), "%Y"))

# Tabelle einlesen
df_all <- reactiveValues(tbl = get_table())

########################## UI ###############################################
ui <- page_sidebar(
  
  # CSS style fuer DT
  tags$style(HTML("
    .dataTable {
        font-size: 11px;
    }")),
  
  # Theme festlegen
  theme = bs_theme(bootswatch = "yeti"),
  # Titel
  title = "Kostenabrechnung",
  fillable_mobile = FALSE,
  # Eingabefelder
  sidebar = sidebar(
    width = 300,
    open = "open",
    title = "Eingabe",
    selectizeInput(inputId = "text", 
                   label = "Kostenpunkt",
                   choices = c("", unique(isolate(df_all$tbl)$Kostenpunkt)),
                   options = list(create = TRUE)),
    
    numericInput("cost", "Betrag", value = NULL),
    
    textInput("comm", label = "Kommentar", placeholder = NULL),
    radioButtons("user", label = NULL, 
                 choices = c(user1, user2),
                 selected = user1),
    shinyWidgets::switchInput("disc2", label = "Korrekturfaktor", value = TRUE,
                              onLabel = "Ja", offLabel = "Nein"),
    dateInput("date", label = "Datum"),
    actionButton("save", label = "Speichern"),
    br(),
    # Output fuer Bestaetigung nach speichern
    textOutput("out")
  ),
  
  # Filter für angezeigte Tabelle mit Berechnungsergebnis und Loeschen-Button
  layout_columns(
    col_widths = breakpoints(
      sm = c(12, 12, 12, 12),
      md = c(6, 6, 12, 12),
      lg = c(4, 4, 4, 12)
    ), 
    selectInput("month", strong("Monat"), choices = months, 
                selected = months[as.numeric(format(Sys.Date(), "%m"))])
    , selectInput("year", strong("Jahr"), choices = yrs,
                selected = max(yrs))
    , h4(htmlOutput("final"))
    , actionButton("delete_button", "Eintrag löschen", icon("trash-alt"))
  ),
  
  # Tabellen-Output
  fluidRow(DT::dataTableOutput('table'))
  
)



########### Server Function ##############################################
server <- function(input, output, session) {
  # bs_themer()
  
  
  # Tabelle mit den Eingaben erzeugen
  out_df <- reactive({
    data.frame(
      User = stringr::str_squish(input$user),
      Kostenpunkt = stringr::str_squish(input$text),
      # Betrag = stringr::str_squish(
      #   stringr::str_replace(input$cost, "\\.", ",")),
      Betrag = input$cost,
      Rabatt = ifelse(input$disc2, "ja", "nein"),
      Kommentar = stringr::str_squish(input$comm),
      Datum = as.character(input$date)
    )
  })
  
  
  # Eingabe in DB schreiben
  observeEvent(input$save, priority = 30, {
    
    out_db <- out_df()
    conn <- dbConnect(drv = RSQLite::SQLite(),
                      db_path)
    dbWriteTable(conn,
                 "ausgaben",
                 out_db,
                 append = TRUE)
    dbDisconnect(conn)
    
    out_db
  })
  
  
  # Print confirmation after save
  observeEvent(input$save, priority = 25, {
    
    output$out <- renderText(
      paste(isolate(input$text), " gespeichert."))
  })
  
  
  # Update inputs and reload DB table
  observeEvent(input$save, priority = 20, {
    
    updateSelectizeInput(session = session, inputId = "text", selected = "")
    updateNumericInput(session = session, inputId = "cost", value = "")
    updateTextInput(session = session, inputId = "comm", value = "")
    
    df_all$tbl <- get_table()
  })
  
  
  # Filtern der Tabelle auf das Monat im Input
  df_month <- reactive({
    
    m <- which(input$month == months)
    y <- as.numeric(input$year)
    
    df_month <- df_all$tbl %>%
      dplyr::filter(as.numeric(stringr::str_sub(Datum, 1, 4)) == y,
                    as.numeric(stringr::str_sub(Datum, 6, 7)) == m)
    df_month
  })
  
  
  # Abrechnung berechnen
  calc <- reactive({
    df_month <- df_month()
    b_rab <- df_month %>% 
      dplyr::filter(Rabatt == "ja", User == user1) %>% 
      dplyr::pull(Betrag) %>% 
      sum()
    m_rab <- df_month %>% 
      dplyr::filter(Rabatt == "ja", User == user2) %>% 
      dplyr::pull(Betrag) %>% 
      sum()
    m_final <- (b_rab - m_rab) * korr_factor
    
    b_wo_rab <- df_month %>% 
      dplyr::filter(Rabatt == "nein", User == user1) %>% 
      dplyr::pull(Betrag) %>% 
      sum()
    m_wo_rab <- df_month %>% 
      dplyr::filter(Rabatt == "nein", User == user2) %>% 
      dplyr::pull(Betrag) %>% 
      sum()
    m_final <- m_final + (b_wo_rab - m_wo_rab) * 0.5
    round(m_final, 2)
  })
  
  
  # Output der Abrechnung
  output$final <- renderText({
    paste0("Michi muss an Bettina <b>",
           calc(),
           "€</b> überweisen!")
  })
  
  
  # Output der Tabelle
  output$table <- DT::renderDataTable({

    m <- which(input$month == months)
    y <- as.numeric(input$year)

    df_month <- df_all$tbl %>%
      dplyr::filter(as.numeric(stringr::str_sub(Datum, 1, 4)) == y,
                    as.numeric(stringr::str_sub(Datum, 6, 7)) == m)

    DT::datatable(df_month,
                  selection = "single", 
                  rownames = FALSE,
                  options = list(paging = FALSE,
                                 columnDefs = list(
                                   list(visible = FALSE, targets = c(0,5)))

                                 )
                  )
  },
  server = FALSE)
  
  
  
  # Loeschen von Eintraegen definieren als reactive
  deleteData <- reactive({
    
    SQL_df <- df_month()
    
    row_selection <- SQL_df[input$table_rows_selected, "rowid"]
    
    conn <- dbConnect(drv = RSQLite::SQLite(),
                      db_path)
    quary <- lapply(row_selection, function(nr){
      dbSendStatement(conn,
                      sprintf('DELETE FROM "ausgaben" WHERE "rowid" == ("%s")',
                              nr))
    })
    dbDisconnect(conn)
    
  })
  
  
  # Loeschvorgang ausfuehren
  observeEvent(input$delete_button, priority = 20,{
    
    if(length(input$table_rows_selected)>=1 ){
      deleteData()
    }
    
    showModal(
      
      if(length(input$table_rows_selected) < 1 ){
        modalDialog(
          title = "Warnung!",
          paste("Bitte Zeile auswählen." ),easyClose = TRUE
        )
      })
  })
  
  
  # Tabelle neu aus DB laden nach dem Loeschen
  observeEvent(input$delete_button, priority = 10,{
    df_all$tbl <- get_table()
  })
  
}


shinyApp(ui, server)