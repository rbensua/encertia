options(rgl.useNULL=TRUE) # for MAC rgl incompatibility
# Loading libraries ------
library(plyr)
library(dplyr)
library(shiny)
library(shinydashboard)
library(shinyWidgets)
library(deaR)
library(plotly)
library(DT)
options(encoding = "UTF-8")

# Loading extra code ----
source("global.R")

# UI function ------------------
ui <- dashboardPage(title = "Plataforma Encertia",
  dashboardHeader(
    title = tags$a(tags$img(src = "encertia_blanco_transparente_v1.png",
                            align = "center", width='150')),
    titleWidth = 250
  ),
  dashboardSidebar(width = 250,
    sidebarMenu(
      menuItem(" Introduction",
               tabName = "intro",
               icon = icon("circle-info")
      ),
      
      menuItem("Efficiency for business",
               tabName = "eficiencia",
               icon = icon("gears", verify_fa = FALSE)
      )
    )
  ),
  dashboardBody(
    tags$head( # change the font size to 17
      tags$style(HTML(".main-sidebar { font-size: 17px; }"))
    ),
  
    tabItems(
      ## Introduccion ---------
      tabItem(
        tabName = "intro",
        fluidRow(
          tags$iframe(src = './Descripcion_encertia3.html', 
                      width = '100%', height = '800px', 
                      frameborder = 0, scrolling = 'auto'
          )
        )
      ),
      ## DEA tab content ----------
      tabItem(
        tabName = "eficiencia",
        fluidRow(
          box(width = 12, 
              column(width = 4,
                     radioButtons("dataselection", 
                                  label = h3("Select your data"),
                                  choices = c("Default data" = 1, 
                                              "Custom data" = 2), 
                                  selected = 1)
              ),
              column(width = 4, 
                     uiOutput("upload_dataUI")
              ),
              column(width = 4, 
                     htmlOutput("file"), br(),
                     actionButton('reset', 'Reset Data')
              )
              ),
          box(width = 12,
              column(width = 4, 
                     uiOutput("empresaUI"),
                     uiOutput("excluir")
              ),
              column(width = 4,
                     htmlOutput("nempleados")
              ),
              column(width = 4, 
                     uiOutput("filtro_empleados_ui"),
                     htmlOutput("nempresas")
              )),
          box(width = 12, 
              column(width = 6,
                     selectInput(
                       "rts",
                       label = "Returns to scale",
                       choices = list(
                         "Constant" = "crs",
                         "Variable (greater probability of being efficient)" = "vrs"
                       ),
                       selected = "crs"
                     ),
                     selectInput("secondstage",
                                 label = "Slacks (orange zone)",
                                 choices = list(
                                   "Arbitrary" = "maxslack_no",
                                   "Max-slack" = "maxslack",
                                   "Min-slack (MILP) (slow)" = "milp",
                                   "Min-slack (MF) (slow)" = "mf"
                                 ),
                                 selected = "maxslack_no"
                     )
              ),
              column(width =3, 
                     actionButton("compute_target", 
                                  tags$span("Compute!", 
                                            style = "font-size: 20px; color: white; font-weight: bold;"),
                                  width = "100%",
                                  class = "btn-primary btn-lg")
                     
              ),
              column(width = 3,
                     htmlOutput("beta_value")
              )
          )
        ),
        uiOutput("deaUI")#,
     
      )     
    )
  )
)






# Define server logic ----
server <- function(input, output, session) {
  

  ## Define reactive values ------
  n_empresas_ref <- reactiveVal(value = NULL)
  info_dea_list <- reactiveVal(value = NULL)
  current_data <- reactiveVal(value = NULL)
  upload_state <- reactiveVal(value = NULL)
  is_finished <- reactiveVal(FALSE)
  
  beta_value <- reactiveVal(NULL)
  values <- reactiveValues(
    upload_state = NULL
  )
  
  # Set of reactives values to listen....
  toListen <- reactive({
    list(input$dataselection,input$dataupload, values$upload_state)
  })
  
  ## Reset function ----
  reset_dash <- function(){
    values$upload_state <- 'reset'
    current_data(NULL)
    is_finished(FALSE)
    
    output$upload_dataUI <- renderUI({
      if(input$dataselection == 2){
        fileInput("dataupload",label = "Select your file",
                  multiple = FALSE,
                  accept = c("text/csv",
                             "text/comma-separated-values,text/plain",
                             ".csv"))
      }else{
        NULL
      }
    })
    
    output$input1_gauge <- NULL
    output$input1_target <- NULL
    
    output$input2_gauge <- NULL
    output$input2_target <- NULL
    
    output$input3_gauge <- NULL
    output$input3_target <- NULL
    
    output$output_gauge <- NULL
    output$output_target <- NULL
    
    output$plot_ref <- NULL
    
    output$info_ref <- NULL
    output$plot_ref <- NULL
    output$beta_value <- NULL
    
    
    
    
  }
  
  
  
  ## Triger dataupload ----
  observeEvent(input$dataupload, {
    values$upload_state <- 'uploaded'
    is_finished(FALSE)
  })
  
  ## Reset button ----
  observeEvent(input$reset, {
    req(input$dataselection == 2)
    reset_dash()
  })

  

  ## File input state reactive value ----
 
  
  file_input <- reactive({
    if (is.null(values$upload_state)) {
      return(NULL)
    } else if (values$upload_state == 'uploaded') {
      return(input$dataupload)
    } else if (values$upload_state == 'reset') {
      return(NULL)
    }
  })
  
  

  ## Reading data (either default or uploaded file) -----
  observeEvent(toListen(),{
    if(is_finished()){
      reset_dash()
    }
    if(input$dataselection == 1){ # Default data
       df <- data_df
       upload_state(NULL)
       output$file <- renderInfoBox({
         infoBox("File","default", icon = icon("list"))
         })
    }else { # Custom data
      current_data(NULL)
      inFile <- file_input()
      if(!is.null(inFile)){
        df <-  tryCatch(read.csv(inFile$datapath, sep = " "),
                        error=function(e) {
                          message('An Error Occurred')
                          print(e)
                        })
        if(is.data.frame(df)){
        upload_state(1)
        output$file <- renderInfoBox({
          infoBox("File",inFile$name, icon = icon("list"))
        })
        }else{
          mess <- df$message
          showModal(modalDialog(title = mess,
                                footer = modalButton("Aceptar")))
          df <- NULL
        }
      }else {
        upload_state(NULL)
        output$file <- renderInfoBox({
          infoBox("File", "none", icon = icon("list"))
        })
        df <- NULL
      }
    }
    current_data(df)
  })
  
  ## Renders upload file UI if user selects "custom data" ----
  output$upload_dataUI <- renderUI({
    if(input$dataselection == 2){
      fileInput("dataupload",label = "Select your file", 
                multiple = FALSE,
                accept = c("text/csv",
                           "text/comma-separated-values,text/plain",
                           ".csv"))
    }else{
      NULL
    }
  })
  
  ## Render Company selection company UI ----
  
  observeEvent(input$empresa, {
    req(is_finished())
    
    is_finished(FALSE)
    
    output$input1_gauge <- NULL
    output$input1_target <- NULL
    
    output$input2_gauge <- NULL
    output$input2_target <- NULL
    
    output$input3_gauge <- NULL
    output$input3_target <- NULL
    
    output$output_gauge <- NULL
    output$output_target <- NULL
    
    output$plot_ref <- NULL
    
    output$info_ref <- NULL
    output$plot_ref <- NULL
    output$beta_value <- NULL
    
    
  })
  
  
  output$empresaUI <- renderUI({
    pickerInput("empresa", 
                label = "Company:", 
                choices = current_data()$DMU,       
                multiple = FALSE,
                options = list(
                  `actions-box` = FALSE,
                  `deselect-all-text` = "Deselect All",
                  `select-all-text` = "Select All",
                  `none-selected-text` = "None Selected",
                  `live-search` = TRUE,
                  `live-search-normalize` = TRUE
                )
    )
  })
  
  # Checking and Fixing the data ----
  observeEvent(toListen(),{
    df <- current_data()
    if(input$dataselection == 2){
      if(!is.null(df)){
        pass <- check_data(df)
        mess <- NULL
        if(pass == 0){
          mess <- "Wrong number of columns!\n Should be 5 or 6"
          df <- NULL
          showModal(modalDialog(HTML(mess), 
                                title = "Warning!",
                                footer = modalButton("Aceptar")))  
          
        }else if(pass == -1){
          mess <- "Wrong data types!\n Should be numeric"
          df <- NULL
          showModal(modalDialog(HTML(mess), 
                                title = "Warning!",
                                footer = modalButton("Aceptar")))  
          
        }else if(pass == -2){
          mess <- "Warning: DMU names changed to alphanumeric"
          df[,1] <- paste0("DMU",df[,1])
          showModal(modalDialog(HTML(mess),
                                title = "Warning!",
                                footer = modalButton("Aceptar")))
          
        }
        if(!is.null(df)){
          ncols <- ncol(df)
          colnames(df)[1:5] <- c("DMU", "Input1", "Input2", "Input3", "Output")
          if (ncols == 5){
            mess <- paste0("Number of employees assumed 1 for all DMUs.<br>",mess)
            
            df$Nemployees <- 1
            showModal(modalDialog(HTML(mess), 
                                  title = "Warning!",
                                  footer = modalButton("Aceptar")))  
          }else{
            colnames(df)[6] <- "Nemployees"          
          }
        }
        current_data(df)
        
        
      }else{
        current_data(NULL)}
    }  
  }
  )
  
  # DEA Logic ----------------------

  ## Rendering  N. Employees infobox -----
  
  output$nempleados <- renderInfoBox({
    if(!is.null(current_data())){
      df <- current_data()
      empresa_real <-  input$empresa
      nemp <- df$Nemployees[df$DMU == empresa_real]
      valor <- tags$span(as.character(nemp), 
                         style = "font-size: 40px; font-weight: bold;")
      
    }else{
      valor <- tags$span(as.character(0), 
                         style = "font-size: 40px; font-weight: bold;")
    }
    infoBox("N. Employees of Selected Company",
            valor , 
            icon = icon("list"))
  })
  
  
  ## Render exlude companies UI -----
  
  output$excluir <- renderUI({
    if(!is.null(current_data())){
      
      empresa_real <-  input$empresa
      dmu_ref <- 1:nrow(current_data()) 
      dmu_eval <- which(current_data()$DMU == empresa_real)#
      
      pickerInput(
        inputId = "blacklist",
        label = "Exclude",
        multiple = TRUE,
        options = list(
          `actions-box` = TRUE,
          `deselect-all-text` = "Deselect All",
          `select-all-text` = "Select All",
          `none-selected-text` = "None Selected",
          `live-search` = TRUE,
          `live-search-normalize` = TRUE
        ),
        choices = sort(current_data()$DMU[dmu_ref[-which(dmu_ref == dmu_eval)]])
      )
    }else{
        NULL
      }
  })


  ## Render n. Employees filtering -----
  output$filtro_empleados_ui <- renderUI({
      req(current_data())
      minNempleados <- min(current_data()$Nemployees, na.rm = TRUE)
      maxNempleados <- max(current_data()$Nemployees, na.rm = TRUE)
      tags$div(
        title = "Filter employees",
        sliderInput("filtroNempleados",
                    "N. Employees",
                    min = minNempleados,
                    max = maxNempleados,
                    value = c(minNempleados, maxNempleados)
        )
      )
    
  })
  
  output$nempresas <- renderText({
    
    Nempleados <- input$filtroNempleados

    dmu_ref_orig <- which(current_data()$Nemployees >= Nempleados[1] &
                            current_data()$Nemployees <= Nempleados[2] )
    if(!is.null(input$blacklist)){
      dmu_ref <- dmu_ref_orig[-which(current_data()$DMU %in% input$blacklist)]
    }else{
            dmu_ref <- dmu_ref_orig
          }

    n_empresas_ref(length(dmu_ref))
    str <- paste0(
      "<p style=\"font-size:20px\">N. companies:  <font color=\"#0000FF\"><b>",
      n_empresas_ref())
    return(str)
  })
  
  ## Values Inputs / Output --------------
  output$deaUI <- renderUI({
   
      fluidRow(
        column(
          width = 12,
          box(
            title = "Input: Total assets",
            width = 3,
            htmlOutput("input1_value"), 
            htmlOutput("input1_target"), 
            br(),
            plotlyOutput("input1_gauge", height = 260),
            sliderInput("input1_dir",
                        label = "Relative ease of change",
                        min = 0, max = 1, value = 1, step = 0.05
            )
          ),
          box(
            title = "Input: Staff costs", width = 3,
            htmlOutput("input2_value"),
            htmlOutput("input2_target"), 
            br(),
            plotlyOutput("input2_gauge", height = 260),
            sliderInput("input2_dir",
                        label = "Relative ease of change",
                        min = 0, max = 1, value = 1, step = 0.05
            )
          ),
          box(
            title = "Input: Other costs", width = 3,
            htmlOutput("input3_value"),
            htmlOutput("input3_target"), 
            br(),
            plotlyOutput("input3_gauge", height = 260),
            sliderInput("input3_dir",
                        label = "Relative ease of change",
                        min = 0, max = 1, value = 1, step = 0.05
            )
          ),
          box(
            title = "Output: Operating revenues", width = 3,
            htmlOutput("output_value"),
            htmlOutput("output_target"),
            br(),
            plotlyOutput("output_gauge", height = 260),
            sliderInput("output_dir",
                        label = "Relative ease of change",
                        min = 0, max = 1, value = 1, step = 0.05
            )
          )
        ),
    
        box(
          title = "Companies in the reference set", width = 12,
          dataTableOutput("info_ref"),br(),
          plotlyOutput("plot_ref"),
        )
      )
      
    
  })
  
  
  output$input1_value <- renderText({
    req(input$empresa)

      if(!is.null(current_data())){
        empresa_real <- input$empresa
        dmu_eval <- which(current_data()$DMU == empresa_real)
        valor <- current_data()[dmu_eval,2]
        omag <- floor(log10(valor))
        if(length(omag)){
          if (omag < 3) {
            multipl <- 1
            oid <- NULL
          } else if (omag < 6) {
            multipl <- 1e-3
            oid <- "K"
          } else {
            multipl <- 1e-6
            oid <- "M"
          }
          valor <- round(valor * multipl, 2)
          valor_str <- paste0(
            "<p style=\"font-size:20px\">Original value:  <font color=\"#0000FF\"><b>",
            valor, oid, "€</b></font></p>"
          )
          return(valor_str)
        }
      }else{
        NULL
      }
    
  })

  output$input2_value <- renderText({
    req(input$empresa)
    
    if(!is.null(current_data())){
    empresa_real <- input$empresa
    dmu_eval <- which(current_data()$DMU == empresa_real)
    valor <- current_data()[dmu_eval,3]
      omag <- floor(log10(valor))
      if(length(omag)){
        if (omag < 3) {
          multipl <- 1
          oid <- NULL
        } else if (omag < 6) {
          multipl <- 1e-3
          oid <- "K"
        } else {
          multipl <- 1e-6
          oid <- "M"
        }
        valor <- round(valor * multipl, 2)
        valor_str <- paste0(
          "<p style=\"font-size:20px\">Original value:  <font color=\"#0000FF\"><b>",
          valor, oid, "€</b></font></p>"
        )
        return(valor_str)
      }
    }else{
      NULL
    }
  })
  
  
  output$input3_value <- renderText({
    req(input$empresa)
    
    if(!is.null(current_data())){
    empresa_real <- input$empresa
    dmu_eval <- which(current_data()$DMU == empresa_real)
    valor <- current_data()[dmu_eval,4]
      omag <- floor(log10(valor))
      if(length(omag)){
        
        if (omag < 3) {
          multipl <- 1
          oid <- NULL
        } else if (omag < 6) {
          multipl <- 1e-3
          oid <- "K"
        } else {
          multipl <- 1e-6
          oid <- "M"
        }
        valor <- round(valor * multipl, 2)
        valor_str <- paste0(
          "<p style=\"font-size:20px\">Original value:  <font color=\"#0000FF\"><b>",
          valor, oid, "€</b></font></p>"
        )
        return(valor_str)
      }
    }else{
      NULL
    }
  })
  output$output_value <- renderText({
    req(input$empresa)
    
    if(!is.null(current_data())){
      empresa_real <- input$empresa
      dmu_eval <- which(current_data()$DMU == empresa_real)
      valor <- current_data()[dmu_eval,5]
      omag <- floor(log10(valor))
      if(length(omag)){
        
        if (omag < 3) {
          multipl <- 1
          oid <- NULL
        } else if (omag < 6) {
          multipl <- 1e-3
          oid <- "K"
        } else {
          multipl <- 1e-6
          oid <- "M"
        }
        valor <- round(valor * multipl, 2)
        valor_str <- paste0(
          "<p style=\"font-size:20px\">Original value:  <font color=\"#0000FF\"><b>",
          valor, oid, "€</b></font></p>"
        )
        return(valor_str)
      }
    }else{
      NULL
    }
  })
  ## DEA  computation ----
  
  observeEvent(input$compute_target, {
    if(!is.null(current_data())){
      directions <- c(
        input$input1_dir,
        input$input2_dir,
        input$input3_dir,
        input$output_dir
      )
      if(max(directions) == 0){
        showModal(modalDialog(title = "¡No pueden ser todas 0!",
                              footer = modalButton("Aceptar")))
      }else{
 
        info <- recoger_info(
          empresa = input$empresa,
          Nempleados = input$filtroNempleados,
          rts = input$rts,
          df = current_data(),
          secondstage = input$secondstage,
          directions = c(
            input$input1_dir,
            input$input2_dir,
            input$input3_dir,
            input$output_dir
          ),
          blacklist = input$blacklist,
          model = "linear"
        )
        showModal(modalDialog(title = "Done!",
                              easyClose = TRUE,
                              footer = modalButton("Aceptar")#NULL
        ))
        info_dea_list(info) # Updates reactive value info_dea_list
        
        ## Filling the results ----
        
        ### Input 1 ----
        
        output$input1_target <- renderText({
          i <- 1
          if(!is.null(current_data())){
            empresa_real <- input$empresa
            dmu_eval <- which(current_data()$DMU == empresa_real)
            valor <- info$proj_io[i]
            omag <- floor(log10(valor))
            if (omag < 3) {
              multipl <- 1
              oid <- NULL
            } else if (omag < 6) {
              multipl <- 1e-3
              oid <- "K"
            } else {
              multipl <- 1e-6
              oid <- "M"
            }
            valor <- round(valor * multipl, 2)
            valor_str <- paste0(
              "<p style=\"font-size:20px\">Target:  <font color=\"#FF0000\"><b>",
              valor, oid, "€</b></font></p>"
            )
            return(valor_str)
          }else{
            NULL
          }
          
        })
        
        
        output$input1_gauge <- renderPlotly({

          i <- 1
          fig <- plot_ly(
            domain = list(x = c(0, 1), y = c(0, 1)),
            value = info$targ_io[i],
            title = list(text = "Efficient Projection"),
            type = "indicator",
            mode = "gauge+number+delta",
            delta = list(
              reference = info$original_io[i],
              increasing = list(color = "red")
            ),
            gauge = list(
              axis = list(range = list(NULL, info$gmax[i])),
              bar = list(thickness = 0),
              steps = list(
                list(range = c(0, info$gmin[i]), color = "darkblue"),
                list(range = c(info$gmin[i], info$proj_io[i]), color = "orange"),
                list(range = c(info$proj_io[i], info$gmax[i]), color = "red")
              )
            )
          ) %>% layout(
            margin = list(l = 20, r = 30),
            paper_bgcolor = "white",
            font = list(color = "black", family = "Arial")
          )
          
        })
        
        ### Input 2 ----
        
        output$input2_target <- renderText({
          i <- 2
          if(!is.null(current_data())){
            empresa_real <- input$empresa
            dmu_eval <- which(current_data()$DMU == empresa_real)
            valor <- info$proj_io[i]
            omag <- floor(log10(valor))
            if (omag < 3) {
              multipl <- 1
              oid <- NULL
            } else if (omag < 6) {
              multipl <- 1e-3
              oid <- "K"
            } else {
              multipl <- 1e-6
              oid <- "M"
            }
            valor <- round(valor * multipl, 2)
            valor_str <- paste0(
              "<p style=\"font-size:20px\">Target:  <font color=\"#FF0000\"><b>",
              valor, oid, "€</b></font></p>"
            )
            return(valor_str)
          }else{
            NULL
          }
          
        })
        
        
        output$input2_gauge <- renderPlotly({

          i <- 2
          fig <- plot_ly(
            domain = list(x = c(0, 1), y = c(0, 1)),
            value = info$targ_io[i],
            title = list(text = "Efficient Projection"),
            type = "indicator",
            mode = "gauge+number+delta",
            delta = list(
              reference = info$original_io[i],
              increasing = list(color = "red")
            ),
            gauge = list(
              axis = list(range = list(NULL, info$gmax[i])),
              bar = list(thickness = 0),
              steps = list(
                list(range = c(0, info$gmin[i]), color = "darkblue"),
                list(range = c(info$gmin[i], info$proj_io[i]), color = "orange"),
                list(range = c(info$proj_io[i], info$gmax[i]), color = "red")
              )
            )
          ) %>% layout(
            margin = list(l = 20, r = 30),
            paper_bgcolor = "white",
            font = list(color = "black", family = "Arial")
          )
          
         
        })
        
        ### Input 3 ----
        
        output$input3_target <- renderText({
          i <- 3
          if(!is.null(current_data())){
            empresa_real <- input$empresa
            dmu_eval <- which(current_data()$DMU == empresa_real)
            valor <- info$proj_io[i]
            omag <- floor(log10(valor))
            if (omag < 3) {
              multipl <- 1
              oid <- NULL
            } else if (omag < 6) {
              multipl <- 1e-3
              oid <- "K"
            } else {
              multipl <- 1e-6
              oid <- "M"
            }
            valor <- round(valor * multipl, 2)
            valor_str <- paste0(
              "<p style=\"font-size:20px\">Target:  <font color=\"#FF0000\"><b>",
              valor, oid, "€</b></font></p>"
            )
            return(valor_str)
          }else{
            NULL
          }
          
        })
        
        output$input3_gauge <- renderPlotly({
          
          
          i <- 3
          fig <- plot_ly(
            domain = list(x = c(0, 1), y = c(0, 1)),
            value = info$targ_io[i],
            title = list(text = "Efficient Projection"),
            type = "indicator",
            mode = "gauge+number+delta",
            delta = list(
              reference = info$original_io[i],
              increasing = list(color = "red")
            ),
            gauge = list(
              axis = list(range = list(NULL, info$gmax[i])),
              bar = list(thickness = 0),
              steps = list(
                list(range = c(0, info$gmin[i]), color = "darkblue"),
                list(range = c(info$gmin[i], info$proj_io[i]), color = "orange"),
                list(range = c(info$proj_io[i], info$gmax[i]), color = "red")
              )
            )
          ) %>% layout(
            margin = list(l = 20, r = 30),
            paper_bgcolor = "white",
            font = list(color = "black", family = "Arial")
          )
          
          
        })
        
        ### Output ----
        
        output$output_target <- renderText({
          i <- 4
          if(!is.null(current_data())){
            empresa_real <- input$empresa
            dmu_eval <- which(current_data()$DMU == empresa_real)
            valor <- info$proj_io[i]
            omag <- floor(log10(valor))
            if (omag < 3) {
              multipl <- 1
              oid <- NULL
            } else if (omag < 6) {
              multipl <- 1e-3
              oid <- "K"
            } else {
              multipl <- 1e-6
              oid <- "M"
            }
            valor <- round(valor * multipl, 2)
            valor_str <- paste0(
              "<p style=\"font-size:20px\">Target:  <font color=\"#FF0000\"><b>",
              valor, oid, "€</b></font></p>"
            )
            return(valor_str)
          }else{
            NULL
          }
          
        })
        
        output$output_gauge <- renderPlotly({
          
          
          i <- 4
          fig <- plot_ly(
            domain = list(x = c(0, 1), y = c(0, 1)),
            value = info$targ_io[i],
            title = list(text = "Efficient Projection"),
            type = "indicator",
            mode = "gauge+number+delta",
            delta = list(
              reference = info$original_io[i],
              increasing = list(color = "red")
            ),
            gauge = list(
              axis = list(range = list(NULL, info$gmax[i])),
              bar = list(thickness = 0),
              steps = list(
                list(range = c(0, info$gmin[i]), color = "darkblue"),
                list(range = c(info$gmin[i], info$proj_io[i]), color = "red"),
                list(range = c(info$proj_io[i], info$gmax[i]), color = "orange")
              )
            )
          ) %>% layout(
            margin = list(l = 20, r = 30),
            paper_bgcolor = "white",
            font = list(color = "black", family = "Arial")
          )
          
         
        })
   
        ### Inefficiency value (beta) ----
        
        output$beta_value <- renderInfoBox({
          req(current_data())
          valor <- beta_value(info$beta)
          valor <- ifelse(!is.null(current_data()),
                          beta_value(),
                          NULL)
          valor <- tags$span(as.character(round(valor, digits = 2)), 
                             style = "font-size: 40px; 
                             font-weight: bold;")
           infoBox("Ineff. Score", valor, # icon = icon("euro-sign"),
                    color = "red")
        })
        
        
        
        ## Plot ----
        
        output$plot_ref <- renderPlotly({
          if(!is.null(current_data())){
            lmb <- info$lmb
            lmbnorm <- lmb / sum(lmb)
            refprc <- 100 * signif(sort(lmbnorm, decreasing = FALSE), 4)
            refprc <- refprc[which(refprc != 0)]
            
            df <- data.frame(
              group = names(refprc),
              count = refprc
            )
            refplot <- ggplot(df, aes(x = group, y = count, fill = 4)) +
              geom_col() +
              theme_bw() +
              coord_flip() +
              scale_x_discrete(limits = df$group, drop = FALSE) +
              xlab("") +
              ylab("Percentage") +
              ylim(c(0, 100)) + ggtitle("Importance as a reference (%)") + 
              theme(legend.position = "none")
            ggplotly(refplot)
          }else{
            NULL
          }
        })
        
        ## Table Info ----
        
        output$info_ref <- renderDataTable({
          if(!is.null(current_data())){
            lmb <- info$lmb
            lmbnorm <- lmb / sum(lmb)
            refprc <- 100 * signif(sort(lmbnorm, decreasing = FALSE), 4)
            idx_no_zero <- which(refprc != 0)
            names_idx <- names(lmb)[idx_no_zero]
            refprc <- refprc[idx_no_zero]
            inputs <- current_data()[current_data()$DMU %in% names_idx,2:4] 
            outputs <- current_data()[current_data()$DMU %in% names_idx,5] 
            df <- data.frame(
              group = names(refprc),
              count = refprc
            )
            df <- tryCatch(cbind(df, inputs, outputs), 
                           error = function(e){
                             msg <- message("You need to recompute the targets!")
                             return(msg)
                           } )
            names(df) <- c("Empresa",
                           "count",
                           "Activo total",
                           "Gastos de personal",
                           "Otros gastos",
                           "Ingresos de explotación")
            
            omagTA <- floor(log10(df$`Activo total`))
            omagGP <- floor(log10(df$`Gastos de personal`))
            omagRG <- floor(log10(df$`Otros gastos`))
            omagIE <- floor(log10(df$`Ingresos de explotación`))
            
            TA <- lapply(omagTA, function(x) {
              if (x < 3) {
                res <- data.frame(multipl = 1, oid = "")
              } else if (x < 6) {
                res <- data.frame(multipl = 1e-3, oid = "K")
              } else {
                res <- data.frame(multipl = 1e-6, oid = "M")
              }
              return(res)
            })
            TA <- do.call(rbind, TA)
            
            GP <- lapply(omagGP, function(x) {
              if (x < 3) {
                res <- data.frame(multipl = 1, oid = "")
              } else if (x < 6) {
                res <- data.frame(multipl = 1e-3, oid = "K")
              } else {
                res <- data.frame(multipl = 1e-6, oid = "M")
              }
              return(res)
            })
            GP <- do.call(rbind, GP)
            
            RG <- lapply(omagRG, function(x) {
              if (x < 3) {
                res <- data.frame(multipl = 1, oid = "")
              } else if (x < 6) {
                res <- data.frame(multipl = 1e-3, oid = "K")
              } else {
                res <- data.frame(multipl = 1e-6, oid = "M")
              }
              return(res)
            })
            RG <- do.call(rbind, RG)
            
            IE <- lapply(omagIE, function(x) {
              if (x < 3) {
                res <- data.frame(multipl = 1, oid = "")
              } else if (x < 6) {
                res <- data.frame(multipl = 1e-3, oid = "K")
              } else {
                res <- data.frame(multipl = 1e-6, oid = "M")
              }
              return(res)
            })
            IE <- do.call(rbind, IE)
            
            valorTA <- round(df[, "Activo total"] * TA$multipl, 2)
            valorGP <- round(df[, "Gastos de personal"] * GP$multipl, 2)
            valorRG <- round(df[, "Otros gastos"] * RG$multipl, 2)
            valorIE <- round(df[, "Ingresos de explotación"] * IE$multipl, 2)
            
            tabla_ref <- data.frame(NULL)
            
            colnames(current_data())
            
            if("Nemployees" %in% colnames(current_data())){
              for(i in 1:nrow(df)){
                tabla_ref <- rbind(tabla_ref, 
                                   data.frame(
                                     Company = df[i, "Empresa"],
                                     N.employees = current_data()$Nemployees[current_data()$DMU == df[i, "Empresa"]],
                                     Total.assets= paste0(valorTA[i], TA$oid[i], "€"),
                                     Staff.costs = paste0(valorGP[i], GP$oid[i], "€"),
                                     Other.costs= paste0(valorRG[i], RG$oid[i], "€"),
                                     Operating.revenues= paste0(valorIE[i], IE$oid[i], "€")
                                     
                                   ))
              }
            }else{
              for(i in 1:nrow(df)){
                tabla_ref <- rbind(tabla_ref, 
                                   data.frame(
                                     Company = df[i, "Empresa"],
                                    # N.employees = current_data()$Nemployees[current_data()$DMU == df[i, "Empresa"]],
                                     Total.assets= paste0(valorTA[i], TA$oid[i], "€"),
                                     Staff.costs = paste0(valorGP[i], GP$oid[i], "€"),
                                     Other.costs= paste0(valorRG[i], RG$oid[i], "€"),
                                     Operating.revenues= paste0(valorIE[i], IE$oid[i], "€")
                                     
                                   ))
              }
            }
           
            return(datatable(tabla_ref))
          }else{
            NULL
          }
          
        })
        
      }
      
      is_finished(TRUE) # Updates reactive value is_finished
    }
  })

}