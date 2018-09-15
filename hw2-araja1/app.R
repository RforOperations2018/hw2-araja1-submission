#
# Name: Ashok Raja
# Homework # 2
# Andrew ID: araja1
# Dataset used was the Medicaid drug reimbuiresement data for 2010, due to the volume of data, it was filtered to Pennsylvania and top 20 products per quarter and utilization type
# link to data : https://catalog.data.gov/dataset/center-for-medicare-amp-medicaid-services-cms-medicare-claims-data-79a6c

# Loading libraries

library(shiny)
library(dplyr)
library(plyr)
library(plotly)
library(tibble)


# Fetching the dataset mtcars into cars and changing the rowname to a column called "Model"
mdrp=read.csv("mdrp-pa20.csv")
mdrp.load = mdrp

# Define UI for application 
ui <- navbarPage("Cars",
                # Manin tab panel to switch between graphs and data 
                 tabPanel("Plot",
                          sidebarPanel(
                            # Selecting the product in a selection box
                            selectInput("prod_select",
                                        "Product:",
                                        choices = mdrp$Product.Name,
                                        multiple = TRUE,
                                        selectize = TRUE
                                       ),
                            #selecting the quarter using a slider
                            sliderInput("qtr_select",
                                        "QUARTER:",
                                        min = min(mdrp$Quarter, na.rm = T),
                                        max = max(mdrp$Quarter, na.rm = T),
                                        value = c(min(mdrp$Quarter, na.rm = T), max(mdrp$Quarter, na.rm = T)),
                                        step = 1
                                        ),
                            #selecting the utilization type
                            checkboxGroupInput("util_select",
                                               "Utilization:",
                                               choices = levels(mdrp$Utilization.Type),
                                               selected=1
                                        ),
                            #Bookmark button
                            bookmarkButton()
                                      ),
                          # Main panel with the 3 graphs
                          mainPanel(plotlyOutput("plot"),plotlyOutput("plot2"),plotlyOutput("plot3") )
                       ),
                 # Data Panel
                 tabPanel("Data",
                          inputPanel(
                          # Download Button
                              downloadButton("downloadData","Download Medicaid Data")
                          ),
                          fluidPage(dataTableOutput("table")) 
                 )
)

# Define server logic 
server <- function(input, output, session=session) 
{
  # Caputing the inputs for reactive functions
  swInput <- reactive({
    mdrp=mdrp.load %>%
  # Filtering the slider for quarters
        filter(Quarter >=input$qtr_select[1] & Quarter <= input$qtr_select[2])
  # Filtering the products selected
     if(length(input$prod_select)>0){
     mdrp <- subset(mdrp, Product.Name %in% input$prod_select)
   }
   #Filtering the utilization type
      if(length(input$util_select)>0){
       mdrp <- subset(mdrp, Utilization.Type %in% input$util_select)  
   }
    return(mdrp)
    })
  
  
  # Plot the amount reimbursed by quarter
  output$plot <- renderPlotly({
    mdrp=swInput()
    ggplotly(ggplot(data=mdrp,aes(x=Quarter,y=sum(Amount),colour=Utilization.Type))+
               geom_col()+
               labs(title="Total Amount Reimbursed by Year",x="Quarter",y="Amount",colour="Type")
             )
  })
  # Plot the Units and AMount
  output$plot2 <- renderPlotly({
    mdrp=swInput()
    ggplotly(ggplot(data=mdrp,aes(x=Units,y=Prescriptions,colour=Quarter,text=paste("<b>", Product.Name, ":</b> ")))+
               geom_point()+
               labs(title="Total Amount Reimbursed by Year",x="Units",y="Amount",colour="Quarter")
              ,tooltip="text"
             )
  })
  # Plot the Product and amount
  output$plot3 <- renderPlotly({
    mdrp=swInput()
    ggplotly(ggplot(data=mdrp,aes(x=Product.Name,y=sum(Amount),colour=Quarter))+
               geom_col()+
               theme(axis.text.x = element_text(angle = 60, hjust = 1,size=5)) +
               labs(title="Total Amount Reimbursed by Year",x="Product",y="Amount",colour="Quarter")
    )
  })
  # Data table 
  output$table <- renderDataTable(mdrp)
  # Download function
  output$downloadData <- downloadHandler(
    filename = function() {
    paste("download", ".csv", sep = "")
  },
  content = function(file) {
    write.csv(mdrp, file)
  }
  )   
  # Bookmark Function
  observe({
    reactiveValuesToList(input)
    session$doBookmark
  })
  onBookmarked(function(url) updateQueryString(url))
  
}


# Run the application 
shinyApp(ui = ui, server = server, enableBookmarking = "url")

