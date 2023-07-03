# Load required libraries
library(shiny)
library(dplyr)
library(igraph)
library(tidyr)
library(shinythemes)
library(data.table)
library(ggplot2)
library(visNetwork)
library(leaflet)

# Load the data
df <- read.table("unicorn-txt.txt", header=TRUE)
# Reading investor and startup data
load('world_data.RData')
data <- world_data

com.data <- data %>%
  group_by(Company) %>%
  summarize(investors = list(investor), year = mean(year), BValuation = mean(BValuation), 
            n_investors = mean(n_investors), avg_invest = mean(avg_invest)) %>%
  unique()

# Define the function to return the data
function_test_data <- function() {
  data
}


test_again <- function(industry, top) {
  
  # Group the data by investor and industry, and count the number of times each
  # investor-industry combination appears
  df_count <- df %>%
    group_by(investor, Industry) %>%
    summarise(count = n(), .groups = "drop") %>%
    arrange(desc(count))
  
  # View the resulting data frame
  df_count <- subset(df_count, Industry == industry)
  df_count  <- head(df_count, top)
  df_count
}


top_x <- function(x, y) {
  
  colname <- match.arg(y, c("BValuation","year","avg_invest", "n_investors"))
  
  # Order by top x startups
  df_unique <- distinct(df, Company, .keep_all = TRUE)
  startup.x <- head(df_unique[order(-df_unique[, colname]), "Company"], x)
  df_subset <- df[df$Company %in% startup.x, ] 
  unique_investors <- unique(df_subset$investor)
  
  unique_startups <- unique(df_subset$Company)
  all.startups <- data.frame(name = unique_startups, type = FALSE)
  
  unique_investors <- unique(df_subset$investor)
  all.investors <- data.frame(name = unique_investors, type = FALSE)
  
  all.vertices <- rbind(all.investors, all.startups)
  # Create graph
  edges <- df_subset[, c("Company", "investor")]
  g <- graph.data.frame(edges, directed = FALSE, vertices = all.vertices)
  
  V(g)$type <- bipartite_mapping(g)$type
  V(g)$color <- ifelse(V(g)$type, "salmon", "lightblue")
  g
}

startup_plot <- function(vv){
  company_i = vv
  new_df <- subset(data, Company == company_i)
  
  unique_company <- unique(new_df$Company)
  unique_investors <- unique(new_df$investor)
  all.industries <- data.frame(name = unique_company, type=TRUE)
  all.investors <- data.frame(name = unique_investors, type=FALSE)
  all.vertices <- rbind(all.industries, all.investors)
  
  # Create graph
  edges <- new_df[, c("Company", "investor")]
  
  g2 <- graph.data.frame(edges, directed = FALSE, vertices = all.vertices)
  V(g2)$type <- bipartite_mapping(g2)$type
  V(g2)$label <- V(g2)$name
  V(g2)$color <- ifelse(V(g2)$type, "salmon", "lightblue")
  g2
}


startup_plot2 <- function(company_i){
  # Find investors who have invested in the selected company
  dt.investor.50 <- subset(data, Company == company_i)
  #investors <- dt.investor.50[Company == selected_company, unique(investor)]
  unique_investors <- unique(dt.investor.50$investor)
  
  # Find all companies that these investors have invested in
  neighbor_companies <- subset(data, investor %in% unique_investors & Company != company_i)
  neighbor_companies <- unique(neighbor_companies$Company)
  
  # Subset the original data to include only the selected company and its neighbor companies
  #dt.selected <- data[Company %in% c(company_i, neighbor_companies)]
  dt.selected <- subset(data, Company %in% c(company_i, neighbor_companies))
  
  unique_company <- unique(dt.selected$Company)
  unique_investors <- unique(dt.selected$investor)
  all.industries <- data.frame(name = unique_company, type=TRUE)
  all.investors <- data.frame(name = unique_investors, type=FALSE)
  all.vertices <- rbind(all.industries, all.investors)
  
  # Create graph
  edges <- dt.selected[, c("Company", "investor")]
  
  g2 <- graph.data.frame(edges, directed = FALSE, vertices = all.vertices)
  V(g2)$type <- bipartite_mapping(g2)$type
  V(g2)$label <- V(g2)$name
  V(g2)$color <- ifelse(V(g2)$type, "salmon", "lightblue")
  g2
}


investor_plot <- function(cc){
  new_df <- subset(data, investor == cc)
  
  unique_company <- unique(new_df$Company)
  unique_investors <- unique(new_df$investor)
  all.industries <- data.frame(name = unique_company, type=TRUE)
  all.investors <- data.frame(name = unique_investors, type=FALSE)
  all.vertices <- rbind(all.industries, all.investors)
  
  # Create graph
  edges <- new_df[, c("Company", "investor")]
  
  g.2 <- graph.data.frame(edges, directed = FALSE, vertices = all.vertices)
  V(g.2)$type <- bipartite_mapping(g.2)$type
  V(g.2)$label <- V(g.2)$name
  V(g.2)$color <- ifelse(V(g.2)$type, "salmon", "lightblue")
  g.2
}


industry_plot <- function(cc, cx){
  if(cc == "ALL" & cx == "None"){
    new_df <- data
  } else if(cc == "ALL") {
    new_df <- data
  } else {
    new_df <- subset(data, Industry == cc | Industry == cx)
  }
  unique_industry <- unique(new_df$Industry)
  unique_investors <- unique(new_df$investor)
  all.industries <- data.frame(name = unique_industry, type=TRUE)
  all.investors <- data.frame(name = unique_investors, type=FALSE)
  all.vertices <- rbind(all.industries, all.investors)
  
  # Create graph
  edges <- new_df[, c("Industry", "investor")]
  
  g.2 <- graph.data.frame(edges, directed = FALSE, vertices = all.vertices)
  V(g.2)$type <- bipartite_mapping(g.2)$type
  V(g.2)$label <- V(g.2)$name
  V(g.2)$color <- ifelse(V(g.2)$type, "salmon", "lightblue")
  g.2
}

# Define UI -----------------------------------------------------------------------------------------------------------------------------------
ui <- navbarPage('Startups and their Investors', 
                 # First tab for general statistics
                 tabPanel('General Statistics', fluidPage(theme = shinytheme("readable")),
                          pageWithSidebar(h4(''), 
                                          sidebarPanel(width = 4, style = "margin-top: 29px;",
                                                       selectInput(inputId ='Industry', 'Choose an Industry:',c("All", unique(data$Industry))),
                                                       sliderInput("year", "Founding Years", 
                                                                   min = 2007, max = 2021, value = c(2007,2021), step = 1),
                                                       sliderInput("BValuation", "Startup Valuation (in $B)", 
                                                                   min = 1, max = 140, value = c(1, 140), step = 1),
                                                       sliderInput("num_invest", "Top Investors/Startups by Investments", 
                                                                   min = 1, max = 50, value = 10, step = 1),
                                          ),
                                          mainPanel(titlePanel('Startups and their VC Investors'),
                                                    textOutput('intro'),
                                                    br(), # adds space before the map
                                                    leafletOutput("map"),
                                                    br(), # adds space before the map
                                                    h4('Top Investors & Startups'),
                                                    textOutput('top.investors'),
                                                    br(), # adds space before the map
                                                    fluidRow(
                                                      column(width = 6, tableOutput('top.x.investors')),
                                                      column(width = 6, tableOutput('top.x.startups')),
                                                      br(), # adds space before the map
                                                      br(), # adds space before the map
                                                      h4('Founded Startups per Year'),
                                                      textOutput("gen.text"),
                                                      plotOutput(outputId = "distPlot")
                                                      
                                                    )))
                 ),
                 # Add the third page with the top X companies
                 tabPanel("Top Companies",
                          h1("The investors investing in these startups: "),
                          sliderInput("top_x", "The top startups:", min = 1, max = 100, value = 50),
                          selectInput("top_y", "Select a measure", choices = c("Valuation ($B)" = "BValuation", 
                                                                               "Most recent" = "year", 
                                                                               "Avg Investment per investor ($B)" = "avg_invest", 
                                                                               "Number of Investors" = "n_investors")),
                          selectInput("yes_no_s", "Show startup names:", choices = c( "No" = "False", "Yes" = "True")),
                          selectInput("yes_no_i", "Show investor names:", choices = c( "No" = "False", "Yes" = "True")),
                          plotOutput("my_top_x", width = "100%", height = "500px"),
                          tableOutput("centrality4")
                 ), 
                 
                 tabPanel("Startup & Investor Network",
                          tabsetPanel(
                            tabPanel("Choose a Startup",
                                     # Use a single selectInput() widget for both tabs
                                     selectInput("startup", "Select a startup", choices = unique(data$Company)),
                                     # Add a nested tabsetPanel with two tabs
                                     tabsetPanel(
                                       tabPanel("Only this company",
                                                plotOutput("start_plot"),
                                                tableOutput("centrality1")
                                       ),
                                       tabPanel("Include neighbours",
                                                plotOutput("start_plot2"),
                                                tableOutput("centrality2")
                                       )
                                     )
                            ),
                            tabPanel("Choose an Investor",
                                     selectInput("investor", "Select an investor", choices = unique(data$investor)),
                                     plotOutput("invest_plot"),
                                     tableOutput("centrality3")
                            )
                          )
                 ),
                 tabPanel("Industry Details",
                          h1("Looking for investment?"),
                          tabsetPanel(
                            tabPanel("Overall", 
                                     selectInput("industrial", "Select an industry", choices = c("ALL", unique(data$Industry))),
                                     selectInput("industrial2", "Select a second industry", choices = c("None", unique(data$Industry))),
                                     selectInput("name_indust", "Show investor names:", choices = c( "No" = "False", "Yes" = "True")),
                                     plotOutput("indust", width = "100%", height = "500px"),
                            ),
                            tabPanel("Choose an Industry",
                                     selectInput("industry", "Select an industry", choices = unique(data$Industry)),
                                     sliderInput("industry_amount", "How many investors:", min = 1, max = 10, value = 5),
                                     plotOutput("pie")
                            )
                          )
                 ),
                 tabPanel("Data",
                          numericInput("maxrows", "Rows to show", 25),
                          verbatimTextOutput("rawtable"),
                          downloadButton("downloadCsv", "Download as CSV"),tags$br(),tags$br(),
                          "Adapted from ", tags$a(href="https://www.kaggle.com/datasets/prasertk/unicorn-decacorn-hectocron-in-2021?resource=download", 
                                                  "Kaggle: Unicorn, Decacorn and Hectocorn startups in 2021.")
                 )
)


# Define Server -------------------------------------------------------------------------------------------------------------------------------
server <- function(input, output) {
  
  # TEXT intro
  output$intro <- renderText(
    "This is the ultimate hub for startup enthusiasts, investors, and innovators alike. 
    Our platform is dedicated to connecting ambitious entrepreneurs with the resources about funding they need to succeed.
    Use the sliders on the left panel to customize your search.
    Let's start by looking at investments made around the world."
  )
  
  # TEXT general statistics
  output$gen.text <- renderText(
    paste("Let's start by looking at the amount of startups that were founded in your chosen year range between ", input$year[1], " and ", input$year[2], ".")
  )
  
  
  # OUTPUT histogram startups per years general statistics
  output$distPlot <- renderPlot({
    if (input$Industry == 'All') {
      industry <- data 
    } else {
      industry <- data %>% filter(Industry == input$Industry)
    }
    x <- filter(industry, 
                year >= input$year[1], 
                year <= input$year[2],
                BValuation >= input$BValuation[1], 
                BValuation <= input$BValuation[2])$year
    bins <- seq(min(x), max(x))
    hist(x, breaks = bins, col = "#007bc2", border = "white",
         xlab = "Year",
         main = "Number of Startups Founded per Year")
  })
  
  # TEXT interesting facts
  output$top.investors <- renderText(
    paste("The following displays the top ", input$num_invest, " investors and startups by your chosen Industry. All values are in $B.")
  )
  
  # OUTPUT table top investors
  output$top.x.investors <- renderTable({
    if (input$Industry == 'All') {
      industry <- data 
    } else {
      industry <- data %>% filter(Industry == input$Industry)
    }
    selected.investors <- industry %>%
      group_by(investor) %>%
      summarize(sum_investments = sum(avg_invest)) %>%
      arrange(desc(sum_investments)) %>%
      head(input$num_invest) %>%
      select(investor, sum_investments)
    selected.investors
  })
  
  # TEXT interesting facts
  output$top.startups <- renderText(
    paste("The top", input$num_startups, " startups are the following:")
  )
  
  # OUTPUT table top investors
  output$top.x.startups <- renderTable({
    if (input$Industry == 'All') {
      startup <- data 
    } else {
      startup <- data %>% filter(Industry == input$Industry)
    }
    selected.startups <- startup %>%
      group_by(Company) %>%
      summarize(Valuation = mean(BValuation)) %>%
      arrange(desc(Valuation)) %>%
      head(input$num_invest) %>%
      select(Company, Valuation)
    selected.startups
  })
  
  # OUTPUT Map
  output$map <- renderLeaflet({
    if (input$Industry == 'All') {
      industry <- data 
    } else {
      industry <- data %>% filter(Industry == input$Industry)
    }
    x <- filter(industry, 
                year >= input$year[1], 
                year <= input$year[2],
                BValuation >= input$BValuation[1], 
                BValuation <= input$BValuation[2])
    
    map_data <- x %>%
      group_by(Country) %>%
      summarize(BValuation = sum(BValuation), latitude = mean(latitude), longitude = mean(longitude)) %>%
      select(Country, latitude, longitude, BValuation)
    
    leaflet(map_data) %>%
      addTiles() %>%
      addMarkers(
        popup = ~paste0("Country: ", Country, "<br>",
                        "Startup Valuation (in $B): ", BValuation)
      )
  })
  # Call your function to get the data
  data <- function_test_data()
  
  # Render the table output on the "Table Output" page
  output$my_table <- renderTable({
    data
  })
  
  # Render the histogram on the "Histogram" page
  output$my_histogram <- renderPlot({
    hist(data$BValuation)
  })
  
  # OUTPUT histogram startups per years general statistics
  output$distPlot <- renderPlot({
    x <- filter(com.data, 
                year >= input$year[1], 
                year <= input$year[2],
                BValuation >= input$BValuation[1], 
                BValuation <= input$BValuation[2])$year
    bins <- seq(min(x), max(x))
    hist(x, breaks = bins, col = "#007bc2", border = "white",
         xlab = "Year",
         main = "Number of Startups per Year")
  })
  
  
  # Render the top X companies on the "Top X Companies" page
  output$my_top_x <- renderPlot({
    g <- top_x(input$top_x, input$top_y)
    #plot(g, vertex.size = 6, vertex.label = NA) 
    if(input$yes_no_s == "True"){
      if(input$yes_no_i == "True"){
        V(g)$label <- V(g)$name
      } else {
        V(g)$label <- ifelse(V(g)$type, V(g)$name, "")
      }
    } else {
      if(input$yes_no_i == "True"){
        V(g)$label <- ifelse(!(V(g)$type), V(g)$name, "")
      } else {
        V(g)$label <- ""
      }
    }
    
    plot(g, vertex.size = 4, vertex.label.dist = 1, vertex.label.font = 2, 
         vertex.label.cex = 0.8, vertex.label.color = "black", 
         layout = layout_with_kk, asp = 0.5)
  })
  
  output$centrality4 <- renderTable({
    g.connect  <- top_x(input$top_x, input$top_y)
    centrality <- data.frame(Degree = mean(degree(g.connect)),
                             Closeness = mean(closeness(g.connect, normalized = TRUE)),
                             Betweenness = mean(betweenness(g.connect, normalized = TRUE)),
                             Eigenvector = mean(evcent(g.connect)$vector))
    row.names(centrality) <- c("Average centrality")
    return(centrality)
  })
  

  output$start_plot <- renderPlot({
    g <- startup_plot(input$startup)
    # Plot the graph
    V(g)$label <- V(g)$name
    plot(g, vertex.size = 4, vertex.label.dist = 1, vertex.label.font = 2, 
         vertex.label.cex = 0.8, vertex.label.color = "black", 
         layout = layout_with_kk, asp = 0.5)
  })
  
  output$centrality1 <- renderTable({
    g.connect <- startup_plot(input$startup)
    centrality <- data.frame(Node = V(g.connect)$name,
                             Degree = degree(g.connect),
                             Closeness = round(closeness(g.connect, normalized = TRUE), 4),
                             Betweenness = round(betweenness(g.connect, normalized = TRUE), 4),
                             Eigenvector = round(evcent(g.connect)$vector, 4))
  })
  
  output$start_plot2 <- renderPlot({
    g <- startup_plot2(input$startup)
    
    # Plot the graph
    V(g)$label <- V(g)$name
    plot(g, vertex.size = 4, vertex.label.dist = 1, vertex.label.font = 2, 
         vertex.label.cex = 0.8, vertex.label.color = "black", 
         layout = layout_with_kk, asp = 0.5)
  })
  
  
  
  output$centrality2 <- renderTable({
    g.connect <- startup_plot2(input$startup)
    centrality <- data.frame(Node = V(g.connect)$name,
                             Degree = degree(g.connect),
                             Closeness = round(closeness(g.connect, normalized = TRUE), 4),
                             Betweenness = round(betweenness(g.connect, normalized = TRUE), 4),
                             Eigenvector = round(evcent(g.connect)$vector, 4))
  })
  
  
  output$pie <- renderPlot({
    # Create the pie chart using ggplot2
    investor_data <- test_again(input$industry, input$industry_amount)
    # Define a pastel color palette
    my_palette <- c("#E6E6FA", "#ADD8E6", "#98FB98", "#D872AE", "#FFFACD", '#B0E0E6','#90EE90','#FFFFE0','#FFE5B4','#DCD0FF' )
    
    # Create the pie chart using ggplot2 and the pastel palette
    ggplot(investor_data, aes(x = "", y = count, fill = investor)) + 
      geom_bar(stat = "identity", width = 1) +
      coord_polar(theta = "y") +
      ggtitle("Investor Industry Pie Chart") +
      labs(x = NULL, y = NULL, fill = "Investor") +
      scale_fill_manual(values = my_palette) +  # Set the color palette
      theme_void()
  })
  
  output$invest_plot <- renderPlot({
    g.2 <- investor_plot(input$investor)
    # Plot the graph
    V(g.2)$label <- V(g.2)$name
    # Set up custom font and color for labels
    plot(g.2, vertex.size = 4, vertex.label.dist = 1, vertex.label.font = 2, 
         vertex.label.cex = 0.8, vertex.label.color = "black", 
         layout = layout_with_kk, asp = 0.5)
  })
  
  
  output$centrality3 <- renderTable({
    g.connect  <- investor_plot(input$investor)
    centrality <- data.frame(Node = V(g.connect)$name,
                             Degree = degree(g.connect),
                             Closeness = round(closeness(g.connect, normalized = TRUE), 4),
                             Betweenness = round(betweenness(g.connect, normalized = TRUE), 4),
                             Eigenvector = round(evcent(g.connect)$vector, 4))
  })
  
  
  output$indust <- renderPlot({
    g.companies <- industry_plot(input$industrial, input$industrial2)
    if(input$name_indust == "True"){
        V(g.companies)$label <- V(g.companies)$name
    } else {
        V(g.companies)$label <- ifelse(!V(g.companies)$type, V(g.companies)$name, NA)
      }
    plot(g.companies, 
         vertex.size = ifelse(!V(g.companies)$type, 4, 2),  # set size based on type
         vertex.label.dist = 1,
         vertex.label.font = 2,
         vertex.label.cex = 0.8,
         vertex.label.color = "black",
         layout = layout_with_kk, 
         asp = 0.5
    )
    
  })
  
  # output to download data
  output$downloadCsv <- downloadHandler(
    filename = function() {
      paste("Startup_data.csv", sep="")
    },
    content = function(file) {
      write.csv(data, file)
    }
  )
  
  output$rawtable <- renderPrint({
    orig <- options(width = 1000)
    print(head(data, input$maxrows), row.names = FALSE)
    options(orig)
  })
}

# Run the app
shinyApp(ui = ui, server = server)