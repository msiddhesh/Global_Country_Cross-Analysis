#loading required libraries
library(shiny)
library(shinythemes)
library(shinydashboard)
library(ggplot2)
library(dplyr)
library(maps)
library(readxl)
library(gridExtra)
library(patchwork)
# Data loading
load("World_data_Preprocessed.RData")
df <- data
#View(data)
my_colums<- c("Population","GDP","Tax_Revenue","Unemployment_rate",
                "GDP_per_capita","Co2_Emissions","Gasoline_Price","CPI",
                "Birth_Rate","Infant_mortality","Life_expectancy","Fertility_Rate",
                "Maternal_mortality_ratio","Out_of_Pocket_Health_Expenditure",
                "Physicians_Per_Thousand","Density_Per_Km2","Agricultural_Land",
                "Agricultural_Land","CPI","CPI_Change","Urban_population",
                "Population_Labor_Force_Participation","Total_tax_rate","Armed_Forces_size", "Density_Per_Km2", "Agricultural_Land", "Land_Area_Km2", "Armed_Forces_size",
                "Birth_Rate", "Calling_Code", "Co2_Emissions", "CPI", "CPI_Change",
                "Fertility_Rate", "Forested_Area", "Gasoline_Price", "GDP",
                "Gross_primary_education_enrollment", "Gross_tertiary_education_enrollment",
                "Infant_mortality", "Life_expectancy", "Maternal_mortality_ratio",
                "Minimum_Wage", "Out_of_Pocket_Health_Expenditure", "Physicians_Per_Thousand",
                "Population", "Population_Labor_Force_Participation", "Tax_Revenue",
                "Total_tax_rate", "Unemployment_rate", "Urban_population",
                "Latitude", "Longitude", "GDP_per_capita")

numeric_columns <- c("Population","GDP","Tax_Revenue","Unemployment_rate",
                     "GDP_per_capita","Co2_Emissions","Gasoline_Price","CPI",
                     "Birth_Rate","Infant_mortality","Life_expectancy","Fertility_Rate",
                     "Maternal_mortality_ratio","Out_of_Pocket_Health_Expenditure",
                     "Physicians_Per_Thousand","Density_Per_Km2","Agricultural_Land",
                     "Agricultural_Land","CPI","CPI_Change","Urban_population",
                     "Population_Labor_Force_Participation","Total_tax_rate","Armed_Forces_size")
ui <- dashboardPage(
  
  skin = "blue",
  dashboardHeader(title = "World Country Data Analysis Dashboard"),
  dashboardSidebar(
    sidebarMenu(
      menuItem("Home", tabName = "home"),
      menuItem("Dataset Description", tabName = "Description"),
      menuItem("Visualization ",
               menuSubItem("Top Countries by Atributes", tabName = "di"),
               menuSubItem("Dual-Country Analysis", tabName = "1vs1"),
               menuSubItem("Individual  Atrribute", tabName = "i") ,
               menuSubItem("Dual-Attribute Analysis", tabName = "1svs1")),
      menuItem("Conclusion", tabName = "con")
      
    )
  ),
  dashboardBody( tabItems(
    tabItem(
      tabName = "home",
      tags$head(
        tags$style(
          HTML(
            "
       
        h2 {
          color: blue; /* Change to the color you prefer */
          font-size: 28px; /* Change to the desired font size */
        }
        p {
          font-size: 16px; /* Change to the desired font size */
        }
        "
          )
        )
      ),
      h2("Welcome to the World Country Data Analysis Dashboard"),
      p("This App offers an extensive analysis of global country data. The main goal of this study is to find important patterns and connections between different countries, allowing us to compare them and understand what’s happening on a global scale. It may serve as a valuable tool for researchers and analysts seeking deeper insights into global trends and international dynamics."),
      h2("Data Introduction and Source"),
      p("This extensive dataset provides a wealth of information about countries across the globe, encompassing 35 different features that are detailed in the Data Description."),
      p("This data was sourced from Kaggle. View the dataset: ",
        a("Kaggle Dataset.", href = "https://www.kaggle.com/datasets/nelgiriyewithana/countries-of-the-world-2023"),"The dataset has undergone preprocessing for analysis purposes."),
      
      # Other content for the home tab...
    )
    ,tabItem(tabName = "con",
             fluidRow(
               tags$head(
                 tags$style(
                   HTML(
                     "
          .conclusion-content {
            padding: 0 20px; /* Adjust the padding as needed */
          }
          h2 {
            color: blue;
            font-size: 28px;
          }
          p {
            font-size: 16px; 
          }
          "
                   )
                 )
               ),
               div(class = "conclusion-content",
                   h2("Conclusion:"),
                   p("In conclusion, this App offers valuable insights into the complex interplay of economic, demographic, and social factors on a global scale. It highlights the dominance of a few countries in shaping the world economy, the challenges posed by large populations, and the significance of healthcare and family planning in improving overall well-being. Researchers and analysts can leverage these findings to better understand and address global trends and dynamics."),
                   p("By identifying patterns, our goal is to enable comprehensive comparisons and foster a nuanced understanding of global dynamics. This resource is tailored for researchers and analysts seeking enriched insights into international trends and broader global dynamics.")
               )
             )
    )
    ,
                     
    
    
    # Dataset Description tab content
    tabItem(tabName = "Description",fluidRow(
      column(width = 12,
             div(
               style = "display: flex; flex-direction: row;",
               div(style = "flex: 1; padding: 25px; font-size: 16px;",  # Increase font size
                   h3(style = "color: #3366ff; font-weight: bold;font-size: 28px;", "Dataset Overview"),
                   p("This dataset contains information about various aspects of countries worldwide."),
                   p("Variables included:"),
                   tags$ul(
                     style = "list-style-type: disc; margin-left: 20px;",
                     tags$li("Country: Name of the country."),
                     tags$li("Density Per Km2: Population density measured in individuals per square kilometer."),
                     tags$li("Agricultural Land: Total land area used for agricultural purposes."),
                     tags$li("Land Area Km2: Total land area in square kilometers."),
                     tags$li("Armed Forces Size: Size of armed forces personnel."),
                     tags$li("Birth Rate: Number of live births per 1,000 individuals."),
                     tags$li("Calling Code: International calling code."),
                     tags$li("CO2 Emissions: Carbon dioxide emissions in metric tons."),
                     tags$li("CPI: Consumer Price Index."),
                     tags$li("CPI Change: Percentage change in Consumer Price Index."),
                     tags$li("Fertility Rate: Average number of children born per woman."),
                     tags$li("Forested Area: Land area covered by forests or wooded land."),
                     tags$li("Gasoline Price: Price of gasoline or petrol."),
                     tags$li("GDP: Gross Domestic Product."),
                     tags$li("Gross Primary Education Enrollment: Enrollment in primary education."),
                     tags$li("Gross Tertiary Education Enrollment: Enrollment in tertiary education."),
                     tags$li("Infant Mortality: Number of infant deaths per 1,000 live births."),
                     tags$li("Life Expectancy: Average life expectancy in years."),
                     tags$li("Maternal Mortality Ratio: Maternal deaths per 100,000 live births."),
                     tags$li("Minimum Wage: Legally mandated minimum wage."),
                     tags$li("Out of Pocket Health Expenditure: Health expenditures paid directly by individuals."),
                     tags$li("Physicians Per Thousand: Number of physicians per thousand individuals."),
                     tags$li("Population: Total population of the country."),
                     tags$li("Population Labor Force Participation: Percentage of the working-age population in the labor force."),
                     tags$li("Tax Revenue: Total revenue collected through taxation."),
                     tags$li("Total Tax Rate: Overall tax rate applied to income or profits."),
                     tags$li("Unemployment Rate: Percentage of the labor force unemployed and seeking employment."),
                     tags$li("Urban Population: Total population residing in urban areas."),
                     tags$li("Latitude: North-south geographic coordinate."),
                     tags$li("Longitude: East-west geographic coordinate."),
                     tags$li("GDP Million Dollar: Gross Domestic Product in millions of dollars."),
                     tags$li("GDP Per Capita: Gross Domestic Product per capita.")
                   )
                   # Add more descriptions of your variables here
               )
             )
      )
    ),
    fluidRow(
      column(width = 4,
             selectInput("country_Input", "Country", choices = unique(df$Country),selected = "India")
      ),
      
      tags$head(
        tags$style(
          HTML(
            "
        .table-bordered {
          border: 100 px solid red !important;
        }
        "
          )
        )
      ),
      column(width = 8,
             tableOutput("details_table")
      )
    )
    
    ),
    # Envisage Data tab content
    # Inside the tabItem for "di" (Visualization) tab, add the following content
    
    tabItem(tabName = "di",
            fluidRow(
              column(6,
                     box(
                       title = "Select Attribute",
                       width = 200,
                       selectInput("selected_attribute", "Choose Attribute", 
                                   choices = unique(numeric_columns))
                     )
              ),
              column(6,
                     box(
                       title = "Select Number of Countries",
                       width = 200,
                       sliderInput("selected_countries", "Number of Countries to Display", 
                                   min = 1, max = 100, value = 20)
                     )
              )
            ),
            fluidRow(
              column(
                width = 12,
                p(
                  style = "border-top: 1px solid #ccc; color: blue; text-align: center; font-size: 16px; font-family: Arial, sans-serif;",
                  "The bar graph represents the top selected countries based on the chosen attribute. It visualizes the relationship or distribution of the selected attribute across different countries, highlighting the top performers or values based on the criteria you've specified."
                )
              )
            ),
            fluidRow(
              box(
                title = "Top Countries Bar Graph",
                width = 12,
                plotOutput("bar_graph")
              )
            ),
            # Line t"o separate sections
            
            
            # Section for displaying the table of selected attribute values for top 20 countries
            fluidRow(
              box(
                title = "Selected Attribute Values for Selected Top Countries",
                width = 12,
                tableOutput("selected_attr_values_table")
              ))
            ),
            # You can add more sections or visualizations as needed
    
    
    # Conclusion - Seasons tab content
    tabItem(tabName = "s",
            # Content related to Seasons Conclusion
            # Visualizations, summaries, or tables can be placed here
    ),
    # Conclusion - Teams tab content
    # New tab for 1vs1 comparisons
    tabItem(
      tabName = "1vs1",
      fluidRow(
        column(
          width = 6,
          box(
            title = "Select First Country",
            selectInput("first_country", "First Country", 
                        choices = unique(df$Country),
                        selected = "India")
          )
        ),
        column(
          width = 6,
          box(
            title = "Select Second Country",
            selectInput("second_country", "Second Country", 
                        choices = unique(df$Country),
                        selected = "United States")
          )
          
        ),
        fluidRow(
          box(
            title = "Select Attributes",
            width = 12,
            fluidRow(
              column(
                width = 12,
                tags$style(HTML("
        .checkbox-columns {
          column-count: 3; /* Set the number of columns you want */
        }
        .checkbox-columns label {
          display: block;
        }
      ")),
                checkboxGroupInput("selected_attributes", "Choose Attributes", 
                                   choices = c(
                                     unique(numeric_columns)[1:10],
                                     unique(numeric_columns)[11:20],
                                     unique(numeric_columns)[21:length(unique(numeric_columns))]
                                   ),
                                   selected = "GDP"),
                class = "checkbox-columns" # Apply the class to format as columns
              )
            )
          )
          
          
          
          
        ),
        fluidRow(
          column(
            width = 12,
            p(
              style = "border-top: 1px solid #ccc; color: mediumblue; text-align: center; font-size: 16px; font-family: Arial, sans-serif;",
              "Bar graphs compare chosen countries based on selected attributes, revealing top performers and distribution trends."
            )
          )
        )
        ,
        fluidRow(
          uiOutput("plots") # Dynamic UI for plots
        )
        
      )),tabItem(
        tabName = "1svs1",
        fluidRow(
          column(
            width = 6,
            box(
              title = "Select First Attribute",
              selectInput("first_Atrribute", "First Attribute", 
                          choices = unique(numeric_columns),
                          selected = "Population")
            )
          ),
          column(
            width = 6,
            box(
              title = "Select Second Attribute",
              selectInput("second_attribute", "Second Attribute", 
                          choices = unique(numeric_columns),
                          selected = "GDP")
            )
            
          ),
          fluidRow(
            box(
              title = "Select Number Counties sorted on First Attribute",
              width = 12,
              fluidRow(
                column(
                  width = 12, # Adjust the width as needed
                  sliderInput("country_range", "Select Country Range", 
                              min = 1, max = max(nrow(df)), value = c(1, 174))
                )
              )
            )),fluidRow(
              column(
                width = 12,
                p(
                  style = "border-top: 1px solid #ccc; color: mediumblue; text-align: center; font-size: 14px; font-family: Arial, sans-serif;",
                  "This scatter plot illustrates the relationship between the first and second attributes. The countries are arranged based on the descending order of the first attribute. This visualization allows for easy examination of the correlation or association between the two attributes across the range of countries sorted by the first attribute."
                )
              )
            ), fluidRow(
              column(offset = 2, width = 6,
                     align = "center",
                     plotOutput("my_scatter_plots") # Dynamic UI for plots
              )
            )
          
        )),
    # Conclusion - Individuals tab content
    tabItem(
      tabName = "i",
      # New tab content for scatter plot
      fluidRow(
        box(
          title = "Attribute Distribution",
          width = 12,
          fluidRow(
            column(
              width = 6,
              selectInput("selected_attribute_scatter", "Choose Attribute", 
                          choices = numeric_columns, selected = "GDP")
            ),
            column(
              width = 6,
              sliderInput("country_my_range", "Select Country Range", 
                          min = 1, max = max(nrow(df)), value = c(1, 174))
            )
          ),fluidRow(
            column(
              width = 12,
              p(
                style = "border-top: 1px solid #ccc; color: mediumblue; text-align: center; font-size: 16px; font-family: Arial, sans-serif;",
                "Below the box plot distribution of your chosen attribute, you'll find the ascending range of countries based on this attribute. You can select the range what you want."
              )
            )
          ),
          plotOutput("boxplot_distribution")
        ),fluidRow(
          column(
            width = 12,
            p(
            
              "This visualization allows easy identification of outliers within the data. By observing this plot, outliers—data points that significantly differ from the majority—can be easily identified, providing insights into extreme values or anomalies within the dataset."
            )
          )
        ),
      )
    )
  )
  
  ))

server <- function(input, output) {
  
  selected_data <- reactive({
    df_sorted <- df %>% arrange(desc(!!as.name(input$selected_attribute))) 
    selected <- head(df_sorted, input$selected_countries)
    return(selected)
  })
  
  # Rendering the table of top 20 countries based on selected attribute
  output$top_20_table <- renderTable({
    top_20_countries()
  })
  
  # Creating a bar graph based on the selected attribute
  output$bar_graph <- renderPlot({
    req(input$selected_attribute, input$selected_countries) 
    
    ggplot(selected_data(), aes(x = reorder(Country, desc(!!as.name(input$selected_attribute))), 
                                y = !!as.name(input$selected_attribute))) +
      geom_bar(stat = "identity", fill = "lightslateblue", color = "white") +
      labs(x = "Country", y = input$selected_attribute, 
           title = paste("Top", input$selected_countries, "Countries by", input$selected_attribute)) +
      theme(axis.text.x = element_text(angle = 45, hjust = 1))
    
  })
  
  # Rendering the table for the selected attribute values of top 20 countries
  output$selected_attr_values_table <- renderTable({
    req(input$selected_attribute, input$selected_countries) 
    
    selected_attr <- input$selected_attribute
    selected_attr_values <- selected_data() %>%
      select(Country, !!as.name(selected_attr))
    
    colnames(selected_attr_values) <- c("Country", selected_attr)
    
    # Add row numbers to the table
    rownames(selected_attr_values) <- NULL
    
    return(selected_attr_values)
  }, rownames = TRUE)  # Set rownames argument to TRUE
  
  filtered_data <- reactive({
    filter(df, Country %in% c(input$first_country, input$second_country))
  })
  
  observe({
    req(input$selected_attributes, input$first_country, input$second_country)
    data <- filtered_data()
    
    selected_attributes <- input$selected_attributes
    
    plots <- lapply(selected_attributes, function(attr) {
      plotOutput(paste0("comparison_plot_", attr), height = 300)
    })
    
    output$plots <- renderUI({
      fluidRow(
        tags$style(".col { padding: 0px; }"),
        div(class = "row",
            lapply(plots, function(plot) {
              column(width = 4, class = "col", plot)
            })
        )
      )
    })
    
    lapply(selected_attributes, function(attr) {
      output[[paste0("comparison_plot_", attr)]] <- renderPlot({
        plot_data <- data %>%
          ggplot(aes_string(x = "Country", y = attr, fill = "Country")) +
          geom_bar(stat = "identity", position = "dodge") +
          labs(x = "Country", y = attr, 
               title = paste("Comparison of", attr, "between countries")) +
          theme_minimal() +
          scale_fill_manual(values = c( "cyan4","mediumblue","darkgoldenrod1"))  # Add your desired colors here
        
        plot_data
      })
    })
  })
  observeEvent(c(input$selected_attribute_scatter, input$country_range), {
    req(input$selected_attribute_scatter, input$country_my_range)
    
    attribute <- input$selected_attribute_scatter
    my_range <- input$country_my_range
    
    # Sort the data frame by the selected attribute
    sorted_data <- df[order(df[[input$selected_attribute_scatter]], decreasing = TRUE), ]
    
    # Subset data based on the selected range
    #data_range <- sorted_data[country_my_range[1]:country_my_range[2], ]
    
    # Update the reactive plot
    output$boxplot_distribution <- renderPlot({
      ggplot(sorted_data[input$country_my_range[1]:input$country_my_range[2], ], aes_string(x = attribute)) +
        geom_boxplot(fill = "lightslateblue", color = "lightslateblue") +  # Green color scheme
        labs(x = input$selected_attribute_scatter) +
        #coord_flip() +  # Flip to make it horizontal
        theme_minimal() +
        theme(panel.grid.major.x = element_blank(),  # Remove vertical gridlines
              axis.title.y = element_blank(),        # Remove y-axis label
              axis.text.y = element_text(size = 10))  # Modify text size for axis values
    })
    plotOutput("boxplot_distribution")
  })
  
  
  # UI code
  #plotOutput("boxplot_distribution")
  observe({
    req(input$country_range, input$first_Atrribute, input$second_attribute)
    
    # Sort the data frame by the first selected attribute
    sorted_data <- df[order(df[[input$first_Atrribute]],decreasing = TRUE), ]
    
    # Subset data based on the selected range
    data_range <- sorted_data[input$country_range[1]:input$country_range[2], ]
    
    # Create a scatter plot using the second selected attribute
    output$my_scatter_plots <- renderPlot({
      ggplot(data_range, aes_string(x = input$first_Atrribute, y = input$second_attribute)) +
      geom_point(color="slateblue") +
        scale_x_log10() +  # Change x-axis to logarithmic scale
        scale_y_log10() +  # Change y-axis to logarithmic scale
        labs(x = input$first_Atrribute, y = input$second_attribute) +
        theme_minimal()
    }, width = 800, height = 400)
    
    # UI code
    plotOutput("my_scatter_plots_container")
    
  })
  country_data <- reactive({
    req(input$country_Input)
    # Filter data for the selected country
    df[df$Country == input$country_Input, 2:36]
  })
  
  output$details_table <- renderTable({
    req(input$country_Input)
    
    # Get data for the selected country
    country <- country_data()
    
    # Create a table showing column names and corresponding values
    my_my_data <- data.frame(
      Column = names(country),
      Value = unlist(country)
    )
    my_my_data
  })
  
  
  
}




# Rest of your code remains the same...


# Run the application 
shinyApp(ui = ui, server = server)


