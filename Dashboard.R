library(tidyverse)
library(plotly)
library(mosaic)
library(tidyr)
library(httr)
library(httr2)
library(jsonlite)
library(readxl)

options(rsconnect.max.bundle.size = 3000000000000)

#Importing GDP Per Capita dataset 
path<-"http://api.worldbank.org/v2/country/indicator/NY.GDP.PCAP.CD?format=json&per_page=20000"
GDP_Per_Capita<-GET(url=path)
GDP_Per_Capita <- content(GDP_Per_Capita, as="text", encoding = "UTF-8")
GDP_Per_Capita <- fromJSON(GDP_Per_Capita,flatten=TRUE)
GDP_Per_Capita <- GDP_Per_Capita[[2]]
GDP_Per_Capita$date<-as.numeric(GDP_Per_Capita$date)

#Choosing only year 2019 
GDP_Per_Capita <- subset (GDP_Per_Capita, date == "2019")

GDP_Per_Capita <- GDP_Per_Capita %>%
  mutate(`value` = as.numeric(`value`),  # Convert column to numeric
         `value` = round(`value`))      # Round the values

colnames(GDP_Per_Capita)[colnames(GDP_Per_Capita) == "country.value"] <- "Country Name"
colnames(GDP_Per_Capita)[colnames(GDP_Per_Capita) == "countryiso3code"] <- "Country Code" 
colnames(GDP_Per_Capita)[colnames(GDP_Per_Capita) == "value"] <- "GDP Per Capita ($)"

GDP_Per_Capita <- GDP_Per_Capita %>%
  select("Country Name", "Country Code", "GDP Per Capita ($)")

#Inserting data to gain access to information about regions 
req <- request("http://api.worldbank.org/v2/country?format=json&per_page=310")
req
req_dry_run(req)

response <- req_perform(req)
response

resp_status(response)
resp_status_desc(response)

result <- resp_body_json(response)

result <- result[[2]]

CountryCode <- unlist(lapply(result, `[[`, "id"))
CountryName <- unlist(lapply(result, `[[`, "name"))

Region <- lapply(result, `[[`, "region")
Region <- unlist(lapply(Region, `[[`, "value"))


CountryDF <- tibble(CountryName, CountryCode, Region)

#Removing aggregate regions 
CountryDF <- CountryDF %>%
  filter(Region != "Aggregates")

colnames(CountryDF)[colnames(CountryDF) == "CountryCode"] <- "Country Code"

#Adding Regions column to GDP_Per_Capita dataset 
GDP_Per_Capita <- merge(GDP_Per_Capita, CountryDF, by = "Country Code")

GDP <- GDP_Per_Capita %>% 
  select("Country Name", "Country Code", "GDP Per Capita ($)", "Region")

#Finding which columns include NAs
colSums(is.na(GDP))
#There are 48 NAs in Region column, this is because in country Name column 
#There are some groups that are not countries but are grouped by certain characteristic 
#For example "Euro area". I'm going to remove all that rows from my data. 

#Seeing the rows which have NAs
GDP[!complete.cases(GDP), ]

#Removing NAs from test dataset 
GDP<-GDP[complete.cases(GDP), ]

#Changing order of columns in GDP Dataset 
GDP <- GDP %>%
  select("Country Name", "Country Code", "Region", "GDP Per Capita ($)" )


#Importing Life Expectancy Dataset   
path<-"http://api.worldbank.org/v2/country/indicator/SP.DYN.LE00.IN?format=json&per_page=20000"
Life_Expectancy<-GET(url=path)
Life_Expectancy <- content(Life_Expectancy, as="text", encoding = "UTF-8")
Life_Expectancy <- fromJSON(Life_Expectancy,flatten=TRUE)
Life_Expectancy <- Life_Expectancy[[2]]
Life_Expectancy$date<-as.numeric(Life_Expectancy$date)

#Choosing only year 2019 
Life_Expectancy <- subset (Life_Expectancy, date == "2019")

Life_Expectancy <- Life_Expectancy %>%
  mutate(`value` = as.numeric(`value`),  # Convert column to numeric
         `value` = round(`value`))      # Round the values

colnames(Life_Expectancy)[colnames(Life_Expectancy) == "country.value"] <- "Country Name"
colnames(Life_Expectancy)[colnames(Life_Expectancy) == "countryiso3code"] <- "Country Code" 
colnames(Life_Expectancy)[colnames(Life_Expectancy) == "value"] <- "Life Expectancy"

Life_Expectancy <- Life_Expectancy %>%
  select("Country Name", "Country Code", "Life Expectancy")

#Finding which columns include NAs
colSums(is.na(Life_Expectancy))

#Seeing the rows which have NAs
Life_Expectancy[!complete.cases(Life_Expectancy), ]

#Removing NAs from test dataset 
Life_Expectancy<-Life_Expectancy[complete.cases(Life_Expectancy), ]

#Merge GDP and Life_Expectancy datasets 
GDP_LifeExpectancy <- merge(GDP, Life_Expectancy, by = c("Country Name", "Country Code"))  


  
#Importing a dataset of Homicide rate per 100,000 people in countries 
path<-"http://api.worldbank.org/v2/country/indicator/VC.IHR.PSRC.P5?format=json&per_page=20000"
Homicide_Rate<-GET(url=path)
Homicide_Rate <- content(Homicide_Rate, as="text", encoding = "UTF-8")
Homicide_Rate <- fromJSON(Homicide_Rate,flatten=TRUE)
Homicide_Rate <- Homicide_Rate[[2]]
Homicide_Rate$date<-as.numeric(Homicide_Rate$date)

#Choosing only year 2019 
Homicide_Rate <- subset (Homicide_Rate, date == "2019")

Homicide_Rate <- Homicide_Rate %>%
  mutate(`value` = as.numeric(`value`),  # Convert column to numeric
         `value` = round(`value`))      # Round the values

colnames(Homicide_Rate)[colnames(Homicide_Rate) == "country.value"] <- "Country Name"
colnames(Homicide_Rate)[colnames(Homicide_Rate) == "countryiso3code"] <- "Country Code" 
colnames(Homicide_Rate)[colnames(Homicide_Rate) == "value"] <- "Intentional Homicide Per 100,000 People"

Homicide_Rate <- Homicide_Rate %>%
  select("Country Name", "Country Code", "Intentional Homicide Per 100,000 People")

#Finding which columns include NAs
colSums(is.na(Homicide_Rate))

#Seeing the rows which have NAs
Homicide_Rate[!complete.cases(Homicide_Rate), ]

#Removing NAs from test dataset 
Homicide_Rate<-Homicide_Rate[complete.cases(Homicide_Rate), ]

#Merge GDP and Homicide_Rate datasets 
GDP_Homicide <- merge(GDP, Homicide_Rate, by = c("Country Name", "Country Code"))



#Importing infant mortality rate (per 1,000 births) dataset 
path<-"http://api.worldbank.org/v2/country/indicator/SP.DYN.IMRT.IN?format=json&per_page=20000"
Infant_Mortality<-GET(url=path)
Infant_Mortality <- content(Infant_Mortality, as="text", encoding = "UTF-8")
Infant_Mortality <- fromJSON(Infant_Mortality,flatten=TRUE)
Infant_Mortality <- Infant_Mortality[[2]]
Infant_Mortality$date<-as.numeric(Infant_Mortality$date)

#Choosing only year 2019 
Infant_Mortality <- subset (Infant_Mortality, date == "2019")

Infant_Mortality <- Infant_Mortality %>%
  mutate(`value` = as.numeric(`value`),  # Convert column to numeric
         `value` = round(`value`))      # Round the values

colnames(Infant_Mortality)[colnames(Infant_Mortality) == "country.value"] <- "Country Name"
colnames(Infant_Mortality)[colnames(Infant_Mortality) == "countryiso3code"] <- "Country Code" 
colnames(Infant_Mortality)[colnames(Infant_Mortality) == "value"] <- "Infant Mortality per 1,000 Births"

Infant_Mortality <- Infant_Mortality %>%
  select("Country Name", "Country Code", "Infant Mortality per 1,000 Births")

#Finding which columns include NAs
colSums(is.na(Infant_Mortality))

#Seeing the rows which have NAs
Infant_Mortality[!complete.cases(Infant_Mortality), ]

#Removing NAs from test dataset 
Infant_Mortality<-Infant_Mortality[complete.cases(Infant_Mortality), ]

#Merge GDP and Infant_Mortality datasets 
GDP_InfantMortality <- merge(GDP, Infant_Mortality, by = c("Country Name", "Country Code"))



#Importing a dataset of secondary school enrollment as a % of total population of a country 
path<-"http://api.worldbank.org/v2/country/indicator/SE.SEC.ENRR?format=json&per_page=20000"
Secondary_School_Enrollment<-GET(url=path)
Secondary_School_Enrollment <- content(Secondary_School_Enrollment, as="text", encoding = "UTF-8")
Secondary_School_Enrollment <- fromJSON(Secondary_School_Enrollment,flatten=TRUE)
Secondary_School_Enrollment <- Secondary_School_Enrollment[[2]]
Secondary_School_Enrollment$date<-as.numeric(Secondary_School_Enrollment$date)

#Choosing only year 2019 
Secondary_School_Enrollment <- subset (Secondary_School_Enrollment, date == "2019")

Secondary_School_Enrollment <- Secondary_School_Enrollment %>%
  mutate(`value` = as.numeric(`value`),  # Convert column to numeric
         `value` = round(`value`))      # Round the values

colnames(Secondary_School_Enrollment)[colnames(Secondary_School_Enrollment) == "country.value"] <- "Country Name"
colnames(Secondary_School_Enrollment)[colnames(Secondary_School_Enrollment) == "countryiso3code"] <- "Country Code" 
colnames(Secondary_School_Enrollment)[colnames(Secondary_School_Enrollment) == "value"] <- "Secondary School Enrollment (% Gross)"

Secondary_School_Enrollment <- Secondary_School_Enrollment %>%
  select("Country Name", "Country Code", "Secondary School Enrollment (% Gross)")

#Finding which columns include NAs
colSums(is.na(Secondary_School_Enrollment))

#Seeing the rows which have NAs
Secondary_School_Enrollment[!complete.cases(Secondary_School_Enrollment), ]

#Removing NAs from test dataset 
Secondary_School_Enrollment<-Secondary_School_Enrollment[complete.cases(Secondary_School_Enrollment), ]

#Merge GDP and Secondary_School_Enrollment datasets 
GDP_SchoolEnrollment <- merge(GDP, Secondary_School_Enrollment, by = c("Country Name", "Country Code"))  

# In this table in some cases I see that % is more or equal to 100%. However this
# is not a mistake and can be explained by several reasons.

# 1. Over-aged and under-aged students. 
# The calculation of the Gross Enrollment Ratio is based on the population 
# of the age group that corresponds to the level of education. 
# In some cases, individuals who are older or younger than the typical age range 
# for secondary education may still be enrolled. 

# 2. Non-resident students



#Importing a dataset of university enrollment as a % of total population of a country 
path<-"http://api.worldbank.org/v2/country/indicator/SE.TER.ENRR?format=json&per_page=20000"
University_Enrollment<-GET(url=path)
University_Enrollment <- content(University_Enrollment, as="text", encoding = "UTF-8")
University_Enrollment <- fromJSON(University_Enrollment,flatten=TRUE)
University_Enrollment <- University_Enrollment[[2]]
University_Enrollment$date<-as.numeric(University_Enrollment$date)

#Choosing only year 2019 
University_Enrollment <- subset (University_Enrollment, date == "2019")

University_Enrollment <- University_Enrollment %>%
  mutate(`value` = as.numeric(`value`),  # Convert column to numeric
         `value` = round(`value`))      # Round the values

colnames(University_Enrollment)[colnames(University_Enrollment) == "country.value"] <- "Country Name"
colnames(University_Enrollment)[colnames(University_Enrollment) == "countryiso3code"] <- "Country Code" 
colnames(University_Enrollment)[colnames(University_Enrollment) == "value"] <- "University Enrollment (% Gross)"

University_Enrollment <- University_Enrollment %>%
  select("Country Name", "Country Code", "University Enrollment (% Gross)")

#Finding which columns include NAs
colSums(is.na(University_Enrollment))

#Seeing the rows which have NAs
University_Enrollment[!complete.cases(University_Enrollment), ]

#Removing NAs from test dataset 
University_Enrollment<-University_Enrollment[complete.cases(University_Enrollment), ]

#Merge GDP and University_Enrollment datasets 
GDP_UniversityEnrollment <- merge(GDP, University_Enrollment, by = c("Country Name", "Country Code"))  


# In this table in some cases I see that % is more or equal to 100%. However this
# is not a mistake and can be explained by several reasons.

# 1. Over-aged and under-aged students. 
# The calculation of the Gross Enrollment Ratio is based on the population 
# of the age group that corresponds to the level of education. 
# In some cases, individuals who are older or younger than the typical age range 
# for tertiary education may still be enrolled. 

# 2. Non-resident students 

# 3. Double-counting or multiple enrollments. 
# In some cases, students may be enrolled in multiple tertiary institutions 
# simultaneously, leading to double-counting in the enrollment data.    


  
#Importing unemployment dataset. That is unemployed people as share of the labor 
#force that is without work but available for and seeking employment. 
path<-"http://api.worldbank.org/v2/country/indicator/SL.UEM.TOTL.NE.ZS?format=json&per_page=20000"
Unemployment<-GET(url=path)
Unemployment <- content(Unemployment, as="text", encoding = "UTF-8")
Unemployment <- fromJSON(Unemployment,flatten=TRUE)
Unemployment <- Unemployment[[2]]
Unemployment$date<-as.numeric(Unemployment$date)

#Choosing only year 2019 
Unemployment <- subset (Unemployment, date == "2019")

Unemployment <- Unemployment %>%
  mutate(`value` = as.numeric(`value`),  # Convert column to numeric
         `value` = round(`value`,2))      # Round the values

colnames(Unemployment)[colnames(Unemployment) == "country.value"] <- "Country Name"
colnames(Unemployment)[colnames(Unemployment) == "countryiso3code"] <- "Country Code" 
colnames(Unemployment)[colnames(Unemployment) == "value"] <- "Unemployment (%)"

Unemployment <- Unemployment %>%
  select("Country Name", "Country Code", "Unemployment (%)")

#Finding which columns include NAs
colSums(is.na(Unemployment))

#Seeing the rows which have NAs
Unemployment[!complete.cases(Unemployment), ]

#Removing NAs from test dataset 
Unemployment<-Unemployment[complete.cases(Unemployment), ]

#Merge GDP and Unemployment datasets 
GDP_Unemployment <- merge(GDP, Unemployment, by = c("Country Name", "Country Code"))  



#Importing Press Freedom Index dataset     
Press_Freedom_Index <- read.csv("https://tcdata360-backend.worldbank.org/api/v1/datasets/1000/dump.csv")

#Creating a dataset that will only include the 
#Country Code, Name and year which is 2019
Press_Freedom_Index <- Press_Freedom_Index %>% 
  select("Country.Name", "Country.ISO3", "X2019", "Indicator")

#Filtering out data from Press_Freedom_Index in the way that it only includes 
#Press Freedom Index as indicator 

Press_Freedom_Index <- Press_Freedom_Index %>%
  filter(Indicator == "Press Freedom Index")

#Finding which columns include NAs
colSums(is.na(Press_Freedom_Index))

#Seeing the rows which have NAs
Press_Freedom_Index[!complete.cases(Press_Freedom_Index), ]

#Removing NAs from test dataset 
Press_Freedom_Index<-Press_Freedom_Index[complete.cases(Press_Freedom_Index), ]

#Removing 4th Column 
Press_Freedom_Index<-Press_Freedom_Index[,-4]

#Changing Column Name 
colnames(Press_Freedom_Index)[colnames(Press_Freedom_Index) == "X2019"] <- "Press Freedom Index"
colnames(Press_Freedom_Index)[colnames(Press_Freedom_Index) == "Country.ISO3"] <- "Country Code"
colnames(Press_Freedom_Index)[colnames(Press_Freedom_Index) == "Country.Name"] <- "Country Name"

#Merging GDP and Press Freedom Index datasets 
GDP_PressFreedom <- merge(GDP, Press_Freedom_Index, by = c("Country Name", "Country Code"))


library(shiny)
library(shinydashboard)
library(ggplot2)
library(plotly)

# Define UI
ui <- dashboardPage(
  dashboardHeader(title = "GDP Correlation Dashboard"),
  dashboardSidebar(
    checkboxGroupInput(
      inputId = "regions",
      label = "Please select the regions you want to see",
      choices = c("All", "South Asia", "Europe & Central Asia", "Middle East & North Africa", "East Asia & Pacific", "Latin America & Caribbean ", "North America", "Sub-Saharan Africa "),
      selected = "All",
      inline = FALSE,
      width = NULL
    ), #Creating a checkbox which automatically shows All choices when entering
    #the dashboard
    conditionalPanel(
      condition = "input.tabs == 'Homicide Rate'",
      selectInput(
        inputId = "countrySelectHomicide",
        label = "Please select a specific country you want to see",
        choices = c("", unique(GDP_Homicide$`Country Name`)),
        selected = ""
      )
    ),
    conditionalPanel(
      condition = "input.tabs == 'Life Expectancy'",
      selectInput(
        inputId = "countrySelectExpectancy",
        label = "Please select a specific country you want to see",
        choices = c("", unique(GDP_LifeExpectancy$`Country Name`)),
        selected = ""
      )
    ),
    conditionalPanel(
      condition = "input.tabs == 'Infant Mortality'",
      selectInput(
        inputId = "countrySelectInfant",
        label = "Please select a specific country you want to see",
        choices = c("", unique(GDP_InfantMortality$`Country Name`)),
        selected = ""
      )
    ),
    conditionalPanel(
      condition = "input.tabs == 'School Enrollment'",
      selectInput(
        inputId = "countrySelectSchool",
        label = "Please select a specific country you want to see",
        choices = c("", unique(GDP_SchoolEnrollment$`Country Name`)),
        selected = ""
      )
    ),
    conditionalPanel(
      condition = "input.tabs == 'University Enrollment'",
      selectInput(
        inputId = "countrySelectUniversity",
        label = "Please select a specific country you want to see",
        choices = c("", unique(GDP_UniversityEnrollment$`Country Name`)),
        selected = ""
      )
    ),
    conditionalPanel(
      condition = "input.tabs == 'Unemployment'",
      selectInput(
        inputId = "countrySelectUnemployment",
        label = "Please select a specific country you want to see",
        choices = c("", unique(GDP_Unemployment$`Country Name`)),
        selected = ""
      )
    ),
    conditionalPanel(
      condition = "input.tabs == 'Press Freedom'",
      selectInput(
        inputId = "countrySelectPress",
        label = "Please select a specific country you want to see",
        choices = c("", unique(GDP_PressFreedom$`Country Name`)),
        selected = ""
      )
    )
  ),
  dashboardBody(
    fluidPage(
      fluidRow(
        column(
          width = 12,
          tabBox(
            id = "tabs", height = 600,
            title = "",
            width = NULL,
            tabPanel("Homicide Rate",
                     plotlyOutput(outputId = "homicide_plot"),
                     infoBoxOutput("boxHomicide1", width = 4),
                     infoBoxOutput("boxHomicide2", width = 8)
            ),
            tabPanel("Life Expectancy",
                     plotlyOutput(outputId = "expectancy_plot"),
                     infoBoxOutput("boxExpectancy1", width = 4),
                     infoBoxOutput("boxExpectancy2", width = 8)
            ),
            tabPanel("Infant Mortality",
                     plotlyOutput(outputId = "infant_plot"),
                     infoBoxOutput("boxInfant1", width = 4),
                     infoBoxOutput("boxInfant2", width = 8)
            ),
            tabPanel("School Enrollment",
                     plotlyOutput(outputId = "school_plot"),
                     infoBoxOutput("boxSchool1", width = 4),
                     infoBoxOutput("boxSchool2", width = 8)
            ),
            tabPanel("University Enrollment",
                     plotlyOutput(outputId = "university_plot"),
                     infoBoxOutput("boxUni1", width = 4),
                     infoBoxOutput("boxUni2", width = 8)
            ),
            tabPanel("Unemployment",
                     plotlyOutput(outputId = "unemployment_plot"),
                     infoBoxOutput("boxUnemployment1", width = 4),
                     infoBoxOutput("boxUnemployment2", width = 8)
            ),
            tabPanel("Press Freedom",
                     plotlyOutput(outputId = "press_plot"),
                     infoBoxOutput("boxPress1", width = 4),
                     infoBoxOutput("boxPress2", width = 8)
            ),
            
          )
        )
      )
    )
  )
)


# Define server
server <- function(input, output) {
  # Filter data based on selected regions and country
  filtered_data_homicide <- reactive({
    data <- if ("All" %in% input$regions) {
      GDP_Homicide
    } else {
      GDP_Homicide[GDP_Homicide$Region %in% input$regions, ]
    }
    
    if (!is.null(input$countrySelectHomicide) && input$countrySelectHomicide != "") {
      selected_country <- input$countrySelectHomicide
      data <- rbind(data[data$`Country Name` == selected_country, ], data)
    }
    
    data
  })
  
  
  # Filter data based on selected regions and country
  filtered_data_expectancy <- reactive({
    data <- if ("All" %in% input$regions) {
      GDP_LifeExpectancy
    } else {
      GDP_LifeExpectancy[GDP_LifeExpectancy$Region %in% input$regions, ]
    }
    
    if (!is.null(input$countrySelectExpectancy) && input$countrySelectExpectancy != "") {
      selected_country <- input$countrySelectExpectancy
      data <- rbind(data[data$`Country Name` == selected_country, ], data)
    }
    
    data
  })
  
  # Filter data based on selected regions and country
  filtered_data_infant <- reactive({
    data <- if ("All" %in% input$regions) {
      GDP_InfantMortality
    } else {
      GDP_InfantMortality[GDP_InfantMortality$Region %in% input$regions, ]
    }
    
    if (!is.null(input$countrySelectInfant) && input$countrySelectInfant != "") {
      selected_country <- input$countrySelectInfant
      data <- rbind(data[data$`Country Name` == selected_country, ], data)
    }
    
    data
  })
  
  # Filter data based on selected regions and country
  filtered_data_school <- reactive({
    data <- if ("All" %in% input$regions) {
      GDP_SchoolEnrollment
    } else {
      GDP_SchoolEnrollment[GDP_SchoolEnrollment$Region %in% input$regions, ]
    }
    
    if (!is.null(input$countrySelectSchool) && input$countrySelectSchool != "") {
      selected_country <- input$countrySelectSchool
      data <- rbind(data[data$`Country Name` == selected_country, ], data)
    }
    
    data
  })
  
  # Filter data based on selected regions and country
  filtered_data_university <- reactive({
    data <- if ("All" %in% input$regions) {
      GDP_UniversityEnrollment
    } else {
      GDP_UniversityEnrollment[GDP_UniversityEnrollment$Region %in% input$regions, ]
    }
    
    if (!is.null(input$countrySelectUniversity) && input$countrySelectUniversity != "") {
      selected_country <- input$countrySelectUniversity
      data <- rbind(data[data$`Country Name` == selected_country, ], data)
    }
    
    data
  })
  
  
  # Filter data based on selected regions and country
  filtered_data_unemployment <- reactive({
    data <- if ("All" %in% input$regions) {
      GDP_Unemployment
    } else {
      GDP_Unemployment[GDP_Unemployment$Region %in% input$regions, ]
    }
    
    if (!is.null(input$countrySelectUnemployment) && input$countrySelectUnemployment != "") {
      selected_country <- input$countrySelectUnemployment
      data <- rbind(data[data$`Country Name` == selected_country, ], data)
    }
    
    data
  })
  
  # Filter data based on selected regions and country
  filtered_data_press <- reactive({
    data <- if ("All" %in% input$regions) {
      GDP_PressFreedom
    } else {
      GDP_PressFreedom[GDP_PressFreedom$Region %in% input$regions, ]
    }
    
    if (!is.null(input$countrySelectPress) && input$countrySelectPress != "") {
      selected_country <- input$countrySelectPress
      data <- rbind(data[data$`Country Name` == selected_country, ], data)
    }
    
    data
  })
  
  
  # Generate the Homicide Rate plot
  output$homicide_plot <- renderPlotly({
    p <- ggplot(data = filtered_data_homicide(), aes(x = `GDP Per Capita ($)`, y = `Intentional Homicide Per 100,000 People`, col = `Region`, text = paste("Country Name:", `Country Name`, "\n",
                                                                                                                                                           "Region:", `Region`, "\n",
                                                                                                                                                           "GDP Per Capita ($):", `GDP Per Capita ($)`, "\n",
                                                                                                                                                           "Intentional Homicide Per 100,000 People:", `Intentional Homicide Per 100,000 People`))) +
      geom_point(aes(alpha = ifelse(`Country Name` == input$countrySelectHomicide, 1, 0.9)), show.legend = FALSE) +
      #The alpha function in ifelse ensures that if user selects a specific country 
      #that country will have a transparency of 1 and all others will have 0.5
      scale_x_continuous(breaks= c(30000,60000,90000,120000,150000),
                         limits= c(0,180000)) + 
      scale_y_continuous(breaks= c(10,20,30,40,50),
                         
                         limits= c(0,50)) + labs(x = "GDP Per Capita ($)", y = "Intentional Homicide Per 100,000 People") +
      ggtitle("Correlation: GDP Per Capita vs. Intentional Homicide")
    
    ggplotly(p, tooltip = "text")
  })
  
  
  
  # Generate the life expectancy plot
  output$expectancy_plot <- renderPlotly({
    p <- ggplot(data = filtered_data_expectancy(), aes(x = `GDP Per Capita ($)`, y = `Life Expectancy`, col = `Region`, text = paste("Country Name:", `Country Name`, "\n",
                                                                                                                                     "Region:", `Region`, "\n",
                                                                                                                                     "GDP Per Capita ($):", `GDP Per Capita ($)`, "\n",
                                                                                                                                     "Life Expectancy:", `Life Expectancy`))) +
      geom_point(aes(alpha = ifelse(`Country Name` == input$countrySelectExpectancy, 1, 0.9)), show.legend = FALSE) +
      scale_x_continuous(breaks= c(30000,60000,90000),
                         limits= c(0,90000)) + 
      scale_y_continuous(breaks= c(50,60,70,80,90),
                         
                         limits= c(50,90)) + labs(x = "GDP Per Capita ($)", y = "Life Expectancy") +
      ggtitle("Correlation: GDP Per Capita vs. Life Expectancy")
    
    ggplotly(p, tooltip = "text")
  })
  
  # Generate the Infant Mortality Score plot
  output$infant_plot <- renderPlotly({
    p <- ggplot(data = filtered_data_infant(), aes(x = `GDP Per Capita ($)`, y = `Infant Mortality per 1,000 Births`, col = `Region`, text = paste("Country Name:", `Country Name`, "\n",
                                                                                                                                                   "Region:", `Region`, "\n",
                                                                                                                                                   "GDP Per Capita ($):", `GDP Per Capita ($)`, "\n",
                                                                                                                                                   "Infant Mortality:", `Infant Mortality per 1,000 Births`))) +
      geom_point(aes(alpha = ifelse(`Country Name` == input$countrySelectInfant, 1, 0.9)), show.legend = FALSE) +
      scale_x_continuous(breaks= c(30000,60000,90000),
                         limits= c(0,120000)) + 
      scale_y_continuous(breaks= c(20,40,60,80),
                         
                         limits= c(0,90)) + labs(x = "GDP Per Capita ($)", y = "Infant Mortality Per 1,000 Births") +
      ggtitle("Correlation: GDP Per Capita vs. Infant Mortality")
    
    ggplotly(p, tooltip = "text")
  })
  
  # Generate the University Enrollment plot
  output$university_plot <- renderPlotly({
    p <- ggplot(data = filtered_data_university(), aes(x = `GDP Per Capita ($)`, y = `University Enrollment (% Gross)`, col = `Region`, text = paste("Country Name:", `Country Name`, "\n",
                                                                                                                                                     "Region:", `Region`, "\n",
                                                                                                                                                     "GDP Per Capita ($):", `GDP Per Capita ($)`, "\n",
                                                                                                                                                     paste("University Enrollment: ", `University Enrollment (% Gross)`, "%", sep="" )))) +
      geom_point(aes(alpha = ifelse(`Country Name` == input$countrySelectUniversity, 1, 0.9)), show.legend = FALSE) +
      scale_x_continuous(breaks= c(30000,60000,90000),
                         limits= c(0,120000)) + 
      scale_y_continuous(breaks= c(20,40,60,80,100,120,140),
                         
                         limits= c(0,150)) + labs(x = "GDP Per Capita ($)", y = "University Enrollment (% Gross)") +
      ggtitle("Correlation: GDP Per Capita vs. University Enrollment %")
    
    ggplotly(p, tooltip = "text")
  })
  
  # Generate the Secondary School Enrollment plot
  output$school_plot <- renderPlotly({
    p <- ggplot(data = filtered_data_school(), aes(x = `GDP Per Capita ($)`, y = `Secondary School Enrollment (% Gross)`, col = `Region`, text = paste("Country Name:", `Country Name`, "\n",
                                                                                                                                                       "Region:", `Region`, "\n",
                                                                                                                                                       "GDP Per Capita ($):", `GDP Per Capita ($)`, "\n",
                                                                                                                                                       paste("School Enrollment: ", `Secondary School Enrollment (% Gross)`, "%", sep="" )))) +
      geom_point(aes(alpha = ifelse(`Country Name` == input$countrySelectSchool, 1, 0.9)), show.legend = FALSE) +
      scale_x_continuous(breaks= c(30000,60000,90000),
                         limits= c(0,120000)) + 
      scale_y_continuous(breaks= c(20,40,60,80,100,120,140,160),
                         
                         limits= c(10,160)) + labs(x = "GDP Per Capita ($)", y = "Secondary School Enrollment (% Gross)") +
      ggtitle("Correlation: GDP Per Capita vs. Secondary School Enrollment %")
    
    ggplotly(p, tooltip = "text")
  })
  
 
  # Generate the Unemployment plot
  output$unemployment_plot <- renderPlotly({
    p <- ggplot(data = filtered_data_unemployment(), aes(x = `GDP Per Capita ($)`, y = `Unemployment (%)`, col = `Region`, text = paste("Country Name:", `Country Name`, "\n",
                                                                                                                                        "Region:", `Region`, "\n",
                                                                                                                                        "GDP Per Capita ($):", `GDP Per Capita ($)`, "\n",
                                                                                                                                        paste("Unemployment: ", `Unemployment (%)`, "%", sep="")))) +
      geom_point(aes(alpha = ifelse(`Country Name` == input$countrySelectUnemployment, 1, 0.9)), show.legend = FALSE) +
      scale_x_continuous(breaks= c(30000,60000,90000),
                         limits= c(0,120000)) + 
      scale_y_continuous(breaks= c(5,10,15,20,25,30),
                         
                         limits= c(0,30)) + labs(x = "GDP Per Capita ($)", y = "Unemployment (%)") +
      ggtitle("Correlation: GDP Per Capita vs. Unemployment")
    
    ggplotly(p, tooltip = "text")
  })
  
  # Generate the Press Freedom Index plot
  output$press_plot <- renderPlotly({
    p <- ggplot(data = filtered_data_press(), aes(x = `GDP Per Capita ($)`, y = `Press Freedom Index`, col = `Region`, text = paste("Country Name:", `Country Name`, "\n",
                                                                                                                                    "Region:", `Region`, "\n",
                                                                                                                                    "GDP Per Capita ($):", `GDP Per Capita ($)`, "\n",
                                                                                                                                    "Freedom Index:", `Press Freedom Index`))) +
      geom_point(aes(alpha = ifelse(`Country Name` == input$countrySelectPress, 1, 0.9)), show.legend = FALSE) +
      scale_x_continuous(breaks= c(30000,60000,90000),
                         limits= c(0,120000)) + 
      scale_y_continuous(breaks= c(10,20,30,40,50,60,70,80),
                         
                         limits= c(0,90)) + labs(x = "GDP Per Capita ($)", y = "Press Freedom Index") +
      ggtitle("Correlation: GDP Per Capita vs. Press Freedom")
    
    ggplotly(p, tooltip = "text")
  })
  
  
  # Create the Homicide Rate info box1
  output$boxHomicide1 <- renderInfoBox({
    infoBox(
      "Selected Countries",
      nrow(filtered_data_homicide()),
      icon = icon("globe"),
      color = "light-blue"
    )
  })
  # Create the Homicide Rate info box2
  output$boxHomicide2 <- renderInfoBox({
    # Check if a specific country has been selected
    if (!is.null(input$countrySelectHomicide) && input$countrySelectHomicide != "") {
      selected_country <- input$countrySelectHomicide
      # Get the region of the selected country
      country_region <- unique(GDP_Homicide[GDP_Homicide$`Country Name` == selected_country,]$Region)
      
      # Calculate the average GDP and Homicide rate for the region
      average_gdp <- mean(GDP_Homicide[GDP_Homicide$Region == country_region, ]$`GDP Per Capita ($)`)
      average_homicide <- mean(GDP_Homicide[GDP_Homicide$Region == country_region, ]$`Intentional Homicide Per 100,000 People`)
      #Generate different comments for each data digest infobox by using if/else function
      infoBox(
        "Data Digest",
        HTML(paste(
          "<div style='font-size: 15px;'>",
          "This country is in region: ", country_region,
          "<br>",
          paste("Average GDP Per capita for countries in this region is: $", round(average_gdp), sep=""),
          "<br>",
          "Average Homicide rate for countries in this region is:", round(average_homicide, 2),
          "</div>"
        )),
        icon = icon("search"),
        color = "olive"
      )
    } else {
      selected_regions <- input$regions
      
      if (is.null(selected_regions) || length(selected_regions) == 0) {
        infoBox(
          "Data Digest",
          "Select the countries to see the information.",
          icon = icon("search"),
          color = "olive"
        )
      } else if ("All" %in% selected_regions) {
        # Calculate average for all countries
        average_gdp <- mean(GDP_Homicide$`GDP Per Capita ($)`)
        average_homicide <- mean(GDP_Homicide$`Intentional Homicide Per 100,000 People`)
        
        infoBox(
          "Data Digest",
          HTML(paste(
            "<div style='font-size: 15px;'>",
            paste("Average GDP Per capita for these countries is: $", round(average_gdp), sep=""),
            "<br>",
            "Average Homicide rate for these countries is:", round(average_homicide, 2),
            "</div>"
          )),
          icon = icon("search"),
          color = "olive"
        )
      } else if (length(selected_regions) == 1) {
        # Calculate average for a single region
        selected_region <- selected_regions[1]
        average_gdp <- mean(GDP_Homicide[GDP_Homicide$Region == selected_region, ]$`GDP Per Capita ($)`)
        average_homicide <- mean(GDP_Homicide[GDP_Homicide$Region == selected_region, ]$`Intentional Homicide Per 100,000 People`)
        
        infoBox(
          "Data Digest",
          HTML(paste(
            "<div style='font-size: 15px;'>",
            paste("Average GDP Per capita for countries in this region is: $", round(average_gdp), sep=""),
            "<br>",
            "Average Homicide rate for countries in this region is:", round(average_homicide, 2),
            "</div>"
          )),
          icon = icon("search"),
          color = "olive"
        )
      } else {
        selected_regions <- selected_regions[selected_regions != "All"]
        average_gdp <- mean(GDP_Homicide[GDP_Homicide$Region %in% selected_regions, ]$`GDP Per Capita ($)`)
        average_homicide <- mean(GDP_Homicide[GDP_Homicide$Region %in% selected_regions, ]$`Intentional Homicide Per 100,000 People`)
        
        infoBox(
          "Data Digest",
          HTML(paste(
            "<div style='font-size: 15px;'>",
            paste("Average GDP Per capita for countries in these regions is: $", round(average_gdp), sep=""),
            "<br>",
            "Average Homicide rate for countries in these regions is:", round(average_homicide, 2),
            "</div>"
          )),
          icon = icon("search"),
          color = "olive"
        )
      }
    }
  })
  
  # Create the Life Expectancy info box1
  output$boxExpectancy1 <- renderInfoBox({
    infoBox(
      "Selected Countries",
      nrow(filtered_data_expectancy()),
      icon = icon("globe"),
      color = "light-blue"
    )
  })
  # Create the Life Expectancy info box2
  output$boxExpectancy2 <- renderInfoBox({
    # Check if a specific country has been selected
    if (!is.null(input$countrySelectExpectancy) && input$countrySelectExpectancy != "") {
      selected_country <- input$countrySelectExpectancy
      # Get the region of the selected country
      country_region <- unique(GDP_LifeExpectancy[GDP_LifeExpectancy$`Country Name` == selected_country,]$Region)
      
      # Calculate the average GDP and Homicide rate for the region
      average_gdp <- mean(GDP_LifeExpectancy[GDP_LifeExpectancy$Region == country_region, ]$`GDP Per Capita ($)`)
      average_lifeexpectancy <- mean(GDP_LifeExpectancy[GDP_Homicide$Region == country_region, ]$`Life Expectancy`)
      
      infoBox(
        "Data Digest",
        HTML(paste(
          "<div style='font-size: 15px;'>",
          "This country is in region: ", country_region,
          "<br>",
          paste("Average GDP Per capita for countries in this region is: $", round(average_gdp), sep=""),
          "<br>",
          "Average life expectancy for countries in this region is:", round(average_lifeexpectancy),
          "</div>"
        )),
        icon = icon("search"),
        color = "olive"
      )
    } else {
      selected_regions <- input$regions
      
      if (is.null(selected_regions) || length(selected_regions) == 0) {
        infoBox(
          "Data Digest",
          "Select the countries to see the information.",
          icon = icon("search"),
          color = "olive"
        )
      } else if ("All" %in% selected_regions) {
        # Calculate average for all countries
        average_gdp <- mean(GDP_LifeExpectancy$`GDP Per Capita ($)`)
        average_lifeexpectancy <- mean(GDP_LifeExpectancy$`Life Expectancy`)
        
        infoBox(
          "Data Digest",
          HTML(paste(
            "<div style='font-size: 15px;'>",
            paste("Average GDP Per capita for these countries is: $", round(average_gdp), sep=""),
            "<br>",
            "Average life expectancy for these countries is:", round(average_lifeexpectancy),
            "</div>"
          )),
          icon = icon("search"),
          color = "olive"
        )
      } else if (length(selected_regions) == 1) {
        # Calculate average for a single region
        selected_region <- selected_regions[1]
        average_gdp <- mean(GDP_LifeExpectancy[GDP_LifeExpectancy$Region == selected_region, ]$`GDP Per Capita ($)`)
        average_lifeexpectancy <- mean(GDP_LifeExpectancy[GDP_LifeExpectancy$Region == selected_region, ]$`Life Expectancy`)
        
        infoBox(
          "Data Digest",
          HTML(paste(
            "<div style='font-size: 15px;'>",
            paste("Average GDP Per capita for countries in this region is: $", round(average_gdp), sep=""),
            "<br>",
            "Average life expectancy for countries in this region is:", round(average_lifeexpectancy),
            "</div>"
          )),
          icon = icon("search"),
          color = "olive"
        )
      } else {
        selected_regions <- selected_regions[selected_regions != "All"]
        average_gdp <- mean(GDP_LifeExpectancy[GDP_LifeExpectancy$Region %in% selected_regions, ]$`GDP Per Capita ($)`)
        average_lifeexpectancy <- mean(GDP_LifeExpectancy[GDP_LifeExpectancy$Region %in% selected_regions, ]$`Life Expectancy`)
        
        infoBox(
          "Data Digest",
          HTML(paste(
            "<div style='font-size: 15px;'>",
            paste("Average GDP Per capita for countries in these regions is: $", round(average_gdp), sep=""),
            "<br>",
            "Average life expectancy rate for countries in these regions is:", round(average_lifeexpectancy),
            "</div>"
          )),
          icon = icon("search"),
          color = "olive"
        )
      }
    }
  })
  
  # Create the Infant Mortality Rate info box1
  output$boxInfant1 <- renderInfoBox({
    infoBox(
      "Selected Countries",
      nrow(filtered_data_infant()),
      icon = icon("globe"),
      color = "light-blue"
    )
  })
  # Create the Infant Mortality Rate info box1
  output$boxInfant2 <- renderInfoBox({
    # Check if a specific country has been selected
    if (!is.null(input$countrySelectInfant) && input$countrySelectInfant != "") {
      selected_country <- input$countrySelectInfant
      # Get the region of the selected country
      country_region <- unique(GDP_InfantMortality[GDP_InfantMortality$`Country Name` == selected_country,]$Region)
      
      # Calculate the average GDP and Homicide rate for the region
      average_gdp <- mean(GDP_InfantMortality[GDP_InfantMortality$Region == country_region, ]$`GDP Per Capita ($)`)
      average_infant <- mean(GDP_InfantMortality[GDP_InfantMortality$Region == country_region, ]$`Infant Mortality per 1,000 Births`)
      
      infoBox(
        "Data Digest",
        HTML(paste(
          "<div style='font-size: 15px;'>",
          "This country is in region: ", country_region,
          "<br>",
          paste("Average GDP Per capita for countries in this region is: $", round(average_gdp), sep=""),
          "<br>",
          "Average infant mortality rate for countries in this region is:", round(average_infant),
          "</div>"
        )),
        icon = icon("search"),
        color = "olive"
      )
    } else {
      selected_regions <- input$regions
      
      if (is.null(selected_regions) || length(selected_regions) == 0) {
        infoBox(
          "Data Digest",
          "Select the countries to see the information.",
          icon = icon("search"),
          color = "olive"
        )
      } else if ("All" %in% selected_regions) {
        # Calculate average for all countries
        average_gdp <- mean(GDP_InfantMortality$`GDP Per Capita ($)`)
        average_infant <- mean(GDP_InfantMortality$`Infant Mortality per 1,000 Births`)
        
        infoBox(
          "Data Digest",
          HTML(paste(
            "<div style='font-size: 15px;'>",
            paste("Average GDP Per capita for these countries is: $", round(average_gdp), sep=""),
            "<br>",
            "Average infant mortality rate for these countries is:", round(average_infant),
            "</div>"
          )),
          icon = icon("search"),
          color = "olive"
        )
      } else if (length(selected_regions) == 1) {
        # Calculate average for a single region
        selected_region <- selected_regions[1]
        average_gdp <- mean(GDP_InfantMortality[GDP_InfantMortality$Region == selected_region, ]$`GDP Per Capita ($)`)
        average_infant <- mean(GDP_InfantMortality[GDP_InfantMortality$Region == selected_region, ]$`Infant Mortality per 1,000 Births`)
        
        infoBox(
          "Data Digest",
          HTML(paste(
            "<div style='font-size: 15px;'>",
            paste("Average GDP Per capita for countries in this region is: $", round(average_gdp), sep=""),
            "<br>",
            "Average infant mortality rate for countries in this region is:", round(average_infant),
            "</div>"
          )),
          icon = icon("search"),
          color = "olive"
        )
      } else {
        selected_regions <- selected_regions[selected_regions != "All"]
        average_gdp <- mean(GDP_InfantMortality[GDP_InfantMortality$Region %in% selected_regions, ]$`GDP Per Capita ($)`)
        average_infant <- mean(GDP_InfantMortality[GDP_InfantMortality$Region %in% selected_regions, ]$`Infant Mortality per 1,000 Births`)
        
        infoBox(
          "Data Digest",
          HTML(paste(
            "<div style='font-size: 15px;'>",
            paste("Average GDP Per capita for countries in these regions is: $", round(average_gdp), sep=""),
            "<br>",
            "Average infant mortality rate for countries in these regions is:", round(average_infant),
            "</div>"
          )),
          icon = icon("search"),
          color = "olive"
        )
      }
    }
  })
  
  # Create the Secondary School Enrollment info box1
  output$boxSchool1 <- renderInfoBox({
    infoBox(
      "Selected Countries",
      nrow(filtered_data_school()),
      icon = icon("globe"),
      color = "light-blue"
    )
  })
  # Create the Secondary School Enrollment info box2
  output$boxSchool2 <- renderInfoBox({
    # Check if a specific country has been selected
    if (!is.null(input$countrySelectSchool) && input$countrySelectSchool != "") {
      selected_country <- input$countrySelectSchool
      # Get the region of the selected country
      country_region <- unique(GDP_SchoolEnrollment[GDP_SchoolEnrollment$`Country Name` == selected_country,]$Region)
      
      # Calculate the average GDP and Homicide rate for the region
      average_gdp <- mean(GDP_SchoolEnrollment[GDP_SchoolEnrollment$Region == country_region, ]$`GDP Per Capita ($)`)
      average_school <- mean(GDP_SchoolEnrollment[GDP_SchoolEnrollment$Region == country_region, ]$`Secondary School Enrollment (% Gross)`)
      
      infoBox(
        "Data Digest",
        HTML(paste(
          "<div style='font-size: 15px;'>",
          "This country is in region: ", country_region,
          "<br>",
          paste("Average GDP Per capita for countries in this region is: $", round(average_gdp), sep = ""),
          "<br>",
          paste("Average secondary school enrollment rate for countries in this region is: ", round(average_school), "%", sep=""),
          "</div>"
        )),
        icon = icon("search"),
        color = "olive"
      )
    } else {
      selected_regions <- input$regions
      
      if (is.null(selected_regions) || length(selected_regions) == 0) {
        infoBox(
          "Data Digest",
          "Select the countries to see the information.",
          icon = icon("search"),
          color = "olive"
        )
      } else if ("All" %in% selected_regions) {
        # Calculate average for all countries
        average_gdp <- mean(GDP_SchoolEnrollment$`GDP Per Capita ($)`)
        average_school <- mean(GDP_SchoolEnrollment$`Secondary School Enrollment (% Gross)`)
        
        infoBox(
          "Data Digest",
          HTML(paste(
            "<div style='font-size: 15px;'>",
            paste("Average GDP Per capita for these countries is: $", round(average_gdp), sep=""),
            "<br>",
            paste("Average secondary school enrollment rate for these countries is: ", round(average_school), "%", sep=""),
            "</div>"
          )),
          icon = icon("search"),
          color = "olive"
        )
      } else if (length(selected_regions) == 1) {
        # Calculate average for a single region
        selected_region <- selected_regions[1]
        average_gdp <- mean(GDP_SchoolEnrollment[GDP_SchoolEnrollment$Region == selected_region, ]$`GDP Per Capita ($)`)
        average_school <- mean(GDP_SchoolEnrollment[GDP_SchoolEnrollment$Region == selected_region, ]$`Secondary School Enrollment (% Gross)`)
        
        infoBox(
          "Data Digest",
          HTML(paste(
            "<div style='font-size: 15px;'>",
            paste("Average GDP Per capita for countries in this region is: $", round(average_gdp), sep=""),
            "<br>",
            paste("Average secondary school enrollment for countries in this region is: ", round(average_school), "%", sep=""),
            "</div>"
          )),
          icon = icon("search"),
          color = "olive"
        )
      } else {
        selected_regions <- selected_regions[selected_regions != "All"]
        average_gdp <- mean(GDP_SchoolEnrollment[GDP_SchoolEnrollment$Region %in% selected_regions, ]$`GDP Per Capita ($)`)
        average_school <- mean(GDP_SchoolEnrollment[GDP_SchoolEnrollment$Region %in% selected_regions, ]$`Secondary School Enrollment (% Gross)`)
        
        infoBox(
          "Data Digest",
          HTML(paste(
            "<div style='font-size: 15px;'>",
            paste("Average GDP Per capita for countries in these regions is: $", round(average_gdp), sep=""),
            "<br>",
            paste("Average secondary school enrollment rate for countries in these regions is: ", round(average_school), "%", sep=""),
            "</div>"
          )),
          icon = icon("search"),
          color = "olive"
        )
      }
    }
  })
  
  # Create the University Enrollment info box1
  output$boxUni1 <- renderInfoBox({
    infoBox(
      "Selected Countries",
      nrow(filtered_data_university()),
      icon = icon("globe"),
      color = "light-blue"
    )
  })
  # Create the University Enrollment info box2
  output$boxUni2 <- renderInfoBox({
    # Check if a specific country has been selected
    if (!is.null(input$countrySelectUniversity) && input$countrySelectUniversity != "") {
      selected_country <- input$countrySelectUniversity
      # Get the region of the selected country
      country_region <- unique(GDP_UniversityEnrollment[GDP_UniversityEnrollment$`Country Name` == selected_country,]$Region)
      
      # Calculate the average GDP and Homicide rate for the region
      average_gdp <- mean(GDP_UniversityEnrollment[GDP_UniversityEnrollment$Region == country_region, ]$`GDP Per Capita ($)`)
      average_university <- mean(GDP_UniversityEnrollment[GDP_UniversityEnrollment$Region == country_region, ]$`University Enrollment (% Gross)`)
      
      infoBox(
        "Data Digest",
        HTML(paste(
          "<div style='font-size: 15px;'>",
          "This country is in region: ", country_region,
          "<br>",
          paste("Average GDP Per capita for countries in this region is: $", round(average_gdp), sep = ""),
          "<br>",
          paste("Average University Enrollment rate for countries in this region is: ", round(average_university), "%", sep=""),
          "</div>"
        )),
        icon = icon("search"),
        color = "olive"
      )
    } else {
      selected_regions <- input$regions
      
      if (is.null(selected_regions) || length(selected_regions) == 0) {
        infoBox(
          "Data Digest",
          "Select the countries to see the information.",
          icon = icon("search"),
          color = "olive"
        )
      } else if ("All" %in% selected_regions) {
        # Calculate average for all countries
        average_gdp <- mean(GDP_UniversityEnrollment$`GDP Per Capita ($)`)
        average_university <- mean(GDP_UniversityEnrollment$`University Enrollment (% Gross)`)
        
        infoBox(
          "Data Digest",
          HTML(paste(
            "<div style='font-size: 15px;'>",
            paste("Average GDP Per capita for these countries is: $", round(average_gdp), sep=""),
            "<br>",
            paste("Average University Enrollment rate for these countries is: ", round(average_university), "%", sep=""),
            "</div>"
          )),
          icon = icon("search"),
          color = "olive"
        )
      } else if (length(selected_regions) == 1) {
        # Calculate average for a single region
        selected_region <- selected_regions[1]
        average_gdp <- mean(GDP_UniversityEnrollment[GDP_UniversityEnrollment$Region == selected_region, ]$`GDP Per Capita ($)`)
        average_university <- mean(GDP_UniversityEnrollment[GDP_UniversityEnrollment$Region == selected_region, ]$`University Enrollment (% Gross)`)
        
        infoBox(
          "Data Digest",
          HTML(paste(
            "<div style='font-size: 15px;'>",
            paste("Average GDP Per capita for countries in this region is: $", round(average_gdp), sep=""),
            "<br>",
            paste("Average University Enrollment for countries in this region is: ", round(average_university), "%", sep=""),
            "</div>"
          )),
          icon = icon("search"),
          color = "olive"
        )
      } else {
        selected_regions <- selected_regions[selected_regions != "All"]
        average_gdp <- mean(GDP_UniversityEnrollment[GDP_UniversityEnrollment$Region %in% selected_regions, ]$`GDP Per Capita ($)`)
        average_university <- mean(GDP_UniversityEnrollment[GDP_UniversityEnrollment$Region %in% selected_regions, ]$`University Enrollment (% Gross)`)
        
        infoBox(
          "Data Digest",
          HTML(paste(
            "<div style='font-size: 15px;'>",
            paste("Average GDP Per capita for countries in these regions is: $", round(average_gdp), sep=""),
            "<br>",
            paste("Average University Enrollment rate for countries in these regions is: ", round(average_university), "%", sep=""),
            "</div>"
          )),
          icon = icon("search"),
          color = "olive"
        )
      }
    }
  })
  
 
  
  # Create the Unemployment info box1
  output$boxUnemployment1 <- renderInfoBox({
    infoBox(
      "Selected Countries",
      nrow(filtered_data_unemployment()),
      icon = icon("globe"),
      color = "light-blue"
    )
  })
  # Create the Unemployment Rate info box2
  output$boxUnemployment2 <- renderInfoBox({
    # Check if a specific country has been selected
    if (!is.null(input$countrySelectUnemployment) && input$countrySelectUnemployment != "") {
      selected_country <- input$countrySelectUnemployment
      # Get the region of the selected country
      country_region <- unique(GDP_Unemployment[GDP_Unemployment$`Country Name` == selected_country,]$Region)
      
      # Calculate the average GDP and unemployment for the region
      average_gdp <- mean(GDP_Unemployment[GDP_Unemployment$Region == country_region, ]$`GDP Per Capita ($)`)
      average_unemployment <- mean(GDP_Unemployment[GDP_Unemployment$Region == country_region, ]$`Unemployment (%)`)
      
      infoBox(
        "Data Digest",
        HTML(paste(
          "<div style='font-size: 15px;'>",
          "This country is in region: ", country_region,
          "<br>",
          paste("Average GDP Per capita for countries in this region is: $", round(average_gdp), sep=""),
          "<br>",
          paste("Average unemployment for countries in this region is: ", round(average_unemployment, 2),"%", sep=""),
          "</div>"
        )),
        icon = icon("search"),
        color = "olive"
      )
    } else {
      selected_regions <- input$regions
      
      if (is.null(selected_regions) || length(selected_regions) == 0) {
        infoBox(
          "Data Digest",
          "Select the countries to see the information.",
          icon = icon("search"),
          color = "olive"
        )
      } else if ("All" %in% selected_regions) {
        # Calculate average for all countries
        average_gdp <- mean(GDP_Unemployment$`GDP Per Capita ($)`)
        average_unemployment <- mean(GDP_Unemployment$`Unemployment (%)`)
        
        infoBox(
          "Data Digest",
          HTML(paste(
            "<div style='font-size: 15px;'>",
            paste("Average GDP Per capita for these countries is: $", round(average_gdp), sep=""),
            "<br>",
            paste("Average unemployment for these countries is: ", round(average_unemployment, 2),"%", sep=""),
            "</div>"
          )),
          icon = icon("search"),
          color = "olive"
        )
      } else if (length(selected_regions) == 1) {
        # Calculate average for a single region
        selected_region <- selected_regions[1]
        average_gdp <- mean(GDP_Unemployment[GDP_Unemployment$Region == selected_region, ]$`GDP Per Capita ($)`)
        average_unemployment <- mean(GDP_Unemployment[GDP_Unemployment$Region == selected_region, ]$`Unemployment (%)`)
        
        infoBox(
          "Data Digest",
          HTML(paste(
            "<div style='font-size: 15px;'>",
            paste("Average GDP Per capita for countries in this region is: $", round(average_gdp), sep=""),
            "<br>",
            paste("Average unemployment for countries in this region is: ", round(average_unemployment, 2),"%", sep=""),
            "</div>"
          )),
          icon = icon("search"),
          color = "olive"
        )
      } else {
        selected_regions <- selected_regions[selected_regions != "All"]
        average_gdp <- mean(GDP_Unemployment[GDP_Unemployment$Region %in% selected_regions, ]$`GDP Per Capita ($)`)
        average_unemployment <- mean(GDP_Unemployment[GDP_Unemployment$Region %in% selected_regions, ]$`Unemployment (%)`)
        
        infoBox(
          "Data Digest",
          HTML(paste(
            "<div style='font-size: 15px;'>",
            paste("Average GDP Per capita for countries in these regions is: $", round(average_gdp), sep=""),
            "<br>",
            paste("Average unemployment for countries in these regions is: ", round(average_unemployment, 2), "%", sep=""),
            "</div>"
          )),
          icon = icon("search"),
          color = "olive"
        )
      }
    }
  })
  
  # Create the Press Freedom Index info box1
  output$boxPress1 <- renderInfoBox({
    infoBox(
      "Selected Countries",
      nrow(filtered_data_press()),
      icon = icon("globe"),
      color = "light-blue"
    )
  })
  # Create the Press Freedom Index Rate info box2
  output$boxPress2 <- renderInfoBox({
    # Check if a specific country has been selected
    if (!is.null(input$countrySelectPress) && input$countrySelectPress != "") {
      selected_country <- input$countrySelectPress
      # Get the region of the selected country
      country_region <- unique(GDP_PressFreedom[GDP_PressFreedom$`Country Name` == selected_country,]$Region)
      
      # Calculate the average GDP and press freedom index for the region
      average_gdp <- mean(GDP_PressFreedom[GDP_PressFreedom$Region == country_region, ]$`GDP Per Capita ($)`)
      average_press <- mean(GDP_PressFreedom[GDP_PressFreedom$Region == country_region, ]$`Press Freedom Index`)
      
      infoBox(
        "Data Digest",
        HTML(paste(
          "<div style='font-size: 15px;'>",
          "This country is in region: ", country_region,
          "<br>",
          paste("Average GDP Per capita for countries in this region is: $", round(average_gdp), sep=""),
          "<br>",
          "Average press freedom index for countries in this region is:", round(average_press, 2),
          "</div>"
        )),
        icon = icon("search"),
        color = "olive"
      )
    } else {
      selected_regions <- input$regions
      
      if (is.null(selected_regions) || length(selected_regions) == 0) {
        infoBox(
          "Data Digest",
          "Select the countries to see the information.",
          icon = icon("search"),
          color = "olive"
        )
      } else if ("All" %in% selected_regions) {
        # Calculate average for all countries
        average_gdp <- mean(GDP_PressFreedom$`GDP Per Capita ($)`)
        average_press <- mean(GDP_PressFreedom$`Press Freedom Index`)
        
        infoBox(
          "Data Digest",
          HTML(paste(
            "<div style='font-size: 15px;'>",
            paste("Average GDP Per capita for these countries is: $", round(average_gdp), sep=""),
            "<br>",
            "Average press freedom index  for these countries is:", round(average_press, 2),
            "</div>"
          )),
          icon = icon("search"),
          color = "olive"
        )
      } else if (length(selected_regions) == 1) {
        # Calculate average for a single region
        selected_region <- selected_regions[1]
        average_gdp <- mean(GDP_PressFreedom[GDP_PressFreedom$Region == selected_region, ]$`GDP Per Capita ($)`)
        average_press <- mean(GDP_PressFreedom[GDP_PressFreedom$Region == selected_region, ]$`Press Freedom Index`)
        
        infoBox(
          "Data Digest",
          HTML(paste(
            "<div style='font-size: 15px;'>",
            paste("Average GDP Per capita for countries in this region is: $", round(average_gdp), sep=""),
            "<br>",
            "Average press freedom index for countries in this region is:", round(average_press, 2),
            "</div>"
          )),
          icon = icon("search"),
          color = "olive"
        )
      } else {
        selected_regions <- selected_regions[selected_regions != "All"]
        average_gdp <- mean(GDP_PressFreedom[GDP_PressFreedom$Region %in% selected_regions, ]$`GDP Per Capita ($)`)
        average_press <- mean(GDP_PressFreedom[GDP_PressFreedom$Region %in% selected_regions, ]$`Press Freedom Index`)
        
        infoBox(
          "Data Digest",
          HTML(paste(
            "<div style='font-size: 15px;'>",
            paste("Average GDP Per capita for countries in these regions is: $", round(average_gdp), sep=""),
            "<br>",
            "Average press freedom index  for countries in these regions is:", round(average_press, 2),
            "</div>"
          )),
          icon = icon("search"),
          color = "olive"
        )
      }
    }
  })
  
  
}

# Run the app
shinyApp(ui = ui, server = server) 