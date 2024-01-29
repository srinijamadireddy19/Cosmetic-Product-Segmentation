library(shiny)
library(shinydashboard)
library(dplyr)
library(ggplot2)
library(packcircles)
library(RColorBrewer)
library(viridis)
library(plotly)
library(scales)

# Sample data
dat3 <- read.csv('gender.csv')
dat <- read.csv('ad.csv')
dat2 <- read.csv('final.csv')

ui <- dashboardPage(
  dashboardHeader(
    title = "Cosmetics Analytics",
    titleWidth = 300, # Set the title width
    tags$li(
      class = "dropdown",
      tags$style(HTML("
        .main-header .logo {width: 300px;}
        .main-header .navbar {width: 100%;} 
        .main-content p { font-size: 20px; }
      "))
    )),
  
  dashboardSidebar(
    sidebarMenu(
      menuItem("Home", tabName = "home", icon = icon("home")),
      menuItem("Segment", tabName = "segment", icon = icon("chart-line")),
      menuItem("About", tabName = "about", icon = icon("info"))
    )),
  
  dashboardBody(
    tags$style(HTML("
      .custom-paragraph p {
        font-size: 16px; /* Change the font size to your desired value */
        color: #333; /* Change the color to your desired value */
      }")),
    tabItems(
      tabItem(
        tabName = "home",
        h2("Cosmetic Product Segmentation for Luxury Brands "),
        fluidRow(
          valueBoxOutput("totalProducts"),
          valueBoxOutput("averagePrice")
        ),
        fluidRow(
          valueBoxOutput('chemicals'),
          valueBoxOutput("highestRank")
        ),
        h4("Project Description"),
        p("The Cosmetic Product Segmentation Dashboard leverages data analytics to uncover consumer preferences, analyzing gender-based, age-specific, and skin type-related patterns. It provides insights into top brands, chemical usage, and overall product distribution, enhancing cosmetic market understanding.",class='custom-paragraph')
      ),
      tabItem(
        tabName = "segment",
        h2("Product Segmentation Analysis"),
        tabsetPanel(
          tabPanel("Gender Purchase", 
                   plotlyOutput("Purchase", width = "50%", height = "500px")),
          
          tabPanel("Ages", 
                   plotlyOutput("Ages", width = "70%", height = "500px")),
          
          tabPanel("Skin Type", 
                   plotlyOutput("skin_dist", width = "75%", height = "500px")),
          
          tabPanel("Top Brand", 
                   plotlyOutput("brand", width = "75%", height = "500px")),
          
          tabPanel("Chemicals", 
                   plotlyOutput("chemical_counts", width = "75%", height = "500px")),
          
          tabPanel("Top Chemical", 
                   plotlyOutput("top_chems", width = "75%", height = "600px"))
        )),
      tabItem(
        tabName = "about",
        h2("About"),
        h3('Group Number: 15'),
        h3('Group Members: '),
        p('1. M. Srinija - 2211CS030103'),
        p('2. N. Pravalika - 2211CS030127'),
        p('3. Srinethri Shinde - 2211CS030153'),
        p('4. V. Mamatha - 2211CS030168')
        
      ))
  ))

server <- function(input, output, session) {
  output$totalProducts <- renderValueBox({
    valueBox(
      value = nrow(dat)+nrow(dat2),
      subtitle = "Total Products",
      icon =  icon("product-hunt")
    )
  })
  output$chemicals <- renderValueBox({
    valueBox(
    value =paste('2500'),
    subtitle = 'unique chemical ingredients',
    icon = icon("flask")
    )
  })
  output$averagePrice <- renderValueBox({
    valueBox(
      value = round(mean(dat2$Price), 2),
      subtitle = "Average Price",
      icon = icon("dollar-sign")
    )
  })
  
  output$highestRank <- renderValueBox({
    valueBox(
      
      value = max(dat2$Rank),
      subtitle = "Highest Rank",
      icon = icon("sort-numeric-up")
    )
  })
  
  output$brand <- renderPlotly({
    top_brand <- data.frame(table(dat$BrandName)) %>% arrange(desc(Freq)) %>% head(10)
    a<-ggplot(top_brand,aes(x=reorder(Var1,-Freq),y=Freq,fill=Var1, text =paste(Var1,':\n',Freq))) + 
      geom_bar(stat = 'summary',position = position_dodge(width = 0.8),width = 0.6) +
      scale_fill_manual(values = brewer.pal(10, 'Paired'))+
      theme_minimal() +
      labs(title = 'Most Popular Cosmetic Brands',
           x='Top 10 Brands',
           y='Count of Products')+
      theme(plot.title =  element_text(face = 'bold',size = 20),
            axis.title.x = element_text(face = 'bold',size = 15) ,
            axis.title.y = element_text(face = 'bold',size = 15),
            legend.position = 'none',
            axis.text.x = element_text(angle = 45,hjust = 1),
            legend.text = element_text(size = 10))
    
    ggplotly(a,tooltip = 'text')
  })
  
  
  output$chemical_counts <- renderPlotly({
    avg_chem <- dat %>%
      group_by(PrimaryCategory) %>%
      summarise(total = n())
    
    constant <- 1e-6  # You can adjust this value based on your data
    avg_chem$total <- avg_chem$total + constant
    cc <- ggplot(avg_chem, aes(x = reorder(PrimaryCategory, -total), y = log10(total), fill = PrimaryCategory,text=paste(PrimaryCategory,':\n',total))) +
      geom_bar(stat = 'identity') +
      labs(title = 'Chemical Counts in Categories',
           x = 'Categories of Products',
           y = 'Counts of Chemicals') +
      scale_fill_manual(values = brewer.pal(12, 'Paired')) +
      theme(plot.title = element_text(face = 'bold', size = 20),
            axis.title.x = element_text(face = 'bold', size = 15),
            axis.title.y = element_text(face = 'bold', size = 15),
            axis.text.x = element_text(angle = 45, hjust = 1),
            legend.text = element_text(size = 10),
            legend.position = 'none') +
      scale_y_continuous(labels = scales::label_number()) 
    
    ggplotly(cc, tooltip = 'text')
    
  })
  output$top_chems <- renderPlotly({
    top_chem <- data.frame(table(dat$ChemicalName)) %>% filter(Freq!=1&Var1!='Titanium dioxide')
    colnames(top_chem) <- c('Chemical','Freq')
    top_chem$Chemical <- as.character(top_chem$Chemical)
    top_chem$Chemical[15] <- 'Vitamin A\n palmitate'
    top_chem$Chemical[6] <- ' Ginkgo \nbiloba \nextract'
    top_chem$Chemical[2] <- 'Butylated \nhydroxyanisole'
    top_chem$Chemical[5] <- ' Cocamide \ndiethanolamine'
    top_chem$Chemical[10] <-  'Retinyl \npalmitate'
    top_chem$Chemical <- as.factor(top_chem$Chemical)
    packing <- circleProgressiveLayout(top_chem$Freq, sizetype='area')
    
    packing$radius <- 0.99*packing$radius
    top_chem <- cbind(top_chem, packing)
    dat.gg <- circleLayoutVertices(packing, npoints=50)
    
    tc<- ggplot() + 
      geom_polygon(data = dat.gg, aes(x, y, group = id, fill=id), colour = "black", alpha = 0.6) +
      scale_fill_viridis() +
      geom_text(data = top_chem, aes(x, y, size=Freq, label = Chemical), color="black") +
      theme_void() + 
      theme(legend.position="none",
            panel.grid = element_blank(),
            axis.line = element_blank(),
            plot.title = element_text(face = 'bold', size = 20))+ 
      coord_equal() +
      labs(title = 'Top Chemicals used in Products')
    
    ggplotly(tc, tooltip = 'label')
  })
  output$Purchase <- renderPlotly({
    dat3$Purchased <- as.factor(dat3$Purchased)
    dat3$Gender <- as.factor(dat3$Gender)
    
    gen <- ggplot(dat3, aes(x = Gender, fill = Purchased,text=paste(Gender,'\n','Purchased :',Purchased))) +
      geom_bar(position = 'fill', width = 0.7, color = 'white') +
      scale_fill_manual(values = c("#66c2a5", "#fc8d62")) +  # Custom color palette
      labs(title = "Purchase Distribution by Gender",
           x = "Gender",
           y = "Proportion",
           fill = "Purchase Status") +
      theme_minimal()
    
    ggplotly(gen, tooltip = 'text')
  })
  output$Ages <- renderPlotly({
    dat3$count <- 1
    ag <- ggplot(dat3,aes(x=Age,fill=..count..)) +
      scale_fill_gradient(low = 'lightblue', high = 'blue') +
      geom_histogram(binwidth = 5, color = 'black' ) +
      labs(x='Age Groups',
           y='Count',
           title = 'Histogram Of Age')+
      theme(plot.title =  element_text(face = 'bold',size = 20),
            axis.title.x = element_text(face = 'bold',size = 15) ,
            axis.title.y = element_text(face = 'bold',size = 15),
            legend.position = 'none',
            legend.text = element_text(size = 10))
    
    ggplotly(ag,tooltip = c('fill','Age'))
  })
  output$skin_dist <- renderPlotly({
    skin_type_frequency <- dat2 %>%
      summarise(
        Combination = sum(Combination),
        Dry = sum(Dry),
        Normal = sum(Normal),
        Oily = sum(Oily),
        Sensitive = sum(Sensitive)
      )
    skin_type_frequency_long <- tidyr::gather(skin_type_frequency, key = "SkinType", value = "Frequency")
    skin_type_pie_plotly <- plot_ly(skin_type_frequency_long, labels = ~SkinType, values = ~Frequency, type = 'pie',
                                    marker = list(colors = rainbow(length(unique(skin_type_frequency_long$SkinType))),textfont = list(size = 20)))
    skin_type_pie_plotly <- skin_type_pie_plotly %>% layout(title = "Skin Type Frequency",legend=list(font=list(size=20)))
    skin_type_pie_plotly
    
  })
  
}

shinyApp(ui, server)
