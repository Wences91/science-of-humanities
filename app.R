library(shiny)
library(shinythemes)
library(dplyr)
library(ggplot2)

# Import data
df <- read.delim2('data/data.tsv', stringsAsFactors = FALSE)

# Define UI
ui <- navbarPage(
  
  # Application title
  theme = shinytheme('cosmo'),
  title='Science of Humanities',
  
  # Tab panels
  tabPanel('Trends',
           h2('Publication trends'),
           sidebarLayout(
             sidebarPanel(
               selectInput('area', 'Research area:',
                           c('Anthropology', 'Archaeology', 'Art', 'Cultural studies',
                             'Geography', 'History', 'Language and linguistics',
                             'Literature', 'Music','Paleontology', 'Philology',
                             'Philosophy', 'Translation & Interpretation')),
               sliderInput('range', 'Publication year:',
                           min = 1900, max = 2020,
                           value = c(1970,2020))
               ),
             # Show a plot of the generated distribution
             mainPanel(
               plotOutput('trendPlot', height = 600)
               )
             ),
           hr(),
           helpText(HTML('By Wenceslao Arroyo-Machado <a href="https://twitter.com/Wences91"><i class="fab fa-twitter"></i></a> & Nicolas Robinson-Garcia <a href="https://twitter.com/nrobinsongarcia"><i class="fab fa-twitter"></i></a> | <a href="https://github.com/Wences91/science-of-humanities">GitHub</a>'))),
  
  tabPanel('Comparative',
           sidebarLayout(
             sidebarPanel(
               selectInput('area1', 'Research area 1:',
                           c('Anthropology', 'Archaeology', 'Art', 'Cultural studies',
                             'Geography', 'History', 'Language and linguistics',
                             'Literature', 'Music','Paleontology', 'Philology',
                             'Philosophy', 'Translation & Interpretation')),
               selectInput('area2', 'Research area 2:',
                           c('Anthropology', 'Archaeology', 'Art', 'Cultural studies',
                             'Geography', 'History', 'Language and linguistics',
                             'Literature', 'Music','Paleontology', 'Philology',
                             'Philosophy', 'Translation & Interpretation'),
                           selected = 'Art'),
               sliderInput('ranges', 'Publication year:',
                           min = 1900, max = 2020,
                           value = c(1970,2020)),
               selectInput('type', 'Typology:',
                           c('Article', 'Book', 'Chapter', 'Conference'))
               ),
             mainPanel(
               plotOutput('comparePlot1', height = 70),
               plotOutput('comparePlot2', height = 600)
               )
             ),
           hr(),
           helpText(HTML('By Wenceslao Arroyo-Machado <a href="https://twitter.com/Wences91"><i class="fab fa-twitter"></i></a> & Nicolas Robinson-Garcia <a href="https://twitter.com/nrobinsongarcia"><i class="fab fa-twitter"></i></a> | <a href="https://github.com/Wences91/science-of-humanities">GitHub</a>'))),
  
  tabPanel('Data',
           sidebarLayout(
             sidebarPanel(
               selectInput('area_d', 'Research area:',
                           c('Anthropology', 'Archaeology', 'Art', 'Cultural studies',
                             'Geography', 'History', 'Language and linguistics',
                             'Literature', 'Music','Paleontology', 'Philology',
                             'Philosophy', 'Translation & Interpretation'),
                           multiple = TRUE, selected=c('Anthropology', 'Archaeology', 'Art', 'Cultural studies',
                                                       'Geography', 'History', 'Language and linguistics',
                                                       'Literature', 'Music','Paleontology', 'Philology',
                                                       'Philosophy', 'Translation & Interpretation')),
               sliderInput('range_d', 'Publication year:',
                           min = 1900, max = 2020,
                           value = c(1970,2020)),
               downloadButton('downloadData', 'Download all data')
               ),
             mainPanel(
               column(12,
                      dataTableOutput('table')
                      ))
             ),
           hr(),
           helpText(HTML('By Wenceslao Arroyo-Machado <a href="https://twitter.com/Wences91"><i class="fab fa-twitter"></i></a> & Nicolas Robinson-Garcia <a href="https://twitter.com/nrobinsongarcia"><i class="fab fa-twitter"></i></a> | <a href="https://github.com/Wences91/science-of-humanities">GitHub</a>'))),

  tabPanel('About',
           h2('About'),
           fluidRow(column(width=12,
                  HTML('The visualizations and data available here are part of the work <b>Towards a Science of Humanities: How Big Data can solve the limitations of scientometrics</b>, developed as part of the project <a href="https://compare-project.eu/about/"><b>COntextual Mapping of academic Pathways Analysis for Research Evaluation (COMPARE)</b></a> – PID2020-117007RA-I00.'),
                  HTML('Preliminary results were presented in the work in progress <a href="https://doi.org/10.5281/zenodo.5793175"><b>Big Data and the birth of a Science of the Humanities</b></a> at the <i>1st International Conference on Humanities and Big Data in Ibero-America</i> organized by IberLab UGR which took place on 16-17th December, 2021 in Granada. The final results of this study will be published in a forthcoming book titled <b>Humanities and Big Data in Ibero-America</b> to be published by De Gruyter, and edited by Ana Gallego-Cuiñas and Daniel Torres-Salinas.'),
                  tags$h2('Authors'),
                  fluidRow(
                    column(width=6,
                           tags$h3('Wenceslao Arroyo-Machado'),
                           HTML('Wenceslao Arroyo Machado is PhD student in Information and Communication Technologies, supported by a FPU predoctoral fellowship at the Department of Information and Communication of the University of Granada (UGR). He holds a degree in Information and Documentation and a master’s degree in Data Science and Computer Engineering (UGR). He has worked as a researcher in the project Knowmetrics – evaluation of knowledge in the digital society funded by the BBVA Foundation and as technical support staff in Medialab UGR. His works focus on scientometrics and other areas derived related to the new paradigm of altmetrics and data science.')
                           ),
                    column(width=6,
                           tags$h3('Nicolas Robinson-Garcia'),
                           HTML('Nicolas Robinson-Garcia is a researcher in the field of bibliometrics and research evaluation. He currently enjoys a Ramón y Cajal grant at the University of Granada (Spain). He worked previously as a Marie Sklodowska-Curie Fellow at Delft Institute of Applied Mathematics, TU Delft (Netherlands), the School of Public Policy at Georgia Institute of Technology and at INGENIO (CSIC-UPV) in Spain. He holds a PhD on Social Sciences at the University of Granada. He is member of the Steering Committee of the European Summer School for Scientometrics. He is Associate Editor on Open Science and New Metrics of the journal Scientometrics and member of the editorial board of Research Evaluation and Quantitative Science Studies.')
                           )),
                  fluidRow(column(width=12,
                                  tags$div(
                                    tags$img(src = 'https://raw.githubusercontent.com/Wences91/science-of-humanities/main/images/compare_logo.png', width=150, style='padding-right:25px'),
                                    tags$img(src = 'https://raw.githubusercontent.com/Wences91/science-of-humanities/main/images/ugr_logo.png', height=50, style='padding-right:25px'),
                                    style='padding-top:75px;text-align:right')
                         ))
           )),
           hr(),
           helpText(HTML('By Wenceslao Arroyo-Machado <a href="https://twitter.com/Wences91"><i class="fab fa-twitter"></i></a> & Nicolas Robinson-Garcia <a href="https://twitter.com/nrobinsongarcia"><i class="fab fa-twitter"></i></a> | <a href="https://github.com/Wences91/science-of-humanities">GitHub</a>')))
  )

# Define server 
server <- function(input, output) {
  df_aux <- df %>%
    dplyr::group_by(year, type, area) %>%
    dplyr::summarise(pubs=n())
  
  output$trendPlot <- renderPlot({
    ggplot(data=df_aux[which(df_aux$area==input$area & df_aux$year>=input$range[1] & df_aux$year<=input$range[2]),], aes(x=year, y=pubs, fill=type))+
      geom_col()+
      facet_wrap(.~type, scales = 'free', ncol = 2)+
      scale_fill_manual(values = c('Article'='#e3072a',
                                   'Book'='#098a74',
                                   'Chapter'='#e47820',
                                   'Conference'='#5facc8'))+
      labs(x='Year', y='Publications')+
      theme_light()+
      theme(legend.position = 'none',
            strip.background=element_rect(colour='#373a3c', fill='#373a3c'),
            text=element_text(family='Arial', size=16, color='black'),
            axis.text=element_text(color='black', size=13),
            axis.ticks=element_line(color='black'),
            strip.text = element_text(size=15))
    })
  
  output$comparePlot1 <- renderPlot({
    
    df_aux2 <- df_aux[which(df_aux$area %in% c(input$area1,input$area2) & df_aux$type == input$type & df_aux$year>=input$ranges[1] & df_aux$year<=input$ranges[2]),]
    df_aux2$area <- factor(df_aux2$area, levels=c(input$area1,input$area2), ordered = TRUE)
    
    ggplot(data=df_aux2,
           aes(x='',
               y=100*pubs/sum(df_aux2$pubs),
               fill=area))+
      geom_col()+
      coord_flip()+
      scale_fill_manual(name = 'Area', values = c('#cb4047','#647ea1'))+
      labs(x='', y='Publications (%)')+
      theme_light()+
      theme(
        #axis.line=element_blank(),
        #axis.text.y=element_blank(),
        #axis.ticks=element_blank(),
        #axis.title.x=element_blank(),
        #axis.title.y=element_blank(),
        legend.position='none',
        panel.background=element_blank(),
        panel.border=element_blank(),
        panel.grid.major=element_blank(),
        panel.grid.minor=element_blank(),
        plot.background=element_blank(),
        text=element_text(family='Arial', size=16, color='black'),
        axis.text=element_text(color='black', size=13))
    })
    
    output$comparePlot2 <- renderPlot({
      
      df_aux2 <- df_aux[which(df_aux$area %in% c(input$area1,input$area2) & df_aux$type == input$type & df_aux$year>=input$ranges[1] & df_aux$year<=input$ranges[2]),]
      df_aux2$area <- factor(df_aux2$area, levels=c(input$area1,input$area2), ordered = TRUE)
      
      # draw the histogram with the specified number of bins
      ggplot(data=df_aux2, aes(x=year, y=pubs, fill=area))+
        geom_col()+
        scale_fill_manual(name = 'Area', values = c('#cb4047','#647ea1'))+
        labs(x='Year', y='Publications')+
        theme_light()+
        theme(legend.position = 'bottom',
              text=element_text(family='Arial', size=16, color='black'),
              axis.text=element_text(color='black', size=13),
              axis.ticks=element_line(color='black'))
    })
    
    output$table <- renderDataTable(df_aux[which(df_aux$area %in% input$area_d & df_aux$year>=input$range_d[1] & df_aux$year<=input$range_d[2]),])
    
    output$downloadData <- downloadHandler(
      filename = function() {
        paste('save.tsv', sep = '')
      },
      content = function(file) {
        write.table(df_aux[which(df_aux$area %in% input$area_d & df_aux$year>=input$range_d[1] & df_aux$year<=input$range_d[2]),], file, row.names = FALSE, sep='\t')
      }
    )
}

# Run the application 
shinyApp(ui = ui, server = server)
