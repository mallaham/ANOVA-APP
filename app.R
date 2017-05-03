#################
# ANOVA Shiny App using R and HTML. 
# Visualizations using gpplot2 and plotly.
# By: Mowafak Allaham
# Date: 04/20/2017
# Revised on: 05/02/2017
#####################
# Installing libraries/dependencies:
# shiny:
# plotly:
# afex:
# MASS:
# mvtnorm:
##########################################
############
# Libraries
############
##########################################
library(shiny)
library(plotly)
library(MASS)
library(afex)
##########################################
##################
# Simulating data
#################
##########################################
# Global function to generate a simulated dataset
getDist <- function(N,dCor,m1,m2,m3,m4,m5,m6,s1,s2,s3,s4,s5,s6){
  pType =dCor
  Means.V<- c(m1,m2,m3,m4,m5,m6)
  #Generating covarience matrix to correlate user responses of their feelings of empathy towards each robot type
  CovMatrix.V <- matrix(c(s1,rep(pType,5),
                          pType,s2,rep(pType,4),
                          rep(pType,2),s3,rep(pType,3),
                          rep(pType,3),s4,rep(pType,2),
                          rep(pType,4),s5,pType,
                          rep(pType,5),s6),6,6)
  
  #Generating a distribution based on the correlated responses that were generated in the previous step (CovMatrix)
  roboData<-mvrnorm(n=N, mu=Means.V,Sigma=CovMatrix.V, empirical=TRUE)
  #Transpose the dataframe so that each row represents only 1 subject
  roboDataDF<-as.data.frame(as.table(t(roboData))) 
  colnames(roboDataDF)<- c('Robot','Uncanny','Empathy')
  roboDataDF$Uncanny <- c(rep(c("Low","Medium","High"),N))
  roboDataDF$Robot <- c(rep("Mechanical",3), rep("Humanoid", 3))
  roboDataDF$Empathy[roboDataDF$Empathy <0 ] <- 0
  roboDataDF$Empathy[roboDataDF$Empathy >7 ] <- 7
  return(roboDataDF)
}

##########################################
##############
# ANOVA app UI
##############
##########################################
ui <- fluidPage(
  #titlePanel("Understanding ANOVA Through Visualization"),
  tags$br(),
  sidebarLayout( 
    sidebarPanel( width = 3,
      #coloring the slides based on robot type.
      tags$style(HTML(".js-irs-0 .irs-single, .js-irs-0 .irs-bar-edge, .js-irs-0 .irs-bar {background: black}")),
      tags$style(HTML(".js-irs-1 .irs-single, .js-irs-1 .irs-bar-edge, .js-irs-1 .irs-bar {background: blue}")),
      tags$style(HTML(".js-irs-2 .irs-single, .js-irs-2 .irs-bar-edge, .js-irs-2 .irs-bar {background: orange}")),
      tags$style(HTML(".js-irs-3 .irs-single, .js-irs-3 .irs-bar-edge, .js-irs-3 .irs-bar {background: orange}")),
      tags$style(HTML(".js-irs-4 .irs-single, .js-irs-4 .irs-bar-edge, .js-irs-4 .irs-bar {background: orange}")),
      tags$style(HTML(".js-irs-5 .irs-single, .js-irs-5 .irs-bar-edge, .js-irs-5 .irs-bar {background: orange}")),
      tags$style(HTML(".js-irs-6 .irs-single, .js-irs-6 .irs-bar-edge, .js-irs-6 .irs-bar {background: orange}")),
      tags$style(HTML(".js-irs-7 .irs-single, .js-irs-7 .irs-bar-edge, .js-irs-7 .irs-bar {background: orange}")),
      tags$style(HTML(".js-irs-8 .irs-single, .js-irs-8 .irs-bar-edge, .js-irs-8 .irs-bar {background: purple}")),
      tags$style(HTML(".js-irs-9 .irs-single, .js-irs-9 .irs-bar-edge, .js-irs-9 .irs-bar {background: purple}")),
      tags$style(HTML(".js-irs-10 .irs-single, .js-irs-10 .irs-bar-edge, .js-irs-10 .irs-bar {background: purple}")),
      tags$style(HTML(".js-irs-11 .irs-single, .js-irs-11 .irs-bar-edge, .js-irs-11 .irs-bar {background: purple}")),
      tags$style(HTML(".js-irs-12 .irs-single, .js-irs-12 .irs-bar-edge, .js-irs-12 .irs-bar {background: purple}")),
      tags$style(HTML(".js-irs-13 .irs-single, .js-irs-13 .irs-bar-edge, .js-irs-13 .irs-bar {background: purple}")),
      tags$style(HTML(".js-irs-14 .irs-single, .js-irs-14 .irs-bar-edge, .js-irs-14 .irs-bar {background: purple}")),
      #Initating the slide bar panel
      sliderInput(inputId = "slideN", 
                  "Number of observations:", 
                  min = 10,
                  max = 50, 
                  value = 25),
      sliderInput(inputId = "cor", 
                  "Correlations", 
                  min = 0,
                  max = 1,
                  step = 0.1,
                  value = .2),
      tags$hr(style="background-color: #2E2C2C;height: 1px;"),
      "Low Uncanny:",
      sliderInput(inputId= "slideM1",
                  "Mean",
                  min=1,
                  max=7,
                  value=3),
      sliderInput(inputId= "slideSD1",
                  "Standard Deviation",
                  min=1,
                  max=7,
                  value=3),
      "Medium Uncanny:",
      sliderInput(inputId= "slideM2",
                  "Mean",
                  min=1,
                  max=7,
                  value=3),
      sliderInput(inputId= "slideSD2",
                  "Standard Deviation",
                  min=1,
                  max=7,
                  value=3),
      "High Uncanny:",
      sliderInput(inputId= "slideM3",
                  "Mean",
                  min=1,
                  max=7,
                  value=3),
      sliderInput(inputId= "slideSD3",
                  "Standard Deviation",
                  min=1,
                  max=7,
                  value=3),
      tags$hr(style="background-color: #2E2C2C;height: 1px;"),
      titlePanel("Humanoid Robot"),
      "Low Uncanny",
      sliderInput(inputId= "slideM4",
                  "Average",
                  min=1,
                  max=7,
                  value=6),
      sliderInput(inputId= "slideSD4",
                  "Standard Deviation",
                  min=1,
                  max=7,
                  value=6),
      "Medium Uncanny",
      sliderInput(inputId= "slideM5",
                  "Average",
                  min=1,
                  max=7,
                  value=3),
      sliderInput(inputId= "slideSD5",
                  "Standard Deviation",
                  min=1,
                  max=7,
                  value=3),
      "High Uncanny",
      sliderInput(inputId= "slideM6",
                  "Average",
                  min=1,
                  max=7,
                  value=2),
      sliderInput(inputId= "slideSD6",
                  "Standard Deviation",
                  min=1,
                  max=7,
                  value=2)
    ),
    # The main panel, is the space where the plots will be dispalyed.
    # fixedRow, fixates a plot, or set of plots, in a specific order.
    mainPanel( 
      fixedRow(
        tags$br(),
        column(plotOutput("dist"),height="200px",width =12),
        tags$hr(style="background-color: #2E2C2C;height: 1px;")
      ),
      #creating a space of box plots
      fixedRow(
        tags$br(),
        tags$hr(style="background-color: #2E2C2C;height: 1px;"),
        column(plotOutput("box1"),height="500px",width=6),
        column(plotOutput("box2"),height="500px",width=6)
        
      ),
      #creating a space for pie charts.
      #I used plotly to plot the pie charts. Make sure to use plotlyOuput to render plotly graphs in your Shiny App.
      fixedRow(
        tags$br(),
        tags$hr(style="background-color: #2E2C2C;height: 1px;"),
        tags$output(HTML("<center><p style= 'font-style: normal;font-size: 40px;color: black'>Explained and Unexplained Variance</p></center>")),
        tags$output(HTML("<center><p style='color:black;font-style: normal;font-size: 15px;'><i>Explained variance is represented in 3 colors (orange, purple, and blue). <br>Unexplained variance is represented in red</i></p></center>")),
        #tags$style("p {font-style: normal;font-size: 40px;color: black}"),
        tags$hr(style="background-color: #2E2C2C;height: 1px;"),
        column(4,plotlyOutput("between",height = "400px")),
        column(4,plotlyOutput("rm",height = "400px")),
        column(4,plotlyOutput("mixed",height = "400px"))
        ),
      #giving myself some credit ;-)
      tags$output(HTML("<center><p style='color:black;font-style: normal;font-size: 12px;'><i>Note: You can press on the legend color to add/remove one or more components that form the pie</i> </p></center>")),
      tags$hr(style="background-color: #2E2C2C;height: 1px;"),
      tags$output(HTML("<center><p style='color:black;font-style: normal;font-size: 15px;'>This app is developed by <a href ='https://twitter.com/mowafakallaham'>Mowafak Allaham</a>, a PhD student at the University of Illinois at Chicago</p><br>April, 2017</center>"))
    )
  )
  )

##########################################
##############
# Server
##############
##########################################
server <- function(input, output) {
  #The rgb codes below correspond to the following color names purple, blue, orange, red, red, red,red.
  #Note: only for mixed model, I ordered the colors in a different way so it can match the between and RM ANOVA.
  colors <- c('rgb(160, 143, 223)', 'rgb(30, 144, 255)',  'rgb(255, 183, 0)','rgb(255, 0, 0)','rgb(255, 0, 0)','rgb(255, 0, 0)','rgb(255, 0, 0)')
  
  #Another important component is "reactive" function.
  #It mainly allows you to exchange variables between functions within the server environment.
  #The code below calls getDist (that is repsonsible to generate the distribution) each time the user
  #changes the sliders values.
  getData<- reactive({
    dataDF<-getDist(input$slideN,input$cor,input$slideM1,input$slideM2,input$slideM3,input$slideM4,input$slideM5,input$slideM6,
                    input$slideSD1,input$slideSD2,input$slideSD3,input$slideSD4,input$slideSD5,input$slideSD6)
  })
  
  #Plotting density plots
  output$dist <- renderPlot({
    distData<-getData() #the parentheses are required to fetch the value of dataDF in getData, because getData is of class reactive. 
    ggplot(distData, aes(x=Empathy, fill=Uncanny)) +
      geom_density(alpha=0.4)+
      theme(text = element_text(size=20))+
      scale_x_continuous(breaks = seq(0, 7, 1))+
      scale_y_continuous(name = "Density")+
      #geom_histogram(binwidth=.5,  position="dodge")+
      facet_grid(.~Robot)+theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank())
  })
  
  #Plotting the between subject ANOVA pie
  #You'll note something from this point onwards that the structure of my "subject" column tend to change
  #depending on the type of ANOVA I'm interested in analyzing.
  #Between subjects design (fully between):
  #----------------------
  #The error is only at the group level. Meaning that we careless about the variation between subjects who belong
  #to the same experimental group. Accordingly, we want to parse out within group variance and count it among the unexplained variance
  output$between <- renderPlotly({
    distData<-getData()
    distData$Subject<-  seq(1:length(distData$Robot))
    Model.aov.1<-aov_car(Empathy ~ Uncanny*Robot + Error(Subject), data=distData,anova_table = list(es = "pes"), return="univariate")
    SS<-Model.aov.1$`Sum Sq` #fetching the Sum of Squares from the ANOVA table
    SS<-SS[2:length(SS)]
    SS_label <-c('Uncanny - Explained','Robot - Explained','Uncanny*Robot - Explained','Residual - Unexplained')
    viz <-data.frame(
      SS_viz = SS,
      Label = SS_label
    )
    #plotting the pie
    plot_ly(viz, labels = ~Label, values =viz$SS_viz, type = 'pie',
            marker = list(colors = colors,line = list(color = '#FFFFFF', width = 1) )) %>%
      layout(title = 'Between Subjects',
             xaxis = list(showgrid = FALSE, zeroline = FALSE, showticklabels = FALSE),
             yaxis = list(showgrid = FALSE, zeroline = FALSE, showticklabels = FALSE),
             legend = list(orientation='h'))
  })
  
  #Repeated Measures ANOVA (RM- fully within):
  #-------------------------------------------
  #In this design, the error term is Subject/ Uncanny * Robot. In this case we care about within subject variance as
  #each subject provided responses for all 6 conditions (2 robot types x 3 uncanny levels)
  #Note the change in the structure of the subject column when compared to the fully between design.
  output$rm <- renderPlotly({
    distData<-getData()
    distData$Subject<-rep(1:input$slideN,each=6)
    Model.aov.1<-aov_car(Empathy ~ Uncanny*Robot + Error(Subject/Uncanny*Robot), data=distData,return="univariate")
    SS<-Model.aov.1$univariate.tests[c(seq(2,4))] # accessing Sum of Squares by index from the ANOVA table
    ERR<-Model.aov.1$univariate.tests[c(9,10,11,12)] #accessing the Error terms by index
    anovaVis <- c(SS,ERR)
    SS_label <-c('Uncanny - Explained','Robot - Explained','Uncanny*Robot - Explained', #missing 'Residual - Unexplained'
                 'Uncanny_Error','Robot_Error','Uncanny*Robot_Error','Subject_Error - Unexplained')
    viz <-data.frame(
      SS_viz = anovaVis,
      Label = SS_label
    )
    plot_ly(viz, labels = ~Label, values =viz$SS_viz, type = 'pie',
            marker = list(colors = colors,line = list(color = '#FFFFFF', width = 1) )) %>%
      layout(title = 'Repeated Measure',
             xaxis = list(showgrid = FALSE, zeroline = FALSE, showticklabels = FALSE),
             yaxis = list(showgrid = FALSE, zeroline = FALSE, showticklabels = FALSE),
             legend = list(orientation='h'))
  })
  #Mixed ANOVA:
  #------------
  #The error term in this design is the within subject variance divided by the within condition variance (Uncanny).
  #Note the change in the structure of the subject column when compared to the fully between and fully within designs.
  output$mixed <- renderPlotly({
    #Change the sujects column so it reflects a mixed design
    distData<-getData()
    distData$Subject<-rep(1:(input$slideN*2),each=3)
    distData$within<-as.numeric(factor(distData$Uncanny, levels=c("Low","Medium","High"), labels=c(0,1,2)))
    distData$btwn<-as.numeric(factor(distData$Robot, levels=c("Mechanical","Humanoid"), labels=c(0,1)))
    Model.aov.1<-aov_car(Empathy ~ Uncanny*Robot + Error(Subject/Uncanny), data=distData, return="univariate")
    SS<-Model.aov.1$univariate.tests[c(seq(2,4))] #Sum_of_Squares
    ERR<-Model.aov.1$univariate.tests[c(10,11)]
    anovaVis <- c(SS,ERR)
    #colors: blue, purple, orange, red, red, red,,red
    mixed_model_colors <- c('rgb(30, 144, 255)','rgb(160, 143, 223)','rgb(255, 183, 0)','rgb(255, 0, 0)','rgb(255, 0, 0)','rgb(255, 0, 0)','rgb(255, 0, 0)')
    SS_label <-c('Robot - Explained','Uncanny - Explained','Uncanny*Robot - Explained', #missing 'Residual - Unexplained'
                 'Between Subjects Error','Residual')
    viz <-data.frame(
      SS_viz = anovaVis,
      Label = SS_label
    )
    plot_ly(viz, labels = ~Label, values =viz$SS_viz, type = 'pie',marker = list(colors = mixed_model_colors,line = list(color = '#FFFFFF', width = 1) )) %>%
      layout(title = 'Mixed',
             xaxis = list(showgrid = FALSE, zeroline = FALSE, showticklabels = FALSE),
             yaxis = list(showgrid = FALSE, zeroline = FALSE, showticklabels = FALSE),
             legend = list(orientation='h'))
  })
  
  #Visualing the data based on the uncanny level
  output$box1 <- renderPlot({
    distData <- getData()
    ggplot(distData,aes(distData$Uncanny, distData$Empathy) ) +
      geom_boxplot()+
      facet_grid(.~Robot)+ aes(fill = as.factor(distData$Uncanny))+
      theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank()) +
      theme(legend.position="none") + labs (x="Uncanny", y="Empathy")+theme(text = element_text(size=20))
  })
  #Visualing the data based on the type of robot
  output$box2 <- renderPlot({
    distData <- getData()
    ggplot(distData,aes(distData$Robot, distData$Empathy) ) +
      geom_boxplot(fill=c('orange', 'purple'))+
      theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank())+
      theme(legend.position = "none")+labs(x='Robot',y='Empathy')+theme(text = element_text(size=20))
  })
  

}

#This calls the interface and the server to launch the shiny App.
shinyApp(ui, server)



