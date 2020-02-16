###########################################
#Arquivo: server.R
#Autor: Lucas Bicalho
###########################################

## Reading data set
data_acid = read.table("data/ACIDENTES.csv", header=T, sep=";")
data_dist = read.table("data/MDCDISTRITO.csv", header=T, sep=";")
data_ruas = read.table("data/RUAS.csv", header=T, sep=";")
data_latlong = read.table("data/LATLONG.csv", header=T, sep=";")

server <- function(input, output) {
  
  ## stuffs for dashboard tab
  output$vbox1 <- renderValueBox({ 
    valueBox(
      "Accidents: ",
      nrow(data_acid),
      input$count,
      color = "orange",
      icon = icon("car"))
  })
  output$vbox2 <- renderValueBox({ 
    valueBox(
      "Dead and Injured: ",
      sum(sum(data_acid$mortos), sum(data_acid$feridos)),
      input$count,
      color = "red",
      icon = icon("plus")
    )
  })
  
  ## stuffs for data tab
  # data table output
  output$da.tab <- DT::renderDataTable(
    datatable(data_acid, extensions = 'Buttons', style = "bootstrap",
              filter = list(position = 'top', clear = T, plain = F),
              options = list(pageLength = 10, lengthMenu = c(10,25,50,100), scrollX = TRUE, dom = 'Blfrtip', scrollX = TRUE,
                            oLanguage = list(sLengthMenu = 'Showing _MENU_ records'),
                            buttons = list('copy', 'print', list(
                                          extend = 'collection',
                                          buttons = c('csv', 'excel', 'pdf'),
                                          text = 'Download')
                                      )
                        )
    )
  )
  
  ## stuffs for data explorer
  
  #for tab Dot Charts
  data_acid20 = read.table("data/ACIDENTES.csv", header=T, sep=";") [1:300,]
  
  ## read in date/time info in format 'd/m/y h:m'
  x <- c(as.character(data_acid$data))
  #mes <- month(strptime(x, "%d/%m/%Y %H:%M"))
  #1: transformar valores para continuo e degrade leg (as.numeric())
  #2: valores discretos e separar por intervalos
    # for subtab mortos
    output$plotDotMortos <- renderPlotly({
      # build graph with ggplot syntax
      y <- as.character(data_acid20$mortos)
      p1 <- ggplot(data_acid20, color = y) +
        geom_jitter(aes(data, distrito, color = y),
                    show.legend = T,
                    inherit.aes = T,
                    width = 0.0,
                    height = 0.0,
                    size = 2,
                    alpha =1) +
        ggthemes::theme_gdocs() + scale_colour_gdocs()
  
      ggplotly(p1)
    })
  
    # for subtab feridos
    output$plotDotFeridos <- renderPlotly({
      # build graph with ggplot syntax
      p2 <- ggplot(data_acid20, color = data) +
        geom_jitter(aes(feridos, distrito, color = data),
                    show.legend = T,
                    inherit.aes = T,
                    width = 0.0,
                    height = 0.0,
                    size = 2,
                    alpha =1) +
        ggthemes::theme_gdocs() + scale_colour_gdocs()
  
      ggplotly(p2)
    })
  
    
  # for tab Pie Charts
    # for subtab: pie1
    datasetInput1 <- reactive({
      subset(data_acid, data_acid[, input$variable1]>0)
    })
    
    library(data.table)
    period = ceiling(hour(strptime(data_acid$data, "%d/%m/%Y %H:%M")))
    data_acid$period = cut(period, breaks = c(0,6,12,18,24), labels = c("Dawn", "Morning", "Afternoon", "Night"))
    
    output$plotPie1 <- renderPlotly({
      subdf1 = datasetInput1()
      tab1 = table(subdf1$period)
      df_pie1 = data.frame(lbs = names(tab1), vls = as.numeric(tab1))
      plot_ly(df_pie1, labels = ~lbs, values = ~vls, type = 'pie',
              textposition = 'inside',textinfo = 'label+percent',
              insidetextfont = list(color = '#FFFFFF'),hoverinfo = 'text',text = ~paste(vls),
              marker = list(colors = colors,line = list(color = '#FFFFFF', width = 1)),showlegend = FALSE) %>%
        layout(title = 'Period of Accidents',
               xaxis = list(showgrid = F, zeroline = F, showticklabels = F),
               yaxis = list(showgrid = F, zeroline = F, showticklabels = F))
    })
    
    # for subtab: pie2
    datasetInput2 <- reactive({
      subset(data_acid, data_acid[, input$variable2]>0)
    })
    
    output$plotPie2 <- renderPlotly({
      subdf2 = datasetInput2()
      subdf2$tipo_cod_acid = gsub("CO","Collision",subdf2$tipo_acidente)
      subdf2$tipo_cod_acid = gsub("CF","Frontal Collision",subdf2$tipo_cod_acid)
      subdf2$tipo_cod_acid = gsub("AT","Trampling",subdf2$tipo_cod_acid)
      subdf2$tipo_cod_acid = gsub("CL","Side Collision",subdf2$tipo_cod_acid)
      subdf2$tipo_cod_acid = gsub("CT","Back Collision",subdf2$tipo_cod_acid)
      subdf2$tipo_cod_acid = gsub("CH","Impact",subdf2$tipo_cod_acid)
      subdf2$tipo_cod_acid = gsub("QD","Fall Occupant Inside",subdf2$tipo_cod_acid)
      subdf2$tipo_cod_acid = gsub("QF","Fall Occupant Outside",subdf2$tipo_cod_acid)
      subdf2$tipo_cod_acid = gsub("SI","No Information",subdf2$tipo_cod_acid)
      subdf2$tipo_cod_acid = gsub("QM","Motorcycle Fall",subdf2$tipo_cod_acid)
      subdf2$tipo_cod_acid = gsub("QB","Bicycle Fall",subdf2$tipo_cod_acid)
      subdf2$tipo_cod_acid = gsub("CV","Transversal Collision",subdf2$tipo_cod_acid)
      subdf2$tipo_cod_acid = gsub("CP","Rollover",subdf2$tipo_cod_acid)
      subdf2$tipo_cod_acid = gsub("TB","Tipping",subdf2$tipo_cod_acid)
      subdf2$tipo_cod_acid = gsub("AA","Animal Trampling",subdf2$tipo_cod_acid)
      subdf2$tipo_cod_acid = gsub("QV","Vehicle Fall",subdf2$tipo_cod_acid)
      subdf2$tipo_cod_acid = gsub("OU","Others",subdf2$tipo_cod_acid)
      tab2 = table(subdf2$tipo_cod_acid)
      df_pie2 = data.frame(lbs2 = names(tab2), vls2 = as.numeric(tab2))
      
      plot_ly(df_pie2, labels = ~lbs2, values = ~vls2, type = 'pie',
              textposition = 'inside', textinfo = 'label+percent',
              insidetextfont = list(color = '#FFFFFF'), hoverinfo = 'text', text = ~paste(vls2),
              marker = list(colors = colors,line = list(color = '#FFFFFF', width = 1)), showlegend = FALSE) %>%
        layout(title = 'Types of Accidents',
               xaxis = list(showgrid = F, zeroline = F, showticklabels = F),
               yaxis = list(showgrid = F, zeroline = F, showticklabels = F))
      })
    

  # for tab Bar Charts
    # for subtab: bar1
    datasetInput3 <- reactive({
    })
    datasetInput4 <- reactive({
    })
    #laço de gsub em data_dist
     for (i in 1:length(data_dist$Cod_distri)) {
       data_acid$distrito = gsub(paste("^", data_dist$Cod_distri[i], "$", sep=""),data_dist$Nome_distr[i],data_acid$distrito)
     }
    
    output$plotBar1 <- renderPlotly({
      subdf3 =  subset(data_acid, data_acid[, input$variable3]>0  & data_acid[, 'period']==input$variable4)
      tab = table(subdf3$distrito, subdf3$mortos)
      cont = as.numeric(colnames(tab))
      res = apply(tab, 1, function(linha) sum(linha * cont))
      df_bar1 = data.frame(lbs = names(res), vls = as.numeric(res))
      y = list(title='deaths')
      x = list(title = 'districts', showticklabels = FALSE)
      p4<- plot_ly(df_bar1, x=~lbs, y=~vls, width = 1000, height = 370,
                   name = "Quantidade total de mortos por região",
                   type = "bar")
      p4 %>% layout(xaxis = x, yaxis= y)
    })
    
    # for subtab: bar2
    output$plotBar2 <- renderPlotly({
      tab = data.frame(vehicle = c("car", "motocycle", "bus", "truck", "bicycle"),
                       frequency = c(sum(data_acid$automovel), sum(data_acid$moto), sum(data_acid$onibus), sum(data_acid$caminhao), 
                                sum(data_acid$bicicleta)))
      p5<- ggplot(tab, aes(x=vehicle, y=frequency))+
        geom_bar(width = 1, stat = "identity")+
        ggthemes::theme_gdocs() + scale_colour_gdocs()
      ggplotly(p5)
    })
    
  # for tab Animated Plots
    # for subtab: plotanim1
    library(data.table)
    df = data.frame(semana = ceiling(mday(strptime(data_acid$data, "%d/%m/%Y %H:%M"))/7),
                     mes = month(strptime(data_acid$data, "%d/%m/%Y %H:%M")),
                     distrito = data_acid$distrito, feridos = data_acid$feridos)
    
    output$plotanim1 <- renderPlotly({
      countryByArea <- read.table(
        "https://bit.ly/2h6vscu",
        header = TRUE, stringsAsFactors = FALSE
      )
      gap <- gapminder %>%
        dplyr::left_join(countryByArea, by = "country") %>%
        transform(popDen = pop / area) %>%
        transform(country = forcats::fct_reorder(country, popDen))
      
      gap$lat = rnorm(nrow(gap))
      gap$lon = rnorm(nrow(gap))
      
      gapKey <- crosstalk::SharedData$new(gap, ~country)
      
      p1 <- plot_ly(df, y = ~distrito, x = ~semana, hoverinfo = "x") %>%
        # add_markers(alpha = 0.1, color = I("black")) %>%
        add_markers(data = df, frame = ~mes, ids = ~distrito, color = I("red")) %>%
        layout(xaxis = list(range = c(0,5)))
      
      colfun = colorRampPalette(c("red","yellow","springgreen","royalblue"))
      uniq = sort(unique(df$feridos))
      ntam = length(uniq)
      df$cores = sapply(df$feridos, function(f) colfun(ntam)[which(f == uniq)]) 
      
      p2 <- plot_ly(df, x = ~distrito, y = ~semana, size = ~feridos, colors=colfun(ntam),
                    text = ~feridos, hoverinfo = "text") %>%
            add_markers(data = df, frame = ~mes, color = as.character(df$feridos))
      
        # add_markers(data = df, frame = ~mes, ids = ~distrito, color = I("red"))
      #gsub do 8 para 13
      # table(df$mes)
      # table(df$semana)
      
      subplot(p1, p2, nrows = 1, widths = c(0.3, 0.7), titleX = TRUE) %>%
        hide_legend() %>%
        animation_opts(1000, redraw = FALSE) %>%
        layout(hovermode = "y", margin = list(l = 100)) %>%
        highlight("plotly_selected", color = "blue", opacityDim = 1, hoverinfo = "none")
    })
    # for subtab: plotanim2
    output$plotanim2 <- renderPlotly({
      library(tourr)
      library(plotly)
      
      mat <- rescale(USArrests[, 1:4])
      tour <- new_tour(mat, grand_tour(), NULL)
      
      tour_dat <- function(step_size) {
        step <- tour(step_size)
        proj <- center(mat %*% step$proj)
        data.frame(x = proj[,1], y = proj[,2], state = rownames(mat))
      }
      
      proj_dat <- function(step_size) {
        step <- tour(step_size)
        data.frame(
          x = step$proj[,1], y = step$proj[,2], state = colnames(mat)
        )
      }
      
      steps <- c(0, rep(1/15, 200))
      stepz <- cumsum(steps)
      
      # tidy version of tour data
      tour_dats <- lapply(steps, tour_dat)
      tour_datz <- Map(function(x, y) cbind(x, step = y), tour_dats, stepz)
      tour_dat <- dplyr::bind_rows(tour_datz)
      
      # tidy version of tour projection data
      proj_dats <- lapply(steps, proj_dat)
      proj_datz <- Map(function(x, y) cbind(x, step = y), proj_dats, stepz)
      proj_dat <- dplyr::bind_rows(proj_datz)
      
      
      ax <- list(
        title = "", showticklabels = FALSE,
        zeroline = FALSE, showgrid = FALSE,
        range = c(-1.1, 1.1)
      )
      
      # for nicely formatted slider labels
      options(digits = 3)
      
      tour_dat <- crosstalk::SharedData$new(tour_dat, ~state, group = "A")
      
      tour <- proj_dat %>%
        plot_ly(x = ~x, y = ~y, frame = ~step, color = I("black")) %>%
        add_segments(xend = 0, yend = 0, color = I("gray80")) %>%
        add_text(text = ~state) %>%
        add_markers(data = tour_dat, text = ~state, hoverinfo = "text") %>%
        layout(xaxis = ax, yaxis = ax)
      
      dend <- USArrests %>% 
        dist() %>% 
        hclust() %>%
        as.dendrogram() %>%
        plot_dendro(set = "A", xmin = -100, height = 900, width = 1100)
      
      USArrests$state <- rownames(USArrests)
      USArrests$abb <- setNames(state.abb, state.name)[USArrests$state]
      
      map <- plot_geo(USArrests, color = I("black")) %>% 
        add_trace(locations = ~abb, locationmode = "USA-states",
                  key = ~state, set = "A", mode="markers") %>% 
        layout(geo = list(
          scope = 'usa',
          projection = list(type = 'albers usa'),
          lakecolor = toRGB('white')
        ))
      
      subplot(tour, map, nrows = 2, margin = 0) %>%
        subplot(dend, shareY = FALSE, margin = 0) %>%
        hide_legend() %>%
        animation_opts(33, redraw = FALSE) %>%
        highlight(persistent = TRUE, dynamic = TRUE)
    })

  ## stuffs for heatmap
    # output$plotHM <- renderPlotly({
    #   p7 <- plot_ly(z = volcano, type = "heatmap")
    #   
    #   ggplotly(p7)
    # })
    # output$plotHM <- renderPlotly({
    #   m <- matrix(rnorm(nrow(data_latlong)*nrow(data_latlong)), nrow = nrow(data_latlong), ncol = nrow(data_latlong))
    #   p7 <- plot_ly(
    #     x = c("a", "b", "c"), y = c("d", "e", "f"),
    #     z = m, type = "heatmap"
    #   )
    #   
    # })
    
    getPage<-function() {
      return(includeHTML("mapa_conv_data.html"))
    }
    output$inc<-renderUI({getPage()})
  
}