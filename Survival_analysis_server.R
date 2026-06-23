
########################## VARIABLES #################################

#Variable of data export
DeadTableExport <- reactiveVal()

##### Survival analysis
#Remove dead animals
channelsToRemove <- reactiveVal()

#Dead animal table
deadTable <- reactiveVal()

#Data without last inactivity period
cleanData <- reactiveVal()

#Dead animal
Dead_animal <- reactiveVal()
Dead_noInactivity <- reactiveVal()

SurvivalFigures <- reactiveValues(allChannels = NULL, diedChannels = NULL, Survival = NULL,
                                  timeSurvival = NULL)
SurvivalFiguresTitles <- reactiveValues(allChannels = "Actogram per channel", 
                                        diedChannels = "Dead animals actogram", 
                                        Survival = "Survival",
                                        SurvivalPerTime = "Survival over time")

SurvivalFiguresXlabels <- reactiveValues(allChannels = "Time", Survival = "Conditions",
                                         SurvivalPerTime = "Time")

SurvivalFiguresYlabels <- reactiveValues(allChannels = "Channels", Survival = "Survival (%)",
                                         SurvivalPerTime = "Survival (%)")

SurvivalData <- reactiveValues(allChannels = NULL, diedChannels = NULL, Survival = NULL)



######################### FUNCTIONS ##########################

# RLE-based death detection function (same algorithm as standalone script)
# Detects dead animals based on longest continuous period of zero activity
detect_dead_animals_rle <- function(dt, death_inactivity_hours = 12) {

  # Get unique animal IDs
  animal_ids <- unique(dt[, id, meta = TRUE])

  dead_info <- data.table::data.table()
  alive_ids <- c()

  for (animal_id in animal_ids) {
    # Get activity data for this animal
    animal_data <- dt[id == animal_id]
    activity <- animal_data[, activity]
    time_points <- animal_data[, t]

    # Check for continuous zero activity using RLE
    is_zero <- activity == 0
    rle_result <- rle(is_zero)

    # Find the longest continuous zero period (in minutes, assuming 1-min bins)
    max_zero_length <- 0
    death_start_idx <- NA

    if (any(rle_result$values)) {
      zero_lengths <- rle_result$lengths[rle_result$values]
      max_zero_length <- max(zero_lengths)

      # Find when this longest period started
      if (max_zero_length >= death_inactivity_hours * 60) {
        cumsum_lengths <- cumsum(rle_result$lengths)
        zero_run_idx <- which(rle_result$values & rle_result$lengths == max_zero_length)[1]

        if (zero_run_idx > 1) {
          death_start_idx <- cumsum_lengths[zero_run_idx - 1] + 1
        } else {
          death_start_idx <- 1
        }
      }
    }

    max_zero_hours <- max_zero_length / 60

    # Determine if dead
    is_dead <- max_zero_hours >= death_inactivity_hours

    if (is_dead) {
      death_time <- time_points[death_start_idx]

      dead_info <- rbind(dead_info, data.table::data.table(
        id = animal_id,
        status = "DEAD",
        max_inactivity_hours = round(max_zero_hours, 2),
        death_time_seconds = death_time,
        death_start_index = death_start_idx
      ))
    } else {
      alive_ids <- c(alive_ids, animal_id)
    }
  }

  return(list(
    dead_info = dead_info,
    alive_ids = alive_ids,
    dead_ids = if(nrow(dead_info) > 0) dead_info$id else c()
  ))
}

#Create graphs and data
ActogramPerChannel <- function(){
  
  req(damData$dt)
  
  ##### Actogram per channel #####
  if(input$clean_data == TRUE & !is.null(cleanData())){
    SurvivalData$allChannels <- cleanData()
  }
  else{
    SurvivalData$allChannels <- damData$dt
  }
  
  # Convert Period to numeric seconds for R 4.5+ compatibility
  fig <- ggetho(SurvivalData$allChannels,aes(x = t, y = interaction(File,labels,region_id,order,sep=" - "),
                                             z = activity),summary_FUN = sum,summary_time_window = as.numeric(mins(60), unit = "secs")) +
    stat_tile_etho() +
    labs(title=SurvivalFiguresTitles$allChannels, y = SurvivalFiguresYlabels$allChannels)
  
  fig <- whiteBackground(fig, input$titleLetterSize, input$axisLabelSize,
                         input$axisNumbersSize, input$dataLabelSize)
  
  SurvivalFigures$allChannels <- fig
}

DiedPerChannel <- function(){
  
  req(!is.null(Dead_animal()))
  
  #Dead actogram
  if(!is.null(Dead_animal())){
    
    if(input$clean_data == TRUE){
      SurvivalData$diedChannels <- Dead_noInactivity()
    }
    else{
      SurvivalData$diedChannels <- Dead_animal()
    }
  }
  
  # Convert Period to numeric seconds for R 4.5+ compatibility
  fig <- ggetho(SurvivalData$diedChannels,aes(x = t, y = interaction(File,labels,region_id,order,sep=" - "),
                                              z = activity),summary_FUN = sum,summary_time_window = as.numeric(mins(60), unit = "secs")) +
    stat_tile_etho() +
    labs(title=SurvivalFiguresTitles$diedChannels, y = SurvivalFiguresYlabels$allChannels)
  
  fig <- whiteBackground(fig, input$titleLetterSize, input$axisLabelSize,
                         input$axisNumbersSize, input$dataLabelSize)
  
  SurvivalFigures$diedChannels <- fig
  
}

SurvivalEnd <- function(){
  
  req(damData$dt)
  
  #Survival graph
  N_all  <- aggregate(paste(id) ~ interaction(labels,File, order, sep = " -- "), data=damData$dt[,,meta=T], FUN=length)
  Survival <- rep(100,nrow(N_all))
  
  if(!is.null(Dead_animal())){
    N_died  <- aggregate(paste(id) ~ interaction(labels,File,order,sep = " -- "), data=Dead_animal()[,,meta=T], FUN=length)
    
    if(nrow(N_died)>0){
      
      for (i in 1:nrow(N_died)){
        row <- match(N_died[i,1],N_all[,1])
        if(!is.null(row)){
          Survival[row] <- (N_all[row,2]-N_died[i,2])/N_all[row,2] * 100
        }
      }
    }
  }
  
  Labels <- c()
  for(i in 1:nrow(N_all)){
    split <- split_path <- str_split(N_all[i,1], ' -- ')[[1]]
    Labels <- c(Labels,split[1])
  }
  
  SurvivalData$Survival <- data.frame(Labels,Survival)
  
  if (input$SurvivalPlot == "BarPlot"){
    fig <- ggplot(SurvivalData$Survival, aes(x = Labels,y=Survival, fill = Labels, na.rm = TRUE)) +
      stat_summary(fun.data = input$errorSurvival, size = 0.5, width = 0.25, geom="errorbar", position = position_dodge(width=0.8))+
      stat_summary(geom = "bar",fun = "mean", width = 0.6, color = "black", size = 0.5, position = position_dodge(width=0.8))
  }
  else{
    if (input$SurvivalPlot == "BoxPlot"){
      fig <- ggplot(SurvivalData$Survival, aes(x = Labels,y=Survival, fill = Labels, na.rm = TRUE)) +
        geom_boxplot(outlier.colour = "black", na.rm=TRUE)}
      else{
        if (input$SurvivalPlot == "pointRange"){
          fig <- ggplot(SurvivalData$Survival, aes(x=Labels, y=Survival, colour=Labels, na.rm=TRUE))+
            stat_summary(fun.data = input$errorSurvival, position = position_dodge(width=0.75), size = 1, width = 0.25, geom="errorbar")+
            stat_summary(geom = "point",fun = "mean", size = 3, position = position_dodge(width=0.75))

        }
      }
  }

  fig <- whiteBackground(fig, input$titleLetterSize, input$axisLabelSize,
                         input$axisNumbersSize, input$dataLabelSize)+
    labs(title=SurvivalFiguresTitles$Survival, y = SurvivalFiguresYlabels$Survival)+
    coord_cartesian(ylim = input$ySurvival)+
    scale_colour_manual(values = graphsAestethics$df[,'lineColor'])+
    scale_fill_manual(values = alpha(graphsAestethics$df[,'lineColor'], .8))

  SurvivalFigures$Survival <- fig
    
  
  ######
  Conditions <- aggregate(Survival ~ Labels, data=SurvivalData$Survival, FUN=mean)[,1]
  
  N  <- aggregate(Survival ~ Labels, data=SurvivalData$Survival, FUN=length)[,2]
  Survival_Mean <- round(aggregate(Survival ~ Labels, data=SurvivalData$Survival, FUN=mean)[,2],3)
  Survival_SEM <- round(aggregate(Survival ~ Labels, data=SurvivalData$Survival, FUN=se)[,2],3)
  Survival_SD <- round(aggregate(Survival ~ Labels, data=SurvivalData$Survival, FUN=sd)[,2],3)
  Survival_Median <- round(aggregate(Survival ~ Labels, data=SurvivalData$Survival, FUN=median)[,2],3)
  Survival_25Q <- round(aggregate(Survival ~ Labels, data=SurvivalData$Survival, FUN=quantile)[,2][,2],3)
  Survival_75Q <- round(aggregate(Survival ~ Labels, data=SurvivalData$Survival, FUN=quantile)[,2][,4],3)
  
  survivalStatistics <- data.frame(Conditions, N, Survival_Mean, Survival_SEM, 
                                   Survival_SD, Survival_Median, Survival_25Q, Survival_75Q)
  
  output$survivalSummary <- DT::renderDataTable(survivalStatistics,
                                                escape = FALSE, selection = 'none', 
                                                editable  = list(target = 'cell',disable = list(columns = c(2,3,4,5,6,7,8))))
  
}

SurvivalOverTime <- function(){
  
  req(nrow(Dead_animal())>0)
  
  labels <- unique(damData$dt[,labels,meta = T])

  survivalTimeData <- data.frame()
  
  damData$dt[,Alive_status :=  TRUE]
  
  deadIds <- unique(Dead_noInactivity()[,id,meta=T])
  
  
  damData$dt[which(damData$dt[,interaction(id,t,sep = "--")] %in% Dead_animal()[,interaction(id,t,sep = "--")]),Alive_status := FALSE]
  damData$dt[which(damData$dt[,interaction(id,t,sep = "--")] %in% Dead_noInactivity()[,interaction(id,t,sep = "--")]),Alive_status := TRUE]
  
  for(l in labels){
    total <- length(which(damData$dt[,labels,meta=T]==l))

    conditionData <- damData$dt[which(labels == l & t %in% Dead_noInactivity()[,t]),]
    
    
    ConditionSurvival <- aggregate(as.numeric(Alive_status) ~ t, data=conditionData, FUN=sum)
    
    ConditionSurvival[,1] <- as.numeric(ConditionSurvival[,1]) #Time
    ConditionSurvival[,2] <- as.numeric(ConditionSurvival[,2])/total*100

    row <- data.frame(ConditionSurvival[1], l, ConditionSurvival[2]) 
    survivalTimeData <- rbind(survivalTimeData, row)


  }
  
  colnames(survivalTimeData)[1] <- "t"
  colnames(survivalTimeData)[2] <- "labels"
  colnames(survivalTimeData)[3] <- "Survival_percentage"

  fig <- ggplot(survivalTimeData,aes(x = t, y = Survival_percentage, colour = labels))+
    geom_step(size=1) + scale_x_hours() +
    labs(title=SurvivalFiguresTitles$SurvivalPerTime, y = SurvivalFiguresYlabels$SurvivalPerTime)

  fig <- whiteBackground(fig, input$titleLetterSize, input$axisLabelSize,
                         input$axisNumbersSize, input$dataLabelSize) +
    coord_cartesian(ylim = input$ySurvival)+
    scale_colour_manual(values = graphsAestethics$df[,'lineColor'])+
    scale_linetype_manual(values = graphsAestethics$df[,'lineType'])

  SurvivalFigures$timeSurvival <- fig
}

#Update Labels
updateSurvivalLabels <- function(){
  
  if (input$deadTabs == "Actogram per channel"){
    SurvivalFiguresXlabels$allChannels <- input$xLabelSurvival
    SurvivalFiguresYlabels$allChannels <- input$yLabelSurvival
    SurvivalFiguresTitles$allChannels <- input$SurvivalTitle
  }
  else{
    if (input$deadTabs == "Dead animals"){
      SurvivalFiguresXlabels$allChannels <- input$xLabelSurvival
      SurvivalFiguresYlabels$allChannels <- input$yLabelSurvival
      SurvivalFiguresTitles$diedChannels <- input$SurvivalTitle
    }
    else{
      if(input$deadTabs == "Survival"){
        SurvivalFiguresXlabels$Survival <- input$xLabelSurvival
        SurvivalFiguresYlabels$Survival <- input$yLabelSurvival
        SurvivalFiguresTitles$Survival <- input$SurvivalTitle
      }
      else{
        SurvivalFiguresXlabels$SurvivalPerTime <- input$xLabelSurvival
        SurvivalFiguresYlabels$SurvivalPerTime <- input$yLabelSurvival
        SurvivalFiguresTitles$SurvivalPerTime <- input$SurvivalTitle
      }
    }
  }
  
}

#From seconds to days:hours:minutes:seconds
dhms <- function(time){

  list <- c()
  # Convert Period to numeric seconds for R 4.5+ compatibility
  day_secs <- as.numeric(days(1), unit = "secs")
  hour_secs <- as.numeric(hours(1), unit = "secs")
  min_secs <- as.numeric(mins(1), unit = "secs")

  for (i in 1:length(time)){
    value <- paste("Day", time[i] %/% day_secs," "
                   ,paste(formatC(time[i] %/% hour_secs %% 24, width = 2, format = "d", flag = "0") #Hours
                          ,formatC(time[i] %/% min_secs %% 60, width = 2, format = "d", flag = "0") #minutes
                          ,formatC(time[i] %% min_secs, width = 2, format = "d", flag = "0") #Seconds
                          ,sep = ":"
                   ), sep= ""
    )

    list <- c(list,value)
  }
  return(list)
}

####################### RESET analysis #####################
observeEvent(input$files,{
  
  DeadTableExport(NULL)
  channelsToRemove(NULL)
  deadTable(NULL)
  cleanData(NULL)
  Dead_animal(NULL)
  Dead_noInactivity(NULL)
  
  SurvivalFigures$allChannels <- NULL 
  SurvivalFigures$diedChannels <- NULL 
  SurvivalFigures$Survival <- NULL
  SurvivalFigures$SurvivalOverTime <- NULL
  
  SurvivalFiguresTitles$allChannels <- "Actogram per channel"
  SurvivalFiguresTitles$diedChannels <- "Dead animals actogram"
  SurvivalFiguresTitles$Survival <- "Survival"
  
  SurvivalFiguresXlabels$allChannels <- "Time"
  SurvivalFiguresXlabels$Survival <- "Conditions"
  
  SurvivalFiguresYlabels$allChannels <- "Channels"
  SurvivalFiguresYlabels$Survival <- "Survival (%)"
  
  SurvivalData$allChannels <- NULL 
  SurvivalData$diedChannels <- NULL 
  SurvivalData$Survival <- NULL 
})

observeEvent(input$startanalysis,{
  
  ##### RESET variables #####
  DeadTableExport(NULL)
  channelsToRemove(NULL)
  deadTable(NULL)
  cleanData(NULL)
  Dead_animal(NULL)
  Dead_noInactivity(NULL)
  
  SurvivalFigures$allChannels <- NULL 
  SurvivalFigures$diedChannels <- NULL 
  SurvivalFigures$Survival <- NULL 
  
  SurvivalFiguresTitles$allChannels <- "Actogram per channel"
  SurvivalFiguresTitles$diedChannels <- "Dead animals actogram"
  SurvivalFiguresTitles$Survival <- "Survival"
  
  SurvivalFiguresXlabels$allChannels <- "Time"
  SurvivalFiguresXlabels$Survival <- "Conditions"
  
  SurvivalFiguresYlabels$allChannels <- "Channels"
  SurvivalFiguresYlabels$Survival <- "Survival (%)"
  
  SurvivalData$allChannels <- NULL 
  SurvivalData$diedChannels <- NULL 
  SurvivalData$Survival <- NULL
  
  ##### Actogram per channel #####
  ActogramPerChannel()
  
  enable("evaluateDeath")
})

####################### Update Labels ######################
observeEvent(input$deadTabs,{
  
  req(nrow(damData$dt)>0)
  
  if (input$deadTabs == "Actogram per channel"){
    title <- SurvivalFiguresTitles$allChannels
    y <- SurvivalFiguresXlabels$allChannels
    x <- SurvivalFiguresYlabels$allChannels
  }
  else{
    if (input$deadTabs == "Dead animals"){
      title <- SurvivalFiguresTitles$diedChannels
      y <- SurvivalFiguresXlabels$allChannels
      x <- SurvivalFiguresYlabels$allChannels
    }
    else{
      if (input$deadTabs == "Survival"){
        title <- SurvivalFiguresTitles$Survival
        y <- SurvivalFiguresXlabels$Survival
        x <- SurvivalFiguresYlabels$Survival
      }
      else{
        title <- SurvivalFiguresTitles$SurvivalPerTime
        y <- SurvivalFiguresXlabels$SurvivalPerTime
        x <- SurvivalFiguresYlabels$SurvivalPerTime
      }
    }
  }
  updateTextInput(session,"SurvivalTitle", value=title)
  updateTextInput(session,"yLabelSurvival", value=y)
  updateTextInput(session,"xLabelSurvival", value=x)
})
######################## Dead analysis #####################

#Find dead larvae and create dead Table
observeEvent(input$evaluateDeath,{
  
  #Disable delete larvae button while the program finds dead larvae
  
  validate(
    need(nrow(damData$dt)>0,"")
  )
  
  withProgress(message = 'Analyzing dead animals', value = 0, {
    if (is.null(cleanData())){
      #Remove last inactivity period from animals
      withProgress(message = 'Analyzing last inactivity', value = 0, {
        # Convert Period to numeric seconds for R 4.5+ compatibility
        # curate_dead_animals() disabled (threshold set to 20000h = never triggers);
        # death detection handled by detect_dead_animals_rle() above
        cleanData(curate_dead_animals(damData$dt,prop_immobile = 0,time_window = as.numeric(hours(20000), unit = "secs"), resolution = 20000))
        
        deadTable(NULL)
        enable('clean_data')
        
      })
    }
    # Track completely inactive lanes (no activity at all)
    inactiveLaneIds <- c()
    if(length(unique(cleanData()[,id,meta=T]))!= length(unique(damData$dt[,id,meta=T]))){
      aliveId <- cleanData()[,id,meta=T]
      allId <-damData$dt[,id,meta=T]
      message <- ""
      for (i in 1:length(allId)){
        if(!any(as.character(allId[i]) %in% as.character(aliveId))){
          message <- paste(message,allId[i], sep = "; ")
          inactiveLaneIds <- c(inactiveLaneIds, as.character(allId[i]))
          index <- which(damData$dt[,id]==as.character(allId[i]))[1]
          damData$dt[index,activity:=1]
          damData$dt[index,moving:=TRUE]}
      }
      showNotification(paste("The following lanes don't have activity:", message), type = "error", duration = 10)

      withProgress(message = 'Re-analyzing last inactivity', value = 0, {
        # Convert Period to numeric seconds for R 4.5+ compatibility
        # curate_dead_animals() disabled (threshold set to 20000h = never triggers);
        # death detection handled by detect_dead_animals_rle() above
        cleanData(curate_dead_animals(damData$dt,prop_immobile = 0,time_window = as.numeric(hours(20000), unit = "secs"), resolution = 20000))

        deadTable(NULL)
        enable('clean_data')

      })
    }
    
  
    incProgress(0.33)
  
    # Get the final indexes of each replica
    if (nrow(tableData$df)>1){
      finalIndexes <- which(is.nan(damData$dt[,timeDiff]))-1
      finalIndexes <- c(finalIndexes[2:length(finalIndexes)],nrow(damData$dt))
    }
    else{
      finalIndexes <- nrow(damData$dt)}
    
    #Final times from the indexes
    finalTimes <- unique(damData$dt[finalIndexes,t])
    
    #Detect dead animals using RLE-based method (finds longest continuous inactivity period)
    # This method checks for the longest period of zero activity anywhere in the recording
    death_results <- detect_dead_animals_rle(damData$dt, death_inactivity_hours = input$deadTime)

    # Get the IDs of dead animals
    differences <- death_results$dead_ids

    # Log detection results
    if (length(differences) > 0) {
      cat("\n=== RLE Death Detection Results ===\n")
      cat("Threshold:", input$deadTime, "hours of continuous inactivity\n")
      cat("Dead animals detected:", length(differences), "\n")
      if (nrow(death_results$dead_info) > 0) {
        for (i in 1:nrow(death_results$dead_info)) {
          cat(sprintf("  %s: %.1f hours inactivity\n",
                      death_results$dead_info$id[i],
                      death_results$dead_info$max_inactivity_hours[i]))
        }
      }
      cat("===================================\n\n")
    }

    incProgress(0.33)

    # Also add completely inactive lanes to the differences (these are definitely dead)
    if (length(inactiveLaneIds) > 0) {
      differences <- unique(c(differences, inactiveLaneIds))
    }

    Files <- damData$dt[,file_info, meta=T]
    Channels <- damData$dt[,region_id, meta=T]
    
    #Time of death
    ToD <-c()
    first <- c()
    
    indexesToDelete <- c()
    for (i in seq_len(length(differences))){
      index <- match(differences[i],damData$dt[,id, meta=T])
      fileName <- Files[index][[1]]$file
      channel <- Channels[index]
      
      conditionsIndex <- which(Conditions$df[,4]==channel)
      for (j in seq_len(length(conditionsIndex))){
        if (Conditions$df[conditionsIndex[j],1]==fileName){
          indexesToDelete <- c(indexesToDelete,conditionsIndex[j])
          break
        }
      }
      
    }
    
    TODs <- rep(NA,nrow(Conditions$df))
    Survival_status <- rep("Survived",nrow(Conditions$df))
    
    
    ###### Create deadTable #####
    # Fix: Check length instead of NULL since empty vector c() is not NULL
    if (length(indexesToDelete) > 0){
      
      
      #Present animals to remove to the user
      indexesToDelete <- sort(indexesToDelete, decreasing = TRUE)
      channelsToRemove(indexesToDelete)
      
      File <- tools::file_path_sans_ext(Conditions$df[indexesToDelete,1])
      Channels <- Conditions$df[indexesToDelete,4]
      Labels <- Conditions$df[indexesToDelete,5]
      
      
      #Dead and live animal variables
      
      channels <- Conditions$df[indexesToDelete,'region_id']
      paste_channels <- c()
      for (i in 1:length(channels)){
        if (channels[i]<10)
          paste_channels <- c(paste_channels,paste0('0',channels[i]))
        else
          paste_channels <- c(paste_channels,channels[i])
      }
      
      ids <- paste(Conditions$df[indexesToDelete,'start_datetime'],paste0(Conditions$df[indexesToDelete,'file']),paste_channels,sep="|")
      
      #Time of death
      for (k in 1:length(ids)){
        timeAlive <- cleanData()[which(id == ids[k]),t]
        TODs[indexesToDelete[k]] <- dhms(timeAlive[length(timeAlive)])
        Survival_status[indexesToDelete[k]] <- 'Died'
        ToD <- c(ToD, dhms(timeAlive[length(timeAlive)]))
      }
      
      Time_of_death <- ToD
      table <- data.frame(File,Channels,Labels,Time_of_death)
      
      #Table of dead larvae
      deadTable(table)
      
      DeadTableExport()
      #Dead animals
      
      Dead_animal(damData$dt[which(damData$dt[,'id']==ids[1]),])
      Dead_noInactivity(cleanData()[which(cleanData()[,'id']==ids[1]),])
      
      if(length(ids)>1){
        for(i in 2:length(ids)){
          deadAnimal <- damData$dt[which(damData$dt[,'id']==ids[i]),]
          Dead_animal(bind_behavr_list(list(Dead_animal(),deadAnimal)))
          
          dead_active <- cleanData()[which(cleanData()[,'id']==ids[i]),]
          Dead_noInactivity(bind_behavr_list(list(Dead_noInactivity(),dead_active)))
         }
      }
      
      
      shinyjs::enable("Death_graphs")
      shinyjs::enable('deleteInactivity')
      shinyjs::enable('deleteAnimals')
      
      
    }
    else{
      deadTable(data.frame(NULL))
      # Notify user that no dead animals were detected
      showNotification(paste("No dead animals detected with", input$deadTime, "hours inactivity threshold. All animals appear to be alive."),
                      type = "message", duration = 8)
    }

    LaT <- rep(NA,nrow(Conditions$df))
    for(j in 1:nrow(Conditions$df)){
      if (Conditions$df[j,'region_id']<10)
        paste_channel <- paste0('0',Conditions$df[j,'region_id'])
      else
        paste_channel <- Conditions$df[j,'region_id']
      
      identifier <- paste(Conditions$df[j,'start_datetime'],paste0(Conditions$df[j,'file']),paste_channel,sep="|")
      time <- cleanData()[which(id == identifier),t]
      LaT[j] <- dhms(time[length(time)])
    }
    
    Time_of_death <- TODs
    Last_activity_timepoint <- LaT
    
    ###Export variable
    Files <- tools::file_path_sans_ext(Conditions$df[,1])
    Start_time <- Conditions$df[,2]
    End_time <- Conditions$df[,3]
    Channels <- Conditions$df[,4]
    Labels <- Conditions$df[,5]
    
    #Data table export
    DeadTableExport(data.frame(Files, Start_time, End_time, Channels, Labels, Survival_status, Time_of_death, Last_activity_timepoint))
    
    
    settings <- settingsTable()
    settings[10,2] <- input$deadTime
    settingsTable(settings)
    incProgress(0.33)
    })
  
})

### Table of dead animals
observeEvent(deadTable(),{
  output$DeadTubes <- DT::renderDataTable(
    data.frame(deleteButtonColumn(deadTable(), 'delete_button')), escape = FALSE, selection = 'none',editable  = FALSE)
})

#Delete deleted data from analysis
observeEvent(input$deletePressed, {
  rowNum <- parseDeleteEvent(input$deletePressed)
  
  if(input$pages == "Survival analysis"){
    
    req(nrow(Conditions$df)>0)
    req(length(channelsToRemove())>0)
    
    for (k in seq(nrow(Conditions$df),1,-1)){
      if (Conditions$df[k,4] == deadTable()[rowNum,2] & tools::file_path_sans_ext(Conditions$df[k,1]) == deadTable()[rowNum,1]){
        
        ch <- Conditions$df[k,4]
        row <- ceiling(ch/8)
        col <- ch-8*(row-1)
        shinyjs::enable(paste0(paste0("cbox",col),row))
        
        
        if (ch<10)
          paste_channels <- paste0('0',ch)
        else
          paste_channels <- ch
        
        ids <- paste(Conditions$df[row,'start_datetime'],paste0(Conditions$df[row,'file']),paste_channels,sep="|")
        
        indexesToDelete <- channelsToRemove()
        subtract1 <- FALSE
        for (i in seq(from = length(indexesToDelete),to = 1,by = -1)){
          if (subtract1 == TRUE){
            indexesToDelete[i] <- indexesToDelete[i]-1
          }
          if (indexesToDelete[i]==k){
            indexesToDelete <- indexesToDelete[-i]
            subtract1 <- TRUE
          }
        }
        channelsToRemove(indexesToDelete)
        
        #Delete value from metadata
        Conditions$df <- Conditions$df[-k,]
        
        deadTable(deadTable()[-rowNum,])
        
        break
      }
    }
    settings <- settingsTable()
    settings[12,2] <- TRUE
    settingsTable(settings)
  
    
    MinTime(TRUE)
    
    if (nrow(Conditions$df)==0){
      damData$dt <- NULL
      cleanData(NULL)
    }
    else{
      #Dead animals
      damData$dt<-damData$dt[which(damData$dt[,'id']!=ids),]
      cleanData(cleanData()[which(cleanData()[,'id']!=ids),])
    }
    
    
    #Select all ticks
    shinyjs::enable(paste0(paste0("cbox",9),1))
    shinyjs::enable(paste0(paste0("cbox",9),2))
    shinyjs::enable(paste0(paste0("cbox",9),3))
    shinyjs::enable(paste0(paste0("cbox",9),4))
    
    n<- getFileNr()
    
    if(length(Conditions$df[,1]>0)){
      for (k in seq(length(Conditions$df[,1]))){
        if (Conditions$df[k,1] == ImportedFiles()[n]){
          
          ch <- Conditions$df[k,4]
          if (ch>=1 & ch<=8){
            shinyjs::disable(paste0(paste0("cbox",9),1))}
          if (ch>=9 & ch<=16){
            shinyjs::disable(paste0(paste0("cbox",9),2))}
          if (ch>=17 & ch<=24){
            shinyjs::disable(paste0(paste0("cbox",9),3))}
          if (ch>=25 & ch<=32){
            shinyjs::disable(paste0(paste0("cbox",9),4))}
        }}
    }
    
    # Delete the row from the data frame
    tableData$df <- tableData$df[-rowNum,]
  }
})

####### Create graphs after death evaluation #############
observeEvent(input$evaluateDeath,{
  
  ActogramPerChannel()
  DiedPerChannel()
  SurvivalEnd()
  SurvivalOverTime()
})

###### Update graphs
observeEvent(input$updateSurvivalGraphs,{
  
  #Update labels
  updateSurvivalLabels()
  
  #Update figures
  ActogramPerChannel()
  DiedPerChannel()
  SurvivalEnd()
  SurvivalOverTime()
  
})

#################### Plots #####################

# Plots
output$allChannels <- renderPlot({
    
  validate(
    need (SurvivalFigures$allChannels,"No data selected")
  )
    
  fig <- SurvivalFigures$allChannels
    
  return(fig)
    
},width = function() input$SurvivalWidth,
height = function() input$SurvivalHeight,
res = 96)
  
output$deadChannels <- renderPlot({
  
  validate(
    need (SurvivalFigures$diedChannels,"No data selected")
  )
    
  fig <- SurvivalFigures$diedChannels
    
  return(fig)
  
},width = function() input$SurvivalWidth,
height = function() input$SurvivalHeight,
res = 96)

#Survival bar plot
output$Survival <- renderPlot({
  
  validate(
    need (SurvivalFigures$Survival,"No data selected")
  )
  
  fig <- SurvivalFigures$Survival
  
  return(fig)
}, width = function() input$SurvivalWidth,
height = function() input$SurvivalHeight,
res = 96)

# Survival bar plot
output$SurvivalTime <- renderPlot({


  validate(
    need (SurvivalFigures$timeSurvival,"No data selected")
  )

  fig <- SurvivalFigures$timeSurvival

  return(fig)
}, width = function() input$SurvivalWidth,
height = function() input$SurvivalHeight,
res = 96)

################### Delete animals ######################


##### Remove all dead animals #
observeEvent(input$deleteAnimals,{
  
  validate(
    need(length(channelsToRemove())>0,"")
  )
  
  disable('deleteInactivity')
  disable("deleteAnimals")
  disable("evaluateDeath")
  
  numberConditions <- length(unique(damData$dt[,'labels']))
  
  withProgress(message = 'Removing dead animals', value = 0, {
    indexesToDelete <- channelsToRemove()
    
    #Delete dead larvae
    paste_channels <- c()
    for (i in 1:length(indexesToDelete)){
      if (as.numeric(Conditions$df[indexesToDelete[i],'region_id'])<10)
        paste_channels <- c(paste_channels,paste0('0',Conditions$df[indexesToDelete[i],'region_id']))
      else
        paste_channels <- c(paste_channels,Conditions$df[indexesToDelete[i],'region_id'])
    }
    
    ids <- paste(Conditions$df[indexesToDelete,'start_datetime'],paste0(Conditions$df[indexesToDelete,'file']),paste_channels,sep="|")
    
    #Dead animals
    for (i in 1:length(ids)){
      damData$dt<-damData$dt[which(damData$dt[,'id']!=ids[i]),]
      cleanData(cleanData()[which(cleanData()[,'id']!=ids[i]),])
    }
    
    
    updateChannels()
    
    # output$DeadTubes <- DT::renderDataTable(
    #   NULL, escape = FALSE, selection = 'none',editable  = FALSE)
    
    
    for (k in 1:length(indexesToDelete)){
      ch <- Conditions$df[indexesToDelete[k],4]
      row <- ceiling(ch/8)
      col <- ch-8*(row-1)
      shinyjs::enable(paste0(paste0("cbox",col),row))
      
      Conditions$df <- Conditions$df[-indexesToDelete[k],]
    }
    
    channelsToRemove(c())
    
  })
  
  settings <- settingsTable()
  settings[11,2] <- TRUE
  settingsTable(settings)
  
  if (length(unique(damData$dt[,'labels'])) < numberConditions){
    graphsAestethics$df <- graphsAestethics$df[-nrow(graphsAestethics$df),]}
  
  MinTime(TRUE)
  updateSliderInput(session,"movingAverage",value = 60)
  
  
  # updateFigures()
  # 
  # PeriodicRepresentationsData()
  # ActivityRepresentationsData()
  # 
  # ActivityDataSummary()
  # SleepDataSummary()
  # updateActivityFigures()
  # updateSleepFigures()
  # 
  # req(nrow((BoutActivityData$lightDark))>0)
  # 
  # ActivityBoutsData()
  # SleepBoutsData()
  # 
  # updateBoutActivityFigures()
  # updateBoutTimeFigures()
  # updateBoutSleepTimeFigures()
  # updateBoutSleepLatencyFigures()
  # 
  # updateYBoutActivity()
  # updateYBoutTime()
  # updateYSleepTime()
  # updateYSleepLatency()
})

##### REMOVE INACTIVITY OF ANIMALS #
observeEvent(input$deleteInactivity,{
  req(damData$dt)
  
  damData$dt <- cleanData()
  
  disable("clean_data")
  disable("deleteInactivity")
  disable('deleteAnimals')
  
  settings <- settingsTable()
  settings[13,2] <- TRUE
  settingsTable(settings)
  
  # updateFigures()
  # 
  # PeriodicRepresentationsData()
  # ActivityRepresentationsData()
  # 
  # ActivityDataSummary()
  # SlepDataSummary()
  # updateActivityFigures()
  # updateSleepFigures()
  # 
  # req(nrow((BoutActivityData$lightDark))>0)
  # ActivityBoutsData()
  # SleepBoutsData()
  # 
  # updateBoutActivityFigures()
  # updateBoutTimeFigures()
  # updateBoutSleepTimeFigures()
  # updateBoutSleepLatencyFigures()
  # 
  # updateYBoutActivity()
  # updateYBoutTime()
  # updateYSleepTime()
  # updateYSleepLatency()
})

############### Save death analysis and figures ##########
#Save dead table
observe({
  req(nrow(DeadTableExport())>0)
  
  if (nrow(DeadTableExport())>0){
    shinyjs::enable("saveDeath")
  }
  else{
    shinyjs::disable("saveDeath")
  }
  
  output$saveDeath <- downloadHandler(
    filename = function(){
      paste0("Dead_table.xlsx")
    },
    content = function(file){

      wb <- createWorkbook()

      settings <- settingsTable()[10:13,]
      addWorksheet(wb, "Settings")
      writeData(wb, "Settings", settings)

      data <- DeadTableExport()

      data[,2] <- as.character(data[,2])
      data[,3] <- as.character(data[,3])

      addWorksheet(wb, "All channels")
      writeData(wb, "All channels", data)

      # Create sheets
      Labels <- data[,'Labels']
      uniqueLabels <- unique(Labels)
      if (length (uniqueLabels)>1){
        start <- match(uniqueLabels[1],Labels)
        for (i in 2:length(uniqueLabels)){
          sheetName <- checkSymbolsExcel(uniqueLabels[i-1])
          addWorksheet(wb, sheetName)
          end <- match(uniqueLabels[i],Labels)-1
          writeData(wb, sheetName, data[start:end,])
          start <- end+1
        }
        sheetName <- checkSymbolsExcel(uniqueLabels[length(uniqueLabels)])
        addWorksheet(wb, sheetName)
        writeData(wb, sheetName, data[start:nrow(data),])
      }
      else{
        sheetName <- checkSymbolsExcel(uniqueLabels[1])
        addWorksheet(wb, sheetName)
        writeData(wb, sheetName, data)
      }
      saveWorkbook(wb, file, overwrite = TRUE)
    })
})

#save Figures
observe({
  req(nrow(damData$dt)>0)
  
  req(SurvivalFigures$allChannels)
  
  if (nrow(damData$dt)>0){
    shinyjs::enable("saveDeathFigure")
  }
  else{
    shinyjs::disable("saveDeathFigure")
  }
  
  output$saveDeathFigure <- downloadHandler(
    
    filename = function(){
      paste0(input$deadTabs,input$DeathFig)
    },
    content = function(file){
      
      if (input$deadTabs == "Actogram per channel"){
        fig <- SurvivalFigures$allChannels
      }
      else{
        if (input$deadTabs == "Dead animals"){
          fig <- SurvivalFigures$diedChannels
        }
        else{
          if(input$deadTabs == "Survival"){
            fig <- SurvivalFiguress$Survival
          }
          else{
            fig <- SurvivalFigures$SurvivalPerTime
          }
        }
      }
      
      req(!is.null(fig))
      ggsave(filename = file, plot = fig,
             width = round(input$SleepWidth/97), height= round(input$SleepHeight/97))
    })
  
})