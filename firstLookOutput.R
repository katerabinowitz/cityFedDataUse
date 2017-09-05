#plot functions
plot <- function(x, y) {
  temp <- as.data.frame(table(x)) %>% subset(x != "")
  print(ggplot(data=temp, aes(x=reorder(x, Freq), y=Freq),
               environment = environment()) +
          geom_bar(stat="identity") +
          scale_y_continuous(expand = c(0, 0), limits = c(0, max(temp$Freq) + 5)) +
          coord_flip() +
          theme_bw() +
          theme(axis.title.x=element_blank(),
                axis.title.y=element_blank()) +
          ggtitle(y))
}

plotMultiQ <- function(x, y) {
  useCol <- subset(colnames(output), grepl(x, colnames(output)))
  use <- output[(colnames(output) %in% useCol)]
  use <- stack(use)[c(1)]
  use <- as.data.frame(table(use$values)) %>% subset(Var1 != "")
  
  print(ggplot(data=use, aes(x=reorder(Var1, Freq), y=Freq),
               environment = environment()) +
          geom_bar(stat="identity") +
          scale_y_continuous(expand = c(0, 0), limits = c(0, max(use$Freq) + 5)) +
          coord_flip() +
          theme_bw() +
          theme(axis.title.x=element_blank(),
                axis.title.y=element_blank()) +
          ggtitle(y)) 
}


#order of questions 
tableList <- as.list(output)

plot(tableList$cityClean, "What city do you work for?")
plot(tableList$fedUse, "Does your department use federal data?")

multiQs <- c("use", "data")
multiTitle <- c("What types of federal data does your department use? Select all that apply.", 
                "What federal agencies does your department rely on most for data? Pick up to three.")
mapply(plotMultiQ, multiQs, multiTitle)

plot(tableList$howAccess, "How does your department typically access federal data?")

multiQs <- c("geo", "do")
multiTitle <- c("If your department uses geographic data, what levels of geography do you look at?", 
                "How does your department use federal data?")
mapply(plotMultiQ, multiQs, multiTitle)

tableList <- subset(tableList, names(tableList) %in% c("import", "startUse", "pastChg", "futureChg", "freq", "access", "easeUse", "accuracy",
                                                       "futureFreq","futureAccess", "futureUse", "futureAccuracy", "mena", "hispanic", "lostAccess", "wishData"))
titleList <- c("How important is federal data to your department's mission?",
               "When did your department start using federal data", "Does your department use more or less federal data today than 5 years ago?", 
               "Do you anticipate your department using more or less federal data in the next 5 years?", "How would you describe the publishing schedule of federal data?",
               "How easy it is to find and access federal data?", "How easy is it to use federal data?", "How would you rate the accuracy of federal data?", 
               "How do you expect frequency to change in the next 5 years?", "How do you expect access to change in the next 5 years?",
               "How do you expect ease of use to change in the next 5 years?", "How do you expect accuracy to change in the next 5 years?", 
               "Please indicate how valuable a Middle Eastern + North African Census race question would be to your department", 
               "Please indicate how valuable a Hispanic Census race question would be to your department?", "Has your department lost access to any federal data since January?",
               "Is there any federal data your department would like but do not have access to?") #19

mapply(plot, tableList, titleList)