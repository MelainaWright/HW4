fisheries_summary = function(fishprice, fishnumber, graph="true") {
  
  # * most frequently caught fish in each location
  for(colName in names(fishnumber)){
    if(colName!="species"){
      maxCount = max(fishnumber[[colName]])
      maxFish = fishnumber$species[fishnumber[[colName]] == maxCount]
      cat("Location: ", colName,"\t maxFish: ", maxFish, "\n")
    }
  }
  
  
  # * total revenue for each location
  totalRevenueByLocation = c()
  for(colName in names(fishnumber)){
    if(colName !="species"){
      currentFishRevenue = 0
      for(species in fishnumber$species){
        price = fishprice$price[fishnumber$species==species]
        amount = fishnumber[[colName]][fishnumber$species==species]
        currentFishRevenue = currentFishRevenue + price*amount
      }
      cat("Location: ", colName, "\t currentFishRevenue: ", currentFishRevenue, "\n")
      totalRevenueByLocation = c(totalRevenueByLocation,currentFishRevenue)
    }
  }
  locs = names(fishnumber)
  locs = locs[-1]
  TotalRevenueByLocationDF = data.frame(Location=locs,totalRevenueByLocation, stringsAsFactors = FALSE)
  
  
  
  # * total fisheries sum
  TotalFisheriesSum = sum(TotalRevenueByLocationDF$totalRevenueByLocation)
  TotalFisheriesSumDF = c(Location="Total", TotalFisheriesSum, stringsAsFactors = FALSE)
  
  RevenueDF = rbind(TotalRevenueByLocationDF, TotalFisheriesSumDF)
  
  
  # * if user requests it graph revenue by location and add total to the plot
  if(graph=="true"){
    RevenueGraph = ggplot(RevenueDF, aes(x=as.factor(Location), y=totalRevenueByLocation)) +
      geom_bar(stat = "identity") +
      labs(x="Location", y="Total Revenue") +
      ggtitle("Fisheries Revenue by Location")
    RevenueGraph
    
  }
  
}#end of function