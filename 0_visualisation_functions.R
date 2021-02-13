# COLOR DEFINITIONS

# this batch of colors is only used for Austria at the moment
coal<-"#774239"
green<-"#166222"
hydroblue<-"#4B7EA4"
nucleargrey<-"#9B7C58"
gasgrey<-"#9c968f"
oil<-"#181716"

# these are the standard colors for Power
RenewablesColour <<- "#feedde"
HydroColour <<- "#e6550d"
NuclearColour <<- "#fd8d3c"
GasCapColour <<- "#fdbe85"
OilCapColour <<- "#a63603"
CoalCapColour <<- "#7A2701"

# 2DII colors used throughout the plots
primary_blue <- "#1b324f"; primary_orange <- "#ff9623"; primary_green <- "#00c082"; primary_grey <- "#d0d7e1"
secondary_red <- "#a63d57"; secondary_yellow <- "#f2e06e"; secondary_soft_blue <- "#78c4d6"
secondary_dark_purple <- "#574099"; secondary_moss_green <- "#4a5e54"
background_blue <- "#8597a6"; background_light_blue <- "#d0d7e1"
hybrid_green <- "#0E7969"; hybrid_blue <- "#50C3BA"; hybrid_orange <- "#FBAF3C"

auto_palette<-c("#d0d7e1",	"#78c4d6",	"#1b324f")
power_palette<-c("#7A2701",	"#a63603", 	"#fdbe85",	"#fd8d3c",	"#e6550d",	"#feedde")

primary_blue_faded <- "#96ADCA"
secondary_red_faded <- "#D290A1"
oil_faded <- "#A49382"
secondary_moss_green_faded <- "#99BEAB"
power_faded <- "#F0CAA7"

# fiveyear colors

darkred<-"#FFFFCC"
lighterred<-"#FDE291"
violet<-"#E07B73"
lightblue<-"#C3D69B"
blue<- "#9CAB7C"

# theme used in most of the plots

theme_2dii_ggplot <- function() {
  theme_classic() %+replace%
    theme(
      plot.title = element_text(face = "bold", family = "Helvetica", color = primary_blue, size = 15.5, margin = margin(25, 2, 8, 2)),
      plot.subtitle = element_text(family="Helvetica", color=primary_blue, size = 10, margin = margin(2, 2, 2, 2)),
      legend.position="bottom",
      legend.title=element_blank(),
      axis.title = element_text(family="Helvetica", color=primary_blue, face = "bold", size = 12, margin = margin(3, 3, 3, 3)),
      axis.text = element_text(face = "bold", family="Helvetica", color=primary_blue, size = 10),
      axis.line = element_line(color=primary_blue), 
      legend.text = element_text(family="Helvetica", color=primary_blue, face = "bold", size = 10),
      panel.spacing = unit(0.5, "cm"),
      strip.text = element_text(family="Helvetica", color=primary_blue, size = 10, face = "bold"),
      strip.background = element_rect(color = "white", size = 1)
    )
}

translate <- function(data){
  for(i in 1:length(translations$original)){
    data[data==translations[[i,1]]]<-translations[[i,2]]
  }
  return(data)
}



plot_tech_share <- function(technology_choice, title){

  data <- bonds %>%
    filter(scenario=="WEO2019_SDS", scenario_geography=="GlobalAggregate", year=="2020",
           technology==technology_choice) %>%
    select(company_name, plan_tech_share) %>%
    arrange(plan_tech_share) %>%
    filter(!(company_name %in% c("Enel Finance Intl Nv",
                                 "Iberdrola Fin Ireland",
                                 "Iberdrola Finanzas Sau",
                                 "Ge Capital Uk Funding Unlimited Co",
                                 "Ge Capital Euro Funding",
                                 "E.On Intl Finance Bv", "Petrol Dd Ljubljana")))
  
  data$company_name <- reorder(data$company_name, data$plan_tech_share)
  
  p <- ggplot(data, aes(x=company_name, y=plan_tech_share)) + 
    geom_bar(stat="identity", fill=primary_blue, width=0.55) +
    coord_flip() +
    ylab("") + xlab("") +
    scale_y_continuous(labels = function(x) paste0(100*x, "%")) +
    theme_2dii_ggplot() + 
    theme(axis.ticks.y=element_blank()) +
    ggtitle(title) +
    theme(axis.text = element_text(family="Helvetica", color="#1b324f", size = 11, margin= margin(0.5, 0.5, 0.5, 0.5)))
  p
  
  return(p)
  
}


plot_tech_share_sector <- function(technologies, title_sector, palette){
  
  portfolio_techmix <- bonds_portfolio %>%
    filter(!(scenario %in% c("GECO2019_1.5c", "GECO2019_2c_m", "GECO2019_ref"))) %>%
    filter(scenario_geography!="GlobalAggregate") %>%
    filter(year==startyear) %>%
    select(portfolio_name, scenario_geography, ald_sector, technology, plan_tech_share) %>%
    distinct() %>%
    filter(technology %in% technologies) %>%
    group_by(scenario_geography) %>%
    mutate(plan_tech_share=plan_tech_share/sum(plan_tech_share, na.rm=T)) %>%
    ungroup()
  
  market_techmix <- market_bonds %>%
    filter(!(scenario %in% c("GECO2019_1.5c", "GECO2019_2c_m", "GECO2019_ref"))) %>%
    filter(scenario_geography!="GlobalAggregate") %>%
    filter(year==startyear) %>%
    select(portfolio_name, scenario_geography, ald_sector, technology, plan_tech_share) %>%
    distinct() %>%
    filter(technology %in% technologies) %>%
    group_by(scenario_geography) %>%
    mutate(plan_tech_share=plan_tech_share/sum(plan_tech_share, na.rm=T)) %>%
    ungroup()
  
  data_techmix <- rbind(portfolio_techmix, market_techmix)
  data_techmix[data_techmix=="GlobalMarket"]<-"Market"
  data_techmix[data_techmix=="Meta Portfolio"]<-"ECB CF"
  data_techmix$portfolio_name<-paste0(data_techmix$scenario_geography, "\n", data_techmix$portfolio_name)
  
  
  p<-ggplot(data=data_techmix, aes(fill=technology, x=portfolio_name, y=plan_tech_share, width=.45)) +
    geom_bar(stat="identity") +
    ggtitle(paste("Aggregate technology mix of", title_sector, "companies \n in the collateral framework")) +
    scale_y_continuous(labels = function(x) paste0(100*x, "%")) +
    scale_fill_manual(values=palette) + 
    ylab("") + xlab("") +
    theme_2dii_ggplot() +
    theme(axis.text = element_text(family="Helvetica", color="#1b324f", size = 14, margin= margin(0.5, 0.5, 0.5, 0.5))) +
    theme(axis.title = element_text(family="Helvetica", color="#1b324f", size = 14, margin= margin(0.5, 0.5, 0.5, 0.5))) +
    theme(legend.text = element_text(family="Helvetica", color="#1b324f",  size = 14, margin=margin(5, 5, 5, 5)))
  p
  
  return(p)
  
}












plot_ald_map<-function(technology_choice, title){
  
  library(ggplot2)
  require(maps)
  require(viridis)
  theme_set(
    theme_void()
  )

  
  map_data<-bonds_map%>%
    filter(technology==technology_choice) %>%
    filter(year==startyear) %>%
    #filter(allocation=="ownership_weight") %>%
    group_by(ald_location) %>%
    summarise(sum_weight=sum(plan_alloc_wt_tech_prod, na.rm=T)) %>%
    ungroup()
  map_data<-na.omit(map_data)
  
  countries<-map_data$ald_location
  map_data$ald_location<-iso.expand(countries, regex=TRUE)
  
  map_data[map_data=="(^France)|(^Clipperton Island)"]<-"France"
  map_data[map_data=="(^China(?!:Hong Kong|:Macao))|(^Paracel Islands)"]<-"China"
  map_data[map_data=="Norway(?!:Bouvet|:Svalbard|:Jan Mayen)"]<-"Norway"
  map_data[map_data=="UK(?!r)"]<-"UK"
  map_data[map_data=="(^Spain)|(^Canary Islands)"]<-"Spain"
  
  map_data_2<-map_data %>% mutate(share=sum_weight/sum(sum_weight, na.rm=T))
  
  world_map <- map_data("world")
  joined_map<-left_join(world_map, map_data, by=c("region"="ald_location"))
  #joined_map$sum_weight<-ifelse(is.na(joined_map$sum_weight), 0, joined_map$sum_weight)
  #joined_map$sum_weight<-joined_map$sum_weight/100000000
  
  p1<-ggplot(joined_map, aes(x = long, y = lat, group = group, fill=sum_weight)) +
    #scale_fill_gradient("",colours=brewer.pal(9,"YlOrBr"))+
    geom_polygon() +
    ggtitle(title) +
    theme_2dii_ggplot() +
    #theme(legend.position="none")+
    theme(axis.line=element_blank()) +
    theme(axis.text = element_blank())+
    theme(axis.title=element_blank())
  p1
  
  return(p1)
}


fiveyeargrowth <- function(tech_choice, geography, plot_title, ytitle){
  
  if(tech_choice=="ICE"){
    high <- "ETP2017_NPS"
    middle <- "ETP2017_SDS"
    low <- "ETP2017_B2DS"
    color_scheme<-rev(c(lighterred, darkred, violet, blue))
  } else if(tech_choice=="RenewablesCap") {
    low <- "WEO2019_CPS"
    middle <- "WEO2019_NPS"
    high <- "WEO2019_SDS"
    color_scheme <-rev(c(darkred, lighterred, blue, violet))
    } else if(tech_choice %in% c("Electric", "Hybrid")){
      low <- "ETP2017_NPS"
      middle <- "ETP2017_SDS"
      high <- "ETP2017_B2DS"
      color_scheme <-rev(c(darkred, lighterred, blue, violet))
    } else {
    high <- "WEO2019_CPS"
    middle <- "WEO2019_NPS"
    low <- "WEO2019_SDS"
    color_scheme<-rev(c(lighterred, darkred, violet, blue))
  }
  
  results<-bonds_portfolio %>%
    filter(scenario_geography==geography, year!="2030") %>%
    filter(scenario %in% c(high, middle, low)) %>%
    filter(technology==tech_choice) %>%
    select(scenario, year, plan_tech_prod, scen_tech_prod) %>%
    rename(production=plan_tech_prod, scenarios=scen_tech_prod)
  

  market_bonds_2<-market_bonds %>%
    filter(scenario_geography==geography, allocation=="portfolio_weight", year!="2030", portfolio_name=="GlobalMarket") %>%
    filter(scenario %in% c(high, middle, low)) %>%
    filter(technology==tech_choice) %>%
    select(scenario, year, plan_tech_prod) %>%
    rename(market=plan_tech_prod)
  
  fiveyear <- left_join(results, market_bonds_2, by=c("scenario", "year"))
  fiveyear <- fiveyear %>%
    mutate(market=market/fiveyear[[1, 5]]*fiveyear[[1, 3]])
  
  fiveyear <- melt(fiveyear, id.vars=c("scenario", "year"))

  lower_area_border <- min(fiveyear$value)
  upper_area_border <- max(fiveyear$value)
  
  ALD.sc <- fiveyear %>%
    filter(variable=="scenarios") %>%
    select(scenario, year, value)
  ALD.sc$year<-as.numeric(as.character(ALD.sc$year))
  
  plot_sc_lower<-ALD.sc %>%
    filter(scenario!=high)
  plot_sc_lower[plot_sc_lower==middle]<-high
  plot_sc_lower[plot_sc_lower==low]<-middle
  
  plot_sc<-left_join(ALD.sc, plot_sc_lower, by=c("year", "scenario"))
  
  plot_sc[is.na(plot_sc)]<-lower_area_border
  
  add_on<-plot_sc %>%
    filter(scenario==high)
  add_on$value.y<-add_on$value.x
  add_on$value.x<-upper_area_border
  add_on$scenario<-"HCPS"
  
  
  cps<-plot_sc %>%
    filter(scenario==high)
  npsrts<-plot_sc %>%
    filter(scenario==middle)
  sds<-plot_sc %>%
    filter(scenario==low)
  
  
  ALD.prod<-fiveyear %>%
    filter(variable=="production") %>%
    select(scenario, year, value)
  ALD.prod$year<-as.numeric(as.character(ALD.prod$year))
  
  ALD.market<-fiveyear %>%
    filter(variable=="market") %>%
    select(scenario, year, value)
  ALD.market$year<-as.numeric(as.character(ALD.market$year))
  
  
  p<-ggplot(plot_sc) +
    geom_ribbon(data=add_on, aes(ymin=value.y , ymax=value.x, x=year,fill=violet, group=1),alpha=0.75) +
    geom_ribbon(data=cps, aes(ymin=value.y , ymax=value.x, x=year,fill=scenario, group=1),alpha=0.75) +
    geom_ribbon(data=npsrts, aes(ymin=value.y , ymax=value.x, x=year,fill=darkred, group=1),alpha=0.75) +
    geom_ribbon(data=sds, aes(ymin=value.y , ymax=value.x, x=year,fill=lightblue, group=1),alpha=0.75) +
    scale_fill_manual(values=color_scheme, name="fill") +
    geom_line(data=ALD.prod, aes(x=year, y=value), size=1.4) +
    geom_line(data=ALD.market, aes(x=year, y=value), color=primary_grey, size=1.4) +
    theme_2dii_ggplot() +
    theme(axis.line = element_blank()) +
    xlab("") + ylab(ytitle) +
    ggtitle(plot_title) +
    theme(legend.position="none") +
    theme(axis.text = element_text(family="Helvetica", color="#1b324f", size = 16, margin= margin(5, 5, 5, 5))) +
    theme(axis.title = element_text(family="Helvetica", color="#1b324f", size = 14, margin= margin(5, 5, 5, 5)))
  p
  
  return(p)
  
}


