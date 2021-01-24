rm(list=ls())
library(tidyverse)
library(httr)
library(r2dii.utils)
library(treemap)

github_folder<-"/Users/fionaspuler/Documents/GitHub/Collateral-Assets-Dashboard"
pacta_code_folder<-"/Users/fionaspuler/Documents/GitHub/PACTA_analysis_ecb"
pacta_results_folder<-"/Users/fionaspuler/Dropbox (2° Investing)/PortCheck_v2/10_Projects/ecb_collateral"
ald_folder<-"/Users/fionaspuler/Dropbox (2° Investing)/PortCheck/00_Data/07_AnalysisInputs/2019Q4_10042020_2020"

setwd(github_folder)

### download adta from ECB website

source(get_data.R)

###  transform data into PACTA readable format

data <- raw_data %>%
  select(TYPE, HAIRCUT_CATEGORY, ISIN_CODE) %>%
  mutate(NumberofShares=NA, MarketValue=100, Currency="EUR")

colnames(data)<-c("Investor.Name", "Portfolio.Name", "ISIN", "NumberofShares", "MarketValue", "Currency")
write_excel_csv(data, "/Users/fionaspuler/Dropbox (2° Investing)/PortCheck_v2/10_Projects/ecb_collateral/20_Raw_Inputs/ecb_collateral_Input.csv")

### run PACTA

source(paste0(pacta_code_folder, "/1_portfolio_check_initialisation.R"))
source(paste0(pacta_code_folder, "2_project_input_analysis.R"))
source(paste0(pacta_code_folder, "/3_run_analysis.R"))



### visualize results

setwd(github_folder)

source("0_visualisation_functions.R")
startyear<-2020

# load the PACTA results

total<-readRDS(paste0(pacta_results_folder, "/30_Processed_Inputs/ecb_collateral_total_portfolio.rda")) %>%
  filter(portfolio_name!="Meta Portfolio")

bonds<-readRDS(paste0(pacta_results_folder, "/40_Results/Bonds_results_company.rda")) %>%
  filter(portfolio_name=="Meta Portfolio")

bonds_portfolio<-readRDS(paste0(pacta_results_folder, "/40_Results/Bonds_results_portfolio.rda")) %>%
  filter(portfolio_name=="Meta Portfolio") 

bonds_map<-readRDS(paste0(pacta_results_folder, "/40_Results/ECB/Bonds_results_map.rda"))

audit<-total<-readRDS(paste0(pacta_results_folder, "/30_Processed_Inputs/ecb_collateral_audit_file.rda")) %>%
  filter(portfolio_name=="Meta Portfolio")

market_bonds<-readRDS(paste0(ald_folder, "/0_MarketPortfolios_bonds_portfolio.rda"))

# overview of companies in PACTA sectors

overview_companies <- total %>%
  select(company_name, financial_sector, ) %>%
  distinct() %>%
  filter(financial_sector!="Other", financial_sector!="Unclassifiable") %>%
  mutate(count=1) %>%
  select(financial_sector, company_name, count)
  
png(filename="plots/tree.png",width=800, height=800)
treemap_plot <- treemap(overview_companies, index=c("financial_sector","company_name"),     vSize="count", 
                        type="index",
                        palette=c(primary_blue, secondary_red , primary_grey, oil, hybrid_orange, secondary_moss_green),
                        title="Overview of companies in PACTA sectors in the collateral framework",
                        
                        fontsize.labels=c(18,9),                # size of labels. Give the size per level of aggregation: size for group, size for subgroup, sub-subgroups...
                        fontcolor.labels=c("white","white"),    # Color of labels
                        fontface.labels=c(2,1),                  # Font of labels: 1,2,3,4 for normal, bold, italic, bold-italic...
                        bg.labels=c("transparent"),              # Background color of labels
                        align.labels=list(
                          c("left", "top"), 
                          c("left", "bottom")
                        ),                                   # Where to place labels in the rectangle?
                        overlap.labels=0.3,                      # number between 0 and 1 that determines the tolerance of the overlap between labels. 0 means that labels of lower levels are not printed if higher level labels overlap, 1  means that labels are always printed. In-between values, for instance the default value .5, means that lower level labels are printed if other labels do not overlap with more than .5  times their area size.
                        inflate.labels=F,                        # If true, labels are bigger when rectangle is bigger.
                        
)
dev.off()

# overview of type of financial instrument

security_type <- as.data.frame(table(total$security_type))



# share of renewable  and coal power capacity

p1<-plot_tech_share("RenewablesCap", "Share of renewable power capacity as percent of total capacity")
p1
ggsave(filename="plots/share_ren.png", width=30, height=15, units="cm", dpi=350)

p2<-plot_tech_share("CoalCap", "Share of coal power capacity as percent of total capacity")
p2
ggsave(filename="plots/share_coal.png", width=30, height=15, units="cm", dpi=350)



# map of financed oil, gas, coal power, renewable power, hydro power

plot_oil<-plot_ald_map("Oil", "Oil production (mbbl)")
ggsave(filename="plots/map_oil.png", width=30, height=15, units="cm", dpi=350)

plot_gas<-plot_ald_map("Gas", "Gas production (m3)")
ggsave(filename="plots/map_gas.png", width=30, height=15, units="cm", dpi=350)

plot_ren<-plot_ald_map("RenewablesCap", "Capacity in Renewable Energies (MW)")
ggsave(filename="plots/map_ren.png", width=30, height=15, units="cm", dpi=350)

plot_coal<-plot_ald_map("CoalCap", "Coal Capacity (MW)")
ggsave(filename="plots/map_coalcap.png", width=30, height=15, units="cm", dpi=350)

plot_hydro<-plot_ald_map("HydroCap", "Hydro Capacity (MW)")
ggsave(filename="plots/map_hydrocap.png", width=30, height=15, units="cm", dpi=350)



# plot: coal, oil coal power, renewables aggregate fiveyear expansion plans

fiveyear_oil <- fiveyeargrowth(tech_choice="Oil", geography="Global", plot_title="Oil Production", ytitle="Million Barrels Oil (mbbl)")
ggsave(filename="plots/fiveyear_oil.png", width=20, height=20, units="cm", dpi=350)

fiveyear_gas <- fiveyeargrowth(tech_choice="Gas", geography="Global", plot_title="Gas Production", ytitle="Cubic meters Gas (m3)")
ggsave(filename="plots/fiveyear_gas.png", width=20, height=20, units="cm", dpi=350)

fiveyear_coalcap <- fiveyeargrowth(tech_choice="CoalCap", geography="GlobalAggregate", plot_title="Coal Power Capacity", ytitle="Capacity (MW)")
ggsave(filename="plots/fiveyear_coalcap.png", width=20, height=20, units="cm", dpi=350)

fiveyear_oilcap <- fiveyeargrowth(tech_choice="OilCap", geography="GlobalAggregate", plot_title="Oil Power Capacity", ytitle="Capacity (MW)")
ggsave(filename="plots/fiveyear_oilcap.png", width=20, height=20, units="cm", dpi=350)

fiveyear_gascap <- fiveyeargrowth(tech_choice="GasCap", geography="GlobalAggregate", plot_title="Gas Power Capacity", ytitle="Capacity (MW)")
fiveyear_gascap
ggsave(filename="plots/fiveyear_gascap.png", width=20, height=20, units="cm", dpi=350)

fiveyear_ice <- fiveyeargrowth(tech_choice="ICE", geography="Global", plot_title="ICE Production", ytitle="Number of Vehicles")
fiveyear_ice
ggsave(filename="plots/fiveyear_ice.png", width=20, height=20, units="cm", dpi=350)

fiveyear_electric <- fiveyeargrowth(tech_choice="Electric", geography="Global", plot_title="Electric Vehicle Production", ytitle="Number of Vehicles")
fiveyear_electric
ggsave(filename="plots/fiveyear_electric.png", width=20, height=20, units="cm", dpi=350)

fiveyear_hybrid <- fiveyeargrowth(tech_choice="Hybrid", geography="Global", plot_title="Hybrid Vehicle Production", ytitle="Number of Vehicles")
fiveyear_hybrid
ggsave(filename="plots/fiveyear_hybrid.png", width=20, height=20, units="cm", dpi=350)

fiveyear_ren <- fiveyeargrowth(tech_choice="RenewablesCap", geography="GlobalAggregate", plot_title="Power Capacity from Renewable Energy", ytitle="Capacity (MW)")
fiveyear_ren
ggsave(filename="plots/fiveyear_ren.png", width=20, height=20, units="cm", dpi=350)






