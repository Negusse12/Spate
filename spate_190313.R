library(DAutilities)
library(decisionSupport)

#The file path is the same as the .Rproj (or relative to it)
filepath<-"D:/PhD/Turkana/spate/crop_production/Single_intervention/"  

make_variables<-function(est,n=1)
{ x<-random(rho=est, n=n)
for(i in colnames(x)) assign(i, as.numeric(x[1,i]),envir=.GlobalEnv)}

make_variables(estimate_read_csv(paste(filepath,"Turkana_estimates_190313.csv",sep="")))

spate_model<-function(x, varnames)
{
  ### Risks
  
  # Risk of improper design and implementation (in this we have two type of risks - the risk of 
  # improper design and the risk of poor interpretation and implementation of the design, which 
  # leads to either total structural failure or incure additional cost such as supervision cost 
  # and repair and maintenance
  
  # Risk of improper design
  # This risk is only in the first year and if there is a design problem, then the risk will persist 
  # until the end of the project life, unless the structure fails. Thus, I loop it accordingly.
  
  #improper_design_factor <- cummax(c(chance_event(prob_improper_design, value_if = 1, value_if_not = 0, n = 1),rep(0,n_years-1)))
    
  ## Risk of poor interpretation and implementation of the design and rejecting supervisors suggestion. 
   
   # The chance of highering supervisor is computed separately for ease of calculating the perecntage of cost, which 
   # expressed as percentage of total construction cost, and will calculate the actual supervision cost later 
   # (i.e. in the cost catagory).
  #chance_supervision<-c(rbinom(1,1,prob_supervision),rep(0,n_years-1))
  
  #reject_supervisor_suggestion<-cummax(chance_supervision*c(rbinom(1,1,prob_reject_suggestions),rep(0,n_years-1))) 
    
  # structural damage due to either improper design or poor interpretation and implementation of 
  # the design. This could lead to either maintenace of the structure or total structural damage.
    
  # For total structural damage
  failure_due_to_poor_implementation<-cummax(c(0, chance_event(prob_total_str_failure, value_if = 1, value_if_not = 0, n = n_years-1)))  
  
  #str_failure<-cummax(c(0, chance_event(prob_total_str_failure, value_if = 1, value_if_not = 0, n = n_years-1)))  
  #structural_failure<-pmax(improper_design_factor,reject_supervisor_suggestion)*str_failure
    
  # For repair and maintence
  # rep_maint_factor<-as.numeric(structural_failure==0)
   
  ## Risk of conflict and conflict management cost. This is the cost from the resource based conflicts, which
  # resulted from the land use change (i.e. the change from pasture area into framing area). This can happen
  # at any time and, thus, we use the chance event function.
    
  cost_conflict_management<-c(0, chance_event(prob_conflict, value_if = cost_managing_conflict, value_if_not = 0, n = n_years-1, CV_if= general_CV))
    
  ## Risk of increase in siltation and sedimentation
  siltation<-c(0, chance_event(prob_siltation, value_if = 1, value_if_not = 0, n = n_years-1))
    
  ## Risk of insufficeinet floodwater (i.e.due to low rainfall), which reduce crop productivity. In this case, 
  # drought is refered to the precipitation below the expected minimum threshold, and occurance_rainfall is 
  # equal to one when the rainfall is above the expected minimum threshold.
  
  # Crop yield reduction due to reduction in precipitation below the expected minimum threshold (i.e. drought)
  drought<- 1-chance_event(prob_yield_red_dry, value_if = perc_reduction_crop_yield/100, value_if_not = 0, n = n_years,CV_if=general_CV)
    
  # Risk of excess flooding. Here, the expected rainfall could be higher than the maximum threshold, which crops could
    # opitmally grow, that lead to flooding and loss in crop productivity. 
  
  yield_reduction_flooding<- 1-chance_event(prob_yield_red_flood, value_if = perc_reduction_crop_yield_flood/100, value_if_not = 0, n = n_years, CV_if=general_CV)
  
  ### Costs of the proposed intervention. The propsed intervention has the following cost catagories, which we catagorized them as
    # implementer's or project cost and the additional costs to the community and the environment. In the first part the implementer's 
    # costs are scripted. 
    
    ## Implementer's cost
    
    ## cost 1: Construction cost of the structure. The construction is expected to be completed in the first year. The estimate is for 
    # modern Spate irrigation infrastructure (i.e. headwork or diversion structure and required canals).
    
    construction_cost_stru<-c(construction_cost, rep(0, n_years-1))
    
    ## cost 2: Community awareness and mobilization cost. This is the cost that expected to spend for training and 
    # other community bizars.
    community_awareness_mobilization_cost<-c(training_cost, rep(0, n_years-1))
    
    ## cost 3: Cost of study and design
    cost_study_design<-c(study_design_cost, rep(0, n_years-1))
    
    ## cost 4: Watershed management cost. 
    watershed_management_cost<-c(watershed_area*perc_watershed_area_intervened/100*cost_watershed_mgmt, rep(0, n_years-1))
    
    ## cost 5: Repair and maintenance cost (first the total budget was eastimated and allocated for the annual 
    # repair and maintenace budget fo all years starting the second year. However, the actual cost is subject to
    # the repair and maintenance need.
    
    total_repair_maintenace_budget<-(construction_cost_stru[1]*perc_repair_maint_cost/100)/(n_years-1)
    annual_repair_cost<-c(0, rep(total_repair_maintenace_budget,n_years-1))
    
    # cost 6: Additional cost due to topography (i.e. having slopey area).Lomidat area has different 
    # topographic setting, which might lead to an additional cost of constructing the structure.
    
    slopey_area_cost_factor<- c(chance_event(prob_slopey_area, value_if = 1, value_if_not = 0, n = 1),rep(0, n_years-1))
    additional_cost_slopey<-construction_cost_stru*perc_increase_cost_due_to_slope/100*slopey_area_cost_factor
    
    # cost 7: The additional cost due to the recruitment of external supervisor is the product of the chance of 
    # supervision, percentage of the supervision cost, and the construction cost of the structure.
    
    #cost_supervision<-construction_cost_stru*perc_supervision_cost*chance_supervision
    
    
    ## total cost to the implementer is then computed by summing up all the above mentioned costs
    total_implementers_cost<- construction_cost_stru+community_awareness_mobilization_cost+
                              cost_study_design+watershed_management_cost+annual_repair_cost+
                              additional_cost_slopey
    ## Communal costs
    
    # cost 8: Cost of community contribution for repairing and maintainance of minor structural failures, which
    # is contributed in terms of labour. Because, for the material and financial aspect, the project will cover
    # from the repair and maintenaance budget. 
    
    cost_family_labour<-c(0,vv(family_labour_cost, general_CV, n_years-1))
    commun_structural_rm_contribution<- c(0, (vv(labour_contribution, general_CV,n_years-1)*cost_family_labour[2:n_years]))*
                                      (1-failure_due_to_poor_implementation)*siltation
    
    # cost 9:loss of alluvial deposite by having proper watershed management practice as well as diversion of the sediment
    # rich floods in to the farming area. This is a reduction in the donstream area. 
    cost_reduction_alluvial_deposite<-c(0, (vv(area_benefited_alluvial_deposits,general_CV,n_years-1)*
                                              vv(value_alluvial_deposits,general_CV,n_years-1 )))*(1-failure_due_to_poor_implementation)
                                           
    
    # cost 10: human health deterioration due to the water diversion practices and water logging
    human_health_cost <-c(0, (vv(no_people_affected,general_CV,n_years-1)*vv(cost_health_treatment,general_CV,n_years-1)))*
                        (1-failure_due_to_poor_implementation)
    
    # Cost 11: cost of lossing the current pasture production by shifting to crop production
    value_pasture_without_interv<-vv(current_value_pasture, general_CV, n_years)
    total_farming_area<-c(0,vv(total_area,general_CV, n_years-1))*(1-failure_due_to_poor_implementation)
    
    value_pasture_lost<-total_farming_area*prop_area_converted_crop*value_pasture_without_interv
    # Total communal cost
    total_communal_costs<-commun_structural_rm_contribution+cost_reduction_alluvial_deposite+human_health_cost+value_pasture_lost
    
    ### Communal benefits
    
    
    ## benefit 1: revenue from farming practice. This is the difference between the value of crop production and its residue
    # and the opperational costs, including other costs: pastor clearing and perimeter fencing costs). Though the literature 
    # suggested a production of twice per year, we consider a production of once a year (i.e. to be in a safe side and reducing 
    # the bias of over estimation). 
    
    price_crop<-vv(crop_price, general_CV, n_years)
    price_residue<-vv(residue_price, general_CV, n_years)
    
    crop_productivity<-c(0,vv(crop_production, general_CV, n_years-1, lower_limit = 0))
    crop_residue<-c(0,vv(crop_residue, general_CV, n_years-1, lower_limit = 0))
    
    farm_reveneu<-(total_farming_area*((crop_productivity*price_crop)+(crop_residue*price_residue)))*
                   drought*yield_reduction_flooding
    
    # Farm operational cost (it is the sum of labour cost and other input costs). This is an annual cost and starts in the
    # second year. Because, farming starts in the second year.
    
    farm_labour<-c(0,vv(farm_job_created, general_CV, n_years-1))
    other_farm_cost<-c(0,vv(farm_input_cost, general_CV, n_years-1))
    
    farm_operational_cost<-total_farming_area*((farm_job_created*cost_family_labour)+other_farm_cost)
    
    # pasture area clearing cost (i.e. to convert pasture area to crop area). This is only for the second year.
    
    clearing_cost_pasture<-rep(0, n_years)
    clearing_cost_pasture[2]<- pasture_clearing_cost
    
    pasture_area_clearing_cost<-total_farming_area*prop_area_converted_crop*clearing_cost_pasture
    
    # perimeter fencing of farm area. Here, we consider the cost for the perimetr fencing of the total farming area
    # and is expected to be done every five year
    perimeter_fencing_cost<-rep(0,n_years)
    perimeter_fencing_cost[c(2, 7, 12,17, 22)]<-perimeter_fenced*cost_fence
    
    #net farm profit
    farm_profit<-farm_reveneu-farm_operational_cost-pasture_area_clearing_cost-perimeter_fencing_cost
    
    ## benefit 2: capacity building due to training (mostly, these people are pastoralists and they are equiped 
    # with traditional knowledge of animal hussbandry. However, the government will train them on crop farming
    # practice and thus develop the capacity of the pastoralists, which is for those who has trained and adopted 
    # it.Here the experts estimated it based on the cost of acquiring this knowledge.
    total_beneficieries<-round (total_population*prop_trained*prop_adopted)
    
    capacity_development_benefit<-c(total_beneficieries*value_capacity_development, rep(0,n_years-1))
    
    ## benefit 3: employment opportunity created from the intervention
    
    # farm job opportunity
    farm_job_benefit<-total_farming_area*farm_job_created*cost_family_labour
    
    # off farm job opportunity
    off_farm_cost_labour<-vv(off_farm_labour_cost, general_CV, n_years)
    
    off_farm_job_benefit<-vv(off_farm_job, general_CV, n_years)*off_farm_cost_labour*(1-failure_due_to_poor_implementation)
    
    #construction job opportunity
    perc_job_construction_work<-c(construction_job_perc/100,rep(0, n_years-1))
    
    construction_job_benefit<-perc_job_construction_work*construction_cost_stru
    
    #watershed management job
    watershed_management_job_created<-c(watershed_mgmt_job,rep(0, n_years-1))
    
    watershed_management_job_benefit<-watershed_management_job_created*watershed_area*
      perc_watershed_area_intervened/100*off_farm_labour_cost
    
    #repair and maintenance job (this is the job opportunity created from the contribution of the community, which 
    # is the same as the one we consider it as communal cots above). This suggest, the overall impact on the community
    # is zero.
    
    repair_maintenance_job<-commun_structural_rm_contribution
    
    # total employment benefit (i.e. the sum of all employment benefits)
    total_employment_benefit<-farm_job_benefit+off_farm_job_benefit+construction_job_benefit+
      watershed_management_job_benefit+repair_maintenance_job
    
    ## benefit 4: benefit reducing flooding effect
    Occurance_flooding_hazard<-chance_event(prob_flooding_hazard,value_if = 1, value_if_not = 0, n = n_years)
    flood_hazard_reduction<-Occurance_flooding_hazard*c(0, vv(value_red_flooding_effect, general_CV, n_years-1))*
                            (1-failure_due_to_poor_implementation)
    
    ## total other complementary communal benefits (i.e. the sum of all benefits)
    
    total_communal_benefit<-farm_profit+capacity_development_benefit+total_employment_benefit+flood_hazard_reduction
    
    
    ## environmental impact of the propsed intervention
    
    # Environmental benefits. These benefits are mostly obtained from catchment restoration, which expressed as the
    # benefit of increase in vegetation, improve micro climate, and land reclamation.
    
    # Increase in vegetative cover and land reclaimed at the catchment area
    
    perc_annual_increase_veg_cov_density<-cumsum(c(0,vv(veg_cover_density, general_CV, n_years-1, lower_limit = 0, upper_limit = 1)))
    perc_annual_increase_veg_cov_density[perc_annual_increase_veg_cov_density>1]<-1
    
    perc_annual_area_reclaimed<-cumsum(c(0,vv(perc_area_reclaimed/100, general_CV, n_years-1, lower_limit = 0, upper_limit = 100)))
                                           
    perc_annual_area_reclaimed[perc_annual_area_reclaimed>1]<-1
    
    vegetative_cover_value<-vv(value_vegetative_cover, general_CV, n_years)
    area_reclaimed_value<-vv(value_reclaimed_area, general_CV, n_years)
    
    value_increase_vegetative_cover<-perc_annual_increase_veg_cov_density*watershed_area*perc_watershed_area_intervened/100*
                                     vegetative_cover_value
    
    value_increase_area_reclamation<-perc_annual_area_reclaimed*watershed_area*perc_watershed_area_intervened/100*
                                   area_reclaimed_value
    
    # improve in micro climate and ecosystem due to catchment management
    value_improvement_micro_climate<-vv(value_improve_ecosystem, general_CV, n_years)
    
    value_improvement_micro_climate<-watershed_area*perc_watershed_area_intervened/100*value_improvement_micro_climate*
      perc_annual_increase_veg_cov_density
    
    ##total environmental benefit of the proposed intervention
    total_environmental_benfit<-value_improvement_micro_climate+value_increase_vegetative_cover+value_increase_area_reclamation 
    
    # Environmental cost. This is a cost from the reduction in vegetation and farming area further below the diversion structure,
    # emission from the farming practices and deforestation for fencing.
    
    # Reduction in vegetative and forest area further down from the structure
    
    veg_forest_area_reduction_cost<- c(0,rep(vegetative_area_affected*value_vegetative_cover/100, n_years-1))*(1-failure_due_to_poor_implementation)
    
    # environmental cost due to soil disturbance and emission
    soil_exposed_perc<-c(0, vv(perc_soil_disturbed/100, general_CV, n_years-1, lower_limit = 0, upper_limit = 100))*(1-failure_due_to_poor_implementation)
    
    add_cost_soil_disturbance_emission<-c(0, vv(value_soil_disturbance, general_CV, n_years-1))
    
    cost_soil_disturbance_emission<-total_farming_area*soil_exposed_perc*add_cost_soil_disturbance_emission
    
    #Deforestation cost for fencing. Like perimeter fencing the cost of deferostation is computed every five years, because we hipotesized
    # the perimeter fenccing to be implemented every five years.
    deforestation_cost<-rep(0, n_years)
    deforestation_cost[c(2, 7, 12,17, 22)] <-perimeter_fenced*value_tree_cleared
    
    # total environmental cost of the proposed intervention
    total_environmental_cost<-veg_forest_area_reduction_cost+cost_soil_disturbance_emission+deforestation_cost
    
    # Cash flow (i.e. the sum of all undiscounted impacts - communal , implementer's and environmental) of the proposed
    # intervention. It is changed to dollar at an exchange rate of $1=100ksh.
    
    # Net communal effect
    
    communal_effect_cash_flow<-(total_communal_benefit-total_communal_costs)/exchange_rate
    
    # Expressing the communal outcome per person. Here we use the total number of farmers who trained and adopted 
    # the farming practice.
    income_per_farmer_cash_flow<-communal_effect_cash_flow/total_beneficieries
    income_per_farmer_cash_flow<-unlist(sapply(income_per_farmer_cash_flow,function(x) if(x==0) rnorm(n=1,0.1,0.0001) else x))
    
    # Net environmental effect
    environmental_effect_cash_flow<-(total_environmental_benfit-total_environmental_cost)/exchange_rate
    environmental_effect_cash_flow<-unlist(sapply(environmental_effect_cash_flow,function(x) if(x==0) rnorm(n=1,0.1,0.0001) else x))
    
    
    # Net implementer's effect
    implementer_cash_flow<--total_implementers_cost/exchange_rate
    implementer_cash_flow<-unlist(sapply(implementer_cash_flow,function(x) if(x==0) rnorm(n=1,0.1,0.0001) else x))
    
    
    # Net project effect (before discounting)
    overall_project_effect_cash_flow<-communal_effect_cash_flow+environmental_effect_cash_flow+implementer_cash_flow
    
    # Net overall project effect (discounted)
    overall_project_effect_NPV<-sum(NPV(communal_effect_cash_flow,discount_rate_farmers,TRUE))+sum(NPV(environmental_effect_cash_flow,discount_rate_farmers,TRUE))+
      sum(NPV(implementer_cash_flow,discount_rate,TRUE))
    
    return(list(Communal_effect_NPV=sum(NPV(communal_effect_cash_flow,discount_rate_farmers,TRUE)),
                Income_per_farmer_NPV=sum(NPV(income_per_farmer_cash_flow,discount_rate_farmers,TRUE)),
                Environmental_effect_NPV=sum(NPV(environmental_effect_cash_flow,discount_rate_farmers,TRUE)),
                Implementer_NPV=sum(NPV(implementer_cash_flow,discount_rate,TRUE)),
                Overall_project_effect_NPV=overall_project_effect_NPV,
                Cashflow_Communal_effect_NPV=communal_effect_cash_flow,
                Cashflow_Income_per_farmer_NPV=income_per_farmer_cash_flow,
                Cashflow_Environmental_effect_NPV=environmental_effect_cash_flow,
                Cashflow_Implementer_NPV=implementer_cash_flow,
                Cashflow_Overall_project_effect_NPV=overall_project_effect_cash_flow
                
                
    ))
    
}

decisionSupport(paste(filepath,"Turkana_estimates_190313.csv",sep=""), #input file with estimates
                paste(filepath,"MCResults",sep=""), #output folder
                write_table=TRUE,spate_model,10000,
                functionSyntax="plainNames")


#make evpi plots now with a real file
base_folder<-paste(filepath,"MCResults/",sep="")   #the folder where your inputs are
out_folder<-paste(filepath,"DAresults/",sep="")   #the folder where your outputs should go
dir.create(out_folder)
MC_file<-read.csv(paste(base_folder,"mcSimulationResults.csv",sep=""))
outvars<-list( "Communal_effect_NPV",
               "Income_per_farmer_NPV",
               "Environmental_effect_NPV",
               "Implementer_NPV",
               "Overall_project_effect_NPV"
               
)

labels<-list("Communal NPV",
             "NPV per beneficiary",
             "Environmental NPV",
             "Implementer's NPV",
             "Overall project NPV"
             
)

legendtable<-read.csv(paste(filepath,"Turkana_legend.csv",sep="")) # this one has to be prepared manually

for (i in seq_along(outvars))
{makeMCPlot(NPV_table=MC_file,variable_names=outvars[i],
            colorscheme="minus_plus_red_green",
            fonttype="serif",lwd=2,cex.axis=2,
            x_label=paste("Simulated",labels[i],sep=""),borderlines=TRUE,scaler="auto",fileformat="png",
            file_name=paste(out_folder,"MCplot_",labels[i],sep=""),
            percentile_remove=c(0.01,0.99))
  
  PLStab<-read.csv(paste(base_folder,outvars[i],"_pls_results.csv",sep=""))
  
  makePLSplot(PLS_table=PLStab,npls=7,plot_title=paste(labels[i],sep=""),cex.axis=1.5,
              lwd=0.1,legend_table=legendtable,
              fileformat="png",filename=paste(out_folder,"PLSplot_",labels[i],".png",sep=""))
}


mc_EVPI<-MC_file[,-grep("cashflow",colnames(MC_file),ignore.case = TRUE)]

empirical_EVPI(mc_EVPI,"Communal_effect_NPV",fileformat="png",outfolder=out_folder,write_table=TRUE)

dir.create(paste(filepath,"Compound figures/",sep=""))
for (outvar in outvars) 
{
  compound_figure(variable_name=outvar,
                  MC_table=MC_file,
                  PLS_table=read.csv(paste(base_folder,outvar,"_pls_results.csv",sep="")),
                  EVPI_table=read.csv(paste(out_folder,"EVPI_table_",outvar,".csv",sep="")),
                  cash_flow_vars=paste("Cashflow_",outvar,sep=""),
                  nbreaks=100,scaler="auto",percentile_remove=c(.01,.99),
                  npls=15,plsthreshold=0.8,colorscheme="quant_col",MCcolor="negpos",fonttype='sans',
                  borderlines=FALSE,lwd=1,
                  fileformat="png",filename=paste(filepath,"Compound figures/",outvar,sep=""),
                  legend_table=legendtable)
  
  
  
  
  
}
