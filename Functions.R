###################################
######### Useful functions ########
###################################

### Main analysis functions ###

### Plot preparation function 
plot_prep_f <- function(model_in) {
  
  # Remove intercept
  model_2_res <- model_in %>%  filter(!Variable=='(Intercept)')
  
  # Create reference level DF
  Ref_level <- data.frame(
    Variable = c(
      "Crop value: R$0",
      "Cattle density: Lowest",
      "Forest cover (2010): Lowest",
      "Waterbody: No",
      "Steep slope: No",
      "Farm class: Medium",
      "Elevation: Lowland",
      "Ruggedness: Lowest",
      "Road distance: Lowest",
      "Travel time: Lowest",
      "Forest management: Lowest",
      "Population density: Lowest",
      "Illiteracy rate: Lowest",
      "Non-profit presence: No"
    ),
    Odds = c(rep(1, 14)),
    Upper = c(rep(1, 14)),
    Lower = c(rep(1, 14)),
    Var_t = c(rep("Reference", 14))
  )
  
  # Combine
  model_2_res <- rbind(model_2_res, Ref_level)
  
  # Rename variables
  model_2_res$Variable <- factor(
    model_2_res$Variable,
    levels = c(
      # Crop value
      "Crop value: R$0",
      "fm_vh_m_cat1-2000",
      "fm_vh_m_cat2000-4000",
      "fm_vh_m_cat4000+",
      
      # Cattle density
      "Cattle density: Lowest",
      "cat_nh_m_levelLow" ,
      "cat_nh_m_levelHigh",
      "cat_nh_m_levelHighest",
      
      # Farm class
      "FM_classSmall Property",
      "Farm class: Medium",
      "FM_classLarge Property",
      
      # Forest cover (2010)
      "Forest cover (2010): Lowest",
      "For_p_10_levelLow",
      "For_p_10_levelHigh",
      "For_p_10_levelHighest",
      
      # Forest loss (1990-2010)
      "Defor_ss",
      
      # Waterbody
      "Waterbody: No",
      "Wat_011",
      
      # Slope 
      "Steep slope: No",
      "Slope_011",
      
      # Elevation
      "Elevation: Lowland" ,
      "Elev_01Upland",
      
      # Ruggedness
      "Ruggedness: Lowest" ,
      "Elev_sd_levelLow" ,
      "Elev_sd_levelHigh",
      "Elev_sd_levelHighest" ,
      
      # Road distance
      "Road distance: Lowest",
      "Rd_dis_levelLow",
      "Rd_dis_levelHigh",
      "Rd_dis_levelHighest",
      
      # Travel time
      "Travel time: Lowest",
      "Trl_time_levelLow",
      "Trl_time_levelHigh" ,
      "Trl_time_levelHighest" ,
      
      # Farms with forest management practices
      "Forest management: Lowest",
      "For_pract_levelLow",
      "For_pract_levelHigh",
      "For_pract_levelHighest",
      
      # Population density
      "Population density: Lowest",
      "Pop_den_levelLow",
      "Pop_den_levelHigh",
      "Pop_den_levelHighest",
      
      # HDI
      "HDI_I_ss",
      
      # Illiteracy
      "Illiteracy rate: Lowest",
      "Ill_rate_levelLow",
      "Ill_rate_levelHigh",
      "Ill_rate_levelHighest",
      
      # Votes
      "Votes_ss",
      
      # NGO
      "Non-profit presence: No",
      "NGO_p1",
      
      # Technical assistance
      "Ag_ass"
    )
  )
  
  model_2_res$Variable <- model_2_res$Variable %>%
    dplyr::recode(
      "fm_vh_m_cat1-2000" = 'Crop value: R$1-R$2000',
      "fm_vh_m_cat2000-4000" = 'Crop value: R$2000-R$4000',
      "fm_vh_m_cat4000+" = 'Crop value: R$4000+',
      
      "cat_nh_m_levelLow" = 'Cattle density: Low',
      "cat_nh_m_levelHigh" = 'Cattle density: High',
      "cat_nh_m_levelHighest" = 'Cattle density: Highest',
      
      "FM_classSmall Property" = 'Farm class: Small',
      "FM_classLarge Property" = 'Farm class: Large',
      
      "For_p_10_levelLow" = 'Forest cover (2010): Low',
      "For_p_10_levelHigh" = 'Forest cover (2010): High',
      "For_p_10_levelHighest" = 'Forest cover (2010): Highest',
      
      "Defor_ss" = 'Forest loss (1990-2010)',
      
      "Wat_011" = 'Waterbody: Yes',
      
      "Slope_011" = 'Steep slope: Yes',
      
      "Elev_01Upland" = 'Elevation: Upland',
      
      "Elev_sd_levelLow" = 'Ruggedness: Low',
      "Elev_sd_levelHigh" = 'Ruggedness: High',
      "Elev_sd_levelHighest" = 'Ruggedness: Highest',
      
      "Rd_dis_levelLow" = 'Road distance: Low',
      "Rd_dis_levelHigh" = 'Road distance: High',
      "Rd_dis_levelHighest" = 'Road distance: Highest',
      
      "Trl_time_levelLow" = 'Travel time: Low',
      "Trl_time_levelHigh" = 'Travel time: High',
      "Trl_time_levelHighest" = 'Travel time: Highest',
      
      "For_pract_levelLow" = 'Forest management: Low',
      "For_pract_levelHigh" = 'Forest management: High',
      "For_pract_levelHighest" = 'Forest management: Highest',
      
      "Pop_den_levelLow" = 'Population density: Low',
      "Pop_den_levelHigh" = 'Population density: High',
      "Pop_den_levelHighest" = 'Population density: Highest',
      
      "HDI_I_ss" = "HDI - income",
      
      "Ill_rate_levelLow" = 'Illiteracy rate: Low',
      "Ill_rate_levelHigh" = 'Illiteracy rate: High',
      "Ill_rate_levelHighest" = 'Illiteracy rate: Highest',
      
      "Votes_ss" = 'Votes',
      
      "NGO_p1" = 'Non-profit presence: Yes',
      
      "Ag_ass"  = 'Technical assistance'
    )
  
  ### Split labels over multiple lines
  model_2_res$Variable_split <-
    as.factor(sapply(
      strwrap(model_2_res$Variable, 30, simplify = FALSE),
      paste,
      collapse = "\n"
    ))
  model_2_res <- with(model_2_res, model_2_res[order(Variable), ])
  model_2_res$Variable_split <-
    factor(model_2_res$Variable_split,
           levels = unique(model_2_res$Variable_split[order(model_2_res$Variable)]))
  return(model_2_res)
}


### Main plotting function 
plot_do_f <- function(model_res) {
  
  ### Variables in each sub-plot 
  Innovation_vars <- c("Crop value: R$0", "Crop value: R$1-R$2000","Crop value: R$2000-R$4000", "Crop value: R$4000+","Cattle density: Lowest"  ,
                       "Cattle density: Low","Cattle density: High","Cattle density: Highest" ,"Farm class: Small","Farm class: Medium","Farm class: Large"  ,
                       "Forest cover (2010): Lowest","Forest cover (2010): Low" ,      
                       "Forest cover (2010): High","Forest cover (2010): Highest","Forest loss (1990-2010)") 
  
  Adopter_vars <- c("Waterbody: No", "Waterbody: Yes", "Steep slope: No","Steep slope: Yes",   "Elevation: Lowland"   , 
                    "Elevation: Upland",  "Ruggedness: Lowest","Ruggedness: Low","Ruggedness: High","Ruggedness: Highest" ,  
                    "Road distance: Lowest","Road distance: Low","Road distance: High","Road distance: Highest" ,"Travel time: Lowest" ,  
                    "Travel time: Low","Travel time: High","Travel time: Highest")
  
  Context_vars <- c("Forest management: Lowest","Forest management: Low","Forest management: High","Forest management: Highest" ,
                    "Population density: Lowest", "Population density: Low","Population density: High","Population density: Highest", "HDI - income",
                    "Illiteracy rate: Lowest","Illiteracy rate: Low","Illiteracy rate: High","Illiteracy rate: Highest" ,  
                    "Votes", "Non-profit presence: No", "Non-profit presence: Yes", "Technical assistance")
  
  # Subset to desired variables 
  model_res_Innovation <- model_res[model_res$Variable_split %in% Innovation_vars,]
  model_res_Adopter <- model_res[model_res$Variable_split %in% Adopter_vars,]
  model_res_Context <- model_res[model_res$Variable_split %in% Context_vars,]
  
  # Drop unused levels
  model_res_Innovation <- droplevels(model_res_Innovation)
  model_res_Adopter <- droplevels(model_res_Adopter)
  model_res_Context <- droplevels(model_res_Context)
  
  # Plot the graph
  p.1 <-
    ggplot(model_res_Innovation,
           aes(
             x = Variable_split,
             y = Odds,
             ymin = Lower,
             ymax = Upper
           )) +
    geom_linerange(fatten = 0.5, size = 0.5) +
    geom_point(aes(shape = Var_t), size = 1) +
    scale_shape_manual(values = c(1, 16)) +
    scale_colour_manual(values = c("white", "black")) +
    ylab("Odds")
  p.1 <- p.1 + ylim(0.13, 3.04)
  p.1 <-
    p.1 + geom_hline(yintercept = 1,
                     color = "#8b0000",
                     size = 0.2)
  p.1 <- p.1 + coord_flip()
  p.1 <-
    p.1 + scale_x_discrete(breaks = c(levels(model_res_Innovation$Variable_split)),
                           limits = rev(
                             c(
                               levels(model_res_Innovation$Variable_split)[1:4],
                               "skip",
                               levels(model_res_Innovation$Variable_split)[5:8],
                               "skip",
                               levels(model_res_Innovation$Variable_split)[9:11],
                               "skip",
                               levels(model_res_Innovation$Variable_split)[12:15],
                               "skip",
                               levels(model_res_Innovation$Variable_split)[16]
                             )
                           ))  +
    theme_minimal() +
    theme(
      axis.text.y.right = element_blank(),
      axis.ticks.y.right = element_blank(),
      axis.line.y.right = element_blank(),
      axis.title.y = element_blank()) 

  p.1 <- p.1 + theme(text = element_text(size = 8))
  p.1 <- p.1 + theme(legend.position = "none")  +
    theme(plot.margin = unit(c(0, 0, 0, 0), 'lines')) 
  
  # Plot the graph
  p.2 <-
    ggplot(model_res_Adopter,
           aes(
             x = Variable_split,
             y = Odds,
             ymin = Lower,
             ymax = Upper
           )) +
    geom_linerange(fatten = 0.5, size = 0.5) +
    geom_point(aes(shape = Var_t), size = 1) +
    scale_shape_manual(values = c(1, 16)) +
    scale_colour_manual(values = c("white", "black")) +
    ylab("Odds")
  p.2 <- p.2 + ylim(0.13, 3.04)
  p.2 <-
    p.2 + geom_hline(yintercept = 1,
                     color = "#8b0000",
                     size = 0.2)
  p.2 <- p.2 + coord_flip()
  p.2 <-
    p.2 + scale_x_discrete(breaks = c(levels(model_res_Adopter$Variable_split)),
                           limits = rev(
                             c(levels(model_res_Adopter$Variable_split)[1:2],
                               "skip",
                               levels(model_res_Adopter$Variable_split)[3:4],
                               "skip",
                               levels(model_res_Adopter$Variable_split)[5:6],
                               "skip",
                               levels(model_res_Adopter$Variable_split)[7:10],
                               "skip",
                               levels(model_res_Adopter$Variable_split)[11:14],
                               "skip",
                               levels(model_res_Adopter$Variable_split)[15:18]
                             )
                           ))  +
    theme_minimal() +
    theme(
      axis.text.y.right = element_blank(),
      axis.ticks.y.right = element_blank(),
      axis.line.y.right = element_blank(),
      axis.title.y = element_blank()
    ) 
  
  p.2 <- p.2 + theme(text = element_text(size = 8))
  p.2 <- p.2 + theme(legend.position = "none") +
    theme(plot.margin = unit(c(0, 0, 0, 0), 'lines'))
  
  # Plot the graph
  p.3 <-
    ggplot(model_res_Context,
           aes(
             x = Variable_split,
             y = Odds,
             ymin = Lower,
             ymax = Upper
           )) +
    geom_linerange(fatten = 0.5, size = 0.5) +
    geom_point(aes(shape = Var_t), size = 1) +
    scale_shape_manual(values = c(1, 16)) +
    scale_colour_manual(values = c("white", "black")) +
    ylab("Odds")
  p.3 <- p.3 + ylim(0, 3.04)
  p.3 <-
    p.3 + geom_hline(yintercept = 1,
                     color = "#8b0000",
                     size = 0.2)
  p.3 <- p.3 + coord_flip()
  p.3 <-
    p.3 + scale_x_discrete(breaks = c(levels(model_res_Context$Variable_split)),
                           limits = rev(
                             c(
                               levels(model_res_Context$Variable_split)[1:4],
                               "skip",
                               levels(model_res_Context$Variable_split)[5:8],
                               "skip",
                               levels(model_res_Context$Variable_split)[9],
                               "skip",
                               levels(model_res_Context$Variable_split)[10:13],
                               "skip",
                               levels(model_res_Context$Variable_split)[14],
                               "skip",
                               levels(model_res_Context$Variable_split)[15:16],
                               "skip",
                               levels(model_res_Context$Variable_split)[17]
                             )
                           ))  +
    theme_minimal() +
    theme(
      axis.text.y.right = element_blank(),
      axis.ticks.y.right = element_blank(),
      axis.line.y.right = element_blank(),
      axis.title.y = element_blank()
    )
  
  p.3 <- p.3 + theme(text = element_text(size = 8))
  p.3 <- p.3 + theme(legend.position = "none") +
    theme(plot.margin = unit(c(0, 0, 0, 0), 'lines'))
  
  
  # Merge
  p <- ggpubr::ggarrange(p.1, p.2, p.3, ncol = 3,  align = "v")
  
  # Ruturn the plot
  return(p)
}


### Sub-analysis functions ###
# Create function
plot_prep_f_sz <- function(model_in) {
  
  # Remove intercept
  model_2_res <- model_in %>%  filter(!Variable=='(Intercept)')
  
  # Create reference level DF
  Ref_level <- data.frame(
    Variable = c(
      "Crop value: R$0",
      "Cattle density: Lowest",
      "Forest cover (2010): Lowest",
      "Waterbody: No",
      "Steep slope: No",
      "Elevation: Lowland",
      "Ruggedness: Lowest",
      "Road distance: Lowest",
      "Travel time: Lowest",
      "Forest management: Lowest",
      "Population density: Lowest",
      "Illiteracy rate: Lowest",
      "Non-profit presence: No"
    ),
    Odds = c(rep(1, 13)),
    Upper = c(rep(1, 13)),
    Lower = c(rep(1, 13)),
    Var_t = c(rep("Reference", 13))
  )
  
  # Combine
  model_2_res <- rbind(model_2_res, Ref_level)
  
  # Rename variables
  model_2_res$Variable <- factor(
    model_2_res$Variable,
    levels = c(
      # Crop value
      "Crop value: R$0",
      "fm_vh_m_cat1-2000",
      "fm_vh_m_cat2000-4000",
      "fm_vh_m_cat4000+",
      
      # Cattle density
      "Cattle density: Lowest",
      "cat_nh_m_levelLow" ,
      "cat_nh_m_levelHigh",
      "cat_nh_m_levelHighest",
      
      # Forest cover (2010)
      "Forest cover (2010): Lowest",
      "For_p_10_levelLow",
      "For_p_10_levelHigh",
      "For_p_10_levelHighest",
      
      # Forest loss (1990-2010)
      "Defor_ss",
      
      # Waterbody
      "Waterbody: No",
      "Wat_011",
      
      # Slope 
      "Steep slope: No",
      "Slope_011",
      
      # Elevation
      "Elevation: Lowland" ,
      "Elev_01Upland",
      
      # Ruggedness
      "Ruggedness: Lowest" ,
      "Elev_sd_levelLow" ,
      "Elev_sd_levelHigh",
      "Elev_sd_levelHighest" ,
      
      # Road distance
      "Road distance: Lowest",
      "Rd_dis_levelLow",
      "Rd_dis_levelHigh",
      "Rd_dis_levelHighest",
      
      # Travel time
      "Travel time: Lowest",
      "Trl_time_levelLow",
      "Trl_time_levelHigh" ,
      "Trl_time_levelHighest" ,
      
      # Farms with forest management practices
      "Forest management: Lowest",
      "For_pract_levelLow",
      "For_pract_levelHigh",
      "For_pract_levelHighest",
      
      # Population density
      "Population density: Lowest",
      "Pop_den_levelLow",
      "Pop_den_levelHigh",
      "Pop_den_levelHighest",
      
      # HDI
      "HDI_I_ss",
      
      # Illiteracy
      "Illiteracy rate: Lowest",
      "Ill_rate_levelLow",
      "Ill_rate_levelHigh",
      "Ill_rate_levelHighest",
      
      # Votes
      "Votes_ss",
      
      # NGO
      "Non-profit presence: No",
      "NGO_p1",
      
      # Technical assistance
      "Ag_ass"
    )
  )
  
  model_2_res$Variable <- model_2_res$Variable %>%
    dplyr::recode(
      "fm_vh_m_cat1-2000" = 'Crop value: R$1-R$2000',
      "fm_vh_m_cat2000-4000" = 'Crop value: R$2000-R$4000',
      "fm_vh_m_cat4000+" = 'Crop value: R$4000+',
      
      "cat_nh_m_levelLow" = 'Cattle density: Low',
      "cat_nh_m_levelHigh" = 'Cattle density: High',
      "cat_nh_m_levelHighest" = 'Cattle density: Highest',
      
      "For_p_10_levelLow" = 'Forest cover (2010): Low',
      "For_p_10_levelHigh" = 'Forest cover (2010): High',
      "For_p_10_levelHighest" = 'Forest cover (2010): Highest',
      
      "Defor_ss" = 'Forest loss (1990-2010)',
      
      "Wat_011" = 'Waterbody: Yes',
      
      "Slope_011" = 'Steep slope: Yes',
      
      "Elev_01Upland" = 'Elevation: Upland',
      
      "Elev_sd_levelLow" = 'Ruggedness: Low',
      "Elev_sd_levelHigh" = 'Ruggedness: High',
      "Elev_sd_levelHighest" = 'Ruggedness: Highest',
      
      "Rd_dis_levelLow" = 'Road distance: Low',
      "Rd_dis_levelHigh" = 'Road distance: High',
      "Rd_dis_levelHighest" = 'Road distance: Highest',
      
      "Trl_time_levelLow" = 'Travel time: Low',
      "Trl_time_levelHigh" = 'Travel time: High',
      "Trl_time_levelHighest" = 'Travel time: Highest',
      
      "For_pract_levelLow" = 'Forest management: Low',
      "For_pract_levelHigh" = 'Forest management: High',
      "For_pract_levelHighest" = 'Forest management: Highest',
      
      "Pop_den_levelLow" = 'Population density: Low',
      "Pop_den_levelHigh" = 'Population density: High',
      "Pop_den_levelHighest" = 'Population density: Highest',
      
      "HDI_I_ss" = "HDI - income",
      
      "Ill_rate_levelLow" = 'Illiteracy rate: Low',
      "Ill_rate_levelHigh" = 'Illiteracy rate: High',
      "Ill_rate_levelHighest" = 'Illiteracy rate: Highest',
      
      "Votes_ss" = 'Votes',
      
      "NGO_p1" = 'Non-profit presence: Yes',
      
      "Ag_ass"  = 'Technical assistance'
    )
  
  ### Split labels over multiple lines
  model_2_res$Variable_split <-
    as.factor(sapply(
      strwrap(model_2_res$Variable, 30, simplify = FALSE),
      paste,
      collapse = "\n"
    ))
  model_2_res <- with(model_2_res, model_2_res[order(Variable), ])
  model_2_res$Variable_split <-
    factor(model_2_res$Variable_split,
           levels = unique(model_2_res$Variable_split[order(model_2_res$Variable)]))
  return(model_2_res)
}


### Main plotting function 
plot_do_f_sz <- function(model_res) {
  
  ### Variables in each sub-plot 
  Innovation_vars <- c("Crop value: R$0", "Crop value: R$1-R$2000","Crop value: R$2000-R$4000", "Crop value: R$4000+","Cattle density: Lowest"  ,
                       "Cattle density: Low","Cattle density: High","Cattle density: Highest" , "Forest cover (2010): Lowest","Forest cover (2010): Low" ,      
                       "Forest cover (2010): High","Forest cover (2010): Highest","Forest loss (1990-2010)") 
  
  Adopter_vars <- c("Waterbody: No", "Waterbody: Yes", "Steep slope: No","Steep slope: Yes", "Elevation: Lowland"   , 
                    "Elevation: Upland",  "Ruggedness: Lowest","Ruggedness: Low","Ruggedness: High","Ruggedness: Highest" ,  
                    "Road distance: Lowest","Road distance: Low","Road distance: High","Road distance: Highest" ,"Travel time: Lowest" ,  
                    "Travel time: Low","Travel time: High","Travel time: Highest")
  
  Context_vars <- c("Forest management: Lowest","Forest management: Low","Forest management: High","Forest management: Highest" ,
                    "Population density: Lowest", "Population density: Low","Population density: High","Population density: Highest", "HDI - income",
                    "Illiteracy rate: Lowest","Illiteracy rate: Low","Illiteracy rate: High","Illiteracy rate: Highest" ,  
                    "Votes", "Non-profit presence: No", "Non-profit presence: Yes", "Technical assistance")
  
  # Subset to desired variables 
  model_res_Innovation <- model_res[model_res$Variable_split %in% Innovation_vars,]
  model_res_Adopter <- model_res[model_res$Variable_split %in% Adopter_vars,]
  model_res_Context <- model_res[model_res$Variable_split %in% Context_vars,]
  
  # Drop unused levels
  model_res_Innovation <- droplevels(model_res_Innovation)
  model_res_Adopter <- droplevels(model_res_Adopter)
  model_res_Context <- droplevels(model_res_Context)
  
  # Plot the graph
  p.1 <-
    ggplot(model_res_Innovation,
           aes(
             x = Variable_split,
             y = Odds,
             ymin = Lower,
             ymax = Upper, 
             fill = Property, 
             group = Property,
             color = Property)) +
    geom_point(
      data = subset(model_res_Innovation, Var_t=="Reference"),
      aes(colour = "Black", shape = Var_t), size = 1, show.legend = F) + 
    
    geom_point(
      data = subset(model_res_Innovation, Var_t!="Reference"),
      aes(colour = Property, shape = Var_t),
      size = 1, position = position_dodge(width = 0.5), show.legend = F) +
    
    geom_linerange(
      data = subset(model_res_Innovation, Var_t=="Reference"),
      fatten = 0.5, size = 0.5) + 
    
    geom_linerange(
      data = subset(model_res_Innovation, Var_t!="Reference"),
      fatten = 0.5, size = 0.5, position = position_dodge(width = 0.5)) +
    
    
    scale_shape_manual(values = c(1, 16)) +
    scale_colour_manual(values = c("#225ea8" , "#41b6c4", "#a1dab4","black"), breaks = c("Large", "Medium", "Small")) +
    ylab("Odds") 
  #p.1 <- p.1 + ylim(0.02, 4.37)
  
  
  p.1 <-
    p.1 + geom_hline(yintercept = 1,
                     color = "#8b0000",
                     size = 0.2)
  p.1 <- p.1 + coord_flip()
  p.1 <-
    p.1 + scale_x_discrete(breaks = c(levels(model_res_Innovation$Variable_split)),
                           limits = rev(
                             c(
                               levels(model_res_Innovation$Variable_split)[1:4],
                               "skip",
                               levels(model_res_Innovation$Variable_split)[5:8],
                               "skip",
                               levels(model_res_Innovation$Variable_split)[9:12],
                               "skip",
                               levels(model_res_Innovation$Variable_split)[13]
                             )
                           ))  +
    theme_minimal() +
    theme(
      axis.text.y.right = element_blank(),
      axis.ticks.y.right = element_blank(),
      axis.line.y.right = element_blank(),
      axis.title.y = element_blank()
    ) # +
  # labs(title = "a)",
  #      plot.title = element_text(
  #        size = 11,
  #        face = "bold",
  #        colour = "black"
  #      ))
  p.1 <- p.1 + theme(text = element_text(size = 8))
  p.1 <- p.1 + theme(legend.position = "none")  +
    theme(plot.margin = unit(c(0, 0, 0, 0), 'lines'))
  
  
  # Plot the graph
  p.2 <-
    ggplot(model_res_Adopter,
           aes(
             x = Variable_split,
             y = Odds,
             ymin = Lower,
             ymax = Upper, 
             fill = Property, 
             group = Property,
             color = Property)) +
    geom_point(
      data = subset(model_res_Adopter, Var_t=="Reference"),
      aes(colour = "Black", shape = Var_t), size = 1, show.legend = F) + 
    
    geom_point(
      data = subset(model_res_Adopter, Var_t!="Reference"),
      aes(colour = Property, shape = Var_t),
      size = 1, position = position_dodge(width = 0.5), show.legend = F) +
    
    geom_linerange(
      data = subset(model_res_Adopter, Var_t=="Reference"),
      fatten = 0.5, size = 0.5) + 
    
    geom_linerange(
      data = subset(model_res_Adopter, Var_t!="Reference"),
      fatten = 0.5, size = 0.5, position = position_dodge(width = 0.5)) +
    
    
    scale_shape_manual(values = c(1, 16)) +
    scale_colour_manual(values = c("#225ea8" , "#41b6c4", "#a1dab4","black"), breaks = c("Large", "Medium", "Small")) +
    ylab("Odds") 
  #p.2 <- p.2 + ylim(0.02, 4.37)
  
  p.2 <-
    p.2 + geom_hline(yintercept = 1,
                     color = "#8b0000",
                     size = 0.2)
  p.2 <- p.2 + coord_flip()
  p.2 <-
    p.2 + scale_x_discrete(breaks = c(levels(model_res_Adopter$Variable_split)),
                           limits = rev(
                             c(
                               levels(model_res_Adopter$Variable_split)[1:2],
                               "skip",
                               levels(model_res_Adopter$Variable_split)[3:4],
                               "skip",
                               levels(model_res_Adopter$Variable_split)[5:6],
                               "skip",
                               levels(model_res_Adopter$Variable_split)[7:10],
                               "skip",
                               levels(model_res_Adopter$Variable_split)[11:14],
                               "skip",
                               levels(model_res_Adopter$Variable_split)[15:18]
                             )
                           ))  +
    theme_minimal() +
    theme(
      axis.text.y.right = element_blank(),
      axis.ticks.y.right = element_blank(),
      axis.line.y.right = element_blank(),
      axis.title.y = element_blank()
    ) #+
  # labs(title = "b)",
  #      plot.title = element_text(
  #        size = 11,
  #        face = "bold",
  #        colour = "black"
  #      ))
  p.2 <- p.2 + theme(text = element_text(size = 8))
  p.2 <- p.2 + theme(legend.position = "none") +
    theme(plot.margin = unit(c(0, 0, 0, 0), 'lines'))
  
  
  # Plot the graph
  p.3 <-
    ggplot(model_res_Context,
           aes(
             x = Variable_split,
             y = Odds,
             ymin = Lower,
             ymax = Upper, 
             fill = Property, 
             group = Property,
             color = Property)) +
    geom_point(
      data = subset(model_res_Context, Var_t=="Reference"),
      aes(colour = "Black", shape = Var_t), size = 1, show.legend = F) + 
    
    geom_point(
      data = subset(model_res_Context, Var_t!="Reference"),
      aes(colour = Property, shape = Var_t),
      size = 1, position = position_dodge(width = 0.5), show.legend = F) +
    
    geom_linerange(
      data = subset(model_res_Context, Var_t=="Reference"),
      fatten = 0.5, size = 0.5) + 
    
    geom_linerange(
      data = subset(model_res_Context, Var_t!="Reference"),
      fatten = 0.5, size = 0.5, position = position_dodge(width = 0.5)) +
    
    scale_shape_manual(values = c(1, 16)) +
    scale_colour_manual(values = c("#225ea8" , "#41b6c4", "#a1dab4","black"), breaks = c("Large", "Medium", "Small")) +
    ylab("Odds") 
  #p.3 <- p.3 + ylim(0.02, 4.37)
  
  p.3 <-
    p.3 + geom_hline(yintercept = 1,
                     color = "#8b0000",
                     size = 0.2)
  p.3 <- p.3 + coord_flip()
  p.3 <-
    p.3 + scale_x_discrete(breaks = c(levels(model_res_Context$Variable_split)),
                           limits = rev(
                             c(
                               levels(model_res_Context$Variable_split)[1:4],
                               "skip",
                               levels(model_res_Context$Variable_split)[5:8],
                               "skip",
                               levels(model_res_Context$Variable_split)[9],
                               "skip",
                               levels(model_res_Context$Variable_split)[10:13],
                               "skip",
                               levels(model_res_Context$Variable_split)[14],
                               "skip",
                               levels(model_res_Context$Variable_split)[15:16],
                               "skip",
                               levels(model_res_Context$Variable_split)[17]
                             )
                           ))  +
    theme_minimal() +
    theme(
      axis.text.y.right = element_blank(),
      axis.ticks.y.right = element_blank(),
      axis.line.y.right = element_blank(),
      axis.title.y = element_blank()
    ) #+
  # labs(title = "c)",
  #      plot.title = element_text(
  #        size = 11,
  #        face = "bold",
  #        colour = "black"
  #      ))
  p.3 <- p.3 + theme(text = element_text(size = 8))
  p.3 <- p.3 + theme(legend.position = "none") +
    theme(plot.margin = unit(c(0, 0, 0, 0), 'lines'))
  
  
  # Merge
  p <- ggpubr::ggarrange(p.1, p.2, p.3, ncol = 3,  align = "v", common.legend = T, legend = "bottom")
  
  # Ruturn the plot
  return(p)
}

### Function for running the iterations of models 
# Create function
plot_prep_f_comb <- function(model_in) {
  
  # Remove intercept
  model_2_res <- model_in %>%  filter(!Variable=='(Intercept)')
  
  # Rename variables
  model_2_res$Variable <- factor(
    model_2_res$Variable,
    levels = rev(c(
      # Crop value
      "Crop value: R$0",
      "fm_vh_m_cat1-2000",
      "fm_vh_m_cat2000-4000",
      "fm_vh_m_cat4000+",
      
      # Cattle density
      "Cattle density: Lowest",
      "cat_nh_m_levelLow" ,
      "cat_nh_m_levelHigh",
      "cat_nh_m_levelHighest",
      
      # Forest cover (2010)
      "Forest cover (2010): Lowest",
      "For_p_10_levelLow",
      "For_p_10_levelHigh",
      "For_p_10_levelHighest",
      
      # Forest loss (1990-2010)
      "Defor_ss",
      
      # Waterbody
      "Waterbody: No",
      "Wat_011",
      
      # Slope 
      "Steep slope: No",
      "Slope_011",
      
      # Farm class
      "FM_classSmall Property",
      "Farm class: Medium",
      "FM_classLarge Property",
      
      # Elevation
      "Elevation: Lowland" ,
      "Elev_01Upland",
      
      # Ruggedness
      "Ruggedness: Lowest" ,
      "Elev_sd_levelLow" ,
      "Elev_sd_levelHigh",
      "Elev_sd_levelHighest" ,
      
      # Road distance
      "Road distance: Lowest",
      "Rd_dis_levelLow",
      "Rd_dis_levelHigh",
      "Rd_dis_levelHighest",
      
      # Travel time
      "Travel time: Lowest",
      "Trl_time_levelLow",
      "Trl_time_levelHigh" ,
      "Trl_time_levelHighest" ,
      
      # Farms with forest management practices
      "Forest management: Lowest",
      "For_pract_levelLow",
      "For_pract_levelHigh",
      "For_pract_levelHighest",
      
      # Population density
      "Population density: Lowest",
      "Pop_den_levelLow",
      "Pop_den_levelHigh",
      "Pop_den_levelHighest",
      
      # HDI
      "HDI_I_ss",
      
      # Illiteracy
      "Illiteracy rate: Lowest",
      "Ill_rate_levelLow",
      "Ill_rate_levelHigh",
      "Ill_rate_levelHighest",
      
      # Votes
      "Votes_ss",
      
      # NGO
      "Non-profit presence: No",
      "NGO_p1",
      
      # Technical assistance
      "Ag_ass"
    )
    ))
  
  model_2_res$Variable <- model_2_res$Variable %>%
    dplyr::recode(
      "fm_vh_m_cat1-2000" = 'Crop value: R$1-R$2000',
      "fm_vh_m_cat2000-4000" = 'Crop value: R$2000-R$4000',
      "fm_vh_m_cat4000+" = 'Crop value: R$4000+',
      
      "cat_nh_m_levelLow" = 'Cattle density: Low',
      "cat_nh_m_levelHigh" = 'Cattle density: High',
      "cat_nh_m_levelHighest" = 'Cattle density: Highest',
      
      "For_p_10_levelLow" = 'Forest cover (2010): Low',
      "For_p_10_levelHigh" = 'Forest cover (2010): High',
      "For_p_10_levelHighest" = 'Forest cover (2010): Highest',
      
      "Defor_ss" = 'Forest loss (1990-2010)',
      
      "Wat_011" = 'Waterbody: Yes',
      
      "Slope_011" = 'Steep slope: Yes',
      
      "FM_classSmall Property" = 'Farm class: Small',
      "FM_classLarge Property" = 'Farm class: Large',
      
      "Elev_01Upland" = 'Elevation: Upland',
      
      "Elev_sd_levelLow" = 'Ruggedness: Low',
      "Elev_sd_levelHigh" = 'Ruggedness: High',
      "Elev_sd_levelHighest" = 'Ruggedness: Highest',
      
      "Rd_dis_levelLow" = 'Road distance: Low',
      "Rd_dis_levelHigh" = 'Road distance: High',
      "Rd_dis_levelHighest" = 'Road distance: Highest',
      
      "Trl_time_levelLow" = 'Travel time: Low',
      "Trl_time_levelHigh" = 'Travel time: High',
      "Trl_time_levelHighest" = 'Travel time: Highest',
      
      "For_pract_levelLow" = 'Forest management: Low',
      "For_pract_levelHigh" = 'Forest management: High',
      "For_pract_levelHighest" = 'Forest management: Highest',
      
      "Pop_den_levelLow" = 'Population density: Low',
      "Pop_den_levelHigh" = 'Population density: High',
      "Pop_den_levelHighest" = 'Population density: Highest',
      
      "HDI_I_ss" = "HDI - income",
      
      "Ill_rate_levelLow" = 'Illiteracy rate: Low',
      "Ill_rate_levelHigh" = 'Illiteracy rate: High',
      "Ill_rate_levelHighest" = 'Illiteracy rate: Highest',
      
      "Votes_ss" = 'Votes',
      
      "NGO_p1" = 'Non-profit presence: Yes',
      
      "Ag_ass"  = 'Technical assistance'
    )
  
  ### Split labels over multiple lines
  model_2_res$Variable_split <-
    as.factor(sapply(
      strwrap(model_2_res$Variable, 30, simplify = FALSE),
      paste,
      collapse = "\n"
    ))
  model_2_res <- with(model_2_res, model_2_res[order(Variable), ])
  model_2_res$Variable_split <-
    factor(model_2_res$Variable_split,
           levels = unique(model_2_res$Variable_split[order(model_2_res$Variable)]))
  return(model_2_res)
}


### Including multiple models 
plot_do_f_comb <- function(model_res) {
  ### Variables in each sub-plot 
  Innovation_vars <- c("Crop value: R$0", "Crop value: R$1-R$2000","Crop value: R$2000-R$4000", "Crop value: R$4000+","Cattle density: Lowest"  ,
                       "Cattle density: Low","Cattle density: High","Cattle density: Highest" , "Forest cover (2010): Lowest","Forest cover (2010): Low" ,      
                       "Forest cover (2010): High","Forest cover (2010): Highest","Forest loss (1990-2010)", "Waterbody: No", "Waterbody: Yes", "Steep slope: No","Steep slope: Yes") 
  
  Adopter_vars <- c("Farm class: Small","Farm class: Medium","Farm class: Large"  ,   "Elevation: Lowland"   , 
                    "Elevation: Upland",  "Ruggedness: Lowest","Ruggedness: Low","Ruggedness: High","Ruggedness: Highest" ,  
                    "Road distance: Lowest","Road distance: Low","Road distance: High","Road distance: Highest" ,"Travel time: Lowest" ,  
                    "Travel time: Low","Travel time: High","Travel time: Highest")
  
  Context_vars <- c("Forest management: Lowest","Forest management: Low","Forest management: High","Forest management: Highest" ,
                    "Population density: Lowest", "Population density: Low","Population density: High","Population density: Highest", "HDI - income",

                    "Illiteracy rate: Lowest","Illiteracy rate: Low","Illiteracy rate: High","Illiteracy rate: Highest" ,  
                    "Votes", "Non-profit presence: No", "Non-profit presence: Yes", "Technical assistance")
  
  # Subset to desired variables 
  model_res_Innovation <- model_res[model_res$Variable_split %in% Innovation_vars,]
  model_res_Adopter <- model_res[model_res$Variable_split %in% Adopter_vars,]
  model_res_Context <- model_res[model_res$Variable_split %in% Context_vars,]
  
  # Drop unused levels
  model_res_Innovation <- droplevels(model_res_Innovation)
  model_res_Adopter <- droplevels(model_res_Adopter)
  model_res_Context <- droplevels(model_res_Context)
  
  dodge <- .4
  
  # Plot the graph
  p.1 <-
    ggplot(model_res_Innovation,
           aes(
             x = Variable_split,
             y = Odds,
             ymin = Lower,
             ymax = Upper, 
             fill = Model, 
             group = Model,
             color = Model)) +
    geom_linerange(fatten = 0.5, size = 0.5, position = position_dodge(width = dodge)) +
    geom_point(size = 1, position = position_dodge(width = dodge)) +
    ylab("Odds")
  
  p.1 <-
    p.1 + geom_hline(yintercept = 1,
                     color = "#8b0000",
                     size = 0.2)
  p.1 <- p.1 + coord_flip()
  p.1 <-
    p.1 + scale_x_discrete(breaks = c(levels(model_res_Innovation$Variable_split))) +
    theme_minimal() +
    theme(
      axis.text.y.right = element_blank(),
      axis.ticks.y.right = element_blank(),
      axis.line.y.right = element_blank(),
      axis.title.y = element_blank()
    )  #+
  # labs(title = "a) Innovation",
  #      plot.title = element_text(
  #        size = 11,
  #        face = "bold",
  #        colour = "black"
  #      ))
  p.1 <- p.1 + theme(text = element_text(size = 8))
  p.1 <- p.1 +  theme(plot.margin = unit(c(0, 0, 0, 0), 'lines'))
  p.1 <- p.1 + theme(legend.position = "bottom")  
  
  
  # Plot the graph
  p.2 <-
    ggplot(model_res_Adopter,
           aes(
             x = Variable_split,
             y = Odds,
             ymin = Lower,
             ymax = Upper, 
             fill = Model, 
             group = Model,
             color = Model )) +
    geom_linerange(fatten = 0.5, size = 0.5, position = position_dodge(width = dodge)) +
    geom_point(size = 1, position = position_dodge(width = dodge)) +
    ylab("Odds")
  
  p.2 <-
    p.2 + geom_hline(yintercept = 1,
                     color = "#8b0000",
                     size = 0.2)
  #p.2 <- p.2 + ggbreak::scale_y_break(c(4, 10.5),  scales = .1, ticklabels=c(11, 13), space=.3)
  p.2 <- p.2 + coord_flip()
  p.2 <-
    p.2 + scale_x_discrete(breaks = c(levels(model_res_Adopter$Variable_split)))+
    theme_minimal() +
    theme(
      axis.text.y.right = element_blank(),
      axis.ticks.y.right = element_blank(),
      axis.line.y.right = element_blank(),
      axis.title.y = element_blank()
    ) #+
  # labs(title = "b) Adopter",
  #      plot.title = element_text(
  #        size = 11,
  #        face = "bold",
  #        colour = "black"
  #      ))
  p.2 <- p.2 + theme(text = element_text(size = 8))
  p.2 <- p.2 +  theme(plot.margin = unit(c(0, 0, 0, 0), 'lines')) 
  p.2 <- p.2 + theme(legend.position = "bottom")  
  
  
  
  # Plot the graph
  p.3 <-  ggplot(model_res_Context,
                 aes(
                   x = Variable_split,
                   y = Odds,
                   ymin = Lower,
                   ymax = Upper, 
                   fill = Model, 
                   group = Model,
                   color = Model )) +
    geom_linerange(fatten = 0.5, size = 0.5, position = position_dodge(width = dodge)) +
    geom_point(size = 1, position = position_dodge(width = dodge)) +
    ylab("Odds")
  
  p.3 <-
    p.3 + geom_hline(yintercept = 1,
                     color = "#8b0000",
                     size = 0.2)
  p.3 <- p.3 + coord_flip()
  p.3 <-
    p.3 + scale_x_discrete(breaks = c(levels(model_res_Context$Variable_split)))  +
    theme_minimal() +
    theme(
      axis.text.y.right = element_blank(),
      axis.ticks.y.right = element_blank(),
      axis.line.y.right = element_blank(),
      axis.title.y = element_blank()
    ) #+
  # labs(title = "c) Context",
  #      plot.title = element_text(
  #        size = 11,
  #        face = "bold",
  #        colour = "black"
  #      ))
  p.3 <- p.3 + theme(text = element_text(size = 8))
  p.3 <- p.3 + theme(plot.margin = unit(c(0, 0, 0, 0), 'lines'))
  p.3 <- p.3 + theme(legend.position = "bottom")  
  
  
  # Merge
  p <- ggpubr::ggarrange(p.1, p.2, p.3, ncol = 3,  align = "v", common.legend = T, legend = "bottom")
  
  # Ruturn the plot
  return(p)
}

### Exclude HDI - income ###

### Plot preparation function 
plot_prep_f_HDI <- function(model_in) {
  
  # Remove intercept
  model_2_res <- model_in %>%  filter(!Variable=='(Intercept)')
  
  # Create reference level DF
  Ref_level <- data.frame(
    Variable = c(
      "Crop value: R$0",
      "Cattle density: Lowest",
      "Forest cover (2010): Lowest",
      "Waterbody: No",
      "Steep slope: No",
      "Farm class: Medium",
      "Elevation: Lowland",
      "Ruggedness: Lowest",
      "Road distance: Lowest",
      "Travel time: Lowest",
      "Forest management: Lowest",
      "Population density: Lowest",
      "Illiteracy rate: Lowest",
      "Non-profit presence: No"
    ),
    Odds = c(rep(1, 14)),
    Upper = c(rep(1, 14)),
    Lower = c(rep(1, 14)),
    Var_t = c(rep("Reference", 14))
  )
  
  # Combine
  model_2_res <- rbind(model_2_res, Ref_level)
  
  # Rename variables
  model_2_res$Variable <- factor(
    model_2_res$Variable,
    levels = c(
      # Crop value
      "Crop value: R$0",
      "fm_vh_m_cat1-2000",
      "fm_vh_m_cat2000-4000",
      "fm_vh_m_cat4000+",
      
      # Cattle density
      "Cattle density: Lowest",
      "cat_nh_m_levelLow" ,
      "cat_nh_m_levelHigh",
      "cat_nh_m_levelHighest",
      
      # Farm class
      "FM_classSmall Property",
      "Farm class: Medium",
      "FM_classLarge Property",
      
      # Forest cover (2010)
      "Forest cover (2010): Lowest",
      "For_p_10_levelLow",
      "For_p_10_levelHigh",
      "For_p_10_levelHighest",
      
      # Forest loss (1990-2010)
      "Defor_ss",
      
      # Waterbody
      "Waterbody: No",
      "Wat_011",
      
      # Slope 
      "Steep slope: No",
      "Slope_011",
      
      # Elevation
      "Elevation: Lowland" ,
      "Elev_01Upland",
      
      # Ruggedness
      "Ruggedness: Lowest" ,
      "Elev_sd_levelLow" ,
      "Elev_sd_levelHigh",
      "Elev_sd_levelHighest" ,
      
      # Road distance
      "Road distance: Lowest",
      "Rd_dis_levelLow",
      "Rd_dis_levelHigh",
      "Rd_dis_levelHighest",
      
      # Travel time
      "Travel time: Lowest",
      "Trl_time_levelLow",
      "Trl_time_levelHigh" ,
      "Trl_time_levelHighest" ,
      
      # Farms with forest management practices
      "Forest management: Lowest",
      "For_pract_levelLow",
      "For_pract_levelHigh",
      "For_pract_levelHighest",
      
      # Population density
      "Population density: Lowest",
      "Pop_den_levelLow",
      "Pop_den_levelHigh",
      "Pop_den_levelHighest",
      
  
      # Illiteracy
      "Illiteracy rate: Lowest",
      "Ill_rate_levelLow",
      "Ill_rate_levelHigh",
      "Ill_rate_levelHighest",
      
      # Votes
      "Votes_ss",
      
      # NGO
      "Non-profit presence: No",
      "NGO_p1",
      
      # Technical assistance
      "Ag_ass"
    )
  )
  
  model_2_res$Variable <- model_2_res$Variable %>%
    dplyr::recode(
      "fm_vh_m_cat1-2000" = 'Crop value: R$1-R$2000',
      "fm_vh_m_cat2000-4000" = 'Crop value: R$2000-R$4000',
      "fm_vh_m_cat4000+" = 'Crop value: R$4000+',
      
      "cat_nh_m_levelLow" = 'Cattle density: Low',
      "cat_nh_m_levelHigh" = 'Cattle density: High',
      "cat_nh_m_levelHighest" = 'Cattle density: Highest',
      
      "FM_classSmall Property" = 'Farm class: Small',
      "FM_classLarge Property" = 'Farm class: Large',
      
      "For_p_10_levelLow" = 'Forest cover (2010): Low',
      "For_p_10_levelHigh" = 'Forest cover (2010): High',
      "For_p_10_levelHighest" = 'Forest cover (2010): Highest',
      
      "Defor_ss" = 'Forest loss (1990-2010)',
      
      "Wat_011" = 'Waterbody: Yes',
      
      "Slope_011" = 'Steep slope: Yes',
      
      "Elev_01Upland" = 'Elevation: Upland',
      
      "Elev_sd_levelLow" = 'Ruggedness: Low',
      "Elev_sd_levelHigh" = 'Ruggedness: High',
      "Elev_sd_levelHighest" = 'Ruggedness: Highest',
      
      "Rd_dis_levelLow" = 'Road distance: Low',
      "Rd_dis_levelHigh" = 'Road distance: High',
      "Rd_dis_levelHighest" = 'Road distance: Highest',
      
      "Trl_time_levelLow" = 'Travel time: Low',
      "Trl_time_levelHigh" = 'Travel time: High',
      "Trl_time_levelHighest" = 'Travel time: Highest',
      
      "For_pract_levelLow" = 'Forest management: Low',
      "For_pract_levelHigh" = 'Forest management: High',
      "For_pract_levelHighest" = 'Forest management: Highest',
      
      "Pop_den_levelLow" = 'Population density: Low',
      "Pop_den_levelHigh" = 'Population density: High',
      "Pop_den_levelHighest" = 'Population density: Highest',
      
      "Ill_rate_levelLow" = 'Illiteracy rate: Low',
      "Ill_rate_levelHigh" = 'Illiteracy rate: High',
      "Ill_rate_levelHighest" = 'Illiteracy rate: Highest',
      
      "Votes_ss" = 'Votes',
      
      "NGO_p1" = 'Non-profit presence: Yes',
      
      "Ag_ass"  = 'Technical assistance'
    )
  
  ### Split labels over multiple lines
  model_2_res$Variable_split <-
    as.factor(sapply(
      strwrap(model_2_res$Variable, 30, simplify = FALSE),
      paste,
      collapse = "\n"
    ))
  model_2_res <- with(model_2_res, model_2_res[order(Variable), ])
  model_2_res$Variable_split <-
    factor(model_2_res$Variable_split,
           levels = unique(model_2_res$Variable_split[order(model_2_res$Variable)]))
  return(model_2_res)
}


### Main plotting function 
plot_do_f_HDI <- function(model_res) {
  
  ### Variables in each sub-plot 
  Innovation_vars <- c("Crop value: R$0", "Crop value: R$1-R$2000","Crop value: R$2000-R$4000", "Crop value: R$4000+","Cattle density: Lowest"  ,
                       "Cattle density: Low","Cattle density: High","Cattle density: Highest" ,"Farm class: Small","Farm class: Medium","Farm class: Large"  ,
                       "Forest cover (2010): Lowest","Forest cover (2010): Low" ,      
                       "Forest cover (2010): High","Forest cover (2010): Highest","Forest loss (1990-2010)") 
  
  Adopter_vars <- c("Waterbody: No", "Waterbody: Yes", "Steep slope: No","Steep slope: Yes",   "Elevation: Lowland"   , 
                    "Elevation: Upland",  "Ruggedness: Lowest","Ruggedness: Low","Ruggedness: High","Ruggedness: Highest" ,  
                    "Road distance: Lowest","Road distance: Low","Road distance: High","Road distance: Highest" ,"Travel time: Lowest" ,  
                    "Travel time: Low","Travel time: High","Travel time: Highest")
  
  Context_vars <- c("Forest management: Lowest","Forest management: Low","Forest management: High","Forest management: Highest" ,
                    "Population density: Lowest", "Population density: Low","Population density: High","Population density: Highest", 
                    "Illiteracy rate: Lowest","Illiteracy rate: Low","Illiteracy rate: High","Illiteracy rate: Highest" ,  
                    "Votes", "Non-profit presence: No", "Non-profit presence: Yes", "Technical assistance")
  
  # Subset to desired variables 
  model_res_Innovation <- model_res[model_res$Variable_split %in% Innovation_vars,]
  model_res_Adopter <- model_res[model_res$Variable_split %in% Adopter_vars,]
  model_res_Context <- model_res[model_res$Variable_split %in% Context_vars,]
  
  # Drop unused levels
  model_res_Innovation <- droplevels(model_res_Innovation)
  model_res_Adopter <- droplevels(model_res_Adopter)
  model_res_Context <- droplevels(model_res_Context)
  
  # Plot the graph
  p.1 <-
    ggplot(model_res_Innovation,
           aes(
             x = Variable_split,
             y = Odds,
             ymin = Lower,
             ymax = Upper
           )) +
    geom_linerange(fatten = 0.5, size = 0.5) +
    geom_point(aes(shape = Var_t), size = 1) +
    scale_shape_manual(values = c(1, 16)) +
    scale_colour_manual(values = c("white", "black")) +
    ylab("Odds")
  p.1 <- p.1 + ylim(0.13, 3.04)
  p.1 <-
    p.1 + geom_hline(yintercept = 1,
                     color = "#8b0000",
                     size = 0.2)
  p.1 <- p.1 + coord_flip()
  p.1 <-
    p.1 + scale_x_discrete(breaks = c(levels(model_res_Innovation$Variable_split)),
                           limits = rev(
                             c(
                               levels(model_res_Innovation$Variable_split)[1:4],
                               "skip",
                               levels(model_res_Innovation$Variable_split)[5:8],
                               "skip",
                               levels(model_res_Innovation$Variable_split)[9:11],
                               "skip",
                               levels(model_res_Innovation$Variable_split)[12:15],
                               "skip",
                               levels(model_res_Innovation$Variable_split)[16]
                             )
                           ))  +
    theme_minimal() +
    theme(
      axis.text.y.right = element_blank(),
      axis.ticks.y.right = element_blank(),
      axis.line.y.right = element_blank(),
      axis.title.y = element_blank()) 
  
  p.1 <- p.1 + theme(text = element_text(size = 8))
  p.1 <- p.1 + theme(legend.position = "none")  +
    theme(plot.margin = unit(c(0, 0, 0, 0), 'lines')) 
  
  # Plot the graph
  p.2 <-
    ggplot(model_res_Adopter,
           aes(
             x = Variable_split,
             y = Odds,
             ymin = Lower,
             ymax = Upper
           )) +
    geom_linerange(fatten = 0.5, size = 0.5) +
    geom_point(aes(shape = Var_t), size = 1) +
    scale_shape_manual(values = c(1, 16)) +
    scale_colour_manual(values = c("white", "black")) +
    ylab("Odds")
  p.2 <- p.2 + ylim(0.13, 3.04)
  p.2 <-
    p.2 + geom_hline(yintercept = 1,
                     color = "#8b0000",
                     size = 0.2)
  p.2 <- p.2 + coord_flip()
  p.2 <-
    p.2 + scale_x_discrete(breaks = c(levels(model_res_Adopter$Variable_split)),
                           limits = rev(
                             c(levels(model_res_Adopter$Variable_split)[1:2],
                               "skip",
                               levels(model_res_Adopter$Variable_split)[3:4],
                               "skip",
                               levels(model_res_Adopter$Variable_split)[5:6],
                               "skip",
                               levels(model_res_Adopter$Variable_split)[7:10],
                               "skip",
                               levels(model_res_Adopter$Variable_split)[11:14],
                               "skip",
                               levels(model_res_Adopter$Variable_split)[15:18]
                             )
                           ))  +
    theme_minimal() +
    theme(
      axis.text.y.right = element_blank(),
      axis.ticks.y.right = element_blank(),
      axis.line.y.right = element_blank(),
      axis.title.y = element_blank()
    ) 
  
  p.2 <- p.2 + theme(text = element_text(size = 8))
  p.2 <- p.2 + theme(legend.position = "none") +
    theme(plot.margin = unit(c(0, 0, 0, 0), 'lines'))
  
  # Plot the graph
  p.3 <-
    ggplot(model_res_Context,
           aes(
             x = Variable_split,
             y = Odds,
             ymin = Lower,
             ymax = Upper
           )) +
    geom_linerange(fatten = 0.5, size = 0.5) +
    geom_point(aes(shape = Var_t), size = 1) +
    scale_shape_manual(values = c(1, 16)) +
    scale_colour_manual(values = c("white", "black")) +
    ylab("Odds")
  p.3 <- p.3 + ylim(0, 3.04)
  p.3 <-
    p.3 + geom_hline(yintercept = 1,
                     color = "#8b0000",
                     size = 0.2)
  p.3 <- p.3 + coord_flip()
  p.3 <-
    p.3 + scale_x_discrete(breaks = c(levels(model_res_Context$Variable_split)),
                           limits = rev(
                             c(
                               levels(model_res_Context$Variable_split)[1:4],
                               "skip",
                               levels(model_res_Context$Variable_split)[5:8],
                               "skip",
                               levels(model_res_Context$Variable_split)[9:12],
                               "skip",
                               levels(model_res_Context$Variable_split)[13],
                               "skip",
                               levels(model_res_Context$Variable_split)[14:15],
                               "skip",
                               levels(model_res_Context$Variable_split)[16]
                             )
                           ))  +
    theme_minimal() +
    theme(
      axis.text.y.right = element_blank(),
      axis.ticks.y.right = element_blank(),
      axis.line.y.right = element_blank(),
      axis.title.y = element_blank()
    )
  
  p.3 <- p.3 + theme(text = element_text(size = 8))
  p.3 <- p.3 + theme(legend.position = "none") +
    theme(plot.margin = unit(c(0, 0, 0, 0), 'lines'))
  
  
  # Merge
  p <- ggpubr::ggarrange(p.1, p.2, p.3, ncol = 3,  align = "v")
  
  # Ruturn the plot
  return(p)
}




