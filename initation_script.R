#Triangular sample function 
library(data.table)

rtri <- function(a,b,c){
  x <- runif(1)
  
  f_c <- (c-a)/(b-a)
  
  if(x <= f_c) {
    a <- a + sqrt(x*(b-a)*(c-a))
  } else {
    a <-b - sqrt((1-x)*(b-a)*(b-c))
  }
  
  return(a)
}

###Hyperparameters

#Set the number of iterations for MCTS
n_iter <- 5000

#Set the number of time periods
n_t <- 24

#Set c for MCTS
n_c <- 1.5

#Set discount factor (gamma)
n_gamma <- .95


###Decisions

#Modernize
d_modernize <- sample(c('Modernize','Current'),1)

#Mobilize
d_mobilize <- sample(c('Full','Partial','None'),1)

#Posture
d_posture <- sample(c('Forward','Dispersed'),1)

#Key Terrain
d_kt <- sample(c('All','Rear','Most'),1)

###Uncertainties

# Fighting Capacity | Modernization
u_fc <- if(d_modernize == "Current") {
  list(inf_f = list(inf_e = rtri(.05,.15,.1),arm_e = rtri(.02,.1,.05)),
       arm_f = list(inf_e = rtri(.075,.15,.125),arm_e = rtri(.05,.15,.1)),
       inf_e = list(inf_f = rtri(.05,.15,.1),arm_f = rtri(.02,.1,.05)),
       arm_e = list(inf_f = rtri(.075,.15,.125),arm_f = rtri(.05,.15,.1))
  )
} else {
  list(inf_f = list(inf_e = rtri(.05,.2,.15),arm_e = rtri(.02,.125,.075)),
       arm_f = list(inf_e = rtri(.075,.15,.125),arm_e = rtri(.05,.2,.15)),
       inf_e = list(inf_f = rtri(.05,.175,.125),arm_f = rtri(.02,.1,.05)),
       arm_e = list(inf_f = rtri(.075,.15,.125),arm_f = rtri(.05,.15,.1))
  )
}

#Enemy Armor Reinforcements
#Assuming Mean of 2

u_e_arm_rein <- rbinom(24,1,1/12)

#Enemy Infantry Reinforcements
#Assuming Mean of 4
u_e_inf_rein <- rbinom(24,1,1/6)

#Friendly Infantry Reinforcements | Mobilization

u_f_inf_rein <- integer(n_t)
success_count <- 0
inf_res <- ifelse(d_mobilize == "Full",2,4)

# Iterate through the vector, sampling from the binomial distribution
for (i in 1:n_t) {
  if (success_count < inf_res) {
    u_f_inf_rein[i] <- sum(rbinom(inf_res-success_count, 1, ifelse(d_mobilize=="Partial",.25,.2)))
    success_count <- success_count + u_f_inf_rein[i]
    
  } else {
    u_f_inf_rein[i] <- 0
  }
  
}
u_f_inf_rein
#Friendly Armor Reinforcements | Mobilization
u_f_armor_rein <- integer(n_t)
success_count <- 0
armor_res <- ifelse(d_mobilize == "Full",1,2)
# Iterate through the vector, sampling from the binomial distribution
for (i in 1:n_t) {
  if (success_count < armor_res) {
    u_f_armor_rein[i] <- sum(rbinom(armor_res-success_count, 1, ifelse(d_mobilize=="Partial",.175,.1)))
    success_count <- success_count + u_f_armor_rein[i]
    
  } else {
    u_f_armor_rein[i] <- 0
  }
  
}


#Enemy Objective


if(d_mobilize == "Full"){
  f_units <- data.frame(class = c(rep('inf',5),rep('arm',3)), type = 'f',str = 100)
  
  if(d_posture == "Dispersed"){
    f_units$s <- c('111409','141406','101107','081210','081008','101309','121206','081109')
  } else {
    f_units$s <- c('111409','141406','141305','101410','081008','121509','131306','081109')
  }

} else{
  f_units <- data.frame(class = c(rep('inf',4),rep('arm',2)), type = 'f',str = 100)
  if(d_posture == "Dispersed"){
    f_units$s <- c('111409','141406','101107','081210','101309','121206')
  } else {
    f_units$s <- c('111409','141406','141305','101410','121509','131306')
  }
}




f_units$id <- paste(f_units$class,f_units$type,1:nrow(f_units),sep = '_')

f_units

cur_index <- nrow(f_units) +1


#Lanchester Values
if(d_modernize == 'Current') {
  u_lanchester <- data.frame(shooter_side = c(rep('f',4),rep('e',4)),
                             shooter_class = c(rep(c('inf','inf','arm','arm'),2)),
                             target_side = c(rep('e',4),rep('f',4)),
                             target_class = c(rep(c('inf','arm'),4)),
                             mod = c(
                               rtri(.05,.15,.1),
                               rtri(.02,.1,.05),
                               rtri(.075,.15,.125),
                               rtri(.05,.15,.1),
                               rtri(.05,.15,.1),
                               rtri(.02,.1,.05),
                               rtri(.075,.15,.125),
                               rtri(.05,.15,.1)
                             ))
} else {
  
  u_lanchester <- data.frame(shooter_side = c(rep('f',4),rep('e',4)),
                             shooter_class = c(rep(c('inf','inf','arm','arm'),2)),
                             target_side = c(rep('e',4),rep('f',4)),
                             target_class = c(rep(c('inf','arm'),4)),
                             mod = c(
                               rtri(.05,.2,.15),
                               rtri(.02,.125,.075),
                               rtri(.075,.175,.15),
                               rtri(.05,.2,.15),
                               rtri(.05,.15,.1),
                               rtri(.02,.1,.05),
                               rtri(.075,.15,.125),
                               rtri(.05,.15,.1)
                             ))
}



#Terrain Values
if (d_kt == "All") {
  
  key_tern <- data.table(s = c('050607','091108','151102','151506'),
                         value = 1,type  = 'f')
  
} else if (d_kt == "Rear") {
  
  key_tern <- data.table(s = c('050607','091108','151102','151506'),
                         value = c(5,5,1,1),type  = 'f')
  
} else if (d_kt == "Most") {
  
  key_tern <- data.table(s = c('050607','091108','151102','151506'),
                         value = c(3,3,3,1),type  = 'f')
} else {
  # Code to execute if d_kt is none of the above
  print("d_kt is neither All, Rear, nor Most")
}

#Enemy Objectives
u_eny_obj <- sample(c('Single','Border','Full'),1, prob = c(.5,.4,.1))

#Unit Setup

if (u_eny_obj == "Full") {
  
  eny_tern <- data.table(s = c('050607','091108','151102','151506'),
                         value = 1,type  = 'e')
  
} else if (u_eny_obj == "Border") {
  
  eny_tern <- data.table(s = c('050607','091108','151102','151506'),
                         value = c(0,1,5,5),type  = 'e')
  
} else if (u_eny_obj == "Single") {
  
  eny_tern <- data.table(s = c('050607','091108','151102','151506'),
                         value = c(0,1,2,5),type  = 'e')
} else {
  # Code to execute if d_kt is none of the above
  print("doh")
}

e_units <- data.frame(class = c(rep('inf',4),rep('arm',3)), type = 'e',str = 100)
e_units$s <- c('141709','151708','141810','171807','161707','151809','151910')
e_units$id <- paste(e_units$class,e_units$type,1:nrow(e_units),sep = '_')
e_index <- nrow(e_units) +1

f_rein_areas_fwd <- c('091209','101107','081109','091007','101006','081210','071110','090906','080907')
f_rein_areas_rear <- c('050708','060606','060707','040608','050506','040507','040709','060505','040406')

e_rein_areas <- c('161909','171807','171908','181806','162010','151809','161808','151910','141810')



f_rein_areas_fwd[!f_rein_areas_fwd %in% c(f_units$s,e_units$s)][1]



rein_mat <- cbind(u_f_inf_rein,u_f_armor_rein,u_e_inf_rein,u_e_arm_rein)
rein_type <- c('f','f','e','e')
rein_class <- c('inf','arm','inf','arm')


units <- rbind(f_units,e_units)

turn <- 13

for(i in 1:dim(rein_mat)[2]){
  print(i)
  if(rein_mat[turn,i]>0){
    
    for(j in 1:rein_mat[turn,i]){
      
      new_u <- data.frame(class = rein_class[i], type = rein_type[i],str = 100)
      
      if(rein_type[i] == 'f' & key_tern[s == '091108']$type == 'f'){
        
        if(length(f_rein_areas_fwd[!f_rein_areas_fwd %in% units$s]) > 0){
          
          new_u$s <- f_rein_areas_fwd[!f_rein_areas_fwd %in% units$s][1]
          new_u$id <- paste(new_u$class,new_u$type,cur_index,sep = "_")
          cur_index <- cur_index + 1
        } else {
          
          new_u$s <- territory$pos[!territory$pos %in% units$s][1]
          new_u$id <- paste(new_u$class,new_u$type,cur_index,sep = "_")
          cur_index <- cur_index + 1
        }
        
      } else if(rein_type[i] == 'f' & key_tern[s == '091108']$type == 'f'){
        
        if(length(f_rein_areas_rear[!f_rein_areas_rear %in% units$s]) > 0 ){
          
          new_u$s <- f_rein_areas_rear[!f_rein_areas_rear %in% units$s][1]
          new_u$id <- paste(new_u$class,new_u$type,cur_index,sep = "_")
          cur_index <- cur_index + 1
          
        } else {
          
          new_u$s <- territory$pos[!territory$pos %in% units$s][1]
          new_u$id <- paste(new_u$class,new_u$type,cur_index,sep = "_")
          cur_index <- cur_index + 1
        }
        
      } else if(rein_type[i] == 'e') {
        
        if(length(e_rein_areas[!e_rein_areas %in% units$s])>0){
          new_u$s <- e_rein_areas[!e_rein_areas %in% units$s][1]
          
          new_u$id <- paste(new_u$class,new_u$type,e_index,sep = "_")
          e_index <- e_index + 1
        } else {
          
          new_u$s <- rev(territory$pos[!territory$pos %in% units$s][1])
          
          new_u$id <- paste(new_u$class,new_u$type,e_index,sep = "_")
          e_index <- e_index + 1
        }
        
      }
      units <- rbind(units,new_u)
      print(new_u)
    }
    
  }
}

units


for(i in 1:0){
  print(i)
}
if(key_tern[s == '091108']$type == 'f') {
  
  
  
}
# Sample integer value
value <- 0  # You can set this to any integer between 0 and 3

# Run the loop
for(i in 1:seq_len(value)) {
  print(paste("This is iteration", i))
}

