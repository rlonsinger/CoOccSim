#Author: Robert C. Lonsinger
#Affiliation" "U.S. Geological Survey, Oklahoma Cooperative Fish and Wildlife Research Unit, Oklahoma State University, 007 Agriculture Hall, Stillwater, OK 74078, USA

#R script and functions used to generate simulations and perform analyses

#--Function to RUN and COMPILE for CLOSURE
RunClosure<-function(Grd, M, J, Occ, psiA, psiBA, psiBa, pA, pB, Sims = 10, ci = 0.95){
  Comb.dataA <- list() #identify a list for which you want to results for each simulation (among combinations of conditions for psiA)
  for(psiA.i in 1:length(psiA)){
    Comb.dataB <- list() #identify a list for which you want to results for each simulation (among combinations of conditions)
    for(psiB.i in 1:length(psiBA)){
      Sim.data <- list() #identify a list for which you want to results for each simulation (within a Combination of conditions)
      for(sim.i in 1:Sims){
        Sim.data[[sim.i]] <- SimWrapper(Grd = Grd, M = M, J = J, Occ = Occ, psiA = psiA[psiA.i], psiBA = psiBA[psiB.i], psiBa = psiBa[psiB.i], pA = pA, pB = pB, ci = ci)
      }
      Comb.dataB[[psiB.i]]<-Sim.data
    }
    Comb.dataA[[psiA.i]]<-Comb.dataB
  }
  temp.GrdSize <- temp.Sites <- temp.Top.Model.fs <- temp.SIF.fs <- temp.Top.Model.ls <- temp.SIF.ls <- NULL
  temp.TPsiA <- temp.TPsiBA <- temp.TPsiBa <- NULL
  temp.PsiA_fs1 <- temp.PsiBA_fs1 <- temp.PsiBa_fs1 <- NULL
  temp.PsiA_fs2 <- temp.PsiBA_fs2 <- temp.PsiBa_fs2 <- NULL
  temp.PsiA_ls1 <- temp.PsiBA_ls1 <- temp.PsiBa_ls1 <- NULL
  temp.PsiA_ls2 <- temp.PsiBA_ls2 <- temp.PsiBa_ls2 <- NULL

  for(i in 1:length(Comb.dataA)){
    for(j in 1:length(Comb.dataA[[i]])){
      for(k in 1:length(Comb.dataA[[i]][[j]])){
        temp.GrdSize <- append(temp.GrdSize, Comb.dataA[[i]][[j]][[k]]$Sim.data$Simulation.Conditions$Gridsize)
        temp.Sites <- append(temp.Sites, Comb.dataA[[i]][[j]][[k]]$Sim.data$Simulation.Conditions$M)

        temp.TPsiA <- append(temp.TPsiA, Comb.dataA[[i]][[j]][[k]]$Sim.data$Simulation.Conditions$True_PsiA)
        temp.TPsiBA <- append(temp.TPsiBA, Comb.dataA[[i]][[j]][[k]]$Sim.data$Simulation.Conditions$True_PsiBA)
        temp.TPsiBa <- append(temp.TPsiBa, Comb.dataA[[i]][[j]][[k]]$Sim.data$Simulation.Conditions$True_PsiBa)

        temp.Top.Model.fs <- append(temp.Top.Model.fs, Comb.dataA[[i]][[j]][[k]]$OM.fs.data$top)
        temp.SIF.fs <- append(temp.SIF.fs, Comb.dataA[[i]][[j]][[k]]$OM.fs.data$SIF)
        temp.Top.Model.ls <- append(temp.Top.Model.ls, Comb.dataA[[i]][[j]][[k]]$OM.ls.data$top)
        temp.SIF.ls <- append(temp.SIF.ls, Comb.dataA[[i]][[j]][[k]]$OM.ls.data$SIF)

        temp.PsiA_fs1 <- append(temp.PsiA_fs1, Comb.dataA[[i]][[j]][[k]]$OM.fs.data$Ind_Model$real["psiA","est"])
        temp.PsiBA_fs1 <- append(temp.PsiBA_fs1, Comb.dataA[[i]][[j]][[k]]$OM.fs.data$Ind_Model$real["psiBA","est"])
        temp.PsiBa_fs1 <- append(temp.PsiBa_fs1, Comb.dataA[[i]][[j]][[k]]$OM.fs.data$Ind_Model$real["psiBa","est"])

        temp.PsiA_fs2 <- append(temp.PsiA_fs2, Comb.dataA[[i]][[j]][[k]]$OM.fs.data$Inf_Model$real["psiA","est"])
        temp.PsiBA_fs2 <- append(temp.PsiBA_fs2, Comb.dataA[[i]][[j]][[k]]$OM.fs.data$Inf_Model$real["psiBA","est"])
        temp.PsiBa_fs2 <- append(temp.PsiBa_fs2, Comb.dataA[[i]][[j]][[k]]$OM.fs.data$Inf_Model$real["psiBa","est"])

        temp.PsiA_ls1 <- append(temp.PsiA_ls1, Comb.dataA[[i]][[j]][[k]]$OM.ls.data$Ind_Model$real["psiA","est"])
        temp.PsiBA_ls1 <- append(temp.PsiBA_ls1, Comb.dataA[[i]][[j]][[k]]$OM.ls.data$Ind_Model$real["psiBA","est"])
        temp.PsiBa_ls1 <- append(temp.PsiBa_ls1, Comb.dataA[[i]][[j]][[k]]$OM.ls.data$Ind_Model$real["psiBa","est"])

        temp.PsiA_ls2 <- append(temp.PsiA_ls2, Comb.dataA[[i]][[j]][[k]]$OM.ls.data$Inf_Model$real["psiA","est"])
        temp.PsiBA_ls2 <- append(temp.PsiBA_ls2, Comb.dataA[[i]][[j]][[k]]$OM.ls.data$Inf_Model$real["psiBA","est"])
        temp.PsiBa_ls2 <- append(temp.PsiBa_ls2, Comb.dataA[[i]][[j]][[k]]$OM.ls.data$Inf_Model$real["psiBa","est"])
      }
    }
  }
  mod.summary <- data.frame(GrdSize = temp.GrdSize, Sites = temp.Sites,
                            TPsiA = temp.TPsiA, TPsiBA = temp.TPsiBA, TPsiBa = temp.TPsiBa,
                            PsiA_fs1 = temp.PsiA_fs1, PsiBA_fs1 = temp.PsiBA_fs1, PsiBa_fs1 = temp.PsiBa_fs1,
                            PsiA_fs2 = temp.PsiA_fs2, PsiBA_fs2 = temp.PsiBA_fs2, PsiBa_fs2 = temp.PsiBa_fs2,
                            Top.Model.fs = temp.Top.Model.fs, SIF.fs = temp.SIF.fs,
                            PsiA_ls1 = temp.PsiA_ls1, PsiBA_ls1 = temp.PsiBA_ls1, PsiBa_ls1 = temp.PsiBa_ls1,
                            PsiA_ls2 = temp.PsiA_ls2, PsiBA_ls2 = temp.PsiBA_ls2, PsiBa_ls2 = temp.PsiBa_ls2,
                            Top.Model.ls = temp.Top.Model.ls, SIF.ls = temp.SIF.ls)
  mod.results <- Comb.dataA
  return(list(Sim.summary = mod.summary, Sim.results = mod.results))
}

#--Function to supply with conditions that will call other functions and store data
SimWrapper<-function(Grd, M, J, Occ, psiA, psiBA, psiBa, pA, pB, ci = 0.95){
  if(psiBA==psiBa){
    temp.sim <- Sim_Occ(Grd = Grd, M = M, psiA = psiA, psiB = psiBA)
  }else{
    temp.sim <- Sim_Occ2(Grd = Grd, M = M, psiA = psiA, psiBA = psiBA, psiBa = psiBa)
  }
  temp.eh <- eh(sample.data = temp.sim$sample.data, J = J, Occ = Occ, pA = pA, pB = pB)
  temp.fs.om <- OccMods(ehA = temp.eh$ehA, ehB = temp.eh$ehB)
  if(!is.null(temp.eh$ehAcon)){
    temp.ls.om <- OccMods(ehA = temp.eh$ehAcon, ehB = temp.eh$ehBcon)
  }else{
    temp.ls.om <- NULL
  }
  return(list(Sim.data = temp.sim, EH = temp.eh, OM.fs.data = temp.fs.om, OM.ls.data = temp.ls.om))
}

#--Function Sim_Occ creates 1 simulated occupancy dataset assuming Independence among species
Sim_Occ<-function(Grd, M, psiA, psiB){
  Sim.levels<-data.frame(Gridsize = Grd, M = M, True_PsiA = psiA, True_PsiBA = psiB, True_PsiBa = psiB)
  Sim1.data<-data.frame(Site=1:Grd, psiA = sample(c(rep(0,round(Grd-(psiA*Grd),0)), rep(1,round((psiA*Grd),0)))),
                        psiB = sample(c(rep(0,round(Grd-(psiB*Grd),0)), rep(1,round((psiB*Grd),0)))))
  samp1.data<-Sim1.data[sort(sample(1:Grd, M)),]
  return(list(sample.data = samp1.data, Sim.data = Sim1.data, Simulation.Conditions = Sim.levels))
}

#--Function to create 1 simulated occupancy dataset when psiBA != psiBa
Sim_Occ2<-function(Grd, M, psiA, psiBA, psiBa){
  Sim.levels<-data.frame(Gridsize = Grd, M = M, True_PsiA = psiA, True_PsiBA = psiBA, True_PsiBa = psiBa)
  Sim1.data<-data.frame(Site = 1:Grd, psiA = sample(c(rep(0,round(Grd-(psiA*Grd),0)), rep(1,round((psiA*Grd),0)))),psiB = rep(NA,Grd))
  temp.idx<-sort(sample(which(Sim1.data$psiA==1),round(length(which(Sim1.data$psiA==1))*psiBA,0)))
  Sim1.data$psiB[temp.idx]<-1
  temp.idx<-sort(sample(which(Sim1.data$psiA==0),round(length(which(Sim1.data$psiA==0))*psiBa,0)))
  Sim1.data$psiB[temp.idx]<-1
  temp.idx<-which(is.na(Sim1.data$psiB))
  Sim1.data$psiB[temp.idx]<-0
  samp1.data<-Sim1.data[sort(sample(1:Grd, M)),]

  return(list(sample.data = samp1.data, Sim.data = Sim1.data, Simulation.Conditions = Sim.levels))
}

#--Function eh() uses the results of Sim_Occ to create Encounter data based on J surveys (which can be condensed to Occ length) and pA and pB
eh<-function(sample.data, J, Occ = J, pA, pB){
  ehA.data <- ehB.data <- data.frame(row.names=c(1:nrow(sample.data)))
  for(site.i in 1:nrow(sample.data)){
    probA <- sample.data$psiA[site.i]*pA
    probB <- sample.data$psiB[site.i]*pB
    for(survey.j in 1:J){
      ehA.data[site.i,survey.j] <- rbinom(1,1,probA)
      ehB.data[site.i,survey.j] <- rbinom(1,1,probB)
    }
  }
  if(Occ<J){
    NoOcc <- J/Occ
    ehAcon.data <- ehBcon.data <- data.frame(row.names=c(1:NoOcc))
    for(site.i in 1:nrow(sample.data)){
      for(survey.j in 1:NoOcc){
        if(sum(ehA.data[site.i,((survey.j*Occ)-Occ+1):(survey.j*Occ)])==0){
          ehAcon.data[site.i, survey.j]<-0
        } else {
          ehAcon.data[site.i, survey.j]<-1
        }
        if(sum(ehB.data[site.i,((survey.j*Occ)-Occ+1):(survey.j*Occ)])==0){
          ehBcon.data[site.i, survey.j]<-0
        } else {
          ehBcon.data[site.i, survey.j]<-1
        }
      }
    }
  }else {
    ehAcon.data <- ehBcon.data <- NULL
  }
  return(list(ehA = ehA.data, ehB = ehB.data, ehAcon = ehAcon.data, ehBcon = ehBcon.data))
}

#--Function to run the 2 competing occupancy models (i.e., PsiBA==PsiBa vs. PsiBA!=PsiBa)
OccMods<-function(ehA, ehB, ci = 0.95){
  psiB_ind_psiA <- occ2sps(ehA, ehB, model = list(psiA~1,psiBa~1,pA~1,pB~1), data=NULL, ci = ci, verify=TRUE)
  psiB_inf_psiA <- occ2sps(ehA, ehB, model = list(psiA~1,psiBA~1, psiBa~1,pA~1,pB~1), data=NULL, ci = ci, verify=TRUE)

  temp<-data.frame(AICc(psiB_ind_psiA, psiB_inf_psiA), LL=c(psiB_ind_psiA$logLik[1],psiB_inf_psiA$logLik[1]))
  temp<-AICtable(temp)
  temp<-temp[,c(1,2,4,5,6,3)]
  if(is.na(temp$df[1])){
    top <- "NA"
    SIF <- "NA"
  } else {
    if(temp$df[1]==4){
      top<-"Ind"
    } else if(temp$df[1]==5) {
      top<-"Inf"
    } else {
      top<-"ISSUE"
    }
    SIF <- ((psiB_inf_psiA$real["psiA","est"])*(psiB_inf_psiA$real["psiBA","est"]))/(psiB_inf_psiA$real["psiA","est"]*(((psiB_inf_psiA$real["psiA","est"])*(psiB_inf_psiA$real["psiBA","est"]))+ ((1 - psiB_inf_psiA$real["psiA","est"])* psiB_inf_psiA$real["psiBa","est"])))
  }
  return(list(top = top, Mod.Table = temp, SIF = SIF, Ind_Model = psiB_ind_psiA, Inf_Model = psiB_inf_psiA))
}

#--FUNCTION to RUN and COMPILE for NO CLOSURE
RunNoClosure<-function(Grd, M, J, Occ, psiA, psiBA, psiBa, pA, pB, prM, prMA = NULL, Sims = 10, dist.list, ci = 0.95, Site.column = TRUE){
  Comb.dataA <- list()
  for(psiA.i in 1:length(psiA)){
    Comb.dataB <- list()
    for(psiB.i in 1:length(psiBA)){
      Sim.data <- list()
      for(sim.i in 1:Sims){
        Sim.data[[sim.i]] <- SimWrapper2(Grd = Grd, M = M, J = J, Occ = Occ, psiA = psiA[psiA.i], psiBA = psiBA[psiB.i], psiBa = psiBa[psiB.i], pA = pA, pB = pB, dist.list = dist.list, prM = prM, prMA = prMA, ci = 0.95, Site.column = TRUE)
      }
      Comb.dataB[[psiB.i]]<-Sim.data
    }
    Comb.dataA[[psiA.i]]<-Comb.dataB
  }
  temp.GrdSize <- temp.Sites <- temp.Top.Model.fs <- temp.SIF.fs <- temp.Top.Model.ls <- temp.SIF.ls <- NULL
  temp.TPsiA <- temp.TPsiBA <- temp.TPsiBa <- NULL
  temp.PsiA_fs1 <- temp.PsiBA_fs1 <- temp.PsiBa_fs1 <- NULL
  temp.PsiA_fs2 <- temp.PsiBA_fs2 <- temp.PsiBa_fs2 <- NULL
  temp.PsiA_ls1 <- temp.PsiBA_ls1 <- temp.PsiBa_ls1 <- NULL
  temp.PsiA_ls2 <- temp.PsiBA_ls2 <- temp.PsiBa_ls2 <- NULL

  for(i in 1:length(Comb.dataA)){
    for(j in 1:length(Comb.dataA[[i]])){
      for(k in 1:length(Comb.dataA[[i]][[j]])){
        temp.GrdSize <- append(temp.GrdSize, Comb.dataA[[i]][[j]][[k]]$Sim.data$Simulation.Conditions$Gridsize)
        temp.Sites <- append(temp.Sites, Comb.dataA[[i]][[j]][[k]]$Sim.data$Simulation.Conditions$M)

        temp.TPsiA <- append(temp.TPsiA, Comb.dataA[[i]][[j]][[k]]$Sim.data$Simulation.Conditions$True_PsiA)
        temp.TPsiBA <- append(temp.TPsiBA, Comb.dataA[[i]][[j]][[k]]$Sim.data$Simulation.Conditions$True_PsiBA)
        temp.TPsiBa <- append(temp.TPsiBa, Comb.dataA[[i]][[j]][[k]]$Sim.data$Simulation.Conditions$True_PsiBa)

        temp.Top.Model.fs <- append(temp.Top.Model.fs, Comb.dataA[[i]][[j]][[k]]$OM.fs.data$top)
        temp.SIF.fs <- append(temp.SIF.fs, Comb.dataA[[i]][[j]][[k]]$OM.fs.data$SIF)
        temp.Top.Model.ls <- append(temp.Top.Model.ls, Comb.dataA[[i]][[j]][[k]]$OM.ls.data$top)
        temp.SIF.ls <- append(temp.SIF.ls, Comb.dataA[[i]][[j]][[k]]$OM.ls.data$SIF)

        temp.PsiA_fs1 <- append(temp.PsiA_fs1, Comb.dataA[[i]][[j]][[k]]$OM.fs.data$Ind_Model$real["psiA","est"])
        temp.PsiBA_fs1 <- append(temp.PsiBA_fs1, Comb.dataA[[i]][[j]][[k]]$OM.fs.data$Ind_Model$real["psiBA","est"])
        temp.PsiBa_fs1 <- append(temp.PsiBa_fs1, Comb.dataA[[i]][[j]][[k]]$OM.fs.data$Ind_Model$real["psiBa","est"])

        temp.PsiA_fs2 <- append(temp.PsiA_fs2, Comb.dataA[[i]][[j]][[k]]$OM.fs.data$Inf_Model$real["psiA","est"])
        temp.PsiBA_fs2 <- append(temp.PsiBA_fs2, Comb.dataA[[i]][[j]][[k]]$OM.fs.data$Inf_Model$real["psiBA","est"])
        temp.PsiBa_fs2 <- append(temp.PsiBa_fs2, Comb.dataA[[i]][[j]][[k]]$OM.fs.data$Inf_Model$real["psiBa","est"])

        temp.PsiA_ls1 <- append(temp.PsiA_ls1, Comb.dataA[[i]][[j]][[k]]$OM.ls.data$Ind_Model$real["psiA","est"])
        temp.PsiBA_ls1 <- append(temp.PsiBA_ls1, Comb.dataA[[i]][[j]][[k]]$OM.ls.data$Ind_Model$real["psiBA","est"])
        temp.PsiBa_ls1 <- append(temp.PsiBa_ls1, Comb.dataA[[i]][[j]][[k]]$OM.ls.data$Ind_Model$real["psiBa","est"])

        temp.PsiA_ls2 <- append(temp.PsiA_ls2, Comb.dataA[[i]][[j]][[k]]$OM.ls.data$Inf_Model$real["psiA","est"])
        temp.PsiBA_ls2 <- append(temp.PsiBA_ls2, Comb.dataA[[i]][[j]][[k]]$OM.ls.data$Inf_Model$real["psiBA","est"])
        temp.PsiBa_ls2 <- append(temp.PsiBa_ls2, Comb.dataA[[i]][[j]][[k]]$OM.ls.data$Inf_Model$real["psiBa","est"])
      }
    }
  }
  mod.summary <- data.frame(GrdSize = temp.GrdSize, Sites = temp.Sites,
                            TPsiA = temp.TPsiA, TPsiBA = temp.TPsiBA, TPsiBa = temp.TPsiBa,
                            PsiA_fs1 = temp.PsiA_fs1, PsiBA_fs1 = temp.PsiBA_fs1, PsiBa_fs1 = temp.PsiBa_fs1,
                            PsiA_fs2 = temp.PsiA_fs2, PsiBA_fs2 = temp.PsiBA_fs2, PsiBa_fs2 = temp.PsiBa_fs2,
                            Top.Model.fs = temp.Top.Model.fs, SIF.fs = temp.SIF.fs,
                            PsiA_ls1 = temp.PsiA_ls1, PsiBA_ls1 = temp.PsiBA_ls1, PsiBa_ls1 = temp.PsiBa_ls1,
                            PsiA_ls2 = temp.PsiA_ls2, PsiBA_ls2 = temp.PsiBA_ls2, PsiBa_ls2 = temp.PsiBa_ls2,
                            Top.Model.ls = temp.Top.Model.ls, SIF.ls = temp.SIF.ls)
  mod.results <- Comb.dataA
  return(list(Sim.summary = mod.summary, Sim.results = mod.results))
}

#--Function to supply with conditions that will call other functions and store data
SimWrapper2<-function(Grd, M, J, Occ, psiA, psiBA, psiBa, pA, pB, dist.list, prM = 0.01, prMA = NULL, ci = 0.95, Site.column = TRUE){
  if(psiBA==psiBa){
    temp.sim <- Sim_Occ(Grd = Grd, M = M, psiA = psiA, psiB = psiBA)
    temp.sim.data <- temp.sim$Sim.data
  }else{
    temp.sim <- Sim_Occ2(Grd = Grd, M = M, psiA = psiA, psiBA = psiBA, psiBa = psiBa)
    temp.sim.data <- temp.sim$Sim.data
  }
  temp.sim.dataA <- temp.sim.data[,1:2]
  temp.sim.dataB <- temp.sim.data[,c(1,3)]

  temp.resultsA <- Sim_Occ_NC_A(Sim.data = temp.sim.dataA, dist.list = dist.list, J = J, prM = prM)
  temp.resultsB <- Sim_Occ_NC_B(Sim.dataA = temp.resultsA, Sim.dataB = temp.sim.dataB, dist.list = dist.list, J = J, prM = prM, prMA = prMA, psiBA = psiBA, psiBa = psiBa)
  temp.data <- sample_sim(SpeciesA.data = temp.resultsA, SpeciesB.data = temp.resultsB, M = M)
  temp.eh <-eh2(sample.dataA = temp.data$Sample.dataA, sample.dataB = temp.data$Sample.dataB, J = J, Occ = Occ, pA = pA, pB = pB, Site.column = Site.column)

  temp.fs.om <- OccMods(ehA = temp.eh$ehA, ehB = temp.eh$ehB)
  if(!is.null(temp.eh$ehAcon)){
    temp.ls.om <- OccMods(ehA = temp.eh$ehAcon, ehB = temp.eh$ehBcon)
  }else{
    temp.ls.om <- NULL
  }
  return(list(Sim.data = temp.sim, EH = temp.eh, OM.fs.data = temp.fs.om, OM.ls.data = temp.ls.om))
}

#--Function to create USE records for a species over J surveys with a Probability of Movement between surveys >0
Sim_Occ_NC_A<-function(Sim.data, dist.list, J, prM = 0.01){
  temp.A <- Sim.data
  for(j in 1:(J-1)){
    temp.jnext<-temp.A[,c(1,ncol(temp.A))]
    colnames(temp.jnext)<-c("Site", "psiA")
    temp.change<-temp.jnext
    temp.A.random<-temp.A[sample(nrow(temp.A)),c(1,ncol(temp.A))]
    temp.A.ss <- temp.A.random[temp.A.random[,ncol(temp.A.random)]==1,]
    for(i in 1:nrow(temp.A.ss)){
      temp.move <- rbinom(1,1,prM)
      temp <- temp.change
      if(temp.move==1){
        dist.df <- dist.list[[temp.A.ss[i,"Site"]]]
        temp <- temp[-c(temp$Site==temp.A.ss[i,"Site"]),]
        temp <- temp[temp$psiA==0,]
        temp <- merge(dist.df,temp)
        temp.idx<-sample(temp$Site, 1, prob=(1/temp$Dist)/sum(1/temp$Dist))
        temp.change[temp.change$Site==temp.idx,"psiA"] <- 1
        temp.change[temp.change$Site==temp.A.ss[i,"Site"],"psiA"] <- 0
      }
    }
    temp.A <- data.frame(temp.A, temp.change$psiA)
  }
  colnames(temp.A)<-c("Site", paste("S", 1:J, sep=""))
  return(Sim.data = temp.A)
}

#--Function to create USE records for species B when influenced by Species A, over J surveys with a Probability of Movement between surveys >0
Sim_Occ_NC_B <- function(Sim.dataA, Sim.dataB, dist.list, J, prM = 0.01, prMA = NULL, psiBA, psiBa){
  temp.B <- Sim.dataB; temp.A <- Sim.dataA
  if(is.null(prMA)){
    prMA <- prM/0.5
    if(prMA > 1){
      prMA <- floor(prMA)
    }
  }
  for(j in 1:(J-1)){
    temp.jnext<-temp.B[,c(1,ncol(temp.B))]
    colnames(temp.jnext)<-c("Site", "psiB")
    temp.change<-temp.jnext
    temp.B.random<-temp.B[sample(nrow(temp.B)),c(1,ncol(temp.B))]
    temp.B.ss <- temp.B.random[temp.B.random[,ncol(temp.B.random)]==1,]
    for(i in 1:nrow(temp.B.ss)){
      temp.A.use <- temp.A[temp.A$Site==temp.B.ss$Site[i],j+1]
      if(temp.A.use==0){
        temp.move <- rbinom(1,1,prM)
      } else{
        temp.move <- rbinom(1,1,prMA)
      }
      temp <- temp.change
      if(temp.move==1){
        dist.df <- dist.list[[temp.B.ss[i,"Site"]]]
        temp <- temp[-c(temp$Site==temp.B.ss[i,"Site"]),]
        temp <- temp[temp$psiB==0,]
        temp <- merge(dist.df,temp)
        temp.settled <- 0
        while(temp.settled==0){
          temp.idx<-sample(temp$Site, 1, prob=(1/temp$Dist)/sum(1/temp$Dist))
          temp.A.use <- temp.A[temp.A$Site==temp.idx,j+1]
          if(temp.A.use==0){
            temp.settled <- rbinom(1,1,psiBa)
          } else{
            temp.settled <- rbinom(1,1,psiBA)
          }
        }
        temp.change[temp.change$Site==temp.idx,"psiB"] <- 1
        temp.change[temp.change$Site==temp.B.ss[i,"Site"],"psiB"] <- 0
      }
    }
    temp.B <- data.frame(temp.B, temp.change$psiB)
  }
  colnames(temp.B)<-c("Site", paste("S", 1:J, sep=""))
  return(Sim.data = temp.B)
}

#--Function to generate sample data for patterns of use for both species from simulated datasets when closure is not met
sample_sim <- function(SpeciesA.data, SpeciesB.data, M = 100){
  if(all(dim(SpeciesA.data)==dim(SpeciesB.data))){
    temp.site.idx<-sort(sample(1:nrow(SpeciesA.data),M))
    sample.dataA <- SpeciesA.data[temp.site.idx,]
    sample.dataB <- SpeciesB.data[temp.site.idx,]
    return(list(Sample.dataA = sample.dataA, Sample.dataB = sample.dataB))
  } else{
    return("Error - Simulated data for each species are not the same dimensions")
  }
}

#--Function eh2() uses site-specific patterns of use to create Encounter data for J surveys (which can later be condensed to Occ length) based on pA and pB
eh2<-function(sample.dataA, sample.dataB, J, Occ = J, pA, pB, Site.column = TRUE) {
  if(Site.column==TRUE){
    Site <- sample.dataA[,1]
    sample.dataA <- sample.dataA[,-c(1)]
    sample.dataB <- sample.dataB[,-c(1)]
  }
  temp<-sample.dataA
  temp[!is.na(temp)]<-NA
  ehA.data <- ehB.data <- temp
  for(site.i in 1:nrow(sample.dataA)){
    for(survey.j in 1:J){
      probA <- sample.dataA[site.i,survey.j]*pA
      ehA.data[site.i,survey.j] <- rbinom(1,1,probA)
      probB <- sample.dataB[site.i,survey.j]*pB
      ehB.data[site.i,survey.j] <- rbinom(1,1,probB)
    }
  }
  if(Occ<J){
    NoOcc <- J/Occ
    ehAcon.data <- ehBcon.data <- data.frame(row.names=c(1:NoOcc))
    for(site.i in 1:nrow(sample.dataA)){
      for(survey.j in 1:NoOcc){
        if(sum(ehA.data[site.i,((survey.j*Occ)-Occ+1):(survey.j*Occ)])==0){
          ehAcon.data[site.i, survey.j]<-0
        } else {
          ehAcon.data[site.i, survey.j]<-1
        }
        if(sum(ehB.data[site.i,((survey.j*Occ)-Occ+1):(survey.j*Occ)])==0){
          ehBcon.data[site.i, survey.j]<-0
        } else {
          ehBcon.data[site.i, survey.j]<-1
        }
      }
    }
  }else {
    ehAcon.data <- ehBcon.data <- NULL
  }
  return(list(ehA = ehA.data, ehB = ehB.data, ehAcon = ehAcon.data, ehBcon = ehBcon.data))
}

#--Function RunSummary() uses the $Sim.summary output file from the simulations to generate a SIMPLE summary table of the number of simulations that found support for INDEPENDENCE vs. NON-INDEPENDENCE
RunSummary <- function(Sim.summary, closure = TRUE, Pattern, Sims){
  temp <- Sim.summary
  temp.results <- data.frame(matrix(ncol = 13, nrow = 0))
  for (psiA.i in 1:length(unique(temp$TPsiA))){
    temp.A <- temp[temp$TPsiA==unique(temp$TPsiA)[psiA.i],]
    for(psiBA.i in 1:length(unique(temp.A$TPsiBA))){
      temp.B <- temp.A[temp.A$TPsiBA==unique(temp.A$TPsiBA)[psiBA.i],]
      temp.results <- rbind(temp.results,c(unique(temp.B$TPsiA),unique(temp.B$TPsiBA),unique(temp.B$TPsiBa), unique(temp.B$Sites), Sims, sum(temp.B$Top.Model.fs=="Inf" | temp.B$Top.Model.fs=="Ind"),
                                           sum(temp.B$Top.Model.fs=="Inf"), sum(temp.B$Top.Model.fs=="Ind"), sum(temp.B$Top.Model.fs=="Ind")/(sum(temp.B$Top.Model.fs=="Inf" | temp.B$Top.Model.fs=="Ind")),
                                           sum(temp.B$Top.Model.ls=="Inf" | temp.B$Top.Model.ls=="Ind"),
                                           sum(temp.B$Top.Model.ls=="Inf"), sum(temp.B$Top.Model.ls=="Ind"), sum(temp.B$Top.Model.ls=="Ind")/(sum(temp.B$Top.Model.ls=="Inf" | temp.B$Top.Model.ls=="Ind"))))
    }
  }
  colnames(temp.results) <- c("TPsiA", "TPsiBA", "TPsiBa", "M", "Sims_Ran", "Sims_Con.fs",  "N.Inf.fs", "N.Ind.fs", "Perc.Ind.fs", "Sims_Con.ls",  "N.Inf.ls", "N.Ind.ls", "Perc.Ind.ls")
  if(closure==TRUE){
    Closed <- rep("Yes", nrow(temp.results))
  } else Closed <- rep("No", nrow(temp.results))
  Pattern <- rep(Pattern, nrow(temp.results))
  temp.results<-cbind(Closed, cbind(Pattern, temp.results))
  return(temp.results)
}

#--Function RunSummary2() operates on a single output file to produce a list of 9 elements, each with a dataframe with 500 (simulation) rows reporting the estimated psi values (with CI), delta AIC, delta LL, and SIF values
RunSummary2 <- function(Sim.summary, Sim.results, Sims){
  temp <- Sim.summary
  temp2 <- Sim.results
  temp.sim.results<-list()
  for (psiA.i in 1:length(unique(temp$TPsiA))){
    for (psiB.i in 1:length(unique(temp$TPsiBA))){
      temp.results <- data.frame(matrix(ncol = 30, nrow = 0))
        x<-c("Sim", "TPsiA", "TPsiBA", "TPsiBa", "fs.psiA", "fs.psiA_LCL", "fs.psiA_UCL", "fs.psiBA", "fs.psiBA_LCL", "fs.psiBA_UCL", "fs.psiBa", "fs.psiBa_LCL", "fs.psiBa_UCL", "fs.Top", "fs.D.AIC",	"fs.D.LL", "fs.SIF",
               "ls.psiA", "ls.psiA_LCL", "ls.psiA_UCL", "ls.psiBA", "ls.psiBA_LCL", "ls.psiBA_UCL", "ls.psiBa", "ls.psiBa_LCL", "ls.psiBa_UCL", "ls.Top",	"ls.D.AIC",	"ls.D.LL", "ls.SIF")
        colnames(temp.results)<-x
      for(sim.i in 1:Sims){
        temp2.1<-temp2[[psiA.i]][[psiB.i]][[sim.i]]
          temp.results[sim.i,1]<-sim.i
          temp.results[sim.i,2:4]<-unname(temp2.1$Sim.data$Simulation.Conditions[c(3:5)])

          if(temp2.1$OM.fs.data$top == "Inf"){
            temp.results[sim.i,5:13]<-unname(c(temp2.1$OM.fs.data$Inf_Model$real[1,1:3],temp2.1$OM.fs.data$Inf_Model$real[3,1:3],temp2.1$OM.fs.data$Inf_Model$real[2,1:3]))
            } else if(temp2.1$OM.fs.data$top == "Ind"){
              temp.results[sim.i,5:13]<-unname(c(temp2.1$OM.fs.data$Ind_Model$real[1,1:3],temp2.1$OM.fs.data$Ind_Model$real[3,1:3],temp2.1$OM.fs.data$Ind_Model$real[2,1:3]))
              } else {
                temp.results[sim.i,5:13]<-NA
                }
          temp.results[sim.i,14]<-temp2.1$OM.fs.data$top
          temp.results[sim.i,15:16]<-c(temp2.1$OM.fs.data$Mod.Table$Delta[2], temp2.1$OM.fs.data$Mod.Table$LL[2]-temp2.1$OM.fs.data$Mod.Table$LL[1])
          if(temp2.1$OM.fs.data$top == "Inf"){
            temp.results[sim.i,17]<-temp2.1$OM.fs.data$SIF
            } else {
              temp.results[sim.i,17]<-NA
              }

          if(temp2.1$OM.ls.data$top == "Inf"){
            temp.results[sim.i,18:26]<-unname(c(temp2.1$OM.ls.data$Inf_Model$real[1,1:3],temp2.1$OM.ls.data$Inf_Model$real[3,1:3],temp2.1$OM.ls.data$Inf_Model$real[2,1:3]))
            } else if(temp2.1$OM.ls.data$top == "Ind"){
              temp.results[sim.i,18:26]<-unname(c(temp2.1$OM.ls.data$Ind_Model$real[1,1:3],temp2.1$OM.ls.data$Ind_Model$real[3,1:3],temp2.1$OM.ls.data$Ind_Model$real[2,1:3]))
              } else {
                temp.results[sim.i,18:26]<-NA
                }
          temp.results[sim.i,27]<-temp2.1$OM.ls.data$top
          temp.results[sim.i,28:29]<-c(temp2.1$OM.ls.data$Mod.Table$Delta[2], temp2.1$OM.ls.data$Mod.Table$LL[2]-temp2.1$OM.ls.data$Mod.Table$LL[1])
          if(temp2.1$OM.ls.data$top == "Inf"){
            temp.results[sim.i,30]<-temp2.1$OM.ls.data$SIF
            } else {
              temp.results[sim.i,30]<-NA
              }
        }
        temp.sim.results[[length(temp.sim.results)+1]] <-temp.results
      }
    }
  return(temp.sim.results)
}

#--Function RunSummary3() uses out put from RunSummary2 to generate List with 9 elements summarizing the results for each analysis
RunSummary3<-function(Simulation.data, pattern = c("Independence", "Avoidance", "Aggregation" ), Scale = c("fs", "ls"), closed = TRUE, M, Sims){
  temp<-Simulation.data
  TPsiA <- TPsiBA <- TPsiBa <- mn.psiA <- se.psiA <- bias.psiA <- mn.psiBA <- se.psiBA<-bias.psiBA<-mn.psiBa<-se.psiBa <-
    bias.psiBa<-PC.psiA<-PC.psiBA<-PC.psiBa<-N.F<-N.Ind<-P.Ind<-N.Inf<-P.Inf<-DAICmin<-DAICmax<-
    N.Inf_Ind<-Adj.N.Pattern<-Adj.P.Pattern<-SIF.mn<-SIF.se<-SIF.min<-SIF.max<-NULL
  if(Scale == "fs"){
    for(cond.i in 1:length(temp)){
      TPsiA <- append(TPsiA,unname(temp[[cond.i]][1,2]))
      TPsiBA <- append(TPsiBA,unname(temp[[cond.i]][1,3]))
      TPsiBa <- append(TPsiBa,unname(temp[[cond.i]][1,4]))

      mn.psiA <- append(mn.psiA, mean(temp[[cond.i]]$fs.psiA, na.rm=TRUE))
      se.psiA <- append(se.psiA, SE(temp[[cond.i]]$fs.psiA))
      bias.psiA <- append(bias.psiA, bias(temp[[cond.i]]$fs.psiA[!is.na(temp[[cond.i]]$fs.psiA)], parameter = temp[[cond.i]]$TPsiA[1]))

      mn.psiBA <- append(mn.psiBA, mean(temp[[cond.i]]$fs.psiBA, na.rm=TRUE))
      se.psiBA <- append(se.psiBA, SE(temp[[cond.i]]$fs.psiBA))
      bias.psiBA <- append(bias.psiBA, bias(temp[[cond.i]]$fs.psiBA[!is.na(temp[[cond.i]]$fs.psiBA)], parameter = temp[[cond.i]]$TPsiBA[1]))

      mn.psiBa <- append(mn.psiBa, mean(temp[[cond.i]]$fs.psiBa, na.rm=TRUE))
      se.psiBa <- append(se.psiBa, SE(temp[[cond.i]]$fs.psiBa))
      bias.psiBa <- append(bias.psiBa, bias(temp[[cond.i]]$fs.psiBa[!is.na(temp[[cond.i]]$fs.psiBa)], parameter = temp[[cond.i]]$TPsiBa[1]))

      temp.A<-NULL
      for(row.i in 1:nrow(temp[[cond.i]])){
        temp.A<-append(temp.A,between(temp[[cond.i]]$TPsiA[row.i], temp[[cond.i]]$fs.psiA_LCL[row.i], temp[[cond.i]]$fs.psiA_UCL[row.i]))
      }
      temp.A<-sum(temp.A==TRUE, na.rm=TRUE)/length(temp.A[!is.na(temp.A)])
      PC.psiA <- append(PC.psiA, temp.A)

      temp.BA<-NULL
      for(row.i in 1:nrow(temp[[cond.i]])){
        temp.BA<-append(temp.BA,between(temp[[cond.i]]$TPsiBA[row.i], temp[[cond.i]]$fs.psiBA_LCL[row.i], temp[[cond.i]]$fs.psiBA_UCL[row.i]))
      }
      temp.BA<-sum(temp.BA==TRUE, na.rm=TRUE)/length(temp.BA[!is.na(temp.BA)])
      PC.psiBA <- append(PC.psiBA, temp.BA)

      temp.Ba<-NULL
      for(row.i in 1:nrow(temp[[cond.i]])){
        temp.Ba<-append(temp.Ba,between(temp[[cond.i]]$TPsiBa[row.i], temp[[cond.i]]$fs.psiBa_LCL[row.i], temp[[cond.i]]$fs.psiBa_UCL[row.i]))
      }
      temp.Ba<-sum(temp.Ba==TRUE, na.rm=TRUE)/length(temp.Ba[!is.na(temp.Ba)])
      PC.psiBa <- append(PC.psiBa, temp.Ba)

      N.F<-append(N.F, sum(is.na(temp[[cond.i]]$fs.Top)))

      N.Ind<-append(N.Ind, sum(temp[[cond.i]]$fs.Top=="Ind"))
      P.Ind<-append(P.Ind, sum(temp[[cond.i]]$fs.Top=="Ind")/(nrow(temp[[cond.i]])-sum(is.na(temp[[cond.i]]$fs.Top))))

      N.Inf<-append(N.Inf, sum(temp[[cond.i]]$fs.Top=="Inf"))
      P.Inf<-append(P.Inf, sum(temp[[cond.i]]$fs.Top=="Inf")/(nrow(temp[[cond.i]])-sum(is.na(temp[[cond.i]]$fs.Top))))

      if(pattern == "Independence"){
        DAICmin<-append(DAICmin, min(subset(temp[[cond.i]], temp[[cond.i]]$fs.Top=="Inf")$fs.D.AIC, na.rm=TRUE))
        DAICmax<-append(DAICmax, max(subset(temp[[cond.i]], temp[[cond.i]]$fs.Top=="Inf")$fs.D.AIC, na.rm=TRUE))
        N.Inf_Ind<-append(N.Inf_Ind, nrow(subset(temp[[cond.i]], temp[[cond.i]]$fs.Top=="Inf" & temp[[cond.i]]$fs.D.AIC < 2)))
        Adj.N.Pattern<-append(Adj.N.Pattern, c(sum(temp[[cond.i]]$fs.Top=="Ind"))+ nrow(subset(temp[[cond.i]], temp[[cond.i]]$fs.Top=="Inf" & temp[[cond.i]]$fs.D.AIC < 2)))
        Adj.P.Pattern<-append(Adj.P.Pattern, (c(sum(temp[[cond.i]]$fs.Top=="Ind"))+ nrow(subset(temp[[cond.i]], temp[[cond.i]]$fs.Top=="Inf" & temp[[cond.i]]$fs.D.AIC < 2)))/(Sims-sum(is.na(temp[[cond.i]]$fs.Top))))
      } else {
        DAICmin<-append(DAICmin, min(subset(temp[[cond.i]], temp[[cond.i]]$fs.Top=="Ind")$fs.D.AIC, na.rm=TRUE))
        DAICmax<-append(DAICmax, max(subset(temp[[cond.i]], temp[[cond.i]]$fs.Top=="Ind")$fs.D.AIC, na.rm=TRUE))
        N.Inf_Ind<-append(N.Inf_Ind, nrow(subset(temp[[cond.i]], temp[[cond.i]]$fs.Top=="Ind" & temp[[cond.i]]$fs.D.AIC < 2)))
        Adj.N.Pattern<-append(Adj.N.Pattern, c(sum(temp[[cond.i]]$fs.Top=="Inf"))+ nrow(subset(temp[[cond.i]], temp[[cond.i]]$fs.Top=="Ind" & temp[[cond.i]]$fs.D.AIC < 2)))
        Adj.P.Pattern<-append(Adj.P.Pattern, (c(sum(temp[[cond.i]]$fs.Top=="Inf"))+ nrow(subset(temp[[cond.i]], temp[[cond.i]]$fs.Top=="Ind" & temp[[cond.i]]$fs.D.AIC < 2)))/(Sims-sum(is.na(temp[[cond.i]]$fs.Top))))
      }

      SIF.mn<-append(SIF.mn, mean(temp[[cond.i]]$fs.SIF, na.rm=TRUE))
      SIF.se<-append(SIF.se, SE(temp[[cond.i]]$fs.SIF))
      SIF.min<-append(SIF.min, min(temp[[cond.i]]$fs.SIF, na.rm=TRUE))
      SIF.max<-append(SIF.max, max(temp[[cond.i]]$fs.SIF, na.rm=TRUE))

      temp.results <- cbind(TPsiA, TPsiBA, TPsiBa, mn.psiA, se.psiA, bias.psiA, mn.psiBA, se.psiBA, bias.psiBA, mn.psiBa, se.psiBa, bias.psiBa,
                            PC.psiA, PC.psiBA, PC.psiBa, N.F, N.Ind, P.Ind, N.Inf, P.Inf, DAICmin, DAICmax, N.Inf_Ind, Adj.N.Pattern, Adj.P.Pattern,
                            SIF.mn, SIF.se, SIF.min, SIF.max)
      temp.results<-data.frame(temp.results)
      Pattern<-rep(pattern,nrow(temp.results))
      TScale<-rep(Scale,nrow(temp.results))
      Closed<-rep(closed,nrow(temp.results))
      Sites<-rep(M,nrow(temp.results))
      temp.results <- cbind(Pattern, TScale, Closed, Sites, temp.results)

    }
  } else if(Scale == "ls"){
    for(cond.i in 1:length(temp)){
      #True values for PsiA, PsiBA, and PsiBa
      TPsiA <- append(TPsiA,unname(temp[[cond.i]][1,2]))
      TPsiBA <- append(TPsiBA,unname(temp[[cond.i]][1,3]))
      TPsiBa <- append(TPsiBa,unname(temp[[cond.i]][1,4]))

      mn.psiA <- append(mn.psiA, mean(temp[[cond.i]]$ls.psiA, na.rm=TRUE))
      se.psiA <- append(se.psiA, SE(temp[[cond.i]]$ls.psiA))
      bias.psiA <- append(bias.psiA, bias(temp[[cond.i]]$ls.psiA[!is.na(temp[[cond.i]]$ls.psiA)], parameter = temp[[cond.i]]$TPsiA[1]))

      mn.psiBA <- append(mn.psiBA, mean(temp[[cond.i]]$ls.psiBA, na.rm=TRUE))
      se.psiBA <- append(se.psiBA, SE(temp[[cond.i]]$ls.psiBA))
      bias.psiBA <- append(bias.psiBA, bias(temp[[cond.i]]$ls.psiBA[!is.na(temp[[cond.i]]$ls.psiBA)], parameter = temp[[cond.i]]$TPsiBA[1]))

      mn.psiBa <- append(mn.psiBa, mean(temp[[cond.i]]$ls.psiBa, na.rm=TRUE))
      se.psiBa <- append(se.psiBa, SE(temp[[cond.i]]$ls.psiBa))
      bias.psiBa <- append(bias.psiBa, bias(temp[[cond.i]]$ls.psiBa[!is.na(temp[[cond.i]]$ls.psiBa)], parameter = temp[[cond.i]]$TPsiBa[1]))

      temp.A<-NULL
      for(row.i in 1:nrow(temp[[cond.i]])){
        temp.A<-append(temp.A,between(temp[[cond.i]]$TPsiA[row.i], temp[[cond.i]]$ls.psiA_LCL[row.i], temp[[cond.i]]$ls.psiA_UCL[row.i]))
      }
      temp.A<-sum(temp.A==TRUE, na.rm=TRUE)/length(temp.A[!is.na(temp.A)])
      PC.psiA <- append(PC.psiA, temp.A)

      temp.BA<-NULL
      for(row.i in 1:nrow(temp[[cond.i]])){
        temp.BA<-append(temp.BA,between(temp[[cond.i]]$TPsiBA[row.i], temp[[cond.i]]$ls.psiBA_LCL[row.i], temp[[cond.i]]$ls.psiBA_UCL[row.i]))
      }
      temp.BA<-sum(temp.BA==TRUE, na.rm=TRUE)/length(temp.BA[!is.na(temp.BA)])
      PC.psiBA <- append(PC.psiBA, temp.BA)

      temp.Ba<-NULL
      for(row.i in 1:nrow(temp[[cond.i]])){
        temp.Ba<-append(temp.Ba,between(temp[[cond.i]]$TPsiBa[row.i], temp[[cond.i]]$ls.psiBa_LCL[row.i], temp[[cond.i]]$ls.psiBa_UCL[row.i]))
      }
      temp.Ba<-sum(temp.Ba==TRUE, na.rm=TRUE)/length(temp.Ba[!is.na(temp.Ba)])
      PC.psiBa <- append(PC.psiBa, temp.Ba)

      N.F<-append(N.F, sum(is.na(temp[[cond.i]]$ls.Top)))

      N.Ind<-append(N.Ind, sum(temp[[cond.i]]$ls.Top=="Ind"))
      P.Ind<-append(P.Ind, sum(temp[[cond.i]]$ls.Top=="Ind")/(nrow(temp[[cond.i]])-sum(is.na(temp[[cond.i]]$ls.Top))))

      N.Inf<-append(N.Inf, sum(temp[[cond.i]]$ls.Top=="Inf"))
      P.Inf<-append(P.Inf, sum(temp[[cond.i]]$ls.Top=="Inf")/(nrow(temp[[cond.i]])-sum(is.na(temp[[cond.i]]$ls.Top))))

      if(pattern == "Independence"){
        DAICmin<-append(DAICmin, min(subset(temp[[cond.i]], temp[[cond.i]]$ls.Top=="Inf")$ls.D.AIC, na.rm=TRUE))
        DAICmax<-append(DAICmax, max(subset(temp[[cond.i]], temp[[cond.i]]$ls.Top=="Inf")$ls.D.AIC, na.rm=TRUE))
        N.Inf_Ind<-append(N.Inf_Ind, nrow(subset(temp[[cond.i]], temp[[cond.i]]$ls.Top=="Inf" & temp[[cond.i]]$ls.D.AIC < 2)))
        Adj.N.Pattern<-append(Adj.N.Pattern, c(sum(temp[[cond.i]]$ls.Top=="Ind"))+ nrow(subset(temp[[cond.i]], temp[[cond.i]]$ls.Top=="Inf" & temp[[cond.i]]$ls.D.AIC < 2)))
        Adj.P.Pattern<-append(Adj.P.Pattern, (c(sum(temp[[cond.i]]$ls.Top=="Ind"))+ nrow(subset(temp[[cond.i]], temp[[cond.i]]$ls.Top=="Inf" & temp[[cond.i]]$ls.D.AIC < 2)))/(Sims-sum(is.na(temp[[cond.i]]$ls.Top))))
      } else {
        DAICmin<-append(DAICmin, min(subset(temp[[cond.i]], temp[[cond.i]]$ls.Top=="Ind")$ls.D.AIC, na.rm=TRUE))
        DAICmax<-append(DAICmax, max(subset(temp[[cond.i]], temp[[cond.i]]$ls.Top=="Ind")$ls.D.AIC, na.rm=TRUE))
        N.Inf_Ind<-append(N.Inf_Ind, nrow(subset(temp[[cond.i]], temp[[cond.i]]$ls.Top=="Ind" & temp[[cond.i]]$ls.D.AIC < 2)))
        Adj.N.Pattern<-append(Adj.N.Pattern, c(sum(temp[[cond.i]]$ls.Top=="Inf"))+ nrow(subset(temp[[cond.i]], temp[[cond.i]]$ls.Top=="Ind" & temp[[cond.i]]$ls.D.AIC < 2)))
        Adj.P.Pattern<-append(Adj.P.Pattern, (c(sum(temp[[cond.i]]$ls.Top=="Inf"))+ nrow(subset(temp[[cond.i]], temp[[cond.i]]$ls.Top=="Ind" & temp[[cond.i]]$ls.D.AIC < 2)))/(Sims-sum(is.na(temp[[cond.i]]$ls.Top))))
      }

      SIF.mn<-append(SIF.mn, mean(temp[[cond.i]]$ls.SIF, na.rm=TRUE))
      SIF.se<-append(SIF.se, SE(temp[[cond.i]]$ls.SIF))
      SIF.min<-append(SIF.min, min(temp[[cond.i]]$ls.SIF, na.rm=TRUE))
      SIF.max<-append(SIF.max, max(temp[[cond.i]]$ls.SIF, na.rm=TRUE))

      temp.results <- cbind(TPsiA, TPsiBA, TPsiBa, mn.psiA, se.psiA, bias.psiA, mn.psiBA, se.psiBA, bias.psiBA, mn.psiBa, se.psiBa, bias.psiBa,
                            PC.psiA, PC.psiBA, PC.psiBa, N.F, N.Ind, P.Ind, N.Inf, P.Inf, DAICmin, DAICmax, N.Inf_Ind, Adj.N.Pattern, Adj.P.Pattern,
                            SIF.mn, SIF.se, SIF.min, SIF.max)
      temp.results<-data.frame(temp.results)
      Pattern<-rep(pattern,nrow(temp.results))
      TScale<-rep(Scale,nrow(temp.results))
      Closed<-rep(closed,nrow(temp.results))
      Sites<-rep(M,nrow(temp.results))
      temp.results <- cbind(Pattern, TScale, Closed, Sites, temp.results)

    }
  }
  return(temp.results)
}

#--Simple function to calculate standard error
SE <- function(x){
  sd(x, na.rm=TRUE)/sqrt(length(x) - sum(is.na(x)))
}

#################---------START Code for Running Analyses---------#################
###################################################################################
##IMPORT DATA FOR A GRID THAT CAN BE USED AS THE SPATIAL EXTENT FOR SAMPLING.

#Import Grid data
library(readxl)
SimGrid <- read_excel("~/RDatasets/CoOccSim/SimGrid_sites.xls")
View(SimGrid)

temp.grid <- SimGrid
temp.site <- temp.x <- temp.y <- NULL
for(i in 1:length(unique(temp.grid$Site))){
  temp.site <- append(temp.site, i)
  temp.x <- append(temp.x, mean(temp.grid[temp.grid$Site==i, "X"]))
  temp.y <- append(temp.y, mean(temp.grid[temp.grid$Site==i, "Y"]))
}
temp.grid <- data.frame(Site = temp.site, X = temp.x, Y = temp.y)

##--Determine PW distances between points
Site.dist <- list()
for(i in 1:nrow(temp.grid)){
  temp.site <- temp.dist <- NULL
  temp.grid.red <- temp.grid[-i,]
  temp.loc <- temp.grid[i,]
  for(j in 1:nrow(temp.grid.red)){
    temp.dist<-append(temp.dist,round(sqrt((temp.loc[1,"X"] - temp.grid.red[j,"X"])^2 + (temp.loc[1,"Y"] - temp.grid.red[j,"Y"])^2),0))
    temp.site<-append(temp.site, temp.grid.red[j,"Site"])
  }
  Site.dist[[i]] <- data.frame(Site = temp.site, Dist = temp.dist)
}
rm(list=ls(pattern="^temp"))

library(wiqid)
library(SimDesign)
library(dplyr)

#ANALYSES with M = 100 or M = 250 sites and Sims = 500 when CLOSURE IS MET
#Independence
Ind.Sim500.M100.c <- RunClosure(Grd = length(unique(SimGrid$Site)), M = 100, J = 21, Occ = 7, psiA = c(0.4, 0.55, 0.7), psiBA = c(0.45, 0.55, 0.65), psiBa = c(0.45, 0.55, 0.65), pA = 0.05, pB = 0.05, Sims = 500, ci = 0.95)
Ind.Sim500.M250.c <- RunClosure(Grd = length(unique(SimGrid$Site)), M = 250, J = 21, Occ = 7, psiA = c(0.4, 0.55, 0.7), psiBA = c(0.45, 0.55, 0.65), psiBa = c(0.45, 0.55, 0.65), pA = 0.05, pB = 0.05, Sims = 500, ci = 0.95)

#Avoidance
Avd.Sim500.M100.c <- RunClosure(Grd = length(unique(SimGrid$Site)), M = 100, J = 21, Occ = 7, psiA = c(0.4, 0.55, 0.7), psiBA = c(0.25, 0.35, 0.45), psiBa = c(0.65, 0.75, 0.85), pA = 0.05, pB = 0.05, Sims = 500, ci = 0.95)
Avd.Sim500.M250.c <- RunClosure(Grd = length(unique(SimGrid$Site)), M = 250, J = 21, Occ = 7, psiA = c(0.4, 0.55, 0.7), psiBA = c(0.25, 0.35, 0.45), psiBa = c(0.65, 0.75, 0.85), pA = 0.05, pB = 0.05, Sims = 500, ci = 0.95)

#Aggregation
Agg.Sim500.M100.c <- RunClosure(Grd = length(unique(SimGrid$Site)), M = 100, J = 21, Occ = 7, psiA = c(0.4, 0.55, 0.7), psiBA = c(0.65, 0.75, 0.85), psiBa = c(0.25, 0.35, 0.45), pA = 0.05, pB = 0.05, Sims = 500, ci = 0.95)
Agg.Sim500.M250.c <- RunClosure(Grd = length(unique(SimGrid$Site)), M = 250, J = 21, Occ = 7, psiA = c(0.4, 0.55, 0.7), psiBA = c(0.65, 0.75, 0.85), psiBa = c(0.25, 0.35, 0.45), pA = 0.05, pB = 0.05, Sims = 500, ci = 0.95)


#ANALYSES with M = 100 or M = 250 sites and Sims = 500 when CLOSURE IS NOT MET
#Independence
Ind.Sim500.M100.nc<- RunNoClosure(Grd = length(unique(SimGrid$Site)), M = 100, J = 21, Occ = 7, psiA = c(0.4, 0.55, 0.7), psiBA = c(0.45, 0.55, 0.65), psiBa = c(0.45, 0.55, 0.65), pA = 0.05, pB = 0.05, prM = 0.02, prMA = 0.02, Sims = 500, dist.list = Site.dist, ci = 0.95, Site.column = TRUE)
Ind.Sim500.M250.nc <- RunNoClosure(Grd = length(unique(SimGrid$Site)), M = 250, J = 21, Occ = 7, psiA = c(0.4, 0.55, 0.7), psiBA = c(0.45, 0.55, 0.65), psiBa = c(0.45, 0.55, 0.65), pA = 0.05, pB = 0.05, prM = 0.02, prMA = 0.02, Sims = 500, dist.list = Site.dist, ci = 0.95, Site.column = TRUE)

#Avoidance
Avd.Sim500.M100.nc <- RunNoClosure(Grd = length(unique(SimGrid$Site)), M = 100, J = 21, Occ = 7, psiA = c(0.4, 0.55, 0.7), psiBA = c(0.25, 0.35, 0.45), psiBa = c(0.65, 0.75, 0.85), pA = 0.05, pB = 0.05, prM = 0.02, prMA = 0.02, Sims = 500, dist.list = Site.dist, ci = 0.95, Site.column = TRUE)
Avd.Sim500.M250.nc <- RunNoClosure(Grd = length(unique(SimGrid$Site)), M = 250, J = 21, Occ = 7, psiA = c(0.4, 0.55, 0.7), psiBA = c(0.25, 0.35, 0.45), psiBa = c(0.65, 0.75, 0.85), pA = 0.05, pB = 0.05, prM = 0.02, prMA = 0.02, Sims = 500, dist.list = Site.dist, ci = 0.95, Site.column = TRUE)

#Aggregation
Agg.Sim500.M100.nc <- RunNoClosure(Grd = length(unique(SimGrid$Site)), M = 100, J = 21, Occ = 7, psiA = c(0.4, 0.55, 0.7), psiBA = c(0.65, 0.75, 0.85), psiBa = c(0.25, 0.35, 0.45), pA = 0.05, pB = 0.05, prM = 0.02, prMA = 0.02, Sims = 500, dist.list = Site.dist, ci = 0.95, Site.column = TRUE)
Agg.Sim500.M250.nc <- RunNoClosure(Grd = length(unique(SimGrid$Site)), M = 250, J = 21, Occ = 7, psiA = c(0.4, 0.55, 0.7), psiBA = c(0.65, 0.75, 0.85), psiBa = c(0.25, 0.35, 0.45), pA = 0.05, pB = 0.05, prM = 0.02, prMA = 0.02, Sims = 500, dist.list = Site.dist, ci = 0.95, Site.column = TRUE)


#Summarize simulation results
temp1<-RunSummary(Sim.summary = Ind.Sim500.M100.c$Sim.summary, closure = TRUE, Pattern = c("Independence"),Sims = 500)
temp2<-RunSummary(Sim.summary = Ind.Sim500.M250.c$Sim.summary, closure = TRUE, Pattern = c("Independence"),Sims = 500)
temp3<-RunSummary(Sim.summary = Avd.Sim500.M100.c$Sim.summary, closure = TRUE, Pattern = c("Avoidance"),Sims = 500)
temp4<-RunSummary(Sim.summary = Avd.Sim500.M250.c$Sim.summary, closure = TRUE, Pattern = c("Avoidance"),Sims = 500)
temp5<-RunSummary(Sim.summary = Agg.Sim500.M100.c$Sim.summary, closure = TRUE, Pattern = c("Aggregation"),Sims = 500)
temp6<-RunSummary(Sim.summary = Agg.Sim500.M250.c$Sim.summary, closure = TRUE, Pattern = c("Aggregation"),Sims = 500)
Closure.results <- rbind(temp1, temp2, temp3, temp4, temp5, temp6)
rm(temp1, temp2, temp3, temp4, temp5, temp6)

temp1<-RunSummary(Sim.summary = Ind.Sim500.M100.nc$Sim.summary, closure = FALSE, Pattern = c("Independence"),Sims = 500)
temp2<-RunSummary(Sim.summary = Ind.Sim500.M250.nc$Sim.summary, closure = FALSE, Pattern = c("Independence"),Sims = 500)
temp3<-RunSummary(Sim.summary = Avd.Sim500.M100.nc$Sim.summary, closure = FALSE, Pattern = c("Avoidance"),Sims = 500)
temp4<-RunSummary(Sim.summary = Avd.Sim500.M250.nc$Sim.summary, closure = FALSE, Pattern = c("Avoidance"),Sims = 500)
temp5<-RunSummary(Sim.summary = Agg.Sim500.M100.nc$Sim.summary, closure = FALSE, Pattern = c("Aggregation"),Sims = 500)
temp6<-RunSummary(Sim.summary = Agg.Sim500.M250.nc$Sim.summary, closure = FALSE, Pattern = c("Aggregation"),Sims = 500)
NoClosure.results <- rbind(temp1, temp2, temp3, temp4, temp5, temp6)
rm(temp1, temp2, temp3, temp4, temp5, temp6)

#Compile results for CLOSURE
temp.data<-Ind.Sim500.M100.c
  temp.input <- RunSummary2(Sim.summary = temp.data$Sim.summary, Sim.results = temp.data$Sim.results, Sims = 500)
  temp.results1<-RunSummary3(Simulation.data=temp.input, pattern = "Independence", Scale = "fs", closed = TRUE, M = 100, Sims = 500)
  temp.results2<-RunSummary3(Simulation.data=temp.input, pattern = "Independence", Scale = "ls", closed = TRUE, M = 100, Sims = 500)
  Results.Ind.Sim500.M100.c<-list(fs = temp.results1, ls = temp.results2)

temp.data<-Ind.Sim500.M250.c
  temp.input <- RunSummary2(Sim.summary = temp.data$Sim.summary, Sim.results = temp.data$Sim.results, Sims = 500)
  temp.results1<-RunSummary3(Simulation.data=temp.input, pattern = "Independence", Scale = "fs", closed = TRUE, M = 250, Sims = 500)
  temp.results2<-RunSummary3(Simulation.data=temp.input, pattern = "Independence", Scale = "ls", closed = TRUE, M = 250, Sims = 500)
  Results.Ind.Sim500.M250.c<-list(fs = temp.results1, ls = temp.results2)

temp.data<-Avd.Sim500.M100.c
  temp.input <- RunSummary2(Sim.summary = temp.data$Sim.summary, Sim.results = temp.data$Sim.results, Sims = 500)
  temp.results1<-RunSummary3(Simulation.data=temp.input, pattern = "Avoidance", Scale = "fs", closed = TRUE, M = 100, Sims = 500)
  temp.results2<-RunSummary3(Simulation.data=temp.input, pattern = "Avoidance", Scale = "ls", closed = TRUE, M = 100, Sims = 500)
  Results.Avd.Sim500.M100.c<-list(fs = temp.results1, ls = temp.results2)

temp.data<-Avd.Sim500.M250.c
  temp.input <- RunSummary2(Sim.summary = temp.data$Sim.summary, Sim.results = temp.data$Sim.results, Sims = 500)
  temp.results1<-RunSummary3(Simulation.data=temp.input, pattern = "Avoidance", Scale = "fs", closed = TRUE, M = 250, Sims = 500)
  temp.results2<-RunSummary3(Simulation.data=temp.input, pattern = "Avoidance", Scale = "ls", closed = TRUE, M = 250, Sims = 500)
  Results.Avd.Sim500.M250.c<-list(fs = temp.results1, ls = temp.results2)

temp.data<-Agg.Sim500.M100.c
  temp.input <- RunSummary2(Sim.summary = temp.data$Sim.summary, Sim.results = temp.data$Sim.results, Sims = 500)
  temp.results1<-RunSummary3(Simulation.data=temp.input, pattern = "Aggregation", Scale = "fs", closed = TRUE, M = 100, Sims = 500)
  temp.results2<-RunSummary3(Simulation.data=temp.input, pattern = "Aggregation", Scale = "ls", closed = TRUE, M = 100, Sims = 500)
  Results.Agg.Sim500.M100.c<-list(fs = temp.results1, ls = temp.results2)

temp.data<-Agg.Sim500.M250.c
  temp.input <- RunSummary2(Sim.summary = temp.data$Sim.summary, Sim.results = temp.data$Sim.results, Sims = 500)
  temp.results1<-RunSummary3(Simulation.data=temp.input, pattern = "Aggregation", Scale = "fs", closed = TRUE, M = 250, Sims = 500)
  temp.results2<-RunSummary3(Simulation.data=temp.input, pattern = "Aggregation", Scale = "ls", closed = TRUE, M = 250, Sims = 500)
  Results.Agg.Sim500.M250.c<-list(fs = temp.results1, ls = temp.results2)

Sim.Performance.c<-list(Ind.Sim500.M100.c=Results.Ind.Sim500.M100.c,
                      Ind.Sim500.M250.c=Results.Ind.Sim500.M250.c,
                      Avd.Sim500.M100.c=Results.Avd.Sim500.M100.c,
                      Avd.Sim500.M250.c=Results.Avd.Sim500.M250.c,
                      Agg.Sim500.M100.c=Results.Agg.Sim500.M100.c,
                      Agg.Sim500.M250.c=Results.Agg.Sim500.M250.c)

rm(Results.Ind.Sim500.M100.c, Results.Ind.Sim500.M250.c, Results.Avd.Sim500.M100.c, Results.Avd.Sim500.M250.c, Results.Agg.Sim500.M100.c, Results.Agg.Sim500.M250.c)

#Compile results for NO CLOSURE
temp.data<-Ind.Sim500.M100.nc
  temp.input <- RunSummary2(Sim.summary = temp.data$Sim.summary, Sim.results = temp.data$Sim.results, Sims = 500)
  temp.results1<-RunSummary3(Simulation.data=temp.input, pattern = "Independence", Scale = "fs", closed = FALSE, M = 100, Sims = 500)
  temp.results2<-RunSummary3(Simulation.data=temp.input, pattern = "Independence", Scale = "ls", closed = FALSE, M = 100, Sims = 500)
  Results.Ind.Sim500.M100.nc<-list(fs = temp.results1, ls = temp.results2)

temp.data<-Ind.Sim500.M250.nc
  temp.input <- RunSummary2(Sim.summary = temp.data$Sim.summary, Sim.results = temp.data$Sim.results, Sims = 500)
  temp.results1<-RunSummary3(Simulation.data=temp.input, pattern = "Independence", Scale = "fs", closed = FALSE, M = 250, Sims = 500)
  temp.results2<-RunSummary3(Simulation.data=temp.input, pattern = "Independence", Scale = "ls", closed = FALSE, M = 250, Sims = 500)
  Results.Ind.Sim500.M250.nc<-list(fs = temp.results1, ls = temp.results2)

temp.data<-Avd.Sim500.M100.nc
  temp.input <- RunSummary2(Sim.summary = temp.data$Sim.summary, Sim.results = temp.data$Sim.results, Sims = 500)
  temp.results1<-RunSummary3(Simulation.data=temp.input, pattern = "Avoidance", Scale = "fs", closed = FALSE, M = 100, Sims = 500)
  temp.results2<-RunSummary3(Simulation.data=temp.input, pattern = "Avoidance", Scale = "ls", closed = FALSE, M = 100, Sims = 500)
  Results.Avd.Sim500.M100.nc<-list(fs = temp.results1, ls = temp.results2)

temp.data<-Avd.Sim500.M250.nc
  temp.input <- RunSummary2(Sim.summary = temp.data$Sim.summary, Sim.results = temp.data$Sim.results, Sims = 500)
  temp.results1<-RunSummary3(Simulation.data=temp.input, pattern = "Avoidance", Scale = "fs", closed = FALSE, M = 250, Sims = 500)
  temp.results2<-RunSummary3(Simulation.data=temp.input, pattern = "Avoidance", Scale = "ls", closed = FALSE, M = 250, Sims = 500)
  Results.Avd.Sim500.M250.nc<-list(fs = temp.results1, ls = temp.results2)

temp.data<-Agg.Sim500.M100.nc
  temp.input <- RunSummary2(Sim.summary = temp.data$Sim.summary, Sim.results = temp.data$Sim.results, Sims = 500)
  temp.results1<-RunSummary3(Simulation.data=temp.input, pattern = "Aggregation", Scale = "fs", closed = FALSE, M = 100, Sims = 500)
  temp.results2<-RunSummary3(Simulation.data=temp.input, pattern = "Aggregation", Scale = "ls", closed = FALSE, M = 100, Sims = 500)
  Results.Agg.Sim500.M100.nc<-list(fs = temp.results1, ls = temp.results2)

temp.data<-Agg.Sim500.M250.nc
  temp.input <- RunSummary2(Sim.summary = temp.data$Sim.summary, Sim.results = temp.data$Sim.results, Sims = 500)
  temp.results1<-RunSummary3(Simulation.data=temp.input, pattern = "Aggregation", Scale = "fs", closed = FALSE, M = 250, Sims = 500)
  temp.results2<-RunSummary3(Simulation.data=temp.input, pattern = "Aggregation", Scale = "ls", closed = FALSE, M = 250, Sims = 500)
  Results.Agg.Sim500.M250.nc<-list(fs = temp.results1, ls = temp.results2)

Sim.Performance.nc<-list(Ind.Sim500.M100.nc=Results.Ind.Sim500.M100.nc,
                        Ind.Sim500.M250.nc=Results.Ind.Sim500.M250.nc,
                        Avd.Sim500.M100.nc=Results.Avd.Sim500.M100.nc,
                        Avd.Sim500.M250.nc=Results.Avd.Sim500.M250.nc,
                        Agg.Sim500.M100.nc=Results.Agg.Sim500.M100.nc,
                        Agg.Sim500.M250.nc=Results.Agg.Sim500.M250.nc)