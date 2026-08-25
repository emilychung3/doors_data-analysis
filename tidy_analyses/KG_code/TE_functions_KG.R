# K. Garner, 2025, a set of functions to help with the analyses
####################################################################
### functions to compute routine scores
####################################################################
H <- function(x){
  # to be applied over the rows of the matrix of transitions
  -sum(x * log(x), na.rm = TRUE) # E.C. log converts raw probabilities into a scale where high score = more surprise
}

data_2_counts_matrix <- function(data, n_doors){
  # turn data into a counts matrix of transitions made
  mat <- matrix(rep(0, times=n_doors*n_doors), nrow=n_doors, ncol=n_doors)
  idxs <- matrix(c(data[1:length(data)-1], data[2:length(data)]), nrow=2, byrow=TRUE)
  for(i in 1:ncol(idxs)){
    mat[idxs[1,i],idxs[2,i]] <- mat[idxs[1,i],idxs[2,i]] + 1
  }
  mat
}

p_st1_gs <- function(counts_matrix, n_doors){
  # convert the counts matrix into the row probabilities
  denom <- matrix(rep(rowSums(counts_matrix), n_doors), nrow=n_doors, byrow=FALSE)
  out <- counts_matrix / denom
  out[is.na(out)] = 0
  out
}

get_wMTE <- function(counts_matrix, p_mat){
  # get the weighted mean transition entropy from a probability matrix and counts matrix
  lps <- log(p_mat) # E.C. get the log probabilities of each transition
  occupancy_wgt <- counts_matrix/sum(counts_matrix) # E.C. this gets the probability of each transition as a proportion of all transitions
  wMTE = -sum(occupancy_wgt * lps, na.rm = TRUE) # E.C. apply the weights to the log probability of each transition instead of p
  wMTE # E.C and calculated entropy
}

####################################################################
### functions to generate null distributions and random agents
####################################################################
sample_non_consec <- function(observed_data){
  # generate a null set of responses for an individual base on 
  # their observed responses
  n = length(observed_data) # this is the length of the sequence we will generate
  out = sample(observed_data, size=n, replace=FALSE)
  out
}

generate_null_for_one_person <- function(observed_data, n_samples = 1000, n_doors = 16){
  
  null_seqs <- replicate(n_samples, sample_non_consec(observed_data), simplify=FALSE)
  null_counts <- lapply(null_seqs, data_2_counts_matrix, n_doors = n_doors)
  null_ps <- lapply(null_counts, p_st1_gs, n_doors = n_doors)
  null_Rs <- apply(do.call(rbind, lapply(null_ps, function(x) apply(x, 1, H))), 1, sum)
  null_Rs
}

random_agent_responses <- function(doors=c(1,2,3,4)){
  # generate a task environment and have an agent complete it randomly
  ntrls <- 160
  trls <- sample(doors, size=ntrls, replace=TRUE)
  
  # now assuming a perfectly random agent that knows the task perfectly,
  # generate responses to complete the task
  rsps <- c()
  for (i in trls){
    
      rsps_i <- sample(doors, 1) # pick first response
      tgt_i <- i # get the target for this trial
      tgt_fnd = 0
      
      while(!tgt_fnd){
      
        if (tail(rsps_i, 1) == tgt_i){
          tgt_fnd = 1 
        }
      # get the remaining doors and pick the next response
        if(!tgt_fnd){
          rsps_i <- c(rsps_i, sample(doors, 1))
        }
      }
      # print(rsps_i)
      # add this trial to the responses
      rsps <- c(rsps, rsps_i)
  }
  rsps
}

null_z_for_random_agent <- function(n_doors = 16){
  
  # generate the random agents responses
  resps <- random_agent_responses()
  
  # take the random agents null data and generate their 
  # r score, as well as a null distribution
  # get their transition counts, probability matrix, and ent
  counts <- data_2_counts_matrix(resps, n_doors = n_doors)
  probs <- p_st1_gs(counts, n_doors = n_doors)
  ent <- sum(apply(probs, 1, H))
  
  # get their null distribution
  tmp_null <- generate_null_for_one_person(resps)
  
  # compute their z score and output
  z <- (ent-mean(tmp_null))/sd(tmp_null)
  z
}

####################################################################
### various indexing functions to help analysis
####################################################################
get_context_swch_idx_per_prsn <- function(dat, subN){
  # ugly function to do the sorting/splitting of data as follows:
  ## now how to split the data up? First, by context.
  ## Second, we don't want to artificially inflate the routine from
  ## each context with the times that the context has switched but 
  ## the participant still thinks they are in the other context.
  ## So, I will take the data:
  ## when there is a context switch
  ##     -- find next trial where cc is a 1. 
  ##     -- trials between switch and 1 go to previous context
  ##     -- trials cc to end go to current context  
  print(paste("sub =", subN))
  tmp <- dat %>% filter(sub == subN)
  
  # first find context switches
  # select current context is the variable that breaks the data how I want
  swch_idx <- which(diff(tmp$context_assign_ent) != 0) + 1 # get points at which 
  # there is a context switch
  # now add the last trial
  swch_idx <- c(swch_idx, nrow(tmp))
  # now for each switch index row, find the next row that has a 1 in the 
  # door_cc column
  # first make the swch_idx for the starts and ends
  nswch = length(swch_idx)
  swch_strs <- swch_idx[1:nswch-1]
  swch_ends <- swch_idx[2:nswch]
  
  # get indexes for where the new context number should be replaced with the old one
  fll_idxs <- rep(0, length(swch_strs))
  for (i in 1:length(swch_strs)){
    fll_idxs[i] <- swch_strs[i] + which(diff(tmp[swch_strs[i]:(swch_ends[i]-1), "door_cc"]) > 0 )[1] - 1
  }       
  rms <- which(is.na(fll_idxs))  
  if (any(rms)){
    swch_strs <- swch_strs[-rms]
    fll_idxs <- fll_idxs[-rms]
  }
  for (i in 1:length(swch_strs)){
    tmp$context_assign_ent[swch_strs[i]:fll_idxs[i]] <- tmp$context_assign_ent[swch_strs[i]-1]
  }
  
  tmp
}

counts_since_last_switch <- function(dat, subN, cntxtN){
  # count up how many selections since the last switch
  tmp <- dat %>% filter(sub == subN & context_assign_ent == cntxtN)
  cnt_idx = which(!!tmp$switch_assign)
  max_vals = diff(cnt_idx) # what to count up to each time
  max_vals = c(max_vals, nrow(tmp) - tail(cnt_idx, 1) + 1)
  mk_cnt_vector <- function(x){
    1:x
  }
  out <- unlist(lapply(max_vals, mk_cnt_vector))
  tmp$selections_since_last_context_switch = out
  tmp
}

####################################################################
### functions to generate regressors for the impacts modelling
####################################################################
get_Sw <- function(dat, subN){
  # this regrssor computes the number of times a target has been found in a context that is different to where the preceeding context was found, and creates a regressor as long as the responses that reflects this
  # the logical conditions for this are:
  # 1. is this a switch trial? yes/no - is 'switch' a 1?
  # 2. when is the end of the switch trial (this means the target has been found)
  #     for 2: I can get the trial numbers of each switch trial, and then find the 
  #           last entry in the data frame that corresponds to that trial number. 
  # 3. from sw_end + 1, the count goes up
  #     The row after the last entry above is sw+1
  
  tmp <- dat %>% filter(sub == subN)
  sw <- rep(0, nrow(tmp))
  sw_trls <- unique(tmp$t[as.logical(tmp$switch)]) # this gives me the trial numbers where a switch occurred
  sw_tgts_idx <- unlist(lapply(sw_trls, function(x) tail(which(tmp$t == x), 1))) 
  cumsum = 0:length(sw_tgts_idx)
  # add final row of dataframe to idx
  sw_tgts_idx <- c(sw_tgts_idx, nrow(tmp))
  rep_idx <- c(sw_tgts_idx[1], diff(sw_tgts_idx)) # now get number of times each switch should be repeated, before the next switch
  sw <- rep(cumsum, times = rep_idx)
  tmp$Sw <- sw
  tmp <- tmp %>% mutate(Swr = Sw / t) # get switch rate while we are here
  tmp
}

get_p_context <- function(dat, subN){
  # for each trial, get the probability of being in the same context as the last rewarded trial
  
  tmp <- dat %>% filter(sub == subN)
  ##############################################
  # apply an update for each trial that reflects the probability of being in
  # the last context in which you were rewarded
  rw_idx <- c(with(tmp, which(diff(t) != 0)), nrow(tmp)) # get the row numbers where each trial ends, this will allow me to pull out the context for each trial
  cntx <- with(tmp, context[rw_idx]) # get the pattern of context changes
  # now I need to count, for each trial, what is the probability that its the same as the last trial
  cntx_chngs <- diff(cntx) # get whether you were in the same or different context on the previous trial (this gives 2:end
  cntx_chngs <- !cntx_chngs # make the stays a true, and the changes a false
  psC <- c(.5, .5) # the probability you are in the same context as the last one you saw (pdC will be 1 - this number). p=.5 reflects uninformed priors, 2 values as the first update comes at the end of trial 2.
  for (i in 1:(length(cntx_chngs)-1)){ # we only go to the penultimate observation as there are no updates after the last trial 
    psC <- c(psC, sum(cntx_chngs[1:i])/i) # at the end of each trial, I evaluate the probability of a context being the same as the one I were last rewarded in. This will get fed into the next trial as the estimate of the probability of being in the same context as the last rewarded one.
  }
  ts <- with(tmp, unique(t))
  tmp <- inner_join(tmp, tibble(t = ts, psC = psC, pdC = 1 - psC), by="t") 
  
  ####################################
  
  ######################################
  # now I need for each trial, the number of n's (fails from the same context as the previous), and the number of m's (fails from the different context as the previous). A 'fail' is a selection of a task relevant door, and the failure to find a target
  
  # first, get the previously rewarded context
  tmp <- inner_join(tmp, tibble(t = unique(ts), 
                                prv_cntx = c(0, cntx[1:(length(ts)-1)])),
                    by="t")
  
  # we need to assign an appropriate 'previous context' to trial 1, so lets take the context
  # from which they first select a target door
  cntx_a_tgts <- unique(tmp$door[tmp$context == 1 & tmp$door_cc > 0]) 
  cntx_b_tgts <- unique(tmp$door[tmp$context == 2 & tmp$door_cc > 0])
  tgts <- matrix(c(cntx_a_tgts, cntx_b_tgts), ncol=2)
  frst_tgt_door <- tmp$door[which(tmp$door_cc == 1 | tmp$door_oc == 1)[1]]
  prv_cntxt_4_frst_trl <- which(tgts == frst_tgt_door, arr.ind=T)[,'col']
  tmp$prv_cntx[tmp$t == 1] = prv_cntxt_4_frst_trl
  
  ## now I need to recode the data in terms of which context
  ## the participant has evidence in for being, rather than the 
  ## actual physical context they are in (ground truth). This is because,
  ## if I just found a target from context 1, then I have no evidence I
  ## am now in context 2, until I find a target in context 2.
  ## so I need to code cc and oc responses as following:
  ## if current context == 1 & prev context == 1, n = cc, m = oc # current context is indeed current context
  ## if current context == 1 & prev context == 2, n = oc, m = cc # then until you find the target from context 1, you should think that 2 is cc
  ## if current context == 2 & prev context == 2, n = cc, m = oc # and so on
  ## if current context == 2 & prev context == 1, n = oc, m = cc
  tmp$door_n <- 0
  tmp$door_m <- 0 ##### NOTE: door_m is also our DV aka we are modelling responses that
  # are from the context in which you were not just rewarded, to see how much the probability
  # of that context influences your behavioural set
  tmp$door_n[tmp$context == tmp$prv_cntx] <- 
    tmp$door_cc[tmp$context == tmp$prv_cntx]
  # row 1 & 3, [n] from comments above
  tmp$door_n[tmp$context != tmp$prv_cntx] <- 
    tmp$door_oc[tmp$context != tmp$prv_cntx] # row 1 & 3 [m] from comments above. 
  tmp$door_m[tmp$context == tmp$prv_cntx] <- # 
    tmp$door_oc[tmp$context == tmp$prv_cntx] # row 1 & 3 [m]
  tmp$door_m[tmp$context != tmp$prv_cntx ] <- 
    tmp$door_cc[tmp$context != tmp$prv_cntx] # row 2 & 4 [m]
  
  ## now I need to add to this bit a filtering of the unique target selections on that trial, 
  ## and remove the n's and m's which are repetitions, this is because you only get information
  ## the first time you select an n or an m
  remove_repeats <- function(tmp, trialN, tgts){
    ###########
    # here I write a function where for each trial I take the door_n selections (i.e. the relevant ones, relative to last reward). I find which of them are repetitions, and I remove those repetitions
    ##########
    trial_dat <- tmp %>% filter(t == trialN)
    
    ##########
    # first deal with the ns
    tgts_tn <- tgts[,trial_dat$prv_cntx[1]] # what were the n targets on this trial
    tidx <- which(trial_dat$door_n > 0) # on which rows did someone select an n door?
    scnd_ns_this_trl <- duplicated(trial_dat$door[tidx]) # find which selections are  repeat visits
    # get the idx for which ones should be removed
    trial_dat$door_frst_n <- trial_dat$door_n # establish the required variable
    trial_dat$door_frst_n[tidx[scnd_ns_this_trl]] <- 0
    
    ##########
    # now deal with the ms - code is a little clunky. I may generalise this later given time, space and being a better person
    tgts_tm <- tgts[,3-trial_dat$prv_cntx[1]]
    midx <- which(trial_dat$door_m > 0)
    scnd_ms_this_trl <- duplicated(trial_dat$door[midx])
    trial_dat$door_frst_m <- trial_dat$door_m # establish the required variable
    trial_dat$door_frst_m[midx[scnd_ms_this_trl]] <- 0
    
    ########## 
    # now what we actually need to do is shift the first_n variables down 1, and put 0 in the first
    # selection of the trial. This is because we need the regressor to reflect the update that 
    # influences performance on that trial - e.g. trial 2 information affects trial 3, not trial 2
    nr = nrow(trial_dat)
    if (nr > 1){
      trial_dat$door_frst_n <- c(0, trial_dat$door_frst_n[1:(nr-1)]) # because we won't update after 
      # we've found the target
      trial_dat$door_frst_m <- c(0, trial_dat$door_frst_m[1:(nr-1)])
    } else {
      trial_dat$door_frst_n <- 0
      trial_dat$door_frst_m <- 0
    }
    
    ###########
    # return the new dataframe
    trial_dat
  }
  
  trls <- unique(tmp$t)
  tmp <- do.call(rbind, lapply(trls, remove_repeats, tmp=tmp, tgts=tgts))
  
  ################################################################
  
  #############################################################
  # next, remove the last door selection from each trial, as that was a hit (not a null)
  tmp <- tmp[-rw_idx,]
  
  ## now by trial, compute the cumulative sum of door_n and door_m
  tmp <- tmp %>% group_by(t) %>% mutate(sum_n = cumsum(door_frst_n),
                                        sum_m = cumsum(door_frst_m)) 
  #############################################################
  
  #############################################################
  # next, I need a column giving the probability of n or m nulls, given you are
  # in the previously rewarded context (for n), or the other context (for m)
  p_vals <- c(1, .75, .5, .25, 0)
  tmp$p_n_g_sC <- p_vals[as.factor(tmp$sum_n)] # probability of n, given you are in the same context as where you were last rewarded
  tmp$p_m_g_dC <- p_vals[as.factor(tmp$sum_m)] # probability of m, given you are in the different context from where you were last rewarded
  tmp$p_n_g_sC[is.na(tmp$p_n_g_sC)] <- 0
  tmp$p_m_g_dC[is.na(tmp$p_m_g_dC)] <- 0
  #############################################################
  
  #############################################################
  ##### so what I have now on each row is the probability you are in the same context, given your 
  ##### previous experience. I also have the number of n or m you have selected, up to the previous 
  ##### trial
  ##### so the info on each row reflects the information available to influence behaviour on that
  ##### trial (i.e. the info from 1:t-1)
  ##### so then I can compute on each trial, the probability of being in the same context given that
  ##### many ns and ms that have been discovered up until now
  ##### I can then subtract from 1, to get the probability that
  ##### you are in the other context, and then divide the two 
  ##### to get the odds ratio.
  tmp <- tmp %>% mutate(p_Scgn = (p_n_g_sC*psC /  (p_n_g_sC*psC + p_m_g_dC*pdC)),
                        p_Ocgn = 1 - p_Scgn)
  
  #### now compute the odds of being in the same context
  tmp$cntxt_odds <- tmp$p_Scgn / tmp$p_Ocgn
  #############################################################
  ##### now I want to work out the probability of success, given the remaining n and m, and then take the division of the 2 to get that odds ratio
  tmp <- tmp %>% mutate(p_succss_sC = p_Scgn / (4-sum_n),
                        p_succss_oC = p_Ocgn / (4-sum_m),
                        succss_odds = p_succss_sC/p_succss_oC)
  # remove any infs and NAs
  tmp <- tmp %>% filter(!is.infinite(cntxt_odds)) %>%
    filter(!is.nan(cntxt_odds)) %>%
    filter(!is.infinite(succss_odds)) %>%
    filter(!is.nan(succss_odds))
  tmp
}
