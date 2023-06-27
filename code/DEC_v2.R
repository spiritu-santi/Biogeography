library(here)
library(coda)
library(tidyverse)
require(colorspace)
require(ggplot2)
library(furrr)
library(purrr)
library(ape)
library(rgdal)
library(gginnards)
library(plotrix)
library(phytools)
library(RevGadgets)
library(ape)
library(treeio)
library(viridis)
library(patchwork)
library(ggplot2)
library(ggtree)
library(ape)
library(RevGadgets)
library(data.table)
######## UTILITIES ######
add_epoch_times <- function( p, dy_bars, dy_text ) {
  
  max_x = max(p$data$x)
  max_y = max(p$data$y)
  epoch_names = c("T","J","K","Pg","Ng","")
  x_pos = max_x - c(max_x,201.3, 145, 66, 23, 2.588, 0)
  y_pos = rep(max_y, length(x_pos))
  x_pos_mid = ( x_pos[1:(length(x_pos)-1)] + x_pos[2:length(x_pos)] ) / 2 
  box_col = c(rgb(249, 249, 127,150,maxColorValue = 255),
              rgb(255, 230, 25,150, maxColorValue = 255),
              rgb(253, 154, 82, 150,maxColorValue = 255),
              rgb(127, 198, 78, 150,maxColorValue = 255),
              rgb(52, 178, 201,150, maxColorValue = 255),
              rgb(129, 43, 146,150, maxColorValue = 255),
              rgb(240, 64, 40, 150,maxColorValue = 255),stringsAsFactors = FALSE)
  
  for (k in 2:(length(x_pos))) {
    # box_col = "gray92"
    #  if (k %% 2 == 0) box_col = "white"
    box = geom_rect( xmin=x_pos[k-1], xmax=x_pos[k], ymin=dy_bars, ymax=dy_bars+8, fill= box_col[k] )
    p = append_layers(p, box, position = "bottom")
  }
  for (k in 1:length(epoch_names)) {
    p = p + annotate( geom="text", label=epoch_names[k], x=x_pos_mid[k], y=dy_text, hjust=0.5, size=3.25)
  }
  return(p)
  
}
# modified from inset
inset.revgadgets = function (tree_view, insets, width = 0.1, height = 0.1, hjust = 0,vjust = 0, x = "node", pos = 0.5) {
  df <- tree_view$data[as.numeric(names(insets)), ]
  
  # position subviews based on tree part
  x <- match.arg(x, c("node", "branch", "edge", "parent_shoulder"))
  if (x == "node") {
    xx <- df$x
  }
  else if (x == "parent_shoulder") {
    xx <- df$x[ match(df$parent, df$node) ]
  }
  else {
    xx <- df$branch
  }
  yy <- df$y
  xx <- xx - hjust
  yy <- yy - vjust
  
  if (length(width)==1) width = rep(width, length(insets))
  if (length(height)==1) height = rep(height, length(insets))
  
  # add subviews
  tree_view = tree_view + ggimage:::geom_subview(subview = insets, width = width, 
                                                 height = height, x = xx, y = yy)
  
  # return treeview with subviews
  return(tree_view)
}

# modified from https://github.com/GuangchuangYu/ggtree/blob/master/R/tree-utilities.R
getXcoord2 <- function(x, root, parent, child, len, start=0, rev=FALSE) {
  x[root] <- start
  x[-root] <- NA  ## only root is set to start, by default 0
  
  currentNode <- root
  direction <- 1
  if (rev == TRUE) {
    direction <- -1
  }
  while(anyNA(x)) {
    idx <- which(parent %in% currentNode)
    newNode <- child[idx]
    x[newNode] <- x[parent[idx]]+len[idx] * direction
    currentNode <- newNode
  }
  
  return(x)
}
# modified from https://github.com/GuangchuangYu/ggtree/blob/master/R/tree-utilities.R
getXcoord <- function(tr) {
  edge <- tr$edge
  parent <- edge[,1]
  child <- edge[,2]
  root <- ggtree:::getRoot(tr)
  
  len <- tr$edge.length
  
  N <- ggtree:::getNodeNum(tr)
  x <- numeric(N)
  x <- getXcoord2(x, root, parent, child, len)
  return(x)
}
# modified from https://github.com/GuangchuangYu/ggtree/blob/master/R/tree-utilities.R
getYcoord <- function(tr, step=1) {
  Ntip <- length(tr[["tip.label"]])
  N <- ggtree:::getNodeNum(tr)
  
  edge <- tr[["edge"]]
  parent <- edge[,1]
  child <- edge[,2]
  
  cl <- split(child, parent)
  child_list <- list()
  child_list[as.numeric(names(cl))] <- cl
  
  y <- numeric(N)
  tip.idx <- child[child <= Ntip]
  y[tip.idx] <- 1:Ntip * step
  y[-tip.idx] <- NA
  
  currentNode <- 1:Ntip
  while(anyNA(y)) {
    pNode <- unique(parent[child %in% currentNode])
    ## piping of magrittr is slower than nested function call.
    ## pipeR is fastest, may consider to use pipeR
    ##
    ## child %in% currentNode %>% which %>% parent[.] %>% unique
    ## idx <- sapply(pNode, function(i) all(child[parent == i] %in% currentNode))
    idx <- sapply(pNode, function(i) all(child_list[[i]] %in% currentNode))
    newNode <- pNode[idx]
    
    y[newNode] <- sapply(newNode, function(i) {
      mean(y[child_list[[i]]], na.rm=TRUE)
      ##child[parent == i] %>% y[.] %>% mean(na.rm=TRUE)
    })
    
    currentNode <- c(currentNode[!currentNode %in% unlist(child_list[newNode])], newNode)
    ## currentNode <- c(currentNode[!currentNode %in% child[parent %in% newNode]], newNode)
    ## parent %in% newNode %>% child[.] %>%
    ##     `%in%`(currentNode, .) %>% `!` %>%
    ##         currentNode[.] %>% c(., newNode)
  }
  
  return(y)
}
# modified from https://github.com/GuangchuangYu/ggtree/blob/master/R/tree-utilities.R
getParent <- function(tr, node) {
  if ( node == ggtree:::getRoot(tr) )
    return(0)
  edge <- tr[["edge"]]
  parent <- edge[,1]
  child <- edge[,2]
  res <- parent[child == node]
  if (length(res) == 0) {
    stop("cannot found parent node...")
  }
  if (length(res) > 1) {
    stop("multiple parent found...")
  }
  return(res)
}
# set custom state labels
assign_state_labels = function(t, state_labels, include_start_states, n_states=3) {
  
  # exit if no state labels provided
  if (is.null(state_labels)) {
    return(t)
  }
  
  # what is the ancestral state name tag?
  if (include_start_states) {
    state_pos_str_base = c("start_state_", "end_state_")
  } else {
    state_pos_str_base = c("anc_state_")
  }
  
  # create list of ancestral state name tags
  state_pos_str_to_update = c(sapply(1:n_states, function(x) { paste(state_pos_str_base,x,sep="")}))
  
  # overwrite state labels
  for (m in state_pos_str_to_update)
  {
    # get the states
    x_state = attributes(t)$data[[m]]
    x_state_valid = which( x_state != "NA" )
    x_state_invalid = which( x_state == "NA" )
    x_state_tmp = unlist(sapply(x_state, function(z) { state_labels[ names(state_labels)==z ] }))
    x_state[x_state_valid] = x_state_tmp
    x_state[x_state_invalid] = NA
    attributes(t)$data[[m]] = x_state
  }
  
  return(t)
}
# set prob factors
set_pp_factor_range = function(t, include_start_states, n_states=1) {
  
  # what is the ancestral state name tag?
  if (include_start_states) {
    state_pos_str_base = c("start_state_", "end_state_")
  } else {
    state_pos_str_base = c("anc_state_")
  }
  
  # create list of ancestral state name tags
  state_pos_str_to_update = c(sapply(1:n_states, function(x) { paste(state_pos_str_base,x,"_pp",sep="")}))
  
  # overwrite state labels
  for (m in state_pos_str_to_update)
  {
    x_state = attributes(t)$data[[m]]
    #levels(x_state) = c(levels(x_state))
    attributes(t)$data[[m]] = x_state
  }
  return(t)
}
get_bg_state_2 = function(s) {
  if (s==1)       return(c(1))
  else if (s==2)  return(c(2))
  else if (s==3)  return(c(3))
  else if (s==4)  return(c(4))
  else if (s==5)  return(c(5))
  else if (s==6)  return(c(6))
  else if (s==7)  return(c(7))
  else if (s==8)  return(c(8))

  else if (s==9)  return(c(1,2))
  else if (s==10) return(c(1,3))
  else if (s==11) return(c(2,3))
  else if (s==12) return(c(1,4))
  else if (s==13) return(c(2,4))
  else if (s==14) return(c(3,4))
  else if (s==15) return(c(1,5))
  else if (s==16)  return(c(2,5))
  else if (s==17)  return(c(3,5))
  else if (s==18)  return(c(4,5))
  
  else if (s==19)  return(c(1,6))
  else if (s==20) return(c(2,6))
  else if (s==21) return(c(3,6))
  else if (s==22) return(c(4,6))
  else if (s==23) return(c(5,6))
  
  else if (s==24) return(c(1,7))
  else if (s==25) return(c(2,7))
  else if (s==26)  return(c(3,7))
  else if (s==27)  return(c(4,7))
  else if (s==28)  return(c(5,7))
  else if (s==29)  return(c(6,7))
  
  else if (s==30) return(c(1,8))
  else if (s==31) return(c(2,8))
  else if (s==32) return(c(3,8))
  else if (s==33) return(c(4,8))
  else if (s==34) return(c(5,8))
  else if (s==35) return(c(6,8))
  else if (s==36)  return(c(7,8))

}
# Still being developed, but this will create a probability matrix
# for all internal nodes and all sampled states. The matrix will
# be appropriate for use with the pie/bar inset function in ggtree.
build_state_probs = function(t, state_labels, include_start_states, p_threshold = 0.01) {
  
  n_states = length(state_labels)
  n_tips = length(attributes(t)$phylo$tip.label)
  n_node = 2 * n_tips - 1
  
  dat = list()
  
  if (include_start_states) {
    state_tags = c("start","end")
  } else {
    state_tags = c("anc")
  }
  
  for (s in state_tags) {
    dat[[s]] = data.frame( matrix(0, nrow=n_node, ncol=n_states) )
    #dat[[s]] = cbind(node=1:n_node, dat[[s]])
    
    for (i in 1:3)
    {
      m = paste(s,"_state_",i,sep="")
      pp_str = paste(m,"_pp",sep="")
      n_tmp = as.numeric(as.vector(attributes(t)$data$node)) # node index
      x_tmp = as.vector(attributes(t)$data[[m]])
      pp_tmp = as.numeric(as.vector(attributes(t)$data[[pp_str]]))
      
      for (j in 1:length(x_tmp))
      {
        if (!is.na(x_tmp[j])) {
          
          if (pp_tmp[j] > p_threshold) {
            k = which(x_tmp[j]==state_labels)
            dat[[s]][n_tmp[j], k] = pp_tmp[j]
          }
        }
      }
    }
    
    # format column names
    colnames(dat[[s]])=as.vector(unlist(state_labels))
    
    # add probs for >3rd state under ... label
    rem_prob = c()
    for (i in 1:nrow(dat[[s]])) {
      rem_prob[i] = 1
      for (j in 1:length(dat[[s]][i,])) {
        rem_prob[i] = rem_prob[i] - dat[[s]][i,j]
      }
    }
    dat[[s]]$`...` = rem_prob
    dat[[s]]$node = 1:n_node
    #print(dat[[s]][250:260,])
  }
  
  return(dat)
}
collect_probable_states = function(p, p_threshold=0.01){
  labels = c("end_state", "start_state")
  index = c(1,2,3)
  
  codes = c()
  labels_pp = c()
  for (l in labels) {
    for (i in index) {
      label_index = paste(l,"_",i,sep="")
      label_index_pp = paste(l,"_",i,"_pp",sep="")
      index_threshold = p$data[[ label_index_pp ]] > p_threshold
      codes = c(codes, unique( p$data[[label_index]][ index_threshold ] ))
    }
  }
  codes = unique(codes)
  codes = c(codes, "...")
  return(codes)
}

do_ARR_plot <- function(ase.tre = ase.tre, states_file = states_file,tree_layout = "rectangular",tip_pie_diameter=20,node_pie_diameter=20,title=title,output=output,save,burn.in,drop.tips=TRUE){
  t <- read.beast(ase.tre)
  if(drop.tips) { 
    taxa <- read.table(here("rb_out/data/Master_taxa.tsv"),header=T) %>% filter(age!=0) %>% pull(taxon)
    t <- drop.tip(t,tip=taxa)
  }
  state <- read.table(states_file,sep="\t",header=T,stringsAsFactors = F);(state)
  state_labels <- state$state_label
  names(state_labels) = 1:nrow(state)
  state_colors <- state$state_colors.1
  names(state_colors) = 1:nrow(state)
  include_start_states = TRUE
  t = assign_state_labels(t, state_labels, include_start_states,3)
  t = set_pp_factor_range(t, include_start_states)
  state_colors = state_colors
  use_state_colors = !is.null(state_colors)
  if (!is.null(state_colors) && !is.null(state_labels)) {names(state_colors) = state_labels}
  tree = attributes(t)$phylo
  n_node = ggtree:::getNodeNum(tree)
  fams <- getMRCA(t@phylo,tip=c("Cyathea_myosuroides","Sphaeropteris_horrida"))
  fams <- c(fams, getMRCA(t@phylo,tip=c("Dicksonia_lanata","Lophosoria_quadripinnata")))
  fams <- c(fams, getMRCA(t@phylo,tip=c("Cibotium_chamissoi","Cibotium_barometz")))
  fams <- c(fams, getMRCA(t@phylo,tip=c("Metaxya_rostrata","Metaxya_scalaris")))
  fams <- c(fams, getMRCA(t@phylo,tip=c("Plagiogyria_japonica","Plagiogyria_pectinata")))
  fams <- c(fams, getMRCA(t@phylo,tip=c("Culcita_macrocarpa","Culcita_coniifolia")))
  fams <- c(fams, getMRCA(t@phylo,tip=c("Loxsoma_cunninghamii","Loxsomopsis_pearcei")))
  fams <- c(fams, getMRCA(t@phylo,tip=c("Cyathea_myosuroides","Cibotium_barometz")))
  fams <- c(fams, getMRCA(t@phylo,tip=c("Plagiogyria_japonica","Thyrsopteris_elegans")))
  fams <- c(fams, getMRCA(t@phylo,tip=c("Cyathea_myosuroides","Thyrsopteris_elegans")))
  
  t@data %>% slice(c(fams)) %>% 
    mutate(Clade = c(
    "Cyatheaceae","Dicksoniaceae","Cibotiaceae","Metaxyaceae","Plagiogyriaceae","Culcitaceae","Loxomataceae","CDC","PCLT","Cyatheales"
  ),.before=1
  ) %>% select(1:7) %>% pivot_longer(cols = ends_with("pp"),names_to = "var",values_to = "val") %>% 
    select(Clade,var,val) -> probs
  probs
  
  t@data %>% slice(c(fams)) %>% mutate(Clade = c(
    "Cyatheaceae","Dicksoniaceae","Cibotiaceae","Metaxyaceae","Plagiogyriaceae","Culcitaceae","Loxomataceae","CDC","PCLT","Cyatheales"
  ),.before=1
  ) %>% select(1:7) %>% pivot_longer(cols = c("end_state_1","end_state_2","end_state_3"),names_to = "var",values_to = "area") %>% select(Clade,var,area) -> areas
  
  probs %>% mutate(var=sub("_pp$","",var)) %>% left_join(.,areas,by=c("Clade","var")) %>% mutate(val=as.numeric(val)) %>% 
    ggplot(aes(y=Clade,x=val,fill=area,group=desc(var))) +
    geom_bar(position="dodge",stat="identity") +
    #scale_fill_manual(values=c("#A2D7DE","#F6A756","#D4E1CB","#2E5B87","#A2D7DE","#FFD47E","#FFD47E","#1E466E","#A2D7DE","#D4E1CB","#EC7B4B","#FFD47E","NA")) + 
      NULL
  
  
  
  p = ggtree::ggtree(t, layout=tree_layout, ladderize=TRUE)
# p = p + geom_tiplab(size=1, offset=0.03)
  p = p + geom_tippoint(aes(colour=factor(end_state_1)), size=0, alpha=1) 
  p = p + geom_nodepoint(aes(colour=factor(start_state_1), size=0),na.rm=TRUE, alpha=0)
  p = p + geom_nodepoint(aes(colour=factor(start_state_2), size=0),na.rm=TRUE, alpha=0)
  p = p + geom_nodepoint(aes(colour=factor(start_state_3), size=0),na.rm=TRUE, alpha=0)
  p = p + guides(colour="none",order=2,size="none")
  p = p + guides(colour = guide_legend(override.aes = list(size=5)))
  used_states = collect_probable_states(p)
  p = p + scale_color_manual(values=state_colors, labels=state_labels, name="Range", limits = used_states)
  p = p + theme(legend.position="left")
  dat_state = build_state_probs(t, state_labels, include_start_states)
  n_tips = length(tree$tip.label)
  length(which(tree$node.label!=""))
  n_nodes = 2 * n_tips - 1
  node_idx = (n_tips+1):n_nodes
  tip_idx = 1:n_tips
  all_idx = 1:n_nodes
  pies_end = nodepie(dat_state$end,cols=1:(ncol(dat_state$end)-2),color=state_colors,alpha=1)
  pies_start = nodepie(dat_state$start,cols=1:(ncol(dat_state$start)-2),color=state_colors,alpha=1)
  pd = c( rep(tip_pie_diameter, n_tips), rep(node_pie_diameter, n_nodes-n_tips) )
  cat("Plotting ancestral states: stage 1","\n")
  p_node =  inset.revgadgets(tree_view =  p,
                             insets=pies_end[all_idx],
                             x="node",
                             height=pd*2,
                             width=pd*2,
                             hjust=0,
                             vjust=0)
  cat("Plotting ancestral states: stage 2","\n")
  #p_shld = inset.revgadgets(tree_view=p_node,insets=pies_start,x="parent_shoulder",height=pd*1.4,width=pd*1.4,hjust=0,vjust=0)
  p_shld = p_node
  cat("Plotting ancestral states: stage 3","\n")
  p_shld = p_shld + ggtitle(title)
  p_shld <- add_epoch_times(p_shld, dy_bars=-10, dy_text=-10)
  if (save==TRUE) ggsave(p_shld,file=output,width=16,height=14,units="in")
  #p_all = p_shld + coord_cartesian(xlim = c(0,300), ylim=NULL, expand=TRUE)
  #p_all
  #p2 = p_shld + scale_color_manual(values=state_colors, breaks=as.vector(state_labels),limits = used_states[c(1:8,10,11,16,19,20)])
  #p2 = p2 + scale_radius(range = c(15,20))
  #p2 = p2 + theme(legend.position="left")
  # show title
  #p2 = p2 + ggtitle("Cyatheales")
  # set visible area
  #p2 = p2 + coord_cartesian(xlim = c(0,280), ylim = NULL, expand=TRUE)
  #p2 = add_epoch_times(p2, 280,  dy_bars=-10, dy_text=-10)
  #ggsave(p2,file="~/Desktop/ARR_full_mcc_2.pdf")
}

do_STT_plot <- function(stoch, area.codes,n.areas,max.time,burn.in,support_plot,save,output=output){
  bg_colors  = read.csv(area.codes,sep="\t",header=T)
  bg_names = bg_colors$state_label[1:n.areas]
  area_cols = as.vector( bg_colors$state_colors[1:n.areas] )
  names(area_cols) = 1:length(area_cols)
  bg_label_col = as.vector(bg_colors$state_colors[1:n.areas])
  # data dimensions
  n_areas   = length(bg_names)
  n_states  = n_areas
  bin_width = 2
  max_time = max.time
  n_bins    = max_time / bin_width
  ages      = seq(0.0, max_time, by=bin_width)
  # settings to control LSTT accuracy
  D_tol    = 0.05 # how close all sampled LSTT frequencies in a bin must be to the true LSTT frequencies
  alpha_tol = 0.05 # false-positive rate of failing to satisfy the D_tol level
  # apply burnin/thinning if desired
  f_burn    = burn.in
  thinby    = 1
  # read in stochastic mappings
  stoch_bg     = read.csv(stoch,sep="\t", stringsAsFactors=FALSE)
  #stoch_bg     = stoch_bg[ stoch_bg$transition_type != "cladogenetic", ]
  # filter out non-events
  stoch_bg$transition_time[ stoch_bg$transition_type=="no_change" ] = stoch_bg$branch_start_time[ stoch_bg$transition_type=="no_change" ]
  # iterations
  iterations = unique(stoch_bg$iteration)
  n_it       = length(iterations)
  n_burn     = floor(max(1, f_burn*length(iterations)))
  iterations = iterations[n_burn:length(iterations)]
  iterations = iterations[ seq(1, length(iterations), by=thinby) ]
  # Stage 1: construct  bg stochastic mappings
  branches = 1:max(unique(stoch_bg$parent_index), na.rm=TRUE)
  # loop over iterations
  stoch_list = list()
  for (i in 1:length(iterations)) {
    # get biome and biogeography stochastic mappings per iteration
    it = iterations[i]
    cat("Stage 1: bg stochastic mappings, processing iteration ",it," / ", max(iterations), "\r", sep="")
    sample_bg = stoch_bg[ stoch_bg$iteration==it, ]
    # sample_biome = stoch_biome[ stoch_biome$iteration==it, ]
    # loop over branches
    tmp_branch_list = list()
    for (j in 1:length(branches)) {
      # get biome and biogeography stochastic mappings per branch
      nd_idx = branches[j]
      branch_bg = sample_bg[ sample_bg$node_index==nd_idx, ]
      # branch_biome = sample_biome[ sample_biome$node_index==nd_idx, ]
      # interleave biome and biogeography stochastic mappings
      tmp_branch_list[[j]] = as.data.frame(branch_bg, stringsAsFactors=F )
    }
    stoch_list[[i]] = rbindlist(tmp_branch_list) 
  }
  stoch_bg_biome = rbindlist(stoch_list)
  
  # Stage 2: create time-binned bg + biome occupancies (main obj. needed for LSTT)
  # bins
  # index ( bg x biome x time )
  bin_width = 1
  n_bins = max_time / bin_width
  n_bins = floor(n_bins)
  state_bins = array(0, dim=c(n_areas, 1 , n_bins))
  
  # 0.0 to 0.5, 0.5 to 1.0, etc
  ages = seq(0.0, max_time, by=bin_width)
  dat_plot_colnames = c( names(stoch_bg_biome), "age", "joint_state" )
  dat_plot = data.frame(matrix(ncol=length(dat_plot_colnames), nrow=0), stringsAsFactors=F)
  colnames(dat_plot) = dat_plot_colnames
  
  dat_tmp = data.frame(matrix(ncol=length(dat_plot_colnames), nrow=1e3), stringsAsFactors=F)
  colnames(dat_tmp) = dat_plot_colnames
  
  states <- lapply(stoch_bg_biome$start_state,get_bg_state_2)
  dat_plot_tmp <- list()
  idx_tmp = 1
  curr_it = -1
  for (i in 1:nrow(stoch_bg_biome)) {
    # cat("Stage 2: create time-binned bg occupancies, processing iteration ",nrow(stoch_bg_biome)," / ", max(nrow(stoch_bg_biome)), "\n", sep="")
    if (curr_it != stoch_bg_biome$iteration[i]) {
      curr_it = stoch_bg_biome$iteration[i]
cat("Stage 2: create time-binned bg occupancies, processing iteration ",curr_it," / ", max(stoch_bg_biome$iteration), "\r", sep="")
    }
    bg_idx = states[[i]]
    #age_bins = seq(floor(stoch_bg_biome$x2[i]), ceiling(stoch_bg_biome$x1[i]), by=bin_width ) / bin_width
    start_age = floor(stoch_bg_biome$branch_start_time[i])
    end_age = ceiling(stoch_bg_biome$branch_end_time[i])
    age_bins = start_age:end_age * bin_width
    time_idx = age_bins + 1
    for (j in 1:length(age_bins)) {
      for (k in 1:length(bg_idx)) {
        joint_state = bg_idx[k]
        dat_tmp[idx_tmp,] = c( stoch_bg[i,], age=age_bins[j], joint_state=joint_state)
        if (idx_tmp == nrow(dat_tmp)) {
          dat_plot_tmp[[i]] <- dat_tmp
          #dat_plot = rbind(dat_plot, dat_tmp)
          idx_tmp = 1
        } else if (idx_tmp < nrow(stoch_bg_biome)) {
          idx_tmp = idx_tmp + 1
        }
      }
    }
    state_bins[ bg_idx,1, time_idx ] = state_bins[ bg_idx,1, time_idx ] + 1
  }
  dat_plot_tmp = do.call(rbind,dat_plot_tmp)
  dat_plot = rbind(dat_plot, dat_plot_tmp)

  # Stage 3: create plotting table
  cat("Stage 3: plotting....... ", "\n", sep="")
  min_sample = calculate_ske(s=Inf,k=n_states,alpha=alpha_tol,D=D_tol)$n
  ret = list()
  # create a melted data frame with Count/Support for Area/Biome over time (Age)
  d1 = matrix(nrow=0, ncol=6)
  colnames(d1) = c("age","count","Area","Biome","Area_Biome","Support")
  for (i in 1:dim(state_bins)[1]) {
    for (j in 1:dim(state_bins)[2]) {
      for (k in 1:dim(state_bins)[3]) {
        d1 = rbind(d1, c( ages[k], state_bins[i,j,k], i, j, paste(i, j, sep="_"), 0))
      }
    }
  }
  
  # prepare column values
  d2         = data.frame(d1, stringsAsFactors=FALSE)
  d2$age     = as.numeric(d2$age)
  d2$count   = as.numeric(d2$count)
  d2$Support = as.numeric(d2$Support)
  
  # compute confidence in state for each time step using
  # multinomial confidence metric (SK Ernst)
  n_biomes  = 1
  biome_conf    = t(apply( state_bins, 3, rowSums))
  bg_conf    = t(apply( state_bins, 3, rowSums))
  min_sample = min_sample
  for (i in 1:n_bins) {
    for (j in 1:n_biomes) {
      if (biome_conf[j] > min_sample) { 
        biome_conf[j] = 1
      } else {
        biome_conf[j] = 0
      }
    }
    for (j in 1:n_areas) {
      if (bg_conf[i,j] > min_sample) {
        bg_conf[i,j] = 1
      } else {
        bg_conf[i,j] = 0
      }
    }
  }
  
  # only show time-bins that contain more samples than min_sample
  d2_ages = unique(d2$age)
  d2_bg_trunc = d2
  d2_biome_trunc = d2
  
  for (i in 1:length(d2_ages)) {
    for (j in 1:n_areas) {
      c_ij = d2_bg_trunc[ d2_bg_trunc$age==i & d2_bg_trunc$Area==j, ]$count
      if (length(c_ij) == 0) { 
        # do nothing
      } else {
        d2_bg_trunc[ d2_bg_trunc$age==i & d2_bg_trunc$Area==j, ]$Support = bg_conf[i,j]
      }
    }
    # 
    # for (j in 1:n_biomes) {
    #   c_ij = d2_biome_trunc[ d2_biome_trunc$age==i & d2_biome_trunc$Biome==j, ]$count
    #   if (length(c_ij) == 0) { 
    #     # do nothing
    #   } else {
    #     d2_biome_trunc[ d2_biome_trunc$age==i & d2_biome_trunc$Biome==j, ]$Support = biome_conf[j]
    #   }
    # }
  }
  ret$bg = d2_bg_trunc
  #ret$biome = d2_biome_trunc
  
  plot_dat = ret
 save(plot_dat,file=here(output))
}

STT_plot_2  <- function(data_full="output_data/HistoryTable_full.RData", data_pruned="output_data/HistoryTable_pruned.RData",palette="Hiroshige",n_colors=8,support=TRUE,save=TRUE,output="plots/STT_summ.pdf"){
  load(here(data_full))
  plot_dat_full = plot_dat$bg
  load(here(data_pruned))
  plot_dat_pruned = plot_dat$bg
  
  plot_dat_full <- plot_dat_full %>% as_tibble() %>% mutate(Dataset="with fossils")
  plot_dat_pruned <- plot_dat_pruned %>% as_tibble() %>% mutate(Dataset="without fossils")
  
  if(support){ 
  plot_dat_full %>% bind_rows(.,plot_dat_pruned) %>% filter(Support != 0) %>% 
    group_by(age,Dataset) %>% mutate(Percent=count/sum(count)) -> dito
  } else {plot_dat_full %>% bind_rows(.,plot_dat_pruned) %>%
      group_by(age,Dataset) %>% mutate(Percent=count/sum(count)) -> dito }
   
 stt <- dito %>% mutate(Region=case_when(Area%in%c(1,4,6)~"Laurasia",Area%in%c(2,3,5,7,8)~"Gondwana")) %>% group_by(Dataset,age,Area) %>% #summarize(Percent=sum(Percent),Region=first(Region)) %>% 
    ggplot( aes(x=age, y=Percent,fill=fct_relevel(Area,"1","4","6","8","2","3","5","7"),color=fct_relevel(Area,"1","4","6","8","2","3","5","7"))) + scale_x_continuous(limits=c(0,215)) + 
    geom_bar(stat="identity",position="fill",na.rm=F)  +
    #geom_area(position = "fill") +
    scale_x_continuous("Million of years ago", trans="reverse", limits=c(215,0),expand=c(0,0)) + 
    scale_y_continuous("Frequency",limits=c(0,1),expand=c(0,0))+
    scale_color_manual(values= MetBrewer::met.brewer(palette,n=n_colors),name="",
                       labels = c("Nearctic","Palearctic","Asia","India","Neotropics", "Antarctica","Africa","Australasia")) +
    scale_fill_manual(values= MetBrewer::met.brewer(palette,n=n_colors),name="",labels = c("Nearctic","Palearctic","Asia","India", "Neotropics", "Antarctica","Africa","Australasia")) +
    
    theme(legend.position="bottom",panel.grid = element_blank(),panel.background = element_blank(),axis.line = element_line()) + 
    facet_wrap(~Dataset) + labs(title="Lifting the veil of extinction in biogeography",subtitle="Ancestral range reconstruction in tree ferns",caption="Ramírez-Barahona") +
    NULL
  
  if (save==TRUE) ggsave(stt,file=output,width=14,height=8,units="in")
  }

plot_ancestral_states = function(tree_file,summary_statistic="MAP", 
                                 tree_layout="rectangular",
                                 include_start_states=FALSE, 
                                 xlim_visible=c(0, 280), ylim_visible=NULL,
                                 tip_label_size=4, tip_label_offset=5,
                                 tip_label_italics=FALSE,tip_node_size=2,tip_node_shape=15,node_label_size=4, 
                                 node_pp_label_size=0,node_label_nudge_x=0.1,node_pp_label_nudge_x=0.1,
                                 shoulder_label_size=3, shoulder_label_nudge_x=-0.1, 
                                 node_pie_diameter=1.10,tip_pie_diameter=1.08,
                                 pie_nudge_x=0.0,pie_nudge_y=0.0,
                                 alpha=0.5, node_size_range=c(6, 15), 
                                 color_low="#D55E00",color_mid="#F0E442",color_high="#009E73",
                                 show_state_legend=TRUE,show_posterior_legend=TRUE,show_tree_scale=TRUE,
                                 state_labels=NULL,state_colors=NULL,title="",fig_height=7,fig_width=7,...) { 
  
  if ( (summary_statistic %in% c("MAP", "mean", "MAPChromosome", "MAPRange", "PieRange", "PieState")) == FALSE ) {
    print("Invalid summary statistic.")
    return()
  }
  
  # read in tree
  t = read.beast(tree_file)
  
  # add state labels
  #print(state_labels)
  t = assign_state_labels(t, state_labels, include_start_states)
  
  # add range for pp factors
  t = set_pp_factor_range(t, include_start_states)
  
  # add state colors
  use_state_colors = !is.null(state_colors)
  if (!is.null(state_colors) && !is.null(state_labels))
  {
    names(state_colors) = state_labels
  }
  
  tree = attributes(t)$phylo
  n_node = ggtree:::getNodeNum(tree)
  
  # remove underscores from tip labels
  attributes(t)$phylo$tip.label = gsub("_", " ", attributes(t)$phylo$tip.label)
  
  if (tip_label_italics) {
    attributes(t)$phylo$tip.label = paste("italic('", attributes(t)$phylo$tip.label, "')", sep="")
  }
  
  # add tip labels
  p = ggtree(t, layout=tree_layout, ladderize=TRUE)
  p = p + geom_tiplab(size=tip_label_size, offset=tip_label_offset, parse=tip_label_italics)
  
  if (summary_statistic == "MAPChromosome") {
    
    if (include_start_states) {
      
      if (!("start_state_1" %in% colnames(attributes(t)$data))) {
        print("Start states not found in input tree.")
        return()
      }
      
      
      # set the root's start state to NA
      attributes(t)$data$start_state_1[n_node] = NA
      
      # add clado daughter lineage start states on "shoulders" of tree
      # get x, y coordinates of all nodes
      x = getXcoord(tree)
      y = getYcoord(tree)
      x_anc = numeric(n_node)
      node_index = numeric(n_node)
      for (i in 1:n_node) {
        if (getParent(tree, i) != 0) {
          # if not the root, get the x coordinate for the parent node
          x_anc[i] = x[getParent(tree, i)]
          node_index[i] = i
        }
      }
      shoulder_data = data.frame(node=node_index, x_anc=x_anc, y=y)
      p = p %<+% shoulder_data
      
      # plot the states on the "shoulders"
      p = p + geom_text(aes(label=start_state_1, x=x_anc, y=y), hjust="right", nudge_x=shoulder_label_nudge_x, size=shoulder_label_size, na.rm=TRUE)
      
      # add ancestral states as node labels
      p = p + geom_text(aes(label=end_state_1), hjust="left", nudge_x=node_label_nudge_x, size=node_label_size)
      
      # show ancestral states as size / posteriors as color
      p = p + geom_nodepoint(aes(colour=as.numeric(end_state_1_pp), size=as.numeric(end_state_1)), alpha=alpha)
      
    } else {
      
      # add ancestral states as node labels
      p = p + geom_text(aes(label=anc_state_1), hjust="left", nudge_x=node_label_nudge_x, size=node_label_size)
      
      # show ancestral states as size / posteriors as color
      p = p + geom_nodepoint(aes(colour=as.numeric(anc_state_1_pp), size=as.numeric(anc_state_1)), alpha=alpha)
      
    }
    
    min_low = 0.0
    max_up = 1.0
    p = p + scale_colour_gradient2(low=color_low, mid=color_mid, high=color_high, limits=c(min_low, max_up), midpoint=0.5)
    if (show_state_legend) {
      p = p + guides(size=guide_legend("Chromosome Number"))
    } else {
      p = p + guides(size=FALSE)
    }
    if (show_posterior_legend) {
      p = p + guides(colour=guide_legend("Posterior Probability", override.aes = list(size=8)))
    } else {
      p = p + guides(colour=FALSE)
    }
    
  } 
  else if (summary_statistic == "MAPRange") {
    if (!include_start_states) {
      warning("Ignoring that include_start_states is set to FALSE")
    }
    if (!("start_state_1" %in% colnames(attributes(t)$data))) {
      print("Start states not found in input tree.")
      return()
    }
    
    # add ancestral states as node labels
    #p = p + geom_text(aes(label=end_state_1), hjust="left", nudge_x=node_label_nudge_x, size=node_label_size)
    
    # set the root's start state to NA
    attributes(t)$data$start_state_1[n_node] = NA
    
    # add clado daughter lineage start states on "shoulders" of tree
    # get x, y coordinates of all nodes
    x = getXcoord(tree)
    y = getYcoord(tree)
    x_anc = numeric(n_node)
    node_index = numeric(n_node)
    for (i in 1:n_node) {
      if (getParent(tree, i) != 0) {
        # if not the root, get the x coordinate for the parent node
        x_anc[i] = x[getParent(tree, i)]
        node_index[i] = i
      }
    }
    shoulder_data = data.frame(node=node_index, x_anc=x_anc, y=y)
    p = p %<+% shoulder_data
    
    # plot the states on the "shoulders"
    p = p + geom_text(aes(label=start_state_1, x=x_anc, y=y), hjust="right", nudge_x=shoulder_label_nudge_x, size=shoulder_label_size, na.rm=TRUE)
    p = p + geom_nodepoint(aes(colour=factor(start_state_1), x=x_anc, y=y, size=as.numeric(end_state_1_pp)),na.rm=TRUE, alpha=alpha)
    p = p + geom_tippoint(aes(colour=factor(start_state_1), x=x_anc, y=y, size=as.numeric(end_state_1_pp)),na.rm=TRUE, alpha=alpha)
    
    # show tip states as color
    #print(shoulder_data)
    #print(x_anc)
    #print(c(attributes(t)$data$start_state_1,attributes(t)$data$end_state_1))
    
    p = p + geom_tippoint(aes(colour=factor(end_state_1)), size=tip_node_size, alpha=alpha) 
    
    # show ancestral states as color / posteriors as size
    p = p + geom_nodepoint(aes(colour=factor(end_state_1), size=as.numeric(end_state_1_pp)), alpha=alpha)
    
    if (show_state_legend) {
      p = p + guides(colour=guide_legend("Range", override.aes = list(size=8), order=1))
    } else {
      p = p + guides(colour=FALSE)
    }
    
    if (show_posterior_legend) {
      p = p + guides(size=guide_legend("Posterior probability", order=2))
    } else {
      p = p + guides(size=FALSE)
    }
    
    #return(p)
    
  } 
  else if (summary_statistic == "MAP") {
    
    if (include_start_states) {
      print("Start states not yet implemented for MAP ancestral states.")
      return()
      
    }
    if (!("anc_state_1" %in% colnames(attributes(t)$data))) {
      anc_data = data.frame(node=names(attributes(t)$data$end_state_1), 
                            anc_state_1=levels(attributes(t)$data$end_state_1)[attributes(t)$data$end_state_1],
                            anc_state_1_pp=as.numeric(levels(attributes(t)$data$end_state_1_pp))[attributes(t)$data$end_state_1_pp])
      p = p %<+% anc_data
    }
    
    # add ancestral states as node labels
    p = p + geom_text(aes(label=anc_state_1), hjust="left", nudge_x=node_label_nudge_x, size=node_label_size)
    
    # show ancestral states as color / posteriors as size
    p = p + geom_nodepoint(aes(colour=factor(anc_state_1), size=as.numeric(anc_state_1_pp)), alpha=alpha)
    
    pp = as.numeric( as.vector( attributes(t)$data$anc_state_1_pp) )
    #print(pp)
    
    if (!F) {
      pp_offset_range = 2*(c(min(pp), max(pp)) - 0.5)
      nd_offset_interval = node_size_range[2] - node_size_range[1]
      nd_offset = node_size_range[1]
      node_size_range = pp_offset_range * nd_offset_interval + nd_offset
      #node_size_range[1] = node_size_range[1] * min(pp) / 0.5
      #node_size_range[2] = node_size_range[2] * max(pp)
    }
    
    if (node_label_size == 0) {
      p = p + geom_text(aes(label=sprintf("%.02f", as.numeric(anc_state_1_pp))), hjust="left", nudge_x=node_label_nudge_x, size=node_pp_label_size)
    }
    #p = p = scale_fill_continuous(breaks=c(0.6, 0.7, 0.8, 0.9, 1.0))
    
    # show the tip values
    p = p + geom_tippoint(aes(colour=factor(anc_state_1)), size=tip_node_size, alpha=alpha, shape=tip_node_shape)
    
    # set up the legend
    if (show_state_legend) {
      p = p + guides(colour=guide_legend("State"), order=1)        
    } else {
      p = p + guides(colour=FALSE, order=2)
    }
    if (show_posterior_legend) {
      p = p + guides(size=guide_legend("Posterior Probability"), order=3)
    } else {
      p = p + guides(size=FALSE, order=4)
    }
    
  } 
  else if (summary_statistic == "mean") {
    
    if (include_start_states) {
      print("Start states not implemented for mean ancestral states.")
      return()
    }
    
    # add ancestral states as node labels
    p = p + geom_text(aes(label=round(mean, 2)), hjust="left", nudge_x=node_label_nudge_x, size=node_label_size)
    
    # show the size of the 95% CI as color 
    lowers = as.numeric(levels(attributes(t)$data$lower_0.95_CI))[attributes(t)$data$lower_0.95_CI]
    uppers = as.numeric(levels(attributes(t)$data$upper_0.95_CI))[attributes(t)$data$upper_0.95_CI]
    diffs = uppers - lowers
    diffs_df = data.frame(node=names(attributes(t)$data$lower_0.95_CI), diff_vals=diffs)
    p = p %<+% diffs_df 
    
    min_low = min(diffs, na.rm=TRUE)
    max_up = max(diffs, na.rm=TRUE)
    mid_val = min_low + (max_up - min_low) / 2.0
    p = p + scale_colour_gradient2(low=color_low, mid=color_mid, high=color_high, limits=c(min_low, max_up), midpoint=mid_val)
    p = p + geom_nodepoint(aes(size=mean, colour=diff_vals), alpha=alpha)
    
    # show the tip values
    p = p + geom_tippoint(aes(size=mean), color="grey", alpha=alpha)
    
    # set up the legend
    if (show_state_legend) {
      legend_text = "Mean State"
      p = p + guides(size=guide_legend(legend_text))
    } else {
      p = p + guides(size=FALSE)
    }
    if (show_posterior_legend) {
      p = p + guides(colour=guide_legend("95% CI Width", override.aes=list(size=4)))
    } else {
      p = p + guides(colour=FALSE)
    }
  } 
  else if (summary_statistic == "PieState") {
    if (include_start_states) {
      print("Start states not yet implemented for PieState ancestral states.")
      return()
      
    }
    
    if (!("anc_state_1" %in% colnames(attributes(t)$data))) {
      anc_data = data.frame(node=names(attributes(t)$data$end_state_1), 
                            anc_state_1=levels(attributes(t)$data$end_state_1)[attributes(t)$data$end_state_1],
                            anc_state_1_pp=as.numeric(levels(attributes(t)$data$end_state_1_pp))[attributes(t)$data$end_state_1_pp])
      #p = p %<+% anc_data
    }
    
    # print tips
    p = p + geom_tippoint(aes(colour=factor(anc_state_1)), size=1e-2) 
    
    # plot invisible node states (for legend)
    p = p + geom_nodepoint(aes(colour=factor(anc_state_1), size=0),na.rm=TRUE, alpha=0.0)
    p = p + geom_nodepoint(aes(colour=factor(anc_state_2), size=0),na.rm=TRUE, alpha=0.0)
    p = p + geom_nodepoint(aes(colour=factor(anc_state_3), size=0),na.rm=TRUE, alpha=0.0)
    
    
    # set up the legend
    if (show_state_legend) {
      p = p + guides(colour=guide_legend("State", override.aes = list(size=5)), order=1)        
    } else {
      p = p + guides(colour=FALSE, order=2)
    }
    p = p + guides(size=FALSE)
    if (use_state_colors) {
      p = p + scale_color_manual(values=state_colors, breaks=state_labels)
    }
    
    # position legend
    p = p + theme(legend.position="left")
    
    # get anc state matrices (for pie/bar charts)
    dat_state_anc = build_state_probs(t, state_labels, include_start_states)$anc
    
    # make pie objects
    n_tips = length(tree$tip.label)
    n_nodes = 2 * n_tips - 1
    node_idx = (n_tips+1):n_nodes
    tip_idx = 1:n_tips
    all_idx = 1:n_nodes
    pies_anc = nodepie(dat_state_anc, cols=1:(ncol(dat_state_anc)-1), color=state_colors, alpha=alpha)
    
    # print pies
    
    # build pie diameters for tips and internal nodes
    pd = c( rep(tip_pie_diameter, n_tips), rep(node_pie_diameter, n_nodes-n_tips) )
    p_node = inset.revgadgets(tree_view=p,
                              insets=pies_anc[all_idx],
                              x="node",
                              height=pd,
                              width=pd,
                              hjust=pie_nudge_x,
                              vjust=pie_nudge_y)
    
    
    # save pdf
    # ggsave(file=paste(stree_fn,".out_state.pdf",sep=""),device="pdf",height=7,width=7)
    
    return(p_node)
    
  } 
  else if (summary_statistic == "PieRange") {
    
    if (!("start_state_1" %in% colnames(attributes(t)$data))) {
      print("Start states not found in input tree.")
      return()
    }
    
    # set the root's start state to NA
    #attributes(t)$data$start_state_1[n_node] = NA
    
    # print tips
    p = p + geom_tippoint(aes(colour=factor(end_state_1)), size=1e-2, alpha=alpha) 
    
    # plot invisible node states (for legend)
    p = p + geom_nodepoint(aes(colour=factor(start_state_1), size=0),na.rm=TRUE, alpha=0.0)
    p = p + geom_nodepoint(aes(colour=factor(start_state_2), size=0),na.rm=TRUE, alpha=0.0)
    p = p + geom_nodepoint(aes(colour=factor(start_state_3), size=0),na.rm=TRUE, alpha=0.0)
    
    # set up the legend
    if (show_state_legend) {
      p = p + guides(colour=guide_legend("State"), order=1)        
    } else {
      p = p + guides(colour=FALSE, order=2)
    }
    p = p + guides(size=FALSE)
    p = p + guides(colour = guide_legend(override.aes = list(size=5)))
    if (use_state_colors) {
      used_states = collect_probable_states(p)
      p = p + scale_color_manual(values=state_colors, breaks=state_labels,  name="Range", limits = used_states)
    }
    p = p + theme(legend.position="left")
    
    # # MJL: to remove later
    # break_legend = F
    # if (break_legend) {
    #     p$data$x = p$data$x + (15 - max(p$data$x))
    #     x_breaks = 0:15
    #     x_labels = rep("", 16)
    #     x_labels[ c(0,5,10,15)+1 ] = c(0,5,10,15)
    #     p = p + scale_x_continuous(breaks = x_breaks, labels = rev(x_labels), sec.axis = sec_axis(~ ., breaks = 15-c(6.15, 4.15, 2.55, 1.2), labels=c("+K","+O","+M","+H") ))
    #     p = p + theme_tree2()
    #     p = p + coord_cartesian(xlim = c(0,20), expand=TRUE)
    #     p = p + labs(x="Age (Ma)")
    #     p = add_island_times(p)
    #     p = p + theme(legend.position="left", axis.line = element_line(colour = "black"))
    #     p = p + guides(colour = guide_legend(override.aes = list(size=5), nrow=6))
    # }
    
    # get anc state matrices (for pie/bar charts)
    #print(t)
    dat_state_end = build_state_probs(t, state_labels, include_start_states)$end
    dat_state_start = build_state_probs(t, state_labels, include_start_states)$start
    
    # make pie objects
    n_tips = length(tree$tip.label)
    n_nodes = 2 * n_tips - 1
    node_idx = (n_tips+1):n_nodes
    tip_idx = 1:n_tips
    all_idx = 1:n_nodes
    
    pies_end = nodepie(dat_state_end,cols=1:(ncol(dat_state_end)-1),color=state_colors,alpha=alpha)
    pies_start = nodepie(dat_state_start,cols=1:(ncol(dat_state_start)-1),color=state_colors,alpha=alpha)
    
    pd = c( rep(tip_pie_diameter, n_tips), rep(node_pie_diameter, n_nodes-n_tips) )
    
    #n_expr = options()$expressions
    #options(expressions=n_expr * 2)
    p_node =  inset.revgadgets(tree_view=p,
                               insets=pies_end[all_idx],
                               x="node",
                               height=pd,
                               width=pd,
                               hjust=pie_nudge_x,
                               vjust=pie_nudge_y)
    
    
    p_shld = inset.revgadgets(tree_view=p_node,
                              insets=pies_start,
                              x="parent_shoulder",
                              height=node_pie_diameter*0.9,
                              width=node_pie_diameter*0.9,
                              hjust=pie_nudge_x,
                              vjust=pie_nudge_y)
    
    
    p_all = p_shld + coord_cartesian(xlim = xlim_visible, ylim=ylim_visible, expand=TRUE)
    
    return(p_shld)
  } 
  
  
  if (use_state_colors) {
    #print(state_colors)
    #print(state_labels)
    p = p + scale_color_manual(values=state_colors, breaks=as.vector(state_labels))
  }
  
  p = p + scale_radius(range = node_size_range)
  p = p + theme(legend.position="left")
  
  # show title
  p = p + ggtitle(title)
  
  # set visible area
  p = p + coord_cartesian(xlim = xlim_visible, ylim=ylim_visible, expand=TRUE)
  
  return(p)
}

filter_trees <- function(target_tree="output_data/MCC_Master_Pruned.tre",trees="output_data/Master_Resolved.trees",output="output_data/Master_Resolved_Const.trees"){ 
  backbone <- ape::read.nexus(here(target_tree))
  iden <- which(backbone$tip.label %in% c("Plagiogyria_stenoptera",
                                          "Culcita_coniifolia","Loxsoma_cunninghamii" ,"Cibotium_regale" ,
                                          "Dicksonia_gigantea","Sphaeropteris_glauca", "Thyrsopteris_elegans", "Metaxya_rostrata"))
  backbone.p <- drop.tip(backbone,tip =backbone$tip.label[-iden] )
  comparison <- list()
  a <- data.table::fread(here(trees),header = T,sep = "\t")
  burn.in = 0
  f_burn = burn.in * dim(a)[1]
  if (burn.in == 0) f_burn = 1
  a <-a[f_burn:dim(a)[1],]
  tipas <- c("Plagiogyria_stenoptera","Culcita_coniifolia","Loxsoma_cunninghamii" ,
             "Cibotium_regale" ,"Dicksonia_gigantea","Sphaeropteris_glauca", "Thyrsopteris_elegans", "Metaxya_rostrata")
  master_compare <- 1:dim(a)[1] %>% future_map_lgl(function(lis){
    r1 <- a$fbd_tree[lis] %>% ape::read.tree(text = .) 
    r2 <- which(r1$tip.label %in% tipas) 
    r3  <- ape::drop.tip(phy = r1, tip = r1$tip.label[-r2])  %>% all.equal.phylo(backbone.p,.,use.edge.length=FALSE)
    return(r3)
  },.progress = TRUE)
  master_compare
  table(unlist(master_compare))
  write.table(a[which(unlist(master_compare)),],file=here(output),quote = F,col.names = T,row.names = F,sep = "\t")
}

do_rates <- function(tree_file = "output_data/MCC_Master_Resolved.tre",branch_rates_file = "output_data/Master_BDS_Resolved.log",parameter_name="net_div",burnin = 0,viridis_option="F",label_legend="NetDiv") 
{ 
  if ( (parameter_name %in% c("lambda", "mu", "net_div")) == FALSE ) {
    print("Invalid parameter to plot.")
    return()
  }
  # read in tree
  tree <- try(read.tree(tree_file), silent=TRUE)
  if ( class(tree) == "try-error"  ) {
    tree = try(read.nexus(tree_file), silent=TRUE)
  }
  map <- matchNodes(tree)
  # read the posterior distributions
  samples <- read.table(branch_rates_file, sep="\t", stringsAsFactors=FALSE, check.names=FALSE, header=TRUE)
  # calculate net diversification if needed
  if (parameter_name == "net_div") {
    lambdas <- as.matrix(samples[,grepl("avg_lambda", colnames(samples))])
    mus <-  as.matrix(samples[,grepl("avg_mu", colnames(samples))])
    net_divs <- as.data.frame(lambdas - mus)
    colnames(net_divs) <- gsub("lambda","net_div",colnames(net_divs))
    samples <- cbind(samples,net_divs)
  }
  
  # discard some burnin 
  n_samples <- nrow(samples)
  # combine the mcmc output
  rate_output <- samples[-c(1:ceiling(n_samples * burnin)),grepl(paste0("avg_",parameter_name), colnames(samples))]
  # store the parameters
  rate_mean <-  apply(rate_output,2,median) #colMeans(rate_output)
  
  branch_rates <- rate_mean[-length(rate_mean)]
  
  # compute the intervals
  rate_intervals <- pretty(unlist(branch_rates), n=1001)
  #rate_intervals <- c(0,0.2)
  tree_tbl <- as_tibble(tree)
  # compute the legend
  legend_intervals <- pretty(rate_intervals)
  legend_intervals <- legend_intervals[legend_intervals > min(rate_intervals) & legend_intervals < max(rate_intervals)]
  legend_intervals_at <- (legend_intervals - min(rate_intervals)) / diff(range(rate_intervals))
  
  # get the branch rates
  these_rates <- branch_rates[paste0("avg_",parameter_name,"[",map$Rev[match(tree$edge[,2], map$R)],"]")]

  rate_tree = tree
  rate_tree$edge.length <- these_rates
  rate_tbl <- as_tibble(rate_tree)
  tree_tbl$rates <- rate_tbl$branch.length
  this_tree <- as.treedata(tree_tbl)
  
  tree_plot <- ggtree::ggtree(this_tree, aes(color=rates)) + scale_color_viridis(name=paste0(label_legend), option=viridis_option, limits=range(rate_intervals)) + theme(legend.position=c(0.2,0.85), legend.background=element_blank())
  return(list(tree_plot,tree_tbl))
}
plot_rates <- function(pruned=T){ 
  if(pruned) {arbol = "output_data/MCC_Master_Resolved.tre"
  max_age <- 208.10
  output="plots/BDS_pruned_NetDiv.pdf"
  } else {arbol = "output_data/MCC_Master_Resolved.tre"
  max_age <- 215.47
  output="plots/BDS_full_NetDiv.pdf"}
  tree <- read.nexus(here(arbol))
  breaks <- max_age - c(0,2.58,23.03,66,145,201.3,251.9)
  pos <- max_age - c(13,43,105,175,230)
  colors <- c(MaizePal::maize_pal("RubyGold")[c(4,3,6)],MaizePal::maize_pal("OaxacaGreen")[c(2,4)],MaizePal::maize_pal("GlassGem")[2])
  pp <- bds_tree + ylim(c(-40,Ntip(tree))) + 
    annotate(geom="rect",ymin=-40,ymax=-5,xmin=breaks[1],xmax=breaks[2],fill=adjustcolor(colors[1],alpha.f = 0.6),color="black",size=0.2) +
    annotate(geom="rect",ymin=-40,ymax=-5,xmin=breaks[2],xmax=breaks[3],fill=adjustcolor(colors[2],alpha.f = 0.6),color="black",size=0.2) +
    annotate(geom="rect",ymin=-40,ymax=-5,xmin=breaks[3],xmax=breaks[4],fill=adjustcolor(colors[3],alpha.f = 0.6),color="black",size=0.2) + 
    annotate(geom="rect",ymin=-40,ymax=-5,xmin=breaks[4],xmax=breaks[5],fill=adjustcolor(colors[4],alpha.f = 0.6),color="black",size=0.2) + 
    annotate(geom="rect",ymin=-40,ymax=-5,xmin=breaks[5],xmax=breaks[6],fill=adjustcolor(colors[5],alpha.f = 0.6),color="black",size=0.2) +
    annotate(geom="rect",ymin=-40,ymax=-5,xmin=breaks[6],xmax=breaks[7],fill=adjustcolor(colors[6],alpha.f = 0.6),color="black",size=0.2) + 
    annotate(geom="text",x = pos,y = -20,label=c("NEO","PAL","CRE","JUR","TRI"),fontface="bold")
  
  pp_2 <- wrap_plots(list(pp)) + plot_layout(byrow=T,nrow = 1,ncol=1,guides = "keep",tag_level = 'new') + plot_annotation(title = "Diversification rates in tree ferns (Cyatheales)",subtitle="BSDR Model", caption= "Ramírez-Barahona") & theme(legend.position=c(0.1,0.7), legend.background=element_blank())
  ggsave(here(output), width=15, height=15, units="cm",plot=pp_2)
}
rev.process.div.rates = function(speciation_times_file="rb_out/EDR/output/Cyatheales_EBD_speciation_times.log",speciation_rates_file="rb_out/EDR/output/Cyatheales_EBD_speciation_rates.log",extinction_times_file="rb_out/EDR/output/Cyatheales_EBD_extinction_times.log",extinction_rates_file="rb_out/EDR/output/Cyatheales_EBD_extinction_rates.log",fossilization_times_file="",fossilization_rates_file="",
                                 tree = "output_data/MCC_Master_Resolved.tre",maxAge=NULL,burnin=0.25,numIntervals=10){
  
  # Get the time of the tree and divide it into intervals
  if(!is.null(tree)){
    tree <- read.nexus(here(tree))
    time <- max(node.depth.edgelength(tree))
    intervals <- seq(0,time,length.out=numIntervals+1)
  } else {
    intervals <- seq(0,maxAge,length.out=numIntervals+1)
  }
  
  processSpeciationRates <- rev.read.mcmc.output.rates.through.time(here(speciation_times_file), here(speciation_rates_file), intervals, burnin)
  processExtinctionRates <- rev.read.mcmc.output.rates.through.time(here(extinction_times_file), here(extinction_rates_file), intervals, burnin)
  
  # Process the net-diversification and relative-extinction rates
  processNetDiversificationRates <- as.mcmc(processSpeciationRates-processExtinctionRates)
  processRelativeExtinctionRates <- as.mcmc(processExtinctionRates/processSpeciationRates)
  
  processSpeciationRatesShiftProb <- c()
  for ( i in 2:numIntervals ) {
    processSpeciationRatesShiftProb[i] <- mean(processSpeciationRates[,i] > processSpeciationRates[,i-1])
  }
  
  if ( fossilization_times_file != "" && fossilization_rates_file != "" ) {
    
    processFossilizationRates <- rev.read.mcmc.output.rates.through.time(here(fossilization_times_file), here(fossilization_rates_file), intervals, burnin)
    
    res <- list("speciation rate" = processSpeciationRates,
                "extinction rate" = processExtinctionRates,
                "fossilization rate" = processFossilizationRates,
                "net-diversification rate" = processNetDiversificationRates,
                "relative-extinction rate" = processRelativeExtinctionRates,
                "tree" = tree,
                "intervals" = rev(intervals) )
    
    return(res)
    
  } else {
    
    res <- list("speciation rate" = processSpeciationRates,
                "extinction rate" = processExtinctionRates,
                "net-diversification rate" = processNetDiversificationRates,
                "relative-extinction rate" = processRelativeExtinctionRates,
                "speciation rate shift prob" = processSpeciationRatesShiftProb,
                "tree" = tree,
                "intervals" = rev(intervals) )
    
    return(res)
    
  }
}

rev.read.mcmc.output.rates.through.time = function(times_file_name, rates_file_name, intervals, burnin=0.25) {
  
  # Process the rates
  lines_to_skip <- 0
  s <- readLines(times_file_name)[lines_to_skip+1]
  while ( substring(s, 1, 1) == "#" ) {
    lines_to_skip <- lines_to_skip + 1
    s <- readLines(times_file_name)[lines_to_skip+1]
  }
  
  cols <- strsplit(readLines(times_file_name)[lines_to_skip+1],"\t")[[1]]
  col_headers <- c("Iteration","Replicate_ID","Posterior","Likelihood","Prior")
  cols_to_skip <- sum(col_headers %in% cols)
  
  rate_change_times   <- strsplit(readLines(times_file_name)[-(1:(lines_to_skip+1))],"\t")
  rates               <- strsplit(readLines(rates_file_name)[-(1:(lines_to_skip+1))],"\t")
  n_burnt_sampled     <- round(length(rates) * burnin)
  
  process_rates <- as.mcmc(do.call(rbind,lapply(n_burnt_sampled:length(rate_change_times),function(sample) {
    times <- as.numeric(rate_change_times[[sample]][-(1:cols_to_skip)])
    rates <- as.numeric(rates[[sample]][-(1:cols_to_skip)])
    order <- order(times)
    times <- times[order]
    rates <- c(rates[1],rates[-1][order])
    res   <- rates[findInterval(intervals[-1],times)+1]
    res   <- rev(res)
    return (res)
  } )))
  
  return(process_rates)
  
}
matchNodes = function(phy) {
  
  # get some useful info
  num_tips = length(phy$tip.label)
  num_nodes = phy$Nnode
  tip_indexes = 1:num_tips
  node_indexes = num_tips + num_nodes:1
  
  node_map = data.frame(R=1:(num_tips + num_nodes), Rev=NA, visits=0)
  current_node = phy$Nnode + 2
  k = 1
  t = 1
  
  while(TRUE) {
    
    if ( current_node <= num_tips ) {
      node_map$Rev[node_map$R == current_node] = t
      current_node = phy$edge[phy$edge[,2] == current_node,1]
      t = t + 1
    } else {
      
      if ( node_map$visits[node_map$R == current_node] == 0 ) {
        node_map$Rev[node_map$R == current_node] = node_indexes[k]
        k = k + 1
      }
      node_map$visits[node_map$R == current_node] = node_map$visits[node_map$R == current_node] + 1
      
      if ( node_map$visits[node_map$R == current_node] == 1 ) {
        # go right
        current_node = phy$edge[phy$edge[,1] == current_node,2][2]
      } else if ( node_map$visits[node_map$R == current_node] == 2 ) {
        # go left
        current_node = phy$edge[phy$edge[,1] == current_node,2][1]
      } else if ( node_map$visits[node_map$R == current_node] == 3 ) {
        # go down
        if (current_node == num_tips + 1) {
          break
        } else {
          current_node = phy$edge[phy$edge[,2] == current_node,1]
        }
      }
    }
    
  }
  
  return(node_map[,1:2])
  
}
addLegend = function(tree, bins, colors, width=0.1, height=0.4, lwd=1, title="posterior probability", ...) {
  
  lastPP <- get("last_plot.phylo", envir = .PlotPhyloEnv)
  
  x_left   = width
  x_right  = width + width * lastPP$x.lim[2]
  y_bottom = bins[-length(bins)] * height * length(tree$tip.label)
  y_top    = bins[-1] * height * length(tree$tip.label)
  
  ticks = pretty(bins)
  ticks = ticks[ticks > min(bins)]
  ticks = ticks[ticks < max(bins)]
  y_tick = ticks * height * length(tree$tip.label)
  if(lwd > 0) {
    segments(x0=x_right, x1=x_right + 0.01 * abs(diff(lastPP$x.lim)), y0=y_tick, lwd=lwd, ...)
  }
  text(x=x_right + 0.02 * abs(diff(lastPP$x.lim)), y=y_tick, label=ticks, adj=0, ...)
  rect(x_left, y_bottom, x_right, y_top, col=colors, border=colors)
  
  text(x_left - width / 1.5, mean(bins) * height * length(tree$tip.label), labels=title, srt=90, ...)
  # text(x=x_left, y=max(y_top) + 0.02 * length(tree$tip.label), labels=title, adj=0, ...)
  # points(x=x_left, y=max(y_top) + 0.05 * length(tree$tip.label))
  
}

######## GEOGRAPHIC DATA ########

B <- 10
B

setwd("~/Desktop/Niche_Evol/")
library(data.table)
library(sp)
library(maps)
library(mapview)
taxa <- fread("data/Cyathea_taxa.tsv")
taxa <- taxa[which(taxa$age==0),]
taxa
p_clean <- fread("data/dists/Cyatheales_allGBIF.csv")
p_clean <- p_clean[which(p_clean$basisOfRecord=="PRESERVED_SPECIMEN" ),]
names(p_clean)
p_clean <- p_clean[which(!is.na(p_clean$decimalLongitude)),]
p_clean <- as_tibble(p_clean)
p_clean <- p_clean[-which(p_clean$family==""),]
p_clean
list.spp <- unique(p_clean$family)
list.spp <- list.spp[-2]
p_clean$Correct <- FALSE

#### Family level filter (except Cyatheaceae)
for (i in 1:length(list.spp)){ 
  cat(i,"--","Plotting data for", list.spp[i],"\n")
  iden <- which(p_clean$family == list.spp[i])
  lon.lim <- range(p_clean$decimalLongitude[iden])+c(-10,10)
  lat.lim <- range(p_clean$decimalLatitude[iden])+c(-10,10)
  plot(p_clean$decimalLongitude[iden],p_clean$decimalLatitude[iden],xlim=lon.lim,ylim=lat.lim,
       lwd=0.2,pch=19,col=rgb(0,0,0,0.8),cex=0.6)
  maps::map("world",interior=F,lwd=0.3,add=T)
  points(p_clean$decimalLongitude[iden],p_clean$decimalLatitude[iden],lwd=0.2,pch=19,col=rgb(0,0,0,0.8),cex=0.6)
  title(list.spp[i])
  yn <- readline("Is distribution correct (y/n)?")
  if(toupper(yn)=="Y") {cat("Correct distribution!","\n"); next}
  else {
    cat("Define polygon for selecting points","\n")
    xys <- locator()
    xys_poli <- Orcs::coords2Polygons(cbind(xys$x,xys$y),ID="poli")
    sp::plot(xys_poli,add=T)
    iden2 <- sp::point.in.polygon(p_clean$decimalLongitude[iden],p_clean$decimalLatitude[iden],xys$x,xys$y)
    p_clean$Correct[iden][which(iden2==1)] <- TRUE
    points(p_clean$decimalLongitude[iden][p_clean$Correct[iden]],p_clean$decimalLatitude[iden][p_clean$Correct[iden]],pch=19,cex=0.6,col=rgb(1,0,0,0.6))
    legend("bottom",inset=0.02,legend=c("Correct","Suspect"),pch=19,col=c("red","black"))
  }
  again <- readline("Re-define polygon for selecting points again (y/n)?")
  if(again!="y") next
  maps::map("world",interior=F,lwd=0.3)
  points(p_clean$decimalLongitude[iden][p_clean$Correct[iden]],p_clean$decimalLatitude[iden][p_clean$Correction[iden]],lwd=0.2,pch=19,col=rgb(0,0,0,0.8),cex=0.6)
  cat("Define polygon for selecting points","\n")
  xys <- locator()
  xys_poli <- Orcs::coords2Polygons(cbind(xys$x,xys$y),ID="poli")
  sp::plot(xys_poli,add=T)
  iden2 <- sp::point.in.polygon(p_clean$decimalLongitude[iden],p_clean$decimalLatitude[iden],xys$x,xys$y)
  merged$Correct[iden][which(iden2==1)] <- TRUE
  points(p_clean$decimalLongitude[iden][p_clean$Correct[iden]],p_clean$decimalLatitude[iden][p_clean$Correct[iden]],pch=19,cex=0.6,col=rgb(1,0,0,0.6))
  legend("bottom",inset=0.02,legend=c("Correct","Suspect"),pch=19,col=c("red","black"))
}
seasas <- CoordinateCleaner::clean_coordinates(x=p_clean,lon="decimalLongitude",lat="decimalLatitude",species="species",
                            test=c("zeros","centroids", "equal", "gbif", "institutions","capitals"),value="flagged",verbose=T)
p_clean$flagged <- seasas
p_clean$species <- sub(" ","_",p_clean$species)
names(p_clean)
table(p_clean$Correct)
table(p_clean$flagged)

plot(p_clean$decimalLongitude,p_clean$decimalLatitude,
     lwd=0.2,pch=19,col=rgb(1,0,0,0.8),cex=0.6)
maps::map("world",interior=F,lwd=0.3,add=T)
iden_2 <- which(p_clean$Correct)
points(p_clean$decimalLongitude[iden_2],
  p_clean$decimalLatitude[iden_2],lwd=0.2,pch=19,col=rgb(0,0,0,0.8),cex=0.6)

#### Second round of filtering (except Cyatheaceae)
for (i in 1:length(list.spp)){ 
  cat(i,"--","Plotting data for", list.spp[i],"\n")
  iden <- which(p_clean$family == list.spp[i] & p_clean$Correct)
  lon.lim <- range(p_clean$decimalLongitude[iden])+c(-10,10)
  lat.lim <- range(p_clean$decimalLatitude[iden])+c(-10,10)
  plot(p_clean$decimalLongitude[iden],p_clean$decimalLatitude[iden],xlim=lon.lim,ylim=lat.lim,
       lwd=0.2,pch=19,col=rgb(0,0,0,0.8),cex=0.6)
  maps::map("world",interior=F,lwd=0.3,add=T)
  points(p_clean$decimalLongitude[iden],p_clean$decimalLatitude[iden],lwd=0.2,pch=19,col=rgb(0,0,0,0.8),cex=0.6)
  title(list.spp[i])
  yn <- readline("Is distribution correct (y/n)?")
  if(toupper(yn)=="Y") {cat("Correct distribution!","\n"); next}
  else {
    cat("Define polygon for selecting points","\n")
    xys <- locator()
    xys_poli <- Orcs::coords2Polygons(cbind(xys$x,xys$y),ID="poli")
    sp::plot(xys_poli,add=T)
    iden2 <- sp::point.in.polygon(p_clean$decimalLongitude[iden],p_clean$decimalLatitude[iden],xys$x,xys$y)
    p_clean$Correct[iden][which(iden2==1)] <- FALSE
    points(p_clean$decimalLongitude[iden][p_clean$Correct[iden]],p_clean$decimalLatitude[iden][p_clean$Correct[iden]],pch=19,cex=0.6,col=rgb(1,0,0,0.6))
    legend("bottom",inset=0.02,legend=c("Correct","Suspect"),pch=19,col=c("red","black"))
  }
  again <- readline("Re-define polygon for selecting points again (y/n)?")
  if(again!="y") next
  maps::map("world",interior=F,lwd=0.3)
  points(p_clean$decimalLongitude[iden][p_clean$Correct[iden]],p_clean$decimalLatitude[iden][p_clean$Correction[iden]],lwd=0.2,pch=19,col=rgb(0,0,0,0.8),cex=0.6)
  cat("Define polygon for selecting points","\n")
  xys <- locator()
  xys_poli <- Orcs::coords2Polygons(cbind(xys$x,xys$y),ID="poli")
  sp::plot(xys_poli,add=T)
  iden2 <- sp::point.in.polygon(p_clean$decimalLongitude[iden],p_clean$decimalLatitude[iden],xys$x,xys$y)
  merged$Correct[iden][which(iden2==1)] <- FALSE
  points(p_clean$decimalLongitude[iden][p_clean$Correct[iden]],p_clean$decimalLatitude[iden][p_clean$Correct[iden]],pch=19,cex=0.6,col=rgb(1,0,0,0.6))
  legend("bottom",inset=0.02,legend=c("Correct","Suspect"),pch=19,col=c("red","black"))
}

g <- raster(nrows=180*12,ncols=360*12,xmn=-180,xmx=180,ymn=-90,ymx=90,vals=1,crs = CRS("+proj=longlat +datum=WGS84 +no_defs +ellps=WGS84 +towgs84=0,0,0")) %>% as(., 'SpatialPixels')
dists.tib <- p_clean[,23:22] %>% as_tibble(.) %>% SpatialPoints(.,proj4string= CRS("+proj=longlat +datum=WGS84 +no_defs +ellps=WGS84 +towgs84=0,0,0")) %>% 
  sp::over(.,g) %>% enframe(.,name="name")
dists.tib
names(dists.tib) <- c("name","cellID")
p_clean$cellID <- dists.tib$cellID
p_clean
save(p_clean,file="data/dists/Distribution_corrected_1.Rdata")

#### HERE !!!! (1 sept 2020)
#### This is the clean Cyatheaceae database
load("data/dists/Distribution_corrected_1.Rdata")
unique(p_clean$species)
srb <- fread("data/dists/Cyatheaceae_dists.csv")
colnames(srb) <- c("species","decimalLongitude","decimalLatitude")
srb
g <- raster(nrows=180*12,ncols=360*12,xmn=-180,xmx=180,ymn=-90,ymx=90,vals=1,crs = CRS("+proj=longlat +datum=WGS84 +no_defs +ellps=WGS84 +towgs84=0,0,0")) %>% as(., 'SpatialPixels')
dists.tib <- srb[,2:3] %>% as_tibble(.) %>% SpatialPoints(.,proj4string= CRS("+proj=longlat +datum=WGS84 +no_defs +ellps=WGS84 +towgs84=0,0,0")) %>% 
  sp::over(.,g) %>% enframe(.,name="name")
names(dists.tib) <- c("name","cellID")
srb$cellID <- dists.tib$cellID
srb

#### I need to assign a GBIF datum to every datum in the Cyatheaceae database.....
#lista_matcha <- tapply(srb$cellID,srb$species,FUN = function(x) x)
#srb[which(srb$species=="Sphaeropteris_intermedia"),]
#p_clean$cellID[which(p_clean$species=="Sphaeropteris_intermedia")]
#id <- match(lista_matcha$Sphaeropteris_intermedia,p_clean$cellID[which(p_clean$species=="Sphaeropteris_intermedia")])
#p_clean[which(p_clean$species=="Sphaeropteris_intermedia"),][id,]

p <- p_clean[-which(p_clean$family=="Cyatheaceae"),]
names(p)
names(srb)
srb <- bind_rows(srb,p[,c(10,23,22,53)])
srb <- srb
srb
na.spp <- taxa$taxon[which(is.na(match(taxa$taxon,sort(unique(srb$species)))))]
write.table(na.spp,file="data/dists/na.spp2.txt",quote=F,row.names = F,col.names = F)
na.spp <- read.table("data/dists/na.spp.txt",sep=",",header=F,stringsAsFactors = F);na.spp

for(i in 1:dim(na.spp)[1]){ 
  if(length(which(srb$species==na.spp[i,2]))==0) next
  srb[which(srb$species==na.spp[i,2]),"species"] <- na.spp[i,1]
}

spp.final <- taxa$taxon[which(!is.na(match(taxa$taxon,sort(unique(srb$species)))))]
length(spp.final)
srb.2 <- srb[which(srb$species %in% spp.final),]
list.spp <- unique(srb.2$species)
srb.2$Correct <- TRUE
srb.2
for (i in 1:length(list.spp)){ 
  cat(i,"--","Plotting data for", list.spp[i],"\n")
  iden <- which(srb.2$species == list.spp[i])
  lon.lim <- range(srb.2$decimalLongitude[iden])+c(-10,10)
  lat.lim <- range(srb.2$decimalLatitude[iden])+c(-10,10)
  plot(srb.2$decimalLongitude[iden],srb.2$decimalLatitude[iden],xlim=lon.lim,ylim=lat.lim,
       lwd=0.2,pch=19,col=rgb(0,0,0,0.8),cex=0.6)
  maps::map("world",interior=F,lwd=0.3,add=T)
  points(srb.2$decimalLongitude[iden],srb.2$decimalLatitude[iden],lwd=0.2,pch=19,col=rgb(0,0,0,0.8),cex=0.6)
  title(list.spp[i])
  yn <- readline("Is distribution correct (y/n)?")
  if(toupper(yn)=="Y") {cat("Correct distribution!","\n"); next}
  else {
    cat("Define polygon for selecting points","\n")
    xys <- locator()
    xys_poli <- Orcs::coords2Polygons(cbind(xys$x,xys$y),ID="poli")
    sp::plot(xys_poli,add=T)
    iden2 <- sp::point.in.polygon(srb.2$decimalLongitude[iden],srb.2$decimalLatitude[iden],xys$x,xys$y)
    srb.2$Correct[iden][which(iden2==1)] <- FALSE
    points(srb.2$decimalLongitude[iden][srb.2$Correct[iden]],srb.2$decimalLatitude[iden][srb.2$Correct[iden]],pch=19,cex=0.6,col=rgb(1,0,0,0.6))
    legend("bottom",inset=0.02,legend=c("Correct","Suspect"),pch=19,col=c("red","black"))
  }
  again <- readline("Press ENTER")
}
save(srb.2,file="data/Distribution_corrected.Rdata")

maps::map("world",interior = F)
points(srb.2$decimalLongitude,srb.2$decimalLatitude,pch=19,col=rgb(0,0,0,0.5),cex=0.5)
id <- which(srb.2$Correct==FALSE)
points(srb.2$decimalLongitude[id],srb.2$decimalLatitude[id],pch=19,col=rgb(1,0,0,0.5),cex=0.5)


### Score species (eigth areas)
setwd("~/Documents/1.PROYECTOS/4.TREE_FERNS/TREE_FERNS_v4/ARE/")
library(data.table)
library(sp)
library(maps)
library(mapview)
library(tidyverse)
load(file="data/dists/Distribution_corrected_1.Rdata")
sort(unique(p_clean$species))
taxa <- fread("data/Master_taxa.tsv")
dim(taxa)
table(srb.2$Correct)
srb.2 <- srb.2[srb.2$Correct,]
srb.2 <- p_clean
n_areas  = 8
areas <- as.data.frame(matrix(data=0,nrow = dim(taxa)[1],ncol = n_areas))
rownames(areas) <- taxa$taxon
colnames(areas) <- 1:8
srb.2$Area <- NA
list.spp <- sort(unique(srb.2$species))
length(list.spp)
for (i in 1:length(list.spp)){ 
  cat(i,"--","Plotting data for", list.spp[i],"\n")
  iden <- which(srb.2$species == list.spp[i])
  lon.lim <- range(srb.2$decimalLongitude[iden])+c(-10,10)
  lat.lim <- range(srb.2$decimalLatitude[iden])+c(-10,10)
  plot(srb.2$decimalLongitude[iden],srb.2$decimalLatitude[iden],xlim=lon.lim,ylim=lat.lim,
       lwd=0.2,pch=19,col=rgb(0,0,0,0.8),cex=0.6)
  maps::map("world",interior=F,lwd=0.3,add=T)
  yn <- readline("Which area (1-8)?")
  srb.2$Area[iden] <- yn
  yn <- strsplit(yn,"")[[1]]
  areas[which(rownames(areas)==list.spp[i]),which(colnames(areas) %in% yn)] <- 1
}

idd <- which(rowSums(areas)==0)
for (i in 1:length(idd)){ 
  cat(i,"--","Manual entry for", rownames(areas)[idd][i],"\n")
  iden <- which(srb.2$species == rownames(areas)[idd][i])
  if(length(iden)==0) next
  lon.lim <- range(srb.2$decimalLongitude[iden])+c(-10,10)
  lat.lim <- range(srb.2$decimalLatitude[iden])+c(-10,10)
  plot(srb.2$decimalLongitude[iden],srb.2$decimalLatitude[iden],xlim=lon.lim,ylim=lat.lim,
       lwd=0.2,pch=19,col=rgb(0,0,0,0.8),cex=0.6)
  maps::map("world",interior=F,lwd=0.3,add=T)
  yn <- readline("Which area (1-8)?")
  yn <- strsplit(yn,"")[[1]]
  areas[rownames(areas)[idd][i],which(colnames(areas) %in% yn)] <- 1
}

areas <- enframe(apply(areas,MARGIN=1,function(x) paste(x,collapse = "")))


write.table(areas,"data/Templates/New_range_8_new.tsv",quote = F,sep="\t",row.names = F)

### Score niches (test temperature) ####
#### Scoring on 150-tip tree (1 sept 2020)
library(data.table)
library(sp)
library(maps)
library(mapview)
library(tidyverse)
setwd("~/Desktop/Niche_Evol/")
tree <- ape::read.nexus("data/Cyathea.mcc.tre")
tree <- phyloch::drop.tip2(tree,tree$tip.label[c(143,128,95,85,75)])
ape::write.nexus(tree,file="data/Cyathea.mcc.tre")

load(file="data/dists/Distribution_corrected.Rdata")
sort(unique(srb.2$species))
taxa <- fread("data/Cyathea_taxa.tsv")
dim(taxa)
table(srb.2$Correct)
srb.2 <- srb.2[srb.2$Correct,]
gbif <- fread("data/dists/Cyatheales_allGBIF.csv")
names(gbif)
gbif <- gbif[which(!is.na(gbif$decimalLongitude)),]
srb.2 <- rbind(srb.2,cbind(gbif[which(gbif$species=="Sphaeropteris robusta"),c(10,23,22)],cellID=NA,Correct=TRUE,Area=NA))
srb.2$species <- sub("Sphaeropteris robusta","Sphaeropteris_robusta",srb.2$species)
cyathea <- fread("data/dists/Cyatheaceae_dists.csv")
sinos <- c("Cyathea_bellisquamata","Cyathea_decrescens")
okas <- cbind(cyathea[which(cyathea$Id %in% sinos),],cellID=NA,Correct=TRUE,Area=NA)
names(okas) <- names(srb.2)
okas$species <- sub("Cyathea_bellisquamata","Alsophila_bellisquamata",okas$species)
okas$species <- sub("Cyathea_decrescens","Alsophila_decrescens",okas$species)
srb.2 <- rbind(srb.2,okas)
taxa$taxon[which(is.na(match(taxa$taxon,unique(srb.2$species))))]
sort(unique(srb.2$species))
gbif$species[which(gbif$species == "Cyathea bicrenata")] <- "Cyathea stipularis"
gbif$Correct = FALSE
list.spp <- c("Cyathea arborea","Cyathea stipularis")
for (i in 1:length(list.spp)){ 
  cat(i,"--","Plotting data for", list.spp[i],"\n")
  iden <- which(gbif$species == list.spp[i])
  lon.lim <- range(gbif$decimalLongitude[iden])+c(-10,10)
  lat.lim <- range(gbif$decimalLatitude[iden])+c(-10,10)
  plot(gbif$decimalLongitude[iden],gbif$decimalLatitude[iden],xlim=lon.lim,ylim=lat.lim,
       lwd=0.2,pch=19,col=rgb(0,0,0,0.8),cex=0.6)
  maps::map("world",interior=F,lwd=0.3,add=T)
  title(list.spp[i])
  yn <- readline("Is distribution correct (y/n)?")
  if(toupper(yn)=="Y") {cat("Correct distribution!","\n"); next}
  else {
    cat("Define polygon for selecting points","\n")
    xys <- locator()
    xys_poli <- Orcs::coords2Polygons(cbind(xys$x,xys$y),ID="poli")
    sp::plot(xys_poli,add=T)
    iden2 <- sp::point.in.polygon(gbif$decimalLongitude[iden],gbif$decimalLatitude[iden],xys$x,xys$y)
    gbif$Correct[iden][which(iden2==1)] <- TRUE
    points(gbif$decimalLongitude[iden][gbif$Correct[iden]],gbif$decimalLatitude[iden][gbif$Correct[iden]],pch=19,cex=0.6,col=rgb(1,0,0,0.6))
    legend("bottom",inset=0.02,legend=c("Correct","Suspect"),pch=19,col=c("red","black"))
    }
}
srb.2 <- rbind(srb.2,cbind(gbif[gbif$Correct,c(10,23,22)],cellID=NA,Correct=TRUE,Area=NA))
srb.2$species <- sub("Cyathea arborea","Cyathea_arborea",srb.2$species)
srb.2$species <- sub("Cyathea stipularis","Cyathea_stipularis",srb.2$species)
taxa$taxon[which(is.na(match(taxa$taxon,unique(srb.2$species))))]
sort(unique(srb.2$species))

n_niches  = 5
bio <- raster::raster("~/Downloads/wc2.1_2.5m_elev.tif")
bio
bio.pts <- raster::extract(bio,srb.2[,2:3])
srb.2 <- cbind(srb.2,Alt=bio.pts)
srb.2[which(srb.2$species=="Sphaeropteris_excelsa"),"Alt"] <- 800
srb.2[which(srb.2$species=="Dicksonia_arborescens"),"Alt"] <- 700
tapply(srb.2$Alt,srb.2$species,range,na.rm=T)
srb_summary <- srb.2 %>% group_by(.,species) %>% summarise(.,min=min(Alt,na.rm=T),max=max(Alt,na.rm=T))
srb_summary
srb_summary$min[which(srb_summary$min <= 0)] <- 1
alt <- seq(min(srb_summary$min),max(srb_summary$max),1)
alt.intervals <- cut(alt,c(0,880,1720,2640,3520,max(srb_summary$max)),labels=c(LETTERS[1:5]))
min.interval <- srb_summary[,2] %>% as_vector(.) %>% 
alt.intervals[.]
max.interval <- srb_summary[,3] %>% as_vector(.) %>% 
  alt.intervals[.]

srb_summary <- bind_cols(srb_summary,low=min.interval,high=max.interval)
rangos <- list()
for (i in 1:dim(srb_summary)[1]){ 
ui <- LETTERS[as.numeric(paste(srb_summary[i,4:5]))] %>% 
  match(.,LETTERS)
rangos[[i]] <- seq(ui[1],ui[2],1) %>% LETTERS[.] %>% paste(.,collapse = "")
}
rangos <- unlist(rangos)
srb_summary <- bind_cols(srb_summary,ranges=rangos)

srb_summary
niches <- as.data.frame(matrix(data = 0,nrow = dim(srb_summary)[1],ncol = n_niches))
srb_summary <- bind_cols(srb_summary,niches)
srb_summary
for (i in 1:n_niches){
iden <- grep(LETTERS[i],srb_summary$ranges)
srb_summary[,6+i][iden,] <- 1
}
srb_summary[which(rowSums(srb_summary[,7:ncol(srb_summary)])>=4),]
j = 1
srb_summary$species[which(rowSums(srb_summary[,7:ncol(srb_summary)])>=4)[j]]
correction = c(0,1,1,NA,0)
srb_summary[which(rowSums(srb_summary[,7:ncol(srb_summary)])>=4)[j],7:11] <- as.list(correction)

output <- bind_cols(srb_summary$species,apply(srb_summary[,7:ncol(srb_summary)],1,paste,collapse=""))
write.table(output,"data/Niche.tsv",quote = F,sep="\t",row.names = F)







#### SAMPLED ANCESTOR RESOLVER FROM TRACE FILE PRODUCED BY REVBAYES ####
#### The code is still very rough...... but it does the trick       ####
#### This resolves sample ancestors on every single tree from the   ####
#### posterior. A burn-in fraction can be specified (default zero). ####
resolve_sampancestors <- function(trace_file = here("output_data/MCC_Master.tre"),taxa_file = here("rb_out/data/Master_taxa.tsv"),output = here("output_data/MCC_Master_Resolved.tre"),branch_length = 0.00001,burn.in=0){
  a <- data.table::fread(trace_file,header = T,sep = "\t")
  f_burn = burn.in * dim(a)[1]
  if (burn.in == 0) f_burn = 1
  a <-a[f_burn:dim(a)[1],]
  b <- read.table(taxa_file,header=T)
  taxa <-  as.character(b$taxon)
  # Identifying which taxa are sampled ancestors in each tree in trace
  res <- list()
  cat("Identifying sampled ancestors on",dim(a)[1],"trees","\n")
  res <- lapply(a$fbd_tree,function(j) unlist(lapply(taxa,function(x) length(grep(paste("\\)",x,sep=""),j)))))
  taxas <- lapply(res,function(x) cbind(as.character(taxa),x))
  cat("        ---- Indexing","\n")
  index <- lapply(a$fbd_tree,function(x) strsplit(x,"&index="))
  index_2 <- sapply(index,function(y)sapply(y,function(x) sub(']:.*', '', x)))
  max_index <- unlist(lapply(index_2,function(x) max(as.numeric(x[-1]))))
  indices <- lapply(index_2, function(x) setdiff(1:unique(max_index), as.numeric(x[-1,])))
  
  # Edit the tree string to resolve sampled ancestors for each tree in trace
  counter <- length(taxas)
  cat("Resolving sampled ancestors","\n")
  for (k in 1:length(taxas)){
    counter <- counter - 1
    cat("Remaining trees to process:",counter,"\r")
    taxa <- taxas[[k]]
    if(length(which(taxa[,2]==1)) == 0) next
    taxa <- taxa[which(taxa[,2]==1),]
    if (is.null(dim(taxa)[1])){ 
      grep(taxa[1],a$fbd_tree[k],fixed=T)
      yy <- unlist(strsplit(a$fbd_tree[k],"]:"))
      zz <- yy[grep(paste(taxa[1],"[&",sep=""),yy,fixed=T)]
      zzz <- sub(")",",",zz)
      aa <- sub(paste('.*',taxa[1],sep=""),"",zzz)
      #if(length(paste(")",taxa[1],aa,"]",sep=""))>1) break ## probably not necessary (but I got a weird warning at some point)
      #if(paste(",",taxa[1],aa,"]:",branch_length,")",sep="")>1) break ## probably not necessary (but I got a weird warning at some point)
      a$fbd_tree[k] <- sub(paste(")",taxa[1],aa,"]",sep=""), paste(",",taxa[1],aa,"]:",branch_length,")","[&index=",indices[[k]][1],"]",sep=""),  a$fbd_tree[k],fixed=T)
    } else {
      dimas = dim(taxa)[1]
      for (i in 1:dimas){ 
        grep(taxa[i,1],a$fbd_tree[k],fixed=T)
        yy <- unlist(strsplit(a$fbd_tree[k],"]:"))
        zz <- yy[grep(paste(taxa[i,1],"[&",sep=""),yy,fixed=T)]
        zzz <- sub(")",",",zz)
        aa <- sub(paste('.*',taxa[i,1],sep=""),"",zzz)
        #if(length(paste(")",taxa[i,1],aa,"]",sep=""))>1) break ## probably not necessary (but I got a weird warning at some point)
        #if(paste(",",taxa[i,1],aa,"]:",branch_length,")",sep="")>1) break ## probably not necessary (but I got a weird warning at some point)
        a$fbd_tree[k] <- sub(paste(")",taxa[i,1],aa,"]",sep=""), paste(",",taxa[i,1],aa,"]:",branch_length,")","[&index=",indices[[k]][i],"]",sep=""),  a$fbd_tree[k],fixed=T)
        #sub(paste(")",taxa[i,1],aa,"]",sep=""), paste(",",taxa[i,1],aa,"]:",branch_length,")","[&index=",indices[[k]][i],"]",sep=""), a$fbd_tree[k],fixed=T)
        
      }
    }
    
  }
  write.table(a,file=output,quote = F,col.names = T,row.names = F,sep = "\t")
}
#### This is executed prior to the ASE analyses on the full data set ####
#### For the pruned trees, this is unnecessary.                      ####

#### Filter trees from posterior sample that match the family-level backbone topology from the RAxML phylogeny
#### This is necessary  to sample trees from the posterior during the ASE analyses: trees with a weird topology are favored during the reconstruction, which is not correct ####
filter_trees <- function(target_tree="output_data/MCC_Master_Pruned.tre",trees="output_data/Master_Resolved.trees",output="output_data/Master_Resolved_Const.trees"){ 
backbone <- ape::read.nexus(here(target_tree))
iden <- which(backbone$tip.label %in% c("Plagiogyria_stenoptera",
    "Culcita_coniifolia","Loxsoma_cunninghamii" ,"Cibotium_regale" ,
    "Dicksonia_gigantea","Sphaeropteris_glauca", "Thyrsopteris_elegans", "Metaxya_rostrata"))
backbone.p <- drop.tip(backbone,tip =backbone$tip.label[-iden] )
comparison <- list()
a <- data.table::fread(here(trees),header = T,sep = "\t")
burn.in = 0
f_burn = burn.in * dim(a)[1]
if (burn.in == 0) f_burn = 1
a <-a[f_burn:dim(a)[1],]
tipas <- c("Plagiogyria_stenoptera","Culcita_coniifolia","Loxsoma_cunninghamii" ,
  "Cibotium_regale" ,"Dicksonia_gigantea","Sphaeropteris_glauca", "Thyrsopteris_elegans", "Metaxya_rostrata")
master_compare <- 1:dim(a)[1] %>% future_map_lgl(function(lis){
  r1 <- a$fbd_tree[lis] %>% ape::read.tree(text = .) 
  r2 <- which(r1$tip.label %in% tipas) 
  r3  <- ape::drop.tip(phy = r1, tip = r1$tip.label[-r2])  %>% all.equal.phylo(backbone.p,.,use.edge.length=FALSE)
  return(r3)
},.progress = TRUE)
master_compare
table(unlist(master_compare))
write.table(a[which(unlist(master_compare)),],file=here(output),quote = F,col.names = T,row.names = F,sep = "\t")
}

##### This is the slow version: june 2020
#comparison<-list()
#for (i  in 1:100){ 
#  cat("processing tree",i,"\r")
#   target <- ape::read.tree(text = a$fbd_tree[i])
#   iden <- which(target$tip.label %in% c("Plagiogyria_stenoptera","Culcita_conniifolia","Loxoma_cunninghamii" ,"Cibotium_regale" ,"Dicksonia_gigantea","Sphaeropteris_glauca", "Thyrsopteris_elegans", "Metaxya_rostrata"))
#   target.p <- ape::drop.tip(target,tip =target$tip.label[-iden] )
#   comparePhylo(backbone.p,target.p,plot=T)
# comparison[[i]] <- all.equal.phylo(backbone.p,target.p,use.edge.length=FALSE) 
#}
#unlist(comparison)




#### MAKE JOINT FILES FROM  MULTIPLE RUNS OF ASE ####

setwd("~/Documents/6.SCRIPTS/RevBayes_TrialRun/output/")
a <- data.table::fread("my_run.1.no_biome_run_1.trees",header = T,sep = "\t")
burn <- max(a$Iteration) * 0.8
a <- a[-c(1:which(a$Iteration==burn)),]
b <- data.table::fread("my_run.1.no_biome_run_2.trees",header = T,sep = "\t")
burn <- max(b$Iteration) *0.8
b <- b[-c(1:which(b$Iteration==burn)),]
joint <- rbind(a,b)
dim(joint)
joint$Iteration <- seq(10,dim(a)[1]*2*10,10)
write.table(joint,file="my_run.joint.no_biome.trees",quote = F,col.names = T,row.names = F,sep = "\t")

a <- data.table::fread("my_run.1.no_biome.bg.states_run_1.txt",header = T,sep = "\t")
burn <- max(a$Iteration) * 0.5
a <- a[-c(1:which(a$Iteration==burn)),]
b <- data.table::fread("my_run.1.no_biome.bg.states_run_2.txt",header = T,sep = "\t")
burn <- max(b$Iteration) * 0.5
b <- b[-c(1:which(b$Iteration==burn)),]
joint <- rbind(a,b)
dim(joint)
joint$Iteration <- seq(10,dim(a)[1]*2*10,10)
write.table(joint,file="my_run.joint.no_biome.bg.states.txt",quote = F,col.names = T,row.names = F,sep = "\t")

a <- data.table::fread("my_run.1.no_biome.bg.stoch_map_run_1.txt",header = T,sep = "\t")
burn <- max(a$Iteration) * 0.5
a <- a[-c(1:which(a$Iteration==burn)),]
b <- data.table::fread("my_run.1.no_biome.bg.stoch_map_run_2.txt",header = T,sep = "\t")
burn <- max(b$Iteration) * 0.5
b <- b[-c(1:which(b$Iteration==burn)),]
joint <- rbind(a,b)
dim(joint)
joint$Iteration <- seq(10,dim(a)[1]*2*10,10)
write.table(joint,file="my_run.joint.no_biome.bg.stoch_map.txt",quote = F,col.names = T,row.names = F,sep = "\t")
#### This is executed prior to summarizing the ASE results ####

##############
do_ARR_plot( ase.tre = "output_summ/my_run.1.no_biome.bg.ase.tre",states_file="input_data/AreaCodes_n8_2.tsv",
             title = "Cyatheales_ARR_trace" ,output = "plots/ARR_SUMM.pdf" ,tree_layout = "rectangular",
             tip_pie_diameter = 5,node_pie_diameter = 5,save=T,burn.in = 0.0,drop.tips=T)

do_STT_plot(stoch = "output_pruned_summ/my_run.1.no_biome.history.tsv", area.codes = "input_data/AreaCodes_n8_2.tsv",n.areas=8,max.time = 260,burn.in=0.0,support_plot=TRUE,save=TRUE,output = "output_data/HistoryTable_pruned.RData")



ggsave2(plot = p_shld,filename = "plots/ARR_trace_pruned_ASE_july.pdf", height = 10,  width = 10)
ggsave2(plot = p3,filename = "plots/ARR_trace_full_STT_july.pdf", height = 10,  width = 10)
#ggsave2(plot = p_shld / p3,filename = "plots/ARR_trace_full.pdf", height = 10,  width = 10)




################################################################################
#
# @brief Function to plot ancestral states and the associated uncertainty
#        for continuous and discrete characters.
#
#        For discrete characters 'summary_statistic="MAP"' should be used,
#        and for continuous characters 'summary_statistic="mean"'. If the
#        tree and tip labels do not fit in the screen, adjust the visible
#        area using the 'xlim_visible' argument.
#
#        If 'summary_statistic="MAP"', the maximum a posteriori ancestral
#        state will be plotted on each node. The color corresponds to the
#        character state, and the size of the circle represents the posterior
#        probability of that state. Cladogenetic models that estimate 
#        ancestral states for both the beginning and end of each branch
#        are plotted by setting "include_start_states=TRUE".
#
#        Maximum a posteriori ancestral chromosome numbers can be plotted 
#        with 'summary_statistic="MAPChromosome"'. For chromosomes the
#        color represents the posterior probability and the size of the
#        circle represents the chromosome number.
#
#        For 'summary_statistic="mean"' the color represents the size of 
#        the 95% confidence interval, and the size of the cirlce represents
#        represents the mean character state.
#
# @date Last modified: 2016-09-29
# @author Will Freyman
# @version 1.0
# @since 2016-08-31, version 1.0.0
#
# @param    tree_file               character     Path to the ancestral state tree generated by RevBayes.
# @param    summary_statistic       character   The type of summary statistic to plot.
# @param    tree_layout             character   One of 'rectangular', 'slanted', 'fan', 'circular', 'radial', or 'unrooted'.
# @param    include_start_states    logical     Plot start and end ancestral states. Used for cladogenetic models.
#
#
################################################################################

##########

setwd("~/Desktop/RevBayes_TrialRun/")
base_fn = "my_run.1.no_biome.bg"

ot = "output/"
fn      = paste0(ot, base_fn)
out_fp       = paste0( ot, sep="")
plot_fp      = paste0(ot, sep="")

# filenames
phy_fn  = paste0(fn, ".stoch_map.txt")




col_bg_fn    = paste0("data/Templates/","AreaCodes_n8.txt")

plot_fn = paste0(base_fn,".stoch_grid.pdf",sep="")
bg_fn  = paste0(out_fp, "my_run.1.no_biome", ".history.tsv")






#source("vib_div_util.R")
setwd("~/Desktop/RevBayes_TrialRun/")
base_fn = "my_run.1.no_biome.bg"
plot_fp = "Test"
# IO
fn      = paste0("output/", base_fn)
phy_fn  = paste0(fn, ".stoch_map.txt")
col_fn  = paste0("data/Templates/","AreaCodes_n8.txt")
plot_fn = paste0(base_fn,".stoch_grid.pdf",sep="")

# plotting settings
pdf(plot_fn, height=17, width=3.5)
grid = c(4,1)
par(mfrow=grid)

# read data
OMG <- function(col_fn, phy_fn) {
  dat_col = read.table(col_fn,sep="\t",header=T)
  dat_ch  = read.table(phy_fn, sep="\t", header=T)
}

# get vector of stochastically mapped trees
phy = as.vector(dat_ch[,ncol(dat_ch)])

# iterations to sample
iterations = c(260,270,280)
n_it = length(iterations)

# read/plot/append simmap trees
simphy = list()
for (j in 1:n_it) {
  
  # read in individual tree
  n = which(dat_ch[,1]==iterations[j])
  cat("tree",j,":",iterations[j],"->",n,"\n")
  sim2 = read.simmap(text=phy[n])
  sim2 = ladderize.simmap(sim2, right=F)
  sim2$tip.label = rep("",length(sim2$tip.label))
  
  # find relevant colors
  colors = vector()
  for (i in 1:length( sim2$maps ) ) {
    colors = c(colors, names(sim2$maps[[i]]) )
  }
  colors = sort(as.numeric(unique(colors)))
  
  # convert state numbers to state labels    
  colnames(sim2$mapped.edge) = as.vector(dat_col$state_label[as.numeric(colnames(sim2$mapped.edge))])
  for (j in 1:length(sim2$maps)) {
    if (length(sim2$maps[[j]]) > 0) {
      names(sim2$maps[[j]]) = as.vector(dat_col$state_label[as.numeric(names(sim2$maps[[j]]))])
    }
  }
  simphy[[n]] = sim2
  
  # assign state labels to colors
  cols = as.vector(dat_col$state_colors)
  names(cols)=dat_col$state_label
  
  # mar : bottom, left, top, and right    
  root_age = max(branching.times(sim2))
  xlim = c(0,root_age)
  dx = max(branching.times(sim2))
  plotSimmap(sim2, cols, fsize=0.3, lwd=3, xlim=xlim,
             split.vertical=TRUE, direction="rightwards")
  
  
  
  
}

dev.off()



dos <- rgdal::readOGR("~/Desktop/Placas_G/Phanerozoic_EarthByte_Coastlines/reconstructed_190.00Ma.shp")
plot(dos)


  
bds_tree <- do_rates()


edr_times <- rev.process.div.rates(speciation_times_file="rb_out/EDR/output/Cyatheales_EBD_speciation_times.log",speciation_rates_file="rb_out/EDR/output/Cyatheales_EBD_speciation_rates.log",extinction_times_file="rb_out/EDR/output//Cyatheales_EBD_extinction_times.log",extinction_rates_file="rb_out/EDR/output//Cyatheales_EBD_extinction_rates.log",fossilization_times_file="",fossilization_rates_file="",tree = "output_data/MCC_Master_Resolved.tre",maxAge=NULL,burnin=0.25,numIntervals=10)
edr_times
output="plots/EDR_full_RelExt.pdf"
fig.types=c("net-diversification rate","relative-extinction rate")
  type_name <- c("NetDiv","RelExt")
  numIntervals=10
  type = fig.types[1]
  thisOutput <- edr_times[[type]]
  thisOutput %>% t() %>% as_tibble() %>% mutate(Period=1:numIntervals,.before=1) %>% rename_with(~c("Period",paste(type_name[1],1:dim(thisOutput)[1],sep="_"))) -> rate_tib
type = fig.types[2]
  thisOutput <- edr_times[[type]]
  thisOutput %>% t() %>% as_tibble() %>% mutate(Period=1:numIntervals,.before=1) %>% rename_with(~c("Period",paste(type_name[2],1:dim(thisOutput)[1],sep="_"))) %>% left_join(rate_tib,.,by="Period")-> rate_tib

rate_tib %>% pivot_longer(cols=-1,names_to = "Generation", values_to = "Value") %>% separate(Generation,c("Rate","Gen"),sep="_") %>% group_by(Period,Rate) -> to_plot #%>% summarise(Median=median(Value),Upper=quantile(Value,0.95),Lower=quantile(Value,0.05)) 
to_plot %>% mutate(Time=case_when(Period==10~5.33,Period==9~23.03,
                                  Period==8~39,Period==7~56,
                                  Period==6~66,Period==5~100.5,
                                  Period==4~145,Period==3~174.1,
                                  Period==2~201.3,Period==1~251.9),
                   MidPoint=case_when(Period==10 ~ 2.665,Period==9 ~ 14.180,
                                    Period==8~31.015,Period==7~47.5,
                                    Period==6~61,Period==5~83.250,
                                    Period==4~122.750,Period==3~159.550,
                                    Period==2~187.7,Period==1~226.6)) %>% filter(Rate=="RelExt") %>% filter(Gen %in% seq(1,9999,10)) %>% filter(Value > -0.4) -> to_plot
times = (to_plot %>% ungroup() %>% distinct(Time) %>% pull)
times = rev(1:10)
mid=rev(seq(0.5,9.5,1))
#colors <- c(MaizePal::maize_pal("RubyGold")[c(4,3,5,6,2)],MaizePal::maize_pal("OaxacaGreen")[c(2,3)],MaizePal::maize_pal("HopiBlue")[c(5,6)],MaizePal::maize_pal("GlassGem")[2])
colors <- c(viridis::viridis(n = 5,option="B",begin=0.6,direction = -1),
            viridis::viridis(n = 2,option="D",begin=0.6,end=0.9,direction = -1),
            viridis::viridis(n = 2,option="G",begin=0.4,end=0.6,direction = -1),
            viridis::viridis(n = 1,option="C",begin=0.3,direction = 1))

geoscale::timescales$ICS2015 %>% as_tibble() %>% filter(Type=="Epoch") %>% slice(c(3,4,5,6,7,8,9,11,12,15)) %>% mutate(HEX=rgb(Col_R,Col_G,Col_B,maxColorValue=255)) %>% pull(HEX) -> colors

pp <- to_plot %>% 
  ggplot(aes(x=(Period),y=Value)) + theme(panel.background = element_blank(),panel.grid=element_blank(),axis.line.x=element_line(),legend.position="",axis.text.y = element_blank(),axis.ticks.y=element_blank()) + # scale_x_discrete(labels=rev(times)) + 
  geom_hline(yintercept = 0,color="grey50") + 
  labs(x="",y="Net diversification rate") +
  annotate(geom="rect",ymin=-0.25,ymax=-.4,xmin=10.50,xmax=mid[1],fill=adjustcolor(colors[1],alpha.f = 0.7),color="black",size=0.2) + 
  annotate(geom="rect",ymin=-0.25,ymax=-.4,xmin=mid[1],xmax=mid[2],fill=adjustcolor(colors[2],alpha.f = 0.7),color="black",size=0.2) + 
  annotate(geom="rect",ymin=-0.25,ymax=-.4,xmin=mid[2],xmax=mid[3],fill=adjustcolor(colors[3],alpha.f = 0.7),color="black",size=0.2) +
  annotate(geom="rect",ymin=-0.25,ymax=-.4,xmin=mid[3],xmax=mid[4],fill=adjustcolor(colors[4],alpha.f = 0.7),color="black",size=0.2) +
  annotate(geom="rect",ymin=-0.25,ymax=-.4,xmin=mid[4],xmax=mid[5],fill=adjustcolor(colors[5],alpha.f = 0.7),color="black",size=0.2) +
  annotate(geom="rect",ymin=-0.25,ymax=-.4,xmin=mid[5],xmax=mid[6],fill=adjustcolor(colors[6],alpha.f = 0.7),color="black",size=0.2) +
  annotate(geom="rect",ymin=-0.25,ymax=-.4,xmin=mid[6],xmax=mid[7],fill=adjustcolor(colors[7],alpha.f = 0.7),color="black",size=0.2) +
  annotate(geom="rect",ymin=-0.25,ymax=-.4,xmin=mid[7],xmax=mid[8],fill=adjustcolor(colors[8],alpha.f = 0.7),color="black",size=0.2) +
  annotate(geom="rect",ymin=-0.25,ymax=-.4,xmin=mid[8],xmax=mid[9],fill=adjustcolor(colors[9],alpha.f = 0.7),color="black",size=0.2) +
  annotate(geom="rect",ymin=-0.25,ymax=-.4,xmin=mid[9],xmax=mid[10],fill=adjustcolor(colors[10],alpha.f = 0.7),color="black",size=0.2) +
  geom_jitter(aes(alpha=0.3,fill=factor(Period)),shape=21,size=0.8,stroke=0.1,width = 0.1) + 
  #stat_summary(aes(color=factor(Period)),geom="crossbar",fun="median")+
  ggdist::stat_halfeye(aes(fill=factor(Period)),point_colour = NA,adjust = .9, width = .4, .width = 0, alpha=.9,slab_colour = "black", slab_size = .1,justification = -.5, height = 2,normalize="all") + 
  #scale_fill_viridis_b(n.breaks=10,option="G",begin=0.2) + 
  scale_fill_manual(values=rev(colors)) +
  scale_color_manual(values=rev(colors)) +
  #scale_x_continuous(expand = c(0, 0)) + 
  scale_y_continuous(expand = c(0, 0)) + coord_flip() +
  annotate(geom="text",x = times, y = -0.39,hjust = 0, label = (c("Pliocene/Quaternary","Miocene","Oligocene","Eocene","Paleocene","Late Cretaceous","Early Cretaceous","Late/Middle Jurassic","Early Jurassic","Triassic")),fontface="bold",angle = 0,size=3)  + NULL

pp_2 <- wrap_plots(list(pp)) + plot_layout(byrow=T,nrow = 1,ncol=1,guides = "keep",tag_level = 'new') + plot_annotation(title = "Diversification rates in tree ferns (Cyatheales)",subtitle="EBD Model", caption= "Ramírez-Barahona") & theme(legend.position="")

ggsave(here(output), width=15, height=15, units="cm",plot=pp_2)


bds_tree <- do_rates(tree_file = "output_data/MCC_Master_Resolved.tre",branch_rates_file = "output_data/Master_BDS_Resolved.log")
bds_tree_prune <- do_rates(tree_file = "output_data/MCC_Master_Pruned.tre",branch_rates_file = "output_data/Master_BDS_pruned.log")
bds_tree[[2]] -> full_tib
bds_tree_prune[[2]] -> prune_tib
prune_tib %>% filter(!is.na(label)) %>% mutate(rates_prune=rates) %>% select(label,rates_prune) %>%  left_join(.,full_tib %>% filter(!is.na(label)),by="label") %>% #mutate(rates= (rates - min(rates)) / (max(rates)-min(rates)) ,rates_prune= (rates_prune - min(rates_prune)) / (max(rates_prune)-min(rates_prune)) ) %>% 
#ggplot(aes(x=(rates_prune),y=(rates),color=rates_prune)) + geom_point() + geom_abline(slope=1,intercept = 0) + scale_color_viridis_c(option="F")
ggplot(aes(x=(rates),color=rates_prune)) + geom_histogram() + scale_color_viridis_c(option="F")

bds_tree[[1]]
bds_tree_prune[[1]]


data_tree <- read.beast("output_data/MCC_Master_Pruned.tre")@data
tree <- read.nexus("output_data/MCC_Master_Pruned.tre")
spp_pair = read.table("input_data/For_ages.txt",sep=",",header=TRUE)
age_tib <- tibble(Crown_Median=NA,Crown_Inferior=NA,Crown_Superior=NA,Crown_posterior=NA,Stem_Median=NA,Stem_Inferior=NA,Stem_Superior=NA,Stem_posterior=NA)

for (i in 1:nrow(spp_pair)){ 
  cat(i,"\n")
if(!spp_pair[i,2] == spp_pair[i,3]) { 
crown_node <- getMRCA(tree,tip=as_vector(spp_pair[i,2:3]))
stem_node <- parent(tree,crown_node)
inter_tib <- data_tree %>% filter(node==crown_node) %>% unnest(age_0.95_HPD) %>% mutate(HPD = c("Inferior","Superior")) %>%  pivot_wider(values_from = age_0.95_HPD,names_from = HPD) %>% select(4,5,2) %>% mutate(Median = branching.times(tree)[paste(crown_node)],.before=1) %>% rename_all(.,~paste0("Crown_",.x))

inter_tib <- data_tree %>% filter(node==stem_node) %>% unnest(age_0.95_HPD) %>% mutate(HPD = c("Inferior","Superior")) %>%  pivot_wider(values_from = age_0.95_HPD,names_from = HPD) %>% select(4,5,2) %>% mutate(Median = branching.times(tree)[paste(stem_node)],.before=1) %>% 
  rename_all(.,~paste0("Stem_",.x)) %>% bind_cols(inter_tib,.)
age_tib <- bind_rows(age_tib,inter_tib)
} 
  if(spp_pair[i,2] == spp_pair[i,3]) {
stem_node <- getMRCA(tree,tip=as_vector(spp_pair[i,2:3]))
inter_tib <- tibble(Crown_Median=NA,Crown_Inferior=NA,Crown_Superior=NA,Crown_posterior=NA)
inter_tib <- data_tree %>% filter(node==stem_node) %>% unnest(age_0.95_HPD) %>% mutate(HPD = c("Inferior","Superior")) %>%  pivot_wider(values_from = age_0.95_HPD,names_from = HPD) %>% select(4,5,2) %>% mutate(Median = branching.times(tree)[paste(stem_node)],.before=1) %>% 
  rename_all(.,~paste0("Stem_",.x)) %>% bind_cols(inter_tib,.)
age_tib <- bind_rows(age_tib,inter_tib)

  }
}
age_tib %>% slice(-1) %>% mutate(Lineage = spp_pair$Taxa,.before=1) %>% 
  mutate(across(where(is.numeric),round,2)) %>% flextable::flextable(.) %>% flextable::save_as_docx(path="../PAPER/Supplementary_Data/Supplementary_tableS1.docx")

