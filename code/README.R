library(ggridges)
library(ggdist)
library(showtext)
library("rnaturalearth")
library("rnaturalearthdata")
font_add_google(name="EB Garamond")
showtext_auto()

bg_fn  = "output_pruned_summ/my_run.1.no_biome.history.tsv"
col_bg_fn    = "input_data/AreaCodes_n8_2.tsv"
bg_colors    = read.table(col_bg_fn, header=T,sep="\t")
stoch_bg     = read.csv(bg_fn, sep="\t", stringsAsFactors=F)
iterations = unique(stoch_bg$iteration)
n_iter     = length(iterations)
stoch_bg %>% as_tibble() %>% filter(transition_type=="anagenetic") %>% filter(start_state%in%c(1:8)) %>% #filter(transition_time>0) %>% 
    mutate(From = ifelse(start_state%in%c(2,3,5,7,8),"Gondwana","Laurasia"),.before=node_index) %>% 
    mutate(To = ifelse(end_state%in%c(11,16,17,25,26,28,31,32,34,36),"Gondwana",
                       ifelse(end_state%in%c(12,19,22),"Laurasia","Inter")),.after=From) %>% 
    mutate(From_To = paste0(From,"_",To),.after=To) %>% 
    mutate(Route = ifelse(From_To=="Gondwana_Gondwana","Gondwana_Gondwana",
                          ifelse(From_To == "Laurasia_Laurasia","Laurasia_Laurasia",
                                 ifelse(From_To == "Laurasia_Inter", "Laurasia_Gondwana",
                                        ifelse(From_To == "Gondwana_Inter","Gondwana_Laurasia",NA))))) %>% 
    mutate(Route=factor(Route,levels=c("Laurasia_Laurasia","Laurasia_Gondwana","Gondwana_Gondwana","Gondwana_Laurasia"))) %>% mutate(time_cats = cut(transition_time,breaks=seq(0,220,10),labels=seq(0,210,10))) %>% mutate(time_cats = as.numeric(as.character(time_cats))) -> dispersals
dispersals %>% group_by(Route,time_cats) %>% summarise(n=n()*1/n_iter) %>% ungroup() %>% 
    filter(n>=1)
dispersals %>% group_by(Route,time_cats) %>% summarise(n=n()*1/n_iter) %>% ungroup() %>% 
    #filter(n>=1) %>% 
    mutate(n = ifelse(grepl("Laurasia_",Route),n,-n)) %>% 
    ggplot(aes(x=time_cats,y=n, fill = Route)) +
    #geom_density_ridges(stat="binline",scale=0.9) +
    #stat_histinterval(scale=0.9,) +
    geom_col(position="stack") +
    theme(legend.position = "right",panel.background = element_blank(),panel.grid = element_blank(),axis.line = element_line(),legend.background = element_rect(fill = NA),legend.spacing.y = unit(c(0.2),"cm"),legend.title = element_text(family="EB Garamond"),legend.text = element_text(family="EB Garamond"),axis.title.y = element_blank(),axis.title.x = element_text(family="EB Garamond",size=12),axis.text =  element_text(family="EB Garamond",size = 12),axis.text.x =  element_text(family="EB Garamond",size = 10),legend.margin=margin(t=-25),legend.key.size = unit(0.6,"cm"),plot.title = element_text(family="EB Garamond",face="bold",size=18,hjust = 0.5),plot.subtitle = element_text(family="EB Garamond",size=12,hjust = 0.5),legend.direction="vertical") +
    scale_x_reverse(breaks=c(201,145,66,23,0)) +
    #scale_y_continuous(trans="log2",breaks=c(1,4,12,24)) +
    coord_flip(xlim=c(220,0),ylim=c(-25,25)) +
    labs(x="Transition Time (Mya)") +
    geom_hline(yintercept = 0) +
    geom_vline(xintercept = 0) +
    #scale_fill_manual(values = c("#E76254","#1E466E")) +
    #facet_wrap(~Route,ncol=1) +
    NULL

stoch_bg %>% as_tibble() %>% filter(transition_type=="anagenetic") %>% filter(end_state%in%c(1:8)) %>% #filter(transition_time>0) %>%
    mutate(After =  ifelse(end_state%in%c(2,3,5,7,8),"Gondwana","Laurasia"),.before=node_index) %>% 
    mutate(Before = ifelse(start_state %in% c(9,10,13:15,18,20,21,23,24,27,29,30,33,35),"Inter",After),.before=node_index) %>% 
    mutate(Event = paste0(After,"_",Before),.after=Before) %>% 
    mutate(Extirpation = ifelse(Event%in%c("Gondwana_Inter","Laurasia_Laurasia"),"Laurasian","Gondwanan"),.after=Event) %>% mutate(time_cats = cut(transition_time,breaks=seq(0,220,10),labels=seq(0,210,10))) %>% 
    mutate(time_cats = as.numeric(as.character(time_cats))) -> extirps
extirps %>% group_by(Extirpation,time_cats) %>% summarise(n=n()*1/n_iter) %>% ungroup()
extirps %>% group_by(Extirpation,time_cats) %>% summarise(n=n()*1/n_iter) %>% ungroup() %>% 
    ggplot(aes(x=time_cats,y=n,fill=Extirpation)) +
    geom_col(position="dodge") +
    #stat_dots() +
    #geom_density_ridges(stat="binline",scale=0.9) +
    #stat_histinterval(breaks=20,scale=0.9,expand=TRUE) +
    theme(legend.position = "none",
          panel.background = element_blank(),
          panel.grid = element_blank(),
          axis.line = element_line(),
          legend.background = element_rect(fill = NA),
          legend.spacing.y = unit(c(0.2),"cm"),
          legend.title = element_text(family="EB Garamond"),
          legend.text = element_text(family="EB Garamond"),
          axis.title.y = element_blank(),
          axis.title.x = element_text(family="EB Garamond",size=12),
          axis.text =  element_text(family="EB Garamond",size = 12),
          axis.text.x =  element_text(family="EB Garamond",size = 10),
          legend.margin=margin(t=-25),
          legend.key.size = unit(0.6,"cm"),
          plot.title = element_text(family="EB Garamond",face="bold",size=18,hjust = 0.5),plot.subtitle = element_text(family="EB Garamond",size=12,hjust = 0.5),legend.direction="vertical") +
    scale_x_reverse() +
    #scale_y_discrete(limits=rev) +
    labs(x="Transition Time (Mya)") +
    scale_fill_manual(values = c("#1E466E","#E76254")) +
    facet_wrap(~Extirpation,ncol=1) +
    NULL

do_network_map <- function(bg_fn= "output_summ/my_run.1.no_biome.history.tsv",
col_bg_fn    = "input_data/AreaCodes_n8_2.tsv",n_areas  = 8,time_slice=NULL,max_min="min",f_burn = 0.0,
type_trans = c("anagenetic"),bin_width  = 1){
bg_colors    = read.table(col_bg_fn, header=T,sep="\t")
stoch_bg     = read.csv(bg_fn, sep="\t", stringsAsFactors=F)
n_states = n_areas
bg_lbl = bg_colors$state_label[1:n_areas]
from_state_lbl = paste("from",bg_lbl,sep="_")
to_state_lbl = paste("to",bg_lbl,sep="_")
stoch_bg$transition_time[ stoch_bg$transition_type=="no_change" ] = stoch_bg$branch_start_time[ stoch_bg$transition_type=="no_change" ]
if(!is.null(time_slice)) {
    if(max_min=="min") stoch_bg = stoch_bg[ stoch_bg$transition_time > time_slice, ] else stoch_bg = stoch_bg[ stoch_bg$transition_time < time_slice, ]}
# filter files for number of samples
thinby     = 1
iterations = unique(stoch_bg$iteration)
n_burn     = max(1, f_burn*length(iterations))
n_iter     = length(iterations)

#### Stage 1: merge list of biome and biogeo events into one structure ##
#stoch_bg_biome = make_stoch_bg_biome(sample_bg, sample_biome, iterations)

#### Stage 2: reformat event list as LSTT matrix ##
#mean_event = make_bg_biome_tx(stoch_bg, 1, iterations, bg_lbl)
n_states = length(bg_lbl)
n_it = length(iterations)

state_bins = array(0, dim=c(n_states, n_states, n_iter))
hit_bins   = array(0, dim=c(n_states, n_states, n_iter))
state_bins_ext = array(0, dim=c(n_states, n_states, n_iter))
hit_bins_ext   = array(0, dim=c(n_states, n_states, n_iter))
curr_it = -1
for (i in 1:nrow(stoch_bg)) {
        cat(i,"\r")
        if (curr_it != stoch_bg$iteration[i]) {
            curr_it = stoch_bg$iteration[i]
            #cat("Stage 2, processing iteration ",curr_it," / ", max(stoch_bg$iteration), "\n", sep="")
        }
        it_idx = match(curr_it, iterations)
        
        if (stoch_bg$transition_type[i] %in% type_trans) {
            
            from_bg_idx      = get_bg_state_2( stoch_bg$start_state[i] )
            to_bg_idx        = get_bg_state_2( stoch_bg$end_state[i] )
            ret = matrix(NA, nrow=0, ncol=2)
            ret_ex = matrix(NA, nrow=0, ncol=2)
                is_bg_event = !identical(from_bg_idx, to_bg_idx)
                is_dispersal = FALSE
                if (is_bg_event && (length(from_bg_idx) < length(to_bg_idx))) {
                    is_dispersal = TRUE
                }
                is_extirpation = FALSE
                if (is_bg_event && (length(from_bg_idx) > length(to_bg_idx))) {
                    is_extirpation = TRUE
                }
                
                for (from_bg in from_bg_idx) {
                    for (to_bg in to_bg_idx) {
                        if (is_dispersal && from_bg != to_bg) {
                                    from_state = from_bg
                                    to_state = to_bg
                                    ret = rbind(ret, c(from_state, to_state))
                        }
                    }
                }
                for (from_bg in from_bg_idx) {
                    for (to_bg in to_bg_idx) {
                        if (is_extirpation && from_bg != to_bg) {
                            from_state = to_bg
                            to_state = from_bg
                            ret_ex = rbind(ret, c(from_state, to_state))
                        }
                        
                            }
                       # }
                   # }
                }
                colnames(ret) = c("from","to")
                colnames(ret_ex) = c("from","to")
                state_tx = ret
            # store each event's info as count or hit
            if (nrow(state_tx) > 0) {
                for (i in 1:nrow(state_tx)) {
                    state_bins[ state_tx[i,1], state_tx[i,2], it_idx ] = state_bins[ state_tx[i,1], state_tx[i,2], it_idx ] + 1
                    hit_bins[ state_tx[i,1], state_tx[i,2], it_idx ] = 1
                }
            }
                state_tx_ext = ret_ex
                if (nrow(state_tx_ext) > 0) {
                    for (i in 1:nrow(state_tx_ext)) {
                        state_bins_ext[ state_tx_ext[i,1], state_tx_ext[i,2], it_idx ] = state_bins_ext[ state_tx_ext[i,1], state_tx_ext[i,2], it_idx ] + 1
                        hit_bins_ext[ state_tx_ext[i,1], state_tx_ext[i,2], it_idx ] = 1
                    }
            }
       
    }
    
    }
 # validate bins don't include self transitions
    check_bin_diag_zero(state_bins)
    check_bin_diag_zero(state_bins_ext)
    # convert from posterior samples
    #state_bins = state_bins_ext
    mean_event = rowSums(state_bins,dims=2) * (1/n_iter)
    prob_event = rowSums(hit_bins,dims=2) * (1/n_iter)
    # format & filter
    rownames(mean_event)=bg_lbl; colnames(mean_event)=bg_lbl
    rownames(prob_event)=bg_lbl; colnames(prob_event)=bg_lbl
#### Define and collect classes of event counts
class_vals = c(1,2, 4, 8,12,16)
class_lbls = paste0(">", class_vals)
mean_event = round(mean_event, digits=2)
class_event = mean_event
class_event[ mean_event < class_vals[1] ] = 0
for (i in 1:length(class_vals)) {
    class_event[ mean_event >= class_vals[i] ] = i
}

### Reformat data
m_mean = reshape2::melt(mean_event) %>% 
    bind_cols(.,reshape2::melt(prob_event) %>% select(value) %>% rename("prob"=value))
dat_plot = m_mean
colnames(dat_plot) = c("From_State","To_State", "Mean", "Prob")
#dat_plot$From_Biome  = sapply( as.vector(dat_plot$From_State), function(x) { strsplit(x, "\\+")[[1]][1] })
dat_plot$From_Area   = as.vector(dat_plot$From_State)
#dat_plot$To_Biome    = sapply( as.vector(dat_plot$To_State),   function(x) { strsplit(x, "\\+")[[1]][1] })
dat_plot$To_Area     = as.vector(dat_plot$To_State)
cc                   = make_count_class(x=dat_plot$Mean, p=class_vals)
dat_plot$Count_Class = factor( cc, ordered=T, levels=class_lbls )
dat_plot$Present     = dat_plot$Mean > 0.0

### Pre-process graph
# g <- graph.adjacency(class_event, weighted=T, mode="directed")
# # assign edget weights, order, etc.
# weights = E(g)$weight
# edge_order = order(E(g)$weight)
# new_edges = data.frame(from=NA, to=NA, weight=NA)
# new_weights = rep(0, length(weights))
# for (i in 1:length(edge_order)) {
#     j = edge_order[i]
#     new_edges[i,] = c( get.edgelist(g)[j,], E(g)$weight[j] )
# }
# new_edges$weight = as.numeric(new_edges$weight)

# 
# ### Create the main graph object
# g <- graph_from_data_frame(d = new_edges, vertices = bg_lbl )
# 
# ### Tweaking the coordinate system
# coords = layout_in_circle(g)
# coords = coords_rotate(coords, angle=3.5*(1/24*2*pi))
# coords = coords[c(1,8,7,2,5,3,4,6),]
# #  NA, NT, AN, PA, AF, AS, AU, IN
# 
# scale_factor = 9
# coord_labels = (scale_factor*1.5)*coords
# coords = scale_factor*coords
# 
# lab.locs = -apply( coords, 1, get_radian )
# xlim<-c(-1.3*scale_factor-3,1.3*scale_factor+2)
# ylim<-c(-1.3*scale_factor,1.3*scale_factor)
# 
# # colors
area_colors       = bg_colors$state_colors[1:8]
# mark_groups       = list(c(1,4), c(2,3,5:8))
# 
# # apply/rescale colors with graph
# V(g)$color     = area_colors
# V(g)$label.cex = 0.6
# E(g)$color     = "black"
# 
# # determine edge color (darkness) scale
# n_col       = length(class_lbls)
# edge_colors = rev(gray.colors(n=n_col, start=0.05, end=0.9)) #[ E(g)$weight ]
# edge_sizes  = c(6,10,20,40,60,80)/10
# edge_curve  = 0.3
# edge_curves = sample( seq(-0.3, -0.1, length.out = ecount(g)) )
# edge_lty    = c(1,1,1,1,1,1) #c(3,2,5,1,1)
# 
# # plot figure
# plot.igraph(g,asp=0,xlim=xlim,ylim=ylim,layout=coords,rescale=F,
#             vertex.shape="fcircle", vertex.color=area_colors,vertex.frame.color="black",vertex.frame.width=2,vertex.size=200,vertex.label.dist=50,vertex.label.degree=lab.locs,vertex.label.color="black",vertex.label.cex=1,vertex.label.family="sans",mark.groups = mark_groups,mark.col=NA,mark.border = NA,mark.expand=200,mark.shape=1/2,edge.width = edge_sizes[ E(g)$weight ], #E(g)$weight,
#             edge.arrow.size = 0.5, #0.5, #(E(g)$weight/max(E(g)$weight))^2,
#             edge.arrow.width= 0.8, #arrow_heads[ E(g)$weight ], #0.5, #(E(g)$weight/max(E(g)$weight))^2,
#             edge.color=edge_colors[  E(g)$weight ],edge.curved=edge_curves,edge.lty=edge_lty[ E(g)$weight ])
# 
# legend(x=xlim[1]-3,y=ylim[1]+1,
#        title="Num. events\n(posterior mean)",
#        legend=class_lbls,
#        box.lwd=0,
#        lwd=edge_sizes,
#        lty=edge_lty,
#        cex=0.65,
#        col=edge_colors)
# 
# title("Inferred dispersal\nevents (pruned)")

tib_coords <- tibble(From_State=dat_plot$From_State[1:8],
       To_State=dat_plot$From_State[1:8],
       From_x=c(-104,-60,0,40,25,112,145,78),From_y=c(25,-20,-80,60,-10,28,-30,10),
       To_x=c(-104,-60,0,40,25,112,145,78),To_y=c(25,-20,-80,60,-10,28,-30,10))

to_map <- dat_plot %>% left_join(.,tib_coords %>% select(1,3,4),by="From_State") %>% 
    left_join(.,tib_coords %>% select(2,5,6),by="To_State") %>% select(-5,-4,-6) %>% filter(Present)

ggplot() +
geom_sf(data=ne_countries(scale=110,returnclass = "sf",type="countries"),colour="grey95",fill="grey95",size=0.5) +
geom_point(data=tib_coords,aes(x=From_x,y=From_y)) + 
    geom_curve(data = to_map %>% filter(Mean >= 1) %>% 
 arrange(Mean),aes(x=From_x,y=From_y,xend=To_x,yend=To_y,color=Mean),curvature = 0.1,arrow = arrow(ends = "last",type="open",length = unit(0.1, "inches")))  +
    scale_size(range=c(0,1),guide = "none" )  + 
    scale_color_stepsn(colors = MetBrewer::met.brewer("Tam",5),breaks=c(2,4,6,8,10)) +
    theme(panel.background = element_blank(),panel.grid = element_blank()) +
    NULL

state_bins_bu = state_bins
state_bins = state_bins_ext
mean_event = rowSums(state_bins,dims=2) * (1/n_iter)
prob_event = rowSums(hit_bins,dims=2) * (1/n_iter)

# format & filter
rownames(mean_event)=bg_lbl; colnames(mean_event)=bg_lbl
rownames(prob_event)=bg_lbl; colnames(prob_event)=bg_lbl
#### Define and collect classes of event counts
class_vals = c(1, 2, 3, 4, 5,10,15)
class_lbls = paste0(">", class_vals)
mean_event = round(mean_event, digits=1)
class_event = mean_event
class_event[ mean_event < class_vals[1] ] = 0
for (i in 1:length(class_vals)) {
    class_event[ mean_event >= class_vals[i] ] = i
}

m_mean = reshape2::melt(mean_event) %>% 
    bind_cols(.,reshape2::melt(prob_event) %>% select(value) %>% rename("prob"=value))
dat_plot = m_mean
colnames(dat_plot) = c("From_State","To_State", "Mean", "Prob")
#dat_plot$From_Biome  = sapply( as.vector(dat_plot$From_State), function(x) { strsplit(x, "\\+")[[1]][1] })
dat_plot$From_Area   = as.vector(dat_plot$From_State)
#dat_plot$To_Biome    = sapply( as.vector(dat_plot$To_State),   function(x) { strsplit(x, "\\+")[[1]][1] })
dat_plot$To_Area     = as.vector(dat_plot$To_State)
cc                   = make_count_class(x=dat_plot$Mean, p=class_vals)
dat_plot$Count_Class = factor( cc, ordered=T, levels=class_lbls )
dat_plot$Present     = dat_plot$Mean > 0.0

print(dat_plot %>% filter(Present) %>% group_by(To_Area) %>% summarise(Extirpation = sum(Mean)) %>% mutate(To_Area=factor(To_Area,levels=rev(c("Palearctic","Nearctic","Asia","Australasia","Neotropical","Africa","Antartic","India")))))

dat_plot %>% filter(Present) %>% group_by(To_Area) %>% summarise(Extirpation = sum(Mean)) %>% mutate(To_Area=factor(To_Area,levels=rev(c("Palearctic","Nearctic","Asia","Australasia","Neotropical","Africa","Antartic","India")))) %>% 
    ggplot(aes(x = Extirpation, y = To_Area)) + 
    geom_col(fill=area_colors[c(8,3,6,7,5,1,2,4)]) + 
    #geom_col() +
    theme(panel.background = element_blank(),axis.line=element_line()) + labs(y="",x="Number of events", title = "Inferred extirpation")


}


library(sp)
library(sf)
library(rgdal)
library(here)

readOGR(here("gplates/Phanerozoic_EarthByte_Coastlines/Phanerozoic_EarthByte_Coastlines/present.shp")) -> aqui
plot(spTransform(aqui, CRS = "+proj=eck4 +lon_0=150 +x_0=0 +y_0=0 +ellps=WGS84"))

plot(spTransform(aqui, CRS = raster::crs("ESRI:54099", describe=TRUE)))

ocean <- st_point(x = c(0,0)) %>%
    st_buffer(dist = 6371000) %>%
    st_sfc(crs = sf::st_crs("ESRI:54099"))

world <- aqui %>% 
    st_intersection(ocean %>% st_transform(.,st_crs(aqui))) %>% # select visible area only
    st_transform(crs = crs_string) # reproject to ortho

aqui <- rgdal::readOGR("~/Desktop/ne_10m_ocean/ne_10m_ocean.shp")
aqui <- as(aqui,"sf")
plot(sf::st_transform(aqui, sf::st_crs("ESRI:54099")))

