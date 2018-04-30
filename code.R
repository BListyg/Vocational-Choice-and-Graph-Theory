library(rvest)
library(igraph)
library(stringr)
library(reshape2)
library(data.table)
library(tm)
library(ggplot2)
library(cowplot)
library(lme4)

#Obtaining links to different career webpages
career.cluster = data.frame(cbind(
  read_html("https://www.onetonline.org/find/career?c=0&g=Go") %>% 
    html_nodes(".report2ed:nth-child(1)") %>% 
    html_text() %>%
    matrix(),
  read_html("https://www.onetonline.org/find/career?c=0&g=Go") %>% 
    html_nodes(".report2ed+ .report2ed") %>% 
    html_text() %>%
    matrix(),
  read_html("https://www.onetonline.org/find/career?c=0&g=Go") %>% 
    html_nodes(".reportrtd") %>% 
    html_text() %>%
    matrix(),
  read_html("https://www.onetonline.org/find/career?c=0&g=Go") %>% 
    html_nodes(".report2ed a:nth-child(1)") %>% 
    html_text() %>%
    matrix(),
  read_html("https://www.onetonline.org/find/career?c=0&g=Go") %>% 
    html_nodes(".report2ed a:nth-child(1)") %>% 
    html_attr("href")
))

#Some career pages have nested links
#function to obtain nested links
get.links = function(x){
  pasted.x = paste(x)
  X5 = read_html(pasted.x[5]) %>% 
    html_nodes(".excitem a:nth-child(1)") %>% 
    html_attr("href")
  if(length(X5)>0){
    return(cbind(t(replicate(x[1:4],n = length(X5))),X5))}
  else if(length(X5) == 0)
    return(t(x))
}

complete.links = data.frame(do.call("rbind",apply(X = career.cluster,MARGIN = 1,FUN = get.links)))

complete.links = apply(X = complete.links,2,as.character)

complete.links[,5] = gsub(pattern = "summary",replacement = "details",x = complete.links[,5])

complete.links = data.frame(complete.links)

#complete.links = complete.links[unique(complete.links$X5),]

#complete.links$X5 = as.character(complete.links$X5)

#Function to return RIASEC values by job

#Obtains RIASEC scores, wage, and title for each occupation
riasec = function(x)
{
  numbers = read_html(x,) %>%
    html_nodes("#wrapper_Interests .report2a b") %>%
    html_text() %>%
    matrix()
  
  interests = read_html(x) %>%
    html_nodes("#wrapper_Interests .moreinfo b") %>%
    html_text() %>%
    matrix()
  
  wage = read_html(x) %>%
    html_nodes("#wrapper_WagesEmployment tr:nth-child(1) .report2") %>%
    html_text()
  
  title = read_html(x) %>%
    html_nodes(".titleb") %>%
    html_text()  
  
  #wage = str_split(wage,pattern = " ")
  #wage = wage[[1]][3]
  #wage = str_replace_all(wage, "[[:punct:]]", "")
  #wage = as.numeric(gsub("[\\$,]", "", wage))
  
  dat = cbind(interests,numbers)
  dat = dat[order(dat[,1]),]
  dat = rbind(dat,wage,x,title)
  
  row.names(dat) = NULL
  
  if(length(dat) > 6){
    dat[7,1] = paste("income")
    dat[8,1] = paste("X5")
    dat[8,2] = x
    dat[9,1] = paste("title")
    dat[9,2] = title
    rownames(dat) = c("Artistic","Conventional","Enterprising","Investigative","Realistic","Social","income","X5","title")
    return(t(dat[,-1]))}
  else if(length(dat) < 6){
    return(NULL)}
}

#This may result in a timeout error. Final data can be found on my github.
#read.csv("riasec_data.csv",header=T)
riasec_data = do.call("rbind",t(apply(X = matrix(as.character(complete.links$X5)),MARGIN = 1,FUN = riasec)))

riasec_data = data.frame(riasec_data)

#riasec_data<-read.csv("riasec_data.csv",header=T)

riasec_data$income <- as.character(riasec_data$income)

income<-do.call(
  rbind,
  str_split(
    gsub(","
         ,""
         ,riasec_data$income
    )," ",4))[,c(3,4)]

income[,1]<-gsub("[\\$]","",income[,1])

income[,1]<-gsub("[\\+]","",income[,1])

income <- as.matrix(income[,1])

income <- trimws(income)

income <- apply(matrix(income), 1, as.numeric)

riasec_data$income <- income

riasec_data<-riasec_data[order(riasec_data$income, decreasing = FALSE),]

riasec_data$income[c(1:6)] <- riasec_data$income[c(1:6)]*2000

riasec_data$title <- trimws(riasec_data$title)

#####

ccm <- read.csv("https://raw.githubusercontent.com/BListyg/Vocational-Choice-and-Graph-Theory/master/Career%20Changers%20Matrix%20.csv", header=T)
csm <- read.csv("https://raw.githubusercontent.com/BListyg/Vocational-Choice-and-Graph-Theory/master/Career%20Starters%20Matrix%20.csv", header=T)

#ccm$Index <- (max(ccm$Index)+1 - ccm$Index)

ccm.graph <- simplify(graph_from_data_frame(
  data.frame(ccm$Title, 
             ccm$Related.Title), 
  directed = F))

##

ccm.data <- cbind(
  data.frame(
    Title = unique(ccm$Title)),
  O.NET.SOC.Code = unique(ccm$O.NET.SOC.Code),
  
  data.frame(degree = degree(ccm.graph), row.names = NULL),
  
  data.frame(closeness = closeness(ccm.graph), row.names = NULL),
  
  data.frame(betweenness = betweenness(ccm.graph), row.names = NULL))

##

csm.graph <- simplify(graph_from_data_frame(
  data.frame(csm$Title, 
             csm$Related.Title), 
  directed = F))

##

csm.data <- cbind(
  data.frame(
    Title = unique(csm$Title)),
  O.NET.SOC.Code = unique(csm$O.NET.SOC.Code),
  
  data.frame(degree = degree(csm.graph), row.names = NULL),
  
  data.frame(closeness = closeness(csm.graph), row.names = NULL),
  
  data.frame(betweenness = betweenness(csm.graph), row.names = NULL))

#####

#data.frame(eigen = eigen_centrality(ccm.graph), row.names = NULL)))

ccm.data$title = paste(ccm.data$O.NET.SOC.Code," - ",ccm.data$Title,sep = "")

ccm.data = ccm.data[order(ccm.data$title),]

#head(ccm.data)

#####

csm.data$title = paste(csm.data$O.NET.SOC.Code," - ",csm.data$Title,sep = "")

csm.data = csm.data[order(csm.data$title),]

#head(csm.data)

colnames(csm.data)[-ncol(ccm.data)] <- paste("csm.",colnames(csm.data)[-ncol(csm.data)],sep = "")

colnames(ccm.data)[-ncol(ccm.data)] <- paste("ccm.",colnames(ccm.data)[-ncol(ccm.data)],sep = "")

riasec_data <- merge(x = riasec_data, y = csm.data, by = "title", all = TRUE)

riasec_data <- merge(x = riasec_data, y = ccm.data, by = "title", all = TRUE)

#####

riasec_data <- merge(x = complete.links,
        y = riasec_data,
        by = "X5",
        all = TRUE)

#####

riasec_data$Artistic <- as.numeric(as.character(riasec_data$Artistic))

riasec_data$Enterprising <- as.numeric(as.character(riasec_data$Enterprising))

riasec_data$Conventional <- as.numeric(as.character(riasec_data$Conventional))

riasec_data$Investigative <- as.numeric(as.character(riasec_data$Investigative))

riasec_data$Social <- as.numeric(as.character(riasec_data$Social))

riasec_data$Realistic <- as.numeric(as.character(riasec_data$Realistic))

riasec_data$income <- as.numeric(as.character(riasec_data$income))

riasec_data$X.1 = NULL
riasec_data$X = NULL

colnames(riasec_data)[c(1:5)] <- c("url",
                                   'career_cluster',
                                   'career_pathway',
                                   'SOC',
                                   'job_title')

head(riasec_data)

#####

V(ccm.graph)$color <- riasec_data$X1[match( V(ccm.graph)$name,  riasec_data$X4 ) ]

V(csm.graph)$color <- riasec_data$X1[match( V(csm.graph)$name,  riasec_data$X4 ) ]

#####

plot(ccm.graph, vertex.label = NA, vertex.size = 3.5, layout = layout_nicely, edge.width = 0.5)

plot(csm.graph, vertex.label = NA, vertex.size = 3.5, layout = layout_nicely, edge.width = 0.5)

 #####

complete_riasec_data <- unique(riasec_data)

#####

art <- ggplot(complete_riasec_data, aes(Artistic, log(income), colour = career_cluster))+ theme(legend.position="none") + geom_point(aes(colour = factor(career_cluster)), size = 1) + guides(fill=FALSE)+ geom_smooth(se = FALSE, method = "lm")
real <- ggplot(complete_riasec_data, aes(Realistic, log(income), colour = career_cluster))+ theme(legend.position="none") + geom_point(aes(colour = factor(career_cluster)), size = 1) + guides(fill=FALSE)+ geom_smooth(se = FALSE, method = "lm")
conv <- ggplot(complete_riasec_data, aes(Conventional, log(income), colour = career_cluster))+ theme(legend.position="none") + geom_point(aes(colour = factor(career_cluster)), size = 1) + guides(fill=FALSE)+ geom_smooth(se = FALSE, method = "lm")
soci <- ggplot(complete_riasec_data, aes(Social, log(income), colour = career_cluster))+ theme(legend.position="none") + geom_point(aes(colour = factor(career_cluster)), size = 1) + guides(fill=FALSE)+ geom_smooth(se = FALSE, method = "lm")
entr <- ggplot(complete_riasec_data, aes(Enterprising, log(income), colour = career_cluster))+ theme(legend.position="none") + geom_point(aes(colour = factor(career_cluster)), size = 1) + guides(fill=FALSE)+ geom_smooth(se = FALSE, method = "lm")
invest <- ggplot(complete_riasec_data, aes(Investigative, log(income), colour = career_cluster))+ theme(legend.position="none") + geom_point(aes(colour = factor(career_cluster)), size = 1) + guides(fill=FALSE)+ geom_smooth(se = FALSE, method = "lm")

plot_grid(real,
          invest,
          art,
          soci,
          entr,
          conv)

deg <- ggplot(complete_riasec_data, aes(csm.degree, log(income)))+ theme(legend.position="none") + geom_point(aes(colour = factor(career_cluster)), size = 1) + guides(fill=FALSE)

betw <- ggplot(complete_riasec_data, aes(csm.betweenness, log(income)))+ theme(legend.position="none") + geom_point(aes(colour = factor(career_cluster)), size = 1) + guides(fill=FALSE)

close <- ggplot(complete_riasec_data, aes(csm.closeness, log(income)))+ theme(legend.position="none") + geom_point(aes(colour = factor(career_cluster)), size = 1) + guides(fill=FALSE) + geom_smooth()

plot_grid(deg,betw,close)

plot_grid(
ggplot(complete_riasec_data, aes(csm.closeness, log(income), colour = career_cluster)) + theme(legend.position="none") + geom_point(aes(colour = factor(career_cluster)))+ geom_smooth(se = FALSE, method = "lm"),
ggplot(complete_riasec_data, aes(ccm.closeness, log(income), colour = career_cluster)) + theme(legend.position="none") + geom_point(aes(colour = factor(career_cluster)))+ geom_smooth(se = FALSE, method = "lm")
)

plot_grid(
ggplot(complete_riasec_data, aes(csm.betweenness, log(income), colour = career_cluster)) + theme(legend.position="none") + geom_point(aes(colour = factor(career_cluster)))+ geom_smooth(se = FALSE, method = "lm"),
ggplot(complete_riasec_data, aes(ccm.betweenness, log(income), colour = career_cluster)) + theme(legend.position="none") + geom_point(aes(colour = factor(career_cluster)))+ geom_smooth(se = FALSE, method = "lm")
)

plot_grid(
ggplot(complete_riasec_data, aes(csm.degree, log(income), colour = career_cluster)) + theme(legend.position="none") + geom_point(aes(colour = factor(career_cluster)))+ geom_smooth(se = FALSE, method = "lm"),
ggplot(complete_riasec_data, aes(ccm.degree, log(income), colour = career_cluster)) + theme(legend.position="none") + geom_point(aes(colour = factor(career_cluster)))+ geom_smooth(se = FALSE, method = "lm")
)

plot_grid(
ggplot(complete_riasec_data, aes(x = Realistic, y = career_cluster)) + geom_density_ridges2(),
ggplot(complete_riasec_data, aes(x = Investigative, y = career_cluster)) + geom_density_ridges2(),
ggplot(complete_riasec_data, aes(x = Artistic, y = career_cluster)) + geom_density_ridges2(),
ggplot(complete_riasec_data, aes(x = Social, y = career_cluster)) + geom_density_ridges2(),
ggplot(complete_riasec_data, aes(x = Enterprising, y = career_cluster)) + geom_density_ridges2(),
ggplot(complete_riasec_data, aes(x = Conventional, y = career_cluster)) + geom_density_ridges2())

plot_grid(
  ggplot(complete_riasec_data, aes(x = ccm.betweenness, y = career_cluster)) + geom_density_ridges2(),
  ggplot(complete_riasec_data, aes(x = ccm.closeness, y = career_cluster)) + geom_density_ridges2(),
  ggplot(complete_riasec_data, aes(x = ccm.degree, y = career_cluster)) + geom_density_ridges2(),
  ggplot(complete_riasec_data, aes(x = ccm.betweenness, y = career_cluster)) + geom_density_ridges2(),
  ggplot(complete_riasec_data, aes(x = csm.closeness, y = career_cluster)) + geom_density_ridges2(),
  ggplot(complete_riasec_data, aes(x = csm.degree, y = career_cluster)) + geom_density_ridges2())

lmer ( log(income) ~ Realistic + ( Realistic | career_cluster ), complete_riasec_data )

riasec_data



