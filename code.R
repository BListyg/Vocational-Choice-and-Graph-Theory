library(rvest)
library(igraph)
library(stringr)
library(reshape2)
library(data.table)
library(tm)

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

riasec.data = do.call("rbind",t(apply(X = matrix(as.character(complete.links$X5)),MARGIN = 1,FUN = riasec)))

riasec.data = data.frame(riasec.data)

#Cleaning income data
#Leave annual income alone
#Convert hourly income to annual
#Remove punctuation
#Remove "annual" and "hourly"

riasec.data$income.length = do.call(rbind,
                                    lapply(str_split(string = riasec.data$income,pattern = " "),length))

riasec.data$income <- as.numeric(c(
#Only annual
removePunctuation(riasec.data$income[which(riasec.data$income.length==1)]),

#only hourly, convert to annual
#Multiplied by 2000 (50 working weeks, 40 hours per week) to get annual
#UGLY line of code
as.numeric(gsub("[$]","",gsub("hourly",NA,(unlist(str_split(string = riasec.data$income[which(riasec.data$income.length==2)],pattern = " "))))[complete.cases(gsub("hourly",NA,(unlist(str_split(string = riasec.data$income[which(riasec.data$income.length==2)],pattern = " ")))))])) * 2000,

#Both, extract 3rd element of string
removePunctuation(word(riasec.data$income[which(riasec.data$income.length==3)],3))))

##Not sure if I need this portion anymore
final.data = merge(riasec.data[!duplicated(riasec.data$X5),], complete.links[!duplicated(complete.links$X5),], by = "X5")

'final.data = cbind(final.data, income = do.call(rbind, str_split(final.data$income, pattern = " "))[,c(3,4)])

final.data = final.data[which(final.data$income.2 == "annual"),]
final.data$income = NULL
final.data$income.2 = NULL
final.data$income.1 = as.numeric(gsub("[\\$,]", "", final.data$income.1))
final.data[,c(2:7)] = apply(X = final.data[,c(2:7)], MARGIN = 2, FUN = function(x){as.numeric(as.character(x))})
final.data = melt(final.data, id.vars = c("X5", "title", "X1", "X2", "X3", "X4", "income.1"))

colnames(final.data) = c("link", "title","career.cluster", "career.pathway", "code","occupation","income","interest","score")'


#Restructuring data from long to wide
final.data = reshape(final.data, 
        timevar = "interest",
        idvar = c("link", "title", "career.cluster", "career.pathway", "code", "occupation", "income"),
        direction = "wide")

###

#riasec_data <- read.csv("riasec_data.csv", header=T)

###Network portion below###

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

##

#data.frame(eigen = eigen_centrality(ccm.graph), row.names = NULL)))

ccm.data$title = paste(ccm.data$O.NET.SOC.Code," - ",ccm.data$Title,sep = "")

ccm.data = ccm.data[order(ccm.data$title),]

#head(ccm.data)

##

csm.data$title = paste(csm.data$O.NET.SOC.Code," - ",csm.data$Title,sep = "")

csm.data = csm.data[order(csm.data$title),]

#head(csm.data)

colnames(csm.data)[-ncol(ccm.data)] <- paste("csm.",colnames(csm.data)[-ncol(csm.data)],sep = "")

colnames(ccm.data)[-ncol(ccm.data)] <- paste("ccm.",colnames(ccm.data)[-ncol(ccm.data)],sep = "")

final_data <- merge(x = final.data, y = csm.data, by = "title", all = TRUE)

final_data <- merge(x = final.data, y = ccm.data, by = "title", all = TRUE)

final_data$income <- gsub("[\\$,]", "", final_data$income)

final_data$income <- gsub("annual", "", final_data$income)

final_data$income <- trimws(final_data$income)

final_data$income.string.length <- do.call(rbind,lapply(str_split(final_data$income, " "), length))

final_data[order(final_data$income.string.length),]
