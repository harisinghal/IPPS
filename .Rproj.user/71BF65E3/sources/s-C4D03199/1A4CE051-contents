#setwd("~/Desktop/Hari_Data/12-1-17-Backup_R/A/JObs/OnSite/CVS/TakeHome")

# Importing libraries
library(tidyverse) #tidyverse imports ggplot2 and dplyr (charting/manipulation)
library(stringr) #will be used to extract medical codes from categories
library(purrr) #the map function is used to customize functions and apply on df
library(rvest) #importing MDC categorization table
library(knitr) #table display
library(gplots)

# Reading the input data
df = read.csv("IPPS_FY2011.csv")
df<- as_tibble(df)

# Reading the MDC codes
url <- "https://en.wikipedia.org/wiki/Major_Diagnostic_Category"
mdc <- url %>%
  read_html() %>%
  html_node('body #content #bodyContent #mw-content-text .mw-parser-output table') %>%
  html_table(fill = TRUE)

#Separating DRG code and correcting MDCs
sep_col <- function(s)
{
  return(str_extract(s, "\\d+"))
}
df$DRG_code <- map_chr(df$DRG.Definition, sep_col)

mdc <- rename(mdc, DRG_codes = `MS-DRG[1][2]`) #Rename column to DRG_codes
mdc$DRG_codes[27] <- "981 - 997"
mdc$DRG_codes[mdc$MDC == 14] <- "765 - 782" #contained 998, which does not exist in dataset
mdc$DRG_codes[mdc$MDC == 9] <- "570 - 607" #missing codes on Wikipedia


# Inspecting data and cleaning (if necessary)
summary(df)

df %>%
  filter(Provider.Id <=100000) %>%
  group_by(Provider.Name) %>%
  summarise(n=n())

df %>%
  filter(Provider.Zip.Code <=10000) %>%
  group_by(Provider.Name) %>%
  summarise(n=n())

length(unique(df$DRG.Definition))
length(unique(df$Provider.Id))
length(unique(df$Provider.Name))
length(unique(df$Provider.City))
length(unique(df$Provider.State))
unique(df$Provider.State)
length(unique(df$Provider.Zip.Code))
length(unique(df$Hospital.Referral.Region.Description))
summary(df$Total.Discharges)
df %>% filter(Total.Discharges == max(df$Total.Discharges)) # Site with maximum number of discharges

## Including MDC codes
getMDC <- function(x)
{
  for (j in seq_along(mdc$DRG_codes))
  {
    if((as.numeric(x) >= as.numeric(str_sub(mdc$DRG_codes[j],1,3))) &
       (as.numeric(x) <= as.numeric(str_sub(mdc$DRG_codes[j],-3,-1))))
    {
      return(mdc$MDC[j])
      break
    }
  }
}

getMDC_desc <- function(x)
{
  for (j in seq_along(mdc$DRG_codes))
  {
    if((as.numeric(x) >= as.numeric(str_sub(mdc$DRG_codes[j],1,3))) &
       (as.numeric(x) <= as.numeric(str_sub(mdc$DRG_codes[j],-3,-1))))
    {
      return(mdc$Description[j])
      break
    }
  }
}

df$MDC <- map_chr(df$DRG_code, getMDC)
df$MDC_desc <- map_chr(df$DRG_code, getMDC_desc)


# Drawing USA heatmaps
df_State <- df %>% group_by(Provider.State) %>%
  summarise(Total.Discharges=sum(Total.Discharges), Average.Covered.Charges = mean(Average.Covered.Charges),
            Average.Total.Payments=mean(Average.Total.Payments),
            Average.Medicare.Payments=mean(Average.Medicare.Payments),
            Average.OutofPocket=mean(Average.Total.Payments - Average.Medicare.Payments)
  )

df1 = data.frame(df_State)
c = colnames(df1)

l <- list(color = toRGB("white"), width = 2)
# specify some map projection/options
g <- list(
  scope = 'usa',
  projection = list(type ='albers usa'),
  showlakes = TRUE,
  lakecolor = toRGB('white')
)
p <- plot_geo(df1, locationmode = 'USA-states') %>%
  add_trace(
    z = ~Total.Discharges,  
    color = ~Total.Discharges, colors = 'Blues', locations = ~Provider.State
  ) %>%
  colorbar(title = c[2]) %>%
  layout(
    title = c[2],
    geo = g
  )
p

####

p <- plot_geo(df1, locationmode = 'USA-states') %>%
  add_trace(
    z = ~Average.Covered.Charges,  
    color = ~Average.Covered.Charges, colors = 'Purples', locations = ~Provider.State
  ) %>%
  colorbar(title = c[3]) %>%
  layout(
    title = c[3],
    geo = g
  )
p


p <- plot_geo(df1, locationmode = 'USA-states') %>%
  add_trace(
    z = ~Average.Total.Payments,  
    color = ~Average.Total.Payments, colors = 'Purples', locations = ~Provider.State
  ) %>%
  colorbar(title = c[4]) %>%
  layout(
    title = c[4],
    geo = g
  )
p


p <- plot_geo(df1, locationmode = 'USA-states') %>%
  add_trace(
    z = ~Average.Medicare.Payments,  
    color = ~Average.Medicare.Payments, colors = 'Purples', locations = ~Provider.State
  ) %>%
  colorbar(title = c[5]) %>%
  layout(
    title = c[5],
    geo = g
  )
p


p <- plot_geo(df1, locationmode = 'USA-states') %>%
  add_trace(
    z = ~Average.OutofPocket,  
    color = ~Average.OutofPocket, colors = 'Greens', locations = ~Provider.State
  ) %>%
  colorbar(title = c[6]) %>%
  layout(
    title = c[6],
    geo = g
  )
p

# Visualizing specific disease codes
df_State <- df %>% filter(DRG_code == "885") %>% group_by(Provider.State) %>%
  summarise(Total.Discharges=sum(Total.Discharges), Average.Covered.Charges = mean(Average.Covered.Charges),
            Average.Total.Payments=mean(Average.Total.Payments),
            Average.Medicare.Payments=mean(Average.Medicare.Payments),
            Average.OutofPocket=mean(Average.Total.Payments - Average.Medicare.Payments)
  )

df1 = data.frame(df_State)
c = colnames(df1)

l <- list(color = toRGB("white"), width = 2)
# specify some map projection/options
g <- list(
  scope = 'usa',
  projection = list(type ='albers usa'),
  showlakes = TRUE,
  lakecolor = toRGB('white')
)
p <- plot_geo(df1, locationmode = 'USA-states') %>%
  add_trace(
    z = ~Total.Discharges,  
    color = ~Total.Discharges, colors = 'Blues', locations = ~Provider.State
  ) %>%
  colorbar(title = c[2]) %>%
  layout(
    title = c[2],
    geo = g
  )
p

## Different disease code

df_State <- df %>% filter(DRG_code == "392") %>% group_by(Provider.State) %>%
  summarise(Total.Discharges=sum(Total.Discharges), Average.Covered.Charges = mean(Average.Covered.Charges),
            Average.Total.Payments=mean(Average.Total.Payments),
            Average.Medicare.Payments=mean(Average.Medicare.Payments),
            Average.OutofPocket=mean(Average.Total.Payments - Average.Medicare.Payments)
  )
df1 = data.frame(df_State)
c = colnames(df1)

l <- list(color = toRGB("white"), width = 2)
# specify some map projection/options
g <- list(
  scope = 'usa',
  projection = list(type ='albers usa'),
  showlakes = TRUE,
  lakecolor = toRGB('white')
)
p <- plot_geo(df1, locationmode = 'USA-states') %>%
  add_trace(
    z = ~Total.Discharges,  
    color = ~Total.Discharges, colors = 'Blues', locations = ~Provider.State
  ) %>%
  colorbar(title = c[2]) %>%
  layout(
    title = c[2],
    geo = g
  )
p

# Quality check - Testing duplicate entries for providers for a given DRG
#length(df[df$DRG_code == "057",]$Provider.Id)
#length(unique(df[df$DRG_code == "057",]$Provider.Id))

# Distribution of providers

# By the DRG codes
x = data.frame(df %>%
                 group_by(Provider.State,DRG_code,Hospital.Referral.Region.Description) %>%
                 summarise(n=n()))

ggplot(x, aes(x= reorder(DRG_code, -n), y=n)) + 
  geom_bar(stat = "identity") + 
  theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(),
        panel.background = element_blank(), axis.line = element_line(colour = "black"),
        axis.text.x = element_text(angle = 90, hjust = 1)) +
  ggtitle("Distribution of providers") +
  xlab("DRG code") + 
  ylab("No. of providers")

# By DRG code and HRR region
p <- ggplot(x, aes(Hospital.Referral.Region.Description,n))
p + geom_point()+ 
  theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(),
        panel.background = element_blank(), axis.line = element_line(colour = "black"),
        axis.text.x = element_text(angle = 90, hjust = 1, size = 5)) +
  ggtitle("Distribution of providers by DRG code + HRR") +
  xlab("HRR Region") + 
  ylab("No. of providers for a given DRG code")

# Density of providers for a given DRG + HRR
plot(density(x$n), xlab = "Number of providers",main = "Density of providers in a given DRG+HRR combination")


### Exploring total discharges by DRG code

x1 = data.frame(df %>%
                  group_by(DRG_code) %>%
                  summarise(Total.Discharges=sum(Total.Discharges), Average.Covered.Charges = mean(Average.Covered.Charges),
                            Average.Total.Payments=mean(Average.Total.Payments),
                            Average.Medicare.Payments=mean(Average.Medicare.Payments),
                            Average.OutofPocket=mean(Average.Total.Payments - Average.Medicare.Payments)
                  )
)

ggplot(x1, aes(x= reorder(DRG_code, -Total.Discharges), y=Total.Discharges)) + 
  geom_bar(stat = "identity") + 
  theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(),
        panel.background = element_blank(), axis.line = element_line(colour = "black"),
        axis.text.x = element_text(angle = 90, hjust = 1)) +
  ggtitle("Total number of discharges per DRG code") +
  xlab("DRG code") + 
  ylab("Total no. of discharges")

boxplot(x1$Total.Discharges, ylab = "Total no. of discharges",main = "Total number of discharges per DRG code", col = "darkblue")

boxplot(df$Total.Discharges, ylab = "Total no. of discharges",main = "Total number of discharges", col = "darkred")

###

df_Provider <- df %>% group_by(Provider.Name) %>%
  summarise(Total.Discharges=sum(Total.Discharges))

knitr::kable(top_n(df_Provider, 10, Total.Discharges))

ggplot(top_n(df_Provider, 50, Total.Discharges), aes(x= reorder(Provider.Name, -Total.Discharges), y=Total.Discharges)) + 
  geom_bar(stat = "identity") + 
  theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(),
        panel.background = element_blank(), axis.line = element_line(colour = "black"),
        axis.text.x = element_text(angle = 90, hjust = 1)) +
  ggtitle("Total Discharges - Top 50") +
  xlab("Provider name") + 
  ylab("No. of providers")

ggplot(top_n(df_Provider, -50, Total.Discharges), aes(x= reorder(Provider.Name, -Total.Discharges), y=Total.Discharges)) + 
  geom_bar(stat = "identity") + 
  theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(),
        panel.background = element_blank(), axis.line = element_line(colour = "black"),
        axis.text.x = element_text(angle = 90, hjust = 1)) +
  ggtitle("Total Discharges - Bottom 50") +
  xlab("Provider name") + 
  ylab("No. of providers")

df_Provider <- df %>% group_by(Provider.Name) %>%
  summarise(Total.Discharges=sum(Total.Discharges))

knitr::kable(top_n(df_Provider, 10, Total.Discharges))

# States that have billed the most to Medicare
df_State <- df %>%
  mutate(Total.Discharges = Average.Covered.Charges*Total.Discharges) %>% 
  group_by(Provider.State) %>%
  summarise(Total.Discharges=sum(Total.Discharges))

df1 = data.frame(df_State)
l <- list(color = toRGB("white"), width = 2)
# specify some map projection/options
g <- list(
  scope = 'usa',
  projection = list(type ='albers usa'),
  showlakes = TRUE,
  lakecolor = toRGB('white')
)
p <- plot_geo(df1, locationmode = 'USA-states') %>%
  add_trace(
    z = ~Total.Discharges,  
    color = ~Total.Discharges, colors = 'Reds', locations = ~Provider.State
  ) %>%
  colorbar(title = "Billions") %>%
  layout(
    title = "Total Billing to Medicare",
    geo = g
  )
p



# Billing and payments as per the provider
df_Provider <- df %>% mutate(total_c = Average.Covered.Charges*Total.Discharges,
                             total_p = Average.Medicare.Payments*Total.Discharges) %>% 
  group_by(Provider.Name) %>%
  summarise(total_charges=sum(total_c), total_paid = sum(total_p),
            ratio_paid_charged = sum(total_p)/sum(total_c),
            Total.Discharges = sum(Total.Discharges)) %>% 
  top_n(10, wt = total_charges) %>%
  arrange(desc(total_charges))

knitr::kable(df_Provider)


df_Provider <- df %>% mutate(total_c = Average.Covered.Charges*Total.Discharges,
                             total_p = Average.Medicare.Payments*Total.Discharges) %>% 
  group_by(Provider.Name) %>%
  summarise(total_charges=sum(total_c), total_paid = sum(total_p),
            ratio_paid_charged = sum(total_p)/sum(total_c),
            Total.Discharges = sum(Total.Discharges)) %>% 
  arrange(desc(total_charges))

df_Provider %>%
  ggplot(aes(x= total_charges, y=total_paid)) +
  geom_point() +
  xlab("Total charged to Medicare ($)") +
  ylab("Total paid by Medicare ($)") +
  ggtitle("Charges versus payments for Medicare by Provider") +
  coord_flip()+ 
  theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(),
        panel.background = element_blank(), axis.line = element_line(colour = "black")) 


# Heatmap

# Billing and payments as per the provider
df_Provider <- df %>% mutate(total_c = Average.Covered.Charges*Total.Discharges,
                             total_p = Average.Medicare.Payments*Total.Discharges) %>% 
  group_by(Provider.Name) %>%
  summarise(total_charges=sum(total_c), total_paid = sum(total_p),
            ratio_paid_charged = sum(total_p)/sum(total_c),
            Total.Discharges = sum(Total.Discharges)) %>% 
  top_n(20, wt = total_charges) %>%
  arrange(desc(total_charges))

knitr::kable(df_Provider)
x = data.frame(df_Provider)
rownames(x) = x$Provider.Name
x = x[,2:ncol(x)]
x = t(x)
rownames(x) = c("Total Charges", "Total Paid","Ratio: Paid/Charged","Total Discharges")
heatmap.2(as.matrix(x), scale = "row", col = bluered(276), trace = "none", dendrogram = "col", key.title = NULL, density.info = "none", margins = c(13,9), cexRow = 1, cexCol = 0.5)

## Billing based on providers top 20, bottom 20
# Billing and payments as per the provider
df_Provider <- df %>% mutate(total_c = Average.Covered.Charges*Total.Discharges,
                             total_p = Average.Medicare.Payments*Total.Discharges) %>% 
  group_by(Provider.Name) %>%
  summarise(total_charges=sum(total_c), total_paid = sum(total_p),
            ratio_paid_charged = sum(total_p)/sum(total_c),
            Total.Discharges = sum(Total.Discharges)) %>% 
  top_n(50, wt = total_charges) %>%
  arrange(desc(total_charges))

x1 = data.frame(df_Provider)

# Billing and payments as per the provider
df_Provider <- df %>% mutate(total_c = Average.Covered.Charges*Total.Discharges,
                             total_p = Average.Medicare.Payments*Total.Discharges) %>% 
  group_by(Provider.Name) %>%
  summarise(total_charges=sum(total_c), total_paid = sum(total_p),
            ratio_paid_charged = sum(total_p)/sum(total_c),
            Total.Discharges = sum(Total.Discharges)) %>% 
  top_n(-50, wt = total_charges) %>%
  arrange(desc(total_charges))

x2 = data.frame(df_Provider)
x = rbind(x1,x2)

rownames(x) = x$Provider.Name
x = x[,2:ncol(x)]
x = t(x)
rownames(x) = c("Total Charges", "Total Paid","Ratio: Paid/Charged","Total Discharges")
heatmap.2(as.matrix(x), scale = "row", col = bluered(276), trace = "none", dendrogram = "col", key.title = NULL, density.info = "none", margins = c(13,9), cexRow = 1, cexCol = 0.5)



# Billing and payments as per the DRG code
df_DRG <- df %>% mutate(total_c = Average.Covered.Charges*Total.Discharges,
                        total_p = Average.Medicare.Payments*Total.Discharges) %>% 
  group_by(DRG.Definition) %>%
  summarise(total_charges=sum(total_c), total_paid = sum(total_p),
            ratio_paid_charged = sum(total_p)/sum(total_c),
            Total.Discharges = sum(Total.Discharges))  %>%
  arrange(desc(total_charges))

#knitr::kable(df_DRG)

df_DRG %>%
  ggplot(aes(x= total_charges, y=total_paid)) +
  geom_point() +
  xlab("Total charged to Medicare ($)") +
  ylab("Total paid by Medicare ($)") +
  ggtitle("Charges versus payments for Medicare by DRG") +
  coord_flip()+ 
  theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(),
        panel.background = element_blank(), axis.line = element_line(colour = "black")) 

# Heatmaps by DRG
df_DRG <- df %>% mutate(total_c = Average.Covered.Charges*Total.Discharges,
                        total_p = Average.Medicare.Payments*Total.Discharges) %>% 
  group_by(DRG.Definition) %>%
  summarise(total_charges=sum(total_c), total_paid = sum(total_p),
            ratio_paid_charged = sum(total_p)/sum(total_c),
            Total.Discharges = sum(Total.Discharges))  %>%
  arrange(desc(total_charges))


x = data.frame(df_DRG)
rownames(x) = x$DRG.Definition
x = x[,2:ncol(x)]
x = t(x)
rownames(x) = c("Total Charges", "Total Paid","Ratio: Paid/Charged","Total Discharges")
heatmap.2(as.matrix(x), scale = "row", col = bluered(276), trace = "none", dendrogram = "col", key.title = NULL, density.info = "none", margins = c(16,9), cexRow = 1, cexCol = 0.4)

# Top 20 and Bottom 20
df_DRG <- df %>% mutate(total_c = Average.Covered.Charges*Total.Discharges,
                        total_p = Average.Medicare.Payments*Total.Discharges) %>% 
  group_by(DRG.Definition) %>%
  summarise(total_charges=sum(total_c), total_paid = sum(total_p),
            ratio_paid_charged = sum(total_p)/sum(total_c),
            Total.Discharges = sum(Total.Discharges))  %>% 
  top_n(20, wt = total_charges) %>%
  arrange(desc(total_charges))


x1 = data.frame(df_DRG)


df_DRG <- df %>% mutate(total_c = Average.Covered.Charges*Total.Discharges,
                        total_p = Average.Medicare.Payments*Total.Discharges) %>% 
  group_by(DRG.Definition) %>%
  summarise(total_charges=sum(total_c), total_paid = sum(total_p),
            ratio_paid_charged = sum(total_p)/sum(total_c),
            Total.Discharges = sum(Total.Discharges))  %>% 
  top_n(-20, wt = total_charges) %>%
  arrange(desc(total_charges))


x2 = data.frame(df_DRG)
x = rbind(x1,x2)

rownames(x) = x$DRG.Definition
x = x[,2:ncol(x)]
x = t(x)
rownames(x) = c("Total Charges", "Total Paid","Ratio: Paid/Charged","Total Discharges")
heatmap.2(as.matrix(x), scale = "row", col = bluered(276), trace = "none", dendrogram = "col", key.title = NULL, density.info = "none", margins = c(16,9), cexRow = 1, cexCol = 0.4)

# Paid to charged ratios across providers for a particular DRG 
df_Provider <- df %>% filter(DRG_code == "470") %>% mutate(total_c = Average.Covered.Charges*Total.Discharges,
                                                           total_p = Average.Medicare.Payments*Total.Discharges) %>% 
  group_by(Provider.Name) %>%
  summarise(total_charges=sum(total_c), total_paid = sum(total_p),
            ratio_paid_charged = sum(total_p)/sum(total_c),
            Total.Discharges = sum(Total.Discharges)) %>% 
  arrange(desc(total_charges))

df_Provider %>%
  ggplot(aes(x= total_charges, y=total_paid)) +
  geom_point() +
  xlab("Total charged to Medicare ($)") +
  ylab("Total paid by Medicare ($)") +
  ggtitle("Charges versus payments for Medicare (DRG 470) by Provider") +
  coord_flip()+ 
  theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(),
        panel.background = element_blank(), axis.line = element_line(colour = "black")) 


# Top 20 and Bottom 20
df_DRG <- df %>% filter(DRG_code == "470") %>%
  mutate(ratio_paid_charged = Average.Medicare.Payments/Average.Covered.Charges) %>% 
  top_n(10, wt = ratio_paid_charged) %>% 
  arrange(desc(ratio_paid_charged)) %>% 
  select(DRG_code,Provider.Name,Average.Medicare.Payments,Average.Covered.Charges,ratio_paid_charged, Total.Discharges)


x1 = data.frame(df_DRG)


df_DRG <- df %>% filter(DRG_code == "470") %>%
  mutate(ratio_paid_charged = Average.Medicare.Payments/Average.Covered.Charges) %>% 
  top_n(-10, wt = ratio_paid_charged) %>% 
  arrange(desc(ratio_paid_charged)) %>% 
  select(DRG_code,Provider.Name,Average.Medicare.Payments,Average.Covered.Charges,ratio_paid_charged, Total.Discharges)


x2 = data.frame(df_DRG)

x = rbind(x1,x2)

### Heatmap for DRG 470
# Billing and payments as per the provider
df_Provider <- df %>% filter(DRG_code == "470") %>% mutate(total_c = Average.Covered.Charges*Total.Discharges,
                                                           total_p = Average.Medicare.Payments*Total.Discharges) %>% 
  group_by(Provider.Name) %>%
  summarise(total_charges=sum(total_c), total_paid = sum(total_p),
            ratio_paid_charged = sum(total_p)/sum(total_c),
            Total.Discharges = sum(Total.Discharges)) %>% 
  arrange(desc(total_charges))

x = data.frame(df_Provider)
rownames(x) = x$Provider.Name
x = x[,2:ncol(x)]
x = t(x)
rownames(x) = c("Total Charges", "Total Paid","Ratio: Paid/Charged","Total Discharges")
heatmap.2(as.matrix(x), scale = "row", col = bluered(276), trace = "none", dendrogram = "col", key.title = NULL, density.info = "none", margins = c(13,9), cexRow = 1, cexCol = 0.5)

## Exploring the MDC categories
df_MDC <- df %>% mutate(total_c = Average.Covered.Charges*Total.Discharges,
                        total_p = Average.Medicare.Payments*Total.Discharges) %>% 
  group_by(MDC_desc) %>%
  summarise(total_charges=sum(total_c), total_paid = sum(total_p),
            ratio_paid_charged = sum(total_p)/sum(total_c)) %>%
  arrange(desc(total_charges))

knitr::kable(df_MDC)


df_MDC %>% ggplot(aes(x=reorder(MDC_desc, total_charges), y=total_charges)) +
  geom_bar(stat='identity') +
  xlab("MDC") +
  ylab("Total charges by MDC ($)") +
  theme(axis.text = element_text(size = 8), axis.text.x = element_text(angle = 90)) +
  ggtitle("Most billed by MDC in Medicare") +
  coord_flip()+ 
  theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(),
        panel.background = element_blank(), axis.line = element_line(colour = "black"),
        axis.text.x = element_text(angle = 90, hjust = 1))


df_MDC %>% arrange(desc(total_paid)) %>% 
  ggplot(aes(x=reorder(MDC_desc, total_paid), y=total_paid)) +
  geom_bar(stat='identity') +
  xlab("MDC") +
  ylab("Total paid by MDC ($)") +
  theme(axis.text = element_text(size = 8), axis.text.x = element_text(angle = 90)) +
  ggtitle("Most paid MDC in Medicare") +
  coord_flip()+ 
  theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(),
        panel.background = element_blank(), axis.line = element_line(colour = "black"),
        axis.text.x = element_text(angle = 90, hjust = 1))


df_MDC %>% arrange(desc(ratio_paid_charged)) %>% 
  ggplot(aes(x=reorder(MDC_desc, ratio_paid_charged), y=ratio_paid_charged)) +
  geom_bar(stat='identity') +
  xlab("MDC") +
  ylab("Total paid by MDC ($)") +
  theme(axis.text = element_text(size = 8), axis.text.x = element_text(angle = 90)) +
  ggtitle("Highest Paid-to-Charged Ratio by MDC in Medicare") +
  coord_flip()+ 
  theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(),
        panel.background = element_blank(), axis.line = element_line(colour = "black"),
        axis.text.x = element_text(angle = 90, hjust = 1))


### Incorporating MDC along with DRGs
x = df %>% group_by(Provider.Id, Provider.Name,DRG_code,Hospital.Referral.Region.Description, MDC) %>%
  summarise(n=n())
x = x %>% group_by(Provider.Name, MDC) %>% mutate(count = n())


mdc$size = as.numeric(str_sub(mdc$DRG_codes,-3,-1)) - as.numeric(str_sub(mdc$DRG_codes,1,3))

getMDC_size <- function(x)
{
  for (j in seq_along(mdc$MDC))
  {
    if((as.numeric(x) == mdc$MDC[j]) )
    {
      return(mdc$size[j])
      break
    }
  }
}

x$MDC_size <- map_chr(x$MDC, getMDC_size)
x$DRGtoMDC = (as.numeric(x$count)/as.numeric(x$MDC_size))*100

## Density plots

x1 = x %>% filter(MDC ==5)
x2 = x1[!duplicated(x1$Provider.Id),]
plot(density(x2$DRGtoMDC), xlab = "Percentage of DRG procedures within MDC",main = mdc$Description[mdc$MDC == 5],xlim = c(0,100))


x1 = x %>% filter(MDC ==4)
x2 = x1[!duplicated(x1$Provider.Id),]
plot(density(x2$DRGtoMDC), xlab = "Percentage of DRG procedures within MDC",main = mdc$Description[mdc$MDC == 4],xlim = c(0,100))

x1 = x %>% filter(MDC ==3)
x2 = x1[!duplicated(x1$Provider.Id),]
plot(density(x2$DRGtoMDC), xlab = "Percentage of DRG procedures within MDC",main = mdc$Description[mdc$MDC == 3],xlim = c(0,100))

x1 = x %>% filter(MDC ==11)
x2 = x1[!duplicated(x1$Provider.Id),]
plot(density(x2$DRGtoMDC), xlab = "Percentage of DRG procedures within MDC",main = mdc$Description[mdc$MDC == 11],xlim = c(0,100))


x1 = x %>% filter(MDC == 20)
x2 = x1[!duplicated(x1$Provider.Id),]
plot(density(x2$DRGtoMDC), xlab = "Percentage of DRG procedures within MDC",main = mdc$Description[mdc$MDC == 20],xlim = c(0,min(100, max(x2$DRGtoMDC))))

for(i in 1:length(a)){
  x1 = x %>% filter(MDC == a[i])
  x2 = x1[!duplicated(x1$Provider.Id),]
  plot(density(x2$DRGtoMDC), xlab = "Percentage of DRG procedures within MDC",main = mdc$Description[mdc$MDC == a[i]],xlim = c(0,min(100, max(x2$DRGtoMDC))))
  
}

# Boxplots

x1 = x %>% filter(MDC ==5)
x2 = x1[!duplicated(x1$Provider.Id),]
boxplot(x2$DRGtoMDC, xlab = "Providers",main = mdc$Description[mdc$MDC == 5], ylab = "Percentage of DRG procedures within MDC",ylim = c(0,min(100, max(x2$DRGtoMDC))))




# Jitter plot of two different MDCs
### Example 1

# Jitter plot of two different MDCs
x1 = data.frame(x %>% filter(MDC ==4))
x2 = data.frame(x %>% filter(MDC ==5))
x1 = x1[!duplicated(x1$Provider.Id),]
x2 = x2[!duplicated(x2$Provider.Id),]
x3 = merge(x1,x2,by = "Provider.Id", all = FALSE)
x3 %>%
  ggplot(aes(x= DRGtoMDC.x, y=DRGtoMDC.y)) +
  geom_point(col = "darkred") +
  xlab(mdc$Description[mdc$MDC == 4]) +
  ylab(mdc$Description[mdc$MDC == 5]) +
  ggtitle("Variation in percentage of DRGs in two different MDCs") +
  theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(),
        panel.background = element_blank(), axis.line = element_line(colour = "black")) +
  xlim(0, 100) + ylim(0, 100)



# Jitter plot of two different MDCs
x1 = data.frame(x %>% filter(MDC ==11))
x2 = data.frame(x %>% filter(MDC ==5))
x1 = x1[!duplicated(x1$Provider.Id),]
x2 = x2[!duplicated(x2$Provider.Id),]
x3 = merge(x1,x2,by = "Provider.Id", all = FALSE)
x3 %>%
  ggplot(aes(x= DRGtoMDC.x, y=DRGtoMDC.y)) +
  geom_point(col = "darkgreen") +
  xlab(mdc$Description[mdc$MDC == 11]) +
  ylab(mdc$Description[mdc$MDC == 5]) +
  ggtitle("Variation in percentage of DRGs in two different MDCs") +
  theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(),
        panel.background = element_blank(), axis.line = element_line(colour = "black")) +
  xlim(0, 100) + ylim(0, 100)


### Example 2
# Jitter plot of two different MDCs
x1 = data.frame(x %>% filter(MDC ==6))
x2 = data.frame(x %>% filter(MDC ==8))
x1 = x1[!duplicated(x1$Provider.Id),]
x2 = x2[!duplicated(x2$Provider.Id),]
x3 = merge(x1,x2,by = "Provider.Id", all = FALSE)
x3 %>%
  ggplot(aes(x= DRGtoMDC.x, y=DRGtoMDC.y)) +
  geom_point(col = "darkblue") +
  xlab(mdc$Description[mdc$MDC == 6]) +
  ylab(mdc$Description[mdc$MDC == 18]) +
  ggtitle("Variation in percentage of DRGs in two different MDCs") +
  theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(),
        panel.background = element_blank(), axis.line = element_line(colour = "black")) +
  xlim(0, 100) + ylim(0, 100)



# Jitter plot of two different MDCs
x1 = data.frame(x %>% filter(MDC ==8))
x2 = data.frame(x %>% filter(MDC ==18))
x1 = x1[!duplicated(x1$Provider.Id),]
x2 = x2[!duplicated(x2$Provider.Id),]
x3 = merge(x1,x2,by = "Provider.Id", all = FALSE)
x3 %>%
  ggplot(aes(x= DRGtoMDC.x, y=DRGtoMDC.y)) +
  geom_point(col = "darkred") +
  xlab(mdc$Description[mdc$MDC == 8]) +
  ylab(mdc$Description[mdc$MDC == 18]) +
  ggtitle("Variation in percentage of DRGs in two different MDCs") +
  theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(),
        panel.background = element_blank(), axis.line = element_line(colour = "black")) +
  xlim(0, 100) + ylim(0, 100)



## Exploring features in LA region, DRG code 871
### Incorporating MDC along with DRGs
x = df %>% group_by(Provider.Id, Provider.Name,DRG_code,Hospital.Referral.Region.Description, MDC) %>%
  summarise(n=n())
x = x %>% group_by(Provider.Name, MDC) %>% mutate(count = n())


mdc$size = as.numeric(str_sub(mdc$DRG_codes,-3,-1)) - as.numeric(str_sub(mdc$DRG_codes,1,3))

getMDC_size <- function(x)
{
  for (j in seq_along(mdc$MDC))
  {
    if((as.numeric(x) == mdc$MDC[j]) )
    {
      return(mdc$size[j])
      break
    }
  }
}

x$MDC_size <- map_chr(x$MDC, getMDC_size)
x$DRGtoMDC = (as.numeric(x$count)/as.numeric(x$MDC_size))*100

x1 = x[!duplicated(x$Provider.Id),]

## Exploring features in LA region, DRG code 871
x = df %>% filter(DRG_code == "871" & Hospital.Referral.Region.Description == "CA - Los Angeles")
x = data.frame(x)
x = x[,c(2,3,9:12)]
x = merge(x,x1[,c(1,9)], by = "Provider.Id", all.x = TRUE)
rownames(x) = x$Provider.Name
x = x[,-c(1:2)]
x = log10(x) #Log normalization
names = rownames(x)

x = apply(x, 2, scale)
x = data.frame(x)
x$Provider.Names = names

x = as_tibble(x)
plot(density(x$DRGtoMDC), xlab = "Normalized % of DRG in MDC",main = "Density of providers")
plot(density(x$Average.Medicare.Payments), xlab = "Normalized Average Medicare Payments",main = "Density of providers")
plot(density(x$Average.Total.Payments), xlab = "Normalized Average.Total.Payments",main = "Density of providers")
heatmap.2(cor(x[,1:5]), margins = c(10,10), cexRow = 0.8, cexCol = 0.8, col = bluered(276), trace = "none", key.title = "Correlation", main = "DRG 871, HRR = CA - Los Angeles")

x
x %>% filter(Average.Covered.Charges < (-0.5) & DRGtoMDC > 0.5)


## Exploring features in LA region, DRG code 392
x = df %>% filter(DRG_code == "392" & Hospital.Referral.Region.Description == "CA - Los Angeles")
x = data.frame(x)
x = x[,c(2,3,9:12)]
x = merge(x,x1[,c(1,9)], by = "Provider.Id", all.x = TRUE)
rownames(x) = x$Provider.Name
x = x[,-c(1:2)]
x = log10(x) #Log normalization
names = rownames(x)

x = apply(x, 2, scale)
x = data.frame(x)
x$Provider.Names = names

x = as_tibble(x)
plot(density(x$DRGtoMDC), xlab = "Normalized % of DRG in MDC",main = "Density of providers")
plot(density(x$Average.Medicare.Payments), xlab = "Normalized Average Medicare Payments",main = "Density of providers")
plot(density(x$Average.Total.Payments), xlab = "Normalized Average.Total.Payments",main = "Density of providers")
heatmap.2(cor(x[,1:5]), margins = c(10,10), cexRow = 0.8, cexCol = 0.8, col = bluered(276), trace = "none", key.title = "Correlation", main = "DRG 871, HRR = CA - Los Angeles")

x
x %>% filter(Average.Medicare.Payments < (-0.5) & DRGtoMDC > 0.5)




## Exploring features in LA region, DRG code 301
x = df %>% filter(DRG_code == "301" & Hospital.Referral.Region.Description == "CA - Los Angeles")
x = data.frame(x)
x = x[,c(2,3,9:12)]
x = merge(x,x1[,c(1,9)], by = "Provider.Id", all.x = TRUE)
rownames(x) = x$Provider.Name
x = x[,-c(1:2)]
x = log10(x) #Log normalization
names = rownames(x)

x = apply(x, 2, scale)
x = data.frame(x)
x$Provider.Names = names

x = as_tibble(x)
plot(density(x$DRGtoMDC), xlab = "Normalized % of DRG in MDC",main = "Density of providers")
plot(density(x$Average.Medicare.Payments), xlab = "Normalized Average Medicare Payments",main = "Density of providers")
plot(density(x$Average.Total.Payments), xlab = "Normalized Average.Total.Payments",main = "Density of providers")
heatmap.2(cor(x[,1:5]), margins = c(10,10), cexRow = 0.8, cexCol = 0.8, col = bluered(276), trace = "none", key.title = "Correlation", main = "DRG 871, HRR = CA - Los Angeles")

x
x %>% filter(Average.Medicare.Payments < (-0.5) & DRGtoMDC > 0.5)


### Another HRR region

## Exploring features in LA region, DRG code 871
x = df %>% filter(DRG_code == "871" & Hospital.Referral.Region.Description == "AL - Birmingham")
x = data.frame(x)
x = x[,c(2,3,9:12)]
x = merge(x,x1[,c(1,9)], by = "Provider.Id", all.x = TRUE)
rownames(x) = x$Provider.Name
x = x[,-c(1:2)]
x = log10(x) #Log normalization
names = rownames(x)

x = apply(x, 2, scale)
x = data.frame(x)
x$Provider.Names = names

x = as_tibble(x)
plot(density(x$DRGtoMDC), xlab = "Normalized % of DRG in MDC",main = "Density of providers")
plot(density(x$Average.Medicare.Payments), xlab = "Normalized Average Medicare Payments",main = "Density of providers")
plot(density(x$Average.Total.Payments), xlab = "Normalized Average.Total.Payments",main = "Density of providers")
heatmap.2(cor(x[,1:5]), margins = c(10,10), cexRow = 0.8, cexCol = 0.8, col = bluered(276), trace = "none", key.title = "Correlation", main = "DRG 871, HRR = AL - Birmingham")

x
x %>% filter(Average.Medicare.Payments < (-0.5) & DRGtoMDC > 0.5)

### 392

## Exploring features in LA region, DRG code 871
x = df %>% filter(DRG_code == "392" & Hospital.Referral.Region.Description == "AL - Birmingham")
x = data.frame(x)
x = x[,c(2,3,9:12)]
x = merge(x,x1[,c(1,9)], by = "Provider.Id", all.x = TRUE)
rownames(x) = x$Provider.Name
x = x[,-c(1:2)]
x = log10(x) #Log normalization
names = rownames(x)

x = apply(x, 2, scale)
x = data.frame(x)
x$Provider.Names = names

x = as_tibble(x)
plot(density(x$DRGtoMDC), xlab = "Normalized % of DRG in MDC",main = "Density of providers")
plot(density(x$Average.Medicare.Payments), xlab = "Normalized Average Medicare Payments",main = "Density of providers")
plot(density(x$Average.Total.Payments), xlab = "Normalized Average.Total.Payments",main = "Density of providers")
heatmap.2(cor(x[,1:5]), margins = c(10,10), cexRow = 0.8, cexCol = 0.8, col = bluered(276), trace = "none", key.title = "Correlation", main = "DRG 392, HRR = AL - Birmingham")

x
x %>% filter(Average.Medicare.Payments < (-0.5) & DRGtoMDC > 0.5)


## Exploring features in AL region, 301
x = df %>% filter(DRG_code == "301" & Hospital.Referral.Region.Description == "AL - Birmingham")
x = data.frame(x)
x = x[,c(2,3,9:12)]
x = merge(x,x1[,c(1,9)], by = "Provider.Id", all.x = TRUE)
rownames(x) = x$Provider.Name
x = x[,-c(1:2)]
x = log10(x) #Log normalization
names = rownames(x)

x = apply(x, 2, scale)
x = data.frame(x)
x$Provider.Names = names

x = as_tibble(x)
plot(density(x$DRGtoMDC), xlab = "Normalized % of DRG in MDC",main = "Density of providers")
plot(density(x$Average.Medicare.Payments), xlab = "Normalized Average Medicare Payments",main = "Density of providers")
plot(density(x$Average.Total.Payments), xlab = "Normalized Average.Total.Payments",main = "Density of providers")
heatmap.2(cor(x[,1:5]), margins = c(10,10), cexRow = 0.8, cexCol = 0.8, col = bluered(276), trace = "none", key.title = "Correlation", main = "DRG 301, HRR = AL - Birmingham")

x
x %>% filter(Average.Medicare.Payments < (-0.5) & DRGtoMDC > 0.5)



## Exploring features in LA region, DRG code 871
x = df %>% filter(DRG_code == "871" & Hospital.Referral.Region.Description == "MO - Springfield")
x = data.frame(x)
x = x[,c(2,3,9:12)]
x = merge(x,x1[,c(1,9)], by = "Provider.Id", all.x = TRUE)
rownames(x) = x$Provider.Name
x = x[,-c(1:2)]
x = log10(x) #Log normalization
names = rownames(x)

x = apply(x, 2, scale)
x = data.frame(x)
x$Provider.Names = names

x = as_tibble(x)
plot(density(x$DRGtoMDC), xlab = "Normalized % of DRG in MDC",main = "Density of providers")
plot(density(x$Average.Medicare.Payments), xlab = "Normalized Average Medicare Payments",main = "Density of providers")
plot(density(x$Average.Total.Payments), xlab = "Normalized Average.Total.Payments",main = "Density of providers")
heatmap.2(cor(x[,1:5]), margins = c(10,10), cexRow = 0.8, cexCol = 0.8, col = bluered(276), trace = "none", key.title = "Correlation", main = "DRG 871, HRR = MO - Springfield")

x
x %>% filter(Average.Medicare.Payments < (-0.5) & DRGtoMDC > 0.5)

### 392

## Exploring features in LA region, DRG code 871
x = df %>% filter(DRG_code == "392" & Hospital.Referral.Region.Description == "MO - Springfield")
x = data.frame(x)
x = x[,c(2,3,9:12)]
x = merge(x,x1[,c(1,9)], by = "Provider.Id", all.x = TRUE)
rownames(x) = x$Provider.Name
x = x[,-c(1:2)]
x = log10(x) #Log normalization
names = rownames(x)

x = apply(x, 2, scale)
x = data.frame(x)
x$Provider.Names = names

x = as_tibble(x)
plot(density(x$DRGtoMDC), xlab = "Normalized % of DRG in MDC",main = "Density of providers")
plot(density(x$Average.Medicare.Payments), xlab = "Normalized Average Medicare Payments",main = "Density of providers")
plot(density(x$Average.Total.Payments), xlab = "Normalized Average.Total.Payments",main = "Density of providers")
heatmap.2(cor(x[,1:5]), margins = c(10,10), cexRow = 0.8, cexCol = 0.8, col = bluered(276), trace = "none", key.title = "Correlation", main = "DRG 392, HRR = MO - Springfield")

x
x %>% filter(Average.Medicare.Payments < (-0.5) & DRGtoMDC > 0.5)


## Exploring features in AL region, 301
x = df %>% filter(DRG_code == "301" & Hospital.Referral.Region.Description == "MO - Springfield")
x = data.frame(x)
x = x[,c(2,3,9:12)]
x = merge(x,x1[,c(1,9)], by = "Provider.Id", all.x = TRUE)
rownames(x) = x$Provider.Name
x = x[,-c(1:2)]
x = log10(x) #Log normalization
names = rownames(x)

x = apply(x, 2, scale)
x$Provider.Names = names

x = as_tibble(x)
plot(density(x$DRGtoMDC), xlab = "Normalized % of DRG in MDC",main = "Density of providers")
plot(density(x$Average.Medicare.Payments), xlab = "Normalized Average Medicare Payments",main = "Density of providers")
plot(density(x$Average.Total.Payments), xlab = "Normalized Average.Total.Payments",main = "Density of providers")
heatmap.2(cor(x[,1:5]), margins = c(10,10), cexRow = 0.8, cexCol = 0.8, col = bluered(276), trace = "none", key.title = "Correlation", main = "DRG 301, HRR = MO - Springfield")

x
x %>% filter(Average.Medicare.Payments < (-0.5) & DRGtoMDC > 0.5)

# Including other State-wide and Nation-wide features
## Exploring features in LA region, DRG code 301
### Incorporating MDC along with DRGs
x = df %>% group_by(Provider.Id, Provider.Name,DRG_code,Hospital.Referral.Region.Description, MDC) %>%
  summarise(n=n())
x = x %>% group_by(Provider.Name, MDC) %>% mutate(count = n())


mdc$size = as.numeric(str_sub(mdc$DRG_codes,-3,-1)) - as.numeric(str_sub(mdc$DRG_codes,1,3))

getMDC_size <- function(x)
{
  for (j in seq_along(mdc$MDC))
  {
    if((as.numeric(x) == mdc$MDC[j]) )
    {
      return(mdc$size[j])
      break
    }
  }
}

x$MDC_size <- map_chr(x$MDC, getMDC_size)
x$DRGtoMDC = (as.numeric(x$count)/as.numeric(x$MDC_size))*100

x1 = x[!duplicated(x$Provider.Id),]

## Exploring features in entire USA
x = df 
x = data.frame(x)
x = x[!duplicated(x$Provider.Name),]
x = x[,c(2,3,9:12)]
x = merge(x,x1[,c(1,9)], by = "Provider.Id", all.x = TRUE)
rownames(x) = x$Provider.Name
x = x[,-c(1:2)]
x = log10(x) #Log normalization
names = rownames(x)
colnames(x) = paste0("N.",colnames(x))

N.x = x
N.x$Provider.Names = rownames(N.x)

# State

## Exploring features in LA region, DRG code 301
# Exploring features in entire USA
x = df %>% filter(Provider.State == "CA")
x = data.frame(x)
x = x[!duplicated(x$Provider.Name),]
x = x[,c(2,3,9:12)]
x = merge(x,x1[,c(1,9)], by = "Provider.Id", all.x = TRUE)
rownames(x) = x$Provider.Name
x = x[,-c(1:2)]
x = log10(x) #Log normalization
names = rownames(x)
colnames(x) = paste0("S.",colnames(x))

S.x = x
S.x$Provider.Names = rownames(S.x)

### Subsetted x ###

## Exploring features in LA region, DRG code 301
## Exploring features in LA region, DRG code 301
x = df %>% filter(DRG_code == "301" & Hospital.Referral.Region.Description == "CA - Los Angeles")
x = data.frame(x)
x = x[,c(2,3,9:12)]
x = merge(x,x1[,c(1,9)], by = "Provider.Id", all.x = TRUE)
rownames(x) = x$Provider.Name
x = x[,-c(1:2)]
x = log10(x) #Log normalization
names = rownames(x)

x = apply(x, 2, scale)
x = data.frame(x)
x$Provider.Names = names

## Combining the two x

All.x = merge(x,N.x, by = "Provider.Names", all.x = TRUE)
rownames(All.x) = All.x$Provider.Names
## Combining the two x
All.x = merge(All.x,S.x, by = "Provider.Names", all.x = TRUE)
All.x = All.x[,-1]

All.x = as_tibble(All.x)
heatmap.2(cor(All.x), margins = c(10,10), cexRow = 0.8, cexCol = 0.8, col = bluered(276), trace = "none", key.title = "Correlation", main = "DRG 301, HRR = CA - Los Angeles")
