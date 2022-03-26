
library(rvest)
library(dplyr)
library(tidyr)
library(ggplot2)
library(stringr)
library(ggrepel)

##################################################################################
#          Script aims to study prices of rooms in chosen polish city            #
#         To run it properly, just change sCityPL, and sCity variables.          #
# Concept taken from https://blog.prokulski.science/2018/06/05/webscrapping-w-r/ #
##################################################################################

# set theme for plots
theme_set(theme_classic())

# Names with and without polish diacretic signes
sCityPL = 'Gdañsk'
sCity = 'Gdansk'

sURL_Offers_start = paste(
    "https://www.olx.pl/nieruchomosci/stancje-pokoje/",
    sCity,
    "/?&page=",
    sep = ''
    )

dfOffers = data.frame(stringsAsFactors = F)

for(i in 1:24) {

    sURL = paste(sURL_Offers_start, i, sep = "")
    page = read_html(sURL) %>%
        html_node("body.offersview.standard") %>%
        html_node("div#innerLayout") %>%
        html_node("div#listContainer") %>%
        html_node("section#body-container")
    page1 = html_children(page)[3] %>%
        html_node("div.content") %>%
        html_node("div.rel.listHandler") %>%
        html_node("table#offers_table") %>%
        html_node("tbody") %>%
        html_nodes("tr.wrap")
    # page1
    
    price = page1 %>%
        html_node("td.offer") %>%
        html_node("div.offer-wrapper") %>%
        html_node("table") %>%
        html_node("tbody") %>%
        html_node("tr") %>%
        html_node("td.wwnormal.tright.td-price") %>%
        html_node("div") %>%
        html_node("p.price") %>%
        html_text() %>%
        gsub(" ", "", .) %>%
        gsub("\n", "",.) %>%
        gsub("z³", "",.) %>%
        as.numeric()
        
    location = page1 %>%
        html_node("td.offer") %>%
        html_node("div.offer-wrapper") %>%
        html_node("table") %>%
        html_node("tbody") %>%
        html_nodes("tr") %>%
        html_node("td.bottom-cell") %>%
        html_node("div.space.rel") %>%
        html_node("p.lheight16") %>%
        html_node("small") %>%
        html_node("span") %>%
        html_text() %>%
        gsub(paste(sCityPL, ', ', sep=''),"", .)
        
    location = location[!is.na(location)]
    # location

    dfOffers = rbind(dfOffers,
        data.frame(District = location, Price = price))
    
}
    sTime = Sys.time() %>%
        format('%d-%m-%Y %H:%M')

    head(dfOffers, n =5)
    summary(dfOffers)
    drop_na(dfOffers)
    #Number of offers in each district
    tapply(
        dfOffers$Price, 
        INDEX = dfOffers$District, 
        FUN = length) %>% 
        sort(decreasing = T)
    
    #Median price in each district
    tapply(
        dfOffers$Price, 
        INDEX = dfOffers$District, 
        FUN = median) %>% 
        sort(decreasing = T) 
    
    #Drop-out of locations with less then 10 offers.
    DistrictsList = count(dfOffers, District) %>% filter(n >= 10)
    DistrictsList
    
    #Drop-out 5% of the most expensive and 5% of the least expensive offers
    dfOff_cleaned = filter(
        dfOffers,
        District %in% DistrictsList$District,
        Price >= quantile(dfOffers$Price, 0.05, na.rm = TRUE),
        Price <= quantile(dfOffers$Price, 0.95, na.rm = TRUE)
    )
    summary(dfOff_cleaned)
    
    tapply(
        dfOff_cleaned$Price, 
        INDEX = dfOff_cleaned$District, 
        FUN = median) %>% 
        sort(decreasing = T)
    
    MedianPerDistrict = tapply(
        dfOff_cleaned$Price, 
        INDEX = dfOff_cleaned$District, 
        FUN = median)
    
    OffersPerDistr = tapply(
        dfOff_cleaned$Price, 
        INDEX = dfOff_cleaned$District, 
        FUN = length)
    
    dfOffersPerDistr = data.frame(
        District = names(OffersPerDistr), 
        Number = unname(OffersPerDistr), 
        MedianPrice = unname(MedianPerDistrict))

    #Number of offers Vs Distric
    ggplot(dfOffersPerDistr) +
        geom_col(
            aes(Number, reorder(District, Number), fill = MedianPrice)
            ) +
        
        labs(
            title = 'Number of offers per district',
            subtitle = sCityPL,
            y = 'District',
            caption = paste('Data extracted from www.olx.pl', sTime)
        ) + 
        
        scale_x_continuous(
            n.breaks = 10
        ) + 
        
        theme(
            legend.position = c(.6, .1),
            legend.direction = 'horizontal'
        ) + 
        
        geom_label(
            aes(Number, reorder(District, Number), label = Number),
            size = 3
        ) +
        
        scale_fill_continuous(
            type = "viridis"
            )
    
    #Histogram
    ggplot(dfOff_cleaned) +
        
        geom_histogram(
            aes(x = Price), 
            binwidth = 100,
             fill = 'green', 
            # colour = 'black'
            ) +

        geom_vline(
            xintercept = median(dfOff_cleaned$Price), 
            colour = 'red'
            ) +
        
        geom_vline(
            xintercept = mean(dfOff_cleaned$Price), 
            colour = 'blue'
            ) +
        
        scale_y_continuous(
            n.breaks = 10
        ) +
        
        scale_x_continuous(
            n.breaks = 10
        ) +
        
        labs(
            title = "Histogram: prices",
            subtitle = sCityPL,
            caption = paste('Data extracted from www.olx.pl', sTime),
            y = ''
            )

    
    #Histogram v2
    ggplot(dfOff_cleaned, aes(x = Price)) +
        
        geom_histogram(
            aes(y=..density..),
            binwidth = 100,
            fill = 'green', 
            # colour = 'black'
        ) +
        
        geom_density(
            col = 'darkgreen',
            fill = 'lightgreen',
            alpha = .6
        ) +
        
        geom_vline(
            xintercept = median(dfOff_cleaned$Price), 
            colour = 'red',
            linetype = 'dashed',
            size = 1
        ) +
        
        geom_vline(
            xintercept = mean(dfOff_cleaned$Price), 
            colour = 'blue',
            linetype = 'dashed',
            size = 1
        ) +
        
        scale_y_continuous(
            n.breaks = 10
        ) +
        
        scale_x_continuous(
            n.breaks = 10
        ) +
        
        labs(
            title = "Histogram: prices",
            subtitle = sCityPL,
            caption = paste('Data extracted from www.olx.pl', sTime),
            y = ''
        ) 
    
    TopPrices = tapply(dfOff_cleaned$Price, INDEX = dfOff_cleaned$District, FUN = median) %>% 
        sort(decreasing = T)
    dfTopPrices = data.frame(District = names(TopPrices), Price = unname(TopPrices))
    dfTopPrices

    #Price MEDIAN vs District
    ggplot(dfTopPrices, aes(Price, reorder(District, Price))) +
        geom_col(
            aes(fill=Price)
            ) +
        labs(
            title = 'Median price Vs District',
            y = 'District',
            x = 'Price, zl',
            subtitle = sCityPL,
            caption = paste('Data extracted from www.olx.pl', sTime)
            ) +
        
        geom_label(
            aes(Price, reorder(District, Price), label = Price),
            size = 3
        ) +
        
        coord_cartesian(
            xlim = c(min(dfTopPrices$Price)*.9, max(dfTopPrices$Price)*1.1)
            ) +
        
        scale_fill_gradient(
            low = 'yellow',
            high = 'red'
        ) +
        
        theme(
            legend.position = c(.9, .4)
        )
        
    
    #Price vs District BOXPLOT
    ggplot(dfOff_cleaned, aes(y = reorder(District, Price), x = Price)) +
        
        geom_boxplot(
            colour = "navyblue", 
            fill = 'blue', 
            alpha = 0.2
            ) +
        
        geom_vline(
            xintercept = mean(dfOff_cleaned$Price), 
            color = "red",
            linetype = 'dashed',
            size = 1
            ) +
        
       labs(
           title = 'Prices range vs District',
           subtitle = sCityPL,
           y = 'District',
           x = 'Price, zl',
           caption = paste('Data extracted from www.olx.pl', sTime)
       )


    #Number of offers vs price

    ggplot(dfOffersPerDistr, aes(x=Number, y=MedianPrice)) +
        geom_point(
            aes(size = Number, col = District)
        ) +
        
        geom_label_repel(
            aes(Number, MedianPrice, label = District),
            size = 3
            # nudge_y = -50
        ) +
        
        labs(
            title = 'Median price vs Number of offers',
            subtitle = sCityPL,
            y = 'Median price, zl',
            x = 'Number of offers',
            caption = paste('Data extracted from www.olx.pl', sTime)
        ) +
        
        scale_color_discrete(
            name = 'District', 
            guide = 'none'
        ) + 
        
        theme(
            panel.grid.major = element_line(colour = 'grey', size = .5),
            panel.grid.minor = element_line(colour = 'grey', size = .5, linetype = 'dashed')
            
        )
