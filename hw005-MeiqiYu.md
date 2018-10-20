hw005-MeiqiYu.Rmd
================
Meiqi Yu
2018-10-15

## Introduction

The goal of this assignment is to dig further in factor and plot
management. This assignment includes four parts.

For this assignment, I will be using the gapminder dataframe.

``` r
suppressPackageStartupMessages(library(tidyverse))
suppressPackageStartupMessages(library(plotly))
library(knitr)
library(gapminder)
```

## Part 1: Factor management

For this part, I choose the varibles `country` and `continent`. The
first thing to do is to ensure the varibles I’m exploring are indeed
facrots.

### Are `country` and `continent` factors?

``` r
is.factor(gapminder$continent)
```

    ## [1] TRUE

``` r
is.factor(gapminder$country)
```

    ## [1] TRUE

### Look at the levels of `continent`

``` r
fct_count(gapminder$continent) %>% 
kable()
```

| f        |   n |
| :------- | --: |
| Africa   | 624 |
| Americas | 300 |
| Asia     | 396 |
| Europe   | 360 |
| Oceania  |  24 |

### Drop Oceania -level and factor

Let’s drop all the rows of `Oceania`, the new data frame is named as
`gap_drop_oceania`.

``` r
gap_drop_oceania <- gapminder %>%
    filter(continent != "Oceania")
fct_count(gap_drop_oceania$continent) %>% 
kable()
```

| f        |   n |
| :------- | --: |
| Africa   | 624 |
| Americas | 300 |
| Asia     | 396 |
| Europe   | 360 |
| Oceania  |   0 |

``` r
nrow(gapminder)
```

    ## [1] 1704

``` r
nrow(gap_drop_oceania)#check the number of rows before and after change
```

    ## [1] 1680

From the table above, we can know that all the rows of Oceania have been
removed, but the factor still remains. The original `gapminder` data
frame has 1704 rows and 5 factors. The new `gap_drop_oceania` data frame
has 1680 rows and 5 factors.

Now, let’s drop the unused factor, the new data frame is named as
`gap_no_oceania`.

``` r
gap_no_oceania <- gap_drop_oceania %>% 
  droplevels()
fct_count(gap_no_oceania$continent) %>% 
kable()
```

| f        |   n |
| :------- | --: |
| Africa   | 624 |
| Americas | 300 |
| Asia     | 396 |
| Europe   | 360 |

``` r
nrow(gap_no_oceania)#check the number of rows after change
```

    ## [1] 1680

It looks like that Oceania has been removed as an unused factor. The new
`gap_no_oceania` data frame has 1680 rows and 4 factors.

### Reorder the levels of `country` and `continent`

This task is to use the forcats package to change the order of the
factor levels, based on a principled summary of one of the quantitative
variables. I will reorder the columns of `gap_no_oceania` based on the
maximum value of the pop. I plot the data frame in order to show the
order better,

``` r
gap_no_oceania %>%
    mutate(continent = fct_reorder(continent, pop, max)) %>%
    ggplot(aes(continent, pop, fill = continent)) +
    scale_y_log10()+
    geom_violin() +
    labs(title = "population each continent")+
    theme(plot.title = element_text(hjust = 0.5)) #center the title
```

![](hw005-MeiqiYu_files/figure-gfm/unnamed-chunk-6-1.png)<!-- -->

Now, let’s explore the effects of `arrange()` individually.

``` r
gap_no_oceania %>%
    group_by(continent, pop) %>%
    summarise(max_pop = max(pop)) %>%
    arrange(desc(max_pop)) %>%
    ggplot(aes(continent,pop, fill = continent)) +
    scale_y_log10()+
    geom_violin() +
    labs(title = "population each continent")+
    theme(plot.title = element_text(hjust = 0.5)) #center the title
```

![](hw005-MeiqiYu_files/figure-gfm/unnamed-chunk-7-1.png)<!-- -->

Comparing these two plots above, we can know that:

  - `arrange()` only has effects on rows, not on factors, so the order
    of `continent` does NOT change in the second plot.

  - factor reordering coupled with `arrange()` does have effects on the
    order of the factor, so `continent` is in the order of the increase
    of the maximum value of pop.

## Part 2: File I/O

### Make a new data frame

To begin with this part, I will first make a small and reasonable data
frame called `gap_Europe`. This data frame includes all the lifeExp of
each European countries after 1972. We can see see that it is in the
order of alphabet.

``` r
gap_Europe <- gapminder %>%
    filter(continent == "Europe"& year >= 1972) %>% 
    select(country,lifeExp)
gap_Europe %>%     
head() # just show the first few lines of the long table 
```

<div data-pagedtable="false">

<script data-pagedtable-source type="application/json">
{"columns":[{"label":["country"],"name":[1],"type":["fctr"],"align":["left"]},{"label":["lifeExp"],"name":[2],"type":["dbl"],"align":["right"]}],"data":[{"1":"Albania","2":"67.690"},{"1":"Albania","2":"68.930"},{"1":"Albania","2":"70.420"},{"1":"Albania","2":"72.000"},{"1":"Albania","2":"71.581"},{"1":"Albania","2":"72.950"}],"options":{"columns":{"min":{},"max":[10]},"rows":{"min":[10],"max":[10]},"pages":{}}}
  </script>

</div>

To see it clearly, I put it into a graph.

``` r
gap_Europe %>%
  ggplot(aes(country, lifeExp)) +
  geom_point() +
  labs(title = "LifExp in Europe after 1972") +
  theme(axis.text.x = element_text(angle = 90))+
  # use the theme function to rotate x axis to avoid overlapping
  theme(plot.title = element_text(hjust = 0.5)) #center the title
```

![](hw005-MeiqiYu_files/figure-gfm/unnamed-chunk-9-1.png)<!-- -->

### Make the data frame non-alphabetical

Now let’s make it non-alphabetical.The new data frame is called
`gap_Europe_reorder`. I make it in the order of the increase of the
minimum value of lifeExp and plot the graph below which looks more
reasonable.

``` r
gap_Europe_reorder<- gap_Europe %>% 
  mutate(country = fct_reorder(country, lifeExp, min))
gap_Europe_reorder %>% 
  ggplot(aes(country, lifeExp)) +
  geom_point() +
  labs(title = "Life expectancy in Europe after 1972") +
  theme(axis.text.x = element_text(angle = 90))+
  # use the theme function to rotate x axis to avoid overlapping
  theme(plot.title = element_text(hjust = 0.5)) #center the title
```

![](hw005-MeiqiYu_files/figure-gfm/unnamed-chunk-10-1.png)<!-- -->

### Experiment with `write_csv()/read_csv()`

``` r
write_csv(gap_Europe_reorder,"gap_Europe_reorder.csv")
read_csv("gap_Europe_reorder.csv") %>% 
  ggplot(aes(country, lifeExp)) +
  geom_point() +
  labs(title = "Life expectancy in Europe after 1972") +
  theme(axis.text.x = element_text(angle = 90))+
  # use the theme function to rotate x axis to avoid overlapping
  theme(plot.title = element_text(hjust = 0.5)) #center the title
```

    ## Parsed with column specification:
    ## cols(
    ##   country = col_character(),
    ##   lifeExp = col_double()
    ## )

![](hw005-MeiqiYu_files/figure-gfm/unnamed-chunk-11-1.png)<!-- -->

It’s abvious that the order does NOT survive - it is alphabetical again
now.

### Experiment with `saveRDS()/readRDS()`

``` r
saveRDS(gap_Europe_reorder,"gap_Europe_reorder.rds")
readRDS("gap_Europe_reorder.rds") %>% 
  ggplot(aes(country, lifeExp)) +
  geom_point() +
  labs(title = "Life expectancy in Europe after 1972") +
  theme(axis.text.x = element_text(angle = 90))+
  # use the theme function to rotate x axis to avoid overlapping
  theme(plot.title = element_text(hjust = 0.5)) #center the title
```

![](hw005-MeiqiYu_files/figure-gfm/unnamed-chunk-12-1.png)<!-- -->

Yes, the order keeps the same through the round trip of
saveRDS()/readRDS().

### Experiment with `dput()/dget()`

``` r
dput(gap_Europe_reorder,"gap_Europe_reorder.dput")
dget("gap_Europe_reorder.dput") %>% 
  ggplot(aes(country, lifeExp)) +
  geom_point() +
  labs(title = "Life expectancy in Europe after 1972") +
  theme(axis.text.x = element_text(angle = 90))+
  # use the theme function to rotate x axis to avoid overlapping
  theme(plot.title = element_text(hjust = 0.5)) #center the title
```

![](hw005-MeiqiYu_files/figure-gfm/unnamed-chunk-13-1.png)<!-- -->

Yes, the order also keeps the same through the round trip of
dput()/dget().

## Part 3: Visualization design

### Remark a figure

In this part, the task is to remark a new figure, in light of something
I learned in the recent class meetings about visualization design and
color. I am going to replot the first figure I plotted in assignment 2.
In this figure, I meaned to show the relationship between lifeExp and
gdpPercap, but the plot looks squished at the bottom.

``` r
ggplot(gapminder,aes(x=lifeExp, y=(gdpPercap)))+
  geom_point(color='steelblue',size=1)
```

![](hw005-MeiqiYu_files/figure-gfm/unnamed-chunk-14-1.png)<!-- -->

Now, let’s remark this figure and call it `new_gap`.

``` r
new_gap <-gapminder %>% 
  group_by(continent,year) %>% 
  ggplot(aes(y=lifeExp, x=gdpPercap,color = lifeExp))+
  facet_wrap(~continent)+
  scale_x_log10()+
  geom_point(size=1)+
  labs(title = "Relationship between lifeExp and gdpPercap")+
  theme_bw()+
  theme(plot.title = element_text(hjust = 0.5),
        axis.text  = element_text(size=12),
        strip.background = element_rect(fill = "orange"))
new_gap
```

![](hw005-MeiqiYu_files/figure-gfm/unnamed-chunk-15-1.png)<!-- -->

Now, the figure looks more reasonable based on the following aspects:

  - 1.  by `scale_x_log10()`, the plots do NOT look squished any more.

  - 2.  by `facet_wrap()`, the original figure is faceted into 5 figures
        by continents, so the relationship can be seen more clearly for
        each continent

  - 3.  by `color=`, the new figure shows the value of lifeExp
        intuitivly.

  - 4.  the title is added and centered which completes the figure.

### Convert it into a plotly graph (Results only applicable in .html)

``` r
#new_gap %>% 
#  ggplotly()
```

### Convert it into a 3D plot (Results only applicable in .html)

Now, take Asia for an example, let’s add year to form a z-axis for a 3D
plot.

``` r
#gapminder %>% 
#   filter(continent == "Asia") %>% 
#   plot_ly(
#        x = ~gdpPercap, 
#        y = ~lifeExp, 
#        z = ~year,
#        color = ~country, # color by country
#        type = "scatter3d",
#        mode = "markers",
#        opacity = 0.5) %>% 
#   layout(xaxis = list(type = "log"), 
#       yaxis = list(type = "log" ))  # log x and y
```

### Part 4: Writing figures to file

``` r
new_gap # choose new_gap to save
```

![](hw005-MeiqiYu_files/figure-gfm/unnamed-chunk-18-1.png)<!-- -->

``` r
ggsave("new_gap_file.png",new_gap, width=15, height=10, units = "cm")
```

![load Graph](new_gap_file.png)
