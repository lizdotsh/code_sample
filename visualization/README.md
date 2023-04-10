# Freedman's Bureau Visualization


## Description

Much of this readme will be somewhat redundant, as the visualization itself includes quite a lot of text. Almost all the visualizations I make are closer to a mini-writeup than just pure visualizations, so I figured I should make it similar to how I actually work. 

This visualization is exploring the data processed in the code sample. Details about the particular dataset are explained better in that readme. The goal of this to give an example of how one could examine and interpret that data. All figures in this are made with R's ggplot2. Most figures in this are also processed through plotly's ggploty() function to make them interactive in the HTML document. Details about what it actually visualizes is explained in the document itself. 

## Instructions

Open the visualization.html file in any modern web browser. It should work fine and everything _should_ be self contained. Click the 'code' buttons to see the code that generated each figure. Most of the document is organized into tabsets, so it is actually longer than it first looks. click on each little tabset thing to reveal a new figure in each topic. 

The code itself is available as a raw text file in the form of visualization.qmd. It is a quarto document (2nd gen R markdown, basically) and that file is then knit into the html file using quarto. I have already done this for you so no need.


## Data Sources 

Mostly see the code readme. There are also some extra figures tacked on at the end from another project I made. I go into more detail in the visualization itself. 

Tools used: 
1. tidyverse packages (mostly dplyr) for some minor manipulation / getting into tidy format
2. purrr, I used some nesting / the deframe() function to create a few of the graphs at once. Used the purrr map() function to do this. 
3. ggthemr. An easy to use ggplot theming package. 
4. plotly. Used to get ggplotly()

## Audience 

This audience for this visualization is anyone who is interested in the economic history of the Freedman's Bureau in the Williamsburg Area, specifically over time. This links people between various census records, so you get a picture of a particular person over time. This linking process is very complicated and fraught with endogeniety, however. Much of this visualization is exploring this and what makes people rate the match as good vs bad (mostly in a 1-10 scale). Honestly the topic generally is fascinating though, so anyone interested in Economic history would likely find this somewhat informative. 