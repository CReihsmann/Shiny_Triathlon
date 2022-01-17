# **Triathlons: Three Times the Data**
### **Analysis of trends and race outcomes based on Age, Country of Origin, and External Factors**

### Motivation

At the beginning of the pandemic with the closure of gyms, emphasis on social distancing, and extra free time, I took up training for triathlons. Like most people starting something new, I found workouts from whatever website was listed first on google and hit the pavement. However, the thing with endurance sports is that they tend to give the mind a lot of time to wander. You’ve got miles to go and you need to distract yourself from the sound of your own wheezing somehow.

Specifically, I began to wonder about triathlons themselves. Are all disciplines equally as important? Do I really need to run that much or can I stay in the air-conditioning on the bike trainer instead? Is training in this swamp of a state going to give me an edge or does that matter in the long run? Is that pain in my side normal or am I dying? The usual.  

With this project, I hoped to answer some of these questions, or at least sound smarter in the process.

### Data Questions

- Broadly, what are the trends, if any, in the sport as a whole?
- What are the correlations for performance in one discipline to overall race success?
- Does the country you come from/train in act as a predictor for success?

### Known Issues And Challenges

The dataset that I used were results from professional athletes. This resulted in a less diverse dataset and limited some avenues of analysis. Limitations imposed by this were:

- Spread of time between top and bottom finishers were very close preventing a z-normalization on race times
- Age groups were more limited than with a normal race with the oldest contestant being only 42
- Less competitors per race with a range of 21-66 contestants across events

Inconsistent record keeping also posed a problem affecting the size of the data set and consistency of analysis. For example:

- Records of air and water temperature have only relatively recently been available 
- Instances of times not being recorded 
- Mislabeling of events
- No events available after 2019 in API, 2020 aside

### Approach

**Overall Trends**

A simple line graph allows me to quickly see overall trends year by year. To better see distributions of times across years, an additional tab with violin plots per year was added to see distributions of race times across each year. 
To help parse the effect of race locations on these times, a ridgeline density plot of race times per event country was added. 
Finally, boxplots detailing placement spreads was added to see what age groups are most competitive. Filters in the side panel allow for exploration of sprint and Olympic distances, gender, country of origin. For year by year analysis, a year filter has been applied to the ridgeline and boxplot.

**Discipline breakdown**

For each discipline (swimming, cycling, and running), I used a scatterplot with a linear regression so as to quickly see correlations between how success in each discipline affects overall results. 
A density plot of discipline times below reacts to the standard filters on the side along with the added temperature filters. For context, density of overall times (filtered only for gender and race) is also plotted on the same plot.

**Country Exploration**

Simple Choropleths detailing countries with the highest success rate (top 10%) and lowest success rate (bottom 10%) help show which countries are most/least competitive.

### Data Sources

- <www.developers.triathlon.org> from the World Triathlon website


