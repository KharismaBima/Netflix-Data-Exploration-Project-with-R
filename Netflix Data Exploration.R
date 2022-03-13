# Adding the libraries necessary for the analysis
library(tidyverse)

#Importing the dataset
netflix_titles <- read_csv("netflix_titles.csv", show_col_types = 'FALSE')

#Now let's look through the data
glimpse(netflix_titles)
summary(netflix_titles)

#Separating "listed_in" column into three "category" column, and take only the first.
netflix_titles <- netflix_titles %>% separate(listed_in, c("Category1", "Category2", "Category3"), sep = ",") 
netflix_titles <- netflix_titles %>% select(c(-"Category2", -"Category3"))

#Separating "date_added" column into "date_added" and "year_added" column.
netflix_titles <- netflix_titles %>% separate(date_added, c("date_added", "year_added"), sep = ",")

#Visualizing the release year trend

ggplot(netflix_titles, mapping = aes(x=release_year, fill = type)) +
  geom_histogram(color = "black", binwidth=2)+
  labs(title="Netflix Films Released by Year", x="Release Year", y="Total Film")+
  scale_fill_manual(values = c("Movie" = "#F6AE2D","TV Show" = "#0770BB"))

#Visualizing at what year the films added on Netflix
#Firstly change the "year_added" value to numeric 
netflix_titles$year_added <- as.numeric(netflix_titles$year_added)

ggplot(netflix_titles, mapping = aes(x=year_added, fill = type)) +
  geom_histogram(color = "black", binwidth=2)+
  labs(title="Films Added on Netflix by Year", x= "Year", y= "Total Film")+
  scale_fill_manual(values = c("Movie" = "#F6AE2D","TV Show" = "#0770BB"))

# Create a new data frame of Film Types

Film_Types <- netflix_titles %>% group_by(type) %>% count() %>% ungroup() %>% 
  mutate(perc = `n` / sum(`n`)) %>% 
  arrange(perc) %>%
  mutate(labels = scales::percent(perc))

head(Film_Types)

#Visualizing Film Types using pie chart

ggplot(Film_Types, aes(x = "", y = perc, fill = type)) +
  geom_col() +
  geom_text(aes(label = labels), position = position_stack(vjust = 0.5)) +
  coord_polar(theta = "y") +
  labs(title="Type of Netflix Films Comparison") +
  theme_void() +
  scale_fill_manual(values = c("Movie" = "#F6AE2D","TV Show" = "#0770BB"))

# Making the dataframe of top 10 Categories 

Category1 <- netflix_titles %>% group_by(Category1) %>% count()

top9_Categories <- filter(Category1, n > 300) %>% arrange(desc(n))
Other_Categories <- filter(Category1, n <300)
Other_Categories <- data.frame(Category1 = c("Others*"), n = c(sum(Other_Categories$n)))

Top10_Category <- rbind(top9_Categories, Other_Categories)
head(Top10_Category)

#Visualizing Netflix Top 10 Category

ggplot(data = Top10_Category, aes(x = reorder(Category1, n), y = n)) + 
  geom_bar(stat = "identity", fill = "#F6AE2D") +
  coord_flip() +
  labs(title = "Netflix's Top 10 Category", x = "Categories", y = "Total Film", caption = "Others: Horror, British, Docuseries, Anime, International, Classic, etc.")

# Making the dataframe of top 10 Ratings

Ratings <- netflix_titles %>% group_by(rating) %>% count()

top9_Ratings <- filter(Ratings, n > 200) %>% arrange(desc(n))
Other_Ratings <- filter(Ratings, n <200)
Other_Ratings <- drop_na(Other_Ratings)
Other_Ratings <- data.frame(rating = c("Others*"), n = c(sum(Other_Ratings$n)))

Top10_Rating <- rbind(top9_Ratings, Other_Ratings)
head(Top10_Rating)

#Visualizing Netflix's Top 10 Rating

ggplot(data = Top10_Rating, aes(x = reorder(rating, n), y = n)) + 
  geom_bar(stat = "identity", fill = "#0770BB") +
  coord_flip() +
  labs(title = "Netflix's Top 10 Rating", x = "Ratings", y = "Total Film", caption = "Others: NR, G, TV-Y7-FV, NC-17, UR, etc.")

