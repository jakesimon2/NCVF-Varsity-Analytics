# CUSTOM SETTINGS FOR GRAPHS THAT CAN BE CHANGED BASED ON PREFERENCE:

# Boolean for whether or not the school-based graphs should be school colors or the
# same set of colors amongst all of the schools
school_color_scheme <- FALSE
# Specifically for GRAPH 2B only
graph2B_school_colors <- TRUE
# Percent cap excludes teams with make %'s greater than or equal to the cap.
percent_cap <- 1.00 # Write percent as a decimal.
# region only includes schools from one region in the data (for all regions, region<-"All")
region <- "SCCVL"
# for regions with a lot of teams, you can start graphs from a certain observation
startobs <- 1 # Any value <= 0 or greater than nrows(response_import) will be replaced with 1.
endobs <- 99 # Type a really big number here if you want to all remaining observations read.
# gender determines whether or not we will be analyzing men's or women's data
gender <- "Women's"
# determines which year's data is being analyzed and output
year <- "2020"
# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
# readxl is needed to read the Excel spreadsheet.
library(readxl)
# Run the lines within the tildas (~) all at once
# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
# Names of the schools (rows for the chart)
responses_import <- read_xlsx(paste0("~/Documents/NCVF Varsity Project/", year,
                                     "/Club Volleyball Varsity Interest ", year, ".xlsx"), 
                                     sheet = gender)
# Excludes schools that are not apart of the chosen region
if (region != "All") {
  responses_import <- responses_import[responses_import $ Conference == region, ]
}
if (startobs <= 0 || startobs > nrow(responses_import)) {
  startobs <- 1
}
if (endobs > nrow(responses_import)) {
  endobs <- nrow(responses_import)
}
# shortens amount of observations measured in this run of graphs
responses_import <- responses_import[startobs:endobs,]

# Takes the schools out of the response data, so they can be used as row labels
school_import <- as.data.frame(responses_import[,1])
responses_import <- responses_import[,-1]

# Makes a vector school_labels for the rownames of all responses
school_labels <- as.list(length(school_import[,1]))
for (i in 1:length(school_import[,1])) {
  school_labels[i] <- school_import[i,1]
}
# Takes out the conferences column from the data since we are only analyzing one
# conference at a time.
responses_import <- responses_import[,-1]
# Takes the colors out of response_import, so the colors can be separated into
# their own data frame (used for coloring graphs, plots, and charts)
school_colors <- as.data.frame(responses_import[,1:3])
row.names(school_colors) <- school_labels
responses_import <- responses_import[,-(1:3)]

# Converts responses to matrix
all_responses <- as.data.frame(responses_import)

# Names of schools are assigned as rownames to the all_responses tibble.
rownames(all_responses) <- school_labels 
View(all_responses)

# takes the first color from each school for a single-bar barplot.
oneColor <- rep("", times = length(school_labels))
for (i in 1:length(school_labels)) {
  oneColor[i] <- school_colors[i,1]
}
# takes the first two colors from each school for a two-bar barplot.
twoColors <- rep("", times = 2 * length(school_labels))
for (i in 1:length(school_labels)) {
  twoColors[2*i - 1] <- school_colors[i,1]
  twoColors[2*i] <- school_colors[i,2]
}
# takes all three colors from each school for a three-bar barplot.
threeColors <- rep("", times = 3 * length(school_labels))
for (i in 1:length(school_labels)) {
  threeColors[3*i - 2] <- school_colors[i,1]
  threeColors[3*i - 1] <- school_colors[i,2]
  threeColors[3*i] <- school_colors[i,3]
}

# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
# MEN'S and WOMEN'S specific data. Whatever is entered for the "gender" variable
# will determine who is being analyzed during this runthrough of code.
# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
# GRAPH 1A: For each school, three pie graphs are made. The first two demonstrate the
# ratio between the maximum number of spots available vs. the number of varstiy /
# non-varsity players cut. The third shows overall ratio between made vs. cut players.

# Stores proportions from each bar graph for a comparison of averages in GRAPH 1B.
var_make_vector <- rep(0, times = length(school_labels))
nonvar_make_vector <- rep(0, times = length(school_labels))
overall_make_vector <- rep(0, times = length(school_labels))
# Loop that creates pie graphs for each school and adds their make %'s into a vector
for(i in 1:length(school_labels)) {
  # Number of varsity players who will make the team
  var_make <- all_responses[i,7]
  if (var_make > all_responses[i,2]) {
    var_make <- all_responses[i,2]
  }
  # Players who will not make the team = total varsity - make varsity
  var_wont_make <- all_responses[i,2] - var_make
  if (var_wont_make < 0) {
    var_wont_make <- 0
  }
  # Percentage of varsity players from this school who will make their team
  var_make_vector[i] <- var_make / all_responses[i,2]
  # var_make and var_wont_make converted to percentages for the pie graph
  make_per1aa <- round(var_make_vector[i], 2) * 100
  perc1aa <- c(make_per1aa, 100 - make_per1aa)
  
  # Pie chart for Varsity players
  if (all_responses[i,2] > 0) {
    lbls1aa <- c("Max # of Spots", "# of Players Cut") # Labels
    nums1aa <- c(var_make, var_wont_make) # Numeric values for make vs. cut players
    
    # Labels, numeric values, and percentages are combined into a single label
    info1aa <- paste(paste0(lbls1aa, ": ", nums1aa), paste0(perc1aa, "%"), sep = "\n")
    
    color_scheme_1aa <- c("blue", "red") # Default color scheme if not school themed
    if (school_color_scheme) {
      color_scheme_1aa <- c(twoColors[2 * i - 1], twoColors[2 *i])
    }
    graph1aa <- pie(nums1aa, labels = info1aa, col = color_scheme_1aa, 
                    main = paste0(gender, " Varsity Players Trying Out At ", 
                                  school_labels[i]), cex=1.5, cex.main=1.5)
  }
  # If there are more Varsity spots than Varsity players, those spots will be given 
  # to Non-Varsity players
  leftover_var_spots <- all_responses[i,7] - all_responses[i,2]
  if (leftover_var_spots < 0) {
    leftover_var_spots <- 0
  }
  # Non-varsity players who will make the team
  nonvar_make <- all_responses[i,8] + leftover_var_spots
  if (nonvar_make > all_responses[i,6]) {
    nonvar_make <- all_responses[i,6]
  }
  # Players who will not make the team = total non-varsity - make non-varsity
  nonvar_wont_make <- all_responses[i,6] - nonvar_make
  if (nonvar_wont_make < 0) {
    nonvar_wont_make <- 0
  }
  # Percentage of non-varsity players from this school who will make their team
  nonvar_make_vector[i] <- nonvar_make / all_responses[i,6]
  # var_make and var_wont_make converted to percentages for the pie graph
  make_per1ab <- round(nonvar_make_vector[i], 2) * 100
  perc1ab <- c(make_per1ab, 100 - make_per1ab)
  
  # Pie chart for Non-varsity players
  if(all_responses[i,6] > 0) {
    lbls1ab <- c("Max # of Spots", "# of Players Cut") # Labels
    nums1ab <- c(nonvar_make, nonvar_wont_make) # Numeric values for make vs. cut
    # Labels, numeric values, and percentages are combined together into one label
    info1ab <- paste(paste0(lbls1ab, ": ", nums1ab), paste0(perc1ab, "%"), sep = "\n")
    # Default color scheme if not school themed
    color_scheme_1ab <- c("#86BAFF", "#FF8686")
    if (school_color_scheme) {
      color_scheme_1ab <- c(twoColors[2 * i - 1], twoColors[2 *i])
    }
    graph1ab <- pie(nums1ab, labels = info1ab, col = color_scheme_1ab,
                    main = paste0(gender, " Non-varsity Players Trying Out At ", 
                                  school_labels[i]), cex = 1.5, cex.main = 1.5)
  }
  # Pie graph for overall make vs. cut ratio.
  if (length(all_responses[,5]) > 0) {
    lbls1ac <- c("Overall Max # of Spots", "Overall # of Players Cut") # Labels
    # Numeric values for make vs. cut
    overall_make <- nonvar_make + var_make
    overall_wont_make <- nonvar_wont_make + var_wont_make
    nums1ac <- c(overall_make, overall_wont_make)
    # Overall make % stored in vector
    overall_make_vector[i] <- overall_make / (overall_make + overall_wont_make)
    # Percentage values for make vs. cut
    perc1ac <- c(round(overall_make / all_responses[i,5], 2) * 100, 
                 round(overall_wont_make / all_responses[i,5], 2) * 100)
    # Labels, numeric values, and percentages are combined together into one label
    info1ac <- paste(paste0(lbls1ac, ": ", nums1ac), paste0(perc1ac, "%"), sep = "\n")
    # Default color scheme if not school themed
    color_scheme_1ac <- c("#D1E5FF", "#FFC8C8")
    if (school_color_scheme) {
      color_scheme_1ac <- c(twoColors[2 * i - 1], twoColors[2 * i])
    }
    graph1ac <- pie(nums1ac, labels = info1ac, col = color_scheme_1ac,
                    main = paste0("All ", gender, " Players Trying Out At ", 
                                  school_labels[i]), cex = 1.5, cex.main = 1.5)
  }
  else {
    overall_make_vector[i] <- NA
  }
}
# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
# GRAPH 1B: Makes a bar plot showing the percentage of players who will make
# a team between Varsity and Non-varsity players

# Makes sure NA values (0's) are omitted from the data to prevent graph errors
var_make_vector <- na.omit(var_make_vector)
nonvar_make_vector <- na.omit(nonvar_make_vector)
overall_make_vector <- na.omit(overall_make_vector)
# Omits values greater than or equal to a certain percentage from the data
var_make_vector <- var_make_vector[var_make_vector <= percent_cap]
nonvar_make_vector <- nonvar_make_vector[nonvar_make_vector <= percent_cap]
overall_make_vector <- overall_make_vector[overall_make_vector <= percent_cap]
# barplot and legend constructed for the means for each school's make percentage
var_vs_nonvar_make <- as.numeric(as.character(c(round(mean(var_make_vector), 2), 
                                                round(mean(nonvar_make_vector), 2),
                                                round(mean(overall_make_vector), 2))))
graph1b <- barplot(var_vs_nonvar_make, beside = TRUE, cex.axis = 1.5, cex.lab = 1.5,
                  cex.names = 1.5,
                   names.arg = c("Varsity Players", "Non-Varsity Players", "All Players"), 
                  col = c("blue","#86BAFF","#D1E5FF"), ylab = "Percentage", ylim = c(0,1.00), 
                  main = paste0("Average Percentage of Players Who Made Their Club Team",
                                "\n(Region = ", region, ", Gender = ", gender, 
                                ", Percentage Made <= ", percent_cap * 100, "%)"))
text(x = graph1b, y = var_vs_nonvar_make, label = var_vs_nonvar_make, pos = 3, cex = 1.5)

# GRAPH 1C:
# Box plot that shows how many players try out vs. the amount of teams / spots offered per school
boxplot(all_responses[all_responses[,1] <= 1, 5], 
        all_responses[all_responses[,1] >= 2, 5], 
        border = c("cyan","blue"), cex.axis = 1.5, cex.lab = 1.5,
        horizontal = T, xlab = "Number of players", ylab = "Number of teams offered",
        names = c(1,2), main = paste0("Number of Players Trying Out vs. Number of Teams Offered\n",
                                      "(Gender = ", gender, ", Region = ", region, ")"))
abline(v=15, col = "cyan", lwd = 3, lty = 2)
abline(v=30, col = "blue", lwd = 3, lty = 2)

# GRAPH 1D:
# Line graph plot that shows the distribution of players per teams offered
teams_1_int <- all_responses[all_responses[,1] <= 1, 5]
mode_teams_1 <- max(as.numeric(table(teams_1_int)))
seq_teams_1 <- seq(min(teams_1_int) - (min(teams_1_int) %% 5), 
                   max(teams_1_int) + (5 - max(teams_1_int) %% 5), by = 5)
hist(teams_1_int, col = "cyan", 
     breaks = seq_teams_1, cex.axis = 1.5, cex.lab = 1.5,
     main = paste0("Number of Players Interested for Schools with 1 team\n",
                   "(Gender = ", gender, ", Region = ", region, ")"),
     xlab = "Number of Players Interested", xlim = c(0, max(teams_1_int) + 5))
x <- seq_teams_1
curve(mode_teams_1 * 20 * dnorm(x, mean(teams_1_int), sd(teams_1_int)), 
      from = 0, to = max(teams_1_int) + 5, col = "blue", add = T)
abline(v=15, lwd = 3, lty = 2, col = "blue")

teams_2_int <- all_responses[all_responses[,1] >= 2, 5]
mode_teams_2 <- max(as.numeric(table(teams_2_int)))
seq_teams_2 <- seq(min(teams_2_int) - (min(teams_2_int) %% 5), 
                   max(teams_2_int) + (5 - max(teams_2_int) %% 5), by = 5)
hist(teams_2_int, col = "blue",
     breaks = seq_teams_2, cex.axis = 1.5, cex.lab = 1.5,
     main = paste0("Number of Players Interested for Schools with 2+ teams\n",
                   "(Gender = ", gender, ", Region = ", region, ")"),
     xlab = "Number of Players Interested", xlim = c(0, max(teams_2_int) + 5))
x <- seq_teams_2
curve(mode_teams_2 * 20 * dnorm(x, mean(teams_2_int), sd(teams_2_int)), 
      from = 0, to = max(teams_2_int) + 5, col = "cyan", add = T)
abline(v=30, lwd = 3, lty = 2, col = "cyan")

# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
# GRAPH 2A: Pie graph that analyzes the proportion of Community College only
# players to the total amount of Varsity level players

# Vectors to store each school's sample group sizes.
cc_only_vector <- rep(0, rep = length(school_labels))
ncaa_vector <- rep(0, rep = length(school_labels))
max_var_spots_vec <- rep(0, rep = length(school_labels))

# Goes through each school's varsity player statistics
for(i in 1:length(school_labels)) {
  # Varsity players who only played at CC
  cc_only_var <- all_responses[i,4]
  cc_only_vector[i] <- cc_only_var
  # NCAA Varsity playes = Varsity players total - Community college only players
  ncaa_var <- all_responses[i,2] - cc_only_var
  ncaa_vector[i] <-ncaa_var
  # Pie graph comparing the ratio between the two types ofVarsity players
  if (all_responses[i,2] > 0) {
    # Labels
    lbls2a <- c("Varsity players who only \n played in community college",
                "Varsity players who \n played in the NCAA")
    nums2a <- c(cc_only_var, ncaa_var) # Numeric values
    # Percentage of varsity players who made their team
    rounded_cc_2a <- round(cc_only_var / all_responses[i,2], 2) * 100
    # Percentages of varsity players made vs. cut in a single vector
    perc2a <- c(rounded_cc_2a, 100 - rounded_cc_2a)
    # Combines labels, numeric values, and percentages into a single label
    info2a <- paste(paste0(lbls2a, ": ", nums2a), paste0(perc2a, "%"), sep = "\n")
    # Default color scheme if not school themed
    color_scheme_2a <- c("#FDFF00", "#147DF7")
    if (school_color_scheme) {
      color_scheme_2a <- c(twoColors[2 * i - 1], twoColors[2 *i])
    }
    pie(nums2a, labels = info2a, col = color_scheme_2a, 
        main = paste0(gender, " Varsity Player Demographics for ", 
                      school_labels[i]), cex = 1.5, cex.main = 1.5)
  }
}
# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
# GRAPH 2B: Bar graph that shows three sample groups: varsity players who only
# only played in community college, varsity players who played in the
# NCAA, Maximum number of Varsity spots for their club program.

# Information stored for the labels section of the pie graph
rounded2_pie <- round(sum(cc_only_vector) / (sum(cc_only_vector) + sum(ncaa_vector)),2) * 100
perc2_pie <- c(rounded2_pie, 100 - rounded2_pie)
nums2_pie <- c(sum(cc_only_vector), sum(ncaa_vector))
lbls2_pie <- c("Total amount of CC only players", "Total amount of NCAA players")
info2_pie <- paste(paste0(lbls2_pie, ": ", nums2_pie), paste0(perc2_pie, "%"), sep = "\n")

# Pie chart showing the overall ratio of NCAA players to CC only players
pie_2b <- pie(nums2_pie, labels = info2_pie, 
              col = c("#FDFF00", "#147DF7"), 
              main = paste0(gender, " Varsity Player Demographics for ", 
                            region), cex = 1.5, cex.main = 1.5)

# Bar graph depicting the amount of CC only Varsity players, NCAA Varsity players,
# and the maximum number of Varsity spots for their club.
blanks_three_bars <- list("")
for (i in 1:length(school_labels)) {
  # school name goes in middle of the three bars
  blanks_three_bars[3 * i - 2] <- school_labels[i]
  blanks_three_bars[3 * i - 1] <- ""
  blanks_three_bars[3 * i] <- ""
}
# Makes a vector that shows each school's sample groups adjacent to each other
graph_2b_vec <- rep(0, length = 3 * length(school_labels))
# Loop that makes pie graph for each school's Varsity player demographics
for (i in 1:length(school_labels)) {
  # stores maximum number of Varsity spots per club program
  max_var_spots_vec[i] <- 2 * all_responses[i,1]
  # If there are less Varsity players than the max number of Varsity spots, the max
  # value for their school = the number of Varsity players
  if (all_responses[i,2] < max_var_spots_vec[i]) {
    max_var_spots_vec[i] <- all_responses[i,2]
  }
  # stores each category for the school according to: CC only, NCAA, Max Var. spots
  graph_2b_vec[3 * i - 2] <- cc_only_vector[i]
  graph_2b_vec[3 * i - 1] <- ncaa_vector[i]
  graph_2b_vec[3 * i] <- max_var_spots_vec[i]
}
# Default color scheme if not school themed
color_scheme_2b <- c("#FDFF00", "#147DF7", "#FF0000")
if (graph2B_school_colors) {
  color_scheme_2b <- threeColors
}
graph2b <- barplot(graph_2b_vec, space = c(1, 0, 0), col = color_scheme_2b, 
                   names.arg = blanks_three_bars,
                   cex.names = 0.9^(1/3 * length(school_labels)),
                   ylab = "Number of players",
                   ylim = c(0, max(all_responses[,2] + 2)),
                   xlab = "School Name", density = c(1000, 50, 20),
                   main = paste0("Number of Varsity Level Players Based on Collegiate Experience",
                                 "\n(Region = ", region, ", Gender = ", gender, ")"))
nums2b <- as.numeric(as.character(graph_2b_vec))
text(x = graph2b, y = nums2b, label = nums2b, pos = 3)
legend("topleft", legend = c("Community College Only", "Played in NCAA", 
                             "Max Number of Varsity Spots"), 
       density = c(1000, 50, 20),
       x.intersp = 0.5, y.intersp = 0.5)
# Makes and exports a .csv file that will make a data frame using the GRAPH 2B data
varsity_make_stats <- cbind(school_labels, nonvar_make_vector, var_make_vector, 
                            cc_only_vector, ncaa_vector, max_var_spots_vec)
colnames(varsity_make_stats) <- c("School Name", "Non-Varsity Player Make %", "Varsity Player Make %", 
                                  "Only Played in CC", "Played in NCAA", "Max. Number of Varsity Spots")
write.csv(varsity_make_stats, paste0("~/Documents/NCVF Varsity Project/", year, "/",
                                     region, " ", gender, " Varsity Make Stats.csv"))
# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

# GRAPH 3A: Pie graph measuring the demand for an open division that allowed for
# an unlimited amount of Varsity players per team.

# First, the "Yes" and "No" answers for the open division are split into subsets.
yes_open_div <- subset(all_responses, all_responses[,10] == "Yes")
no_open_div <- subset(all_responses, all_responses[,10] == "No")
lbls3a <- c("Yes", "No") # Labels
nums3a <- c(length(yes_open_div[,10]), length(no_open_div[,10])) # Numeric values
# Percentage for how many schools answered "Yes"
rounded_yes_3a <- round(length(yes_open_div[,10]) / length(all_responses[,10]), 2) * 100
# "Yes" and "No" percentages stored in a single vector for the pie graph
perc3a <- c(rounded_yes_3a, 100 - rounded_yes_3a)
# Labels, numeric values, and percentages combined into a single label
info3a <- paste(paste0(lbls3a, ": ", nums3a), paste0(perc3a, "%"), sep = "\n")
if (length(yes_open_div[,10]) > 0 || length(no_open_div[,10]) > 0)
{
  graph3a <- pie(nums3a, labels = info3a, col = c("green", "red"),
                 main = paste0("\nIf your regional league created an Open division\n", 
                               "(where teams could have an unlimited number of Varsity players),\n",
                               "would your club consider forming a team(s) for this division?\n",
                               "(Gender = ", gender, ")"), cex = 1.5, cex.main = 1.5)
}
# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

# GRAPH 3B: Histograms displaying the distribution between the number of teams
# each school would send to represent their university in each gender's Open division.

# All schools are represented in both histograms (teams who replied "No" = 0 teams)
num_open_teams <- rep(0, length(all_responses[,10]))
for (i in 1:length(yes_open_div[,10])) {
  num_open_teams[i] <- as.numeric(yes_open_div[i,11])
}
color3b <- "BLUE"
curve3b <- "PINK"
if (gender == "Women's") { 
  color3b <- "PINK" 
  curve3b <- "BLUE"
}
# Yes for an Open Division Histogram
table_3b <- table(num_open_teams)
hist(num_open_teams, breaks = seq(-0.5, max(num_open_teams) + 0.5, by = 1),
     cex.axis = 1.5, cex.lab = 1.5,
     col = color3b, ylim = c(0, max(table_3b) + 1), xlab = "Number of Open Teams",
     main = paste0("Number of Teams Each School Would Represent in the Open Division",
                   "\n(Region = ", region, ", Gender = ", gender, ")"))
text(x = as.numeric(names(table_3b)), y = as.numeric(table_3b), 
     label = as.numeric(table_3b), pos = 3, cex = 1.5)
curve(max(as.numeric(table_3b)) * dnorm(x, mean = mean(num_open_teams), sd = sd(num_open_teams)), 
      from = -0.5, to = max(num_open_teams) + 0.5, add = T, col = curve3b)

# GRAPH 3C: Pie graph displaying the reasons why a school would not represent teams
# in an Open level division.
if (length(no_open_div[,10]) > 0)
{
  # Creates a table for each of the reasons and calculates their frequency
  no_open_table <- table(na.omit(all_responses[,12]))
  no_open_nums <- as.numeric(no_open_table)
  # Percentages calculated for each reason's frequency
  perc3c <- round(no_open_nums / sum(no_open_nums), 2) * 100
  # Pie chart that displays all the reasons with duplicates represented in numbers
  pie(no_open_nums, label = paste0(names(no_open_table), ": ", no_open_nums, "\n", perc3c, "%"), 
      col = rainbow(length(no_open_nums)),
      main = paste0("Reasons Why a School Would Not Represent Teams in an Open Division",
                    "\n(Region = ", region, ", Gender = ", gender, ")"), cex = 1.5, cex.main = 1.5)
}
# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

# GRAPH 4A: Pie graph displaying how many schools believe community college players
# should still be considered Varsity level players.

# Vectors that separates the schools into two categories based on whether or not they
# believe community college players should be "Varsity" level
yes_cc_var <- subset(all_responses, all_responses[,13] == "Yes")
no_cc_var <- subset(all_responses, all_responses[,13] == "No")
# Makes pie graph for the two options (categories).
if (length(all_responses[,13] > 0)) {
  # Frequencies for "Yes" and "No" responses
  nums4a <- c(length(yes_cc_var[,13]), length(no_cc_var[,13]))
  lbls4a <- c("Yes", "No") # Labels
  # Percentage of schools that answered "Yes"
  perc_cc_yes_4a <- round(length(yes_cc_var[,13]) / length(all_responses[,13]), 2) * 100
  # Percentages for "Yes" and "No" are split into a vector with two elements.
  perc4a <- c(perc_cc_yes_4a, 100 - perc_cc_yes_4a)
  pie(nums4a, labels = paste(paste0(lbls4a, ": ", nums4a), paste0(perc4a, "%"), sep = "\n"),
      col = c("green", "red"), 
      main = paste0("Do you believe community college players should \n be considered Varsity level athletes?
      (Gender = ", gender, ")"), cex = 1.5, cex.main = 1.5)
}
# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
# GRAPH 4B: Pie graph displaying the reasons why a school believes community collegiate
# athletes should still be Varsity level players.
if (length(yes_cc_var[,14]) > 0)
{
  # Schools that answered "Yes" are put into a table displaying their reasons and
  # the frequencies of said reasons.
  yes_cc_var_reasons <- table(yes_cc_var[,14])
  yes_cc_nums <- as.numeric(yes_cc_var_reasons)
  # Percentages for each reason's frequency are put into a separate vector.
  perc4b <- c(0)
  for (i in 1:nlevels(yes_cc_var_reasons)) {
    perc4b[i] <- round(yes_cc_nums[i] / sum(yes_cc_nums), 2) * 100
  }
  # Pie graph that displays all the reasons with duplicates represented in numbers
  pie(yes_cc_nums, label = paste0(names(yes_cc_var_reasons), ": ", as.numeric(yes_cc_var_reasons), 
                                  "\n", perc4b, "%"), 
      col = rainbow(length(yes_cc_var_reasons)),
      main = paste0("Reasons Why Community College Players Should Be Varsity Players",
                    "\n(Region = ", region, ", Gender = ", gender, ")"), cex = 1.5, cex.main = 1.5)
}
# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
# GRAPH 4C: Pie graph displaying the reasons why a school believes community collegiate
# athletes should not be Varsity level players.
if (length(no_cc_var[,14]) > 0)
{
  # Schools that answered "No" are put into a table displaying their reasons and
  # the frequencies of said reasons.
  no_cc_var_reasons <- table(no_cc_var[,14])
  no_cc_nums <- as.numeric(no_cc_var_reasons)
  # Percentages for each reason's frequency are put into a separate vector
  perc4c <- c(0)
  for (i in 1:sum(no_cc_var_reasons)) {
    perc4c[i] <- round(no_cc_nums[i] / sum(no_cc_nums), 2) * 100
  }
  # Pie graph that displays all the reasons with duplicates represented in numbers
  pie(no_cc_nums, label = paste0(names(no_cc_var_reasons), ": ", as.numeric(no_cc_var_reasons), 
                                 "\n", perc4c, "%"), 
      col = rainbow(length(no_cc_var_reasons)),
      main = paste0("Reasons Why Community College Players Should Not Be Varsity Players",
                    "\n(Region = ", region, ", Gender = ", gender, ")"), cex = 1.5, cex.main = 1.5)
}
# Opinion based questions have their collective data written into a .csv file
opinion_questions <- list(region, gender, perc3a[1] / 100, perc3a[2] / 100, 
                          as.numeric(table_3b[1]), 
                          as.numeric(table_3b[2]), 
                          as.numeric(table_3b[3]))
opinion_questions[8] <- 0
if (length(table_3b) >= 4) {
  for (i in 4:length(table_3b)) {
    opinion_questions[8] <- as.numeric(opinion_questions[8]) + as.numeric(table_3b[i])
  } # adds teams who would represent 3+ Open teams
}
expected_value_open_teams <- 0
for (i in 1:length(table_3b)) {
  expected_value_open_teams <- expected_value_open_teams + 
    (i-1) * (as.numeric(table_3b[i]) / sum(as.numeric(table_3b)))
} # calculates expected value for # of Open teams
opinion_questions[9] <- round(expected_value_open_teams, 1)
opinion_questions[10] <- perc4a[1] / 100
opinion_questions[11] <- perc4a[2] / 100
opinion_names <- list("Region", "Gender", "Yes Perc for Open division", "No Perc for Open division",
                      "Open Teams 0", "Open Teams 1", "Open Teams 2", "Open Teams 3 or more", 
                      "Expected Value Open Teams", "Yes Perc for CC Players Being Varsity", 
                      "No Perc for CC Players Being Varsity")
names(opinion_questions) <- opinion_names
write.csv(opinion_questions, paste0("~/Documents/NCVF Varsity Project/", year, "/",
                                    region, " ", gender, " Opinion Data.csv"))
