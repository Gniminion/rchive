df <- read.csv("/Users/ming/Desktop/School/STAT 332/group proj/responses.csv")

# group by treatment
df$font_type <- factor(df$font_type)
df$font_colour <- factor(df$font_colour)
df$treatment <- interaction(df$font_type, df$font_colour)

# eda - descriptive stats
# just font colour
by(df$reading_time, df$font_colour, summary)
# just font type
by(df$reading_time, df$font_type, summary)
# interaction of font and colour
by(df$reading_time, df$treatment, summary)

# eda - boxplots
# font colour
boxplot(reading_time ~ font_colour, data = df, main = "Reading Time by Font Colour",
        col = c("darkslateblue", "orange"),
        xlab = "Colour",
        ylab = "Reading Time (seconds)")
# font type
boxplot(reading_time ~ font_type, data = df, main = "Reading Time by Font Type",
        xlab = "Colour",
        ylab = "Reading Time (seconds)")
# interactions
boxplot(reading_time ~ treatment, data = df,
        col = c("darkslateblue", "orange"),  # dark/light color fill
        main = "Reading Time by Font Type and Colour",
        xlab = "Font Type and Colour",
        ylab = "Reading Time (seconds)")

# eda - overall reading time dist
mean(df$reading_time)
median(df$reading_time)
hist(df$reading_time, main = "Overall Reading Time Distribution", xlab = "Reading Time",
     xlim = c(10,80), ylim = c(0,35))

# eda - interaction plot
interaction.plot(df$font_colour, df$font_type, df$reading_time, 
                 main = "Interaction Plot of Colour vs Font Type on Reading Time",
                 xlab = "Font Colour",
                 ylab = "Mean Reading Time (seconds)")

# Clearly, our histogram has high right-skewness. We choose to take the log.
df$reading_time_log <- log(df$reading_time)
hist(df$reading_time_log,
     main = "Log of Overall Reading Time Distribution",
     xlab = "Log of Reading Time")

# group by treatment
df$font_type <- factor(df$font_type)
df$font_colour <- factor(df$font_colour)
df$treatment <- interaction(df$font_type, df$font_colour)

# eda - descriptive stats
# just font colour
by(df$reading_time_log, df$font_colour, summary)
# just font type
by(df$reading_time_log, df$font_type, summary)
# interaction of font and colour
by(df$reading_time_log, df$treatment, summary)


