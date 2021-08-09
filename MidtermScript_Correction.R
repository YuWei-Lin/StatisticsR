# Correction Code for Midterm question4
SW <- read_delim(file.choose(), "\t", escape_double = FALSE, trim_ws = TRUE)
NEWCON <- sub("'", "", colnames(SW))
NEWCON <- sub("'", "", NEWCON)
colnames(SW) <- NEWCON
SW[SW$creat_68 == 9.99, 4] <- NA
SW <- SW[!(is.na(SW$creat_68)), ]
SW <- SW[order(SW$group), ]
flow <- SW$creat_68 # Year 1968
flow69 <- SW$creat_69 # Year 1969
flow70 <- SW$creat_70 # Year 1970
flow71 <- SW$creat_71 # Year 1971
flow72 <- SW$creat_72 # Year 1972
flow75 <- SW$creat_75 # Year 1975
flow78 <- SW$creat_78 # Year 1978
group <- factor(SW$group)
result <- lm(flow~group)
anova(result)
