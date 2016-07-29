# Graphs

library(ggplot2)
gr1 <- ggplot(df.rap, aes(x=log(TOTAL.ASSETS))) + geom_density(na.omit=TRUE)
gr1
gr1 + facet_grid(default ~ .)

gr1 + facet_grid(Sector.and.Perishability ~ .)

# gr1 + facet_grid(Taxes ~ Net.Income) # This crashed due to memory


require(lattice)

# Density plots by group with lattice:
densityplot(~log(EBITDA),data=df.rap,
            groups=default,
            xlab="x",
            main="title",
            plot.points=FALSE,
            auto.key=TRUE)   # Add no.omit=TRUE

densityplot(~log(Total.COGS),data=df.rap,
             groups=default,
            xlab="x",
             main="title",
             plot.points=FALSE,
             auto.key=TRUE)

densityplot(~COGS.Margin,data=df.rap[df.rap$COGS.Margin<1.5 & df.rap$COGS.Margin>-.5,],   # <--- include this
            groups=default,
            xlab="x",
            main="title",
            plot.points=FALSE,
            auto.key=TRUE)

library(ggvis)
df.rap %>% ggvis(~TOTAL.ASSETS, ~max_pct_per_buyer, fill = ~default) %>% layer_points()
df.rap %>% ggvis(~TOTAL.ASSETS, ~TOTAL.LIABILITIES, fill = ~default) %>% layer_points()

df.rap %>% ggvis(~TOTAL.ASSETS, ~Repeat.Clients, fill = ~default) %>% layer_points()
df.rap %>% ggvis(~TOTAL.ASSETS, ~Sector.and.Perishability, fill = ~default) %>% layer_points()
df.rap %>% ggvis(~TOTAL.ASSETS, ~unique_segments, fill = ~default) %>% layer_points()
df.rap[df.rap$COGS.Margin<1.5 & df.rap$COGS.Margin>-.5,] %>% ggvis(~COGS.Margin, ~Sector.and.Perishability, fill = ~default) %>% layer_points()

df.rap %>% ggvis(~RiskRating, fill= ~default) %>% layer_points()
# Graph each variable grouped by default
