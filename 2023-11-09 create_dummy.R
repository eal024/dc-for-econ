

# Make dummy
library(recipes)


# Example 1 ---------------------------------------------------------------

## Creating dummy variable
nrow(df)

# data.frame
map_lgl( df_school, \(x) is.factor(x))

# Test how convert
df <- df_school |> 
    select( Mjob, Fjob) |> 
    mutate( original = Mjob,
            original2 = paste0(Mjob, "-", Fjob)
            ) 

# recipe
df_rec <- recipe( ~., data = df)

# Summary
summary(df_rec)

#
ref_cell2 <- df_rec |> 
    step_dummy( Mjob) |> 
    prep( training = df)

ref_cell3 <- df_rec |> 
    step_dummy( Mjob, Fjob) |> 
    prep( training = df)


#
summary(ref_cell2)
summary(ref_cell3)

bake( ref_cell2, new_data = NULL, original, starts_with("Mjob"))
bake( ref_cell3, new_data = NULL, original2 , starts_with( c("Mjob", "Fjob") ))


## Example 2
iris <- as_tibble(iris)


iris <- iris |> mutate( original = Species)

irs_rec <- recipe( ~ ., data = iris)


summary(irs_rec)

ref_cell <- irs_rec |> 
    step_dummy(Species) |> 
    prep( training = iris)


summary(ref_cell)

ref_cell

# Get a row for eavh factor level
bake(ref_cell, new_data = NULL, original, starts_with("Species")) |> distinct()

