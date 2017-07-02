comp a b = sort(square a) == sort b
    where square = map (\x->x*x) 
