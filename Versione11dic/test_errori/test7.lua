function integer main () 
    {4}{2} integer numbers = {{1,2},{3,4},{5,6},{7,8}}
    integer i = 0
    integer j = 0
    {4} boolean positive


    while ((i < 4)and(j<2)) do
        positive{i} = isPositive(numbers{i}{j}, 2)
        i++
    end
    return 0
end

--[commento di prova


function boolean isPositive (integer num, integer dim)
    integer i = 0
    while i < dim do
        if num <= 0 then 
            return false
        end
        i++
    end
    return true
end