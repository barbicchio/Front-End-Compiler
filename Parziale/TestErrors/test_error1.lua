function void test (const integer t, ref integer p) 
 t = 5 --[mi deve segnalare l'errore, non posso assegnare un valore a una costante

end



function integer main () 
    {3}{2} integer numbers = {{1,2},{3,4},{5,6},{7,8}}
    integer i = 0

    {4} boolean positive


    while i < 4 do
        positive{i} = isPositive(numbers{i}, 2)
        i++
    end

    pointer integer es_point

    test(7,1)--[segnala la richiesta di una l-expression
    test(7,i)--[modalità corretta
    return 0
end

--[commento di prova


function boolean isPositive ({}integer array, integer dim)
    integer i = 0
    while i < dim do
        if array{i} <= 0 then 
            return false
        end
        i++
    end
    return true
end
