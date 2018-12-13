function void puntatori()

    integer i = 7
    pointer integer p = &i
    --puntatore di puntatore
    pointer pointer integer p_p = &p

    string testo = "testo"

    pointer string p_s = &testo

    writeString(_p_s)

    
    --array di puntatori
    {3}pointer string vettore_p = {p_s, p_s, p_s}

    {5}integer array = {1,2,3,4,5}
    --puntatore ad un array
    pointer {5}integer arr_point = &array

    writeString(_vettore_p{1})
end

function void ifinline()
    for i = 0, 18, 1 do
        string k = i < 5 ? "cinque" : (i < 10 ? "dieci" : (i < 15 ?  "quindici" : "diciotto"))
    end
end

function float trycatch(float d, float div)
    try 
        return d/div
    catch
        writeString("Errore: divisione impossibile")
        return -127819182
    end
end