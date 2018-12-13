function void comp (integer i, float f)
    {3}integer vettore = {1,2,3}

    integer t_i = f
    --boolean ug_arr = vettore == vettore
    --boolean dis_arr = vettore ~= vettore    non-exhaustive patterns

    
    boolean min_str = "pippo" < "topolino"
    boolean mag_str = "pippo" > "topolino"
    boolean min_e_str = "pippo" <= "topolino"
    boolean mag_e_str = "pippo" >= "topolino"
    boolean min_ch = 'x' < 'y'
    boolean mag_ch = 'x' > 'y'
    boolean min_e_ch = 'x' <= 'y'
    boolean mag_e_ch = 'x' >= 'y'
    boolean min_bool = true < false
    boolean mag_bool = true > false
    boolean min_e_bool = true <= false
    boolean mag_e_bool = true >= false

    integer cint = '7'
    string cstr = 'h'

    integer valore = 7
    integer valore_2 = 5
    pointer integer punt_int = &valore
    pointer integer punt_int_2 = &valore_2
    
    boolean punt_ug = punt_int == punt_int
    boolean punt_dis = punt_int ~= punt_int
    boolean punt_min = punt_int < punt_int
    boolean punt_mag = punt_int > punt_int
    
end


function void breakContinue(float max, float step)
    for i = 0, max, step do
        writeFloat(max - i)
    end  

    if i < 7 then
        continue
    end
end