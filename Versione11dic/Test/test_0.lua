function void assegnNumerici()

    integer as_int = 5
    float as_f = 5.0
    float as_fint = as_int

    function void operazioni()
        integer somma_int = 7 + as_int 
        float somma_f = as_f + 3.45
        float somma_f_int = as_int + as_f
        float somma_int_int = as_int + somma_int

        integer molt_int = 7 * as_int
        integer molt_int_int = as_int * molt_int
        float molt_f = 3.0 * as_f
        float molt_f_f = as_f * molt_f
        float molt_i_i = as_int * molt_int
        float molt_f_i = as_int * -7

        integer div_int = 7 / as_int
        integer div_int_int = as_int / molt_int
        float div_f = 3.0 / as_f
        float div_f_f = as_f / molt_f
        float div_i_i = as_int / molt_int
        float div_f_i = 3.0 / as_int

    end

end


function void boolC()
    integer a = 7

    boolean as_b = true
    boolean b_e = true and as_b
    boolean b_o = false or as_b

    boolean mul_exp = as_b or false and b_o or not b_e   
end
