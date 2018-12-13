function void testUg_Dis()

    boolean ug_str = "pippo" == "topolino"
    boolean dis_str = "pippo" ~= "topolino"
    boolean ug_ch = 'x' == 'y'
    boolean dis_ch = 'x' ~= 'y'
    boolean ug_int = 1 == 2
    boolean dis_int = 1 ~= 2
    boolean ug_float = 2.0 == 1.72
    boolean dis_float = 2.0 ~= 1.72
    boolean ug_fl_int = 2.0 == 2
    boolean ug_int_fl = 2 == 2.0
    boolean dis_fl_int = 2.0 ~= 2
    boolean dis_int_fl = 2 ~= 2.0
    boolean ug_bool = true == false
    boolean dis_bool = true ~= false

    integer valore = 7
    integer valore_2 = 5
    pointer integer punt_int = &valore
    pointer integer punt_int_2 = &valore_2

    boolean ug_poi = _punt_int == _punt_int_2
    boolean dis_poi = _punt_int ~= _punt_int_2
    boolean min_poi = _punt_int > _punt_int_2
    boolean mag_poi = _punt_int < _punt_int_2
    boolean min_e_poi = _punt_int >= _punt_int_2
    boolean mag_e_poi = _punt_int <= _punt_int_2

end   

function void conf_Ordin()

    boolean min_int = 1 < 2
    boolean mag_int = 1 > 2
    boolean min_e_int = 1 <= 2
    boolean mag_e_int = 1 >= 2
    boolean min_float = 2.0 < 1.72
    boolean mag_float = 2.0 > 1.72
    boolean min_e_float = 2.0 <= 1.72
    boolean mag_e_float = 2.0 >= 1.72

    boolean hdfhj = 2.0  > 2
    boolean fvcdg = 2  > 2.0
    
end