--[Test sui tipi base
integer i = 1
integer i = 'c'--[ok segnalato
boolean t = true
boolean f = 5 --[ok segnalato
string s = 'c'--[ok segnalato
string s = 45--[ok segnalato
character c = 'f'
character e = "pluto"--[ok segnalato

--[Test sulla visibilità delle funzioni
{5}integer arr1 = {1,2,3,4}  
{2}{3}integer arr2 
{3}{2}{1}integer arr3 
function integer f (integer n1, integer n2, integer n3)
n1 = g(4,5)
n2 = h(1,2)
return n2
end

function integer g (integer n1,integer n2)
  function integer h (integer n1,integer n2)
    n1 = g(7,8)
    n2 = f(10,11,12)
    return n1
  end
return n1
end

function integer g (integer n1,integer n2, boolean t) --[ok correttamente segnalato che g è già stata dichiarata
return 4
end

function integer l (integer n1,float n2, boolean t) --[ok correttamente segnalato che g è già stata dichiarata
float c = n1--
integer d = --n2 --[ok segnalato
return 0
end