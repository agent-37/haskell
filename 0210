


parss pos cur vec ans_mas = if pos== (length vec) 
                            then ans_mas 
                            else if length ans_mas == 0 
                                 then (parss (pos+1) cur vec [[vec !!pos]]) 
                                 else if cur == vec !!pos 
                                      then  (parss (pos+1) cur vec  ( (init ans_mas) ++ ([(last ans_mas)++[vec !!pos]]) )  ) 
                                      else  (parss (pos+1) (vec !!pos) vec (ans_mas++[[vec !!pos]]) )

parse vec = parss 0 (vec !!0) vec []


encode11 vec pos res = if pos== (length vec) 
                       then res
                       else encode11 vec (pos + 1) (res ++ [(length (vec !!pos), (vec !!pos) !! 0 )])


encode vec = encode11 (parse vec) 0 []


dropp vec pos loc_pos num res = if pos== (length vec) 
                                then res
                                else if loc_pos== num-1
                                     then dropp vec (pos+1) 0 num res
                                     else dropp vec (pos+1) (loc_pos+1) num (res++([vec !! pos]))

dropEvery vec num = dropp vec 0 0 num []


ssplit vec pos num res1  res2  = if pos== (length vec) 
                                 then (res1,res2)
                                 else if pos< num 
                                 then ssplit vec (pos+1) num (res1++ [vec !! pos] ) res2
                                 else ssplit vec (pos+1) num  res1 (res2++ [vec !! pos] )



split vec num = ssplit vec 0 num [] []



sslice vec pos n1 n2 res =  if pos== (length vec) 
                            then res
                            else if (n1<=pos && pos<=n2)
                            then sslice vec (pos+1) n1 n2 (res ++ [vec !! pos])
                            else sslice vec (pos+1) n1 n2 res


slice vec n1 n2 = sslice vec 0 (n1-1) (n2-1) []


remmove  vec pos n res1  = if pos== (length vec) 
                                 then (vec !! n,res1)
                                 else if pos == n 
                                 then remmove vec (pos+1) n res1 
                                 else remmove vec (pos+1) n (res1 ++ [vec !! pos]) 

removeAt n vec  = remmove  vec 0 (n-1) [] 



innn vec ch pos n res =  if pos== (length vec) 
                                 then res
                                 else if pos == n 
                                      then innn vec ch (pos+1) n (res++[ch]++[vec !! pos])
                                      else  innn vec ch (pos+1) n (res++[vec !! pos])





insertAt ch vec n = innn vec ch 0 (n-1) []



isPrime n = issprime n 2
   where issprime n i = if i*i>n
                        then  True
                        else if (mod n i) == 0
                        then False
                        else issprime n (i+1)




gcd1 a b = if (mod a b)== 0
          then b
          else gcd1   b (mod a b)


coprime a b = if (gcd a b )== 1
              then True
              else False

pprimef n i res = if i>n
                   then if n ==1 then res else (res++[n])
                   else if (mod n i)==0
                        then pprimef (div n i) i (res++[i])
                        else pprimef n (i+1) res 


primeFactors num = pprimef num 2 []
