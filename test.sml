datatype smltype = 
    tyvar of string 
  | tycons of smltype list * string 
  | func of smltype * smltype 
  | tupletype of smltype list 
  | recursivemearker
