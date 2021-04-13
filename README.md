# sml-base-functor

Enables convenient usage of recursion schemes in SML by automatically generating boilerplate.

Once the base functor + map functions have been generated, you can define e.g. the catamorphism function like this:

```sml
fun cata f t = f (map (cata f) (project t))
```
Note that this function can't be written point-free since that would cause evaluation of the partial application `cata f` to loop. 

## Example

Input:
```sml
datatype smltype = 
    tyvar of string 
  | tycons of smltype list * string 
  | func of smltype * smltype 
  | tupletype of smltype list 
  | recursivemarker
```

Output of `stack run < input.sml`
```sml
datatype ('self) smltypeF =
    tyvarF of string
  | tyconsF of ('self) list * string
  | funcF of 'self * 'self
  | tupletypeF of ('self) list
  | recursivemarkerF

fun project it =
  case it of
      tyvar v => (tyvarF) (v)
    | tycons v => (tyconsF) (v)
    | func v => (funcF) (v)
    | tupletype v => (tupletypeF) (v)
    | recursivemarker => recursivemarkerF

fun embed it =
  case it of
      tyvarF v => (tyvar) (v)
    | tyconsF v => (tycons) (v)
    | funcF v => (func) (v)
    | tupletypeF v => (tupletype) (v)
    | recursivemarkerF => recursivemarker

fun map f it =
  case it of
      tyvarF v => (tyvarF) (v)
    | tyconsF v =>
        (tyconsF) (let
                     val (v1, v2) = v
                   in
                     (let fun g v = (f) (v) in ((List.map) (g)) (v1) end, v2)
                   end)
    | funcF v => (funcF) (let val (v1, v2) = v in ((f) (v1), (f) (v2)) end)
    | tupletypeF v =>
        (tupletypeF) (let fun g v = (f) (v) in ((List.map) (g)) (v) end)
    | recursivemarkerF => recursivemarkerF
```

## Notes
 - For nested functor types, the program tries to infer which `map` function to use by doing `(capitalized type name).map` i.e. looking inside the module "capital type". For example for `list` it uses `List.map`. Some types don't use this format necesssarily (e.g. `Marked.t` would produce the wrong result). The lookup mechanism will be extended later

 - This should work if the input datatype is polymorphic:
 ```sml
datatype 'a list = Nil | Cons of 'a * 'a list

(* Generated code *)
datatype ('self,'a) listF = NilF | ConsF of 'a * 'self

fun project it =
  case it of
    Nil => NilF | Cons v => (ConsF) (v)

fun embed it =
  case it of
    NilF => Nil | ConsF v => (Cons) (v)

fun map f it =
  case it of
      NilF => NilF
    | ConsF v => (ConsF) (let val (v1, v2) = v in (v1, (f) (v2)) end)
 ```
  - This is basically purely syntactical manipulation so it won't
    see through type aliases or mutually recursive types, etc.
  - Lots of extra parenthesis since I didn't want to bother dealing with precedence. Might fix that later