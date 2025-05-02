3 + (if 5 < 6 then 10 else 100) ;;
let x = 14 ;;
let fact = fun f (n : int) : int is if n = 0 then 1 else n * f (n-1) ;;
fact 10 ;;

raise DivisionByZero;;
raise GenericException(1);;
raise GenericException(-2);;
raise GenericException 3 ;;
raise GenericException -4 ;;
try raise DivisionByZero with DivisionByZero -> 5 ;;
try raise DivisionByZero with DivisionByZero -> -5;;
try raise (DivisionByZero) with DivisionByZero -> 6 ;;
try raise (DivisionByZero) with (DivisionByZero) -> -6 ;;
try -7 / 0 with (DivisionByZero) -> -7;;
try 7/0 with DivisionByZero -> 7;;
try 8+0 with GenericException 108 -> 118;;
try 9 / 0 with GenericException(42) -> 9;;
try raise GenericException(101) with GenericException(101) -> 10;;
try raise GenericException(-101) with GenericException(-101) -> -10;;
try raise GenericException(102) with GenericException(201) -> 11;;
try raise (GenericException 42) with  GenericException 42 -> 12;;
try raise (GenericException(103)) with GenericException(103) -> 13;;
try raise (GenericException(104)) with (GenericException(104)) -> 14;;
try raise (GenericException(105)) with (GenericException(105)) -> 15;;
try raise ((GenericException(106))) with ((GenericException(106))) -> 16;;
let x = -10 + (try raise (DivisionByZero) with (DivisionByZero) -> -6) ;;

try
  try raise DivisionByZero with
    DivisionByZero -> 17
with
  DivisionByZero -> 108 ;;

try
  (try raise DivisionByZero with
    DivisionByZero -> 109/0)
with
  DivisionByZero -> 18 ;;

try
    (try 10/0 with
      DivisionByZero -> 109/0)
  with
    DivisionByZero -> -18 ;;

  try
    (try raise DivisionByZero with
      GenericException(201) -> 201)
  with
    DivisionByZero -> 19 ;;

try
    (try raise DivisionByZero with
        GenericException(202) -> 202)
    with
    GenericException(202) -> 20 ;;

try
    try raise (DivisionByZero) with (DivisionByZero) -> raise (GenericException 203)
    with
      (GenericException 203) -> 21 ;;
try
    try 10/0 with (DivisionByZero) -> raise (GenericException 203)
    with
      (GenericException 203) -> -21 ;;
    
try
  try
    raise DivisionByZero
  with DivisionByZero -> raise (GenericException 1)
with GenericException 1 -> -21 ;;

try
      try
          try raise (DivisionByZero) with (DivisionByZero) -> raise (GenericException 1)
        with (GenericException 1) -> raise (GenericException 2)
      with (GenericException 2) -> 22 ;;      


  try
    try
      try
        raise DivisionByZero
      with DivisionByZero -> raise (GenericException 10)
    with GenericException 10 -> raise (GenericException 20)
  with GenericException 20 -> 23 ;;

try
  try
    try
      raise DivisionByZero
    with DivisionByZero -> raise (GenericException 10)
  with GenericException 10 -> raise (GenericException 20)
with GenericException 20 -> 24 ;; 


try
  try
    try
       try
        raise DivisionByZero
      with
        DivisionByZero -> raise (GenericException(1))
    with
      GenericException(2) -> raise DivisionByZero
  with
    DivisionByZero -> raise (GenericException(2))
with
    GenericException(1) -> 25  ;;

    try
      try
        try
           try
            raise DivisionByZero
          with
            DivisionByZero -> raise (GenericException(1))
        with
          GenericException(1) -> raise DivisionByZero
      with
        DivisionByZero -> raise (GenericException(2))
    with
      GenericException(2) -> 26  ;; 

      try
        try
          try
             try
              raise DivisionByZero
            with
              DivisionByZero -> raise (GenericException(1))
          with
            GenericException(2) -> raise DivisionByZero
        with
          DivisionByZero -> raise (GenericException(2))
      with
        GenericException(1) -> 27 ;;

        try
          try
            try
               try
                (10/0)
              with
                DivisionByZero -> raise (GenericException(1))
            with
              GenericException(2) -> raise DivisionByZero
          with
            DivisionByZero -> raise (GenericException(2))
        with
          GenericException(2) -> 28  ;;
  
          try
            try
              try
                 try
                  (10/0)
                with
                  DivisionByZero -> raise (GenericException(1))
              with
                GenericException(1) -> raise DivisionByZero
            with
            GenericException(1) -> raise (GenericException(2))
          with
            GenericException(2) -> 29  ;;
          let fact = fun f (n : int) : int is
            if n < 0 then raise GenericException(-99)
            else if n = 0 then 1
            else n * f (n - 1) ;;
          
          try fact 5 with GenericException(1) -> 30 ;;
          try fact (-3) with GenericException(-99) -> -30 ;;
          let safediv = fun f (n : int) : int is 
          try 62 / n with DivisionByZero -> -31 ;;
          safediv 2 ;;
          safediv 0 ;;
          let fib = fun f (n : int) : int is  
            if n < 21 then 
              if n < 2 then n  
              else f(n - 1) + f(n - 2)  
            else  raise GenericException(-32) ;; 
          try fib 10 with GenericException(-32) -> 33 ;;
          try fib 30 with GenericException(-32) -> -33 ;;
      
          let tryadd = fun  f(x : int) : int is
          try if x < 0 then raise GenericException(-100) else x + 29
          with GenericException(-100) -> -34 ;;
          tryadd 5 ;;   
          tryadd (-2) ;;
try
  {42}
with{
  | DivisionByZero -> 0
  };;
try
  {3 * 5 + (if 5 < 6 then 10 else 100)}
with{
  | DivisionByZero -> 0
  };; 
try
 {21/6}
with{
 | DivisionByZero -> 0
};; 
try
 {21/0}
with{
 | DivisionByZero -> 0
};; 
let multiplyby2 = fun f (n : int) : int is if n = 0 then 0 else 2*n;;
try
 {multiplyby2 5}
with{
 | DivisionByZero -> 0
};; 
let fact = fun f (n : int) : int is if n = 0 then 1 else n * f (n-1);;
try
 {fact 5}
with{
 | DivisionByZero -> 0
};; 
let prime_dividebyn = fun f (n : int) : int is 333/n  ;;
try
 {prime_dividebyn 5}
with{
 | DivisionByZero -> 0
};; 
let prime_dividebyn = fun f (n : int) : int is 333/n  ;;
try
 {prime_dividebyn 0}
with{
 | DivisionByZero -> 100
};;
let safe_prime_dividebyn = fun f (n : int) : int is if n = 0 then 0 else 333/n  ;;
try
 {safe_prime_dividebyn 0}
with{
 | DivisionByZero -> 100
};; 

        

           



