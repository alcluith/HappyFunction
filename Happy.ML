
(*-------------------------------------------------------*)
(* Program to construct the chorus of Pharrel's "Happy"  *)
(* using a recursive function                            *)
(* Claire Quigley (claire.quigley@gmail.com)             *)
(* July 2014                                             *)
(*-------------------------------------------------------*)

(*-------------------------------------------------------*)
(*return all characters before the last space in the list*)
(*-------------------------------------------------------*)
fun tailspace [] ys:char list =  ys
	| tailspace (x::xs)  ys =  if x = #" " then
								ys
						   else
					   	   		tailspace (xs) (x::ys);

(*-------------------------------------------------------*)
(*return all characters after the first space in the list*)
(*-------------------------------------------------------*)
fun headspace [] ys:char list =  ys
	| headspace (x::xs)  ys =  if x = #" " then
								xs
						   else
					   	   		headspace (xs) (x::ys);

(*-------------------------------------------------------*)
(* return the last word in the string                    *)
(*-------------------------------------------------------*)
fun lastword mystr = let 
						val strlist = rev(explode mystr)
						val word = tailspace strlist []
					 in 
					 	implode word
					 end;


(*-------------------------------------------------------*)
(*returns true if list contains x, false otherwise       *)
(*-------------------------------------------------------*)
fun inList x [] = false
    |inList x (y::[]) = if x = y then 
    						true
    				    else
    				    	false
    |inList x (y::ys) = if x = y then 
    						true
    					else 
    						inList x ys;



(*-------------------------------------------------------*)
(*takes a string and returns it as a list of words       *)
(*-------------------------------------------------------*)
fun allwords "" (ys:string list) = ys
   |allwords mystr ys = 
   					let
						val strlist = rev(explode mystr)
					in 	
						if inList #" " strlist then 
						(*there is a space in the string, i.e. more than one word*)
							let 
							val thisword = tailspace strlist []
							val lastword = implode thisword
							val headstr = implode (rev(headspace strlist  []))
							val wordlist = lastword::ys
						 	in 
							    if thisword = [] 
							    then
							 		wordlist
							 	else 
							 		allwords headstr wordlist
							end
						else
							(*it's just one word*)
							mystr::ys
					end;
			 

(*-------------------------------------------------------*)
(*counts how many times an item appears in a list        *)
(*-------------------------------------------------------*)
fun numinlist [] y num = num
    |numinlist (x::[]) y num = if x = y then
    								num +1
    						    else
    						    	num
    |numinlist (x::xs) y num = if x = y then
    							  numinlist xs y (num + 1)
    						   else
    						   	  numinlist xs y num


(*-----------------------------------------------------------------*)
(*returns number of times the word "happy" appears in a string     *)
(*-----------------------------------------------------------------*)
fun happycount str = let val strwords = allwords str [] 
					  in
					  	 numinlist strwords "happy" 0
					  end;

(*-------------------------------------------------------*)
(*add a space to either the left or right of a string    *)
(*-------------------------------------------------------*)

fun lspace str = " "^str;
fun rspace str = str^" ";

(*-------------------------------------------------------*)
(*constructor functions for the lyrics                   *)
(*-------------------------------------------------------*)
fun because str = str^"Because I'm happy";					  

fun clap str = str^"Clap along if you";

fun know str = str^"know that"
fun feel str = str^"feel like"
				  
fun room str = str^"a room without a roof"
fun truth str = str^"happiness is the truth"
fun you str = str^"happiness is to you"
fun wanna str = str^"that's what you wanna do"


(*---------------------------------------------------------*)
(*constructor function for the chorus of Pharrel's "Happy" *)
(*---------------------------------------------------------*)

fun happy "" = happy (because "")
	|happy str =  let val happynum = happycount str
					  val endword = lastword str
				  in
				  		if endword = "happy"  then 
				  			if (happynum = 1) then 
				  				happy (room(rspace(feel(rspace(clap(rspace str))))))
				  			else if (happynum = 2) then 
				  				happy (truth(rspace(feel(rspace(clap(rspace str))))))
				  			else if (happynum = 3) then 
				  				happy (you(rspace(know(rspace(clap(rspace str))))))
				  			else if (happynum = 4) then 
				  				 	  (wanna(rspace(feel(rspace(clap(rspace str))))))
				  			else
				  				str
						else 
			   	   			happy (because (rspace str) )
			   	  end;

