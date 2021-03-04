do_cc_fun <- function(
  x,
  hab,
  index = 1
){
  
  
 cc_fun(
   x,
   carcap = hab$cc[index],
   threshold = hab$threshold[[index]],
   z = hab$z[[index]]
 )
  
}