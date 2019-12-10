// Learn more about F# at http://fsharp.org

//One way to approximate the Mandelbrot set is to consider a certain element within the orbit of every point on the plane (such as the 12th element) 
//and to check whether that element is within a a certain distance from the origin; 
//if it is within this distance, then a non-blank character should be printed, else it should be left blank 
//You should use the following function to calculate distances of points from the origin let norm (x,y) = x*x y*y 
//These distance values can then be used with disp and orbit to turn points on the plane into appropriate ASCII characters within an ASCII plot of the Mandelbrot set 
//Define a function mandelbrot that takes three arguments:
//- the resolution of the approximation, r (used with the plane function), the index of the elements, 1, to check in the orbit lists of the points, 
//and - the formatting list, 1 (to be used with the disp function) 
//This function should return a list of characters that corresponds to a picture approximating the shape of the Mandelbrot set on the plane 
//You will need to combine the split, plane, disp, and orbit functions appro priately; 
//list comprehensions are allowed Once you've defined the function mandelbrot, 
//you can generate an ASCII version of an approximation of the Mandelbrot set by evaluating the expression mandelbrot 17 12 disp_symbols > printfn "%s";;

open System.IO


let disp_symbols = [(0.15, '#'); (0.5, '*'); (1.0, '.')];;

let str chs = List.fold(fun str x -> str + x.ToString()) "" chs;;

//it takes a single integer argument r and returns the list of all points on the cartesian plane of the form (x/r, y/r) 
//where x and y are integers, x/r is between −2 and 1, and y/r is between −1 and 1 
let plane r = [
    for y in -1.0 .. 1.0/r .. 1.0 do
        for x in -2.0 .. 1.0/r .. 1.0 do
            yield (x, y)];;


//prefix (list, k)
//it accepts a positive integer k and a list list as input, and returns a list containing only the first n elements in the input list
let rec prefix (list, k) =
  match list with
  | [] -> []
  | head :: tail -> 
        if k > 0 then
                head :: prefix (tail, k-1)
        else
            []

//suffix (list, k)
//it accepts a positive integer n and a list list as input, and returns the list of elements that remain after the first n elements are dropped from the front of the list
let rec suffix (list, k) =
  match list with
  | [] -> []
  | head :: tail -> 
        if k > 1 then            
           suffix (tail, k-1)
        else
           tail

//myLen list
//it accepts a list list as input, and returns the length of the list
let rec myLen list =
    match list with
    |[] -> 0
    |head :: tail ->
        1 + myLen (suffix (list, 1))


//spilt list n y
//it takes a positive integer n, an element y, and a list list. The function inserts the specified element y after every n elements in the list
let rec split (n, list, y) =
    let p = prefix (list, n)    
    let s = suffix (list, n)
    if (myLen p) < n then
        p
    elif s =[] then
        p @ y
    else
        p @ y @ split (n, s, y)


//
//let pxy (x,y) (u,v)= (u*u-v*v+x, 2*u*v+y)

//orbit (x,y)
//it takes a single point (x,y) as an argument and returns an infinite list corresponding to O(x, y)
//let orbit (x,y) i = 
let rec orbit x y x0 y0 iteration =    
        let xtemp = x*x - y*y + x0
        let yy = 2.0*x*y + y0
        let xx = xtemp
        if iteration > 0 then
           [(xx,yy)]@orbit xx yy x0 y0 (iteration - 1)
        else   
            []        


        
//disp d l
//it takes two arguments: a number d and a list of pairs.
//it returns the character from the list that corresponds to the smallest number on the list that is greater than the 
//input d, and if d is larger than all the number in the list, isp should return a space character, ’ ’
let rec disp (list, (d:float)) = 
    match list with
    | [] -> ' '
    | head :: tail -> 
       if d < fst(head) then
            snd(head)
       else            
            disp (tail, (d:float))

//mandelbrot r i formattinglist
//it takes three arguments: r represents the resolution of the approximation, i represents the index of the elements 
//to check in the orbit lists of the points, and formattinglist represents the symbol list.
//It returns a list of characters that corresponds to a picture approximating the shape of the Mandelbrot set on the plane.
let mandelbrot r i formattinglist = 
    let myPlane = plane r
    let nlength = myPlane.Length
    let rec mandelHelper planelist len = 
        match planelist with
        | [] -> []
        | head :: tail ->
            let headtuple = planelist.Head
            let taillist = planelist.Tail
            let first = fst(headtuple)
            let second = snd(headtuple)
            let orbitList = orbit 0.0 0.0 first second 15    
            let p = orbitList.Item(i)
            let distance(x1,y1) = x1*x1 + y1*y1
            [(disp (disp_symbols, (distance(p))))] @ (mandelHelper taillist (len-1))

    split (3, (mandelHelper myPlane nlength), [' '])



let datalist = mandelbrot 25.0 12 disp_symbols
let nnn= System.String.Concat(Array.ofList(datalist))
//let rec buildString list = 
//    match list with
//    | [] -> "" 
//    | head::tail -> head + (buildString tail)
//let resultBuildString = buildString datalist
nnn |> printfn "%s";
