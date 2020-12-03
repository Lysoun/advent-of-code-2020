open Printf
open String
open List
  
let input = "input.txt"

type slope = {right:int; down:int};;

let init_slope right down = {right=right;down=down};;

type slopeTravel = {slope:slope; currentLine:int; currentColumn:int; treeNumber:int};;

let init_slope_travel slope = {slope=slope;currentLine=0;currentColumn=0;treeNumber=0};;

let slopeTravels = (List.map init_slope_travel ([
    (init_slope 1 1);
    (init_slope 3 1);
    (init_slope 5 1);
    (init_slope 7 1);
    (init_slope 1 2)
]));;

let travel slopeTravel line lineNumber =
    if (lineNumber < slopeTravel.currentLine) then slopeTravel else
    {
        slope = slopeTravel.slope; 
        currentLine = (slopeTravel.currentLine + slopeTravel.slope.down); 
        currentColumn = (slopeTravel.currentColumn + slopeTravel.slope.right); 
        treeNumber = (slopeTravel.treeNumber + (if ((get line (slopeTravel.currentColumn mod (String.length line))) == '#') then 1 else 0))
    };;

let times a b = a * b;;
  
let rec input_lines file slopeTravels lineNumber =
   match try (input_line file) with End_of_file -> "" with
      | "" -> (fold_left times 1 (List.map (function slopeTravel -> slopeTravel.treeNumber) slopeTravels))
      | line -> (input_lines file (List.map (function slopeTravel -> (travel slopeTravel line lineNumber)) slopeTravels) (lineNumber + 1))
    ;;

print_int (input_lines (open_in input) slopeTravels 0);;