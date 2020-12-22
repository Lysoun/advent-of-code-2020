open Printf
open Array
open Int
  
let input = "input.txt";;

let to_relative_coordinates (ferryX, ferryY) (absoluteWayPointX, absoluteWwayPointY) = (absoluteWayPointX - ferryX, absoluteWwayPointY - ferryY);;

let to_absolute_coordinates (ferryX, ferryY) (relativeWayPointX, relativeWayPointY) = (relativeWayPointX + ferryX, relativeWayPointX + ferryY);;

let rotate (x, y) rotationAngle = match rotationAngle with
    | 90 -> (-y, x)
    | 180 -> (-x, -y)
    | _ -> (y, -x)
;;

let computeRotationAngle rotationDirection rotationAngle =  match rotationDirection with
    | 'L' ->  rotationAngle
    | _ -> 360 - rotationAngle;;

let turn rotationDirection rotationAngle wayPointCoordinates = 
    rotate wayPointCoordinates (computeRotationAngle rotationDirection rotationAngle)
;;

let move direction number (x, y) = match direction with
    | 'E' -> (x + number, y)
    | 'S' -> (x, y - number)
    | 'W' -> (x - number, y)
    | _ -> (x, y + number)
;;

let print_state (ferryX, ferryY) (wayPointX, wayPointY) = 
    (print_string "ferry: ");
      (print_int ferryX);
      (print_string " , ");
      (print_int ferryY);
      (print_string "\n");
      (print_string "way point: ");
      (print_int wayPointX);
      (print_string " , ");
      (print_int wayPointY);
      (print_string "\n")
      ;;
  
let rec input_lines file (ferryX, ferryY) (wayPointX, wayPointY) =
   match try (input_line file) with End_of_file -> "" with
      | "" -> (Int.abs ferryX) + (Int.abs ferryY)
      | line -> 
      let value = (int_of_string (String.sub line 1 ((String.length line) - 1))) in
      let instruction = (String.get line 0) in
      if (instruction = 'L') || (instruction = 'R') 
      then (input_lines file (ferryX, ferryY) (turn instruction value (wayPointX, wayPointY)))
      else if (instruction = 'F') then (input_lines file (ferryX + (value * wayPointX), ferryY + (value * wayPointY)) (wayPointX, wayPointY))
      else (input_lines file (ferryX, ferryY) (move instruction value (wayPointX, wayPointY)))
    ;;

print_int (input_lines (open_in input) (0, 0) (10, 1));;