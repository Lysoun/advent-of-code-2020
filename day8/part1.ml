open Printf
open Int64
open String
open Hashtbl
open Set
open Int
  
let input = "input.txt"

type operation = ACC | JMP | NOP;;

let string_to_operation operationString = 
    if (String.compare operationString "acc") = 0 
    then ACC 
    else 
        if (String.compare operationString "jmp") = 0 
        then JMP 
        else NOP;;

type instruction = {lineNumber: int; operation: operation; operand: char ; value: int};;
  
let instructions = Hashtbl.create 623;;

let parse_line lineNumber line = 
    {
        lineNumber = lineNumber; 
        operation = (string_to_operation (String.sub line 0 3));
        operand = (String.get line 4); 
        value = (Int64.to_int (Int64.of_string (String.sub line 5 ((String.length line) - 5))))
    };;

let rec parse_input file currentLineNumber =
   match try (input_line file) with End_of_file -> "" with
      | "" -> ()
      | line -> 
        (Hashtbl.add instructions currentLineNumber (parse_line currentLineNumber line));
        (parse_input file (currentLineNumber + 1))
    ;;

(parse_input (open_in input) 0);;

module IntSet = Set.Make(Int) ;;  

let compute_next_value initialValue instructionOperand instructionValue = if instructionOperand = '+' then (initialValue + instructionValue) else (initialValue - instructionValue);;

let rec execute_instructions instruction executedInstructions accumulator = 
    if (IntSet.mem instruction.lineNumber executedInstructions) 
    then accumulator
    else 
        let newExecutedInstructions = (IntSet.add instruction.lineNumber executedInstructions) in
        if instruction.operation = JMP
        then (
                execute_instructions 
                (Hashtbl.find instructions (compute_next_value instruction.lineNumber instruction.operand instruction.value)) 
                newExecutedInstructions 
                accumulator
            )
        else 
            let nextInstruction = (Hashtbl.find instructions (instruction.lineNumber + 1)) in
            if instruction.operation = ACC 
            then (execute_instructions nextInstruction newExecutedInstructions (compute_next_value accumulator instruction.operand instruction.value))
            else (execute_instructions nextInstruction newExecutedInstructions accumulator)
;;

print_int (execute_instructions (Hashtbl.find instructions 0) (IntSet.empty) 0);;