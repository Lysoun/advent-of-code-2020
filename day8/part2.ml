open Printf
open Int64
open String
open Hashtbl
open Set
open Int
open Seq
  
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
      | "" -> currentLineNumber
      | line -> 
        (Hashtbl.add instructions currentLineNumber (parse_line currentLineNumber line));
        (parse_input file (currentLineNumber + 1))
    ;;

let lastLineNumber = (parse_input (open_in input) 0);;

module IntSet = Set.Make(Int) ;;  

let compute_next_value initialValue instructionOperand instructionValue = if instructionOperand = '+' then (initialValue + instructionValue) else (initialValue - instructionValue);;

let rec execute_instructions instruction instructions executedInstructions accumulator = 
    if (IntSet.mem instruction.lineNumber executedInstructions) 
    then (false, accumulator)
    else 
        let newExecutedInstructions = (IntSet.add instruction.lineNumber executedInstructions) in
        if instruction.operation = JMP
        then 
            let nextInstructionLineNumber = (compute_next_value instruction.lineNumber instruction.operand instruction.value) in
            if nextInstructionLineNumber >= lastLineNumber 
            then (true, accumulator) 
            else
            (
                execute_instructions 
                (Hashtbl.find instructions nextInstructionLineNumber) 
                instructions
                newExecutedInstructions 
                accumulator
            )
        else 
            let nextInstructionLineNumber = (instruction.lineNumber + 1) in
            if nextInstructionLineNumber >= lastLineNumber 
            then (true, accumulator) 
            else
                let nextInstruction = (Hashtbl.find instructions nextInstructionLineNumber) in 
                if instruction.operation = ACC 
                then (execute_instructions nextInstruction instructions newExecutedInstructions (compute_next_value accumulator instruction.operand instruction.value))
                else (execute_instructions nextInstruction instructions newExecutedInstructions accumulator)
;;

let build_instruction_from operation instruction = 
    {
        lineNumber = instruction.lineNumber;
        operation = operation;
        operand = instruction.operand;
        value = instruction.value
    };;

let change_instruction instruction = match instruction.operation with
    | ACC -> instruction 
    | JMP -> (build_instruction_from NOP instruction)
    | NOP -> (build_instruction_from JMP instruction);;

let copy_and_change_instruction instructionLineNumber instructions = 
    let result = (Hashtbl.copy instructions) in
    (Hashtbl.replace result instructionLineNumber (change_instruction (Hashtbl.find instructions instructionLineNumber)));
    result
;;

let check_terminate_when_line_changes lineNumber = 
    let (terminate, accumulator) = (execute_instructions (Hashtbl.find instructions 0) (copy_and_change_instruction lineNumber instructions) (IntSet.empty) 0) in
    if terminate then print_int accumulator
 ;;

(Seq.iter check_terminate_when_line_changes (Hashtbl.to_seq_keys instructions))