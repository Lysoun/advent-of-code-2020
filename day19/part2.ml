open List

module Str = Humane_re.Str

let input = "input_part2.txt";;

let init_rules_hashtbl () = (Hashtbl.create 134);;

let rulesExtractionRegexp = (Str.regexp "^\\([0-9]+\\): \\(.+\\)$");;

let rec read_rules file rules =
   match try (input_line file) with End_of_file -> "" with
      | "" -> rules
      | line -> 
         let groups = (Str.Group.all (List.nth (Str.find_groups rulesExtractionRegexp line) 0)) in
         (Hashtbl.add rules (int_of_string (List.nth groups 0)) (List.nth groups 1));
         (read_rules file rules)
    ;;

let file = (open_in input);;
let rawRules = (read_rules file (init_rules_hashtbl ()));;

(* Compute rules regexp *)

let computedRules = (init_rules_hashtbl ());;
let singleCharacterRuleRegexp = Str.regexp "\"\\([a-z]\\)\"";;
let orRuleRegexp = Str.regexp "\\(.+\\) | \\(.+\\)";;

let rec raw_rule_to_regexp rawRule ruleNumber iterationsNumber =
   if (Str.matches singleCharacterRuleRegexp rawRule)
   then (Str.Group.group_exn (List.nth (Str.find_groups singleCharacterRuleRegexp rawRule) 0) 1)
   else
      if (String.contains rawRule '|')
      then (
         let rawRuleSplitted = (String.split_on_char '|' rawRule) in
         (String.concat "" [
            "\\(\\("; 
            (rules_numbers_to_concatenated_rules (String.trim (List.nth rawRuleSplitted 0)) ruleNumber iterationsNumber);
            "\\)\\|\\(";
            (rules_numbers_to_concatenated_rules (String.trim (List.nth rawRuleSplitted 1)) ruleNumber iterationsNumber);
            "\\)\\)"
      ]))
      else (rules_numbers_to_concatenated_rules rawRule ruleNumber iterationsNumber)
and find_or_compute_rule ruleNumber currentRuleNumber iterationsNumber = 
   if Hashtbl.mem computedRules ruleNumber 
   then (Hashtbl.find computedRules ruleNumber)
   else (
        if iterationsNumber > 100 
        then ""
        else
        if ruleNumber == currentRuleNumber
        then
        (let computedRule = (raw_rule_to_regexp (Hashtbl.find rawRules ruleNumber) ruleNumber (iterationsNumber + 1)) in
        (Hashtbl.add computedRules ruleNumber computedRule);
        computedRule
        ) else
        (let computedRule = (raw_rule_to_regexp (Hashtbl.find rawRules ruleNumber) ruleNumber 0) in
        (Hashtbl.add computedRules ruleNumber computedRule);
        computedRule
        )
   )
and rules_numbers_to_concatenated_rules rulesNumbers currentRuleNumber iterationsNumber = 
         String.concat "" (List.map (function ruleNumber -> (find_or_compute_rule (int_of_string ruleNumber) currentRuleNumber iterationsNumber)) (String.split_on_char ' ' rulesNumbers))
      ;;
;;

let ruleNumber = 0;;

let rule = String.concat "" ["^"; (raw_rule_to_regexp (Hashtbl.find rawRules ruleNumber) 0 0); "$"];;

(* Find number of messages that completely match rule 0 *)

let ruleRegexp = Str.regexp rule;;
let rec count_messages_matching_rule file acc = 
   match try (input_line file) with End_of_file -> "" with
      | "" -> acc
      | line when (Str.matches ruleRegexp line) -> (count_messages_matching_rule file (acc + 1)) 
      | _ -> (count_messages_matching_rule file acc)
    ;;

(print_int (count_messages_matching_rule file 0));;