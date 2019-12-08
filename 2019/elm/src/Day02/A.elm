module Day02.A exposing (..)

import Array exposing (Array)
import Helpers exposing (wrap, fromJust)

main = wrap solve



solve : String -> String
solve input =
  input
    |> parse
    |> prepare (12, 2)
    |> execProg 0
    |> Array.get 0
    |> fromJust
    |> String.fromInt


test : String -> String
test input =
  input
    |> parse
    |> execProg 0
    |> Array.toList
    |> List.map String.fromInt
    |> String.join ","




--- I/O ---

parse : String -> Array Int
parse raw =
  raw
    |> String.trim
    |> String.split ","
    |> List.filter ((/=) "")
    |> List.map (String.toInt >> fromJust)
    |> Array.fromList





--- solution ---

type alias Program =
  Array Int

type alias Instruction =
  (Int, (Int, Int), Int)

type Op
  = Add
  | Mul
  | Halt



toOp : Int -> Op
toOp int = case int of
  1  -> Add
  2  -> Mul
  99 -> Halt
  _  -> Debug.todo <| "INVALID OPCODE: " ++ String.fromInt int



prepare : (Int, Int) -> Program -> Program
prepare (noun, verb) prog =
  prog
    |> Array.set 1 noun
    |> Array.set 2 verb


execProg : Int -> Program -> Program
execProg idx prog =
  case prog |> execInst (getInst idx prog) of
    Just prog_ -> execProg (idx + 4) prog_
    Nothing    -> prog



execInst : Instruction -> Program -> Maybe (Program)
execInst (opcode, (oi1, oi2), ret) prog =
  let
    (op1, op2) = getOperands (oi1, oi2) prog

    write x = prog |> Array.set ret x
  in
    case opcode |> toOp of
      Add  -> Just <| write <| op1 + op2
      Mul  -> Just <| write <| op1 * op2
      Halt -> Nothing


getInst : Int -> Program -> Instruction
getInst idx prog =
  case prog |> Array.slice idx (idx + 4) |> Array.toList of
    opcode :: op1 :: op2 :: ret :: _ -> (opcode, (op1, op2), ret)
    99                          :: _ -> (99    , (0  , 0  ), -1 )
    _                                -> Debug.todo "INSTRUCTION OUT OF BOUNDS"


getOperands : (Int, Int) -> Program -> (Int, Int)
getOperands (i1, i2) prog =
  let
    mV1 = prog |> Array.get i1
    mV2 = prog |> Array.get i2
  in
    case (mV1, mV2) of
      (Just op1, Just op2) -> (op1, op2)
      _ -> Debug.todo "OPERAND NOT FOUND"
