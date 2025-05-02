# READ ME

## Introduction

This README section explains how to use the MiniML language with the new exception handling features, including custom exceptions like `DivisionByZero` and `GenericException`.

MiniML is a functional programming language that supports arithmetic, recursive functions, and exception handling. This version includes two new built-in exceptions:

- **DivisionByZero**: Raised when an attempt is made to divide by zero.
- **GenericException of int**: A generic exception that carries an integer argument.

With the new exception handling system, users can now raise and catch exceptions within their programs.

## Installation

To get started with MiniML, clone the repository: https://github.com/Rmuk655/plzoo and build the environment.

**If you are under the `plzoo` directory, run:** python ./buildRun.py src/miniml/test_exn test_exp

## Basic Syntax

### Integers
MiniML supports the following integer values:
- `3`
- `42`
- `-7`

### Booleans
MiniML supports two boolean values:
- `true`
- `false`

### Arithmetic Operations
MiniML supports the following arithmetic operations:
- `+` (addition)
- `-` (subtraction)
- `*` (multiplication)
- `/` (division)

#### Example:
- 3 + 4 ;;  // Output: 7
- 10 - 5 ;; // Output: 5
- 3 * 6 ;;  // Output: 18
- 10 / 5 ;; // Output: 2
### Conditionals
The syntax for conditionals is:
if <condition> then <expr> else <expr>if 5 < 10 then 1 else 0 ;; // Output: 1
if 5 < 10 then 1 else 0 ;; // Output: 1
### Functions
You can define recursive functions using the fun keyword. The syntax for defining a function is as follows:
let <function_name> = fun <parameter_name> (<parameter_type>) : <return_type> is <function_body>
let fact = fun f (n : int) : int is
  if n = 0 then 1 else n * f (n - 1) ;;
### Function Application
To apply a function, simply call it with its arguments:
fact 5 ;; // Output: 120
## Exception Handling
### Raising Exceptions
MiniML allows you to raise exceptions using the raise keyword. You can raise two types of exceptions:

#### DivisionByZero:
raise DivisionByZero ;;
#### GenericException(n) where n is an integer (positive or negative):
raise GenericException(42) ;;
raise GenericException(-99) ;;
### Catching Exceptions
To catch exceptions, use the try ... with ... syntax. You specify the code to be executed within the try block and the exception handling in the with block.

#### Basic Example
To catch a DivisionByZero exception:
try 10 / 0 with DivisionByZero -> 5 ;;
This will output 5 as it catches the DivisionByZero exception and returns 5.

#### Catching GenericException
You can also catch a GenericException and handle it:
try raise GenericException(42) with GenericException(42) -> 10 ;;

This will output 10 as it catches the GenericException with value 42 and returns 10.

### Examples
Here are a few examples demonstrating the use of the new exception handling features:

#### Example 1: Safe Division
A division function that handles division by zero:
let safediv = fun f (n : int) : int is
  try 62 / n with DivisionByZero -> -31 ;;
safediv 2 ;;  // Output: 31
safediv 0 ;;  // Output: -31
#### Example 2: Factorial Function with Exception Handling
A factorial function that raises an exception for negative inputs:
let fact = fun f (n : int) : int is
  if n < 0 then raise GenericException(-99)
  else if n = 0 then 1
  else n * f (n - 1) ;;
try fact (-3) with GenericException(-99) -> -99 ;; // Output: -99
This catches the exception for negative numbers and returns -99.
#### Example 3: Nested Exception Handling
Here’s how to handle multiple levels of exceptions:
try
  try raise DivisionByZero with DivisionByZero -> 17
with
  DivisionByZero -> 108 ;;
This example demonstrates nested exception handling. The inner try ... with catches the DivisionByZero exception, and the outer one provides a different handler.
