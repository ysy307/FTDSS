# FORD Documentation Comment Rules for Fortran

## 1. General Principles

* All documentation comments for FORD must use `!>`.
* Implementation details must use normal comments `!`.
* Public procedures shall document:
1. Mathematical definition of the problem
2. Mathematical assumptions
3. Numerical guarantees
4. Computational complexity
5. Failure behavior


* All dummy arguments must be documented immediately above their declaration.
* Implementation details must not appear in module interfaces. Submodules shall document implementation strategy only.
* Comments must be concise and avoid duplication.

## 2. Type Declarations

* Place `!>` immediately before a type declaration.
* Describe the purpose and role of the type concisely.
* For public types, provide essential information only.

## 3. Type Members

* Place `!>` immediately before each member declaration.
* Provide a minimal explanation of its purpose, meaning, size, or unit.

## 4. Subroutines and Functions (Interface Level)

* Place `!>` immediately before the procedure declaration.
* Write one sentence describing the purpose.
* Documentation must follow this order:
1. Mathematical definition (using `\[ ... \]`)
2. Assumptions
3. Numerical guarantees
4. Computational complexity
5. Failure behavior


* Do not describe implementation details, loops, or temporary variables in the interface.

## 5. Arguments

* Document each argument immediately above its declaration.
* Include:
* Mathematical meaning
* Shape or size
* Constraints (e.g., SPD required, $tol > 0$)
* Overwrite behavior
* Valid range (for scalars)



## 6. Return Values (Functions)

* If the procedure has a result variable, document it immediately above its declaration.

## 7. Mathematical Notation Rules

* Use `\( ... \)` for inline mathematics.
* Use `\[ ... \]` for displayed equations.
* Do not use alternative delimiters.

## 8. Numerical Guarantee Rule

* When applicable, state numerical guarantees (e.g., backward stability, error bounds).
* If no theoretical bound exists, state: "No theoretical error bound available."

## 9. Computational Complexity Rule

* Computational complexity (Memory and Arithmetic) must be stated for all public procedures using big-O notation.

## 10. Submodule Documentation

* Submodules document implementation strategy only.
* Do not repeat the mathematical contract or public API guarantees.
* Include:
* Algorithm overview
* Numerical considerations
* Parallelization strategy
* Memory usage/work arrays



## 11. Variables

* **Submodule variables:** Document only those with mathematical meaning.
* **Temporary variables:** Do not use FORD comments (`!>`) for loop counters or self-explanatory temporaries. Use normal comments (`!`) if necessary.

## 12. Block Separation

* Use normal comments (`!`) for processing stages or logical blocks (e.g., `! Allocation`).

## 13. Public Declarations & Type-Bound Procedures

* Do not add FORD comments to `public ::` export lists.
* If documentation is provided at the procedure definition, do not duplicate it in the type declaration.

## 14. Language and Formatting

* All documentation must be written in **English**.
* Keep comments concise and purpose-focused.

---

Would you like me to generate a template file based on these rules?