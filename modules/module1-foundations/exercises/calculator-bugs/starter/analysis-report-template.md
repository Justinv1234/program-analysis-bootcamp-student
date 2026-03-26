# Analysis Report: Calculator Bugs

## Student Name: ___________________
## Date: ___________________

---

## Part 1: Static Analysis Findings (ESLint)

Run `npx eslint calculator.js` and record all findings below.

| # | Line | Rule | Description | Severity |
|---|------|------|-------------|----------|
| 1 | 16 | no-undef | `reslt` is not defined (should be `b`) | Error |
| 2 | 22 | no-unreachable | `console.log` after `return` is unreachable | Warning |
| 3 | 30-31 | no-fallthrough | Switch case "add" falls through to "subtract" | Error |
| 4 | 70 | no-unused-vars | `temp` is assigned but never used | Warning |
| 5 | 80 | no-constant-condition | `if (true)` is a constant condition | Warning |

**Total static analysis issues found:** 5

---

## Part 2: Dynamic Analysis Findings (Test Suite)

Run `node test-calculator.js` and record all test failures below.

| # | Test Name | Error Message | Root Cause |
|---|-----------|---------------|------------|
| 1 | add(2, 3) should be 5 | expected 5, got NaN | Bug 1: `reslt` undefined instead of `b` |
| 2 | calculate('add', 10, 5) should be 15 | expected 15, got 5 | Bug 3: switch fallthrough from "add" to "subtract" |
| 3 | divide(10, 0) should throw or return Infinity gracefully | Division by zero not handled | Bug 4: no check for b === 0 |
| 4 | factorial(-1) should handle negative input | Infinite recursion / stack overflow | Bug 5: no base case for negative numbers |
| 5 | multiply('3', 4) should be 12 | expected 12, got 0 | Bug 6: `==` coerces `"3"` to match `"0"` |

**Total dynamic analysis issues found:** 5

---

## Part 3: Comparison

### Which bugs did ONLY static analysis catch?

1. Bug 2: Unreachable code after `return` in `subtract` -- no test fails because the function still returns correctly
2. Bug 7: Unused variable `temp` in `power` -- no functional impact, so tests pass

### Which bugs did ONLY dynamic analysis catch?

1. Bug 4: Division by zero -- static analysis cannot determine runtime values of `b`
2. Bug 5: Infinite recursion with negative input -- requires executing the recursive path
3. Bug 6: Type coercion with `==` vs `===` -- requires passing a string argument at runtime

### Which bugs were found by BOTH approaches?

1. Bug 1: Undefined variable `reslt` -- ESLint flags `no-undef`, tests fail with `NaN`
2. Bug 3: Switch fallthrough -- ESLint flags `no-fallthrough`, tests show wrong result
3. Bug 8: Constant condition `if (true)` -- ESLint flags it, test for `absolute(5)` fails (returns -5)

---

## Part 4: Reflection

### Why can't static analysis catch all bugs?
Static analysis examines code structure without executing it, so it cannot reason about runtime values. Bugs like division by zero or infinite recursion depend on specific input values that are unknown at analysis time. Static analysis also struggles with dynamic typing issues like JavaScript's type coercion.

### Why can't dynamic analysis catch all bugs?
Dynamic analysis only tests the execution paths that are actually exercised. Dead code (unreachable statements) and unused variables have no runtime effect, so tests pass despite their presence. Test coverage is inherently incomplete -- you can only test the scenarios you think of.

### When would you prioritize one approach over the other?
Use static analysis early and continuously (in CI/editors) to catch structural issues like undefined variables, dead code, and style problems cheaply. Use dynamic analysis when correctness depends on runtime behavior, edge cases, or interactions between components. In practice, both should be used together for maximum coverage.
