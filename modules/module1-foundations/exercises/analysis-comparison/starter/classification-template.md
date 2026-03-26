# Analysis Classification Exercise

## Instructions
For each code snippet in `code-samples.md`, fill in the table below.

**Objective categories:** Correctness, Security, Performance
**Detection method:** Static, Dynamic, Both

---

| Snippet | Issue Description | Objective | Detection Method | Explanation |
|---------|-------------------|-----------|-----------------|-------------|
| 1 | SQL injection via string concatenation | Security | Static | A static taint analysis can trace unsanitized `user_id` into the query string; no execution needed |
| 2 | Unreachable code after `return` | Correctness | Static | The `console.log` after `return` is dead code, detectable by control-flow analysis |
| 3 | Division by zero when `numbers` is empty | Correctness | Dynamic | `len(numbers)` is 0 only for specific inputs; static analysis may flag it but runtime triggers the crash |
| 4 | Buffer overflow -- no null terminator written, no bounds checking | Security | Both | Static analysis can detect missing bounds check; dynamic analysis (fuzzing) can trigger the overflow |
| 5 | Off-by-one error (`<=` should be `<`) causes out-of-bounds access | Correctness | Both | Static analysis can flag `<=` with `.length`; dynamic testing exposes `undefined.name` crash |
| 6 | Exponential time complexity -- O(2^n) redundant recomputation | Performance | Dynamic | Static analysis can detect recursion but not easily quantify performance; profiling/benchmarking reveals the slowdown |
| 7 | Resource leak -- `FileInputStream` is never closed | Correctness | Static | Static analysis can track resource acquisition without corresponding `close()` on all paths |
| 8 | Command injection via unsanitized `user_input` in `os.system` | Security | Static | Taint analysis traces user input directly into a shell command without sanitization |
| 9 | Memory leak -- cache grows without bound | Performance | Dynamic | Static analysis cannot easily determine that `cache` is never cleared; long-running dynamic tests reveal growing memory |
| 10 | Unreachable code -- `result.clear()` after `return` | Correctness | Static | Dead code after `return` is detectable by control-flow analysis |
| 11 | Non-atomic transfer -- exception between withdraw and deposit loses money | Correctness | Dynamic | The race/exception condition depends on runtime behavior; static analysis could flag the missing try/catch but dynamic testing exposes the actual data loss |
| 12 | Unnecessary nested loop -- O(n^2) when O(n) suffices; inner `j` is unused | Performance | Static | Static analysis can detect that `j` is never used in the loop body, indicating wasted computation |
| 13 | Cross-site scripting (XSS) via unsanitized `innerHTML` | Security | Static | Static taint analysis can trace `userInput` flowing into `innerHTML` without escaping |
| 14 | Division by zero when `divisor` is 0 | Correctness | Dynamic | Depends on the runtime value of `divisor`; triggers a `ZeroDivisionError` only for specific inputs |
| 15 | Returning pointer to local stack variable (dangling pointer) | Correctness | Static | Static analysis can detect that `arr` is stack-allocated and its address escapes the function |

---

## Summary Questions

### How many snippets had Correctness issues? 8
### How many had Security issues? 4
### How many had Performance issues? 3

### Which issues are best caught by static analysis? Why?
Issues involving code structure and data flow -- unreachable code (snippets 2, 10), resource leaks (7), SQL/command/XSS injection (1, 8, 13), dangling pointers (15), and unused variables (12). These are detectable by analyzing the program's syntax, control flow, and data flow without needing to execute it. Static analysis excels when the bug pattern is a structural property of the code rather than dependent on specific runtime values.

### Which issues require dynamic analysis? Why?
Issues that depend on specific runtime values or environmental conditions -- division by zero (3, 14), infinite recursion with certain inputs (similar to 6's performance), memory leaks over time (9), and non-atomic operations that fail mid-execution (11). These bugs only manifest when particular inputs or execution conditions occur, which static analysis cannot fully predict without actually running the program.
