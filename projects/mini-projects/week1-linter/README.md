# Week 1 Mini-Project: Custom Python Linter

## Overview

Build a custom Python linter that enforces at least 5 style and correctness rules using static analysis techniques from Module 1.

**Duration:** 1 week (estimated 8-10 hours)
**Prerequisites:** Module 1 (Foundations of Program Analysis)

## Objectives

- Apply static analysis concepts to build a practical tool
- Understand how real linters (ESLint, Pylint, Flake8) work under the hood
- Practice identifying common code quality issues programmatically

## Requirements

### Core Requirements (70 points)

1. **Rule Implementation (40 pts):** Implement at least 5 lint rules. Choose from:
   - **Style rules:** line length limit, naming conventions (snake_case for functions, PascalCase for classes), trailing whitespace, missing docstrings
   - **Correctness rules:** unused imports, mutable default arguments, bare `except` clauses, comparison to `None` using `==` instead of `is`
   - **Complexity rules:** function too long (>50 lines), too many parameters (>5), deeply nested code (>4 levels)

2. **CLI Interface (15 pts):** Accept file or directory paths as arguments. Print results to stdout in a readable format.

3. **Configuration (15 pts):** Support enabling/disabling rules via a config file (JSON or YAML).

### Stretch Goals (30 points)

4. **Auto-fix (10 pts):** Implement `--fix` flag for at least 2 rules (e.g., trailing whitespace, naming conventions).
5. **Severity Levels (10 pts):** Classify issues as error, warning, or info with color-coded output.
6. **Exit Codes (5 pts):** Return non-zero exit code when errors are found (useful for CI).
7. **Report Generation (5 pts):** Output results as JSON or HTML report.

## Deliverables

1. `linter.py` -- Main entry point with CLI
2. `rules/` -- Directory containing rule implementations (one file per rule or grouped logically)
3. `config.json` -- Default configuration file
4. `README.md` -- Usage instructions and rule descriptions
5. `test_linter.py` -- Tests covering each rule with sample inputs
6. `sample_code/` -- Example Python files demonstrating each rule violation

## Suggested Approach

1. Start with a simple text-based rule (line length) to build the infrastructure
2. Move to AST-based rules (unused imports, mutable defaults) once you have the framework
3. Add configuration support
4. Write tests for each rule as you go (TDD)

## Grading Rubric

| Criteria | Points |
|----------|--------|
| 5+ rules correctly implemented | 40 |
| CLI interface works on files and directories | 15 |
| Configuration file support | 15 |
| Auto-fix for 2+ rules | 10 |
| Severity levels with color output | 10 |
| Exit codes | 5 |
| Report generation | 5 |
| **Total** | **100** |

## Resources

- Python `ast` module documentation
- [Flake8 source code](https://github.com/PyCQA/flake8) for architecture inspiration
- Module 1 slides on static vs. dynamic analysis
