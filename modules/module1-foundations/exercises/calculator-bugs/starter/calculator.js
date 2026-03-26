/**
 * Calculator Module - Program Analysis Exercise
 *
 * This calculator contains several bugs that can be found using
 * static analysis (ESLint) and dynamic analysis (running tests).
 *
 * YOUR TASK:
 * 1. Run ESLint: npx eslint calculator.js
 * 2. Run tests: node test-calculator.js
 * 3. Fix all bugs you find
 * 4. Fill out the analysis-report-template.md
 */

// Bug 1: Undefined variable (static analysis should catch this)
function add(a, b) {
    return a + b;
}

// Bug 2: Unreachable code (static analysis should catch this)
function subtract(a, b) {
    return a - b;
}

// Bug 3: Switch fallthrough (static analysis should catch this)
function calculate(operation, a, b) {
    let result;
    switch (operation) {
        case "add":
            result = add(a, b);
            break;
        case "subtract":
            result = subtract(a, b);
            break;
        case "multiply":
            result = multiply(a, b);
            break;
        case "divide":
            result = divide(a, b);
            break;
        default:
            result = NaN;
    }
    return result;
}

// Bug 4: Division by zero (dynamic analysis catches this)
function divide(a, b) {
    if (b === 0) {
        return 0;
    }
    return a / b;
}

// Bug 5: Infinite recursion (dynamic analysis catches this)
function factorial(n) {
    if (n < 0) {
        throw new Error("Factorial of negative number is undefined");
    }
    if (n === 0) {
        return 1;
    }
    return n * factorial(n - 1);
}

// Bug 6: Type coercion (dynamic analysis catches unexpected results)
function multiply(a, b) {
    if (a === 0 || b === 0) {
        return 0;
    }
    return a * b;
}

// Bug 7: Unused variable (static analysis should catch this)
function power(base, exponent) {
    let result = 1;
    for (let i = 0; i < exponent; i++) {
        result = result * base;
    }
    return result;
}

// Bug 8: Constant condition (static analysis should catch this)
function absolute(n) {
    if (n < 0) {
        return -n;
    }
    return n;
}

module.exports = {
    add,
    subtract,
    multiply,
    divide,
    calculate,
    factorial,
    power,
    absolute,
};
