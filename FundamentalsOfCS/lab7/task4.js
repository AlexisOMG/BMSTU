'use strict'
const memo = (f) => {
    let cache = {};
    return (...args) => {
        const n = args;
        if (n in cache) {
            return cache[n];
        }
        else {
            const rez = f(n);
            cache[n] = rez;
            return rez;
        }
    }
}

const sqr = (n) => {
    console.log("n = " + n);
    return n ** 2;
}

const a = memo(sqr);
console.log(a(2));
console.log(a(2));
console.log(a(2));
console.log(a(3));
console.log(a(2));
