'use strict'

import fs from 'fs';
import { exit } from 'process';

class Variable {
    constructor(name) {
        this.name = name;
    }
    toString() {
        return this.name;
    }
}

class Constructor {
    constructor(name, cntArgs, ...args) {
        this.name = name;
        this.cntArgs = cntArgs;
        this.args = args;
    }
    toString() {
        let res = this.name + "(";
        this.args.forEach((t, ind)=>{
            res += t.toString();
            if (ind < this.cntArgs - 1) {
                res += ", ";
            }
        });
        res += ")";
        return res;
    }
}

class Constant {
    constructor(value) {
        this.value = value;
    }
    toString() {
        return this.value;
    }
}

const varNames = new Set();
const constructorNames = new Map();
const constants = new Set();

const parseVariableNames = (vars) => {
    vars.split(',').forEach((varName, _) => {
        varNames.add(varName);
    })
};

const parseConstructorNames = (constr) => {
    constr.split(',').forEach((str, _)=>{
        if (str[2] == '0') {
            constants.add(str[0]);
        } else {
            constructorNames.set(str[0], new Constructor(str[0], Number(str[2])));
        }
        
    });
};

const parseConstant = (str) => {
    if (str.expression.length > 0) {
        const c = str.expression[0];
        if (!constants.has(c)) {
            throw new Error("Unknown constant " + c);
        }

        str.expression = str.expression.slice(1);
        return new Constant(c);
    }

    return null;
};

const parseConstructor = (str) => {
    if (str.expression.length > 0) {
        const constructorName = str.expression[0];
        if (!constructorNames.has(constructorName)) {
            throw new Error("Unknown constructor " + constructorName);
        }
        str.expression = str.expression.slice(1);

        if (str.expression[0] != '(') {
            throw new Error("No open bracket for constructor:" + constructorName);
        } else {
            str.expression = str.expression.slice(1);
        }

        const args = [];
        let arg = parseTerm(str);
        args.push(arg);
        while (str.expression[0] == ',') {
            str.expression = str.expression.slice(1);
            arg = parseTerm(str);
            args.push(arg);
        }

        if (str.expression[0] != ')') {
            throw new Error("No close bracket for constructor:" + constructorName);
        }
        if (args.length != constructorNames.get(constructorName).cntArgs) {
            throw new Error("Wrong amount of args for constructor:" + constructorName);
        }

        str.expression = str.expression.slice(1);
        return new Constructor(constructorName, args.length, ...args);
    }
}

const parseVariable = (str) => {
    if (str.expression.length > 0) {
        const v = str.expression[0];
        if (!varNames.has(v)) {
            throw new Error("Unlnown variable " + v);
        }

        str.expression = str.expression.slice(1);
        return new Variable(v);
    }

    return null;
};

const parseTerm = (str) => {
    if (str.expression.length > 0) {
        const curr = str.expression[0];
        if (varNames.has(curr)) {
            return parseVariable(str);
        } else if (constants.has(curr)) {
            return parseConstant(str);
        } else if (constructorNames.has(curr)) {
            return parseConstructor(str);
        } else {
            throw new Error("Invalid expression");
        }

    }
    return null
};

let substitutionTerm1 = "";
let substitutionTerm2 = "";
const unification = (term1, term2) => {
    if (term1 instanceof Variable && term2 instanceof Variable
        && term1.name === term2.name) {
        return term1;
    }
    if (term1 instanceof Variable) {
        substitutionTerm1 += term1.toString() + ":=" + term2.toString() + ", ";
        return term2;
    }
    if (term2 instanceof Variable) {
        substitutionTerm2 += term2.toString() + ":=" + term1.toString() + ", ";
        return term1;
    }
    if (term1 instanceof Constructor && term2 instanceof Constructor
        && term1.name == term2.name && term1.cntArgs == term2.cntArgs) {
        const args = [];
        for (let i = 0; i < term1.cntArgs; ++i) {
            args.push(unification(term1.args[i], term2.args[i]));
        }
        return new Constructor(term1.name, term1.cntArgs, ...args);
    }
    if (term1 instanceof Constant && term2 instanceof Constant
        && term1.value == term2.value) {
        return term1;
    }

    throw new Error("Unable to unify");
}

const main = () => {
    const file = fs.readFileSync(process.argv[2]);
    const testCase = file.toString().split('\n');
    let constructors = "";
    let variables = "";
    let first = "";
    let second = "";
    console.log(testCase);
    testCase.forEach((str, ind) => {
        const pos = str.indexOf('=');
        switch (ind) {
            case 0:
                constructors = str.slice(pos+1).replace(/\s/g, '');
                break;
            case 1:
                variables = str.slice(pos+1).replace(/\s/g, '');
                break;
            case 2:
                first = str.slice(pos+1).replace(/\s/g, '');
                break;
            case 3:
                second = str.slice(pos+1).replace(/\s/g, '');
                break;
            default:
                exit(1);
        }
    });
    
    parseVariableNames(variables);
    parseConstructorNames(constructors);

    let firstTree = {};
    let secondTree = {};

    try {
        firstTree = parseTerm({"expression": first});
    } catch (error) {
        console.log("Unable to parse first term: " + error.message);
        exit(0);
    }

    try {
        secondTree = parseTerm({"expression": second});
    } catch (error) {
        console.log("Unable to parse second term: " + error.message);
        exit(0);
    }

    try {
        const unifier = unification(firstTree, secondTree);
        console.log("Substitutions for first term: " + substitutionTerm1.slice(0, substitutionTerm1.length-2));
        console.log("Substitutions for second term: " + substitutionTerm2.slice(0, substitutionTerm2.length-2));
        console.log("Unifier: %s", unifier.toString());
    } catch (error) {
        console.log(error.message);
    }
};

main();
	