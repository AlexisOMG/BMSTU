'use strict'
const tree = () => {
    const fs = require('fs');
    const path = require('path');
    let space = [0];
    const createTree = (pt, dep, checkD, checkO, out) => {
        let ls = fs.readdirSync(pt);
        if (checkD)
        {
            for (let i = 0; i < ls.length; ++i) {
                if (!fs.statSync(pt + '/' + ls[i]).isDirectory()) {
                    ls.splice(i, 1);
                    --i;
                }
            }
        }
        let len = ls.length;
        ls.forEach(function(el, ind) {
            --len;
            let i = 0;
            for (; i < dep; ++i) {
                if (space[i]) {
                    if (checkO) {
                        fs.appendFileSync(out, "│   ");
                    }
                    else {
                        process.stdout.write("│   ");
                    }
                }
                else {
                    if (checkO) {
                        fs.appendFileSync(out, "    ");
                    }
                    else {
                        process.stdout.write("    ");
                    }
                }
            }
            if (el[0] != '.' && len > 0) {
                if (checkO) {
                    fs.appendFileSync(out, "├── " + el + "\n");
                }
                else {
                    console.log("├── " + el);
                }
            }
            else if (el[0] != '.'){
                if (checkO) {
                    fs.appendFileSync(out, "└── " + el + "\n");
                }
                else {
                    console.log("└── " + el);
                }
            }
            if (el[0] != '.' && fs.statSync(pt + '/' + el).isDirectory()) {
                if (len != 0) {
                    space[i] = 1;
                }
                else {
                    space[i] = 0;
                }
                createTree(pt + "/" + el, dep + 1, checkD, checkO, out);
            }
        });
    }
    if (process.argv.length == 4 && process.argv[2] == "-d") {
        const pt = process.argv[3];
        createTree(pt, 0, 1, 0, "");
    }
    else if (process.argv.length > 4) {
        let checkD = 0, checkO = 0;
        let out;
        for (let i = 0; i < process.argv.length; ++i) {
            if (process.argv[i] == "-d")
                checkD = 1;
            else if (process.argv[i] == "-o") {
                checkO = 1;
                out = process.argv[++i];
            }
        }
        const pt = process.argv[process.argv.length - 1];
        fs.writeFileSync(out, "");
        createTree(pt, 0, checkD, checkO, out);
    }
    else {
        const pt = process.argv[2];
        createTree(pt, 0, 0, 0, "");
    }
}
tree();
