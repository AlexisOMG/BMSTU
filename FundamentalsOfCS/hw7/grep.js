'use strict'
const fs = require('fs');
const grep = () => {
    const search = (searchExpr, file, checkN, checkM, cntMax) => {
        let cnt = 0;
        for (let ind = 0; ind < file.length; ++ind) {
            if (file[ind].match(searchExpr) != null) {
                ++cnt;
                if (checkN) {
                    process.stdout.write((ind + 1).toString());
                }
                console.log(file[ind]);
                if (checkM && cnt == cntMax) {
                    break;
                }
            }
        }
    }
    const clearing = (str) => {
        if (str[0] === '/' && str[str.length - 1] === '/') {
            str = str.slice(1, str.length);
            str = str.slice(0, str.length - 1);
        }
        return str;
    }
    let path = "";
    const options = process.argv;
    let checkI = 0, checkN = 0, checkE = 0, checkM = 0;
    let cntMax = "";
    for (let i = 2; i < options.length; ++i) {
        if (options[i] === "-i")
            checkI = 1;
        else if (options[i] === "-n") {
            checkN = 1;
        }
        else if (options[i] === "-e") {
            checkE = 1;
        }
        else if (options[i] === "-m") {
            checkM = 1;
            cntMax = options[++i];
        }
        else if (i != options.length - 1) {
            path = options[i];
        }
    }
    let filePath = options[options.length - 1];
    let searchExpr;
    if (checkI) {
        searchExpr = new RegExp(clearing(path), 'i');
    }
    else {
        searchExpr = new RegExp(clearing(path));
    }
    let file = fs.readFileSync(filePath, 'utf8').split('\n');
    search(searchExpr, file, checkN, checkM, cntMax);
}
try {
    grep();
}catch(err){
    console.error(err);
}
