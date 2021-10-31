'use strict'
const fs = require('fs');
const speller = () => {
    const cleaner = (xs, splitSymb) => {
        xs = xs.split(splitSymb);
        for (let i = 0; i < xs.length; ++i) {
            if (xs[i] === '') {
                xs = [].concat(xs.slice(0, i), xs.slice(i + 1, xs.length));
                --i;
            }
        }
        return xs;
    }
    const signs = ['.', '!', '?', ':', '-', ','];
    const dictPath = process.argv[2], filePath = process.argv[3];
    let dict = cleaner(fs.readFileSync(dictPath, 'utf8'), ' ');
    for (let i = 0; i < dict.length; ++i) {
        while (dict[i][dict[i].length - 1] === '\n') {
            dict[i] = dict[i].slice(0, dict[i].length - 1);
        }
        while (dict[i][0] === '\n') {
            dict[i] = dict[i].slice(1, dict[i].length);
        }
    }
    for (let i = 0; i < dict.length; ++i) {
        while (signs.indexOf(dict[i][dict[i].length - 1]) != -1) {
            dict[i] = dict[i].slice(0, dict[i].length - 1);
        }
    }
    //console.log(dict);
    let file = fs.readFileSync(filePath, 'utf8');
    let file1 = cleaner(file, ' ');
    for (let i = 0; i < file1.length; ++i) {
        while (signs.indexOf(file1[i][file1[i].length - 1]) != -1) {
            file1[i] = file1[i].slice(0, file1[i].length - 1);
        }
    }
    let answ = {};
    for (let i = 0, r = 1, c = 1, flag = 1, j = 0; i < file.length; ++i, ++c) {
        if (flag && signs.indexOf(file[i]) === -1 && file[i] != ' ') {
            flag = 0;
            let coord = new Object();
            coord.pos = i + 1;
            coord.row = r;
            coord.col = c;
            if (file1[j][file1[j].length - 1] === '\n' || file1[j][0] === '\n') {
                let ch1 = (file1[j][file1[j].length - 1] === '\n');
                let ch2 = (file1[j][0] === '\n');
                while (file1[j][0] === '\n') {
                    file1[j] = file1[j].slice(1, file1[j].length);
                }
                while (file1[j][file1[j].length - 1] === '\n') {
                    file1[j] = file1[j].slice(0, file1[j].length - 1);
                }
                if (ch1) {
                    ++r;
                    c = 1;
                }
                if (ch2) {
                    ++coord.row;
                    ++r;
                    c = 1;
                }
                answ[file1[j]] = coord;
                //console.log(answ[file1[j].slice(0, file1[j].length - 1)]);
            }
            else {
                answ[file1[j]] = coord;
                //console.log(answ[file1[j]]);
            }
            ++j;
        }
        else if (file[i] === ' ') {
            flag = 1;
        }
    }
    for (let key in answ) {
        if (dict.indexOf(key) === -1) {
            console.log(answ[key].row + ':' + answ[key].col + ' ' + key);
        }
    }
}
try {
    speller();
}catch(err){
    console.error(err);
}
