'use strict'
const fs = require('fs');
const wc = () => {
    const options = process.argv;
    const path = options[options.length - 1];
    const name = path.split('/')[path.split('/').length - 1];
    let file = fs.readFileSync(path, 'utf8');
    const stat = fs.statSync(path);
    let wcnt = ((file[0] === ' ' || file[0] === '\n') ? 0 : 1);
    for (let i = 1; i < file.length; ++i) {
        if ((file[i] === ' ' || file[i] === '\n') && (file[i - 1] != ' ' && file[i - 1] != '\n')) {
            ++wcnt;
        }
    }
    --wcnt;
    for (let i = 2; i < options.length - 1; ++i) {
        switch (options[i]) {
            case "-c":
                process.stdout.write(stat['size'] + ' ');
                break;
            case "-l":
                process.stdout.write((file.split('\n').length - 1) + ' ');
                break;
            case "-m":
                process.stdout.write(file.length + ' ');
                break;
            case "-w":
                process.stdout.write(wcnt + ' ');
                break;
        }
    }
    if (options.length === 3) {
        process.stdout.write((file.split('\n').length - 1) + ' ' + wcnt + ' ' + stat['size'] + ' ');
    }
    console.log(name);
}
try {
    wc();
}catch(err){
    console.error(err);
}
