'use strict';
const write = (len, cnt) => {
    const generate = (len) => {
        let s = "";
        const al = "abcdefghijklmnopqrstuvwxyz0123456789";
        const n = al.length;
        while (s.length < len) {
            s += al[Math.random() * n|0];
        }
        return s;
    };
    for (let i = 0; i < cnt; ++i) {
        console.log(generate(len));
    }
};
write(process.argv[2], process.argv[3]);
