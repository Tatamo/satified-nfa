function decode(input) {
  const vars = input.trim().split(" ").map(s => {
    const flag = s[0] !== "~";
    if (!flag) s = s.substring(1);
    const name = s[0];
    s = s.substring(1);
    if (name === "x") return { flag, name, index: +s };
    else if (name === "p") return { flag, name, pos: s.split("_") };
  })
  const result = { x: [], p: [] };
  const xlist = vars.filter(({ name }) => name === "x");
  for (const { flag, index } of xlist) {
    result.x[index - 1] = flag ? 1 : 0;
  }
  const plist = vars.filter(({ name }) => name === "p")
  //    .sort(({ pos: [i1, s1] }, { pos: [i2, s2] }) => i1 - i2 === 0 ? i1 - i2 : s1 - s2);
  for (const { flag, name, pos: [index, state] } of plist) {
    if (result.p[index] === undefined) result.p[index] = [];
    result.p[index][state] = flag ? 1 : 0;
  }
  return result;
}

function formatVars(vars) {
  console.log(vars.x.join(" "));
  for (const l of vars.p) {
    console.log(l.join(" "));
  }
}

const input = require("fs").readFileSync("/dev/stdin", "utf8");
if (input.trim() === "~") {
  console.log("UNSAT");
} else {
  const vars = decode(input)
  formatVars(vars);
  console.log("SAT");
  console.log(`input string: ${vars.x.join("")}`);

}