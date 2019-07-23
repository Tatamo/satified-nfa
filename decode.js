function decode(input) {
  return input.trim().split(" ").map(e => {
    const flag = e[0] !== "~";
    const name = +(flag ? e : e.substring(1));
    return { flag, num: name };
  }).sort((a, b) => a.name - b.name).map(({ flag, name }) => `${name}: ${flag ? 1 : 0}`).join("\n");
}