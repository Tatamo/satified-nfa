function decode(input) {
  return input.trim().split(" ").map(e => {
    const flag = e[0] !== "~";
    const num = +(flag ? e : e.substring(1));
    return { flag, num };
  }).sort((a, b) => a.num - b.num).map(({ flag, num }) => `${num}: ${flag ? 1 : 0}`).join("\n");
}