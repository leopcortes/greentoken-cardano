/**
 * Trunca uma string longa mantendo os primeiros `start` e últimos `end` caracteres.
 * Ex: truncateMiddle("addr_test1wz0j33dc4d75g0wy4ttvxg0s2xm420uurn9h7ytxtrq9lysa0ae0e", 20, 10)
 *  → "addr_test1wz0j33dc4d...sa0ae0e"
 */
export function truncateMiddle(str: string, start: number, end: number): string {
  if (str.length <= start + end + 3) return str;
  return `${str.slice(0, start)}...${str.slice(-end)}`;
}
