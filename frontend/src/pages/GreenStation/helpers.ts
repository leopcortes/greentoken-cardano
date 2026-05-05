import type {
  BottleInvalid,
  BottleKind,
  BottleSize,
  BottleTint,
  InventoryBottleData,
  StageInfo,
} from './types';

export const STAGES: StageInfo[] = [
  { id: 'inserted', label: 'Inserida', reward: 10 },
  { id: 'compacted', label: 'Compactada', reward: 3 },
];

const SIZES: BottleSize[] = ['S', 'M', 'L'];
const TINTS: BottleTint[] = ['clear', 'clear', 'green', 'blue', 'amber', 'clear'];
const KINDS: BottleKind[] = ['PET', 'PET', 'PET', 'HDPE', 'PET'];
const INVALIDS: BottleInvalid[] = [
  null, null, null, null, null, null, null, null, null, 'can', 'glass',
];

export function buildInventory(seed = 7, count = 20): InventoryBottleData[] {
  let s = seed;
  const rand = () => {
    s = (s * 9301 + 49297) % 233280;
    return s / 233280;
  };
  const list: InventoryBottleData[] = [];
  for (let i = 0; i < count; i++) {
    const inv = INVALIDS[Math.floor(rand() * INVALIDS.length)];
    list.push({
      id: `b-${i}`,
      size: SIZES[Math.floor(rand() * SIZES.length)],
      tint: TINTS[Math.floor(rand() * TINTS.length)],
      kind: inv ? (inv === 'can' ? 'AL' : 'GLASS') : KINDS[Math.floor(rand() * KINDS.length)],
      invalid: inv,
      rot: `${(rand() * 24 - 12).toFixed(1)}deg`,
    });
  }
  return list;
}

export function makeReplacementBottle(): InventoryBottleData {
  const rand = Math.random;
  const inv = INVALIDS[Math.floor(rand() * INVALIDS.length)];
  return {
    id: `b-r-${Date.now()}-${Math.floor(rand() * 9999)}`,
    size: SIZES[Math.floor(rand() * SIZES.length)],
    tint: TINTS[Math.floor(rand() * TINTS.length)],
    kind: inv ? (inv === 'can' ? 'AL' : 'GLASS') : KINDS[Math.floor(rand() * KINDS.length)],
    invalid: inv,
    rot: `${(rand() * 24 - 12).toFixed(1)}deg`,
  };
}

export function fakeTxHash(): string {
  const hex = '0123456789abcdef';
  let h = '';
  for (let i = 0; i < 64; i++) h += hex[Math.floor(Math.random() * 16)];
  return h;
}

export function truncMid(s: string, head = 8, tail = 6): string {
  if (!s || s.length <= head + tail + 1) return s;
  return `${s.slice(0, head)}…${s.slice(-tail)}`;
}

export function sizeLabelOf(size: BottleSize): string {
  return size === 'S' ? '300 ml' : size === 'L' ? '2 L' : '600 ml';
}

export function volumeMlOf(size: BottleSize): number {
  return size === 'S' ? 300 : size === 'L' ? 2000 : 600;
}

export function tintLabelOf(tint: BottleTint): string {
  const map: Record<BottleTint, string> = {
    clear: 'Transparente',
    green: 'Verde',
    blue: 'Azul',
    amber: 'Âmbar',
  };
  return map[tint];
}

export function kindLabelOf(b: InventoryBottleData): string {
  if (b.invalid === 'can') return 'Alumínio';
  if (b.invalid === 'glass') return 'Vidro';
  return b.kind;
}

export function materialLabelOf(b: InventoryBottleData): string {
  if (!b.invalid) return 'Reciclável (PET/HDPE)';
  return b.invalid === 'can' ? 'Alumínio (não aceito)' : 'Vidro (não aceito)';
}
