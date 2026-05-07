import type {
  BottleInvalid,
  BottleKind,
  BottleSize,
  BottleTint,
  InventoryBottleData,
  StageInfo,
  TxLogEntry,
} from './types';
import type { Reward } from '@/services/api';
import { STAGE_LABELS } from '@/lib/labels';

export const STAGES: StageInfo[] = [
  { id: 'inserted', label: 'Inserida', reward: 10 },
  { id: 'compacted', label: 'Compactada', reward: 3 },
];

const SIZES: BottleSize[] = ['S', 'M', 'L'];
const TINTS: BottleTint[] = ['clear', 'clear', 'green', 'blue', 'amber', 'clear'];
const KINDS: BottleKind[] = ['PET', 'PET', 'PET', 'HDPE', 'PET'];

export function buildInventory(seed = 7, count = 20): InventoryBottleData[] {
  let s = seed;
  const rand = () => {
    s = (s * 9301 + 49297) % 233280;
    return s / 233280;
  };
  // Pelo menos 1 lata e 1 vidro (em posições distintas e estáveis pela seed).
  const canPos = Math.floor(rand() * count);
  let glassPos = Math.floor(rand() * count);
  if (glassPos === canPos) glassPos = (glassPos + 1) % count;

  const list: InventoryBottleData[] = [];
  for (let i = 0; i < count; i++) {
    const inv: BottleInvalid = i === canPos ? 'can' : i === glassPos ? 'glass' : null;
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

function genId(prefix: string): string {
  return `${prefix}-${Date.now()}-${Math.floor(Math.random() * 9999)}`;
}

export function makeReplacementBottle(): InventoryBottleData {
  const rand = Math.random;
  return {
    id: genId('b-r'),
    size: SIZES[Math.floor(rand() * SIZES.length)],
    tint: TINTS[Math.floor(rand() * TINTS.length)],
    kind: KINDS[Math.floor(rand() * KINDS.length)],
    invalid: null,
    rot: `${(rand() * 24 - 12).toFixed(1)}deg`,
  };
}

export function makeInvalidReplacement(kind: 'can' | 'glass'): InventoryBottleData {
  const rand = Math.random;
  return {
    id: genId('b-r'),
    size: SIZES[Math.floor(rand() * SIZES.length)],
    tint: TINTS[Math.floor(rand() * TINTS.length)],
    kind: kind === 'can' ? 'AL' : 'GLASS',
    invalid: kind,
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

export function fmtDateTime(iso: string): string {
  const d = new Date(iso);
  if (Number.isNaN(d.getTime())) return iso;
  const p = (n: number) => String(n).padStart(2, '0');
  return `${p(d.getDate())}/${p(d.getMonth() + 1)}/${String(d.getFullYear()).slice(-2)} ${p(d.getHours())}:${p(d.getMinutes())}`;
}

export function rewardToTxLog(r: Reward): TxLogEntry {
  return {
    id: r.id,
    hash: r.tx_hash ?? '',
    label: STAGE_LABELS[r.stage] ?? r.stage,
    reward: r.greentoken_amount,
    datetime: fmtDateTime(r.sent_at),
  };
}
